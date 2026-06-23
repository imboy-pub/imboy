-module(message_router_logic).

%%%
% 消息路由模块
% 负责将 WebSocket 消息路由到对应的业务逻辑模块
% 当前作为 messaging_logic 的内部 WebSocket 路由器
%%%

-include("log.hrl").
-include("chat.hrl").

%% ===================================================================
%% API 导出
%% ===================================================================
-export([route/5]).

%% ===================================================================
%% API 函数
%% ===================================================================

%% @doc 统一消息路由入口
%%
%% 将 WebSocket 接收到的消息路由到对应的业务逻辑模块。
%% 根据 Action 字段决定是路由 action 消息还是普通消息。
%%
%% @param MsgId 消息ID
%% @param CurrentUid 当前用户ID
%% @param Data 解码后的消息数据（已包含 sender_did/sender_dtype）
%% @param Type 消息流向类型 (C2C/C2G/C2S/S2C/webrtc_*)
%% @param OriginalMsg 原始消息 JSON（用于 WebRTC 等特殊处理）
%% @return ok | {reply, map()}
%% @end
-spec route(binary(), integer(), map(), binary(), binary()) -> ok | {reply, map()}.
route(MsgId, CurrentUid, Data, Type, OriginalMsg) ->
    %% v2.0: action 在顶层
    Action = maps:get(<<"action">>, Data, <<>>),
    TypeLower = cowboy_bstr:to_lower(Type),

    case {TypeLower, Action} of
        {<<"s2c">>, _} ->
            %% S2C 由 msg_s2c_logic 基于 action 自己分发
            route_normal_message(MsgId, CurrentUid, Data, Type, OriginalMsg);
        {_, ActionBin} when is_binary(ActionBin), ActionBin =/= <<>> ->
            %% action 消息（撤销、编辑等）
            ActionLower = cowboy_bstr:to_lower(ActionBin),
            route_action_message(ActionLower, MsgId, CurrentUid, Data, Type);
        _ ->
            %% 普通消息（无 action）
            route_normal_message(MsgId, CurrentUid, Data, Type, OriginalMsg)
    end.

%% ===================================================================
%% 内部函数
%% ===================================================================

%% @doc 路由普通消息（无 action）
%%
%% 根据 Type 字段分发到对应的 logic 模块。
%%
%% @private
-spec route_normal_message(binary(), integer(), map(), binary(), binary()) -> ok | {reply, map()}.
route_normal_message(MsgId, CurrentUid, Data, Type, OriginalMsg) ->
    case cowboy_bstr:to_lower(Type) of
        <<"c2c">> ->
            %% 单聊消息
            msg_c2c_logic:c2c(MsgId, CurrentUid, Data);
        <<"c2g">> ->
            %% 群聊消息
            msg_c2g_logic:c2g(MsgId, CurrentUid, Data);
        <<"c2s">> ->
            %% 客户端到服务端消息（请求、查询等）
            msg_c2s_logic:c2s(MsgId, CurrentUid, Data);
        <<"s2c">> ->
            %% 服务端到客户端消息（通常客户端不应主动发送）
            %% 但某些场景下客户端可能触发 S2C 操作（如删除消息）
            % Payload = maps:get(<<"payload">>, Data),
            % MsgType = maps:get(<<"msg_type">>, Payload, <<>>),
            Action = maps:get(<<"action">>, Data, <<>>),
            msg_s2c_logic:s2c(Action, MsgId, CurrentUid, Data);
        <<"webrtc_", _Event/binary>> ->
            %% WebRTC 信令处理
            To = maps:get(<<"to">>, Data),
            ToUid = ec_cnv:to_integer(To),
            webrtc_ws_logic:event(CurrentUid, ToUid, MsgId, OriginalMsg);
        _ ->
            %% 未知消息类型
            _ = ?WARN_LOG({unknown_message_type, Type, MsgId, CurrentUid}),
            ok
    end.

%% @doc 路由 action 消息
%%
%% 根据 Type 和 Action 分发到对应的处理函数。
%%
%% 支持的 action：
%% - message_revoke: 消息撤销
%% - message_revoke_ack: 撤销确认
%% - message_edit: 消息编辑
%% - message_edit_ack: 编辑确认
%%
%% @private
-spec route_action_message(binary(), binary(), integer(), map(), binary()) -> ok | {reply, map()}.
route_action_message(Action, MsgId, CurrentUid, Data, Type) ->
    TypeLower = cowboy_bstr:to_lower(Type),

    case TypeLower of
        <<"c2c">> ->
            route_c2c_action(Action, MsgId, CurrentUid, Data);
        <<"c2g">> ->
            route_c2g_action(Action, MsgId, CurrentUid, Data);
        _ ->
            %% 不支持的消息类型
            _ = ?WARN_LOG({unsupported_action_type, TypeLower, Action, MsgId}),
            {reply, message_ds:assemble_s2c(MsgId, <<"invalid_message_type">>, <<>>)}
    end.

%% @doc 路由 C2C action 消息（查表 dispatch，action 注册见 imboy_ws_action_registry）
%%
%% @private
-spec route_c2c_action(binary(), binary(), integer(), map()) -> ok | {reply, map()}.
route_c2c_action(Action, MsgId, CurrentUid, Data) ->
    route_action(<<"c2c">>, Action, MsgId, CurrentUid, Data).

%% @doc 路由 C2G action 消息（查表 dispatch，action 注册见 imboy_ws_action_registry）
%%
%% @private
-spec route_c2g_action(binary(), binary(), integer(), map()) -> ok | {reply, map()}.
route_c2g_action(Action, MsgId, CurrentUid, Data) ->
    route_action(<<"c2g">>, Action, MsgId, CurrentUid, Data).

%% @doc 按 {Type, Action} 查注册表分派到对应 {Module, Function}；
%% 未注册则回 unknown_action。内置 action 由 imboy_ws_action_registry 启动注册，
%% 插件可通过 imboy_ws_action_registry:register/3 扩展。
%%
%% @private
-spec route_action(binary(), binary(), binary(), integer(), map()) -> ok | {reply, map()}.
route_action(Type, Action, MsgId, CurrentUid, Data) ->
    case imboy_ws_action_registry:lookup(Type, Action) of
        {ok, {Module, Function}} ->
            Module:Function(MsgId, CurrentUid, Data);
        undefined ->
            %% 注册表未启动（单元测试）或 action 未注册——回退内置映射
            case imboy_ws_action_registry:builtin_lookup(Type, Action) of
                {ok, {Module, Function}} ->
                    Module:Function(MsgId, CurrentUid, Data);
                undefined ->
                    _ = ?WARN_LOG({unknown_action, Type, Action, MsgId}),
                    {reply, message_ds:assemble_s2c(MsgId, <<"unknown_action">>, <<>>)}
            end
    end.
