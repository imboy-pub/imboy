-module(message_router_logic).

%%%
% 消息路由模块
% 负责将 WebSocket 消息路由到对应的业务逻辑模块
% 替代 websocket_handler 中的消息分发逻辑
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
    Action = maps:get(<<"action">>, Data, null),

    case Action of
        null ->
            %% 普通消息（无 action）
            route_normal_message(MsgId, CurrentUid, Data, Type, OriginalMsg);
        _ ->
            %% action 消息（撤销、编辑等）
            ActionLower = string:lowercase(Action),
            route_action_message(ActionLower, MsgId, CurrentUid, Data, Type)
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
            ToUid = elib_hashids:decode(To),
            webrtc_ws_logic:event(CurrentUid, ToUid, MsgId, OriginalMsg);
        _ ->
            %% 未知消息类型
            ok = ?WARN_LOG({unknown_message_type, Type, MsgId, CurrentUid}),
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
            ok = ?WARN_LOG({unsupported_action_type, TypeLower, Action, MsgId}),
            {reply, message_ds:assemble_s2c(MsgId, <<"invalid_message_type">>, <<>>)}
    end.


%% @doc 路由 C2C action 消息
%%
%% @private
-spec route_c2c_action(binary(), binary(), integer(), map()) -> ok | {reply, map()}.
route_c2c_action(<<"message_revoke">>, MsgId, CurrentUid, Data) ->
    msg_c2c_logic:c2c_revoke(MsgId, CurrentUid, Data);
route_c2c_action(<<"message_revoke_ack">>, MsgId, CurrentUid, Data) ->
    msg_c2c_logic:c2c_revoke_ack(MsgId, CurrentUid, Data);
route_c2c_action(<<"message_edit">>, MsgId, CurrentUid, Data) ->
    msg_c2c_logic:c2c_edit(MsgId, CurrentUid, Data);
route_c2c_action(<<"message_edit_ack">>, MsgId, CurrentUid, Data) ->
    msg_c2c_logic:c2c_edit_ack(MsgId, CurrentUid, Data);
route_c2c_action(Action, MsgId, _CurrentUid, _Data) ->
    ok = ?WARN_LOG({unknown_c2c_action, Action, MsgId}),
    {reply, message_ds:assemble_s2c(MsgId, <<"unknown_action">>, <<>>)}.


%% @doc 路由 C2G action 消息
%%
%% @private
-spec route_c2g_action(binary(), binary(), integer(), map()) -> ok | {reply, map()}.
route_c2g_action(<<"message_revoke">>, MsgId, CurrentUid, Data) ->
    msg_c2g_logic:c2g_revoke(MsgId, CurrentUid, Data);
route_c2g_action(<<"message_revoke_ack">>, MsgId, CurrentUid, Data) ->
    msg_c2g_logic:c2g_revoke_ack(MsgId, CurrentUid, Data);
route_c2g_action(<<"message_edit">>, MsgId, CurrentUid, Data) ->
    msg_c2g_logic:c2g_edit(MsgId, CurrentUid, Data);
route_c2g_action(<<"message_edit_ack">>, MsgId, CurrentUid, Data) ->
    msg_c2g_logic:c2g_edit_ack(MsgId, CurrentUid, Data);
route_c2g_action(Action, MsgId, _CurrentUid, _Data) ->
    ok = ?WARN_LOG({unknown_c2g_action, Action, MsgId}),
    {reply, message_ds:assemble_s2c(MsgId, <<"unknown_action">>, <<>>)}.
