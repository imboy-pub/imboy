-module(conversation_handler).

-behavior(cowboy_rest).

-export([init/2]).

-include("log.hrl").
-include("error_code.hrl").
-import(imboy_error, [error_msg/1]).

%% ===================================================================
%% API
%% ===================================================================

%% @doc 初始化会话处理器
%% 根据请求中的 action 参数调用相应的处理函数
%%
%% @param Req0 Cowboy请求对象
%% @param State0 状态映射，包含 action 和 current_uid 等信息
%% @return {ok, Req1, State} 处理后的请求对象和状态
%% @end
-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    % ?DEBUG_LOG(State),
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 =
        case Action of
            online ->
                online(Req0, State);
            mine ->
                mine(Req0, State);
            pin_conversation ->
                pin_conversation(Req0, State);
            unpin_conversation ->
                unpin_conversation(Req0, State);
            pinned_list ->
                pinned_list(Req0, State);
            delete_conversation ->
                delete_conversation(Req0, State);
            restore_conversation ->
                restore_conversation(Req0, State);
            false ->
                Req0
        end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 获取在线用户信息
%% 返回当前在线用户数量和列表
%%
%% @param Req0 Cowboy请求对象，包含查询参数
%% @param _State 状态映射
%% @return 返回包含在线用户信息的响应
%% @end
-spec online(cowboy_req:req(), map()) -> cowboy_req:req().
online(Req0, _State) ->
    {ok, Vsn} = application:get_key(imboy, vsn),
    CountUser = imboy_syn:count_user(),
    Count = imboy_syn:count(),
    Msg = io_lib:format("vsn ~s, node ~p, 在线总人数: ~p, 在线设备数~p",
                        [Vsn, node(), CountUser, Count]),
    Qs = cowboy_req:parse_qs(Req0),
    Type = proplists:get_value(<<"type">>, Qs, undefined),
    % ?DEBUG_LOG(Res),
    List2 = case Type of
        <<"list">> ->
            LimitBin = proplists:get_value(<<"limit">>, Qs, <<"10">>),
            % ?DEBUG_LOG([limit, Limit]),
            {Limit2, _} = string:to_integer(binary_to_list(LimitBin)),
            % imboy_syn:list_by_limit(Limit);
            List1 = imboy_syn:list_by_limit(Limit2),
            Column = [<<"uid">>, <<"pid">>, <<"dtype">>, <<"did">>, <<"time">>, <<"ref">>, <<"node">>],
            [lists:zipwith(fun(X, Y) -> {X, Y} end,
                           Column,
                    [Uid, Pid, DType, DID, elib_dt:to_rfc3339(Nano), Ref, Node])
              || {{Uid, Pid}, {DType, DID}, Nano, Ref, Node} <- List1 ];
        _ ->
            []
    end,
    elib_response:success(Req0, List2, Msg).

%% @doc 获取我的会话列表
%% 获取当前用户的会话消息列表，并过滤已删除的会话
%%
%% @param Req0 Cowboy请求对象，包含 last_server_ts 参数
%% @param State 状态映射，包含 current_uid
%% @return 返回包含会话列表的响应
%% @end
-spec mine(cowboy_req:req(), map()) -> cowboy_req:req().
mine(Req0, State) ->
    Qs = cowboy_req:parse_qs(Req0),
    ServerTS = proplists:get_value(<<"last_server_ts">>, Qs, undefined),
    CurrentUid = auth_ds:current_uid(State),
    case conversation_logic:list(CurrentUid, #{
        limit => 1000,
        last_server_ts => ServerTS
    }) of
        {ok, List} ->
            elib_response:success(Req0, List);
        {error, Reason} ->
            ?ERROR_LOG([conversation_mine_failed, CurrentUid, Reason]),
            elib_response:error(Req0, error_msg(?ERR_OPERATION_FAILED), ?ERR_OPERATION_FAILED)
    end.

%% @doc 置顶会话
%% 置顶指定的会话（单聊或群聊）
%%
%% @param Req0 Cowboy请求对象，包含请求体
%% @param State 状态映射，包含 current_uid
%% @return 返回操作结果
%% @end
-spec pin_conversation(cowboy_req:req(), map()) -> cowboy_req:req().
pin_conversation(Req0, State) ->
    {ok, Body, _Req1} = cowboy_req:read_body(Req0),
    Payload = case Body of
        <<>> -> #{};
        _ -> jsone:decode(Body, [{object_format, map}])
    end,

    CurrentUid = auth_ds:current_uid(State),
    ConversationId = maps:get(<<"conversation_id">>, Payload, <<>>),
    Type = maps:get(<<"type">>, Payload, <<"c2c">>),

    case conversation_pin_logic:pin(CurrentUid, ConversationId, Type) of
        ok ->
            elib_response:success(Req0, #{});
        {error, Msg} when is_binary(Msg) ->
            elib_response:error(Req0, Msg, ?ERR_OPERATION_FAILED);
        _ ->
            elib_response:error(Req0, error_msg(?ERR_OPERATION_FAILED), ?ERR_OPERATION_FAILED)
    end.

%% @doc 取消置顶会话
%% 取消置顶指定的会话
%%
%% @param Req0 Cowboy请求对象，包含请求体
%% @param State 状态映射，包含 current_uid
%% @return 返回操作结果
%% @end
-spec unpin_conversation(cowboy_req:req(), map()) -> cowboy_req:req().
unpin_conversation(Req0, State) ->
    {ok, Body, _Req1} = cowboy_req:read_body(Req0),
    Payload = case Body of
        <<>> -> #{};
        _ -> jsone:decode(Body, [{object_format, map}])
    end,

    CurrentUid = auth_ds:current_uid(State),
    ConversationId = maps:get(<<"conversation_id">>, Payload, <<>>),
    Type = maps:get(<<"type">>, Payload, <<"c2c">>),

    ok = conversation_pin_logic:unpin(CurrentUid, ConversationId, Type),
    elib_response:success(Req0, #{<<"updated">> => true}).

%% @doc 获取置顶会话列表
%% 获取当前用户的置顶会话列表
%%
%% @param Req0 Cowboy请求对象
%% @param State 状态映射，包含 current_uid
%% @return 返回置顶会话列表
%% @end
-spec pinned_list(cowboy_req:req(), map()) -> cowboy_req:req().
pinned_list(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),

    case conversation_pin_logic:list(CurrentUid) of
        {ok, List} ->
            elib_response:success(Req0, #{<<"items">> => List});
        {error, _Reason} ->
            elib_response:error(Req0, error_msg(?ERR_OPERATION_FAILED), ?ERR_OPERATION_FAILED)
    end.

%% @doc 删除会话（软删除）
%% 删除指定的会话（单聊或群聊）
%%
%% @param Req0 Cowboy请求对象，包含请求体
%% @param State 状态映射，包含 current_uid
%% @return 返回操作结果
%% @end
-spec delete_conversation(cowboy_req:req(), map()) -> cowboy_req:req().
delete_conversation(Req0, State) ->
    {ok, Body, _Req1} = cowboy_req:read_body(Req0),
    Payload = case Body of
        <<>> -> #{};
        _ -> jsone:decode(Body, [{object_format, map}])
    end,

    CurrentUid = auth_ds:current_uid(State),
    ConversationId = maps:get(<<"conversation_id">>, Payload, <<>>),
    Type = maps:get(<<"type">>, Payload, <<"c2c">>),

    case conversation_logic:delete(CurrentUid, ConversationId, Type) of
        ok ->
            elib_response:success(Req0, #{});
        {error, Msg} when is_binary(Msg) ->
            elib_response:error(Req0, Msg, ?ERR_OPERATION_FAILED);
        _ ->
            elib_response:error(Req0, error_msg(?ERR_OPERATION_FAILED), ?ERR_OPERATION_FAILED)
    end.

%% @doc 恢复已删除的会话
%% 恢复指定的已删除会话
%%
%% @param Req0 Cowboy请求对象，包含请求体
%% @param State 状态映射，包含 current_uid
%% @return 返回操作结果
%% @end
-spec restore_conversation(cowboy_req:req(), map()) -> cowboy_req:req().
restore_conversation(Req0, State) ->
    {ok, Body, _Req1} = cowboy_req:read_body(Req0),
    Payload = case Body of
        <<>> -> #{};
        _ -> jsone:decode(Body, [{object_format, map}])
    end,

    CurrentUid = auth_ds:current_uid(State),
    ConversationId = maps:get(<<"conversation_id">>, Payload, <<>>),
    Type = maps:get(<<"type">>, Payload, <<"c2c">>),

    ok = conversation_logic:restore(CurrentUid, ConversationId, Type),
    elib_response:success(Req0, #{<<"restored">> => true}).
