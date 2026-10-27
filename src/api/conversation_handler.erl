-module(conversation_handler).
-dialyzer({nowarn_function, [mine/2, pin_conversation/2, delete_conversation/2]}).

-behaviour(cowboy_rest).

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
            set_manual_pending ->
                set_manual_pending(Req0, State);
            manual_pending_list ->
                manual_pending_list(Req0, State);
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
    Msg = io_lib:format(
        "vsn ~s, node ~p, 在线总人数: ~p, 在线设备数~p",
        [Vsn, node(), CountUser, Count]
    ),
    Qs = cowboy_req:parse_qs(Req0),
    Type = proplists:get_value(<<"type">>, Qs, undefined),
    % ?DEBUG_LOG(Res),
    List2 =
        case Type of
            <<"list">> ->
                LimitBin = proplists:get_value(<<"limit">>, Qs, <<"10">>),
                % ?DEBUG_LOG([limit, Limit]),
                {Limit2, _} = string:to_integer(binary_to_list(LimitBin)),
                % imboy_syn:list_by_limit(Limit);
                List1 = imboy_syn:list_by_limit(Limit2),
                Column = [
                    <<"uid">>, <<"pid">>, <<"dtype">>, <<"did">>, <<"time">>, <<"ref">>, <<"node">>
                ],
                [
                    lists:zipwith(
                        fun(X, Y) -> {X, Y} end,
                        Column,
                        [Uid, Pid, DType, DID, elib_dt:to_rfc3339(Nano), Ref, Node]
                    )
                 || {{Uid, Pid}, {DType, DID}, Nano, Ref, Node} <- List1
                ];
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
    case
        conversation_logic:list(CurrentUid, #{
            limit => 1000,
            last_server_ts => ServerTS
        })
    of
        {ok, List} ->
            elib_response:success(Req0, #{<<"list">> => List});
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
    Payload =
        case Body of
            <<>> -> #{};
            _ -> jsone:decode(Body, [{object_format, map}])
        end,

    CurrentUid = auth_ds:current_uid(State),
    ConversationId0 = maps:get(<<"conversation_id">>, Payload, <<>>),
    % conversation_id 客户端按 TSID 契约以 string 传输（防 JS 精度丢失），
    % 系统边界统一转回 integer 供 logic/ds/repo 使用；非数字原样返回交给 logic 守卫拒绝
    ConversationId = normalize_conversation_id(ConversationId0),
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
    Payload =
        case Body of
            <<>> -> #{};
            _ -> jsone:decode(Body, [{object_format, map}])
        end,

    CurrentUid = auth_ds:current_uid(State),
    ConversationId0 = maps:get(<<"conversation_id">>, Payload, <<>>),
    % conversation_id 客户端按 TSID 契约以 string 传输（防 JS 精度丢失），
    % 系统边界统一转回 integer 供 logic/ds/repo 使用；非数字原样返回交给 logic 守卫拒绝
    ConversationId = normalize_conversation_id(ConversationId0),
    Type = maps:get(<<"type">>, Payload, <<"c2c">>),

    case conversation_pin_logic:unpin(CurrentUid, ConversationId, Type) of
        ok ->
            elib_response:success(Req0, #{<<"updated">> => true});
        {error, Msg} when is_binary(Msg) ->
            elib_response:error(Req0, Msg, ?ERR_OPERATION_FAILED)
    end.

%% @doc W09.a（§6.7/J17.c）：显式目标 true/false 设置某会话人工待处理。
%% 真实未读水位不依赖本元数据；无变化同目标重试返回当前状态不重复副作用。
-spec set_manual_pending(cowboy_req:req(), map()) -> cowboy_req:req().
set_manual_pending(Req0, State) ->
    {ok, Body, _Req1} = cowboy_req:read_body(Req0),
    Payload =
        case Body of
            <<>> -> #{};
            _ -> jsone:decode(Body, [{object_format, map}])
        end,
    CurrentUid = auth_ds:current_uid(State),
    Uk3 = maps:get(<<"uk3">>, Payload, <<>>),
    Value = maps:get(<<"value">>, Payload, undefined),
    %% W09.c（J17.d）：可选 expected_version CAS；冲突返回当前状态供回读
    %% （保留 desired_value 由客户端决定重试，不自动取反）。
    Expected = normalize_expected_version(
        maps:get(<<"expected_version">>, Payload, undefined)
    ),
    case {is_binary(Uk3), Uk3 =/= <<>>, is_boolean(Value)} of
        {true, true, true} ->
            case user_setting_ds:set_manual_pending(CurrentUid, Uk3, Value, Expected) of
                {ok, Entry} ->
                    elib_response:success(Req0, #{
                        <<"uk3">> => Uk3,
                        <<"value">> => Value,
                        <<"version">> => maps:get(<<"version">>, Entry, 0)
                    });
                {conflict, Current} ->
                    elib_response:error(Req0, <<"版本冲突"/utf8>>, ?ERR_OPERATION_FAILED, #{
                        <<"conflict">> => true,
                        <<"current">> => #{
                            <<"uk3">> => Uk3,
                            <<"value">> => maps:get(<<"value">>, Current, false),
                            <<"version">> => maps:get(<<"version">>, Current, 0)
                        }
                    });
                {error, Msg} when is_binary(Msg) ->
                    elib_response:error(Req0, Msg, ?ERR_OPERATION_FAILED);
                _ ->
                    elib_response:error(
                        Req0, error_msg(?ERR_OPERATION_FAILED), ?ERR_OPERATION_FAILED
                    )
            end;
        _ ->
            elib_response:error(Req0, <<"人工待处理参数无效"/utf8>>, ?ERR_OPERATION_FAILED)
    end.

%% expected_version 缺省/非整数 → undefined（旧端语义，直接应用推进版本）。
normalize_expected_version(V) when is_integer(V), V >= 0 -> V;
normalize_expected_version(_) -> undefined.

%% @doc W09.a：当前账号全部人工待处理元数据投影（uk3/value/version）。
-spec manual_pending_list(cowboy_req:req(), map()) -> cowboy_req:req().
manual_pending_list(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    Map = user_setting_ds:manual_pending_map(CurrentUid),
    Items = [
        begin
            #{
                <<"uk3">> => Uk3,
                <<"value">> => maps:get(<<"value">>, Entry, false),
                <<"version">> => maps:get(<<"version">>, Entry, 0)
            }
        end
     || {Uk3, Entry} <- maps:to_list(Map), is_map(Entry)
    ],
    elib_response:success(Req0, #{<<"list">> => Items}).

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
    Payload =
        case Body of
            <<>> -> #{};
            _ -> jsone:decode(Body, [{object_format, map}])
        end,

    CurrentUid = auth_ds:current_uid(State),
    ConversationId0 = maps:get(<<"conversation_id">>, Payload, <<>>),
    % conversation_id 客户端按 TSID 契约以 string 传输（防 JS 精度丢失），
    % 系统边界统一转回 integer 供 logic/ds/repo 使用；非数字原样返回交给 logic 守卫拒绝
    ConversationId = normalize_conversation_id(ConversationId0),
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
    Payload =
        case Body of
            <<>> -> #{};
            _ -> jsone:decode(Body, [{object_format, map}])
        end,

    CurrentUid = auth_ds:current_uid(State),
    ConversationId0 = maps:get(<<"conversation_id">>, Payload, <<>>),
    % conversation_id 客户端按 TSID 契约以 string 传输（防 JS 精度丢失），
    % 系统边界统一转回 integer 供 logic/ds/repo 使用；非数字原样返回交给 logic 守卫拒绝
    ConversationId = normalize_conversation_id(ConversationId0),
    Type = maps:get(<<"type">>, Payload, <<"c2c">>),

    case conversation_logic:restore(CurrentUid, ConversationId, Type) of
        ok ->
            elib_response:success(Req0, #{<<"restored">> => true});
        {error, Msg} when is_binary(Msg) ->
            elib_response:error(Req0, Msg, ?ERR_OPERATION_FAILED)
    end.

%% @doc 归一化 conversation_id 参数
%% 客户端按 TSID 契约以 string 传输 conversation_id（防 JS 精度丢失），
%% 系统边界统一转回 integer 供 logic/ds/repo 使用；非数字原样返回交给 logic 守卫拒绝
%% @end
-spec normalize_conversation_id(term()) -> term().
normalize_conversation_id(Id) when is_binary(Id) ->
    try
        binary_to_integer(Id)
    catch
        _:_ -> Id
    end;
normalize_conversation_id(Id) ->
    Id.
