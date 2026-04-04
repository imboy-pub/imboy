-module(msg_handler).

-behavior(cowboy_rest).

-dialyzer([{nowarn_function, offline/2},
           {nowarn_function, get_c2c_msg_count/2},
           {nowarn_function, get_c2g_msg_count/2},
           {nowarn_function, get_s2c_msg_count/2},
           {nowarn_function, offline_ack/2},
           {nowarn_function, forward/2},
           {nowarn_function, reaction_add/2},
           {nowarn_function, reaction_remove/2},
           {nowarn_function, reaction_list/2}]).
           
-export([
    init/2,
    offline/2,
    offline_ack/2,
    read_stats/2,
    history/2,
    pin/2,
    forward/2,
    reaction_add/2,
    reaction_remove/2,
    reaction_list/2
]).

-include("log.hrl").
-include("error_code.hrl").
-include("chat.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 初始化消息处理器
%% 根据请求中的 action 参数调用相应的处理函数
%%
%% @param Req0 Cowboy请求对象
%% @param State0 状态映射，包含 action 和 current_uid 等信息
%% @return {ok, Req1, State} 处理后的请求对象和状态
%% @end
-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    % ?DEBUG_LOG(State0),
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 = messaging_logic:handle_rest_action(Action, Req0, State),
    {ok, Req1, State}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 处理离线消息请求
%% 获取用户的离线消息，包括C2C、C2G和S2C类型的消息
%%
%% @param Req0 Cowboy请求对象，包含分页参数
%% @param State 状态映射，包含 current_uid
%% @return 返回包含各种类型离线消息的响应
%% @end
-spec offline(cowboy_req:req(), map()) -> cowboy_req:req().
offline(Req0, State) ->
    messaging_logic:offline(Req0, State).

%% @doc 计算下一页的 last_msg_at 参数
%% 根据当前页的消息列表计算出下一页的起始时间戳
%%
%% @param Msgs 当前页的消息列表
%% @param LastMsgAt 当前页的起始时间戳
%% @return 下一页的起始时间戳
%% @end
-spec calculate_next_last_msg_at([map()], binary() | integer()) -> binary() | integer().
calculate_next_last_msg_at([], LastMsgAt) ->
    LastMsgAt;
calculate_next_last_msg_at(Msgs, _LastMsgAt) when length(Msgs) > 0 ->
    % 取最后一条消息的 created_at 作为下一页的起始点
    LastMsg = lists:last(Msgs),
    get_created_at(LastMsg).

%% @doc 辅助函数：获取消息的创建时间
%% 从消息映射或列表中获取 created_at 字段
%%
%% @param Msg 消息对象，可以是 map 或 list 格式
%% @return 消息的创建时间戳
%% @end
-spec get_created_at(map()) -> binary() | integer().
get_created_at(Msg) when is_map(Msg) ->
    maps:get(<<"created_at">>, Msg, 0).

%% @doc 获取C2C消息总数
%% 根据用户ID和最后消息时间戳计算消息总数
%%
%% @param Uid 用户ID
%% @param LastMsgAt 最后消息时间戳，undefined表示获取全部
%% @return 消息总数
%% @end
-spec get_c2c_msg_count(binary() | integer(), binary()) -> integer().
get_c2c_msg_count(Uid, LastMsgAt) ->
    % 使用安全的参数化查询，避免SQL注入
    Tb = msg_c2c_repo:tablename(),
    Sql = <<"SELECT count(*) as count FROM ",
            Tb/binary,
            " WHERE to_id = $1 AND created_at >= $2">>,
    case elib_pg:query(Sql, [Uid, LastMsgAt]) of
        {ok, [#{<<"count">> := Count}]} ->
            Count;
        _ ->
            0
    end.

%% @doc 获取C2G消息总数
%% 根据用户ID和最后消息时间戳计算群组消息总数
%%
%% @param Uid 用户ID
%% @param LastMsgAt 最后消息时间戳，undefined表示获取全部
%% @return 消息总数
%% @end
-spec get_c2g_msg_count(integer(), binary()) -> integer().
get_c2g_msg_count(Uid, LastMsgAt) ->
    % 使用安全的参数化查询，避免SQL注入
    Tb = msg_c2g_timeline_repo:tablename(),
    Sql = <<"SELECT count(*) as count FROM ",
            Tb/binary,
            " WHERE to_id = $1 AND client_ack = 0 AND created_at >= $2">>,
    case elib_pg:query(Sql, [Uid, LastMsgAt]) of
        {ok, [#{<<"count">> := Count}]} ->
            Count;
        _ ->
            0
    end.

%% @doc 获取S2C消息总数
%% 根据用户ID和最后消息时间戳计算系统消息总数
%%
%% @param Uid 用户ID
%% @param LastMsgAt 最后消息时间戳，undefined表示获取全部
%% @return 消息总数
%% @end
-spec get_s2c_msg_count(integer(), binary()) -> integer().
get_s2c_msg_count(Uid, LastMsgAt) ->
    % 使用安全的参数化查询，避免SQL注入
    Tb = msg_s2c_repo:tablename(),
    Sql = <<"SELECT count(*) as count FROM ",
            Tb/binary,
            " WHERE to_id = $1 AND created_at >= $2">>,
    case elib_pg:query(Sql, [Uid, LastMsgAt]) of
        {ok, [#{<<"count">> := Count}]} ->
            Count;
        _ ->
            0
    end.

%% @doc 处理单个消息
%% 将 from_id 和 to_id 替换为编码后的 from 和 to
%%
%% @param Msg 原始消息映射
%% @return 处理后的消息映射
%% @end
-spec process_message(map()) -> map().
process_message(Msg) when is_map(Msg) ->
    % 对于 map 格式的消息（来自 c2g）
    FromId = maps:get(<<"from_id">>, Msg, undefined),
    ToId = maps:get(<<"to_id">>, Msg, undefined),

    % 删除原有的 from_id 和 to_id 字段
    Msg2 = maps:remove(<<"from_id">>, Msg),
    Msg3 = maps:remove(<<"to_id">>, Msg2),

    % 添加编码后的 from 和 to 字段
    Msg4 =
        case FromId of
            undefined ->
                Msg3;
            _ ->
                Msg3#{<<"from">> => elib_hashids:encode(FromId)}
        end,

    case ToId of
        undefined ->
            Msg4;
        ToList when is_list(ToList) ->
            % 对于群组消息，to_id 是一个列表
            ToEncoded = [elib_hashids:encode(ToUid) || ToUid <- ToList],
            Msg4#{<<"to">> => ToEncoded};
        _ ->
            % 对于单个用户
            Msg4#{<<"to">> => elib_hashids:encode(ToId)}
    end.

%% @doc 处理离线消息确认
%% 处理客户端确认已接收的离线消息
%%
%% @param Req0 Cowboy请求对象，包含消息类型和ID列表
%% @param State 状态映射，包含 current_uid
%% @return 返回处理结果响应
%% @end
-spec offline_ack(cowboy_req:req(), map()) -> cowboy_req:req().
offline_ack(Req0, State) ->
    messaging_logic:offline_ack(Req0, State).

%% @doc 处理群消息已读统计请求
%% 获取指定群消息的已读人数和总人数
%%
%% @param Req0 Cowboy请求对象，包含 msg_id 参数
%% @param State 状态映射，包含 current_uid
%% @return 返回已读统计信息
%% @end
-spec read_stats(cowboy_req:req(), map()) -> cowboy_req:req().
read_stats(Req0, State) ->
    messaging_logic:read_stats(Req0, State).

%% @doc 查询消息历史（conv_seq 游标分页）
%%
%% GET /v1/msg/history?chat_type=c2c&peer_id=xxx&after_seq=0&limit=50
%%
%% @param Req0 Cowboy 请求对象
%% @param State 包含 current_uid 的状态
%% @end
-spec history(cowboy_req:req(), map()) -> cowboy_req:req().
history(Req0, State) ->
    messaging_logic:history(Req0, State).

%% @doc 处理消息置顶请求
%% 设置消息的置顶状态
%%
%% @param Req0 Cowboy请求对象，包含消息ID和置顶状态
%% @param State 状态映射，包含 current_uid
%% @return 返回处理结果响应
%% @end
-spec pin(cowboy_req:req(), map()) -> cowboy_req:req().
pin(Req0, State) ->
    case maps:get(current_uid, State, undefined) of
        undefined ->
            elib_response:error(Req0, <<"未授权"/utf8>>, ?ERR_UNAUTHORIZED);
        CurrentUid ->
            % 获取请求参数
            {ok, Body, _Req1} = elib_req:body(Req0, []),
            MsgId = maps:get(<<"msg_id">>, Body, undefined),
            Pinned = maps:get(<<"pinned">>, Body, undefined),

            % 参数验证
            case {MsgId, Pinned} of
                {undefined, _} ->
                    elib_response:error(Req0, <<"缺少消息ID参数"/utf8>>, ?ERR_BAD_REQUEST);
                {_, undefined} ->
                    elib_response:error(Req0, <<"缺少置顶状态参数"/utf8>>, ?ERR_BAD_REQUEST);
                _ ->
                  % 调用逻辑层处理置顶操作
                    case Pinned of
                        true ->
                            case msg_pinned_logic:pin(MsgId, CurrentUid) of
                                ok ->
                                    Payload = #{
                                        <<"msg_id">> => MsgId,
                                        <<"pinned">> => true
                                    },
                                    elib_response:success(Req0, Payload, <<"置顶成功"/utf8>>);
                                {error, not_found} ->
                                    elib_response:error(Req0, <<"消息不存在"/utf8>>, ?ERR_NOT_FOUND);
                                {error, Reason} ->
                                    elib_response:error(Req0, Reason, ?ERR_INTERNAL_SERVER_ERROR)
                            end;
                        false ->
                            case msg_pinned_logic:unpin(MsgId, CurrentUid) of
                                ok ->
                                    Payload = #{
                                        <<"msg_id">> => MsgId,
                                        <<"pinned">> => false
                                    },
                                    elib_response:success(Req0, Payload, <<"取消置顶成功"/utf8>>);
                                {error, not_found} ->
                                    elib_response:error(Req0, <<"消息不存在"/utf8>>, ?ERR_NOT_FOUND);
                                {error, Reason} ->
                                    elib_response:error(Req0, Reason, ?ERR_INTERNAL_SERVER_ERROR)
                            end
                    end
            end
    end.

%% @doc 处理消息转发请求
%% 转发一条或多条消息到指定会话
%%
%% @param Req0 Cowboy请求对象，包含消息ID列表和目标会话信息
%% @param State 状态映射，包含 current_uid
%% @return 返回处理结果响应
%% @end
-spec forward(cowboy_req:req(), map()) -> cowboy_req:req().
forward(Req0, State) ->
    case maps:get(current_uid, State, undefined) of
        undefined ->
            elib_response:error(Req0, <<"未授权"/utf8>>, ?ERR_UNAUTHORIZED);
        CurrentUid ->
            % 获取请求参数
            {ok, Body, _Req1} = elib_req:body(Req0, []),
            MsgIds = maps:get(<<"msg_ids">>, Body, []),
            To = maps:get(<<"to">>, Body, undefined),
            ToType = maps:get(<<"to_type">>, Body, undefined),

            % 参数验证
            case {MsgIds, To, ToType} of
                {[], _, _} ->
                    elib_response:error(Req0, <<"缺少消息ID列表参数"/utf8>>, ?ERR_BAD_REQUEST);
                {_, undefined, _} ->
                    elib_response:error(Req0, <<"缺少目标会话ID参数"/utf8>>, ?ERR_BAD_REQUEST);
                {_, _, undefined} ->
                    elib_response:error(Req0, <<"缺少目标类型参数"/utf8>>, ?ERR_BAD_REQUEST);
                _ ->
                    % 解码目标ID
                    ToId = elib_hashids:decode(To),

                    % 调用逻辑层处理转发
                    case msg_forward_logic:forward(MsgIds, CurrentUid, ToId, ToType) of
                        {ok, ForwardMsgIds} ->
                            Payload = #{
                                <<"msg">> => <<"messages_forwarded">>,
                                <<"forward_msg_ids">> => ForwardMsgIds,
                                <<"forward_count">> => length(ForwardMsgIds)
                            },
                            elib_response:success(Req0, Payload, <<"转发成功"/utf8>>);
                        {error, {invalid_param, Msg}} ->
                            elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
                        {error, {not_friends, Msg}} ->
                            elib_response:error(Req0, Msg, ?ERR_NOT_FRIENDS);
                        {error, {not_group_member, Msg}} ->
                            elib_response:error(Req0, Msg, ?ERR_NOT_GROUP_MEMBER);
                        {error, {in_denylist, Msg}} ->
                            elib_response:error(Req0, Msg, ?ERR_FORBIDDEN);
                        {error, {permission_denied, Msg}} ->
                            elib_response:error(Req0, Msg, ?ERR_ACCESS_DENIED);
                        {error, {msg_not_found, Msg}} ->
                            elib_response:error(Req0, Msg, ?ERR_MESSAGE_NOT_FOUND);
                        {error, Reason} ->
                            elib_response:error(Req0, Reason, ?ERR_INTERNAL_SERVER_ERROR)
                    end
            end
    end.

%% @doc 处理添加表情回应请求
%% 添加表情到指定消息
%%
%% @param Req0 Cowboy请求对象，包含消息ID和emoji
%% @param State 状态映射，包含 current_uid
%% @return 返回处理结果响应
%% @end
-spec reaction_add(cowboy_req:req(), map()) -> cowboy_req:req().
reaction_add(Req0, State) ->
    messaging_logic:reaction_add(Req0, State).

%% @doc 处理移除表情回应请求
%% 从指定消息移除表情
%%
%% @param Req0 Cowboy请求对象，包含消息ID和emoji
%% @param State 状态映射，包含 current_uid
%% @return 返回处理结果响应
%% @end
-spec reaction_remove(cowboy_req:req(), map()) -> cowboy_req:req().
reaction_remove(Req0, State) ->
    messaging_logic:reaction_remove(Req0, State).

%% @doc 处理查询表情列表请求
%% 查询指定消息的所有表情
%%
%% @param Req0 Cowboy请求对象，包含消息ID
%% @param State 状态映射，包含 current_uid
%% @return 返回处理结果响应
%% @end
-spec reaction_list(cowboy_req:req(), map()) -> cowboy_req:req().
reaction_list(Req0, _State) ->
    messaging_logic:reaction_list(Req0, _State).
