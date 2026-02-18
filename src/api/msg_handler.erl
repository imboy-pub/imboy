-module(msg_handler).

-behavior(cowboy_rest).

-dialyzer([{nowarn_function, offline/2},
           {nowarn_function, get_c2c_msg_count/2},
           {nowarn_function, get_c2g_msg_count/2},
           {nowarn_function, get_s2c_msg_count/2},
           {nowarn_function, offline_ack/2},
           {nowarn_function, process_offline_ack/3},
           {nowarn_function, forward/2},
           {nowarn_function, reaction_add/2},
           {nowarn_function, reaction_remove/2},
           {nowarn_function, reaction_list/2}]).
           
-export([init/2]).

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
    Req1 =
        case Action of
            offline ->
                offline(Req0, State);
            offline_ack ->
                offline_ack(Req0, State);
            read_stats ->
                read_stats(Req0, State);
            pin ->
                pin(Req0, State);
            forward ->
                forward(Req0, State);
            reaction_add ->
                reaction_add(Req0, State);
            reaction_remove ->
                reaction_remove(Req0, State);
            reaction_list ->
                reaction_list(Req0, State);
            false ->
                Req0
        end,
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
    {ok, Limit} = elib_param:int(limit, Req0, 1000),
    {ok, C2CLastMsgAtInt} = elib_param:int(c2c_last_msg_at, Req0, 0),
    {ok, C2GLastMsgAtInt} = elib_param:int(c2g_last_msg_at, Req0, 0),
    {ok, S2CLastMsgAtInt} = elib_param:int(s2c_last_msg_at, Req0, 0),

    C2CLastMsgAt = elib_dt:to_rfc3339(C2CLastMsgAtInt, millisecond),
    C2GLastMsgAt = elib_dt:to_rfc3339(C2GLastMsgAtInt, millisecond),
    S2CLastMsgAt = elib_dt:to_rfc3339(S2CLastMsgAtInt, millisecond),

    % 安全获取 current_uid，不存在时返回未授权错误
    case maps:get(current_uid, State, undefined) of
        undefined ->
            elib_response:error(Req0, <<"未授权"/utf8>>, ?ERR_UNAUTHORIZED);
        CurrentUid ->
            % 获取各种类型的总记录数用于判断是否还有更多数据
            CountC2CMsg = get_c2c_msg_count(CurrentUid, C2CLastMsgAt),
            CountC2GMsg = get_c2g_msg_count(CurrentUid, C2GLastMsgAt),
            CountS2CMsg = get_s2c_msg_count(CurrentUid, S2CLastMsgAt),

            % 获取各种类型的离线消息，每种类型独立分页
            C2CMsgs = msg_c2c_ds:read_msg(CurrentUid, Limit, C2CLastMsgAt),
            C2GMsgs = msg_c2g_ds:read_msg(CurrentUid, Limit, C2GLastMsgAt),
            S2CMsgs = msg_s2c_ds:read_msg(CurrentUid, Limit, S2CLastMsgAt),

            % 处理消息数据：替换 from_id 和 to_id 为编码后的 from 和 to
            ProcessedC2CMsgs = [process_message(Msg) || Msg <- C2CMsgs],
            ProcessedC2GMsgs = [process_message(Msg) || Msg <- C2GMsgs],
            ProcessedS2CMsgs = [process_message(Msg) || Msg <- S2CMsgs],

            % 计算每种类型的分页信息并构建 payload 结构
            Payload =
                #{<<"c2c">> =>
                      #{<<"has_more">> => length(ProcessedC2CMsgs) < CountC2CMsg,
                        <<"next_last_msg_at">> =>
                            calculate_next_last_msg_at(ProcessedC2CMsgs, C2CLastMsgAt),
                        <<"total">> => CountC2CMsg,
                        <<"list">> => ProcessedC2CMsgs},
                  <<"c2g">> =>
                      #{<<"has_more">> => length(ProcessedC2GMsgs) < CountC2GMsg,
                        <<"next_last_msg_at">> =>
                            calculate_next_last_msg_at(ProcessedC2GMsgs, C2GLastMsgAt),
                        <<"total">> => CountC2GMsg,
                        <<"list">> => ProcessedC2GMsgs},
                  <<"s2c">> =>
                      #{<<"has_more">> => length(ProcessedS2CMsgs) < CountS2CMsg,
                        <<"next_last_msg_at">> =>
                            calculate_next_last_msg_at(ProcessedS2CMsgs, S2CLastMsgAt),
                        <<"total">> => CountS2CMsg,
                        <<"list">> => ProcessedS2CMsgs}},
            elib_response:success(Req0, Payload)
    end.

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
    CurrentUid = auth_ds:current_uid(State),

    % 获取请求参数
    PostVals = elib_param:post(Req0),
    Type =
        string:lowercase(
            maps:get(<<"type">>, PostVals, <<>>)),
    MsgIds = maps:get(<<"msg_ids">>, PostVals, []),

    ok =
        ?INFO_LOG("Processing offline_ack for user: ~p, type: ~p, msg_count: ~p",
                  [CurrentUid, Type, length(MsgIds)]),

    % 处理离线消息确认
    case process_offline_ack(CurrentUid, Type, MsgIds) of
        {ok, ProcessedCount} ->
            Payload =
                #{<<"msg">> => <<"offline_messages_acknowledged">>,
                  <<"type">> => Type,
                  <<"processed_count">> => ProcessedCount,
                  <<"msg_ids_count">> => length(MsgIds)},
            ok =
                ?INFO_LOG("Offline ack processed successfully: ~p messages for user: ~p",
                          [ProcessedCount, CurrentUid]),
            elib_response:success(Req0, Payload);
        {error, Reason} ->
            ok =
                ?ERROR_LOG("Failed to process offline_ack for user: ~p, reason: ~p",
                           [CurrentUid, Reason]),
            elib_response:error(Req0, Reason)
    end.

%% ===================================================================
%% 离线消息确认相关函数
%% ===================================================================

%% @doc 处理离线消息确认
%% 根据消息类型删除对应的离线消息
%%
%% @param Uid 用户ID
%% @param Type 消息类型（c2c/c2g/s2c）
%% @param MsgIds 消息ID列表
%% @return {ok, Count} 或 {error, Reason}
%% @end
-spec process_offline_ack(integer(), binary(), list()) -> {ok, integer()} | {error, binary()}.
process_offline_ack(Uid, Type, MsgIds) ->
    case Type of
        <<"c2c">> ->
            Count = msg_c2c_repo:delete_by_msg_ids_and_to_id(MsgIds, Uid),
            {ok, Count};
        <<"c2g">> ->
            Count = msg_c2g_timeline_repo:delete_by_msg_ids_and_to_id(MsgIds, Uid),
            {ok, Count};
        <<"s2c">> ->
            Count = msg_s2c_repo:delete_by_msg_ids_and_to_id(MsgIds, Uid),
            {ok, Count};
        _ ->
            {error, <<"unsupported_message_type">>}
    end.

%% @doc 处理群消息已读统计请求
%% 获取指定群消息的已读人数和总人数
%%
%% @param Req0 Cowboy请求对象，包含 msg_id 参数
%% @param State 状态映射，包含 current_uid
%% @return 返回已读统计信息
%% @end
-spec read_stats(cowboy_req:req(), map()) -> cowboy_req:req().
read_stats(Req0, State) ->
    % 获取查询参数
    MsgId = cowboy_req:qs_val(<<"msg_id">>, Req0, undefined),

    case MsgId of
        undefined ->
            % 参数错误
            elib_response:error(Req0, <<"缺少 msg_id 参数"/utf8>>, ?ERR_BAD_REQUEST);
        _ ->
            % 安全获取 current_uid
            case maps:get(current_uid, State, undefined) of
                undefined ->
                    elib_response:error(Req0, <<"未授权"/utf8>>, ?ERR_UNAUTHORIZED);
                CurrentUid ->
                    % 调用逻辑层处理
                    case msg_c2g_logic:read_stats(MsgId, CurrentUid) of
                        {ok, ReadCount, TotalCount} ->
                            Payload = #{
                                <<"read_count">> => ReadCount,
                                <<"total_count">> => TotalCount
                            },
                            elib_response:success(Req0, Payload);
                        {error, not_found} ->
                            elib_response:error(Req0, <<"消息不存在"/utf8>>, ?ERR_NOT_FOUND);
                        {error, permission_denied} ->
                            elib_response:error(Req0, <<"无权限访问该消息"/utf8>>, ?ERR_ACCESS_DENIED);
                        {error, Reason} ->
                            elib_response:error(Req0, Reason, ?ERR_INTERNAL_SERVER_ERROR)
                    end
            end
    end.

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
            {ok, Body} = elib_req:body(Req0, []),
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
            {ok, Body} = elib_req:body(Req0, []),
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
    case maps:get(current_uid, State, undefined) of
        undefined ->
            elib_response:error(Req0, <<"未授权"/utf8>>, ?ERR_UNAUTHORIZED);
        CurrentUid ->
            % 获取请求参数
            {ok, Body} = elib_req:body(Req0, []),
            MsgId = maps:get(<<"msg_id">>, Body, undefined),
            MsgType = maps:get(<<"msg_type">>, Body, <<"c2c">>),
            Emoji = maps:get(<<"emoji">>, Body, undefined),

            % 参数验证
            case {MsgId, Emoji} of
                {undefined, _} ->
                    elib_response:error(Req0, <<"缺少消息ID参数"/utf8>>, ?ERR_BAD_REQUEST);
                {_, undefined} ->
                    elib_response:error(Req0, <<"缺少emoji参数"/utf8>>, ?ERR_BAD_REQUEST);
                {_, <<>>} ->
                    elib_response:error(Req0, <<"emoji不能为空"/utf8>>, ?ERR_BAD_REQUEST);
                _ ->
                    % 调用逻辑层处理
                    case msg_reaction_logic:add(MsgId, MsgType, CurrentUid, Emoji) of
                        {ok, Result} ->
                            Payload = #{
                                <<"msg_id">> => MsgId,
                                <<"emoji">> => Emoji,
                                <<"user_id">> => maps:get(<<"user_id">>, Result),
                                <<"created_at">> => maps:get(<<"created_at">>, Result)
                            },
                            elib_response:success(Req0, Payload, <<"添加表情成功"/utf8>>);
                        {error, {invalid_param, Msg}} ->
                            elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
                        {error, msg_not_found} ->
                            elib_response:error(Req0, <<"消息不存在"/utf8>>, ?ERR_MESSAGE_NOT_FOUND);
                        {error, permission_denied} ->
                            elib_response:error(Req0, <<"无权限访问该消息"/utf8>>, ?ERR_ACCESS_DENIED);
                        {error, not_group_member} ->
                            elib_response:error(Req0, <<"不是群成员"/utf8>>, ?ERR_NOT_GROUP_MEMBER);
                        {error, Reason} ->
                            elib_response:error(Req0, Reason, ?ERR_INTERNAL_SERVER_ERROR)
                    end
            end
    end.

%% @doc 处理移除表情回应请求
%% 从指定消息移除表情
%%
%% @param Req0 Cowboy请求对象，包含消息ID和emoji
%% @param State 状态映射，包含 current_uid
%% @return 返回处理结果响应
%% @end
-spec reaction_remove(cowboy_req:req(), map()) -> cowboy_req:req().
reaction_remove(Req0, State) ->
    case maps:get(current_uid, State, undefined) of
        undefined ->
            elib_response:error(Req0, <<"未授权"/utf8>>, ?ERR_UNAUTHORIZED);
        CurrentUid ->
            % 获取请求参数
            {ok, Body} = elib_req:body(Req0, []),
            MsgId = maps:get(<<"msg_id">>, Body, undefined),
            MsgType = maps:get(<<"msg_type">>, Body, <<"c2c">>),
            Emoji = maps:get(<<"emoji">>, Body, undefined),

            % 参数验证
            case {MsgId, Emoji} of
                {undefined, _} ->
                    elib_response:error(Req0, <<"缺少消息ID参数"/utf8>>, ?ERR_BAD_REQUEST);
                {_, undefined} ->
                    elib_response:error(Req0, <<"缺少emoji参数"/utf8>>, ?ERR_BAD_REQUEST);
                {_, <<>>} ->
                    elib_response:error(Req0, <<"emoji不能为空"/utf8>>, ?ERR_BAD_REQUEST);
                _ ->
                    % 调用逻辑层处理
                    case msg_reaction_logic:remove(MsgId, MsgType, CurrentUid, Emoji) of
                        ok ->
                            Payload = #{
                                <<"msg_id">> => MsgId,
                                <<"emoji">> => Emoji
                            },
                            elib_response:success(Req0, Payload, <<"移除表情成功"/utf8>>);
                        {error, msg_not_found} ->
                            elib_response:error(Req0, <<"消息不存在"/utf8>>, ?ERR_MESSAGE_NOT_FOUND);
                        {error, Reason} ->
                            elib_response:error(Req0, Reason, ?ERR_INTERNAL_SERVER_ERROR)
                    end
            end
    end.

%% @doc 处理查询表情列表请求
%% 查询指定消息的所有表情
%%
%% @param Req0 Cowboy请求对象，包含消息ID
%% @param State 状态映射，包含 current_uid
%% @return 返回处理结果响应
%% @end
-spec reaction_list(cowboy_req:req(), map()) -> cowboy_req:req().
reaction_list(Req0, _State) ->
    % 获取查询参数
    MsgId = cowboy_req:qs_val(<<"msg_id">>, Req0, undefined),
    MsgType = cowboy_req:qs_val(<<"msg_type">>, Req0, <<"c2c">>),

    case MsgId of
        undefined ->
            elib_response:error(Req0, <<"缺少 msg_id 参数"/utf8>>, ?ERR_BAD_REQUEST);
        _ ->
            % 调用逻辑层处理
            case msg_reaction_logic:list(MsgId, MsgType) of
                {ok, Result} ->
                    elib_response:success(Req0, Result);
                {error, Reason} ->
                    elib_response:error(Req0, Reason, ?ERR_INTERNAL_SERVER_ERROR)
            end
    end.
