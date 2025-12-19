-module(msg_handler).
-behavior(cowboy_rest).

-export([init/2]).

%% 应用类型定义
-type message_list() :: [map() | list()].
-type req() :: cowboy_req:req().
-type state() :: map().
-type uid() :: binary() | integer().

-include_lib("imlib/include/log.hrl").

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
-spec init(req(), state()) -> {ok, req(), state()}.
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
-spec offline(req(), state()) -> {ok, req(), state()}.
offline(Req0, State) ->
    {ok, Limit} = imboy_param:int(limit, Req0, 1000),
    {ok, C2CLastMsgAtInt} = imboy_param:int(c2c_last_msg_at, Req0, 0),
    {ok, C2GLastMsgAtInt} = imboy_param:int(c2g_last_msg_at, Req0, 0),
    {ok, S2CLastMsgAtInt} = imboy_param:int(s2c_last_msg_at, Req0, 0),

    C2CLastMsgAt = erlang:list_to_binary(imboy_dt:to_rfc3339(C2CLastMsgAtInt, millisecond)),
    C2GLastMsgAt = erlang:list_to_binary(imboy_dt:to_rfc3339(C2GLastMsgAtInt, millisecond)),
    S2CLastMsgAt = erlang:list_to_binary(imboy_dt:to_rfc3339(S2CLastMsgAtInt, millisecond)),
    CurrentUid = maps:get(current_uid, State),

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
    Payload = #{
        <<"c2c">> => #{
            <<"has_more">> => length(ProcessedC2CMsgs) < CountC2CMsg,
            <<"next_last_msg_at">> => calculate_next_last_msg_at(ProcessedC2CMsgs, C2CLastMsgAt),
            <<"total">> => CountC2CMsg,
            <<"list">> => ProcessedC2CMsgs
        },
        <<"c2g">> => #{
            <<"has_more">> => length(ProcessedC2GMsgs) < CountC2GMsg,
            <<"next_last_msg_at">> => calculate_next_last_msg_at(ProcessedC2GMsgs, C2GLastMsgAt),
            <<"total">> => CountC2GMsg,
            <<"list">> => ProcessedC2GMsgs
        },
        <<"s2c">> => #{
            <<"has_more">> => length(ProcessedS2CMsgs) < CountS2CMsg,
            <<"next_last_msg_at">> => calculate_next_last_msg_at(ProcessedS2CMsgs, S2CLastMsgAt),
            <<"total">> => CountS2CMsg,
            <<"list">> => ProcessedS2CMsgs
        }
    },
    imboy_response:success(Req0, Payload).

%% @doc 计算下一页的 last_msg_at 参数
%% 根据当前页的消息列表计算出下一页的起始时间戳
%%
%% @param Msgs 当前页的消息列表
%% @param LastMsgAt 当前页的起始时间戳
%% @return 下一页的起始时间戳
%% @end
-spec calculate_next_last_msg_at(message_list(), binary() | integer()) -> binary() | integer().
calculate_next_last_msg_at([], LastMsgAt) -> LastMsgAt;
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
-spec get_created_at(map() | list()) -> binary() | integer().
get_created_at(Msg) when is_map(Msg) ->
    maps:get(<<"created_at">>, Msg, 0);
get_created_at(Msg) ->
    proplists:get_value(<<"created_at">>, Msg, 0).

%% @doc 获取C2C消息总数
%% 根据用户ID和最后消息时间戳计算消息总数
%%
%% @param Uid 用户ID
%% @param LastMsgAt 最后消息时间戳，undefined表示获取全部
%% @return 消息总数
%% @end
-spec get_c2c_msg_count(binary() | integer(), binary() | undefined) -> integer().
get_c2c_msg_count(Uid, undefined) ->
    msg_c2c_repo:count_by_to_id(Uid);
get_c2c_msg_count(Uid, LastMsgAt) ->
    Tb = msg_c2c_repo:tablename(),
    Where = <<"to_id = '", (ec_cnv:to_binary(Uid))/binary, "' AND created_at >= '", LastMsgAt/binary, "'">>,
    imboy_db:pluck(Tb, Where, <<"count(*) as count">>, 0).

%% @doc 获取C2G消息总数
%% 根据用户ID和最后消息时间戳计算群组消息总数
%%
%% @param Uid 用户ID
%% @param LastMsgAt 最后消息时间戳，undefined表示获取全部
%% @return 消息总数
%% @end
-spec get_c2g_msg_count(uid(), binary() | undefined) -> integer().
get_c2g_msg_count(Uid, undefined) ->
    msg_c2g_timeline_repo:count_by_to_id(Uid);
get_c2g_msg_count(Uid, LastMsgAt) ->
    Tb = msg_c2g_timeline_repo:tablename(),
    Where = <<"to_id = '", (ec_cnv:to_binary(Uid))/binary, "' AND client_ack = 0 AND created_at >= '", LastMsgAt/binary, "'">>,
    imboy_db:pluck(Tb, Where, <<"count(*) as count">>, 0).

%% @doc 获取S2C消息总数
%% 根据用户ID和最后消息时间戳计算系统消息总数
%%
%% @param Uid 用户ID
%% @param LastMsgAt 最后消息时间戳，undefined表示获取全部
%% @return 消息总数
%% @end
-spec get_s2c_msg_count(uid(), binary() | undefined) -> integer().
get_s2c_msg_count(Uid, undefined) ->
    msg_s2c_repo:count_by_to_id(Uid);
get_s2c_msg_count(Uid, LastMsgAt) ->
    Tb = msg_s2c_repo:tablename(),
    Where = <<"to_id = '", (ec_cnv:to_binary(Uid))/binary, "' AND created_at >= '", LastMsgAt/binary, "'">>,
    imboy_db:pluck(Tb, Where, <<"count(*) as count">>, 0).

% 处理单个消息：将 from_id 和 to_id 替换为编码后的 from 和 to
process_message(Msg) when is_list(Msg) ->
    % 对于 list 格式的消息（来自 c2c 和 s2c）
    FromId = proplists:get_value(<<"from_id">>, Msg),
    ToId = proplists:get_value(<<"to_id">>, Msg),

    % 删除原有的 from_id 和 to_id 字段
    Msg2 = proplists:delete(<<"from_id">>, Msg),
    Msg3 = proplists:delete(<<"to_id">>, Msg2),

    % 添加编码后的 from 和 to 字段
    FromField = case FromId of
        undefined -> [];
        _ -> [{<<"from">>, imboy_hashids:encode(FromId)}]
    end,
    ToField = case ToId of
        undefined -> [];
        _ -> [{<<"to">>, imboy_hashids:encode(ToId)}]
    end,

    FromField ++ ToField ++ Msg3;

process_message(Msg) when is_map(Msg) ->
    % 对于 map 格式的消息（来自 c2g）
    FromId = maps:get(<<"from_id">>, Msg, undefined),
    ToId = maps:get(<<"to_id">>, Msg, undefined),

    % 删除原有的 from_id 和 to_id 字段
    Msg2 = maps:remove(<<"from_id">>, Msg),
    Msg3 = maps:remove(<<"to_id">>, Msg2),

    % 添加编码后的 from 和 to 字段
    Msg4 = case FromId of
        undefined -> Msg3;
        _ -> Msg3#{<<"from">> => imboy_hashids:encode(FromId)}
    end,

    case ToId of
        undefined -> Msg4;
        ToList when is_list(ToList) ->
            % 对于群组消息，to_id 是一个列表
            ToEncoded = [imboy_hashids:encode(ToUid) || ToUid <- ToList],
            Msg4#{<<"to">> => ToEncoded};
        _ ->
            % 对于单个用户
            Msg4#{<<"to">> => imboy_hashids:encode(ToId)}
    end.

% 处理离线消息确认
offline_ack(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),

    % 获取请求参数
    PostVals = imboy_param:post(Req0),
    Type = string:lowercase(proplists:get_value(<<"type">>, PostVals, <<>>)),
    MsgIds = proplists:get_value(<<"msg_ids">>, PostVals, []),

    ?INFO_LOG("Processing offline_ack for user: ~p, type: ~p, msg_count: ~p", [CurrentUid, Type, length(MsgIds)]),

    % 处理离线消息确认
    case process_offline_ack(CurrentUid, Type, MsgIds) of
        {ok, ProcessedCount} ->
            Payload = #{
                <<"msg">> => <<"offline_messages_acknowledged">>,
                <<"type">> => Type,
                <<"processed_count">> => ProcessedCount,
                <<"msg_ids_count">> => length(MsgIds)
            },
            ?INFO_LOG("Offline ack processed successfully: ~p messages for user: ~p", [ProcessedCount, CurrentUid]),
            imboy_response:success(Req0, Payload);
        {error, Reason} ->
            ?ERROR_LOG("Failed to process offline_ack for user: ~p, reason: ~p", [CurrentUid, Reason]),
            imboy_response:error(Req0, Reason)
    end.

%% ===================================================================
%% 离线消息确认相关函数
%% ===================================================================

% 处理离线消息确认
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

