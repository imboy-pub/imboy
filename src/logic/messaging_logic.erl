-module(messaging_logic).
-dialyzer({nowarn_function, [offline_ack/4]}).

-export([
    offline/6,
    offline_ack/4,
    read_stats/2,
    history/5,
    reaction_add/4,
    reaction_remove/4,
    reaction_list/2,
    route_ws/5
]).

%% 供 msg_c2s_logic:handle_sync 等模块复用的工具函数
-export([encode_history_msg/2, next_seq_from_rows/2]).

-include("error_code.hrl").
-include("log.hrl").

%% ARCH-01：本模块此前多个函数直接签名 cowboy_req:req() 并解析
%% cowboy_req:parse_qs/elib_req:body，越界承担了 handler 职责。
%% HTTP 参数解析与响应封装已上移至 msg_handler，本模块函数一律收纯参数、
%% 返回 {ok, Payload} | {ok, Payload, Msg} | {error, Reason} | {error, Reason, Code}。

-spec offline(
    integer(), non_neg_integer(), non_neg_integer(), non_neg_integer(), non_neg_integer(), binary()
) ->
    map().
offline(CurrentUid, Limit, C2CLastMsgAtInt, C2GLastMsgAtInt, S2CLastMsgAtInt, DID) ->
    %% 【P0-1】可选 did：客户端携带时 C2C/S2C 按设备过滤（排除本设备已确认的消息）；
    %% 缺省保持按 uid 的旧语义（旧客户端零破坏）
    C2CLastMsgAt = ms_to_since_ts(C2CLastMsgAtInt),
    C2GLastMsgAt = ms_to_since_ts(C2GLastMsgAtInt),
    S2CLastMsgAt = ms_to_since_ts(S2CLastMsgAtInt),

    CountC2CMsg = msg_c2c_ds:count_unread_since(CurrentUid, C2CLastMsgAt, DID),
    CountC2GMsg = get_c2g_msg_count(CurrentUid, C2GLastMsgAt),
    CountS2CMsg = msg_s2c_ds:count_since(CurrentUid, S2CLastMsgAt, DID),

    C2CMsgs = msg_c2c_ds:read_msg_for_device(CurrentUid, DID, Limit, C2CLastMsgAt),
    C2GMsgs = msg_c2g_ds:read_msg(CurrentUid, Limit, C2GLastMsgAt),
    S2CMsgs = msg_s2c_ds:read_msg_for_device(CurrentUid, DID, Limit, S2CLastMsgAt),

    ProcessedC2CMsgs = [process_message(Msg) || Msg <- C2CMsgs],
    ProcessedC2GMsgs = [process_message(Msg) || Msg <- C2GMsgs],
    ProcessedS2CMsgs = [process_message(Msg) || Msg <- S2CMsgs],

    #{
        <<"c2c">> =>
            #{
                <<"has_more">> => length(ProcessedC2CMsgs) < CountC2CMsg,
                <<"next_last_msg_at">> =>
                    calculate_next_last_msg_at(ProcessedC2CMsgs, C2CLastMsgAt),
                <<"total">> => CountC2CMsg,
                <<"list">> => ProcessedC2CMsgs
            },
        <<"c2g">> =>
            #{
                <<"has_more">> => length(ProcessedC2GMsgs) < CountC2GMsg,
                <<"next_last_msg_at">> =>
                    calculate_next_last_msg_at(ProcessedC2GMsgs, C2GLastMsgAt),
                <<"total">> => CountC2GMsg,
                <<"list">> => ProcessedC2GMsgs
            },
        <<"s2c">> =>
            #{
                <<"has_more">> => length(ProcessedS2CMsgs) < CountS2CMsg,
                <<"next_last_msg_at">> =>
                    calculate_next_last_msg_at(ProcessedS2CMsgs, S2CLastMsgAt),
                <<"total">> => CountS2CMsg,
                <<"list">> => ProcessedS2CMsgs
            }
    }.

-spec read_stats(integer() | binary(), integer()) ->
    {ok, map()} | {error, binary(), integer()}.
read_stats(MsgId, CurrentUid) ->
    case msg_c2g_logic:read_stats(MsgId, CurrentUid) of
        {ok, ReadCount, TotalCount} ->
            {ok, #{
                <<"read_count">> => ReadCount,
                <<"total_count">> => TotalCount
            }};
        {error, not_found} ->
            {error, <<"消息不存在"/utf8>>, ?ERR_NOT_FOUND};
        {error, permission_denied} ->
            {error, <<"无权限访问该消息"/utf8>>, ?ERR_ACCESS_DENIED};
        {error, Reason} ->
            {error, elib_cnv:safe_to_binary(Reason), ?ERR_INTERNAL_SERVER_ERROR}
    end.

%%-------------------------------------------------------------------
%% @doc  消息历史查询（基于 conv_seq 游标）
%%
%% 仅在 msg_archive_enabled=true 且已执行 00000075 DDL 时有效。
%%
%% 参数：
%%   ChatType   : "c2c" | "c2g"
%%   PeerIdEnc  : TSID 格式的对方 uid（C2C）或 group_id（C2G），编码态 binary
%%   AfterSeq   : 上次最后消息的 conv_seq（首次传 0）
%%   Limit      : 每次返回条数（调用方需先夹到 <=100）
%%
%% 返回：
%%   {ok, #{messages, next_seq, has_more, conv_key}} | {error, Reason, Code}
%% @end
%%-------------------------------------------------------------------
-spec history(integer(), binary(), binary(), non_neg_integer(), pos_integer()) ->
    {ok, map()} | {error, binary(), integer()}.
history(CurrentUid, ChatType, PeerIdEnc, AfterSeq, Limit) ->
    case validate_history_params(ChatType, PeerIdEnc, CurrentUid) of
        {error, Reason} ->
            {error, Reason, ?ERR_BAD_REQUEST};
        {ok, ConvKey} ->
            %% 多取 1 条判定 has_more：末页恰好满额（== Limit）时不再虚报
            %% true（旧判定 >= Limit 会让客户端多拉一次空页）
            case msg_archive_ds:history(ConvKey, AfterSeq, Limit + 1) of
                {ok, Rows0} ->
                    HasMore = length(Rows0) > Limit,
                    Rows = lists:sublist(Rows0, Limit),
                    Messages = [encode_history_msg(CurrentUid, Row) || Row <- Rows],
                    NextSeq = next_seq_from_rows(Rows, AfterSeq),
                    {ok, #{
                        <<"messages">> => Messages,
                        <<"next_seq">> => NextSeq,
                        <<"has_more">> => HasMore,
                        <<"conv_key">> => ConvKey
                    }};
                {error, _Reason} ->
                    {error, <<"消息历史暂不可用，请确认服务已开启 msg_archive_enabled"/utf8>>,
                        ?ERR_INTERNAL_SERVER_ERROR}
            end
    end.

%% @private 验证参数并生成 conv_key
validate_history_params(<<"c2c">>, PeerIdEnc, CurrentUid) when PeerIdEnc =/= <<>> ->
    PeerId = ec_cnv:to_integer(PeerIdEnc),
    {ok, msg_archive_ds:conv_key_c2c(CurrentUid, PeerId)};
validate_history_params(<<"c2g">>, PeerIdEnc, _CurrentUid) when PeerIdEnc =/= <<>> ->
    Gid = ec_cnv:to_integer(PeerIdEnc),
    {ok, msg_archive_ds:conv_key_c2g(Gid)};
validate_history_params(<<>>, _, _) ->
    {error, <<"缺少 chat_type 参数"/utf8>>};
validate_history_params(_, <<>>, _) ->
    {error, <<"缺少 peer_id 参数"/utf8>>};
validate_history_params(ChatType, _, _) ->
    {error, iolist_to_binary([<<"不支持的 chat_type: "/utf8>>, ChatType])}.

%% @private 编码历史消息（from_id/to_id → TSID）
encode_history_msg(_CurrentUid, Row) ->
    FromId = maps:get(<<"from_id">>, Row, undefined),
    ToId = maps:get(<<"to_id">>, Row, undefined),
    GroupId = maps:get(<<"group_id">>, Row, undefined),
    Row2 = maps:remove(<<"from_id">>, Row),
    Row3 = maps:remove(<<"to_id">>, Row2),
    Row4 = maps:remove(<<"group_id">>, Row3),
    Row5 =
        case FromId of
            undefined -> Row4;
            _ -> Row4#{<<"from">> => FromId}
        end,
    Row6 =
        case ToId of
            null -> Row5;
            undefined -> Row5;
            _ -> Row5#{<<"to">> => ToId}
        end,
    case GroupId of
        null -> Row6;
        undefined -> Row6;
        _ -> Row6#{<<"group_id">> => GroupId}
    end.

%% @private 从返回行中提取最大 conv_seq 作为 next_seq
next_seq_from_rows([], AfterSeq) ->
    AfterSeq;
next_seq_from_rows(Rows, _AfterSeq) ->
    LastRow = lists:last(Rows),
    maps:get(<<"conv_seq">>, LastRow, 0).

-spec offline_ack(integer(), binary(), list(), binary()) ->
    {ok, map()} | {error, binary()}.
offline_ack(CurrentUid, Type, MsgIds, DID) ->
    ok =
        ?INFO_LOG(
            "Processing offline_ack for user: ~p, type: ~p, msg_count: ~p, did: ~p",
            [CurrentUid, Type, length(MsgIds), DID]
        ),

    case process_offline_ack(CurrentUid, Type, MsgIds, DID) of
        {ok, ProcessedCount} ->
            Payload =
                #{
                    <<"msg">> => <<"offline_messages_acknowledged">>,
                    <<"type">> => Type,
                    <<"processed_count">> => ProcessedCount,
                    <<"msg_ids_count">> => length(MsgIds)
                },
            ok =
                ?INFO_LOG(
                    "Offline ack processed successfully: ~p messages for user: ~p",
                    [ProcessedCount, CurrentUid]
                ),
            {ok, Payload};
        {error, Reason} ->
            ok =
                ?ERROR_LOG(
                    "Failed to process offline_ack for user: ~p, reason: ~p",
                    [CurrentUid, Reason]
                ),
            {error, Reason}
    end.

-spec route_ws(binary(), integer(), map(), binary(), binary()) -> ok | {reply, map()}.
route_ws(MsgId, CurrentUid, Data, Type, OriginalMsg) ->
    message_router_logic:route(MsgId, CurrentUid, Data, Type, OriginalMsg).

-spec reaction_add(integer(), binary() | undefined, binary(), binary() | undefined) ->
    {ok, map(), binary()} | {error, binary(), integer()}.
reaction_add(CurrentUid, MsgId, MsgType, Emoji) ->
    case {MsgId, Emoji} of
        {undefined, _} ->
            {error, <<"缺少消息ID参数"/utf8>>, ?ERR_BAD_REQUEST};
        {_, undefined} ->
            {error, <<"缺少emoji参数"/utf8>>, ?ERR_BAD_REQUEST};
        {_, <<>>} ->
            {error, <<"emoji不能为空"/utf8>>, ?ERR_BAD_REQUEST};
        _ ->
            case msg_reaction_logic:add(MsgId, MsgType, CurrentUid, Emoji) of
                {ok, Result} ->
                    Payload = #{
                        <<"msg_id">> => MsgId,
                        <<"emoji">> => Emoji,
                        <<"user_id">> => maps:get(<<"user_id">>, Result),
                        <<"created_at">> => maps:get(<<"created_at">>, Result)
                    },
                    {ok, Payload, <<"添加表情成功"/utf8>>};
                {error, {invalid_param, Msg}} ->
                    {error, Msg, ?ERR_BAD_REQUEST};
                {error, msg_not_found} ->
                    {error, <<"消息不存在"/utf8>>, ?ERR_MESSAGE_NOT_FOUND};
                {error, permission_denied} ->
                    {error, <<"无权限访问该消息"/utf8>>, ?ERR_ACCESS_DENIED};
                {error, not_group_member} ->
                    {error, <<"不是群成员"/utf8>>, ?ERR_NOT_GROUP_MEMBER};
                {error, Reason} ->
                    {error, Reason, ?ERR_INTERNAL_SERVER_ERROR}
            end
    end.

-spec reaction_remove(integer(), binary() | undefined, binary(), binary() | undefined) ->
    {ok, map(), binary()} | {error, binary(), integer()}.
reaction_remove(CurrentUid, MsgId, MsgType, Emoji) ->
    case {MsgId, Emoji} of
        {undefined, _} ->
            {error, <<"缺少消息ID参数"/utf8>>, ?ERR_BAD_REQUEST};
        {_, undefined} ->
            {error, <<"缺少emoji参数"/utf8>>, ?ERR_BAD_REQUEST};
        {_, <<>>} ->
            {error, <<"emoji不能为空"/utf8>>, ?ERR_BAD_REQUEST};
        _ ->
            case msg_reaction_logic:remove(MsgId, MsgType, CurrentUid, Emoji) of
                ok ->
                    Payload = #{
                        <<"msg_id">> => MsgId,
                        <<"emoji">> => Emoji
                    },
                    {ok, Payload, <<"移除表情成功"/utf8>>};
                {error, msg_not_found} ->
                    {error, <<"消息不存在"/utf8>>, ?ERR_MESSAGE_NOT_FOUND};
                {error, Reason} ->
                    {error, elib_cnv:safe_to_binary(Reason), ?ERR_INTERNAL_SERVER_ERROR}
            end
    end.

-spec reaction_list(binary() | undefined, binary()) ->
    {ok, map()} | {error, binary(), integer()}.
reaction_list(undefined, _MsgType) ->
    {error, <<"缺少 msg_id 参数"/utf8>>, ?ERR_BAD_REQUEST};
reaction_list(MsgId, MsgType) ->
    case msg_reaction_logic:list(MsgId, MsgType) of
        {ok, Result} ->
            {ok, Result};
        {error, Reason} ->
            {error, Reason, ?ERR_INTERNAL_SERVER_ERROR}
    end.

-spec calculate_next_last_msg_at([map()], binary() | integer()) -> binary() | integer().
calculate_next_last_msg_at([], LastMsgAt) ->
    LastMsgAt;
calculate_next_last_msg_at(Msgs, _LastMsgAt) when length(Msgs) > 0 ->
    LastMsg = lists:last(Msgs),
    get_created_at(LastMsg).

-spec get_created_at(map()) -> binary() | integer().
get_created_at(Msg) when is_map(Msg) ->
    maps:get(<<"created_at">>, Msg, 0).

%% @doc 将毫秒时间戳转换为 DS 层可接受的时间参数
%%  0 → undefined（DS 层不加时间过滤，返回全量）
%%  非零 → RFC3339 binary（DS 层用 created_at >= $2 过滤）
-spec ms_to_since_ts(non_neg_integer()) -> binary() | undefined.
ms_to_since_ts(0) -> undefined;
ms_to_since_ts(Ms) -> elib_dt:to_rfc3339(Ms, millisecond).

-spec get_c2g_msg_count(integer(), binary() | undefined) -> integer().
get_c2g_msg_count(Uid, LastMsgAt) ->
    msg_c2g_ds:count_unread_timeline_since(Uid, LastMsgAt).

-spec process_message(map()) -> map().
process_message(Msg) when is_map(Msg) ->
    FromId = maps:get(<<"from_id">>, Msg, undefined),
    ToId = maps:get(<<"to_id">>, Msg, undefined),

    Msg2 = maps:remove(<<"from_id">>, Msg),
    Msg3 = maps:remove(<<"to_id">>, Msg2),

    Msg4 =
        case FromId of
            undefined ->
                Msg3;
            _ ->
                Msg3#{<<"from">> => FromId}
        end,

    case ToId of
        undefined ->
            Msg4;
        ToList when is_list(ToList) ->
            Msg4#{<<"to">> => ToList};
        _ ->
            Msg4#{<<"to">> => ToId}
    end.

-spec process_offline_ack(integer(), binary(), list(), binary()) ->
    {ok, integer()} | {error, binary()}.
process_offline_ack(Uid, <<"c2c">>, MsgIds, DID) when is_binary(DID), DID =/= <<>> ->
    _ = msg_operation_ds:ack_c2c_batch(MsgIds, Uid, DID),
    {ok, length(MsgIds)};
process_offline_ack(Uid, <<"s2c">>, MsgIds, DID) when is_binary(DID), DID =/= <<>> ->
    _ = msg_operation_ds:ack_s2c_batch(MsgIds, Uid, DID),
    {ok, length(MsgIds)};
process_offline_ack(Uid, Type, MsgIds, _DID) ->
    case Type of
        <<"c2c">> ->
            Count = msg_c2c_ds:delete_by_msg_ids_and_to_id(MsgIds, Uid),
            {ok, Count};
        <<"c2g">> ->
            Count = msg_c2g_ds:timeline_delete_by_msg_ids_and_to_id(MsgIds, Uid),
            {ok, Count};
        <<"s2c">> ->
            Count = msg_s2c_ds:delete_by_msg_ids_and_to_id(MsgIds, Uid),
            {ok, Count};
        _ ->
            {error, <<"unsupported_message_type">>}
    end.
