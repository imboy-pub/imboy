-module(messaging_logic).
-dialyzer({nowarn_function, [offline_ack/4]}).

-export([
    offline/6,
    offline_ack/4,
    read_stats/2,
    history/5,
    history/6,
    reaction_add/4,
    reaction_remove/4,
    reaction_list/2,
    route_ws/5
]).

%% 供 msg_c2s_logic:handle_sync 等模块复用的工具函数
-export([encode_history_msg/2, next_seq_from_rows/2, process_message/1]).

%% E2EE per-device fan-out 信封过滤（发生率压降路径2）；纯判定函数导出供 eunit
-export([c2c_deliverable_to_device/3]).

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

    C2CMsgs0 = msg_c2c_ds:read_msg_for_device(CurrentUid, DID, Limit, C2CLastMsgAt),
    %% 路径2：per_device fan-out 信封里没有本机 DID 的收件消息不下发
    %% （密文构造时就不含该设备，客户端恢复密钥也永远解不开）；滤掉的同时
    %% 按 (uid, did) 标记已确认，pending_filter/count 随即排除，不会每轮重取
    C2CMsgs = filter_c2c_for_device(C2CMsgs0, DID, CurrentUid),
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
-spec history(integer(), binary(), binary(), non_neg_integer(), pos_integer(), binary()) ->
    {ok, map()} | {error, binary(), integer()}.
%% 兼容包装：不带 did 的旧语义（did=<<>> 时信封过滤 fail-open 原样下发）
history(CurrentUid, ChatType, PeerIdEnc, AfterSeq, Limit) ->
    history(CurrentUid, ChatType, PeerIdEnc, AfterSeq, Limit, <<>>).

history(CurrentUid, ChatType, PeerIdEnc, AfterSeq, Limit, DID) ->
    case validate_history_params(ChatType, PeerIdEnc, CurrentUid) of
        {error, permission_denied} ->
            {error, <<"无权限访问该群消息历史"/utf8>>, ?ERR_ACCESS_DENIED};
        {error, Reason} ->
            {error, Reason, ?ERR_BAD_REQUEST};
        {ok, ConvKey, MinSeq} ->
            %% E2EE-2026-012（Task 8/LT-03）：游标钳制到授权下界——
            %% seq=0 / 负数 / 越界游标都不能读穿 join boundary。
            AfterSeq2 = erlang:max(AfterSeq, MinSeq - 1),
            %% 多取 1 条判定 has_more：末页恰好满额（== Limit）时不再虚报
            %% true（旧判定 >= Limit 会让客户端多拉一次空页）
            case msg_archive_ds:history(ConvKey, AfterSeq2, Limit + 1) of
                {ok, Rows0} ->
                    HasMore = length(Rows0) > Limit,
                    Rows = lists:sublist(Rows0, Limit),
                    %% 路径2：per_device 信封无本机 DID 的收件行不下发。
                    %% next_seq 仍按**全量行**（含被滤行）推进——滤行位于游标
                    %% 之后，后续拉取天然不会再取到；C2G 行 e2ee 非 fan-out
                    %% 形状，判定函数对其恒 keep，无需按 chat_type 分流。
                    RowsDeliver = filter_c2c_history_for_device(Rows, DID, CurrentUid),
                    Messages = [encode_history_msg(CurrentUid, Row) || Row <- RowsDeliver],
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
    {ok, msg_archive_ds:conv_key_c2c(CurrentUid, PeerId), 0};
validate_history_params(<<"c2g">>, PeerIdEnc, CurrentUid) when PeerIdEnc =/= <<>> ->
    Gid = ec_cnv:to_integer(PeerIdEnc),
    %% E2EE-2026-012（Task 8/LT-03）：F2/R2 共享授权谓词——仅当前 open 世代
    %% start_seq 之后的历史可见；MinSeq 由 history/5 钳制游标（fail-closed deny）。
    case group_ds:authorize_group_history(CurrentUid, Gid) of
        {ok, #{start_seq := MinSeq}} ->
            {ok, msg_archive_ds:conv_key_c2g(Gid), MinSeq};
        {error, denied} ->
            {error, permission_denied}
    end;
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
    Row1 = Row#{
        <<"e2ee">> => decode_history_jsonb(maps:get(<<"e2ee">>, Row, null)),
        <<"payload">> => decode_history_jsonb(maps:get(<<"payload">>, Row, null))
    },
    Row2 = maps:remove(<<"from_id">>, Row1),
    Row3 = maps:remove(<<"to_id">>, Row2),
    Row4 = maps:remove(<<"group_id">>, Row3),
    Row5 =
        case FromId of
            undefined -> Row4;
            _ -> Row4#{<<"from">> => message_ds:envelope_id_to_binary(FromId)}
        end,
    Row6 =
        case ToId of
            null -> Row5;
            undefined -> Row5;
            _ -> Row5#{<<"to">> => message_ds:envelope_id_to_binary(ToId)}
        end,
    case GroupId of
        null -> Row6;
        undefined -> Row6;
        _ -> Row6#{<<"group_id">> => message_ds:envelope_id_to_binary(GroupId)}
    end.

%% epgsql returns jsonb columns as their JSON text representation. Decode them
%% before the HTTP response so history rows match live/offline message shapes.
decode_history_jsonb(Bin) when is_binary(Bin) ->
    try jsone:decode(Bin, [{object_format, map}]) of
        Value -> Value
    catch
        _:_ -> Bin
    end;
decode_history_jsonb(Value) ->
    Value.

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
                Msg3#{<<"from">> => message_ds:envelope_id_to_binary(FromId)}
        end,

    case ToId of
        undefined ->
            Msg4;
        ToList when is_list(ToList) ->
            Msg4#{<<"to">> => ToList};
        _ ->
            Msg4#{<<"to">> => message_ds:envelope_id_to_binary(ToId)}
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

%% ===================================================================
%% E2EE per-device fan-out 信封过滤（发生率压降路径2）
%% ===================================================================

%% @doc 下发前过滤：C2C per_device fan-out 信封里没有请求设备 DID 的
%% **收件**消息不下发——密文构造时就不含该设备的信封，客户端恢复密钥也
%% 永远解不开（典型：换设备后从服务端同步回来的单聊历史）。滤掉的行同时
%% 按 (uid, did) 标记已确认（msg_delivery），pending_filter 与按设备 count
%% 随即排除，不会每轮离线拉取重复取回。
%%
%% DID 缺省（旧客户端未带 did）时原样返回，行为与旧版一致（fail-open：
%% 客户端路径1会以边界文案兜底，不引导用户做无用恢复）。
%%
%% 方向语义：C2C fan-out 的 devices 只装**收件人**设备——from_id=当前用户
%% 的消息（自己发出的）信封里没有本机 DID 属预期，不得据此过滤。
-spec filter_c2c_for_device([map()], binary(), integer()) -> [map()].
filter_c2c_for_device(Msgs, DID, Uid) when is_binary(DID), DID =/= <<>> ->
    {KeepRev, DropIds} =
        lists:foldl(
            fun(Msg, {K, D}) ->
                case c2c_deliverable_to_device(Msg, DID, Uid) of
                    true ->
                        {[Msg | K], D};
                    false ->
                        case delivery_msg_id(Msg) of
                            %% 无法定位 msg_id：宁多勿漏，仍下发由客户端兜底
                            undefined -> {[Msg | K], D};
                            Mid -> {K, [Mid | D]}
                        end
                end
            end,
            {[], []},
            Msgs
        ),
    case DropIds of
        [] ->
            lists:reverse(KeepRev);
        _ ->
            _ = msg_delivery_repo:mark_acked_batch(<<"c2c">>, DropIds, Uid, DID),
            ok =
                ?INFO_LOG([e2ee_envelope_filtered, Uid, DID, length(DropIds)]),
            lists:reverse(KeepRev)
    end;
filter_c2c_for_device(Msgs, _DID, _Uid) ->
    Msgs.

%% @doc history 游标行的纯过滤（不标记 delivery：游标按全量行推进，
%% 滤行位于 next_seq 之后天然不会重取；history 走 msg_archive 只读层）
-spec filter_c2c_history_for_device([map()], binary(), integer()) -> [map()].
filter_c2c_history_for_device(Rows, DID, Uid) when is_binary(DID), DID =/= <<>> ->
    [Row || Row <- Rows, c2c_deliverable_to_device(Row, DID, Uid)];
filter_c2c_history_for_device(Rows, _DID, _Uid) ->
    Rows.

%% @doc 判定一条 C2C 消息对设备 DID 是否可交付（纯函数，供过滤与 eunit）。
%%
%% keep 条件（任一）：非加密消息；非 per_device fan-out（v1/v2 RSA、
%% Megolm 群聊等）；自己发出的（信封装的是对端设备）；per_device 且
%% devices 携带本机 DID。
%% drop 条件：per_device 且（devices 缺失 或 无本机 DID）且非本人发出。
-spec c2c_deliverable_to_device(map(), binary(), integer()) -> boolean().
c2c_deliverable_to_device(Msg, DID, Uid) when is_binary(DID), DID =/= <<>> ->
    case decode_e2ee_meta(maps:get(<<"e2ee">>, Msg, null)) of
        null ->
            true;
        E2ee when is_map(E2ee) ->
            case maps:get(<<"fan_out">>, E2ee, null) of
                <<"per_device">> ->
                    case is_sent_by_uid(Msg, Uid) of
                        true ->
                            true;
                        false ->
                            case maps:get(<<"devices">>, E2ee, null) of
                                Devices when is_map(Devices) ->
                                    maps:is_key(DID, Devices);
                                _ ->
                                    %% fan_out=per_device 却无 devices：
                                    %% 客户端同样判 unrecoverable
                                    %% （fan_out_missing_devices），不下发
                                    false
                            end
                    end;
                _ ->
                    true
            end;
        _ ->
            true
    end;
c2c_deliverable_to_device(_Msg, _DID, _Uid) ->
    true.

%% @private e2ee 元数据归一化：offline 行已解码为 map；msg_store.e2ee 为
%% JSON binary（history 游标行）；null/undefined 视为无元数据；坏 JSON /
%% 非对象（数组等）一律归 null（宁多勿漏 keep，客户端兜底）。
-spec decode_e2ee_meta(term()) -> map() | null.
decode_e2ee_meta(null) ->
    null;
decode_e2ee_meta(undefined) ->
    null;
decode_e2ee_meta(Meta) when is_map(Meta) ->
    Meta;
decode_e2ee_meta(Meta) when is_binary(Meta), Meta =/= <<>> ->
    try jsone:decode(Meta) of
        M when is_map(M) -> M;
        _ -> null
    catch
        _:_ -> null
    end;
decode_e2ee_meta(_) ->
    null.

%% @private from_id 与 Uid 比较：行形状在 offline（integer）与 msg_store
%% （可能 binary）间不一致，统一转整数再比。
-spec is_sent_by_uid(map(), integer()) -> boolean().
is_sent_by_uid(Msg, Uid) ->
    ec_cnv:to_integer(maps:get(<<"from_id">>, Msg, 0)) =:= Uid.

%% @private 交付标记定位 msg_id：优先 msg_id，回退 id；都没有返回
%% undefined（调用方按「宁多勿漏」仍下发）。
-spec delivery_msg_id(map()) -> binary() | undefined.
delivery_msg_id(Msg) ->
    case maps:get(<<"msg_id">>, Msg, undefined) of
        undefined -> maps:get(<<"id">>, Msg, undefined);
        Mid -> Mid
    end.
