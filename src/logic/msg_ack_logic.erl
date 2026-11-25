-module(msg_ack_logic).
%%%
% msg_ack_logic 是消息确认处理逻辑模块
% 统一处理 C2C、C2G、S2C、C2S 的客户端确认 (CLIENT_ACK)
%%%

-include("log.hrl").

-export([client_ack/4]).

%% ===================================================================
%% API
%% ===================================================================

%% @doc 统一的客户端 ACK 处理
%% @param Type 消息类型 (<<"c2c">>, <<"c2g">>, <<"s2c">>, <<"c2s">>)
%% @param MsgId 消息ID
%% @param CurrentUid 当前用户ID
%% @param DID 设备ID
-spec client_ack(binary(), binary(), integer(), binary()) -> ok | {error, binary()}.
client_ack(Type, MsgId, CurrentUid, DID) ->
    ok = ?DEBUG_LOG({unified_ack, Type, MsgId, CurrentUid, DID}),

    % 根据类型执行相应的 ACK 处理 - 使用 DS 层接口
    % 【P0-1】C2C/S2C 按设备送达：DID 有效时只标记该设备已确认，
    % 主行等全部活跃设备确认后才删，避免"一端 ACK、另一端离线永丢"。
    % C2G timeline 仍为 per-uid 标记（V7 多端未读串扰另行立项）。
    Timing = delivery_timing(Type, MsgId, CurrentUid, DID),
    AckResult =
        case Type of
            <<"c2c">> -> msg_operation_ds:ack_c2c_msg(MsgId, CurrentUid, DID);
            <<"c2g">> -> msg_operation_ds:ack_c2g_timeline(MsgId, CurrentUid);
            <<"s2c">> -> msg_operation_ds:ack_s2c_msg(MsgId, CurrentUid, DID);
            <<"c2s">> -> msg_operation_ds:ack_c2s_msg(MsgId, CurrentUid);
            _ -> ok = ?ERROR_LOG({unknown_msg_type_for_ack, Type})
        end,

    %% Duplicate {ok,0} and storage failure must not increment delivery metrics.
    %% ACK must never unstage: msg_store_worker owns durable staging cleanup.
    case AckResult of
        {error, Reason} ->
            {error, Reason};
        {ok, 0} ->
            ok;
        _ ->
            elib_metric:increment(msg_delivered_total),
            record_deliver_duration(AckResult, Timing),
            %% V3-24 G-4：企业托管行送达事件（按 origin 归属解析，Human 消息
            %% 零外发；独立小事务，失败只留痕不阻断 ACK 主链）。
            emit_enterprise_delivered(Type, MsgId),
            ok
    end.

%% 企业托管消息 delivered 归属：只有 c2c/c2g 是企业代发承载表（s2c/c2s
%% 系统通道不查不外发）。内部调用的独立小事务 fail-safe（恒 ok）。
-spec emit_enterprise_delivered(binary(), binary()) -> ok.
emit_enterprise_delivered(<<"c2c">>, MsgId) ->
    enterprise_webhook_logic:emit_delivered_for(<<"msg_c2c">>, MsgId);
emit_enterprise_delivered(<<"c2g">>, MsgId) ->
    enterprise_webhook_logic:emit_delivered_for(<<"msg_c2g">>, MsgId);
emit_enterprise_delivered(_Type, _MsgId) ->
    ok.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% ACK can remove the delivery row, so read the immutable acceptance time first.
delivery_timing(Type, MsgId, Uid, DID) when
    is_binary(MsgId) andalso
        (Type =:= <<"c2g">> orelse
            ((Type =:= <<"c2c">> orelse Type =:= <<"s2c">>) andalso is_binary(DID) andalso
                DID =/= <<>>))
->
    try
        AckMs = elib_dt:millisecond(),
        AcceptedMs =
            case trusted_time(Type, Uid, msg_store_repo:find_by_msg_id(Type, MsgId)) of
                undefined -> trusted_time(Type, Uid, accepted_delivery_row(Type, MsgId, Uid));
                StagedMs -> StagedMs
            end,
        {AckMs, AcceptedMs}
    catch
        %% A metrics read must never prevent the ACK from being persisted.
        _:_ -> undefined
    end;
delivery_timing(_Type, _MsgId, _Uid, _DID) ->
    undefined.

accepted_delivery_row(<<"c2c">>, MsgId, Uid) ->
    msg_c2c_repo:read_msg(
        <<"msg_id = $1 AND to_id = $2">>, <<"to_id, accepted_at">>, 1, [MsgId, Uid]
    );
accepted_delivery_row(<<"c2g">>, MsgId, _Uid) ->
    msg_c2g_repo:list_by_ids([MsgId], <<"accepted_at">>);
accepted_delivery_row(<<"s2c">>, _MsgId, _Uid) ->
    {error, not_found}.

trusted_time(Type, Uid, {ok, [Row]}) ->
    trusted_time(Type, Uid, {ok, Row});
trusted_time(Type, Uid, {ok, Row}) when is_map(Row) ->
    case Type =:= <<"c2g">> orelse maps:get(<<"to_id">>, Row, undefined) =:= Uid of
        true ->
            case msg_store_repo:resolve_accepted_ms(Row) of
                {ok, Ms, trusted} when Ms > 0 -> Ms;
                _ -> undefined
            end;
        false ->
            undefined
    end;
trusted_time(_Type, _Uid, _Result) ->
    undefined.

%% Legacy ACKs only return ok; they cannot prove that a new delivery was recorded.
record_deliver_duration({ok, Count}, {AckMs, AcceptedMs}) when
    is_integer(Count), Count > 0, is_integer(AcceptedMs)
->
    DeltaMs = AckMs - AcceptedMs,
    case DeltaMs >= 0 andalso DeltaMs =< 3600000 of
        true -> elib_metric:record(imboy_msg_deliver_duration_seconds, DeltaMs / 1000);
        false -> ok
    end;
record_deliver_duration(_AckResult, _Timing) ->
    ok.
