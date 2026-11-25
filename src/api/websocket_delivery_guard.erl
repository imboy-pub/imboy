-module(websocket_delivery_guard).
-moduledoc "Fresh discussion authorization and reference-fenced transport retries.".
-export([run/3, run/4, retry/6, cancel_ack/5]).
-include("log.hrl").

%% Keep the binary entry point and fresh authorization on every delivery. The
%% decoded value lives only for this callback; retries still carry the original
%% binary, so a later retry must authorize again.
run(Info, State, Continue, ContinueBinary) ->
    case delivery_binary(Info) of
        {ok, Bin} ->
            Uid = auth_ds:current_uid(State),
            Decoded =
                try
                    jsone:decode(Bin, [{object_format, map}])
                catch
                    _:DecodeErr ->
                        _ = ?WARN_LOG({delivery_decode_failed, DecodeErr}),
                        invalid_json
                end,
            Allowed =
                try
                    micro_community_access_logic:delivery_allowed(Uid, Decoded)
                catch
                    _:AllowErr ->
                        _ = ?WARN_LOG({delivery_allowed_check_failed, Uid, AllowErr}),
                        false
                end,
            case Allowed of
                true ->
                    %% BE-14：连接消费到 pull_offline_msg 触发帧（登录阈值或
                    %% 预算降级轻通知）即释放合并通知槽——客户端将拉取补齐，
                    %% 后续新滞留可再通知。授权在前、通知槽释放在后：
                    %% 被拒投递不清槽（未到达客户端）。
                    _ = maybe_release_backlog_notice(Decoded),
                    Fresh = refresh_quote(Decoded),
                    FreshInfo =
                        case Fresh =:= Decoded of
                            true -> Info;
                            false -> replace_delivery(Info, jsone:encode(Fresh, [native_utf8]))
                        end,
                    ContinueBinary(FreshInfo, State, Fresh);
                false ->
                    cleanup_denied_delivery(Info),
                    {ok, State, hibernate}
            end;
        none ->
            run(Info, State, Continue)
    end.

delivery_binary({reply, Bin}) when is_binary(Bin) -> {ok, Bin};
delivery_binary({timeout, _, {_, {_, _, _}, Bin}}) when is_binary(Bin) -> {ok, Bin};
delivery_binary({timeout, _, Bin}) when is_binary(Bin) -> {ok, Bin};
delivery_binary(_) -> none.

run(Info, State, Continue) ->
    case discussion_delivery_allowed(Info, State) of
        true ->
            Continue(refresh_map_delivery(Info), State);
        false ->
            cleanup_denied_delivery(Info),
            {ok, State, hibernate}
    end.

refresh_map_delivery({reply, Map} = Info) when is_map(Map) ->
    replace_delivery(Info, refresh_quote(Map));
refresh_map_delivery({timeout, _, {_, {_, _, _}, Map}} = Info) when is_map(Map) ->
    replace_delivery(Info, refresh_quote(Map));
refresh_map_delivery({timeout, _, Map} = Info) when is_map(Map) ->
    replace_delivery(Info, refresh_quote(Map));
refresh_map_delivery(Info) ->
    Info.

replace_delivery({reply, _}, New) ->
    {reply, New};
replace_delivery({timeout, Ref, {Delays, {_, _, _} = Key, _}}, New) ->
    {timeout, Ref, {Delays, Key, New}};
replace_delivery({timeout, Ref, _}, New) ->
    {timeout, Ref, New}.

refresh_quote(
    #{
        <<"type">> := <<"C2C">>,
        <<"msg_type">> := <<"quote">>,
        <<"id">> := Id,
        <<"from">> := From,
        <<"to">> := To,
        <<"payload">> := Payload
    } = Message
) ->
    Snapshot =
        case maps:get(<<"reply_source_id">>, Message, missing) of
            null ->
                null;
            _ ->
                try
                    Key = {Id, ec_cnv:to_integer(From), ec_cnv:to_integer(To)},
                    maps:get(Key, msg_c2c_reply_ds:visible_reply_snippets([Id]), null)
                catch
                    _:_ -> null
                end
        end,
    Source = msg_c2c_reply_ds:public_reply_source(<<"quote">>, Snapshot),
    FreshPayload = msg_c2c_reply_ds:redact_reply_payload(Payload, Snapshot),
    maps:merge(Message#{<<"payload">> := FreshPayload}, Source);
refresh_quote(Message) ->
    Message.

cleanup_denied_delivery({timeout, Ref, {_Delays, {Uid, Did, Id}, _Msg}}) ->
    _ = ack_retry_cache:delete_if_value({Uid, Did, Id}, Ref),
    ok;
cleanup_denied_delivery(_) ->
    ok.

discussion_delivery_allowed({reply, Msg}, State) ->
    micro_community_access_logic:delivery_allowed(auth_ds:current_uid(State), Msg);
discussion_delivery_allowed({timeout, _Ref, {_Delays, {_Uid, _Did, _Id}, Msg}}, State) ->
    micro_community_access_logic:delivery_allowed(auth_ds:current_uid(State), Msg);
discussion_delivery_allowed({timeout, _Ref, Msg}, State) ->
    micro_community_access_logic:delivery_allowed(auth_ds:current_uid(State), Msg);
discussion_delivery_allowed(_, _) ->
    true.

retry(Ref, MsLi, {Uid, DID, MsgId} = TimerKey, Msg, State, Encode) ->
    ok = ?DEBUG_LOG({timeout_retry, MsgId, Uid, DID}),
    %% 只有原子消费了当前 Ref 的 timeout 才能续排；旧回调不得覆盖新 timer。
    case ack_retry_cache:delete_if_value(TimerKey, Ref) of
        false ->
            ok = ?DEBUG_LOG({timeout_stale, MsgId, Uid, DID}),
            {ok, State, hibernate};
        true ->
            AckReceivedKey = {ack_received, Uid, DID, MsgId},
            case ack_retry_cache:get(AckReceivedKey) of
                {ok, true} ->
                    ok = ?DEBUG_LOG({timeout_ack_received, MsgId, Uid}),
                    {ok, State, hibernate};
                {ok, _} ->
                    ok = ?WARN_LOG({timeout_ack_flag_invalid, MsgId}),
                    %% 当前 timer 对应一次投递；MsLi 仅是后续重试。
                    %% 只续排而不回复会丢掉所有正延迟投递，含最终重试。
                    websocket_logic:send_next(Uid, MsgId, Msg, MsLi, [DID], true),
                    {reply, Encode(Msg, State), State, hibernate};
                undefined ->
                    ok = ?DEBUG_LOG({timeout_no_ack, MsgId, Uid}),
                    websocket_logic:send_next(Uid, MsgId, Msg, MsLi, [DID], true),
                    {reply, Encode(Msg, State), State, hibernate}
            end
    end.

cancel_ack(Uid, DID, MsgId, Timestamp, State) ->
    ok = ?DEBUG_LOG({ack_cancel_from_remote, MsgId, Uid, DID, Timestamp}),
    try
        websocket_logic:handle_ack_cancel(Uid, DID, MsgId)
    catch
        Class:Reason ->
            _ = ?WARN_LOG({ack_cancel_handle_failed, MsgId, Uid, DID, Class, Reason}),
            ok
    end,
    {ok, State, hibernate}.

%% BE-14：pull_offline_msg 触发帧的消费即合并通知槽释放（见 run/4 调用点
%% 注释）。仅识别解码成功的 map 帧；invalid_json 保持槽位不变。
maybe_release_backlog_notice(#{<<"action">> := <<"pull_offline_msg">>}) ->
    _ = imboy_syn:clear_backlog_notice(self()),
    ok;
maybe_release_backlog_notice(_) ->
    ok.
