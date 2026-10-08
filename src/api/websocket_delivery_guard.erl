-module(websocket_delivery_guard).
-moduledoc "Fresh discussion authorization and reference-fenced transport retries.".
-export([run/3, retry/6, cancel_ack/5]).
-include("log.hrl").

run(Info, State, Continue) ->
    case discussion_delivery_allowed(Info, State) of
        true ->
            Continue(Info, State);
        false ->
            cleanup_denied_delivery(Info),
            {ok, State, hibernate}
    end.

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
