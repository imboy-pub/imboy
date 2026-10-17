-module(moderation_business).
-export([
    inspect/3, inspect_channel/4, inspect_moment/2, inspect_profile/2, new_version/0, bind_version/2
]).

-include("log.hrl").

inspect_channel(ChannelId, MsgType, Content, Payload) ->
    case remote_enabled() of
        false ->
            moderation_policy:inspect(channel_message, Content);
        true ->
            case plaintext(MsgType, Payload) of
                false -> moderation_policy:inspect(channel_message, Content);
                {error, _} -> {error, <<"消息载荷或加密标记无效"/utf8>>};
                true -> inspect_public_channel(ChannelId, Content)
            end
    end.

inspect_public_channel(ChannelId, Content) ->
    case channel_ds:find_by_id(ChannelId, <<"visibility,access_type,scope,status">>) of
        #{<<"visibility">> := 0, <<"access_type">> := 0, <<"status">> := 1} = Channel ->
            Public = maps:get(<<"scope">>, Channel, <<"personal">>) =/= <<"workspace">>,
            inspect(channel_message, [{<<"content">>, Content}], Public);
        Channel when is_map(Channel) ->
            inspect(channel_message, [{<<"content">>, Content}], false);
        _ ->
            {error, <<"无法确认内容可见性"/utf8>>}
    end.

inspect_moment(Visibility, Content) ->
    inspect(moment_post, [{<<"content">>, Content}], Visibility =:= 0).

inspect_profile(Field, Value) ->
    inspect(profile_field, [{Field, Value}], true).

inspect(Surface, Fields, Public) ->
    case Public andalso remote_enabled() of
        false ->
            legacy(Surface, Fields);
        true ->
            RequestId = binary:encode_hex(crypto:strong_rand_bytes(16), lowercase),
            Detection = #{request_id => RequestId, scene => Surface, fields => Fields},
            Transport = moderation_detector_transport:select(),
            Result =
                case Transport of
                    moderation_detector_transport -> {error, not_ready};
                    _ -> Transport:check(Detection, #{request_id => RequestId})
                end,
            case Result of
                {ok, #{decision := allow}} ->
                    allow;
                {ok, #{decision := Decision, hits := Hits} = Response} ->
                    Trace = maps:with(
                        [
                            build_id,
                            ruleset_revision,
                            generation,
                            semantics_id,
                            base_rules_digest,
                            normalization_id,
                            hits_truncated,
                            scan_complete,
                            fields_checked
                        ],
                        Response
                    ),
                    CheckedHits = [Hit#{trace => Trace} || Hit <- Hits],
                    case Decision of
                        block -> {blocked, CheckedHits};
                        review -> {queued, CheckedHits}
                    end;
                {error, Reason} ->
                    failure(Reason)
            end
    end.

remote_enabled() ->
    Conf = config_ds:env(moderation_detector, []),
    proplists:get_value(mode, Conf, legacy) =:= rust.

plaintext(<<"text">>, Payload) when is_map(Payload) ->
    case
        lists:any(
            fun(Key) -> maps:is_key(Key, Payload) end,
            [<<"ciphertext">>, <<"e2ee">>, <<"encrypted">>]
        )
    of
        true -> {error, unsupported_encryption};
        false -> true
    end;
plaintext(<<"text">>, Payload) when is_binary(Payload) ->
    try jsone:decode(Payload) of
        Decoded when is_map(Decoded) -> plaintext(<<"text">>, Decoded);
        _ -> {error, invalid_payload}
    catch
        _:_ -> {error, invalid_payload}
    end;
plaintext(<<"text">>, _) ->
    {error, invalid_payload};
plaintext(_, _) ->
    false.

new_version() -> binary:encode_hex(crypto:strong_rand_bytes(16), lowercase).

bind_version({queued, Hits}, Version) ->
    {queued, [Hit#{content_version => Version} || Hit <- Hits]};
bind_version(Verdict, _) ->
    Verdict.

legacy(Surface, Fields) ->
    Results = [moderation_policy:inspect(Surface, Text) || {_, Text} <- Fields],
    Blocked = lists:append([Hits || {blocked, Hits} <- Results]),
    Reviewed = lists:append([Hits || {queued, Hits} <- Results]),
    case {Blocked, Reviewed} of
        {[], []} -> allow;
        {[], Hits} -> {queued, Hits};
        {Hits, _} -> {blocked, Hits ++ Reviewed}
    end.

failure(Reason) ->
    Facility = lists:member(Reason, [overloaded, not_ready, connection_failed, timeout]),
    Conf = config_ds:env(moderation_detector, []),
    case
        Facility andalso
            proplists:get_value(failure_policy, Conf, availability_compat) =:= availability_compat
    of
        true ->
            ?ERROR_LOG(["textguard_unchecked_degraded", Reason]),
            {unchecked_degraded, Reason};
        false ->
            case Facility of
                true ->
                    {error, <<"内容检查暂不可用，请稍后重试"/utf8>>};
                false ->
                    ?ERROR_LOG(["textguard_check_rejected", Reason]),
                    {error, <<"内容检查未完成，无法发布"/utf8>>}
            end
    end.
