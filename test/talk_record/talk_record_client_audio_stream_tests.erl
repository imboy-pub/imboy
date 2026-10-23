-module(talk_record_client_audio_stream_tests).
-include_lib("eunit/include/eunit.hrl").

revocation_midstream_prevents_next_chunk_test() ->
    with_storage(fun() ->
        B = binding(131072),
        put(audio_guard_count, 0),
        put(audio_callback_count, 0),
        Guard = fun() ->
            N = get(audio_guard_count) + 1,
            put(audio_guard_count, N),
            case N =< 2 of
                true -> {ok, B};
                false -> {error, revoked}
            end
        end,
        F = fun(Bytes, N) ->
            put(audio_callback_count, get(audio_callback_count) + 1),
            N + byte_size(Bytes)
        end,
        ?assertEqual(denied(), talk_record_client_audio_stream:fold(80, {0, 131072}, Guard, F, 0)),
        ?assertEqual(1, get(audio_callback_count)),
        ?assertEqual(1, meck:num_calls(attachment_upload_spool, read_at, '_'))
    end).

guard_before_storage_and_invalid_ranges_test() ->
    with_storage(fun() ->
        F = fun(_, N) -> N end,
        ?assertEqual(
            denied(),
            talk_record_client_audio_stream:fold(80, {0, 100}, fun() -> {error, revoked} end, F, 0)
        ),
        ?assertEqual(
            denied(),
            talk_record_client_audio_stream:fold(80, {99, 2}, fun() -> {ok, binding(100)} end, F, 0)
        ),
        ?assertEqual(0, meck:num_calls(attachment_derived_artifact, with_range, '_'))
    end).

chunks_are_bounded_and_precise_test() ->
    with_storage(fun() ->
        B = binding(131073),
        F = fun(Bytes, N) ->
            ?assert(byte_size(Bytes) =< 65536),
            N + byte_size(Bytes)
        end,
        ?assertEqual(
            {ok, 131073},
            talk_record_client_audio_stream:fold(80, {0, 131073}, fun() -> {ok, B} end, F, 0)
        ),
        ?assertEqual(3, meck:num_calls(attachment_upload_spool, read_at, '_'))
    end).

changed_identity_or_callback_errors_are_sanitized_test() ->
    with_storage(fun() ->
        put(audio_guard_count, 0),
        Guard = fun() ->
            N = get(audio_guard_count) + 1,
            put(audio_guard_count, N),
            {ok, (binding(100))#{<<"attachment_hash">> => N}}
        end,
        ?assertEqual(
            denied(),
            talk_record_client_audio_stream:fold(80, {0, 100}, Guard, fun(_, N) -> N end, 0)
        ),
        ?assertEqual(0, meck:num_calls(attachment_upload_spool, read_at, '_')),
        ?assertEqual(
            denied(),
            talk_record_client_audio_stream:fold(
                80,
                {0, 100},
                fun() -> {ok, binding(100)} end,
                fun(_, _) -> error({secret, <<"synthetic-ticket">>}) end,
                0
            )
        )
    end).

with_storage(F) ->
    meck:new(attachment_derived_artifact, [passthrough, no_link]),
    meck:new(attachment_upload_spool, [passthrough, no_link]),
    try
        meck:expect(attachment_derived_artifact, with_range, fun(80, _, _, Callback) ->
            Callback(synthetic_spool)
        end),
        meck:expect(attachment_upload_spool, read_at, fun(synthetic_spool, _, N) ->
            {ok, binary:copy(<<0>>, N)}
        end),
        F()
    after
        meck:unload(attachment_derived_artifact),
        meck:unload(attachment_upload_spool),
        erase(audio_guard_count),
        erase(audio_callback_count)
    end.
binding(Size) ->
    #{
        <<"attachment_id">> => 80,
        <<"size">> => Size,
        <<"attachment_hash">> => binary:copy(<<"a">>, 64)
    }.
denied() -> {error, audio_source_denied}.
