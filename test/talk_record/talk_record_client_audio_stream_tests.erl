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

%% Real Cowboy/httpc HTTP semantics; only source metadata is synthetic.
%% No PostgreSQL, attachment reads or provider requests occur in this test.
head_http_preserves_length_without_reading_audio_test_() ->
    {timeout, 20, fun head_http_preserves_length_without_reading_audio/0}.

head_http_preserves_length_without_reading_audio() ->
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = application:ensure_all_started(inets),
    meck:new(talk_record_client_audio_logic, [passthrough, no_link]),
    try
        M = #{
            size => 100,
            headers => #{
                <<"content-type">> => <<"audio/aac">>,
                <<"cache-control">> => <<"private, no-store">>
            }
        },
        meck:expect(
            talk_record_client_audio_logic,
            metadata,
            fun(<<"synthetic-head-ticket">>) -> {ok, M} end
        ),
        meck:expect(
            talk_record_client_audio_logic,
            fold,
            fun(_, _, _, _) -> error(head_must_not_read_audio) end
        ),
        Listener = {?MODULE, make_ref()},
        Dispatch = cowboy_router:compile([
            {'_', [
                {"/synthetic-audio", talk_record_client_audio_handler, #{}}
            ]}
        ]),
        {ok, _} = cowboy:start_clear(
            Listener,
            [{ip, {127, 0, 0, 1}}, {port, 0}],
            #{env => #{dispatch => Dispatch}}
        ),
        try
            URL =
                "http://127.0.0.1:" ++ integer_to_list(ranch:get_port(Listener)) ++
                    "/synthetic-audio?ticket=synthetic-head-ticket",
            {ok, {{_, 200, _}, H, <<>>}} = httpc:request(
                head,
                {URL, []},
                [{autoredirect, false}, {timeout, 5000}],
                [{body_format, binary}]
            ),
            ?assertEqual("100", proplists:get_value("content-length", H)),
            ?assertEqual("audio/aac", proplists:get_value("content-type", H)),
            ?assertEqual("private, no-store", proplists:get_value("cache-control", H)),
            {ok, {{_, 206, _}, RH, <<>>}} = httpc:request(
                head,
                {URL, [{"range", "bytes=7-42"}]},
                [{autoredirect, false}, {timeout, 5000}],
                [{body_format, binary}]
            ),
            ?assertEqual("36", proplists:get_value("content-length", RH)),
            ?assertEqual("bytes 7-42/100", proplists:get_value("content-range", RH)),
            ?assertEqual(4, meck:num_calls(talk_record_client_audio_logic, metadata, '_')),
            ?assertEqual(0, meck:num_calls(talk_record_client_audio_logic, fold, '_'))
        after
            ok = cowboy:stop_listener(Listener)
        end
    after
        meck:unload(talk_record_client_audio_logic)
    end.
