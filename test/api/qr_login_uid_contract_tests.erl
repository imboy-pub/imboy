%%% QR UID 契约：真实 handler/JSON 编码，隔离存储与传输 mock，不启动应用或 DB。
%%% QR UID wire contract through the real handlers and JSON serialization.
%%% Scoped mocks model storage/transport boundaries; no application or DB boot.
%%% The atomic PG mock checks handler routing, not PostgreSQL row-lock behavior.
-module(qr_login_uid_contract_tests).
-include_lib("eunit/include/eunit.hrl").
-include("error_code.hrl").

-define(UID, 111174215241304065).
-define(TOKEN, <<"synthetic-qr-contract-token">>).
-define(MOCKS, [cowboy_req, imboy_cache, qr_login_session_repo,
    qr_login_logic, token_ds, syn]).

confirmed_payload_preserves_exact_identity_test() ->
    {ok, Payload} = qr_login_event_ds:confirmed(?TOKEN, ?UID),
    ?assertEqual(expected_payload(), Payload),
    ?assertEqual(Payload, jsone:decode(jsone:encode(Payload))),
    {ok, Largest} = qr_login_event_ds:confirmed(?TOKEN, 9223372036854775807),
    ?assertEqual(<<"9223372036854775807">>, maps:get(<<"uid_str">>, Largest)).

confirmed_payload_rejects_invalid_identity_or_token_test() ->
    lists:foreach(fun(Uid) ->
        ?assertEqual({error, invalid_credentials}, qr_login_event_ds:confirmed(?TOKEN, Uid))
    end, [undefined, null, 0, -1, 1.5, <<"123">>, 9223372036854775808]),
    lists:foreach(fun(Token) ->
        ?assertEqual({error, invalid_credentials}, qr_login_event_ds:confirmed(Token, ?UID))
    end, [undefined, null, <<>>, 123]).

handler_contract_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun(C) -> ?_test(pg_sse_uses_claim_identity(C)) end,
        fun(C) -> ?_test(pg_polling_uses_claim_identity(C)) end,
        fun(C) -> ?_test(pg_sse_then_polling_cannot_reclaim(C)) end,
        fun(C) -> ?_test(pg_polling_then_sse_cannot_reclaim(C)) end,
        fun(C) -> ?_test(pg_duplicate_signal_cannot_reclaim(C)) end,
        fun(C) -> ?_test(pg_sse_and_polling_compete_for_one_claim(C)) end,
        fun(C) -> ?_test(disconnect_before_claim_allows_polling(C)) end,
        fun(C) -> ?_test(disconnect_after_claim_does_not_reissue(C)) end,
        fun(C) -> ?_test(cache_confirmation_carries_same_session_identity(C)) end,
        fun(C) -> ?_test(cache_polling_has_exact_identity_and_sequential_consumption(C)) end,
        fun(C) -> ?_test(invalid_pg_identity_never_emits_credentials(C)) end,
        fun(C) -> ?_test(invalid_cache_identity_never_emits_credentials(C)) end,
        fun(_C) -> ?_test(legacy_token_only_sse_fails_closed()) end
    ]}.

pg_sse_uses_claim_identity(#{table := Table}) ->
    ST = <<"pg-sse">>,
    put_claim(Table, ST, ?UID),
    %% Signal metadata must never override the identity returned by claim.
    Event = #{<<"status">> => <<"confirmed_ready">>,
        <<"token">> => <<"untrusted-signal-token">>, <<"uid">> => 999},
    ?assertMatch({stop, _, _}, qr_login_sse_handler:info(Event, #{}, #{session_token => ST})),
    ?assertEqual([expected_payload()], sse_payloads()),
    ?assertEqual(1, meck:num_calls(qr_login_session_repo, claim_login_token, [ST])).

pg_polling_uses_claim_identity(#{table := Table}) ->
    ST = <<"pg-poll">>,
    put_claim(Table, ST, ?UID),
    %% The find mock deliberately returns another UID; only claim is authoritative.
    Response = poll(ST),
    ?assertEqual(0, maps:get(<<"code">>, Response)),
    ?assertEqual(expected_payload(), maps:get(<<"payload">>, Response)).

pg_sse_then_polling_cannot_reclaim(#{table := Table}) ->
    ST = <<"pg-sse-first">>,
    put_claim(Table, ST, ?UID),
    _ = ready(ST),
    ?assertEqual([expected_payload()], sse_payloads()),
    assert_poll_error(?ERR_QR_LOGIN_ALREADY_USED, poll(ST)).

pg_polling_then_sse_cannot_reclaim(#{table := Table}) ->
    ST = <<"pg-poll-first">>,
    put_claim(Table, ST, ?UID),
    ?assertEqual(expected_payload(), maps:get(<<"payload">>, poll(ST))),
    _ = ready(ST),
    ?assertEqual([], sse_payloads()),
    ?assertEqual([<<": claimed_elsewhere\n\n">>], sent_chunks()).

pg_duplicate_signal_cannot_reclaim(#{table := Table}) ->
    ST = <<"pg-repeat">>,
    put_claim(Table, ST, ?UID),
    _ = ready(ST),
    _ = ready(ST),
    ?assertEqual([expected_payload()], sse_payloads()),
    ?assertEqual(<<": claimed_elsewhere\n\n">>, lists:last(sent_chunks())).

pg_sse_and_polling_compete_for_one_claim(#{table := Table}) ->
    ST = <<"pg-race">>,
    put_claim(Table, ST, ?UID),
    Parent = self(),
    Ref = make_ref(),
    Pids = [spawn_link(fun() ->
        receive {go, Ref} -> Parent ! {Ref, self(), Fun()} end
    end) || Fun <- [fun() -> ready(ST) end, fun() -> poll(ST) end]],
    [Pid ! {go, Ref} || Pid <- Pids],
    Results = [receive {Ref, Pid, Result} -> Result
        after 2000 -> error(claim_race_timeout) end || Pid <- Pids],
    PollPayloads = [Payload || #{<<"code">> := 0, <<"payload">> := Payload} <- Results],
    ?assertEqual([expected_payload()], sse_payloads() ++ PollPayloads),
    assert_poll_error(?ERR_QR_LOGIN_ALREADY_USED, poll(ST)).

disconnect_before_claim_allows_polling(#{table := Table}) ->
    ST = <<"pg-disconnect-before">>,
    put_claim(Table, ST, ?UID),
    {cowboy_loop, Req, State} = qr_login_sse_handler:init(
        #{qs => [{<<"session_token">>, ST}]}, []),
    ok = qr_login_sse_handler:terminate(normal, Req, State),
    ?assertEqual(0, meck:num_calls(qr_login_session_repo, claim_login_token, [ST])),
    ?assertEqual(expected_payload(), maps:get(<<"payload">>, poll(ST))),
    assert_poll_error(?ERR_QR_LOGIN_ALREADY_USED, poll(ST)).

disconnect_after_claim_does_not_reissue(#{table := Table}) ->
    ST = <<"pg-disconnect-after">>,
    put_claim(Table, ST, ?UID),
    meck:expect(cowboy_req, stream_body, fun(Body, Fin, Req) ->
        case meck_helper:is_resident_caller() of
            true -> meck:passthrough([Body, Fin, Req]);
            false -> error(disconnected)
        end
    end),
    ?assertError(disconnected, ready(ST)),
    assert_poll_error(?ERR_QR_LOGIN_ALREADY_USED, poll(ST)).

cache_confirmation_carries_same_session_identity(#{table := Table}) ->
    application:set_env(imboy, qr_login_authority, cache),
    ST = <<"cache-confirm">>,
    QT = <<"opaque-cache-qr">>,
    ets:insert(Table, [{{qr_login_qr, QT}, ST}, {{qr_login, ST}, #{
        <<"status">> => <<"scanned">>, <<"scanned_by">> => ?UID,
        <<"expires_at">> => erlang:system_time(millisecond) + 60000,
        <<"device_id">> => <<"web-device">>, <<"device_name">> => <<"Browser">>,
        <<"platform">> => <<"web">>, <<"purpose">> => <<"human">>
    }}]),
    %% Ignore any caller-supplied UID: authenticated scanner and stored owner match.
    Body = jsone:encode(#{<<"qr_token">> => QT, <<"uid">> => 999}),
    {stop, Req, _} = qr_login_handler:handle_request(
        #{method => <<"POST">>, body => Body}, #{action => confirm, current_uid => ?UID}),
    ?assertEqual(0, maps:get(<<"code">>, jsone:decode(maps:get(response_body, Req)))),
    [Event] = [E || {_, {syn, publish, [_, {qr_login, ST0}, E]}, _} <- meck:history(syn),
        ST0 =:= ST],
    ?assertEqual(expected_payload(), Event),
    _ = qr_login_sse_handler:info(Event, #{}, #{session_token => ST}),
    ?assertEqual([expected_payload()], sse_payloads()),
    ?assert(meck:called(token_ds, encrypt_token, [?UID, <<"web-device">>])).

cache_polling_has_exact_identity_and_sequential_consumption(#{table := Table}) ->
    application:set_env(imboy, qr_login_authority, cache),
    ST = <<"cache-poll">>,
    put_cache_confirmed(Table, ST, ?UID),
    ?assertEqual(expected_payload(), maps:get(<<"payload">>, poll(ST))),
    assert_poll_error(?ERR_NOT_FOUND, poll(ST)).

invalid_pg_identity_never_emits_credentials(#{table := Table}) ->
    lists:foreach(fun(Uid) ->
        ST = <<"invalid-pg">>,
        put_claim(Table, ST, Uid),
        _ = ready(ST),
        put_claim(Table, ST, Uid),
        assert_poll_error(?ERR_QR_LOGIN_ALREADY_USED, poll(ST))
    end, [0, undefined, null, -1, <<"123">>]),
    ?assertEqual([], sse_payloads()),
    ets:insert(Table, {{claim, <<"missing-uid">>}, #{login_token => ?TOKEN}}),
    _ = ready(<<"missing-uid">>),
    ?assertEqual([], sse_payloads()).

invalid_cache_identity_never_emits_credentials(#{table := Table}) ->
    application:set_env(imboy, qr_login_authority, cache),
    ST = <<"invalid-cache">>,
    put_cache_confirmed(Table, ST, undefined),
    assert_poll_error(?ERR_QR_LOGIN_ALREADY_USED, poll(ST)),
    _ = qr_login_sse_handler:info(
        #{<<"status">> => <<"confirmed">>, <<"token">> => ?TOKEN, <<"uid">> => 0},
        #{}, #{session_token => ST}),
    ?assertEqual([], sse_payloads()).

legacy_token_only_sse_fails_closed() ->
    _ = qr_login_sse_handler:info(
        #{<<"status">> => <<"confirmed">>, <<"token">> => ?TOKEN},
        #{}, #{session_token => <<"legacy">>}),
    ?assertEqual([<<": invalid_credentials\n\n">>], sent_chunks()),
    ?assertEqual([], sse_payloads()).

setup() ->
    Authority = application:get_env(imboy, qr_login_authority),
    application:set_env(imboy, qr_login_authority, pg),
    Table = ets:new(?MODULE, [set, public]),
    %% Reuse the shared resident-caller guard, but do not boot unrelated services.
    Mocks = [
        {cowboy_req, [
            {method, 1, fun(Req) -> maps:get(method, Req, <<"GET">>) end},
            {parse_qs, 1, fun(Req) -> maps:get(qs, Req, []) end},
            {read_body, 1, fun(Req) -> {ok, maps:get(body, Req), Req} end},
            {reply, 4, fun(Status, Headers, Body, Req) ->
                Req#{response_status => Status, response_headers => Headers, response_body => Body}
            end},
            {stream_reply, 3, fun(_, _, Req) -> Req end},
            {stream_body, 3, fun(_, _, _) -> ok end}
        ]},
        {qr_login_session_repo, [
            {find_by_session_token, 1, fun(_) ->
                #{<<"status">> => <<"confirmed">>, <<"scanned_by">> => 999}
            end},
            {claim_login_token, 1, fun(ST) ->
                case ets:take(Table, {claim, ST}) of
                    [{_, Claim}] -> {ok, Claim};
                    [] -> {error, not_claimable}
                end
            end}
        ]},
        {imboy_cache, [
            {get, 1, fun(Key) ->
                case ets:lookup(Table, Key) of [{_, Value}] -> {ok, Value}; [] -> undefined end
            end},
            {set, 3, fun(Key, Value, 60) -> true = ets:insert(Table, {Key, Value}), ok end},
            {delete, 1, fun(Key) -> true = ets:delete(Table, Key), ok end}
        ]},
        {token_ds, [{encrypt_token, 2, fun(?UID, <<"web-device">>) -> ?TOKEN end}]},
        {qr_login_logic, [{log_device_login, 4, fun(_, _, _, _) -> ok end}]},
        {syn, [
            {publish, 3, fun(_, _, _) -> {ok, 1} end},
            {join, 3, fun(_, _, _) -> ok end},
            {leave, 3, fun(_, _, _) -> ok end}
        ]}
    ],
    lists:foreach(fun({Mod, Expectations}) ->
        {ok, Mod} = meck_helper:setup_mock(Mod, Expectations)
    end, Mocks),
    #{table => Table, authority => Authority}.

cleanup(#{table := Table, authority := Authority}) ->
    lists:foreach(fun meck_helper:cleanup_mock/1, ?MOCKS),
    ets:delete(Table),
    case Authority of
        undefined -> application:unset_env(imboy, qr_login_authority);
        {ok, Value} -> application:set_env(imboy, qr_login_authority, Value)
    end.

put_claim(Table, ST, Uid) ->
    ets:insert(Table, {{claim, ST}, #{login_token => ?TOKEN, scanned_by => Uid}}).

put_cache_confirmed(Table, ST, Uid) ->
    ets:insert(Table, {{qr_login, ST}, #{<<"status">> => <<"confirmed">>,
        <<"login_token">> => ?TOKEN, <<"scanned_by">> => Uid}}).

ready(ST) ->
    qr_login_sse_handler:info(
        #{<<"status">> => <<"confirmed_ready">>}, #{}, #{session_token => ST}).

poll(ST) ->
    {stop, Req, _} = qr_login_handler:handle_request(
        #{method => <<"GET">>, qs => [{<<"session_token">>, ST}]}, #{action => status}),
    jsone:decode(maps:get(response_body, Req)).

assert_poll_error(Code, Response) ->
    ?assertEqual(Code, maps:get(<<"code">>, Response)),
    ?assertEqual(#{}, maps:get(<<"payload">>, Response)).

expected_payload() ->
    #{<<"status">> => <<"confirmed">>, <<"token">> => ?TOKEN,
        <<"uid">> => ?UID, <<"uid_str">> => <<"111174215241304065">>}.

sent_chunks() ->
    [Body || {_, {cowboy_req, stream_body, [Body, _, _]}, ok} <- meck:history(cowboy_req)].

sse_payloads() ->
    [jsone:decode(binary:part(Json, 0, byte_size(Json) - 2)) ||
        <<"data: ", Json/binary>> <- sent_chunks()].
