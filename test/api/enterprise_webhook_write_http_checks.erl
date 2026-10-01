-module(enterprise_webhook_write_http_checks).
-export([run/1]).
-include_lib("eunit/include/eunit.hrl").

run(S) ->
    secret_snapshot(S),
    lists:foreach(
        fun({Kind, Mode}) -> failed_write(S, Kind, Mode) end,
        [{K, M} || K <- [secret, config], M <- [drop, raise]]
    ).

secret_snapshot(S) ->
    Input = #{
        <<"url">> => <<"https://oa.customer.example.com/intbe02/hook">>,
        <<"events">> => [<<"file.confirmed">>],
        <<"rotate">> => true
    },
    Key = <<"webhook-secret-snapshot">>,
    First = request(S, Input, Key),
    ?assertEqual(200, maps:get(status, First)),
    Secret = maps:get(<<"secret">>, jsone:decode(maps:get(body, First))),
    Stored = cs_pg_test_fixture:scalar(
        <<"SELECT response_body FROM enterprise_internal_idempotency WHERE organization_id=995101 AND application_id=$1 AND idempotency_key=$2">>,
        [maps:get(app_a, S), Key],
        <<>>
    ),
    ?assert(byte_size(Stored) > 0),
    Retry = request(S, Input, Key),
    Contains = binary:match(Stored, Secret) =/= nomatch,
    Matches = maps:get(body, First) =:= maps:get(body, Retry),
    ok = file:write_file(
        filename:join(os:getenv("IMBOY_GATE_RUN_DIR"), "webhook-secret-snapshot.json"),
        jsone:encode(#{
            raw_contains_secret => Contains,
            replay_matches => Matches,
            first_status => maps:get(status, First),
            replay_status => maps:get(status, Retry)
        })
    ),
    ?assertNot(Contains),
    ?assert(Matches),
    ?assertEqual(200, maps:get(status, Retry)),
    legacy_snapshot(S, Input, Key, Stored, maps:get(body, First)),
    snapshot_faults(S, Input, Key, Stored).

legacy_snapshot(S, Input, Key, Stored, Body) ->
    Sql =
        <<"UPDATE enterprise_internal_idempotency SET response_body=$3 WHERE organization_id=995101 AND application_id=$1 AND idempotency_key=$2">>,
    Params = [maps:get(app_a, S), Key],
    try
        ok = cs_pg_test_fixture:exec(Sql, Params ++ [Body]),
        Replay = request(S, Input, Key),
        ?assertEqual(200, maps:get(status, Replay)),
        ?assert(Body =:= maps:get(body, Replay))
    after
        ok = cs_pg_test_fixture:exec(Sql, Params ++ [Stored])
    end.

snapshot_faults(S, Input, Key, Stored) ->
    Before = snapshot(S),
    Count = audit_count(S),
    Params = [maps:get(app_a, S), Key],
    Update =
        <<"UPDATE enterprise_internal_idempotency SET response_body=$3 WHERE organization_id=995101 AND application_id=$1 AND idempotency_key=$2">>,
    try
        ok = cs_pg_test_fixture:exec(Update, Params ++ [<<"{\"_imboy_snapshot_v1\":\"broken\"}">>]),
        assert_snapshot_failure(request(S, Input, Key)),
        ?assertEqual(Before, snapshot(S)),
        ?assertEqual(Count, audit_count(S))
    after
        ok = cs_pg_test_fixture:exec(Update, Params ++ [Stored])
    end,
    Move =
        <<"UPDATE enterprise_internal_idempotency SET idempotency_key=$3 WHERE organization_id=995101 AND application_id=$1 AND idempotency_key=$2">>,
    Other = <<Key/binary, "-copied">>,
    try
        ok = cs_pg_test_fixture:exec(Move, Params ++ [Other]),
        assert_snapshot_failure(request(S, Input, Other)),
        ?assertEqual(Before, snapshot(S)),
        ?assertEqual(Count, audit_count(S))
    after
        ok = cs_pg_test_fixture:exec(Move, [maps:get(app_a, S), Other, Key])
    end,
    snapshot_key_fault(S, Input, Key),
    ?assertEqual(Before, snapshot(S)),
    ?assertEqual(Count, audit_count(S)),
    ok = file:write_file(
        filename:join(os:getenv("IMBOY_GATE_RUN_DIR"), "webhook-snapshot-faults.json"),
        jsone:encode(#{
            tampered_cipher => 500,
            copied_key => 500,
            missing_key => 500,
            wrong_key => 500,
            previous_key_replay => 200,
            business_state_unchanged => true,
            audit_delta => 0
        })
    ).

snapshot_key_fault(S, Input, Key) ->
    {ok, Master} = application:get_env(imboy, postgre_aes_key),
    Original = request(S, Input, Key),
    try
        application:set_env(imboy, postgre_aes_key, <<>>),
        assert_snapshot_failure(request(S, Input, Key)),
        Update = Input#{
            <<"rotate">> => false,
            <<"url">> => <<"https://oa.customer.example.com/intbe02/missing-key">>
        },
        assert_snapshot_failure(request(S, Update, <<Key/binary, "-missing-key-write">>)),
        application:set_env(imboy, postgre_aes_key, <<"SYNTHETIC-WRONG-KEY">>),
        assert_snapshot_failure(request(S, Input, Key)),
        application:set_env(imboy, postgre_aes_key_old, Master),
        Previous = request(S, Input, Key),
        ?assertEqual(200, maps:get(status, Previous)),
        ?assert(maps:get(body, Original) =:= maps:get(body, Previous))
    after
        application:set_env(imboy, postgre_aes_key, Master),
        application:unset_env(imboy, postgre_aes_key_old)
    end.

assert_snapshot_failure(R) ->
    ?assertEqual(500, maps:get(status, R)),
    Json = jsone:decode(maps:get(body, R)),
    ?assertEqual(<<"internal_error">>, maps:get(<<"code">>, maps:get(<<"error">>, Json))),
    ?assertNot(maps:is_key(<<"secret">>, Json)).

failed_write(S, Kind, Mode) ->
    Prefix = <<(atom_to_binary(Kind))/binary, "-", (atom_to_binary(Mode))/binary>>,
    Initial = #{
        <<"url">> => <<"https://oa.customer.example.com/intbe02/hook">>,
        <<"events">> => [<<"file.confirmed">>],
        <<"rotate">> => true
    },
    First = request(S, Initial, <<Prefix/binary, "-initial">>),
    assert_persisted_secret(First, secret),
    Before = snapshot(S),
    Count = audit_count(S),
    install_failure(Kind, Mode),
    Input =
        case Kind of
            secret ->
                Initial;
            config ->
                Initial#{
                    <<"rotate">> => false,
                    <<"url">> => <<"https://oa.customer.example.com/intbe02/new-hook">>
                }
        end,
    Key = <<Prefix/binary, "-failed">>,
    R =
        try
            request(S, Input, Key)
        after
            remove_failure()
        end,
    Json = jsone:decode(maps:get(body, R)),
    ok = record_failure(S, Kind, Mode, R, Before, Count, Json),
    ?assertEqual(500, maps:get(status, R)),
    ?assertEqual(<<"internal_error">>, maps:get(<<"code">>, maps:get(<<"error">>, Json))),
    ?assertEqual(Before, snapshot(S)),
    ?assertEqual(Count, audit_count(S)),
    ?assertNot(maps:is_key(<<"secret">>, Json)),
    Retry = request(S, Input, Key),
    assert_persisted_secret(Retry, Kind),
    ?assertEqual(Count + 1, audit_count(S)).

record_failure(S, Kind, Mode, R, Before, Count, Json) ->
    StoredMatches =
        case maps:find(<<"secret">>, Json) of
            {ok, Secret} -> enterprise_webhook_repo:get_secret(995014) =:= {ok, Secret};
            error -> null
        end,
    Safe = #{
        operation => Kind,
        failure => Mode,
        http_status => maps:get(status, R),
        state_unchanged => Before =:= snapshot(S),
        audit_delta => audit_count(S) - Count,
        returned_secret_matches_storage => StoredMatches
    },
    file:write_file(
        filename:join(
            os:getenv("IMBOY_GATE_RUN_DIR"),
            "webhook-write-failures.jsonl"
        ),
        [jsone:encode(Safe), <<"\n">>],
        [append]
    ).

assert_persisted_secret(R, Kind) ->
    ?assertEqual(200, maps:get(status, R)),
    Json = jsone:decode(maps:get(body, R)),
    case Kind of
        secret ->
            Secret = maps:get(<<"secret">>, Json),
            ?assert(enterprise_webhook_repo:get_secret(995014) =:= {ok, Secret});
        config ->
            ?assertNot(maps:is_key(<<"secret">>, Json))
    end.

request(S, Input, Key) ->
    intbe02_http_support:with_public_dns(fun() ->
        intbe02_http_support:http(
            maps:get(port, S),
            <<"PUT">>,
            <<"/api/internal/v1/webhook">>,
            Input,
            maps:merge(
                intbe02_http_support:auth(maps:get(cred_a, S)),
                intbe02_http_support:idem(Key)
            )
        )
    end).

install_failure(Kind, Mode) ->
    Field =
        case Kind of
            secret -> <<"verify_token_enc">>;
            config -> <<"webhook_url">>
        end,
    Failure =
        case Mode of
            drop -> <<"RETURN NULL;">>;
            raise -> <<"RAISE EXCEPTION 'synthetic webhook write failure';">>
        end,
    ok = cs_pg_test_fixture:exec(
        <<"CREATE FUNCTION gate_skip_webhook_write() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN IF NEW.",
            Field/binary, " IS DISTINCT FROM OLD.", Field/binary, " THEN ", Failure/binary,
            " END IF; RETURN NEW; END $$">>,
        []
    ),
    cs_pg_test_fixture:exec(
        <<"CREATE TRIGGER gate_skip_webhook_write BEFORE UPDATE ON bot FOR EACH ROW EXECUTE FUNCTION gate_skip_webhook_write()">>,
        []
    ).

remove_failure() ->
    ok = cs_pg_test_fixture:exec(<<"DROP TRIGGER gate_skip_webhook_write ON bot">>, []),
    cs_pg_test_fixture:exec(<<"DROP FUNCTION gate_skip_webhook_write()">>, []).

snapshot(_S) ->
    cs_pg_test_fixture:scalar(
        <<"SELECT md5(to_jsonb(b)::text) FROM bot b WHERE user_id=995014">>, [], undefined
    ).

audit_count(S) ->
    cs_pg_test_fixture:scalar(
        <<"SELECT count(*) FROM enterprise_audit_event WHERE organization_id=995101 AND resource_id=$1 AND action='webhook.configured'">>,
        [maps:get(app_a, S)],
        -1
    ).
