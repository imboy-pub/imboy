-module(enterprise_webhook_write_http_checks).
-export([run/1]).
-include_lib("eunit/include/eunit.hrl").

run(S) ->
    lists:foreach(
        fun({Kind, Mode}) -> failed_write(S, Kind, Mode) end,
        [{K, M} || K <- [secret, config], M <- [drop, raise]]
    ).

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
