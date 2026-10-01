%% Real identity responses and refusal to acknowledge an unsaved replay result.
-module(enterprise_identity_contract_http_checks).
-export([run/1]).
-include_lib("eunit/include/eunit.hrl").
-define(PATH, <<"/api/internal/v1/identity-mappings">>).

run(S) ->
    Body = #{<<"external_user_id">> => <<"intbe02-ext-h4">>, <<"user_id">> => 995017},
    completion_failure(S, <<"PUT">>, Body, <<"removed">>),
    Invalid = req(
        S, <<"PUT">>, Body#{<<"user_id">> => 9223372036854775808}, <<"identity-contract-range">>
    ),
    ?assertEqual(400, maps:get(status, Invalid)),
    B = req(S, <<"PUT">>, Body, <<"identity-contract-bind">>),
    BR = req(S, <<"PUT">>, Body, <<"identity-contract-bind">>),
    DBody = maps:with([<<"external_user_id">>], Body),
    completion_failure(S, <<"DELETE">>, DBody, <<"active">>),
    D = req(S, <<"DELETE">>, DBody, <<"identity-contract-revoke">>),
    DR = req(S, <<"DELETE">>, DBody, <<"identity-contract-revoke">>),
    lists:foreach(
        fun({First, Replay}) ->
            ?assertEqual(200, maps:get(status, First)),
            ?assertEqual(200, maps:get(status, Replay)),
            ?assertEqual(maps:get(body, First), maps:get(body, Replay)),
            ?assertEqual(
                <<"true">>, intbe02_http_support:json_header_val(Replay, <<"idempotent-replayed">>)
            )
        end,
        [{B, BR}, {D, DR}]
    ),
    Samples = [
        {<<"put_first">>, B},
        {<<"put_replay">>, BR},
        {<<"delete_first">>, D},
        {<<"delete_replay">>, DR}
    ],
    case os:getenv("IMBOY_GATE_RUN_DIR") of
        false ->
            ok;
        Directory ->
            Data = maps:from_list([{K, jsone:decode(maps:get(body, R))} || {K, R} <- Samples]),
            ok = file:write_file(
                filename:join(Directory, "identity-responses.json"), jsone:encode(Data)
            )
    end.

completion_failure(S, Method, Body, Status) ->
    C = maps:get(conn, S),
    Before = identity_audits(C),
    ok = intbe02_http_support:sql_exec(
        C,
        <<"CREATE FUNCTION synthetic_idem_skip() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN RETURN NULL; END $$">>
    ),
    ok = intbe02_http_support:sql_exec(
        C,
        <<
            "CREATE TRIGGER synthetic_idem_skip BEFORE UPDATE ON enterprise_internal_idempotency "
            "FOR EACH ROW WHEN (NEW.idempotency_key LIKE 'identity-contract-ack-%') EXECUTE FUNCTION synthetic_idem_skip()"
        >>
    ),
    try
        assert_failed_completion(S, Method, Body, Status, Before),
        ok = intbe02_http_support:sql_exec(
            C,
            <<
                "CREATE OR REPLACE FUNCTION synthetic_idem_skip() RETURNS trigger LANGUAGE plpgsql AS $$ "
                "BEGIN RAISE EXCEPTION 'synthetic replay snapshot failure'; END $$"
            >>
        ),
        assert_failed_completion(S, Method, Body, Status, Before)
    after
        ok = intbe02_http_support:sql_exec(
            C, <<"DROP TRIGGER synthetic_idem_skip ON enterprise_internal_idempotency">>
        ),
        ok = intbe02_http_support:sql_exec(C, <<"DROP FUNCTION synthetic_idem_skip()">>)
    end.

assert_failed_completion(S, Method, Body, Status, Before) ->
    R = req(S, Method, Body, <<"identity-contract-ack-", Method/binary>>),
    ?assertEqual(500, maps:get(status, R), maps:get(body, R)),
    ?assertEqual(
        <<"internal_error">>,
        maps:get(<<"code">>, maps:get(<<"error">>, jsone:decode(maps:get(body, R))))
    ),
    C = maps:get(conn, S),
    #{<<"status">> := Status} = intbe02_http_support:one(
        C,
        <<"SELECT status FROM enterprise_external_identity WHERE organization_id=995101 AND external_user_id='intbe02-ext-h4'">>
    ),
    ?assertEqual(Before, identity_audits(C)),
    #{<<"n">> := 0} = intbe02_http_support:one(
        C,
        <<"SELECT count(*) AS n FROM enterprise_internal_idempotency WHERE idempotency_key LIKE 'identity-contract-ack-%'">>
    ).

identity_audits(C) ->
    intbe02_http_support:one(
        C,
        <<"SELECT count(*) AS n FROM enterprise_audit_event WHERE organization_id=995101 AND resource_type='enterprise_external_identity'">>
    ).

req(S, M, Body, Key) ->
    Headers = maps:merge(
        intbe02_http_support:auth(maps:get(cred_a, S)), intbe02_http_support:idem(Key)
    ),
    intbe02_http_support:http(maps:get(port, S), M, ?PATH, Body, Headers).
