%%% Actual pooled governance logic and disposable native PG; no mocked audit result.
-module(enterprise_admin_governance_pg_checks).
-export([run/0]).
-include_lib("eunit/include/eunit.hrl").

run() ->
    H = intbe02_http_support:setup_all(),
    try
        audit_failure()
    after
        intbe02_http_support:teardown_all(H),
        inttest_marker_db:release(H)
    end.

audit_failure() ->
    S = cs_pg_test_fixture:new_scope(),
    Org = maps:get(org_id, S),
    App = cs_pg_test_fixture:id(),
    ok = cs_pg_test_fixture:exec(
        <<"INSERT INTO enterprise_application(id,organization_id,principal_user_id,application_key,name) VALUES($1,$2,$3,$4,'synthetic governance audit')">>,
        [App, Org, maps:get(actor_user_id, S), integer_to_binary(App)]
    ),
    Before = application_row(Org, App),
    ok = cs_pg_test_fixture:exec(
        <<"CREATE FUNCTION gate_drop_admin_audit() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN IF NEW.action='application_status_changed' THEN RETURN NULL; END IF; RETURN NEW; END $$">>,
        []
    ),
    ok = cs_pg_test_fixture:exec(
        <<"CREATE TRIGGER gate_drop_admin_audit BEFORE INSERT ON enterprise_audit_event FOR EACH ROW EXECUTE FUNCTION gate_drop_admin_audit()">>,
        []
    ),
    try
        Result = enterprise_admin_governance_logic:set_status(
            Org,
            App,
            1,
            <<"disabled">>,
            #{account => <<"synthetic-platform-admin">>}
        ),
        After = application_row(Org, App),
        Count = cs_pg_test_fixture:scalar(
            <<"SELECT count(*) FROM enterprise_audit_event WHERE organization_id=$1 AND resource_id=$2 AND action='application_status_changed'">>,
            [Org, App],
            -1
        ),
        ok = file:write_file(
            filename:join(os:getenv("IMBOY_GATE_RUN_DIR"), "governance-audit-failure.json"),
            jsone:encode(#{
                before_state => Before,
                after_state => After,
                operation_success => Result =:= ok,
                audit_count => Count
            })
        ),
        ?assertEqual(Before, After),
        ?assertMatch({error, _}, Result),
        ?assertEqual(0, Count)
    after
        ok = cs_pg_test_fixture:exec(
            <<"DROP TRIGGER gate_drop_admin_audit ON enterprise_audit_event">>, []
        ),
        ok = cs_pg_test_fixture:exec(<<"DROP FUNCTION gate_drop_admin_audit()">>, [])
    end.

application_row(Org, App) ->
    {ok, [Row]} = elib_pg:query(
        <<"SELECT status,version FROM enterprise_application WHERE organization_id=$1 AND id=$2">>,
        [Org, App]
    ),
    Row.
