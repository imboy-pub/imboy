%%% Actual pooled governance logic and disposable native PG; no mocked audit result.
-module(enterprise_admin_governance_pg_checks).
-export([run/0]).
-include_lib("eunit/include/eunit.hrl").

run() ->
    H = intbe02_http_support:setup_all(),
    try
        Cases = [
            status,
            scopes,
            issue_credential,
            rotate_credential,
            revoke_credential,
            issue_grant,
            grant_scopes,
            grant_workspaces,
            grant_combined,
            revoke_grant
        ],
        Evidence = [audit_failure(K, Mode) || K <- Cases, Mode <- [drop, raise]],
        combined_failure(),
        concurrent_cas(),
        tenant_denial(),
        write_evidence(Evidence)
    after
        intbe02_http_support:teardown_all(H),
        inttest_marker_db:release(H)
    end.

audit_failure(Kind, Mode) ->
    S = fixture(),
    Before = snapshot(S),
    install_failure(Mode),
    Result =
        try
            operation(Kind, S)
        after
            remove_failure()
        end,
    After = snapshot(S),
    ?assertEqual(Before, After),
    ?assertMatch({error, {audit_failed, _}}, Result),
    ?assertEqual(0, audit_count(S)),
    ?assert(success(operation(Kind, S))),
    ?assertEqual(1, audit_count(S)),
    assert_safe_audit(S),
    #{operation => Kind, failure => Mode, rolled_back => true, retry_pass => true}.

fixture() ->
    S = cs_pg_test_fixture:new_scope(),
    Org = maps:get(org_id, S),
    App = cs_pg_test_fixture:id(),
    ok = cs_pg_test_fixture:exec(
        <<"INSERT INTO enterprise_application(id,organization_id,principal_user_id,application_key,name) VALUES($1,$2,$3,$4,'synthetic governance audit')">>,
        [App, Org, maps:get(actor_user_id, S), integer_to_binary(App)]
    ),
    {ok, Cred} = enterprise_internal_ops:issue_credential(Org, App, undefined),
    {ok, Grant} = enterprise_internal_ops:issue_grant(Org, App, #{
        scopes => [<<"application:read">>],
        workspace_scope_kind => none,
        expires_at => <<"2099-01-01T00:00:00Z">>,
        idempotency_key => <<"fixture">>
    }),
    S#{
        app_id => App,
        credential_id => maps:get(credential_id, Cred),
        grant_id => maps:get(<<"id">>, Grant)
    }.

actor() -> #{account => <<"synthetic-platform-admin">>, adm_user_id => 12345}.

operation(status, S) ->
    enterprise_admin_governance_logic:set_status(
        maps:get(org_id, S), maps:get(app_id, S), 1, <<"disabled">>, actor()
    );
operation(scopes, S) ->
    enterprise_admin_governance_logic:set_scopes(
        maps:get(org_id, S), maps:get(app_id, S), 1, [<<"application:read">>], actor()
    );
operation(Kind, S) ->
    credential_or_grant_operation(Kind, S).

credential_or_grant_operation(issue_credential, S) ->
    enterprise_admin_governance_logic:issue_credential(
        maps:get(org_id, S), maps:get(app_id, S), undefined, actor()
    );
credential_or_grant_operation(rotate_credential, S) ->
    enterprise_admin_governance_logic:rotate_credential(
        maps:get(org_id, S), maps:get(app_id, S), maps:get(credential_id, S), actor()
    );
credential_or_grant_operation(revoke_credential, S) ->
    enterprise_admin_governance_logic:revoke_credential(
        maps:get(org_id, S), maps:get(app_id, S), maps:get(credential_id, S), actor()
    );
credential_or_grant_operation(issue_grant, S) ->
    enterprise_admin_governance_logic:issue_grant(
        maps:get(org_id, S),
        maps:get(app_id, S),
        #{scopes => [<<"application:read">>], idempotency_key => <<"governance">>},
        1,
        actor()
    );
credential_or_grant_operation(Kind, S) ->
    enterprise_admin_governance_logic:patch_grant(
        maps:get(org_id, S),
        maps:get(app_id, S),
        maps:get(grant_id, S),
        1,
        patch(Kind, S),
        actor()
    ).

patch(grant_scopes, _) ->
    #{scopes => [<<"identities:read">>]};
patch(grant_workspaces, S) ->
    #{workspace_scope_kind => explicit, workspace_ids => [maps:get(workspace_id, S)]};
patch(grant_combined, S) ->
    maps:merge(patch(grant_scopes, S), patch(grant_workspaces, S));
patch(revoke_grant, _) ->
    #{revoke => true}.

success(ok) -> true;
success({ok, _}) -> true;
success(_) -> false.

install_failure(Mode) ->
    Body =
        case Mode of
            drop -> <<"RETURN NULL;">>;
            raise -> <<"RAISE EXCEPTION 'synthetic audit unavailable';">>
        end,
    ok = cs_pg_test_fixture:exec(
        <<"CREATE FUNCTION gate_drop_admin_audit() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN IF NEW.actor_role='platform_admin' THEN ",
            Body/binary, " END IF; RETURN NEW; END $$">>,
        []
    ),
    cs_pg_test_fixture:exec(
        <<"CREATE TRIGGER gate_drop_admin_audit BEFORE INSERT ON enterprise_audit_event FOR EACH ROW EXECUTE FUNCTION gate_drop_admin_audit()">>,
        []
    ).

remove_failure() ->
    ok = cs_pg_test_fixture:exec(
        <<"DROP TRIGGER gate_drop_admin_audit ON enterprise_audit_event">>, []
    ),
    cs_pg_test_fixture:exec(<<"DROP FUNCTION gate_drop_admin_audit()">>, []).

%% Compare every persisted column and child row; do not archive credential digests.
snapshot(S) ->
    Org = maps:get(org_id, S),
    App = maps:get(app_id, S),
    Tables = [
        <<"enterprise_application">>,
        <<"enterprise_application_credential">>,
        <<"enterprise_application_grant">>
    ],
    Parent = [
        cs_pg_test_fixture:scalar(
            <<"SELECT md5(coalesce(jsonb_agg(to_jsonb(t) ORDER BY id)::text,'[]')) FROM ", T/binary,
                " t WHERE organization_id=$1 AND ",
                (case T of
                    <<"enterprise_application">> -> <<"id">>;
                    _ -> <<"application_id">>
                end)/binary, "=$2">>,
            [Org, App],
            undefined
        )
     || T <- Tables
    ],
    Children = [
        cs_pg_test_fixture:scalar(
            <<"SELECT md5(coalesce(jsonb_agg(to_jsonb(t) ORDER BY to_jsonb(t)::text)::text,'[]')) FROM ",
                T/binary,
                " t WHERE grant_id IN (SELECT id FROM enterprise_application_grant WHERE organization_id=$1 AND application_id=$2)">>,
            [Org, App],
            undefined
        )
     || T <- [
            <<"enterprise_application_grant_scope">>,
            <<"enterprise_application_grant_workspace">>
        ]
    ],
    Parent ++ Children.

audit_count(S) ->
    cs_pg_test_fixture:scalar(
        <<"SELECT count(*) FROM enterprise_audit_event WHERE organization_id=$1 AND resource_id=$2 AND actor_role='platform_admin'">>,
        [maps:get(org_id, S), maps:get(app_id, S)],
        -1
    ).

assert_safe_audit(S) ->
    {ok, [Row]} = elib_pg:query(
        <<"SELECT actor_user_id,detail FROM enterprise_audit_event WHERE organization_id=$1 AND resource_id=$2 AND actor_role='platform_admin'">>,
        [maps:get(org_id, S), maps:get(app_id, S)]
    ),
    ?assertEqual(null, maps:get(<<"actor_user_id">>, Row)),
    Detail = maps:get(<<"detail">>, Row),
    Encoded =
        case is_binary(Detail) of
            true -> Detail;
            false -> jsone:encode(Detail)
        end,
    ?assertNotEqual(nomatch, binary:match(Encoded, <<"synthetic-platform-admin">>)),
    [
        ?assertEqual(nomatch, binary:match(Encoded, Key))
     || Key <-
            [<<"secret">>, <<"digest">>]
    ].

combined_failure() ->
    S = fixture(),
    Before = snapshot(S),
    Bad = (patch(grant_combined, S))#{workspace_ids => [maps:get(other_workspace_id, S)]},
    Result = enterprise_admin_governance_logic:patch_grant(
        maps:get(org_id, S), maps:get(app_id, S), maps:get(grant_id, S), 1, Bad, actor()
    ),
    ?assertMatch({error, _}, Result),
    ?assertEqual(Before, snapshot(S)),
    ?assertEqual(0, audit_count(S)),
    ?assertEqual(ok, operation(grant_combined, S)).

concurrent_cas() ->
    S = fixture(),
    Parent = self(),
    Refs = [
        begin
            Ref = make_ref(),
            spawn(fun() -> Parent ! {Ref, operation(status, S)} end),
            Ref
        end
     || _ <- [1, 2]
    ],
    Results = [
        receive
            {R, Result} -> Result
        after 10000 -> error(cas_timeout)
        end
     || R <- Refs
    ],
    ?assertEqual(lists:sort([{error, version_conflict}, ok]), lists:sort(Results)),
    ?assertEqual(1, audit_count(S)).

tenant_denial() ->
    S = fixture(),
    Before = snapshot(S),
    Other = fixture(),
    ?assertEqual(
        {error, not_found},
        enterprise_admin_governance_logic:rotate_credential(
            maps:get(org_id, S), maps:get(app_id, S), maps:get(credential_id, Other), actor()
        )
    ),
    ?assertEqual(
        {error, not_found},
        enterprise_admin_governance_logic:set_status(
            maps:get(other_org_id, S), maps:get(app_id, S), 1, <<"disabled">>, actor()
        )
    ),
    ?assertEqual(Before, snapshot(S)),
    ?assertEqual(0, audit_count(S)).

write_evidence(Evidence) ->
    file:write_file(
        filename:join(os:getenv("IMBOY_GATE_RUN_DIR"), "governance-audit-checks.json"),
        jsone:encode(#{
            audit_failures => Evidence,
            combined_invalid_workspace_rollback => true,
            concurrent_cas_one_winner => true,
            tenant_denial => true
        })
    ).
