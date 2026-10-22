-module(talk_record_governance_relay_tests).
-include_lib("eunit/include/eunit.hrl").

governance_test_() ->
    {timeout, 120,
        {setup, fun setup/0, fun talk_record_t19_support:teardown_all/1, fun(S) ->
            {timeout, 60, ?_test(run(S))}
        end}}.
setup() ->
    S = talk_record_t19_support:setup_all(),
    talk_record_setup_fixture:prepare(
        S,
        fun() ->
            talk_record_t19_support:seed(S),
            application:set_env(imboy, enterprise_attachment_retention_days, 1),
            S
        end,
        fun talk_record_t19_support:teardown_all/1
    ).
run(S) ->
    C = maps:get(conn, S),
    I = talk_record_t19_support:ids(),
    Org = maps:get(org_a, I),
    Ws = maps:get(ws_a1, I),
    {ok, R} = talk_record_t15_support:create_org_record(700001, Org, 0, 801),
    A = artifact(C, R, false),
    {ok, _, _} = bind(C, R, A, <<"plain-org">>),
    {ok, E} = enterprise_attachment_retention_repo:find_tx(C, A),
    ?assertEqual(null, maps:get(<<"application_id">>, E)),
    ?assertEqual(<<"talk_record">>, maps:get(<<"source_kind">>, E)),
    ?assertEqual(R, maps:get(<<"talk_record_id">>, E)),
    {ok, [#{<<"n">> := 0}]} = elib_pg:query(
        C, <<"SELECT count(*) n FROM workspace_attachment_retention WHERE attachment_id=$1">>, [A]
    ),
    application:set_env(imboy, enterprise_attachment_retention_days, 900),
    {ok, _, _} = bind(C, R, A, <<"plain-org">>),
    {ok, E2} = enterprise_attachment_retention_repo:find_tx(C, A),
    ?assertEqual(maps:get(<<"retention_until">>, E), maps:get(<<"retention_until">>, E2)),
    ?assertMatch(
        {ok, {blocked_retention, _}},
        talk_record_attachment_adapter:evaluate_release_blockers_tx(C, R)
    ),
    {ok, 1} = elib_pg:execute(C, <<"UPDATE talk_record SET retention_until=NULL WHERE id=$1">>, [R]),
    ?assertMatch(
        {ok, {blocked_retention, _}},
        talk_record_attachment_adapter:evaluate_release_blockers_tx(C, R)
    ),
    guard_rejects(C, A, Org, R),
    failed_bind(C, R),
    {ok, _, _} = talk_record_delete_logic:delete(700001, R, <<"1">>),
    {ok, #{cleanup_status := <<"blocked_retention">>}} = elib_pg:with_tx(fun(X) ->
        talk_record_cleanup_logic:execute_purge_tx(X, #{<<"record_id">> => R})
    end),
    workspace_purge(C, Org, Ws),
    personal(C),
    legacy_application(C, Org),
    admin_api(C, Org),
    ok.

bind(_C, R, A, K) ->
    elib_pg:with_tx(fun(X) ->
        talk_record_attachment_adapter:bind_artifact_tx(X, R, A, <<"export">>, K)
    end).
artifact(C, R, Old) ->
    {ok, created, J} = elib_pg:with_tx(fun(X) ->
        talk_record_job_repo:enqueue_tx(
            X,
            #{
                record_id => R,
                kind => <<"export">>,
                dedupe_key => integer_to_binary(elib_tsid:generate(attachment))
            }
        )
    end),
    Bytes = <<"synthetic governance artifact">>,
    {ok, A} = elib_pg:with_tx(fun(X) ->
        attachment_derived_artifact:reserve_tx(
            X,
            #{job_id => J, attempt => 1, actor_id => 700001},
            #{
                <<"path">> =>
                    <<"governance/", (integer_to_binary(R))/binary, "/",
                        (integer_to_binary(elib_tsid:generate(attachment)))/binary>>,
                <<"url">> => <<>>,
                <<"name">> => <<"test.txt">>,
                <<"size">> => byte_size(Bytes),
                <<"mime_type">> => <<"text/plain">>,
                <<"scope">> => <<"talk_record">>,
                <<"scope_ref">> => integer_to_binary(R),
                <<"file_hash256">> => binary:encode_hex(crypto:hash(sha256, Bytes), lowercase)
            }
        )
    end),
    ok = attachment_derived_artifact:write(A, Bytes),
    case Old of
        true ->
            {ok, 1} = elib_pg:execute(
                C, <<"UPDATE attachment SET created_at=now()-interval '2 days' WHERE id=$1">>, [A]
            );
        false ->
            ok
    end,
    A.

workspace_purge(C, Org, Ws) ->
    application:set_env(imboy, enterprise_attachment_retention_days, 1),
    {ok, R} = talk_record_t15_support:create_org_record(700001, Org, Ws, 802),
    A = artifact(C, R, true),
    {ok, _, _} = bind(C, R, A, <<"workspace">>),
    {ok, [#{<<"n">> := 1}]} = elib_pg:query(
        C,
        <<
            "SELECT count(*) n FROM workspace_attachment_retention"
            " WHERE attachment_id=$1 AND scope='talk_record' AND workspace_id=$2"
        >>,
        [A, Ws]
    ),
    ok = enterprise_attachment_retention_repo:hold_tx(C, A, <<"synthetic legal hold">>),
    ?assertEqual(
        {ok, {blocked_hold, 1}}, talk_record_attachment_adapter:evaluate_release_blockers_tx(C, R)
    ),
    blocked_purge(C, A),
    ok = enterprise_attachment_retention_repo:release_tx(C, A),
    ?assertEqual(
        {ok, false},
        elib_pg:with_tx(fun(X) -> attachment_repo:refresh_gc_verified_tx(X, path(C, A)) end)
    ),
    {ok, _} = enterprise_attachment_retention_repo:purge_tx(C, A),
    ?assertEqual(
        {error, intent_conflict},
        elib_pg:with_tx(fun(X) ->
            attachment_repo:intent_purged_delete_tx(X, path(C, A), <<"must not bypass bound refs">>)
        end)
    ),
    {ok, _} = elib_pg:with_tx(fun(X) -> talk_record_attachment_ref_repo:release_record_tx(X, R) end),
    ?assertEqual(
        {ok, true},
        elib_pg:with_tx(fun(X) -> attachment_repo:refresh_gc_verified_tx(X, path(C, A)) end)
    ),
    {ok, 1} = elib_pg:execute(
        C,
        <<
            "INSERT INTO attach_pending(object_key,bucket,scope,creator_user_id,session_state,"
            "upload_expires_at,confirmed_attachment_id) VALUES($1,'fixture','talk_record',700001,'registered',NULL,NULL)"
        >>,
        [path(C, A)]
    ),
    ?assertEqual(
        {error, intent_conflict},
        elib_pg:with_tx(fun(X) ->
            attachment_repo:intent_purged_delete_tx(X, path(C, A), <<"must not bypass upload cap">>)
        end)
    ),
    {ok, 1} = elib_pg:execute(C, <<"DELETE FROM attach_pending WHERE object_key=$1">>, [path(C, A)]),
    ?assertEqual(
        ok,
        elib_pg:with_tx(fun(X) ->
            attachment_repo:intent_ordinary_delete_tx(X, path(C, A), <<"test-approved-governance">>)
        end)
    ),
    ?assertEqual({error, already_purged}, enterprise_attachment_retention_repo:purge_tx(C, A)).
path(C, A) ->
    {ok, [#{<<"path">> := P}]} = elib_pg:query(C, <<"SELECT path FROM attachment WHERE id=$1">>, [A]),
    P.
blocked_purge(_C, A) ->
    ?assertEqual(
        {rollback, purge_blocked},
        elib_pg:with_tx(
            fun(X) ->
                case enterprise_attachment_retention_repo:purge_tx(X, A) of
                    {error, purge_blocked} -> throw(purge_blocked);
                    V -> V
                end
            end,
            [{reraise, false}]
        )
    ).

guard_rejects(C, A, Org, R) ->
    lists:foreach(
        fun(Sql) -> ?assertMatch({error, _}, elib_pg:query(C, Sql, [A])) end,
        [
            <<"UPDATE enterprise_attachment_retention SET application_id=1 WHERE attachment_id=$1">>,
            <<"UPDATE enterprise_attachment_retention SET retention_until=now()-interval '1 day' WHERE attachment_id=$1">>,
            <<"DELETE FROM enterprise_attachment_retention WHERE attachment_id=$1">>
        ]
    ),
    ?assertMatch(
        {error, _},
        elib_pg:execute(
            C,
            <<
                "INSERT INTO workspace_attachment_retention"
                "(attachment_id,organization_id,workspace_id,scope,scope_ref) VALUES($1,$2,999,'talk_record',$3)"
            >>,
            [A, Org, integer_to_binary(R)]
        )
    ),
    blocked_purge(C, A).
failed_bind(C, R) ->
    A = artifact(C, R, false),
    ?assertMatch({error, _}, bind(C, R, A, <<"plain-org">>)),
    ?assertEqual({error, not_found}, enterprise_attachment_retention_repo:find_tx(C, A)),
    ?assertEqual(
        {ok, []},
        elib_pg:query(C, <<"SELECT 1 FROM talk_record_attachment_ref WHERE attachment_id=$1">>, [A])
    ).

personal(C) ->
    {ok, R} = talk_record_t15_support:create_record(700001, 803, <<"personal unchanged">>),
    A = artifact(C, R, false),
    {ok, _, _} = bind(C, R, A, <<"personal">>),
    ?assertEqual({error, not_found}, enterprise_attachment_retention_repo:find_tx(C, A)).
legacy_application(C, Org) ->
    {ok, 1} = elib_pg:execute(
        C, <<"INSERT INTO enterprise_application(id,organization_id) VALUES(810001,$1)">>, [Org]
    ),
    {ok, R} = talk_record_t15_support:create_record(700001, 804, <<"application fixture source">>),
    A = artifact(C, R, false),
    {ok, E} = enterprise_attachment_retention_repo:upsert_tx(C, A, Org, 810001),
    ?assertEqual(<<"application">>, maps:get(<<"source_kind">>, E)),
    ?assertEqual(null, maps:get(<<"talk_record_id">>, E)),
    {ok, [#{<<"attachment_id">> := TalkAid}]} = elib_pg:query(
        C,
        <<"SELECT attachment_id FROM enterprise_attachment_retention WHERE source_kind='talk_record' LIMIT 1">>,
        []
    ),
    ?assertEqual(
        {error, upsert_empty_result},
        enterprise_attachment_retention_repo:upsert_tx(C, TalkAid, Org, 810001)
    ).

admin_api(C, Org) ->
    {ok, R} = talk_record_t15_support:create_org_record(700002, Org, 0, 805),
    A = artifact(C, R, true),
    {ok, _, _} = bind(C, R, A, <<"admin-meta">>),
    ?assertEqual({error, not_found}, talk_record_repo:find_visible(700001, R)),
    {ok, Meta} = talk_record_governance_logic:read(700001, R, A),
    ?assertEqual(false, maps:is_key(<<"title">>, Meta)),
    ?assertEqual(false, maps:is_key(<<"path">>, Meta)),
    ?assertEqual({error, not_found}, talk_record_governance_logic:read(700002, R, A)),
    ?assertEqual({error, not_found}, talk_record_governance_logic:read(700004, R, A)),
    ?assertEqual(
        {error, precondition_required},
        talk_record_governance_logic:mutate(700001, R, A, undefined, <<"x">>, #{
            <<"op">> => <<"purge">>
        })
    ),
    ?assertEqual(
        {error, invalid_body},
        talk_record_governance_logic:mutate(
            700001,
            R,
            A,
            <<"1">>,
            <<"long-reason">>,
            #{<<"op">> => <<"hold">>, <<"reason">> => binary:copy(<<"x">>, 201)}
        )
    ),
    ?assertEqual(
        {error, not_found},
        talk_record_governance_logic:mutate(
            700002,
            R,
            A,
            <<"1">>,
            <<"unicode-reason">>,
            #{<<"op">> => <<"hold">>, <<"reason">> => binary:copy(<<"中"/utf8>>, 200)}
        )
    ),
    ?assertEqual(
        {error, hold_not_set},
        talk_record_governance_logic:mutate(700001, R, A, <<"1">>, <<"none-held">>, #{
            <<"op">> => <<"release_hold">>
        })
    ),
    audit_rollback(C, R, A),
    {ok, created, H} = talk_record_governance_logic:mutate(
        700001, R, A, <<"1">>, <<"admin-hold">>, #{
            <<"op">> => <<"hold">>, <<"reason">> => <<"synthetic hold">>
        }
    ),
    ?assertEqual(2, maps:get(<<"revision">>, H)),
    ?assertMatch(
        {ok, replay, _},
        talk_record_governance_logic:mutate(700001, R, A, <<"1">>, <<"admin-hold">>, #{
            <<"op">> => <<"hold">>, <<"reason">> => <<"synthetic hold">>
        })
    ),
    ?assertEqual(
        {error, revision_mismatch},
        talk_record_governance_logic:mutate(700001, R, A, <<"1">>, <<"stale">>, #{
            <<"op">> => <<"release_hold">>
        })
    ),
    {ok, 1} = elib_pg:execute(
        C,
        <<"UPDATE organization_member SET status='suspended' WHERE organization_id=$1 AND user_id=700001">>,
        [Org]
    ),
    ?assertEqual(
        {error, not_found},
        talk_record_governance_logic:mutate(700001, R, A, <<"1">>, <<"admin-hold">>, #{
            <<"op">> => <<"hold">>, <<"reason">> => <<"synthetic hold">>
        })
    ),
    {ok, 1} = elib_pg:execute(
        C,
        <<"UPDATE organization_member SET status='active' WHERE organization_id=$1 AND user_id=700001">>,
        [Org]
    ),
    {ok, created, _} = talk_record_governance_logic:mutate(700001, R, A, <<"2">>, <<"release">>, #{
        <<"op">> => <<"release_hold">>
    }),
    {ok, _, _} = talk_record_delete_logic:delete(700002, R, <<"3">>),
    {ok, #{cleanup_status := <<"retained_by_other_reference">>}} = elib_pg:with_tx(fun(X) ->
        talk_record_cleanup_logic:execute_purge_tx(X, #{<<"record_id">> => R})
    end),
    Guard = unknown_processor_fixture(C, R),
    {ok, created, Blocked} = talk_record_governance_logic:mutate(
        700001, R, A, <<"4">>, <<"approved-purge">>, #{<<"op">> => <<"purge">>}
    ),
    ?assertEqual(false, maps:get(<<"cleanup_scheduled">>, Blocked)),
    ?assertEqual(<<"retained_by_other_reference">>, maps:get(<<"cleanup_status">>, Blocked)),
    %% This explicit synthetic run never launched an OS process; only its known fixture is stopped.
    ok = talk_record_processor_run_repo:finish(Guard, <<"stopped">>),
    {ok, created, P} = talk_record_governance_logic:mutate(
        700001, R, A, <<"5">>, <<"reconcile-approved-purge">>, #{<<"op">> => <<"purge">>}
    ),
    ?assertEqual(true, maps:get(<<"cleanup_scheduled">>, P)),
    ?assertEqual(<<"pending">>, maps:get(<<"cleanup_status">>, P)),
    {ok, [#{<<"n">> := 4}]} = elib_pg:query(
        C,
        <<
            "SELECT count(*) n FROM talk_record_audit"
            " WHERE record_id=$1 AND action='attachment_governance'"
        >>,
        [R]
    ).

audit_rollback(C, R, A) ->
    ok = meck:new(talk_record_repo, [passthrough, no_link]),
    meck:expect(talk_record_repo, insert_audit_tx, fun(_, _, _, _, _, _) ->
        {error, synthetic_audit_failed}
    end),
    try
        ?assertEqual(
            {error, synthetic_audit_failed},
            talk_record_governance_logic:mutate(
                700001,
                R,
                A,
                <<"1">>,
                <<"audit-failure">>,
                #{<<"op">> => <<"hold">>, <<"reason">> => <<"fixture">>}
            )
        )
    after
        meck:unload(talk_record_repo)
    end,
    {ok, E} = enterprise_attachment_retention_repo:find_tx(C, A),
    ?assertEqual(<<"none">>, maps:get(<<"hold_state">>, E)),
    {ok, [#{<<"revision">> := 1}]} = elib_pg:query(
        C, <<"SELECT revision FROM talk_record WHERE id=$1">>, [R]
    ),
    ?assertEqual(
        {ok, []},
        elib_pg:query(
            C, <<"SELECT 1 FROM talk_record_idempotency WHERE idempotency_key='audit-failure'">>, []
        )
    ).

unknown_processor_fixture(C, R) ->
    {ok, created, Jid} = talk_record_job_repo:enqueue_tx(C, #{
        record_id => R, kind => <<"transcribe">>, dedupe_key => <<"governance-unknown">>
    }),
    J = #{
        <<"id">> => Jid,
        <<"record_id">> => R,
        <<"attempt">> => 1,
        <<"lease_token">> => <<"11111111-2222-4333-8444-555555555555">>
    },
    ok = talk_record_processor_run_repo:reserve_tx(C, J),
    ok = talk_record_processor_run_repo:finish(J, <<"unknown">>),
    J.
