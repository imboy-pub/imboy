%%% @doc EB-03R P11 套件：offboarding case / item / CAS 的正向持久化能力。
%%%
%%% 服务 EB-08（用户裁决 §三「预先补齐」）：case INSERT/GET/LIST、item INSERT/LIST/
%%% UPDATE(retry)、CAS execute。判定：写入后可读回、CAS 版本不匹配即 conflict、
%%% 重试保持幂等键逐字不变、同 Org+leaver 未完成 case 唯一。
-module(eb_offboarding_pg_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

offboarding_test_() ->
    {setup, fun setup/0, fun cleanup_db/1, fun cases/1}.

setup() ->
    eunit_runner:eunit_setup_with_db().

cleanup_db({ok, Conn}) ->
    eunit_runner:eunit_cleanup_db(Conn);
cleanup_db(_Other) ->
    ok.

cases({ok, _Conn}) ->
    [
        {timeout, 60, fun case_insert_get_list_round_trips/0},
        {timeout, 60, fun unfinished_case_is_unique_per_org_and_leaver/0},
        {timeout, 60, fun item_insert_is_idempotent_on_idempotency_key/0},
        {timeout, 60, fun item_retry_keeps_idempotency_key_and_bumps_attempt/0},
        {timeout, 60, fun case_advance_is_a_version_cas/0},
        {timeout, 60, fun invalid_transitions_stay_in_domain_and_cas_never_lies/0}
    ];
cases(_Skipped) ->
    {skip, "offboarding suite requires the scratch database connection"}.

case_insert_get_list_round_trips() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        CaseId = insert_case(Scope, <<"eb03r-case-1">>),
        {ok, Case} = eb_pg_store:fetch_offboarding_case(Org, Ws, CaseId),
        ?assertEqual(draft, maps:get(status, Case)),
        ?assertEqual(1, maps:get(version, Case)),
        ?assertEqual(maps:get(actor_user_id, Scope), maps:get(leaver_user_id, Case)),
        ?assertEqual(maps:get(owner_user_id, Scope), maps:get(successor_user_id, Case)),
        {ok, Cases} = eb_pg_store:list_offboarding_cases(Org, Ws),
        ?assert(lists:any(fun(C) -> maps:get(id, C) =:= CaseId end, Cases)),
        %% 跨 Org 一律 not_found
        ?assertEqual(
            {error, not_found},
            eb_pg_store:fetch_offboarding_case(
                maps:get(other_org_id, Scope), maps:get(other_workspace_id, Scope), CaseId
            )
        )
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

unfinished_case_is_unique_per_org_and_leaver() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        _First = insert_case(Scope, <<"eb03r-case-2a">>),
        %% 同 Org + 同 leaver 的第二个未完成 case：DB 部分唯一索引裁决 → conflict
        ?assertEqual(
            {error, conflict},
            eb_pg_store:insert_offboarding_case(Org, Ws, case_params(Scope, <<"eb03r-case-2b">>))
        )
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

item_insert_is_idempotent_on_idempotency_key() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        CaseId = insert_case(Scope, <<"eb03r-case-3">>),
        ItemId = insert_item(Scope, CaseId, <<"eb03r-idem-1">>),
        {ok, Items} = eb_pg_store:list_offboarding_items(Org, Ws, CaseId),
        ?assertEqual(1, length(Items)),
        ?assertEqual(pending, maps:get(status, hd(Items))),
        %% 同一幂等键重放：不得增行（conflict 而非静默成功）
        ?assertEqual(
            {error, conflict},
            eb_pg_store:insert_offboarding_item(
                Org, Ws, item_params(Scope, CaseId, <<"eb03r-idem-1">>)
            )
        ),
        ?assertEqual(1, length(element(2, eb_pg_store:list_offboarding_items(Org, Ws, CaseId)))),
        _ = ItemId,
        ok
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

item_retry_keeps_idempotency_key_and_bumps_attempt() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        CaseId = insert_case(Scope, <<"eb03r-case-4">>),
        ItemId = insert_item(Scope, CaseId, <<"eb03r-idem-2">>),
        {ok, Failed} = eb_pg_store:update_offboarding_item(Org, Ws, #{
            id => ItemId,
            status => failed,
            failure_reason => <<"eb03r-synthetic-failure">>
        }),
        ?assertEqual(failed, maps:get(status, Failed)),
        ?assertEqual(0, maps:get(attempt, Failed)),
        %% 重试：status 回 pending、attempt 递增、幂等键逐字不变
        {ok, Retried} = eb_pg_store:update_offboarding_item(Org, Ws, #{
            id => ItemId, status => pending
        }),
        ?assertEqual(pending, maps:get(status, Retried)),
        ?assertEqual(1, maps:get(attempt, Retried)),
        ?assertEqual(<<"eb03r-idem-2">>, maps:get(idempotency_key, Retried)),
        ?assertEqual(CaseId, maps:get(case_id, Retried)),
        %% 失败原因列受 ck_eoi_failure_reason 约束：非 failed 状态必须为 NULL
        ?assertEqual(undefined, maps:get(failure_reason, Retried)),
        %% 非 failed 状态下调用方传入的 failure_reason 被**归一化清空**（DB CHECK 强制
        %% `failure_reason IS NULL OR status='failed'`；港口不得把非法组合留在库里）
        {ok, Cleared} = eb_pg_store:update_offboarding_item(Org, Ws, #{
            id => ItemId,
            status => pending,
            failure_reason => <<"should-be-cleared">>
        }),
        ?assertEqual(undefined, maps:get(failure_reason, Cleared)),
        ?assertEqual(
            0,
            eb_pg_test_fixture:scalar(
                <<
                    "SELECT count(*) AS n FROM enterprise_offboarding_item"
                    " WHERE organization_id=$1 AND id=$2 AND status <> 'failed'"
                    "   AND failure_reason IS NOT NULL"
                >>,
                [Org, ItemId]
            )
        )
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

case_advance_is_a_version_cas() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        CaseId = insert_case(Scope, <<"eb03r-case-5">>),
        ok = eb_pg_store:advance_offboarding_case(Org, Ws, CaseId, 1, frozen, #{
            total => 0, success => 0, failed => 0
        }),
        {ok, Case} = eb_pg_store:fetch_offboarding_case(Org, Ws, CaseId),
        ?assertEqual(frozen, maps:get(status, Case)),
        ?assertEqual(2, maps:get(version, Case)),
        %% 陈旧版本 ⇒ conflict（不得覆盖）
        ?assertEqual(
            {error, conflict},
            eb_pg_store:advance_offboarding_case(Org, Ws, CaseId, 1, transferring, #{
                total => 0, success => 0, failed => 0
            })
        ),
        ?assertEqual(
            frozen,
            maps:get(status, element(2, eb_pg_store:fetch_offboarding_case(Org, Ws, CaseId)))
        )
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

%% CAS 只保证「版本一致才写」；状态机合法性由 domain 裁决（本模块不发明状态机）。
invalid_transitions_stay_in_domain_and_cas_never_lies() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        {Org, Ws} = tenant(Scope),
        CaseId = insert_case(Scope, <<"eb03r-case-6">>),
        %% domain 判 draft→completed 非法（必须经 verifying）
        ?assertEqual(
            {error, {invalid_transition, draft, completed}},
            eb_offboarding:transition(draft, completed)
        ),
        %% 但 DB/Port 的 CAS 只校验版本：本用例**故意**不调用它（避免把非法迁移写成事实）
        ?assertEqual(
            {error, conflict},
            eb_pg_store:advance_offboarding_case(Org, Ws, CaseId, 99, completed, #{
                total => 0, success => 0, failed => 0
            })
        ),
        %% 合法路径逐级推进
        ok = eb_pg_store:advance_offboarding_case(Org, Ws, CaseId, 1, frozen, #{
            total => 3, success => 0, failed => 0
        }),
        ok = eb_pg_store:advance_offboarding_case(Org, Ws, CaseId, 2, transferring, #{
            total => 3, success => 0, failed => 0
        }),
        %% FND-6：verify 时点带真实终值（2 成功 1 失败）——case 行计数必须可回读
        ok = eb_pg_store:advance_offboarding_case(Org, Ws, CaseId, 3, verifying, #{
            total => 3, success => 2, failed => 1
        }),
        %% 计数专用 CAS：状态不变（verifying）也要求 version 匹配，且不递增 version
        ok = eb_pg_store:update_offboarding_case_counts(
            Org, Ws, CaseId, 4, verifying, #{total => 3, success => 3, failed => 0}
        ),
        ?assertEqual(
            {error, conflict},
            eb_pg_store:update_offboarding_case_counts(
                Org, Ws, CaseId, 99, verifying, #{total => 3, success => 3, failed => 0}
            )
        ),
        {ok, Mid} = eb_pg_store:fetch_offboarding_case(Org, Ws, CaseId),
        ?assertEqual(3, maps:get(item_total, Mid)),
        ?assertEqual(3, maps:get(item_success, Mid)),
        ?assertEqual(0, maps:get(item_failed, Mid)),
        ?assertEqual(4, maps:get(version, Mid)),
        ok = eb_pg_store:advance_offboarding_case(Org, Ws, CaseId, 4, completed, #{
            total => 3, success => 3, failed => 0
        }),
        {ok, Done} = eb_pg_store:fetch_offboarding_case(Org, Ws, CaseId),
        ?assertEqual(completed, maps:get(status, Done)),
        ?assertNotEqual(undefined, maps:get(completed_at, Done))
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

%% ===================================================================
%% 辅助
%% ===================================================================

tenant(Scope) ->
    {maps:get(org_id, Scope), maps:get(workspace_id, Scope)}.

case_params(Scope, Reason) ->
    #{
        id => eb_pg_test_fixture:id(),
        leaver_user_id => maps:get(actor_user_id, Scope),
        successor_user_id => maps:get(owner_user_id, Scope),
        created_by_user_id => maps:get(owner_user_id, Scope),
        reason => Reason
    }.

insert_case(Scope, Reason) ->
    {Org, Ws} = tenant(Scope),
    Params = case_params(Scope, Reason),
    {ok, Row} = eb_pg_store:insert_offboarding_case(Org, Ws, Params),
    maps:get(id, Row).

item_params(Scope, CaseId, IdemKey) ->
    #{
        id => eb_pg_test_fixture:id(),
        case_id => CaseId,
        business_identity_id => maps:get(service_identity_id, Scope),
        from_user_id => maps:get(actor_user_id, Scope),
        to_user_id => maps:get(owner_user_id, Scope),
        idempotency_key => IdemKey
    }.

insert_item(Scope, CaseId, IdemKey) ->
    {Org, Ws} = tenant(Scope),
    Params = item_params(Scope, CaseId, IdemKey),
    {ok, Row} = eb_pg_store:insert_offboarding_item(Org, Ws, Params),
    maps:get(id, Row).
