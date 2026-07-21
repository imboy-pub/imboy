-module(trust_audit_repo_integration_tests).
%%%
%% trust_audit_repo:insert_event/1 真连 PostgreSQL 集成测（**非 meck**）。
%%
%% 实证只有真实 DB 才能保证的不变量（当前 logic-eunit 用 meck 绕过）：
%%   1. event_id partial UNIQUE + ON CONFLICT DO NOTHING → 幂等（同 event_id 只落 1 行）；
%%   2. event_id 抢占核对 → 同 event_id 异归属返回 event_id_conflict，不吞合法事件；
%%   3. per-target advisory 锁内读 MAX(target_identity_version) → 版本回退拒绝。
%%
%% 依赖：本地库 imboy_v1（trust_audit 表须已存在，migration 44+47 已应用）。
%% 运行（须把本地 pg_conf 载入 eunit VM，否则 config_ds:env(pg_conf)=undefined）：
%%   IMBOYENV=local make eunit t=trust_audit_repo_integration_tests \
%%     EUNIT_ERL_OPTS="-config config/sys.local -pa ebin -pa test"
%%   （用高位测试 target_uid，setup/teardown 清理自身行，不动 schema）
%%%

-include_lib("eunit/include/eunit.hrl").

%% 测试专用 target_uid（高位，避免撞真实数据）+ 设备 id
-define(TU, 88880001).
-define(TD, <<"itest-dev-b">>).

%%%===================================================================
%%% fixture
%%%===================================================================

setup() ->
    _ = application:load(imboy),
    application:set_env(imboy, env, test),
    _ = application:start(pooler),
    PgConf = config_ds:env(pg_conf),
    %% 幂等：池已存在则忽略 already_started
    _ = pooler:new_pool(PgConf),
    cleanup_rows(),
    ok.

teardown(_) ->
    cleanup_rows(),
    ok.

cleanup_rows() ->
    Tb = trust_audit_repo:tablename(),
    _ = elib_pg:query(
        <<"DELETE FROM ", Tb/binary, " WHERE target_uid = $1">>, [?TU]
    ),
    ok.

base_n(EventId) ->
    #{
        actor_uid => 100,
        target_uid => ?TU,
        target_device_id => ?TD,
        target_ed25519 => <<"ZWQtYg==">>,
        from_state => <<"unverified">>,
        to_state => <<"verified">>,
        method => <<"qr_scan">>,
        actor_signature => <<"c2ln">>,
        event_id => EventId,
        issued_at => 1700000000000,
        expires_at => 1700000060000,
        actor_device_generation => 1,
        target_identity_version => 1
    }.

%%%===================================================================
%%% tests
%%%===================================================================

insert_event_test_() ->
    {setup, fun setup/0, fun teardown/1, [
        {"first insert → inserted", fun first_insert_inserts/0},
        {"same event_id same owner → duplicate (idempotent, 1 row)",
            fun idempotent_replay_is_duplicate/0},
        {"same event_id different owner → event_id_conflict", fun event_id_takeover_rejected/0},
        {"lower target_identity_version → identity_version_rollback",
            fun version_rollback_rejected/0}
    ]}.

first_insert_inserts() ->
    E = <<"aaaa0001-0000-4000-8000-000000000001">>,
    ?assertEqual({ok, inserted}, trust_audit_repo:insert_event(base_n(E))),
    ?assertEqual(1, row_count(E)).

idempotent_replay_is_duplicate() ->
    E = <<"aaaa0002-0000-4000-8000-000000000002">>,
    ?assertEqual({ok, inserted}, trust_audit_repo:insert_event(base_n(E))),
    ?assertEqual({ok, duplicate}, trust_audit_repo:insert_event(base_n(E))),
    %% 幂等：重放不新增行
    ?assertEqual(1, row_count(E)).

event_id_takeover_rejected() ->
    E = <<"aaaa0003-0000-4000-8000-000000000003">>,
    ?assertEqual({ok, inserted}, trust_audit_repo:insert_event(base_n(E))),
    %% 同 event_id、不同 actor（归属不符）→ 拒绝，不吞掉原合法事件
    Other = (base_n(E))#{actor_uid => 999},
    ?assertEqual(
        {error, <<"event_id_conflict">>}, trust_audit_repo:insert_event(Other)
    ),
    ?assertEqual(1, row_count(E)).

version_rollback_rejected() ->
    E1 = <<"aaaa0004-0000-4000-8000-000000000004">>,
    E2 = <<"aaaa0005-0000-4000-8000-000000000005">>,
    High = (base_n(E1))#{target_identity_version => 5},
    ?assertEqual({ok, inserted}, trust_audit_repo:insert_event(High)),
    %% 同 target device、更低版本 → 回退拒绝（advisory 锁内读 MAX 判定）
    Low = (base_n(E2))#{target_identity_version => 4},
    ?assertEqual(
        {error, <<"identity_version_rollback">>},
        trust_audit_repo:insert_event(Low)
    ),
    ?assertEqual(0, row_count(E2)).

%%%===================================================================
%%% helper
%%%===================================================================

row_count(EventId) ->
    Tb = trust_audit_repo:tablename(),
    case
        elib_pg:query(
            <<"SELECT COUNT(*) AS c FROM ", Tb/binary, " WHERE event_id = $1">>,
            [EventId]
        )
    of
        {ok, [#{<<"c">> := C} | _]} -> C;
        _ -> -1
    end.
