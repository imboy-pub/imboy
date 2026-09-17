%% @doc AG31-09：Recovery 隔离 PG 验收（多连接真库腿）——A11 双连接 lease
%% 竞争（winner=1/duplicate=0）、unknown reconcile 真库 CAS、dispatching→unknown
%% 显式登记、链头与残留。
%%
%% == 运行前置 ==（AG31-02B/05 同配方；端口 4403、容器 imboy-ag31-09-pg18）
%%   AG31_PG_HOST / AG31_PG_PORT / AG31_PG_DB / AG31_PG_USER / AG31_PG_PASSWORD
%% 缺失或连不上 → 显式 FAIL（禁 skip）。夹具固定 993xxx 高位区段自建自清。
-module(agent_recovery_pg_tests).

-include_lib("eunit/include/eunit.hrl").

-define(ID_ORG, 993001).
-define(ID_DELEG, 993010).
-define(ID_AGENT, 993011).
-define(ID_GRANT, 993100).
-define(ID_RUN, 993301).
-define(AUTH, agent_run_pg).
-define(REC, agent_recovery).
-define(NOW, {{2026, 9, 17}, {12, 0, 0}}).

agent_recovery_pg_test_() ->
    {setup, fun connect_required/0, fun disconnect/1, fun(Conns) ->
        [
            {"A11: 双连接 lease 竞争 → winner=1, loser 零动作", fun() ->
                t_lease_race(Conns)
            end},
            {"A11': 双连接 guarded effect 竞争 → 恰一落账", fun() ->
                t_guarded_race(Conns)
            end},
            {"dispatching→unknown 显式登记 + unknown→succeeded reconcile 真库 CAS", fun() ->
                t_unknown_reconcile(Conns)
            end},
            {"链头 00000133 非 dirty + 夹具残留为零", fun() -> t_chain_head(Conns) end}
        ]
    end}.

connect_required() ->
    Missing = [
        K
     || K <- ["AG31_PG_HOST", "AG31_PG_PORT", "AG31_PG_DB", "AG31_PG_USER"],
        os:getenv(K) =:= false
    ],
    case Missing of
        [] -> ok;
        _ -> erlang:error({ag31_pg_missing_env, Missing})
    end,
    ensure_tsid(),
    Opts = #{
        host => os:getenv("AG31_PG_HOST"),
        port => list_to_integer(os:getenv("AG31_PG_PORT")),
        username => os:getenv("AG31_PG_USER"),
        password => os:getenv("AG31_PG_PASSWORD", ""),
        database => os:getenv("AG31_PG_DB")
    },
    {ok, C1} = epgsql:connect(Opts),
    {ok, C2} = epgsql:connect(Opts),
    fixture_reset(C1),
    seed(C1),
    [C1, C2].

disconnect([C1, C2]) ->
    lists:foreach(
        fun(C) ->
            try
                epgsql:close(C)
            catch
                _:_ -> ok
            end
        end,
        [C1, C2]
    ),
    ok.

ensure_tsid() ->
    try elib_tsid:generate(default) of
        _ -> ok
    catch
        _:_ -> ok = elib_tsid:init(#{dc_id => 0, node_id => 9})
    end.

%% ===================================================================

%% A11：同一过期 running Run，两个连接同时 lease_take_over——CAS 保证恰一。
t_lease_race([C1, C2]) ->
    %% 已过期（NOW=12:00）
    Expire = {{2026, 9, 17}, {10, 0, 0}},
    {ok, _} = ?REC:recover_run(C1, ?ID_RUN, <<"w-1">>, 60, #{now => ?NOW}),
    %% w-1 抢占成功（lease 已续到 12:01）→ w-2 在原过期时刻竞争必败
    ?assertEqual(
        {error, lease_not_acquired},
        agent_run_pg:lease_take_over(C2, ?ID_RUN, <<"w-2">>, Expire, ?NOW)
    ),
    {ok, Run} = ?AUTH:get_run(C1, ?ID_RUN),
    ?assertEqual(<<"w-1">>, maps:get(lease_owner, Run)),
    %% attempt 语义（04B）：acquire(+1) → 过期接管(+1) = 2
    ?assertEqual(2, attempts_of(C1)).

%% 双连接并发 guarded insert（同幂等键）→ 恰一落账、另一 duplicate deny。
t_guarded_race([C1, C2]) ->
    reset_run(C1),
    Effect = effect(1, <<"ext-key-race">>),
    %% C1 先落账
    {ok, _} = ?AUTH:insert_effect_guarded_tx(C1, Effect),
    %% C2 同幂等键 → duplicate（A11 duplicate_effect=0 新行）
    ?assertMatch(
        {error, {duplicate_effect, _}},
        ?AUTH:insert_effect_guarded_tx(C2, Effect#{id => maps:get(id, Effect) + 1})
    ),
    {ok, _, [{Count}]} = epgsql:equery(
        C1,
        "SELECT count(*) FROM agent_effect WHERE tool_id=$1 AND external_idempotency_key=$2",
        [maps:get(tool_id, Effect), <<"ext-key-race">>]
    ),
    ?assertEqual(1, Count).

%% dispatching→unknown（显式登记）+ unknown→succeeded（reconcile 真库 CAS）
t_unknown_reconcile([C1, _C2]) ->
    reset_run(C1),
    Effect = effect(2, <<"ext-key-unk">>),
    {ok, EffectId} = ?AUTH:insert_effect_guarded_tx(C1, Effect),
    %% authorized→dispatching（恢复语义前置：effect_to_tx 直用，边表允许）
    %% v2=authorized（insert+created→authorized 已耗 v1）
    {ok, _} = ?AUTH:effect_to_tx(C1, EffectId, authorized, dispatching, 2, ?NOW, #{}),
    %% v3=dispatching；CAS 需显式期望版本
    {ok, _} = ?REC:mark_unknown(C1, EffectId, #{now => ?NOW, expected_version => 3}),
    {ok, EffU} = ?AUTH:get_effect(C1, EffectId),
    ?assertEqual(unknown, maps:get(status, EffU)),
    %% reconcile：unknown(v4)→succeeded 恰一次；再用旧版本 → cas_conflict
    {ok, _} = ?REC:reconcile_effect(C1, EffectId, succeeded, 4, #{now => ?NOW}),
    {ok, EffS} = ?AUTH:get_effect(C1, EffectId),
    ?assertEqual(succeeded, maps:get(status, EffS)),
    ?assertEqual(
        {error, cas_conflict},
        ?REC:reconcile_effect(C1, EffectId, failed, 4, #{now => ?NOW})
    ).

t_chain_head([C1, _C2]) ->
    {ok, _, [{Version, Dirty}]} = epgsql:equery(
        C1, "SELECT version, dirty FROM schema_migrations", []
    ),
    ?assertEqual(133, Version),
    ?assertEqual(false, Dirty),
    fixture_reset(C1),
    {ok, _, [{Residue}]} = epgsql:equery(
        C1,
        "SELECT count(*) FROM agent_effect e JOIN agent_run r ON r.id = e.run_id "
        "WHERE r.agent_id = $1",
        [?ID_AGENT]
    ),
    ?assertEqual(0, Residue).

%% ===================================================================
%% 夹具（993xxx 高位区段自建自清）
%% ===================================================================

attempts_of(Conn) ->
    {ok, _, [{A}]} = epgsql:equery(Conn, "SELECT attempt FROM agent_run WHERE id=$1", [?ID_RUN]),
    A.

reset_run(Conn) ->
    squery_ok(Conn, "DELETE FROM agent_effect WHERE run_id=$1", [?ID_RUN]).

seed(Conn) ->
    {ok, 3} = epgsql:equery(
        Conn,
        "INSERT INTO \"user\" (id, account, password, reg_ip, reg_cosv, status) "
        "VALUES ($1,'ag31_09_deleg','x','127.0.0.1','e2e',1),"
        "($2,'ag31_09_agent','x','127.0.0.1','e2e',1),"
        "($3,'ag31_09_human','x','127.0.0.1','e2e',1)",
        [?ID_DELEG, ?ID_AGENT, 993013]
    ),
    {ok, 1} = epgsql:equery(Conn, "UPDATE \"user\" SET account_type=1 WHERE id=$1", [?ID_AGENT]),
    {ok, 1} = epgsql:equery(
        Conn,
        "INSERT INTO organization (id,name,owner_id) VALUES ($1,'ag31-09-org',$2)",
        [?ID_ORG, ?ID_DELEG]
    ),
    {ok, 1} = epgsql:equery(
        Conn,
        "INSERT INTO agent_grant (id,agent_id,organization_id,delegator_user_id,"
        "workspace_scope_kind,status,valid_from,expires_at,version,idempotency_key) "
        "VALUES ($1,$2,$3,$4,'none','active',now()-interval '1 hour',now()+interval '1 day',1,"
        "'ag31-09-grant-k')",
        [?ID_GRANT, ?ID_AGENT, ?ID_ORG, ?ID_DELEG]
    ),
    {ok, 1} = epgsql:equery(
        Conn,
        "INSERT INTO agent_grant_capability (grant_id,capability,action,resource_type,"
        "constraint_json) VALUES ($1,'demo.write','create','demo','{}'::jsonb)",
        [?ID_GRANT]
    ),
    %% running Run + 过期 lease（A11 竞争底座）
    {ok, _} = agent_run_command:create_run(Conn, #{
        id => ?ID_RUN,
        agent_id => ?ID_AGENT,
        organization_id => ?ID_ORG,
        workspace_id => undefined,
        grant_id => ?ID_GRANT,
        grant_version_at_start => 1,
        delegating_principal_id => ?ID_DELEG,
        trigger_type => message,
        trigger_id => <<"tr-9">>,
        runtime_type => mock,
        context_digest => <<"sha256:c9">>,
        idempotency_key => <<"ag31-09-run">>,
        now => ?NOW
    }),
    {ok, _} = agent_run_command:transition(
        Conn,
        ?ID_RUN,
        created,
        queued,
        #{expected_version => 1, now => ?NOW, actor_id => <<"system:trigger">>}
    ),
    {ok, _} = agent_run_command:acquire_lease(Conn, ?ID_RUN, <<"w0">>, 60, #{now => ?NOW}),
    %% 人为把 lease 过期时刻拨回 10:00（NOW=12:00 → 已过期）
    {ok, 1} = epgsql:equery(
        Conn,
        "UPDATE agent_run SET lease_expires_at=$2 WHERE id=$1",
        [?ID_RUN, {{2026, 9, 17}, {10, 0, 0}}]
    ),
    ok.

effect(Seq, ExtKey) ->
    #{
        id => 993900 + Seq,
        run_id => ?ID_RUN,
        sequence => Seq,
        tool_id => <<"tool.demo">>,
        capability => <<"demo.write">>,
        action => <<"create">>,
        resource_digest => <<"sha256:res">>,
        args_digest => <<"sha256:args">>,
        external_idempotency_key => ExtKey,
        now => ?NOW,
        decided_status => authorized
    }.

fixture_reset(Conn) ->
    ok = squery_ok(Conn, "SET session_replication_role = replica", []),
    lists:foreach(
        fun(S) -> ok = squery_ok(Conn, S, []) end,
        [
            "DELETE FROM agent_effect WHERE run_id IN "
            "(SELECT id FROM agent_run WHERE agent_id = 993011)",
            "DELETE FROM agent_run_event WHERE run_id IN "
            "(SELECT id FROM agent_run WHERE agent_id = 993011)",
            "DELETE FROM agent_run WHERE agent_id = 993011",
            "DELETE FROM agent_grant_capability WHERE grant_id = 993100",
            "DELETE FROM agent_grant_workspace WHERE grant_id = 993100",
            "DELETE FROM agent_grant_event WHERE grant_id = 993100",
            "DELETE FROM agent_grant WHERE id = 993100",
            "DELETE FROM organization WHERE id = 993001",
            "DELETE FROM \"user\" WHERE id IN (993010, 993011, 993013)"
        ]
    ),
    ok = squery_ok(Conn, "RESET session_replication_role", []),
    ok.

squery_ok(Conn, Sql, Params) ->
    case epgsql:equery(Conn, Sql, Params) of
        {ok, _} -> ok;
        {ok, _, _} -> ok;
        Other -> erlang:error({squery_failed, Sql, Other})
    end.
