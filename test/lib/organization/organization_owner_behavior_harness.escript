#!/usr/bin/env escript
%%! -noshell
%% ===================================================================
%% ORG-01 Owner Invariant 行为矩阵 harness（一次性 PG / disposable marker）
%% -------------------------------------------------------------------
%% 用法:
%%   PGHOST=127.0.0.1 PGPORT=4398 PGUSER=imboy_user PGPASSWORD=... \
%%   PGDATABASE=org01_disposable_marker_20260916 IMBOY_DIR=<worktree> \
%%   test/lib/organization/organization_owner_behavior_harness.escript
%%
%% 前置: 该库已应用迁移 00000126/00000127（drill_migrate.escript up）。
%% 探针: P01-P06 = 应用层 organization_owner_transfer:transfer/3 真调；
%%       A01-A08 = DB 不变量矩阵（冻结实现第 1-4 点正反例）；
%%       C01 = 并发 transfer（两连接，expected-version = 锁内 owner 快照）。
%% fixture 全部使用 8e8 段 bigint id；密码只从环境读取，不打印。
%% ===================================================================
main(_) ->
    RepoDir = env("IMBOY_DIR", "."),
    code:add_pathsa([
        filename:join([RepoDir, "ebin"]),
        filename:join([RepoDir, "deps/epgsql/ebin"]),
        filename:join([RepoDir, "deps/pooler/ebin"]),
        filename:join([RepoDir, "deps/lager/ebin"]),
        filename:join([RepoDir, "deps/erlware_commons/ebin"]),
        filename:join([RepoDir, "deps/goldrush/ebin"])
    ]),
    ConnOpts = conn_opts(),
    {ok, Conn} = epgsql:connect(ConnOpts),
    {ok, Conn2} = epgsql:connect(ConnOpts),
    Counters = counters:new(1, []),
    ok = app_probes(Conn, Counters),
    ok = db_probes(Conn, Counters),
    ok = concurrency_probe(Conn, Conn2, Counters),
    cleanup(Conn),
    epgsql:close(Conn),
    epgsql:close(Conn2),
    io:format("~nPROBE-SUMMARY pass=~p~n", [counters:get(Counters, 1)]),
    halt(0).

conn_opts() ->
    #{host => env("PGHOST", "127.0.0.1"),
      port => list_to_integer(env("PGPORT", "4398")),
      username => env("PGUSER", "imboy_user"),
      password => env("PGPASSWORD", ""),
      database => env("PGDATABASE", "org01_disposable_marker_20260916")}.

%%--------------------------------------------------------------------
%% P01-P06: 应用层 organization_owner_transfer:transfer/3（真 PG + with_tx）
%%--------------------------------------------------------------------
app_probes(Conn, Counters) ->
    {ok, _} = application:ensure_all_started(pooler),
    %% with_tx 的连接池名来自 config_ds:env(sql_driver)（application:env imboy）
    ok = application:set_env(imboy, sql_driver, pgsql),
    {ok, _} = pooler:new_pool(maps:merge(#{name => pgsql, init_count => 1, max_count => 4,
                                           queue_max => 20},
                                          #{start_mfa => {epgsql, connect, [conn_opts()]}})),
    ok = fixture_base(Conn),
    %% P01 正常转移成功（响应 map 与既有 API 契约一致）
    ok = fixture_org(Conn, 800000001, 800000001, [{admin, 800000002}]),
    case organization_owner_transfer:transfer(800000001, 800000001, 800000002) of
        {ok, #{organization_id := 800000001, owner_id := 800000002,
               previous_owner_id := 800000001, previous_owner_role := <<"admin">>}} ->
            case {owner_of(Conn, 800000001), owner_member(Conn, 800000001)} of
                {800000002, 800000002} -> pass(Counters, "P01 transfer command success + projection");
                Other1 -> fail(Counters, "P01 member state after transfer", Other1)
            end;
        Other -> fail(Counters, "P01 transfer command success", Other)
    end,
    %% P02 已降级的旧 owner 再发起 = 403
    case organization_owner_transfer:transfer(800000001, 800000001, 800000003) of
        {error, {403, _}} -> pass(Counters, "P02 demoted initiator rejected 403");
        Other2 -> fail(Counters, "P02 demoted initiator rejected 403", Other2)
    end,
    %% P03 self transfer = 400
    case organization_owner_transfer:transfer(800000002, 800000001, 800000002) of
        {error, {400, _}} -> pass(Counters, "P03 self transfer rejected 400");
        Other3 -> fail(Counters, "P03 self transfer rejected 400", Other3)
    end,
    %% P04 removed 成员作 target = 409
    ok = fixture_org(Conn, 800000002, 800000004, [{member, 800000005}]),
    {ok, _} = x(Conn, <<"UPDATE organization_member SET status='removed'"
                        " WHERE organization_id=800000002 AND user_id=800000005">>),
    case organization_owner_transfer:transfer(800000004, 800000002, 800000005) of
        {error, {409, _}} -> pass(Counters, "P04 removed target rejected 409");
        Other4 -> fail(Counters, "P04 removed target rejected 409", Other4)
    end,
    %% P05 Agent 作 target = 409（C04/C12：仅 Human）
    ok = fixture_agent(Conn, 800000006),
    {ok, _} = x(Conn, <<"INSERT INTO organization_member"
                        " (organization_id,user_id,role,status,joined_at,created_at,updated_at)"
                        " VALUES (800000002,800000006,'member','active',"
                        "CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP)">>),
    case organization_owner_transfer:transfer(800000004, 800000002, 800000006) of
        {error, {409, _}} -> pass(Counters, "P05 agent target rejected 409");
        Other5 -> fail(Counters, "P05 agent target rejected 409", Other5)
    end,
    %% P06 不存在的组织 = 404
    case organization_owner_transfer:transfer(800000004, 800000099, 800000005) of
        {error, {404, _}} -> pass(Counters, "P06 missing org rejected 404");
        Other6 -> fail(Counters, "P06 missing org rejected 404", Other6)
    end,
    ok.

%%--------------------------------------------------------------------
%% A01-A08: DB 不变量矩阵（SQL 正反例，与 transfer command 等价的语句序列）
%%--------------------------------------------------------------------
db_probes(Conn, Counters) ->
    ok = fixture_base(Conn),
    %% A01 正常 transfer 语句序列（先降旧 → 再升新 → 最后改投影）提交成功
    ok = fixture_org(Conn, 800000011, 800000011, [{admin, 800000012}]),
    {ok, _} = tx(Conn, fun(C) ->
        {ok, _} = x(C, <<"SELECT id FROM organization WHERE id=800000011 FOR UPDATE">>),
        {ok, _} = x(C, <<"UPDATE organization_member SET role='admin',updated_at=CURRENT_TIMESTAMP"
                         " WHERE organization_id=800000011 AND user_id=800000011"
                         " AND role='owner' AND status='active'">>),
        {ok, _} = x(C, <<"UPDATE organization_member SET role='owner',updated_at=CURRENT_TIMESTAMP"
                         " WHERE organization_id=800000011 AND user_id=800000012"
                         " AND status='active' AND role IN ('admin','member')">>),
        {ok, _} = x(C, <<"UPDATE organization SET owner_id=800000012,updated_at=CURRENT_TIMESTAMP"
                         " WHERE id=800000011">>),
        commit
    end),
    case {owner_of(Conn, 800000011), owner_member(Conn, 800000011),
          admin_member(Conn, 800000011, 800000011)} of
        {800000012, 800000012, true} -> pass(Counters, "A01 transfer sequence commits");
        OtherA -> fail(Counters, "A01 transfer sequence commits", OtherA)
    end,
    %% A02 双 owner 行：partial unique index 即时拒绝（23505）
    ok = fixture_org(Conn, 800000013, 800000013, [{admin, 800000014}]),
    {aborted, <<"23505">>} = tx(Conn, fun(C) ->
        {ok, _} = x(C, <<"UPDATE organization_member SET role='owner'"
                         " WHERE organization_id=800000013 AND user_id=800000014 AND status='active'">>),
        commit
    end),
    pass(Counters, "A02 double active owner rejected by unique index"),
    %% A03 零 owner：绕过 guard 直接降级 → 提交时 member invariant 拒（23514）
    ok = fixture_org(Conn, 800000015, 800000015, []),
    set_guard_disabled(Conn, true),
    {aborted, <<"23514">>} = tx(Conn, fun(C) ->
        {ok, _} = x(C, <<"UPDATE organization_member SET role='member'"
                         " WHERE organization_id=800000015 AND user_id=800000015 AND role='owner'">>),
        commit
    end),
    set_guard_disabled(Conn, false),
    pass(Counters, "A03 zero owner commit rejected by member invariant"),
    %% A04 Agent 成 owner：禁 guard 后降 Human + 升 Agent → 提交时 invariant 拒（23514）
    ok = fixture_agent(Conn, 800000016),
    ok = fixture_org(Conn, 800000017, 800000017, []),
    {ok, _} = x(Conn, <<"INSERT INTO organization_member"
                        " (organization_id,user_id,role,status,joined_at,created_at,updated_at)"
                        " VALUES (800000017,800000016,'member','active',"
                        "CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP)">>),
    set_guard_disabled(Conn, true),
    {aborted, <<"23514">>} = tx(Conn, fun(C) ->
        {ok, _} = x(C, <<"UPDATE organization_member SET role='member'"
                         " WHERE organization_id=800000017 AND user_id=800000017 AND role='owner'">>),
        {ok, _} = x(C, <<"UPDATE organization_member SET role='owner'"
                         " WHERE organization_id=800000017 AND user_id=800000016 AND status='active'">>),
        commit
    end),
    set_guard_disabled(Conn, false),
    pass(Counters, "A04 agent owner commit rejected by member invariant"),
    %% A05 投影不一致：禁 sync + guard，改 owner_id 不动成员行 → 提交时 org invariant 拒（23514）
    ok = fixture_org(Conn, 800000018, 800000018, [{admin, 800000019}]),
    set_guard_disabled(Conn, true),
    {ok, _} = x(Conn, <<"ALTER TABLE organization DISABLE TRIGGER trg_organization_owner_member_sync">>),
    {aborted, <<"23514">>} = tx(Conn, fun(C) ->
        {ok, _} = x(C, <<"UPDATE organization SET owner_id=800000019 WHERE id=800000018">>),
        commit
    end),
    {ok, _} = x(Conn, <<"ALTER TABLE organization ENABLE TRIGGER trg_organization_owner_member_sync">>),
    set_guard_disabled(Conn, false),
    pass(Counters, "A05 projection drift commit rejected by org invariant"),
    %% A06 未转移先降级（guard 启用）：提交时 deferred guard 拒（23514）
    ok = fixture_org(Conn, 800000020, 800000020, []),
    {aborted, <<"23514">>} = tx(Conn, fun(C) ->
        {ok, _} = x(C, <<"UPDATE organization_member SET role='admin'"
                         " WHERE organization_id=800000020 AND user_id=800000020 AND role='owner'">>),
        commit
    end),
    pass(Counters, "A06 demote without transfer rejected by deferred guard"),
    %% A07 未转移删 owner 用户：RESTRICT 稳定拒绝（23503），组织保留
    ok = fixture_org(Conn, 800000021, 800000021, []),
    {aborted, DelCode} = tx(Conn, fun(C) ->
        {ok, _} = x(C, <<"DELETE FROM \"user\" WHERE id=800000021">>),
        commit
    end),
    %% RESTRICT 类 FK 删除拒绝：23503 foreign_key_violation / 23001 restrict_violation
    true = (DelCode =:= <<"23503">> orelse DelCode =:= <<"23001">>),
    {ok, _, [{<<"1">>}]} = q(Conn, <<"SELECT count(*)::text FROM organization WHERE id=800000021">>),
    pass(Counters, "A07 delete owner user rejected by RESTRICT (org survives)"),
    %% A08 transfer 后删除旧 owner 用户：成员行随 CASCADE 清理，组织与新 owner 保留
    ok = fixture_org(Conn, 800000022, 800000022, [{admin, 800000023}]),
    {ok, _} = tx(Conn, fun(C) ->
        {ok, _} = x(C, <<"SELECT id FROM organization WHERE id=800000022 FOR UPDATE">>),
        {ok, _} = x(C, <<"UPDATE organization_member SET role='admin'"
                         " WHERE organization_id=800000022 AND user_id=800000022 AND role='owner'">>),
        {ok, _} = x(C, <<"UPDATE organization_member SET role='owner'"
                         " WHERE organization_id=800000022 AND user_id=800000023 AND status='active'">>),
        {ok, _} = x(C, <<"UPDATE organization SET owner_id=800000023 WHERE id=800000022">>),
        commit
    end),
    {ok, _} = x(Conn, <<"DELETE FROM \"user\" WHERE id=800000022">>),
    case {owner_of(Conn, 800000022), owner_member(Conn, 800000022)} of
        {800000023, 800000023} -> pass(Counters, "A08 post-transfer delete of old owner keeps org");
        OtherB -> fail(Counters, "A08 post-transfer delete of old owner keeps org", OtherB)
    end,
    ok.

set_guard_disabled(Conn, true) ->
    {ok, _} = x(Conn,
        <<"ALTER TABLE organization_member DISABLE TRIGGER trg_organization_primary_owner_member_guard">>),
    ok;
set_guard_disabled(Conn, false) ->
    {ok, _} = x(Conn,
        <<"ALTER TABLE organization_member ENABLE TRIGGER trg_organization_primary_owner_member_guard">>),
    ok.

%%--------------------------------------------------------------------
%% C01: 并发 transfer（expected-version = 锁内 owner 快照；恰好一个成功序列）
%%--------------------------------------------------------------------
concurrency_probe(Conn, Conn2, Counters) ->
    ok = fixture_base(Conn),
    ok = fixture_org(Conn, 800000031, 800000031,
                     [{admin, 800000032}, {member, 800000033}]),
    Parent = self(),
    W1 = spawn(fun() -> Parent ! {td, self(), transfer_sequence(Conn, 800000031, 800000031, 800000032)} end),
    W2 = spawn(fun() -> Parent ! {td, self(), transfer_sequence(Conn2, 800000031, 800000031, 800000033)} end),
    R1 = receive {td, W1, X1} -> X1 after 15000 -> timeout end,
    R2 = receive {td, W2, X2} -> X2 after 15000 -> timeout end,
    OkCount = length([ok || R <- [R1, R2], R =:= {ok, committed}]),
    case {OkCount, owner_of(Conn, 800000031), owner_member(Conn, 800000031)} of
        {1, 800000032, 800000032} ->
            pass(Counters, "C01 concurrent transfer: exactly one success sequence");
        OtherC ->
            fail(Counters, "C01 concurrent transfer", {R1, R2, OtherC})
    end.

%% transfer command 的 DB 语句序列（含锁内 owner_id 快照校验 = expected-version）
transfer_sequence(Conn, OrgId, ActorUid, TargetUid) ->
    IdB = integer_to_binary(OrgId),
    tx(Conn, fun(C) ->
        {ok, _, [{DbOwner}]} = q(C,
            [<<"SELECT owner_id FROM organization WHERE id=">>, IdB, <<" FOR UPDATE">>]),
        DbOwner = ActorUid,  % 锁内快照校验；不匹配即异常回滚
        {ok, _} = x(C, [<<"UPDATE organization_member SET role='admin',updated_at=CURRENT_TIMESTAMP"
                          " WHERE organization_id=">>, IdB,
                         <<" AND user_id=">>, integer_to_binary(ActorUid),
                         <<" AND role='owner' AND status='active'">>]),
        {ok, _} = x(C, [<<"UPDATE organization_member SET role='owner',updated_at=CURRENT_TIMESTAMP"
                          " WHERE organization_id=">>, IdB,
                         <<" AND user_id=">>, integer_to_binary(TargetUid),
                         <<" AND status='active' AND role IN ('admin','member')">>]),
        {ok, _} = x(C, [<<"UPDATE organization SET owner_id=">>, integer_to_binary(TargetUid),
                         <<",updated_at=CURRENT_TIMESTAMP WHERE id=">>, IdB]),
        commit
    end).

%%--------------------------------------------------------------------
%% fixture / sql helpers
%%--------------------------------------------------------------------
fixture_base(Conn) ->
    cleanup(Conn),
    {ok, _} = x(Conn, <<"INSERT INTO \"user\" (id,password,account,reg_ip,reg_cosv,account_type)"
                        " SELECT i, 'x', 'org01-u' || i, '127.0.0.1', '', 0"
                        " FROM generate_series(800000001, 800000033) AS i">>),
    ok.

fixture_agent(Conn, Uid) ->
    UidB = integer_to_binary(Uid),
    {ok, _} = x(Conn, [<<"INSERT INTO \"user\" (id,password,account,reg_ip,reg_cosv,account_type)"
                         " VALUES (">>, UidB, <<",'x','org01-agent','127.0.0.1','',1)"
                         " ON CONFLICT (id) DO UPDATE SET account_type = EXCLUDED.account_type">>]),
    ok.

fixture_org(Conn, OrgId, OwnerUid, Extra) ->
    OrgB = integer_to_binary(OrgId),
    {ok, _} = x(Conn, [<<"INSERT INTO organization (id,name,owner_id) VALUES (">>,
                       OrgB, <<",'org01-org',">>, integer_to_binary(OwnerUid), <<")">>]),
    lists:foreach(
        fun({Role, Uid}) ->
            RoleB = atom_to_binary(Role),
            UidB = integer_to_binary(Uid),
            Sql = <<"INSERT INTO organization_member"
                    " (organization_id,user_id,role,status,joined_at,created_at,updated_at)"
                    " VALUES (", OrgB/binary, ",", UidB/binary, ",'", RoleB/binary,
                   "','active',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP)">>,
            {ok, _} = x(Conn, Sql)
        end,
        Extra),
    ok.

cleanup_sqls() ->
    [<<"DELETE FROM organization_business_identity_assignment"
       " WHERE organization_id BETWEEN 800000001 AND 800000099;">>,
     <<"DELETE FROM organization WHERE id BETWEEN 800000001 AND 800000099;">>,
     <<"DELETE FROM \"user\" WHERE id BETWEEN 800000001 AND 800000099;">>].

cleanup(Conn) ->
    lists:foreach(fun(Sql) -> _ = x(Conn, Sql) end, cleanup_sqls()),
    ok.

owner_of(Conn, OrgId) ->
    {ok, _, [{V}]} = q(Conn,
        [<<"SELECT owner_id FROM organization WHERE id=">>, integer_to_binary(OrgId)]),
    V.

owner_member(Conn, OrgId) ->
    {ok, _, [{V}]} = q(Conn,
        [<<"SELECT coalesce(max(user_id),0) FROM organization_member"
           " WHERE organization_id=">>, integer_to_binary(OrgId),
         <<" AND role='owner' AND status='active'">>]),
    V.

admin_member(Conn, OrgId, Uid) ->
    {ok, _, [{N}]} = q(Conn,
        [<<"SELECT count(*) FROM organization_member WHERE organization_id=">>,
         integer_to_binary(OrgId), <<" AND user_id=">>, integer_to_binary(Uid),
         <<" AND role='admin' AND status='active'">>]),
    N =:= 1.

q(Conn, Sql) -> epgsql:equery(Conn, iolist_to_binary(Sql), []).
%% 统一写语句封装：UPDATE/DELETE/INSERT 返回 {ok, Count}；SELECT 返回 {ok, 0}；
%% 失败抛异常（由 tx 捕获归类）。
x(Conn, Sql) ->
    case epgsql:equery(Conn, iolist_to_binary(Sql), []) of
        {ok, N} when is_integer(N) -> {ok, N};
        {ok, _, _} -> {ok, 0};
        {error, Reason} -> erlang:error({sql_failed, Reason})
    end.

%% 事务执行：Fun 返回 commit 则 COMMIT；语句异常/throw 则 ROLLBACK。
%% 返回 {ok, _}（提交成功）或 {aborted, SqlStateBinary}。
tx(Conn, Fun) ->
    {ok, _, _} = epgsql:squery(Conn, "BEGIN"),
    Exec =
        try {ok, Fun(Conn)}
        catch _Class:Reason -> {exception, Reason}
        end,
    case Exec of
        {ok, commit} ->
            case epgsql:squery(Conn, "COMMIT") of
                {ok, _, _} -> {ok, committed};
                {error, CommitErr} ->
                    {ok, _, _} = epgsql:squery(Conn, "ROLLBACK"),
                    {aborted, sqlstate(CommitErr)}
            end;
        {exception, ExceptionReason} ->
            {ok, _, _} = epgsql:squery(Conn, "ROLLBACK"),
            {aborted, sqlstate(ExceptionReason)};
        Other ->
            {ok, _, _} = epgsql:squery(Conn, "ROLLBACK"),
            {aborted, {rolled_back, Other}}
    end.

sqlstate({sql_failed, E}) -> sqlstate(E);
sqlstate({error, _S, Code, _Cn, _Msg, _Extra}) when is_binary(Code) -> Code;
sqlstate(Other) -> Other.

pass(Counters, Name) ->
    counters:add(Counters, 1, 1),
    io:format("PROBE ~s PASS~n", [Name]).

fail(_Counters, Name, Detail) ->
    io:format("PROBE ~s FAIL detail=~p~n", [Name, Detail]),
    halt(1).

env(K, D) -> case os:getenv(K) of false -> D; V -> V end.
