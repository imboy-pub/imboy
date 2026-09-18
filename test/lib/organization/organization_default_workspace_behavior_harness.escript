#!/usr/bin/env escript
%%! -noshell
%% ===================================================================
%% ORG-05 Explicit Default Workspace 行为矩阵 harness（一次性 PG / disposable marker）
%% -------------------------------------------------------------------
%% 用法:
%%   PGHOST=127.0.0.1 PGPORT=4395 PGUSER=imboy_user PGPASSWORD= \
%%   PGDATABASE=org05_disposable_marker_20260916 IMBOY_DIR=<worktree> \
%%   test/lib/organization/organization_default_workspace_behavior_harness.escript
%%
%% 前置: 该库已应用迁移 00000130（drill_migrate.escript up），且
%%       backfill 等值 fixtures（800900000+ 段）已由外部播种并重放 130。
%% 探针:
%%   P01-P07 = 应用层 organization_default_workspace_app get/set/clear 真调；
%%   P08     = 负例：无关系行时读取不得按 min-ID 推导（ORG-A09）；
%%   A01-A03 = DB 不变量（触发器 active 守卫 / 组合 FK 同 Org / org 派生）；
%%   A04     = 并发 create/set 恰一（两连接同 Org 首建，3 轮）；
%%   A05     = archive 同事务交接（replace-with-min-active → clear）；
%%   A06     = rename / owner transfer 不影响默认；
%%   A07     = backfill 等值断言（复算 legacy min-ID）。
%% fixture 全部使用 8009xxxxxx 段 bigint id；密码不入任何输出。
%% ===================================================================
main(_) ->
    RepoDir = env("IMBOY_DIR", "."),
    code:add_pathsa([
        filename:join([RepoDir, "ebin"]),
        filename:join([RepoDir, "test"]),
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
    ok = fixtures(Conn),
    ok = app_probes(Conn, Counters),
    ok = db_probes(Conn, Conn2, Counters),
    ok = cleanup(Conn),
    epgsql:close(Conn),
    epgsql:close(Conn2),
    io:format("~nPROBE-SUMMARY pass=~p~n", [counters:get(Counters, 1)]),
    halt(0).

conn_opts() ->
    #{host => env("PGHOST", "127.0.0.1"),
      port => list_to_integer(env("PGPORT", "4395")),
      username => env("PGUSER", "imboy_user"),
      password => env("PGPASSWORD", ""),
      database => env("PGDATABASE", "org05_disposable_marker_20260916")}.

%% epgsql 命令返回归一：无 RETURNING → {ok, N}；有 → {ok, N, Rows}
okc({ok, _}) -> ok;
okc({ok, _, _}) -> ok.

%%--------------------------------------------------------------------
%% fixtures（harness 专用 8009 段；与外部 backfill fixtures 不重叠）
%%--------------------------------------------------------------------
-define(U1, 800930001).
-define(U2, 800930002).
-define(ORG_P, 800940001).
-define(ORG_CC, 800940002).
-define(ORG_AH, 800940003).
-define(ORG_NG, 800940004).
-define(WS_P, 800950001).
-define(WS_NG, 800950004).
-define(WS_AH1, 800950005).
-define(WS_AH2, 800950006).
-define(ADM, 910000000).

fixtures(Conn) ->
    %% 上次异常中断的残留自清理（幂等），保证 P01 起于干净基线
    cleanup(Conn),
    okc(epgsql:equery(Conn,
        <<"INSERT INTO \"user\" (id, account, password, account_type, reg_ip, reg_cosv)"
          " VALUES ($1,'org05h1','x',0,'127.0.0.1',''),($2,'org05h2','x',0,'127.0.0.1','')"
          " ON CONFLICT DO NOTHING">>, [?U1, ?U2])),
    okc(epgsql:equery(Conn,
        <<"INSERT INTO organization (id, name, owner_id, status, created_at) VALUES"
          " ($1,'org05_h_app',$2,'active',now()),($3,'org05_h_cc',$2,'active',now()),"
          " ($4,'org05_h_ah',$2,'active',now()),($5,'org05_h_ng',$2,'active',now())"
          " ON CONFLICT DO NOTHING">>,
        [?ORG_P, ?U1, ?ORG_CC, ?ORG_AH, ?ORG_NG])),
    okc(epgsql:equery(Conn,
        <<"INSERT INTO organization_member (organization_id,user_id,role,status,joined_at,created_at,updated_at)"
          " SELECT d.org, $2::bigint, 'owner', 'active', now(), now(), now()"
          " FROM (VALUES ($1::bigint),($3::bigint),($4::bigint),($5::bigint)) AS d(org)"
          " ON CONFLICT DO NOTHING">>,
        [?ORG_P, ?U1, ?ORG_CC, ?ORG_AH, ?ORG_NG])),
    okc(epgsql:equery(Conn,
        <<"INSERT INTO workspace (id,name,owner_id,organization_id,status,created_at) VALUES"
          " ($1,'ws_p',$2,$3,'active',now()),($4,'ws_ng',$2,$5,'active',now()),"
          " ($6,'ws_ah1',$2,$7,'active',now()),($8,'ws_ah2',$2,$7,'active',now())"
          " ON CONFLICT (id) DO NOTHING">>,
        [?WS_P, ?U1, ?ORG_P, ?WS_NG, ?ORG_NG, ?WS_AH1, ?ORG_AH, ?WS_AH2])),
    ok.

cleanup(Conn) ->
    okc(epgsql:equery(Conn,
        <<"DELETE FROM organization_default_workspace"
          " WHERE organization_id IN ($1,$2,$3,$4)">>,
        [?ORG_P, ?ORG_CC, ?ORG_AH, ?ORG_NG])),
    okc(epgsql:equery(Conn,
        <<"DELETE FROM workspace WHERE organization_id IN ($1,$2,$3,$4)">>,
        [?ORG_P, ?ORG_CC, ?ORG_AH, ?ORG_NG])),
    %% organization 删除级联清理 organization_member（ORG-01 owner invariant
    %% 触发器会拦截「先删 owner membership」的顺序，故不单独删成员）
    okc(epgsql:equery(Conn,
        <<"DELETE FROM organization WHERE id IN ($1,$2,$3,$4)">>,
        [?ORG_P, ?ORG_CC, ?ORG_AH, ?ORG_NG])),
    okc(epgsql:equery(Conn,
        <<"DELETE FROM \"user\" WHERE id IN ($1,$2)">>, [?U1, ?U2])),
    ok.

%%--------------------------------------------------------------------
%% P01-P08: 应用层命令（真 PG + with_tx 连接池）
%%--------------------------------------------------------------------
app_probes(Conn, Counters) ->
    {ok, _} = application:ensure_all_started(pooler),
    ok = application:set_env(imboy, sql_driver, pgsql),
    %% 连接编码与生产 pg_conf 同口径：timestamptz 参数走 RFC3339 binary codec
    PoolOpts =
        maps:merge(#{name => pgsql, init_count => 1, max_count => 4, queue_max => 20},
                   #{start_mfa =>
                         {epgsql, connect,
                          [maps:put(codecs, [{epgsql_codec_rfc3339_bin, []}], conn_opts())]}}),
    {ok, _} = pooler:new_pool(PoolOpts),
    %% P01 set changed + get 回读显式行
    case organization_default_workspace_app:set(?U1, ?ORG_P, ?WS_P) of
        {ok, changed} ->
            case organization_default_workspace_app:get(?ORG_P) of
                {ok, ?WS_P} -> pass(Counters, "P01 set changed + get explicit row");
                Other1 -> fail(Counters, "P01 get after set", Other1)
            end;
        Other01 -> fail(Counters, "P01 set changed", Other01)
    end,
    %% P02 set 同值幂等
    case organization_default_workspace_app:set(?U1, ?ORG_P, ?WS_P) of
        {ok, unchanged} -> pass(Counters, "P02 set same value unchanged");
        Other2 -> fail(Counters, "P02 set idempotent", Other2)
    end,
    %% P03 cross-org set 拒（target 属 ?ORG_AH、请求 org 是 ?ORG_P）
    case organization_default_workspace_app:set(?U1, ?ORG_P, ?WS_AH1) of
        {error, {409, _}} -> pass(Counters, "P03 cross-org set rejected 409");
        Other3 -> fail(Counters, "P03 cross-org set 409", Other3)
    end,
    %% P04 archived target 拒（先归档探后恢复）
    okc(epgsql:equery(Conn,
        <<"UPDATE workspace SET status='archived' WHERE id=$1">>, [?WS_AH1])),
    R4 = organization_default_workspace_app:set(?U1, ?ORG_AH, ?WS_AH1),
    okc(epgsql:equery(Conn,
        <<"UPDATE workspace SET status='active' WHERE id=$1">>, [?WS_AH1])),
    case R4 of
        {error, {409, _}} -> pass(Counters, "P04 archived target rejected 409");
        Other4 -> fail(Counters, "P04 archived target 409", Other4)
    end,
    %% P05 missing target 404
    case organization_default_workspace_app:set(?U1, ?ORG_P, 899999999) of
        {error, {404, _}} -> pass(Counters, "P05 missing target rejected 404");
        Other5 -> fail(Counters, "P05 missing target 404", Other5)
    end,
    %% P06/P07 clear 幂等
    case organization_default_workspace_app:clear(?U1, ?ORG_P) of
        {ok, cleared} ->
            case organization_default_workspace_app:clear(?U1, ?ORG_P) of
                {ok, already_empty} -> pass(Counters, "P06/P07 clear idempotent");
                Other7 -> fail(Counters, "P07 clear already_empty", Other7)
            end;
        Other6 -> fail(Counters, "P06 clear", Other6)
    end,
    %% P08 负例（ORG-A09）：org 有 active ws 但无关系行（迁移后新增），
    %% 读取不得按 min-ID 推导命中；而 legacy SQL 会命中 ⇒ 两读法已分流
    case organization_default_workspace_app:get(?ORG_NG) of
        {error, not_set} ->
            case epgsql:equery(Conn,
                <<"SELECT id FROM workspace WHERE organization_id=$1 AND status='active'"
                  " ORDER BY id LIMIT 1">>, [?ORG_NG]) of
                {ok, _, [_]} -> pass(Counters, "P08 no-min-ID fallback (explicit only)");
                _ -> fail(Counters, "P08 legacy comparison row missing", no_row)
            end;
        Other8 -> fail(Counters, "P08 negative read not_set", Other8)
    end,
    %% A05 前置：为 ?ORG_AH 重新设置默认 = ws_ah1
    {ok, changed} = organization_default_workspace_app:set(?U1, ?ORG_AH, ?WS_AH1),
    ok.

%%--------------------------------------------------------------------
%% A01-A07: DB 不变量矩阵
%%--------------------------------------------------------------------
db_probes(Conn, Conn2, Counters) ->
    %% A01 触发器守卫：archived 目标 INSERT → 23514
    okc(epgsql:equery(Conn,
        <<"UPDATE workspace SET status='archived' WHERE id=$1">>, [?WS_AH1])),
    R1 = epgsql:equery(Conn,
        <<"INSERT INTO organization_default_workspace (organization_id, workspace_id)"
          " VALUES ($1,$2)">>, [?ORG_AH, ?WS_AH1]),
    okc(epgsql:equery(Conn,
        <<"UPDATE workspace SET status='active' WHERE id=$1">>, [?WS_AH1])),
    case R1 of
        {error, {error, error, <<"23514">>, _, _, _}} ->
            pass(Counters, "A01 archived target guard 23514");
        Other1 -> fail(Counters, "A01 archived guard 23514", Other1)
    end,
    %% A02 组合 FK 正向探针：合法同 Org 对 (ORG_P, WS_P) 通过 FK 落行。
    %% （跨 Org 原始 INSERT 在触发器派生下不可达 23503——A03 已证派生改写；
    %%   应用层跨 Org 拒绝由 P03 覆盖，FK 是触发器之下的第二道保险。）
    okc(epgsql:equery(Conn,
        <<"DELETE FROM organization_default_workspace WHERE organization_id=$1">>,
        [?ORG_P])),
    R2 = epgsql:equery(Conn,
        <<"INSERT INTO organization_default_workspace (organization_id, workspace_id)"
          " VALUES ($1,$2)">>, [?ORG_P, ?WS_P]),
    case R2 of
        {ok, 1} ->
            pass(Counters, "A02 same-org composite FK accepts valid pair");
        Other2 -> fail(Counters, "A02 same-org composite FK", Other2)
    end,
    %% A03 organization_id 由 workspace 行派生（传错 org 被覆盖）；
    %% 先清掉 ORG_AH 既有默认行，避免派生后撞 PK
    okc(epgsql:equery(Conn,
        <<"DELETE FROM organization_default_workspace WHERE organization_id=$1">>,
        [?ORG_AH])),
    case epgsql:equery(Conn,
        <<"INSERT INTO organization_default_workspace (organization_id, workspace_id)"
          " VALUES ($1,$2) RETURNING organization_id">>, [899999998, ?WS_AH1]) of
        {ok, 1, _, [{?ORG_AH}]} ->
            pass(Counters, "A03 organization_id derived from workspace row");
        Other3 -> fail(Counters, "A03 org derivation", Other3)
    end,
    %% A05 断言基线：ORG_AH 默认 = ws_ah1（覆盖 A03 的派生行）
    okc(epgsql:equery(Conn,
        <<"INSERT INTO organization_default_workspace (organization_id, workspace_id)"
          " VALUES ($1,$2) ON CONFLICT (organization_id) DO UPDATE"
          " SET workspace_id = EXCLUDED.workspace_id">>,
        [?ORG_AH, ?WS_AH1])),
    %% A04 并发 create/set 恰一（两连接同 Org 首建，3 轮）
    concurrency_exactly_one(Conn, Conn2, Counters),
    %% A05 archive 同事务交接（replace-with-min-active → clear）
    archive_handover(Conn, Counters),
    %% A06 rename / owner transfer 不影响默认（对 ?ORG_P 默认行改名/换主）
    okc(epgsql:equery(Conn,
        <<"UPDATE organization_default_workspace SET workspace_id=$2"
          " WHERE organization_id=$1">>, [?ORG_P, ?WS_P])),
    okc(epgsql:equery(Conn,
        <<"UPDATE workspace SET name='renamed', owner_id=$2 WHERE id=$1">>,
        [?WS_P, ?U2])),
    case epgsql:equery(Conn,
        <<"SELECT workspace_id FROM organization_default_workspace WHERE organization_id=$1">>,
        [?ORG_P]) of
        {ok, _, [{?WS_P}]} -> pass(Counters, "A06 rename/transfer keeps default");
        Other6 -> fail(Counters, "A06 rename/transfer", Other6)
    end,
    %% A07 backfill 等值断言（限定 backfill fixtures 的 800910000-800919999 段；
    %% 运行期探针（A04/A05）产生的增量不参与——运行期语义由 P/A 探针覆盖）
    {ok, _, [{Eq}]} = epgsql:equery(Conn,
        <<"SELECT (SELECT count(*) FROM organization_default_workspace r"
          "  WHERE r.organization_id BETWEEN 800910000 AND 800919999"
          "    AND r.workspace_id = (SELECT min(w.id) FROM workspace w"
          "   WHERE w.organization_id = r.organization_id AND w.status='active'))"
          " = (SELECT count(*) FROM organization_default_workspace"
          "  WHERE organization_id BETWEEN 800910000 AND 800919999)">>, []),
    {ok, _, [{Cov}]} = epgsql:equery(Conn,
        <<"SELECT (SELECT count(*) FROM organization_default_workspace"
          "  WHERE organization_id BETWEEN 800910000 AND 800919999)"
          " = (SELECT count(DISTINCT organization_id) FROM workspace"
          "  WHERE organization_id BETWEEN 800910000 AND 800919999"
          "    AND status='active')">>, []),
    case {Eq, Cov} of
        {true, true} -> pass(Counters, "A07 backfill equivalence (min-ID reproducible)");
        Other7 -> fail(Counters, "A07 backfill equivalence", Other7)
    end,
    ok.

%% 两连接同 Org 首建：各自事务内插 workspace + ensure_first_workspace_tx，
%% PK 冲突仲裁后该 Org 默认行必须恰一（ORG-A09 并发 create/set 恰一）。
concurrency_exactly_one(Conn, Conn2, Counters) ->
    Rounds = 3,
    lists:foreach(
        fun(Round) ->
            W1 = 800960000 + Round * 10,
            W2 = 800960000 + Round * 10 + 1,
            Self = self(),
            P1 = spawn_link(fun() ->
                Self ! {c1, tx_first(Conn, W1)}
            end),
            P2 = spawn_link(fun() ->
                Self ! {c2, tx_first(Conn2, W2)}
            end),
            wait_both([P1, P2]),
            {ok, _, [{Cnt}]} = epgsql:equery(Conn,
                <<"SELECT count(*) FROM organization_default_workspace"
                  " WHERE organization_id=$1">>, [?ORG_CC]),
            case Cnt of
                1 -> ok;
                N -> fail(Counters, {round, Round, N}, not_exactly_one)
            end
        end,
        lists:seq(1, Rounds)
    ),
    pass(Counters, "A04 concurrent create/set exactly one (3 rounds)").

tx_first(Conn, W) ->
    epgsql:equery(Conn, <<"BEGIN">>, []),
    R = epgsql:equery(Conn,
        <<"INSERT INTO workspace (id,name,owner_id,organization_id,status,created_at)"
          " VALUES ($1,'cc',$2,$3,'active',now())">>, [W, ?U1, ?ORG_CC]),
    R2 =
        case R of
            {ok, _} ->
                try organization_default_workspace_pg:ensure_first_workspace_tx(
                        Conn, ?ORG_CC, W
                    )
                catch
                    _:_ -> hook_error
                end;
            {error, _} -> ws_conflict
        end,
    epgsql:equery(Conn, <<"COMMIT">>, []),
    R2.

wait_both(Pids) ->
    [begin
         Ref = erlang:monitor(process, Pid),
         receive
             {'DOWN', Ref, process, _, _} -> ok
         end
     end
     || Pid <- Pids].

%% 归档交接：归档当前默认 → replace 为剩余最小 active；归档最后一个 → clear。
archive_handover(Conn, Counters) ->
    %% 当前默认 = ws_ah1（P 探针末尾设置），剩余 active = ws_ah2
    case workspace_logic:admin_archive(?ADM, ?WS_AH1) of
        {ok, _} ->
            case epgsql:equery(Conn,
                <<"SELECT workspace_id FROM organization_default_workspace"
                  " WHERE organization_id=$1">>, [?ORG_AH]) of
                {ok, _, [{?WS_AH2}]} ->
                    case workspace_logic:admin_archive(?ADM, ?WS_AH2) of
                        {ok, _} ->
                            {ok, _, Rows} = epgsql:equery(Conn,
                                <<"SELECT workspace_id FROM organization_default_workspace"
                                  " WHERE organization_id=$1">>, [?ORG_AH]),
                            case Rows of
                                [] -> pass(Counters, "A05 archive handover replace-then-clear");
                                Rows5 -> fail(Counters, "A05 final clear", Rows5)
                            end;
                        Other52 -> fail(Counters, "A05 archive last default", Other52)
                    end;
                {ok, _, RowsAH} -> fail(Counters, "A05 replace with min active", RowsAH)
            end;
        Other51 -> fail(Counters, "A05 archive default ws", Other51)
    end.

%%--------------------------------------------------------------------
pass(Counters, Label) ->
    counters:add(Counters, 1, 1),
    io:format("~p~n", [{pass, Label}]),
    ok.

fail(_Counters, Label, Got) ->
    io:format("~p~n", [{fail, Label, Got}]),
    erlang:error({probe_failed, Label, Got}).

env(K, D) ->
    case os:getenv(K) of
        false -> D;
        V -> V
    end.
