%% 真 PG 并发幂等及 retention 查询计划回归。
-module(enterprise_rc_concurrency_pg_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

%% 998 段独立 ID
-define(ORG_A, 998101).
-define(OWNER, 998001).
-define(PRIN, 998014).
-define(APP_A, 998201).
%% retention 压量：2000 行足够让规划器在"无索引可用"时选 Seq Scan
-define(PLAN_ROWS, 2000).

%%%===================================================================
%%% Fixture
%%%===================================================================

setup_conn() ->
    try
        elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3})
    catch
        _:_ -> ok
    end,
    {ok, _} = application:ensure_all_started(throttle),
    State = inttest_marker_db:provision(#{
        env_prefix => <<"FULL07RC_INTTEST">>,
        connect_extra => #{codecs => [{epgsql_codec_rfc3339_bin, []}]}
    }),
    C = maps:get(conn, State),
    Extra =
        try
            ok = exec(C, <<"BEGIN">>),
            E = seed_matrix(C),
            ok = exec(C, <<"COMMIT">>),
            E
        catch
            Class:Reason:Stack ->
                _ = exec_quiet(C, <<"ROLLBACK">>),
                inttest_marker_db:release(State),
                erlang:raise(Class, {fixture_seed_failed, Reason}, Stack)
        end,
    maps:merge(State, Extra).

close_conn(State) ->
    inttest_marker_db:release(State),
    ok.

connect_marker(State) ->
    #{host := Host, port := Port, username := User, password := Pass} = maps:get(server, State),
    #{db := Db} = State,
    {ok, Conn} = inttest_marker_db:safe_connect(#{
        host => Host,
        port => Port,
        username => User,
        password => Pass,
        database => Db,
        timeout => 10000,
        codecs => [{epgsql_codec_rfc3339_bin, []}]
    }),
    Conn.

exec(C, IoData) ->
    Sql = iolist_to_binary(IoData),
    case elib_pg:query(C, Sql, []) of
        {ok, _} -> ok;
        {error, Reason} -> erlang:error({sql_error, Reason, Sql})
    end.

exec_quiet(C, IoData) ->
    _ = elib_pg:query(C, iolist_to_binary(IoData), []),
    ok.

one(C, Sql, Params) ->
    case elib_pg:query(C, Sql, Params) of
        {ok, [Row | _]} -> Row;
        {ok, []} -> #{};
        {error, Reason} -> erlang:error({sql_error, Reason, Sql})
    end.

seed_matrix(C) ->
    seed_user(C, ?OWNER, 0, 1),
    seed_user(C, ?PRIN, 0, 1),
    seed_org(C, ?ORG_A, ?OWNER),
    seed_org_member(C, ?ORG_A, ?PRIN, <<"active">>),
    {ok, App} = enterprise_application_repo:create_tx(
        C, ?ORG_A, <<"full07rc-oa-a">>, <<"full07rc org A oa"/utf8>>, ?PRIN
    ),
    #{app_a => maps:get(<<"id">>, App)}.

seed_user(C, Uid, AccountType, Status) ->
    exec(C, [
        <<"INSERT INTO \"user\" (id, password, account, account_type, status, reg_ip, reg_cosv) VALUES (">>,
        integer_to_binary(Uid),
        ", 'x', 't998_u",
        integer_to_binary(Uid),
        "', ",
        integer_to_binary(AccountType),
        ", ",
        integer_to_binary(Status),
        <<", '127.0.0.1', 'x')">>
    ]).

seed_org(C, OrgId, OwnerUid) ->
    exec(C, [
        <<"INSERT INTO organization (id, name, owner_id, status, branding, settings, created_at, updated_at) VALUES (">>,
        integer_to_binary(OrgId),
        ", 'full07rc-org', ",
        integer_to_binary(OwnerUid),
        <<", 'active', '{}'::jsonb, '{}'::jsonb, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)">>
    ]).

seed_org_member(C, OrgId, Uid, Status) ->
    exec(C, [
        <<"INSERT INTO organization_member (organization_id, user_id, role, status, joined_at, created_at, updated_at) VALUES (">>,
        integer_to_binary(OrgId),
        ", ",
        integer_to_binary(Uid),
        ", 'member', '",
        Status,
        <<"', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)">>
    ]).

app_a(State) ->
    maps:get(app_a, State).

%% 新建一条 active Grant，返回 #{id, version}
begin_tx(C) ->
    ok = exec_quiet_rollback(C),
    ok = exec(C, <<"BEGIN">>),
    ok.

end_tx(C) ->
    ok = exec_quiet_rollback(C),
    ok.

exec_quiet_rollback(C) ->
    _ = elib_pg:query(C, <<"ROLLBACK">>, []),
    ok.

%% 每条并发分支在**自己的连接 + 自己的事务**里跑（真并发，不是串行调用）
in_own_tx(State, Fun) ->
    Parent = self(),
    Ref = make_ref(),
    spawn(fun() ->
        C = connect_marker(State),
        Result =
            try
                ok = exec(C, <<"BEGIN">>),
                R = Fun(C),
                ok = exec(C, <<"COMMIT">>),
                {ok, R}
            catch
                Class:Reason -> {error, {Class, Reason}}
            after
                catch epgsql:close(C)
            end,
        Parent ! {Ref, Result}
    end),
    Ref.

await(Ref) ->
    receive
        {Ref, Result} -> Result
    after 30000 -> erlang:error({concurrent_branch_timeout, Ref})
    end.

await_all(Refs) ->
    [await(R) || R <- Refs].

%%%===================================================================
%%% ① 并发授权撤销：CAS 恰一个赢家
%%%===================================================================

%%%===================================================================
%%% Suite
%%%===================================================================

enterprise_rc_concurrency_pg_test_() ->
    {timeout, 900,
        {setup, fun setup_conn/0, fun close_conn/1, fun(State) ->
            C = maps:get(conn, State),
            {inorder, [
                {"idempotency_concurrent_single_effect",
                    {timeout, 200, fun() -> idempotency_concurrent(State) end}},
                {"retention_query_plan_uses_partial_index",
                    {timeout, 300, fun() -> retention_query_plan(State) end}}
            ]}
        end}}.

%% 8 条独立连接同时用同一个 expected_version 撤销 → 恰 1 个 ok，7 个 version_conflict；
%% 库内 version 只 +1（没有"撤销被写了 8 次"这类丢失更新）。
idempotency_concurrent(State) ->
    C = maps:get(conn, State),
    Key = <<"full07rc-idem-key">>,
    %% 幂等 v2（本 run）：request_digest/3 返回 {ok, Digest} | {error, non_canonical}。
    {ok, Digest} = enterprise_internal_idempotency:request_digest(
        <<"POST">>, <<"/api/internal/v1/messages/direct">>, #{<<"k">> => <<"v">>}
    ),
    Ctx = #{organization_id => ?ORG_A, application_id => app_a(State)},
    N = 6,
    Refs = [
        in_own_tx(State, fun(Cc) ->
            enterprise_internal_idempotency:begin_tx(
                Cc, Ctx, <<"enterprise_message">>, Key, Digest
            )
        end)
     || _ <- lists:seq(1, N)
    ],
    Results = await_all(Refs),
    ?assert(
        lists:all(
            fun(R) -> element(1, R) =:= ok end,
            Results
        )
    ),
    Outcomes = [R || {ok, R} <- Results],
    Inserted = [R || R <- Outcomes, R =:= {ok, inserted}],
    Others = lists:usort([R || R <- Outcomes, R =/= {ok, inserted}]),
    ?assertEqual(1, length(Inserted)),
    ?assertEqual(N - 1, length([R || R <- Outcomes, R =/= {ok, inserted}])),
    %% 输家只能是 pending（在途）—— 赢家已插入，其余看到未完成行
    ?assertEqual([{ok, pending}], Others),
    IdemTable = enterprise_internal_idempotency_repo:tablename(),
    Row = one(
        C,
        [
            <<"SELECT count(*) AS n FROM ">>,
            IdemTable,
            <<" WHERE organization_id = $1 AND application_id = $2 AND idempotency_key = $3">>
        ],
        [?ORG_A, app_a(State), Key]
    ),
    ?assertEqual(1, maps:get(<<"n">>, Row)),
    ok.

%% retention 可 purge 查询必须走部分索引 i_ear_purgeable（retention_until）
%% WHERE purge_state='live' AND hold_state='none'，严禁 Seq Scan。
%% 证据 = 2000 行真数据 + ANALYZE + EXPLAIN (FORMAT JSON) 的完整计划树。
retention_query_plan(State) ->
    C = maps:get(conn, State),
    Table = enterprise_attachment_retention_repo:tablename(),
    View = enterprise_attachment_retention_repo:view_purgeable(),
    AttachmentTable = attachment_repo:tablename(),
    ok = begin_tx(C),
    %% 压量：attachment(基表 FK) + enterprise_attachment_retention 各 2000 行
    %% 只写 id/path——attachment 的其它列都有默认值，而 path 上有全量唯一约束
    %% uk_attachment_path（迁移 000015 用 path 取代了旧的 md5 去重键）。
    ok = exec(C, [
        <<"INSERT INTO ">>,
        AttachmentTable,
        <<" (id, path) SELECT 998000000 + g, 'rc998/p/' || g::text FROM generate_series(1, ">>,
        integer_to_binary(?PLAN_ROWS),
        <<") AS g">>
    ]),
    ok = exec(C, [
        <<"INSERT INTO ">>,
        Table,
        <<" (attachment_id, organization_id, application_id, retention_until,">>,
        <<" hold_state, purge_state) SELECT 998000000 + g, ">>,
        integer_to_binary(?ORG_A),
        <<", ">>,
        integer_to_binary(app_a(State)),
        <<", CASE WHEN (g % 2) = 0 THEN CURRENT_TIMESTAMP - INTERVAL '1 day'">>,
        <<" ELSE CURRENT_TIMESTAMP + INTERVAL '30 days' END, 'none', 'live'">>,
        <<" FROM generate_series(1, ">>,
        integer_to_binary(?PLAN_ROWS),
        <<") AS g">>
    ]),
    ok = exec(C, [<<"ANALYZE ">>, Table]),
    ok = exec(C, [<<"ANALYZE ">>, AttachmentTable]),
    {ok, Rows} = elib_pg:query(
        C, [<<"EXPLAIN (FORMAT JSON) SELECT attachment_id FROM ">>, View], []
    ),
    %% epgsql 的 json 列以 binary 文本返回（无 json codec），先解码再断言。
    Raw = maps:get(<<"QUERY PLAN">>, hd(Rows)),
    Plan =
        case Raw of
            B when is_binary(B) -> jsone:decode(B);
            L when is_list(L) -> L
        end,
    %% EXPLAIN 顶层是「一行一个计划根」的数组（PG 返回单元素数组）
    ?assertEqual(1, length(Plan)),
    PlanNode = hd(Plan),
    PlanBin = iolist_to_binary(jsone:encode(Plan)),
    %% ① 必须出现部分索引
    ?assertNotEqual(nomatch, binary:match(PlanBin, [<<"i_ear_purgeable">>])),
    %% ② 基表上不得出现 Seq Scan（全表扫 = 留存判定在大表上退化为线性扫描）
    ?assertEqual(nomatch, binary:match(PlanBin, [<<"Seq Scan">>])),
    %% ③ 必须出现真正的索引访问路径（Index Only / Index / Bitmap Index Scan）
    ExecutionPlan = iolist_to_binary(jsone:encode(maps:get(<<"Plan">>, PlanNode))),
    %% 证据留痕（eunit 会吞掉测试进程的 stdout，故落到文件）：
    %% FULL07_RC_PLAN_OUT 指向的路径会拿到本次 EXPLAIN 的原始 JSON 计划。
    %% 未设置该 env 时静默跳过——门禁命令显式设置它，保证证据可复现可引用。
    write_plan_evidence(Raw),
    ?assertNotEqual(
        [], lists:usort([M || M <- plan_values(maps:get(<<"Plan">>, PlanNode), <<"Node Type">>)])
    ),
    ?assert(
        lists:any(
            fun(Kw) -> binary:match(ExecutionPlan, [Kw]) =/= nomatch end,
            [<<"Index Only Scan">>, <<"Index Scan">>, <<"Bitmap Index Scan">>]
        )
    ),
    ok = end_tx(C),
    ok.

%% 把 EXPLAIN 原始计划写到 FULL07_RC_PLAN_OUT（未设置则跳过）。
write_plan_evidence(Raw) ->
    case os:getenv("FULL07_RC_PLAN_OUT") of
        false ->
            ok;
        "" ->
            ok;
        Path ->
            case file:write_file(Path, Raw) of
                ok -> ok;
                {error, Reason} -> erlang:error({plan_evidence_write_failed, Path, Reason})
            end
    end.

%% 递归收集计划树里某个键的全部取值（EXPLAIN JSON 的 Plan / Plans 嵌套）。
plan_values(Node, Key) when is_map(Node) ->
    Here = [V || {K, V} <- maps:to_list(Node), K =:= Key],
    Sub =
        case maps:get(<<"Plans">>, Node, undefined) of
            L when is_list(L) -> lists:append([plan_values(N, Key) || N <- L]);
            _ -> []
        end,
    Here ++ Sub;
plan_values(_Node, _Key) ->
    [].
