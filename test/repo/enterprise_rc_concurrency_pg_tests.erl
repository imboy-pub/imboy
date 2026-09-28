%% enterprise_rc_concurrency_pg_tests
%% FULL-07 §8 —— 发布候选的**真 PG 并发**与**查询计划**证据（不是 mock 断言）。
%%
%% §8 原文：「真 PG 并发授权撤销、idempotency、outbox claim、retention query plan」。
%% 本套件覆盖其中三项 + 撤销的**下一请求失效**闭环；outbox claim 由 FULL-03 已落地
%% 的真库用例 `enterprise_webhook_migration_pg_tests:concurrent_claim_single_winner`
%% 覆盖（本套件不重写副本，见 checkpoint 的交叉引用）。
%%
%% ① 并发授权撤销：N 条**独立连接**同时对同一 Grant 做 CAS 撤销 → 恰一个赢家
%%    （其余 version_conflict），DB 版本只 +1，revoked_by 是赢家。
%% ② 撤销对下一请求立即生效：生产读面 effective_scopes_tx（请求链取
%%    granted_scopes 的同源函数）在撤销后看不到该 scope —— 不是"下次登录才生效"。
%% ③ 并发 idempotency：同 key 同 digest 的 N 条并发 begin → 恰一个 inserted，
%%    其余 pending；库内只有一行。
%% ④ retention 查询计划：可 purge 视图查询必须走部分索引 i_ear_purgeable，
%%    严禁 Seq Scan —— 用 2000 行真数据 + ANALYZE 后的 EXPLAIN(JSON) 断言。
%%
%% 一次性 marker 库（inttest_marker_db 配方，env 前缀 FULL07RC_INTTEST）：空库
%% 全量 up（erlang_migrate strict）→ 合同 oracle。并发用例自带独立连接（真并发，
%% 不是同一连接上的顺序调用）。ID 段 998xxx。
%%
%% 运行：make eunit-local t=enterprise_rc_concurrency_pg_tests

-module(enterprise_rc_concurrency_pg_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

%% 998 段独立 ID
-define(ORG_A, 998101).
-define(OWNER, 998001).
-define(PRIN, 998014).
-define(APP_A, 998201).
-define(GRANT_ID_BASE, 998301).
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
    lists:foreach(
        fun(Name) ->
            case lists:member(Name, elib_tsid:registered()) of
                true -> ok;
                false -> elib_tsid:register(Name)
            end
        end,
        [group_info, enterprise_message, enterprise_audit_event]
    ),
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
        C,
        ?ORG_A,
        <<"full07rc-oa-a">>,
        <<"full07rc org A oa"/utf8>>,
        {?PRIN, [<<"messages:send">>, <<"application:read">>]}
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
new_grant(State, C, IdemKey) ->
    {ok, G} = enterprise_application_grant_repo:create_tx(C, ?ORG_A, app_a(State), #{
        scopes => [<<"messages:send">>],
        workspace_scope_kind => none,
        expires_at => future_rfc3339(),
        idempotency_key => IdemKey
    }),
    #{id => maps:get(<<"id">>, G), version => maps:get(<<"version">>, G)}.

future_rfc3339() ->
    Secs = calendar:datetime_to_gregorian_seconds(calendar:universal_time()) + 86400,
    {{Y, Mo, D}, {H, Mi, S}} = calendar:gregorian_seconds_to_datetime(Secs),
    iolist_to_binary(
        io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ", [Y, Mo, D, H, Mi, S])
    ).

%% 事务型用例统一走这里：先清掉上一个用例可能残留的失败事务
%% （PG 一旦 25P02，后续语句全被忽略——不清理会把一个失败放大成一片假红）。
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
                {"grant_revoke_concurrent_cas_single_winner",
                    {timeout, 200, fun() -> grant_revoke_concurrent(State) end}},
                {"revoke_effective_on_next_request",
                    {timeout, 200, fun() -> revoke_effective_next_request(State) end}},
                {"idempotency_concurrent_single_effect",
                    {timeout, 200, fun() -> idempotency_concurrent(State) end}},
                {"retention_query_plan_uses_partial_index",
                    {timeout, 300, fun() -> retention_query_plan(State) end}}
            ]}
        end}}.

%% 8 条独立连接同时用同一个 expected_version 撤销 → 恰 1 个 ok，7 个 version_conflict；
%% 库内 version 只 +1（没有"撤销被写了 8 次"这类丢失更新）。
grant_revoke_concurrent(State) ->
    C = maps:get(conn, State),
    ok = begin_tx(C),
    G = new_grant(State, C, <<"full07rc-idem-revoke">>),
    ok = exec(C, <<"COMMIT">>),
    N = 8,
    Refs = [
        in_own_tx(State, fun(Cc) ->
            enterprise_application_grant_repo:revoke_tx(
                Cc, ?ORG_A, app_a(State), maps:get(id, G), maps:get(version, G), ?OWNER
            )
        end)
     || _ <- lists:seq(1, N)
    ],
    Results = await_all(Refs),
    %% 每条分支自己的事务必须成功提交（不是连接/事务层面的失败）
    ?assert(
        lists:all(
            fun(R) -> element(1, R) =:= ok end,
            Results
        )
    ),
    Outcomes = [R || {ok, R} <- Results],
    Winners = [R || R <- Outcomes, R =:= ok],
    Losers = [R || R <- Outcomes, R =/= ok],
    ?assertEqual(1, length(Winners)),
    ?assertEqual(N - 1, length(Losers)),
    %% 输家必须全部是 **CAS 语义拒绝**：与赢家并发（先看到 active+version=1）者
    %% version_conflict；在赢家提交后才读到行者为 already_revoked。两者都是
    %% 「没有写成功」的合法语义；绝不允许出现 DB 异常或 internal_error ——
    %% 那意味着撤销路径在并发下崩了，而不是被正确拒绝。
    LoserReasons = [
        case R of
            {error, X} -> X;
            Other -> Other
        end
     || R <- Losers
    ],
    ?assertEqual([], [R || R <- LoserReasons, R =/= version_conflict, R =/= already_revoked]),
    %% CAS 闸门本身（确定性，不依赖调度）：stale version → version_conflict；
    %% 正确 version → ok；再撤一次 → already_revoked。上面 8 路并发的输家究竟
    %% 停在哪个分支由调度决定（两者都是「没写成功」），所以闸门语义在这里用
    %% 确定性路径单独钉死，不把 flaky 的「至少一个 version_conflict」当断言。
    G2 = new_grant(State, C, <<"full07rc-idem-casgate">>),
    ?assertEqual(
        {error, version_conflict},
        enterprise_application_grant_repo:revoke_tx(
            C, ?ORG_A, app_a(State), maps:get(id, G2), maps:get(version, G2) + 41, ?OWNER
        )
    ),
    ?assertEqual(
        ok,
        enterprise_application_grant_repo:revoke_tx(
            C, ?ORG_A, app_a(State), maps:get(id, G2), maps:get(version, G2), ?OWNER
        )
    ),
    ?assertEqual(
        {error, already_revoked},
        enterprise_application_grant_repo:revoke_tx(
            C, ?ORG_A, app_a(State), maps:get(id, G2), maps:get(version, G2), ?OWNER
        )
    ),
    Row = one(
        C,
        <<
            "SELECT status, version, revoked_by_user_id, revoked_at IS NOT NULL AS has_ts"
            " FROM enterprise_application_grant WHERE organization_id = $1"
            " AND application_id = $2 AND id = $3"
        >>,
        [?ORG_A, app_a(State), maps:get(id, G)]
    ),
    ?assertEqual(<<"revoked">>, maps:get(<<"status">>, Row)),
    ?assertEqual(maps:get(version, G) + 1, maps:get(<<"version">>, Row)),
    ?assertEqual(?OWNER, maps:get(<<"revoked_by_user_id">>, Row)),
    ?assertEqual(true, maps:get(<<"has_ts">>, Row)),
    ok.

%% 撤销对**下一请求**立即生效：生产读面 effective_scopes_tx 立刻看不到该 scope
%% （不是"下次登录才生效"，也不是缓存过期才生效）。
revoke_effective_next_request(State) ->
    C = maps:get(conn, State),
    ok = begin_tx(C),
    G = new_grant(State, C, <<"full07rc-idem-effective">>),
    %% 撤销前：生产读面看得到 messages:send
    {ok, Before} = enterprise_application_grant_repo:effective_scopes_tx(C, ?ORG_A, app_a(State)),
    ?assert(lists:member(<<"messages:send">>, Before)),
    ?assertEqual(
        {ok, true}, enterprise_application_grant_repo:grant_governed_tx(C, ?ORG_A, app_a(State))
    ),
    ok = enterprise_application_grant_repo:revoke_tx(
        C, ?ORG_A, app_a(State), maps:get(id, G), maps:get(version, G), ?OWNER
    ),
    %% 撤销后同一连接的下一次读取（= 下一个请求）立刻失效
    {ok, After} = enterprise_application_grant_repo:effective_scopes_tx(C, ?ORG_A, app_a(State)),
    ?assertNot(lists:member(<<"messages:send">>, After)),
    ?assertEqual([], After),
    %% 受治理标记仍为真（撤销不解除治理，只清空有效 scope）—— 防止"撤销后
    %% 退回无治理自由通过"的降级路径
    ?assertEqual(
        {ok, true}, enterprise_application_grant_repo:grant_governed_tx(C, ?ORG_A, app_a(State))
    ),
    %% 治理读面仍能看到 revoked 行（审计可见）
    {ok, Rows} = enterprise_application_grant_repo:list_tx(C, ?ORG_A, app_a(State)),
    ?assert(
        lists:any(
            fun(R) ->
                maps:get(<<"id">>, R) =:= maps:get(id, G) andalso
                    maps:get(<<"status">>, R) =:= <<"revoked">>
            end,
            Rows
        )
    ),
    ok = end_tx(C),
    ok.

%% N 条独立连接并发对同一 (key, digest) 做 begin_tx → 恰一个 inserted，其余 pending；
%% 库里只有一行（幂等键的唯一性由 DB 唯一约束兜底）。
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
