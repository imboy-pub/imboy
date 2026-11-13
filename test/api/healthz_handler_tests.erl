-module(healthz_handler_tests).

%%%===================================================================
%%% @doc C-49：/healthz 探针。
%%%
%%% 判据「curl 200；PG 挂掉返 503」的关键在于**挂掉时是 503 而不是 500** ——
%%% 探针拿到 500 与拿到 503 的处置不同。
%%%
%%% T02 起 probe_db 不再走 elib_pg:query（其 with_conn/2 第二参数是初始
%%% 重试 sleep，池耗尽会累计约 1+2+3 秒长挂），改为独立有界路径
%%% pooler:take_member/2 + 受监控的 epgsql 查询。本套件同步迁移到
%%% pooler/epgsql mock，并钉住预算合同。
%%% @end
%%%===================================================================

-include_lib("eunit/include/eunit.hrl").

setup() ->
    meck:new(imboy_app, [no_link, passthrough]),
    meck:expect(imboy_app, readiness, fun() -> ready end),
    meck:new(elib_pg, [no_link, passthrough]),
    %% V10 起 /healthz = /readyz 别名（聚合 PG + TSID）。本套件测端点
    %% 合同（状态码/响应体/缓存），TSID readiness 行为已由 elib_tsid_tests
    %% 覆盖，这里统一 mock readiness 为 ready；probe_db 系列不触碰该 mock
    meck:new(elib_tsid, [no_link, passthrough]),
    meck:expect(elib_tsid, readiness, fun() -> ready end),
    %% T02：探针独立路径的依赖。pooler/epgsql 用 no_link mock；
    %% 假连接进程由用例自建自清理。
    meck:new(pooler, [no_link, passthrough]),
    meck:new(epgsql, [no_link, passthrough]),
    ok.

cleanup(_) ->
    meck:unload(imboy_app),
    catch meck:unload(elib_pg),
    catch meck:unload(elib_tsid),
    catch meck:unload(pooler),
    catch meck:unload(epgsql),
    ok.

probe_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun db_up_is_healthy/0,
        fun db_error_tuple_is_unhealthy/0,
        fun db_exception_is_unhealthy_not_crash/0,
        fun dead_borrowed_conn_is_unhealthy/0,
        fun pool_exhaustion_returns_false_within_budget/0,
        fun probe_does_not_use_elib_pg_retry_path/0,
        fun hanging_query_killed_within_deadline/0,
        fun probe_is_not_cached/0
    ]}.

db_up_is_healthy() ->
    with_conn(fun() ->
        meck:expect(epgsql, equery, fun(_C, <<"SELECT 1">>, []) ->
            {ok, [#{<<"?column?">> => 1}], [[1]]}
        end),
        ?assert(healthz_handler:probe_db())
    end).

db_error_tuple_is_unhealthy() ->
    with_conn(fun() ->
        meck:expect(epgsql, equery, fun(_C, <<"SELECT 1">>, []) -> {error, econnrefused} end),
        ?assertNot(healthz_handler:probe_db())
    end).

%% 查询异常发生在受监控的等待进程内，探针本身必须收敛成 false 而不是 500。
db_exception_is_unhealthy_not_crash() ->
    with_conn(fun() ->
        meck:expect(epgsql, equery, fun(_C, <<"SELECT 1">>, []) -> erlang:error(no_pool) end),
        ?assertNot(healthz_handler:probe_db()),
        meck:expect(epgsql, equery, fun(_C, <<"SELECT 1">>, []) -> exit(pool_dead) end),
        ?assertNot(healthz_handler:probe_db()),
        meck:expect(epgsql, equery, fun(_C, <<"SELECT 1">>, []) -> throw(nope) end),
        ?assertNot(healthz_handler:probe_db())
    end).

%% 池里借出的成员已死（epgsql 进程被杀后 pooler 尚未剔除）：fail 归还并判不健康。
dead_borrowed_conn_is_unhealthy() ->
    Conn = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    try
        exit(Conn, kill),
        meck:expect(pooler, take_member, fun(pgsql, _Ms) -> Conn end),
        meck:expect(pooler, return_member, fun(pgsql, _C, fail) -> ok end),
        meck:expect(epgsql, equery, fun(_, _, _) -> exit(should_not_query_dead_conn) end),
        ?assertNot(healthz_handler:probe_db()),
        ?assertEqual(1, meck:num_calls(pooler, return_member, [pgsql, Conn, fail]))
    after
        exit(Conn, kill)
    end.

%% T02 核心：池耗尽时探针必须在**取连接预算内**返回 false，
%% 不得沿 elib_pg 公共路径长挂 1+2+3 秒。
pool_exhaustion_returns_false_within_budget() ->
    meck:expect(pooler, take_member, fun(pgsql, Ms) when is_integer(Ms), Ms > 0, Ms =< 1000 ->
        error_no_members
    end),
    Start = erlang:monotonic_time(millisecond),
    ?assertNot(healthz_handler:probe_db()),
    Elapsed = erlang:monotonic_time(millisecond) - Start,
    %% take_member 立即返回 error_no_members（真实池里这一等待由 pooler
    %% 的排队超时承担，上限即传入的 Ms<=1000）：探针自身零额外 sleep。
    ?assert(Elapsed < 1000, {probe_took_too_long, Elapsed}),
    %% 取连接预算参数必须真实下传（区别于旧 with_conn 的初始重试 sleep）。
    ?assertEqual(1, meck:num_calls(pooler, take_member, [pgsql, 1000])),
    ?assertEqual(0, meck:num_calls(pooler, take_member, 1)).

%% 探针不得触碰 elib_pg 公共重试路径（否则预算合同失效）。
probe_does_not_use_elib_pg_retry_path() ->
    with_conn(fun() ->
        meck:expect(epgsql, equery, fun(_C, _, _) -> {ok, [], []} end),
        ?assert(healthz_handler:probe_db()),
        ?assertEqual(0, meck:num_calls(elib_pg, query, '_')),
        ?assertEqual(0, meck:num_calls(elib_pg, with_conn, '_'))
    end).

%% 查询挂死（epgsql sync_command 是 infinity 超时）：探针必须在查询预算内
%% kill 等待进程、以 fail 淘汰状态不可知的连接并返回 false。
hanging_query_killed_within_deadline() ->
    Conn = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    try
        meck:expect(pooler, take_member, fun(pgsql, _Ms) -> Conn end),
        meck:expect(pooler, return_member, fun(pgsql, _C, fail) -> ok end),
        meck:expect(epgsql, equery, fun(_C, <<"SELECT 1">>, []) ->
            timer:sleep(60000),
            {ok, [], []}
        end),
        Start = erlang:monotonic_time(millisecond),
        ?assertNot(
            healthz_handler:probe_db(#{
                take_conn_timeout_ms => 50, query_timeout_ms => 150
            })
        ),
        Elapsed = erlang:monotonic_time(millisecond) - Start,
        ?assert(Elapsed >= 150 andalso Elapsed < 2000, {deadline_not_enforced, Elapsed}),
        ?assertEqual(1, meck:num_calls(pooler, return_member, [pgsql, Conn, fail]))
    after
        exit(Conn, kill)
    end.

%% 评审发现后反转：**刻意不缓存**。初版用 persistent_term 缓存，而
%% persistent_term:put/2 每次写触发**全局 GC**（扫描所有进程），代价与进程数成正比；
%% 本节点每个 WS 连接一个进程。探活又恰好在部署期最密集（每 2s 一次）——
%% 等于在系统最吃紧时反复触发全局 GC。这条断言钉住"别再把缓存加回来"。
probe_is_not_cached() ->
    with_conn(fun() ->
        meck:expect(epgsql, equery, fun(_C, <<"SELECT 1">>, []) -> {ok, [], []} end),
        _ = healthz_handler_probe_via_cache(),
        _ = healthz_handler_probe_via_cache(),
        %% 套件隔离治理：app 常驻后后台 worker 也会调 elib_pg:query 并被
        %% passthrough meck 记账；探针现在只数自己的 SELECT 1 equery 调用。
        ?assertEqual(2, meck:num_calls(epgsql, equery, ['_', <<"SELECT 1">>, []]))
    end).

%% cached_db_ok/0 未导出，经 init/2 触发（不给生产代码开 -ifdef(TEST) 后门）
healthz_handler_probe_via_cache() ->
    meck:new(cowboy_req, [no_link, non_strict]),
    try
        meck:expect(cowboy_req, reply, fun(Code, _H, Body, Req) ->
            put(last_code, Code),
            put(last_body, Body),
            Req
        end),
        {ok, _, _} = healthz_handler:init(fake_req, #{}),
        get(last_code)
    after
        catch meck:unload(cowboy_req)
    end.

%% 状态码语义：健康 200 / 不健康 503（不是 500）
status_code_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun healthy_returns_200/0,
        fun unhealthy_returns_503/0
    ]}.

healthy_returns_200() ->
    with_conn(fun() ->
        meck:expect(epgsql, equery, fun(_C, _, _) -> {ok, [], []} end),
        ?assertEqual(200, healthz_handler_probe_via_cache())
    end).

unhealthy_returns_503() ->
    with_conn(fun() ->
        meck:expect(epgsql, equery, fun(_C, _, _) -> erlang:error(no_pool) end),
        ?assertEqual(503, healthz_handler_probe_via_cache())
    end).

%% C-51：响应必须带版本号 —— 部署就绪判断靠它区分"端口通了"和
%% "**我要的那个版本**通了"（目标色端口有残留旧进程时只探端口会误判成功）。
%% 健康与不健康两条路径都要带，否则 503 时拿不到版本没法定位是谁在占端口。
version_in_body_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun healthy_body_has_version/0,
        fun unhealthy_body_has_version/0
    ]}.

healthy_body_has_version() ->
    with_conn(fun() ->
        meck:expect(epgsql, equery, fun(_C, _, _) -> {ok, [], []} end),
        ?assertEqual(200, healthz_handler_probe_via_cache()),
        ?assertNotEqual(nomatch, binary:match(iolist_to_binary(get(last_body)), <<"\"version\":">>))
    end).

%% 评审发现：nginx 拦了 /metrics 却**没拦 /healthz**，本端点公网可达且匿名。
%% 向匿名者精确报版本号＝替攻击者做 CVE 匹配。取不到 peer 时按外网处理
%% （宁可少报版本，不可因判定失败而泄露）。
version_hidden_from_external_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun external_peer_gets_hidden_version/0
    ]}.

external_peer_gets_hidden_version() ->
    with_conn(fun() ->
        meck:expect(epgsql, equery, fun(_C, _, _) -> {ok, [], []} end),
        meck:new(cowboy_req, [no_link, non_strict]),
        try
            meck:expect(cowboy_req, peer, fun(_Req) -> {{203, 0, 113, 7}, 51000} end),
            meck:expect(cowboy_req, reply, fun(_C, _H, Body, Req) ->
                put(last_body, Body),
                Req
            end),
            {ok, _, _} = healthz_handler:init(fake_req, #{}),
            Body = iolist_to_binary(get(last_body)),
            ?assertNotEqual(nomatch, binary:match(Body, <<"hidden">>)),
            ?assertEqual(nomatch, binary:match(Body, <<"1.0">>))
        after
            catch meck:unload(cowboy_req)
        end
    end).

%% 节点列表公网可见（owner 2026-09-18）：dist 端口只绑 127.0.0.1，节点名
%% 不构成可达路径，泄露面仅限侦察信息。与 version 不同，外网也返回真实列表，
%% 本节点自身必须恒在（lists:usort([node() | nodes()])）。
nodes_in_body_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun body_has_nodes_with_self/0
    ]}.

body_has_nodes_with_self() ->
    with_conn(fun() ->
        meck:expect(epgsql, equery, fun(_C, _, _) -> {ok, [], []} end),
        ?assertEqual(200, healthz_handler_probe_via_cache()),
        Body = iolist_to_binary(get(last_body)),
        ?assertNotEqual(nomatch, binary:match(Body, <<"\"nodes\":">>)),
        Self = <<$", (atom_to_binary(node(), utf8))/binary, $">>,
        ?assertNotEqual(nomatch, binary:match(Body, Self))
    end).

unhealthy_body_has_version() ->
    with_conn(fun() ->
        meck:expect(epgsql, equery, fun(_C, _, _) -> erlang:error(no_pool) end),
        ?assertEqual(503, healthz_handler_probe_via_cache()),
        ?assertNotEqual(nomatch, binary:match(iolist_to_binary(get(last_body)), <<"\"version\":">>))
    end).

%% 公共假连接 fixture：take_member 返回活连接，归还记账。
with_conn(Fun) ->
    Conn = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    try
        meck:expect(pooler, take_member, fun(pgsql, _Ms) -> Conn end),
        %% meck:expect 对同一函数是整体替换不是追加子句：ok/fail 两种
        %% 归还形态必须写在同一个多子句 fun 里。
        meck:expect(pooler, return_member, fun
            (pgsql, _C, ok) -> ok;
            (pgsql, _C, fail) -> ok
        end),
        Fun()
    after
        Conn ! stop
    end.

%%%===================================================================
%%% T02：真实 pooler + 真实 PG 的池耗尽集成用例（peer VM 内独占执行，
%%% 不受本 VM 常驻 app 的 pgsql 池干扰；对照 elib_pg 公共路径——
%%% 旧路径池耗尽需 1+2+3 秒，新路径在取连接预算内返回）。
%%% PG 坐标经环境变量注入（定向 eunit 的 VM 不加载 sys.local.config，
%%% 同 TALK_OWNED_PG_PORT / V5_INTTEST_PG_PORT 先例）；缺变量时 skip。
%%%===================================================================

real_pool_exhaustion_bounded_test_() ->
    case real_pg_opts() of
        {ok, Opts} ->
            {timeout, 120, fun() ->
                %% run/2 在 peer 内执行断言；任何 assert 失败都会以异常传回本 VM。
                ok = isolated_pg_fixture:run(
                    fun() -> real_pool_exhaustion_bounded(Opts) end, 110
                )
            end};
        {skip, _Missing} ->
            %% eunit 没有 {skip, Reason} 顶层描述符（bad descriptor → cancel 整轮），
            %% 测试体 throw 也进不了 skip 通道（run_testfun 把 throw 收敛为 error，
            %% tty 仅支持 module_not_found/no_such_function 两种 skipped 原因）。
            %% 项目惯例：生成器返回空清单静默让位（enterprise_oa_sso_tests 同款），
            %% 缺 env 的原因留在注释与 collect_real_pg_env 结构里便于排查。
            []
    end.

%% T02_REAL_PG_*：host/port/user/db 四件套，任一缺失即整个用例 skip
%% （真实池耗尽行为只在提供独占 PG 的环境验证，禁止连共享开发库）。
real_pg_opts() ->
    case collect_real_pg_env() of
        {ok, {Host, Port, User, Db}} ->
            {ok, #{
                host => Host,
                port => Port,
                username => User,
                database => Db,
                ssl => false,
                timeout => 4000
            }};
        {skip, Missing} ->
            %% eunit 的 {skip, Reason} 要求 Reason 为 string；tuple 形态会被
            %% 判成 bad test descriptor 并把整轮 cancel（T02 实测复现）。
            {skip, "no_real_pg_env: " ++ string:join(Missing, ", ")}
    end.

collect_real_pg_env() ->
    case
        {
            os:getenv("T02_REAL_PG_HOST"),
            os:getenv("T02_REAL_PG_PORT"),
            os:getenv("T02_REAL_PG_USER"),
            os:getenv("T02_REAL_PG_DB")
        }
    of
        {Host, Port, User, Db} when is_list(Host), is_list(Port), is_list(User), is_list(Db) ->
            {ok, {
                Host,
                list_to_integer(Port),
                User,
                Db
            }};
        {H, P, U, D} ->
            {skip, [
                N
             || {N, V} <- [
                    {"T02_REAL_PG_HOST", H},
                    {"T02_REAL_PG_PORT", P},
                    {"T02_REAL_PG_USER", U},
                    {"T02_REAL_PG_DB", D}
                ],
                V =:= false
            ]}
    end.

real_pool_exhaustion_bounded(Opts) ->
    {ok, _} = application:ensure_all_started(pooler),
    {ok, _PoolSup} = pooler:new_pool(#{
        name => pgsql,
        init_count => 1,
        max_count => 1,
        queue_max => 10,
        start_mfa => {epgsql, connect, [Opts]}
    }),
    try
        %% 借走唯一成员：探针的 take_member(pgsql, 1000) 只能等 pooler
        %% 排队超时（queue_max 默认 50，FIFO）。预算 1000ms 到点必须返回
        %% error_no_members → false，而不是 elib_pg 旧路径的 6 秒长挂。
        Held = pooler:take_member(pgsql),
        ?assert(is_pid(Held)),
        Start = erlang:monotonic_time(millisecond),
        false = healthz_handler:probe_db(),
        Elapsed = erlang:monotonic_time(millisecond) - Start,
        ?assert(Elapsed >= 990 andalso Elapsed < 3000, {probe_deadline_broken, Elapsed}),
        %% 活性对照：/livez 判据不依赖 PG（纯函数合同，见 runtime_probe_test）。
        {200, live} = healthz_handler:probe(live, #{}),
        %% 归还后探针应恢复 true（真实 SELECT 1 走通）。
        pooler:return_member(pgsql, Held, ok),
        true = healthz_handler:probe_db(),
        ok
    after
        pooler:rm_pool(pgsql)
    end.
