-module(healthz_handler_tests).

%%%===================================================================
%%% @doc C-49：/healthz 探针。
%%%
%%% 判据「curl 200；PG 挂掉返 503」的关键在于**挂掉时是 503 而不是 500** ——
%%% 探针拿到 500 与拿到 503 的处置不同，而 elib_pg 在无连接池时是**抛异常**
%%% 而不是返回 {error,_}（attach presign 那次已经踩过同一个坑）。
%%% @end
%%%===================================================================

-include_lib("eunit/include/eunit.hrl").

setup() ->
    meck:new(elib_pg, [no_link, passthrough]),
    %% 每个用例前清掉探测缓存，否则上一条的结果会串味
    catch persistent_term:erase({healthz, db}),
    ok.

cleanup(_) ->
    catch meck:unload(elib_pg),
    catch persistent_term:erase({healthz, db}),
    ok.

probe_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun db_up_is_healthy/0,
        fun db_error_tuple_is_unhealthy/0,
        fun db_exception_is_unhealthy_not_crash/0,
        fun probe_is_not_cached/0
    ]}.

db_up_is_healthy() ->
    meck:expect(elib_pg, query, fun(_Sql, _Args) -> {ok, [#{<<"?column?">> => 1}]} end),
    ?assert(healthz_handler:probe_db()).

db_error_tuple_is_unhealthy() ->
    meck:expect(elib_pg, query, fun(_Sql, _Args) -> {error, econnrefused} end),
    ?assertNot(healthz_handler:probe_db()).

%% 核心：连接池耗尽时 elib_pg **抛异常**。不 catch 的话探针会 500 而不是 503。
db_exception_is_unhealthy_not_crash() ->
    meck:expect(elib_pg, query, fun(_Sql, _Args) -> erlang:error(no_pool) end),
    ?assertNot(healthz_handler:probe_db()),
    meck:expect(elib_pg, query, fun(_Sql, _Args) -> exit(pool_dead) end),
    ?assertNot(healthz_handler:probe_db()),
    meck:expect(elib_pg, query, fun(_Sql, _Args) -> throw(nope) end),
    ?assertNot(healthz_handler:probe_db()).

%% 评审发现后反转：**刻意不缓存**。初版用 persistent_term 缓存，而
%% persistent_term:put/2 每次写触发全局 GC（扫描所有进程），代价与进程数成正比；
%% 本节点每个 WS 连接一个进程。探活又恰在部署期最密集（每 2s 一次）——
%% 等于在系统最吃紧时反复触发全局 GC。这条断言钉住"别再把缓存加回来"。
probe_is_not_cached() ->
    meck:expect(elib_pg, query, fun(_Sql, _Args) -> {ok, [#{}]} end),
    _ = healthz_handler_probe_via_cache(),
    _ = healthz_handler_probe_via_cache(),
    ?assertEqual(2, meck:num_calls(elib_pg, query, '_')).

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
    meck:expect(elib_pg, query, fun(_S, _A) -> {ok, [#{}]} end),
    ?assertEqual(200, healthz_handler_probe_via_cache()).

unhealthy_returns_503() ->
    meck:expect(elib_pg, query, fun(_S, _A) -> erlang:error(no_pool) end),
    ?assertEqual(503, healthz_handler_probe_via_cache()).

%% C-51：响应必须带版本号 —— 部署就绪判断靠它区分"端口通了"和
%% "**我要的那个版本**通了"（目标色端口有残留旧进程时只探端口会误判成功）。
%% 健康与不健康两条路径都要带，否则 503 时拿不到版本没法定位是谁在占端口。
version_in_body_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun healthy_body_has_version/0,
        fun unhealthy_body_has_version/0
    ]}.

healthy_body_has_version() ->
    meck:expect(elib_pg, query, fun(_S, _A) -> {ok, [#{}]} end),
    ?assertEqual(200, healthz_handler_probe_via_cache()),
    ?assertNotEqual(nomatch, binary:match(iolist_to_binary(get(last_body)), <<"\"version\":">>)).

%% 评审发现：nginx 拦了 /metrics 却**没拦 /healthz**，本端点公网可达且匿名。
%% 向匿名者精确报版本号＝替攻击者做 CVE 匹配。取不到 peer 时按外网处理
%% （宁可少报版本，不可因判定失败而泄露）。
version_hidden_from_external_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun external_peer_gets_hidden_version/0
    ]}.

external_peer_gets_hidden_version() ->
    meck:expect(elib_pg, query, fun(_S, _A) -> {ok, [#{}]} end),
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
    end.

unhealthy_body_has_version() ->
    meck:expect(elib_pg, query, fun(_S, _A) -> erlang:error(no_pool) end),
    ?assertEqual(503, healthz_handler_probe_via_cache()),
    ?assertNotEqual(nomatch, binary:match(iolist_to_binary(get(last_body)), <<"\"version\":">>)).
