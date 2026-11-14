%%%-------------------------------------------------------------------
%%% @doc LEE-5 QR 登录原子领取 **真实 PostgreSQL 安全验收**（正式验收模式）
%%%
%%% 【测试分类】**真实 PG 集成测试**（handler/DS/Repo 真代码 + 真库行锁
%%% 仲裁；仅 transport 层 meck cowboy_req / pooler——观察替身，模式取自
%%% scripts/cache_v221_qr_entry_dcheck.erl）。多 Erlang 节点竞争证明由
%%% dcheck 双 peer 脚本单独执行（见验收文档），本文件不冒充跨节点证明。
%%%
%%% 【验收模式】LEE5_ACCEPTANCE=1 时：
%%%   - PG 不可用 / 迁移缺失 / 真实并发测试无法执行 → **直接 FAIL**
%%%     （普通开发环境保持 skip，不降低日常可跑性）；
%%%   - PG 故障注入用例使用真实 epgsql 死连接（非 mock 返回值）。
%%%
%%% 运行：
%%%   普通模式：EUNIT_CONFIG=config/sys.closeout-local make eunit-local \
%%%               t=qr_login_claim_acceptance_tests
%%%   验收模式：LEE5_ACCEPTANCE=1 EUNIT_CONFIG=config/sys.closeout-local \
%%%               make eunit-local t=qr_login_claim_acceptance_tests
%%%
%%% 验收 oracle：同一 QR Session 的 LoginToken 在 100 路并发 Polling、
%%% 100 路并发 SSE、50+50 混合竞争、领取后响应丢失重试、PG 故障注入下
%%% ——**任何场景至多一个成功领取者**。
%%% @end
%%%-------------------------------------------------------------------
-module(qr_login_claim_acceptance_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("error_code.hrl").

-define(UID, 990777).
-define(TOKEN, <<"tok_acc_lee5">>).

%% 验收模式：PG 不可用/迁移缺失 = FAIL（普通模式 = skip）
-define(ACCEPTANCE, (os:getenv("LEE5_ACCEPTANCE") =:= "1")).

%% TEST_WITH_DB_TIMEOUT 的验收变体：错误在验收模式下必须失败而非跳过
-define(ACCEPT_WITH_DB(TimeoutSec, TestFun),
    {setup,
        fun() ->
            case eunit_runner:eunit_setup_with_db() of
                {ok, Conn} ->
                    {ok, Conn};
                {error, Reason} ->
                    {error, Reason}
            end
        end,
        fun
            ({ok, Conn}) -> eunit_runner:eunit_cleanup_db(Conn);
            (_) -> ok
        end, fun
            ({ok, _Conn}) ->
                {timeout, TimeoutSec, ?_test((TestFun)())};
            ({error, Reason}) ->
                case ?ACCEPTANCE of
                    true ->
                        {error,
                            "LEE5 acceptance FAILED: real PostgreSQL unavailable: " ++
                                io_lib:format("~0p", [Reason])};
                    false ->
                        {skip, "PG unavailable (dev mode; LEE5_ACCEPTANCE=1 to enforce)"}
                end
        end}
).

%% ===================================================================
%% 门禁：迁移必须已应用（验收模式下缺失 = FAIL）
%% ===================================================================

schema_migration_present_gate_test_() ->
    ?ACCEPT_WITH_DB(30, fun() ->
        {ok, [#{<<"regclass">> := <<"qr_login_session">>}]} = elib_pg:query(
            <<"SELECT to_regclass('public.qr_login_session')::text AS regclass">>, []
        ),
        ok
    end).

%% ===================================================================
%% 验收 1：100 路并发 Polling 恰好一个领取者
%% ===================================================================

hundred_concurrent_polling_single_claim_test_() ->
    ?ACCEPT_WITH_DB(60, fun() ->
        ST = unique_token(<<"st_acc_poll">>),
        setup_confirmed_session(ST),
        try
            install_transport_observer(),
            Results = run_barrier_workers(100, fun poll_worker/1, [ST]),
            Winners = [R || {winner, _, _} = R <- Results],
            Losers = [R || {loser, ?ERR_QR_LOGIN_ALREADY_USED} = R <- Results],
            ?assertEqual([], Results -- (Winners ++ Losers)),
            ?assertEqual(1, length(Winners)),
            [{winner, Token, ?UID}] = Winners,
            ?assertEqual(?TOKEN, Token),
            ?assertEqual(99, length(Losers)),
            %% 全部 100 个响应都得到明确结果（无崩溃/无其他错误码）
            ?assertEqual(100, length(Results)),
            %% 领取后（含响应丢失场景）重试：5203，绝不重发 token
            ?assertMatch({loser, ?ERR_QR_LOGIN_ALREADY_USED}, poll_once(ST))
        after
            catch meck:unload(cowboy_req),
            cleanup_session(ST)
        end
    end).

%% ===================================================================
%% 验收 2：100 路并发 SSE 恰好一个领取者
%% ===================================================================

hundred_concurrent_sse_single_claim_test_() ->
    ?ACCEPT_WITH_DB(60, fun() ->
        ST = unique_token(<<"st_acc_sse">>),
        setup_confirmed_session(ST),
        try
            install_transport_observer(),
            Results = run_barrier_workers(100, fun sse_worker/1, [ST]),
            Winners = [R || {winner, _, _} = R <- Results],
            Losers = [R || {loser, claimed_elsewhere} = R <- Results],
            ?assertEqual([], Results -- (Winners ++ Losers)),
            ?assertEqual(1, length(Winners)),
            [{winner, Token, Uid}] = Winners,
            ?assertEqual(?TOKEN, Token),
            ?assertEqual(?UID, Uid),
            ?assertEqual(99, length(Losers)),
            ?assertEqual(100, length(Results)),
            %% token 已被领取：任何后续领取一律拒绝
            {error, not_claimable} = qr_login_session_repo:claim_login_token(ST)
        after
            catch meck:unload(cowboy_req),
            cleanup_session(ST)
        end
    end).

%% ===================================================================
%% 验收 3：50 SSE + 50 Polling 混合竞争恰好一个领取者
%% ===================================================================

mixed_fifty_sse_fifty_polling_single_claim_test_() ->
    ?ACCEPT_WITH_DB(60, fun() ->
        ST = unique_token(<<"st_acc_mix">>),
        setup_confirmed_session(ST),
        try
            install_transport_observer(),
            Funs =
                [fun sse_worker/1 || _ <- lists:seq(1, 50)] ++
                    [fun poll_worker/1 || _ <- lists:seq(1, 50)],
            Results = run_barrier_workers(100, Funs, [ST]),
            Winners = [R || {winner, _, _} = R <- Results],
            ?assertEqual(1, length(Winners)),
            [{winner, Token, ?UID}] = Winners,
            ?assertEqual(?TOKEN, Token),
            ?assertEqual(99, length(Results) - length(Winners)),
            {error, not_claimable} = qr_login_session_repo:claim_login_token(ST)
        after
            catch meck:unload(cowboy_req),
            cleanup_session(ST)
        end
    end).

%% ===================================================================
%% 验收 4：PG 故障 fail closed——真实 epgsql 死连接注入（非 mock 返回值）
%% claim/轮询/SSE 三入口都拒绝，绝不交付凭据
%% ===================================================================

pg_failure_fail_closed_real_dead_connection_test_() ->
    ?ACCEPT_WITH_DB(60, fun() ->
        ST = unique_token(<<"st_acc_dead">>),
        setup_confirmed_session(ST),
        %% 真实打开一条到同一库的 epgsql 连接再关闭（死 socket）
        {ok, Conn} = open_pg_conn(),
        epgsql:close(Conn),
        try
            meck:new(pooler, [no_link, passthrough]),
            meck:expect(pooler, take_member, fun(pgsql) -> Conn end),
            meck:expect(pooler, return_member, fun(_, _) -> ok end),
            install_transport_observer(),
            %% repo claim：fail closed（区别于"已消费"，WARN 留痕）
            ?assertMatch(
                {error, not_claimable},
                qr_login_session_repo:claim_login_token(ST)
            ),
            %% Polling 入口：明确失败（非 0 错误码），绝无 token
            ?assert(element(1, poll_once(ST)) =/= winner),
            %% SSE 入口：claimed_elsewhere 关连接，绝无 token
            ?assertMatch({loser, claimed_elsewhere}, sse_once(ST)),
            %% 恢复连接后（同一会话）恰好领取一次——故障窗口没有凭据泄漏
            meck:unload(pooler),
            ?assertMatch(
                {ok, #{login_token := ?TOKEN}},
                qr_login_session_repo:claim_login_token(ST)
            ),
            {error, not_claimable} = qr_login_session_repo:claim_login_token(ST)
        after
            catch meck:unload(pooler),
            catch meck:unload(cowboy_req),
            cleanup_session(ST)
        end
    end).

%% ===================================================================
%% Helpers：fixture（真实 repo 状态机）与 transport 观察替身（dcheck 模式）
%% ===================================================================

-spec unique_token(binary()) -> binary().
unique_token(Prefix) ->
    Rand = crypto:strong_rand_bytes(8),
    <<Prefix/binary, "_", (base64:encode(Rand, #{mode => urlsafe}))/binary>>.

setup_confirmed_session(ST) ->
    ok = qr_login_session_repo:create(#{
        session_token => ST,
        qr_token => <<ST/binary, "_qr">>,
        device_id => <<"acc-dev">>,
        device_name => <<"LEE5 Acceptance">>,
        platform => <<"web">>,
        purpose => <<"human">>,
        expires_at_ms => erlang:system_time(millisecond) + 60000
    }),
    {ok, _} = qr_login_session_repo:transition_scan(ST, ?UID),
    {ok, _} = qr_login_session_repo:transition_confirm(ST, ?UID, ?TOKEN),
    ok.

cleanup_session(ST) ->
    {ok, _} = elib_pg:query(
        <<"DELETE FROM public.qr_login_session WHERE session_token = $1">>, [ST]
    ),
    ok.

%% 仅运输观察替身：真实 handler/DS/Repo/elib_response 全部真跑
install_transport_observer() ->
    meck:new(cowboy_req, [no_link, passthrough]),
    meck:expect(cowboy_req, method, fun(_) -> <<"GET">> end),
    meck:expect(cowboy_req, parse_qs, fun(Req) -> maps:get(qs, Req) end),
    meck:expect(cowboy_req, reply, fun(_, _, Body, Req) ->
        Req#{observed_json => jsone:decode(Body)}
    end),
    meck:expect(cowboy_req, stream_reply, fun(_, _, Req) -> Req end),
    meck:expect(cowboy_req, stream_body, fun(Body, _, _) ->
        put(observed_chunks, [iolist_to_binary(Body) | get_chunks()]),
        ok
    end).

get_chunks() ->
    case get(observed_chunks) of
        undefined -> [];
        C -> C
    end.

%% N 个 worker（Funs 为每 worker 的入口函数；单个函数则复制 N 份）同时起跑：
%% 全部 ready 后统一放行（真并发），收集各自结果。
run_barrier_workers(N, FunOrFuns, [ST]) ->
    Funs =
        case is_list(FunOrFuns) of
            true -> FunOrFuns;
            false -> lists:duplicate(N, FunOrFuns)
        end,
    Parent = self(),
    Ref = make_ref(),
    Pids = [
        begin
            F = lists:nth(I, Funs),
            spawn_link(fun() ->
                Parent ! {ready, Ref, self()},
                receive
                    {go, Ref} -> Parent ! {result, Ref, self(), F(ST)}
                after 10000 -> Parent ! {result, Ref, self(), timeout}
                end
            end)
        end
     || I <- lists:seq(1, N)
    ],
    [
        receive
            {ready, Ref, _P} -> ok
        after 10000 -> error(worker_not_ready)
        end
     || _ <- lists:seq(1, N)
    ],
    [P ! {go, Ref} || P <- Pids],
    [
        receive
            {result, Ref, _P, R} -> R
        after 15000 -> error(worker_result_timeout)
        end
     || _ <- lists:seq(1, N)
    ].

%% Polling 入口 worker：真实 handle_request(status) + 真实 elib_response。
%% 返回恒为元组：{winner, Token, Uid} | {loser, Code} | {unexpected_code, Code, Json}
poll_worker(ST) ->
    Req = #{qs => [{<<"session_token">>, ST}]},
    {stop, ReqOut, _} = qr_login_handler:handle_request(Req, #{action => status}),
    case maps:get(observed_json, ReqOut, undefined) of
        #{<<"code">> := 0, <<"payload">> := #{<<"token">> := ?TOKEN, <<"uid">> := ?UID}} ->
            {winner, ?TOKEN, ?UID};
        #{<<"code">> := ?ERR_QR_LOGIN_ALREADY_USED} ->
            {loser, ?ERR_QR_LOGIN_ALREADY_USED};
        #{<<"code">> := Code} = Json ->
            {unexpected_code, Code, Json};
        Other ->
            {unexpected_shape, Other}
    end.

poll_once(ST) ->
    poll_worker(ST).

%% SSE 入口 worker：真实 info(confirmed_ready)（服务端代领路径）。
%% 返回恒为元组：{winner, Token, Uid} | {loser, claimed_elsewhere} | {unexpected, _}
sse_worker(ST) ->
    {stop, _, _} = qr_login_sse_handler:info(
        #{<<"status">> => <<"confirmed_ready">>}, #{}, #{session_token => ST}
    ),
    Chunks = lists:reverse(get_chunks()),
    case Chunks of
        [<<"data: ", Json/binary>> | _] ->
            Body = jsone:decode(binary:part(Json, 0, byte_size(Json) - 2)),
            case maps:get(<<"token">>, Body, undefined) of
                ?TOKEN -> {winner, ?TOKEN, ?UID};
                OtherTok -> {unexpected, {token, OtherTok, Body}}
            end;
        _ ->
            case lists:member(<<": claimed_elsewhere\n\n">>, Chunks) of
                true -> {loser, claimed_elsewhere};
                false -> {unexpected, {chunks, Chunks}}
            end
    end.

sse_once(ST) ->
    case sse_worker(ST) of
        {winner, _, _} -> winner;
        Other -> Other
    end.

%% 经 pg_conf start_mfa 打开独立真实连接（故障注入用）
open_pg_conn() ->
    {ok, Conf} = application:get_env(imboy, pg_conf),
    {M, F, Args} = maps:get(start_mfa, Conf),
    apply(M, F, Args).
