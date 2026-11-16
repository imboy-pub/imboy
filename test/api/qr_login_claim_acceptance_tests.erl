%%%-------------------------------------------------------------------
%%% @doc LEE-5 QR 登录原子领取 **真实 PostgreSQL 安全验收**（正式验收模式）
%%%
%%% 【测试分类】真实 handler/DS/Repo/PG；Cowboy/pooler 是运输/连接观察替身。
%%% Dead-connection tests use real epgsql connections. The lost-reply test
%%% executes a real claim, then injects an error at the query-result boundary;
%%% it does not simulate a database failover or an actual network partition.
%%% 多 Erlang 节点证明由 dcheck 双 peer 脚本单独执行，不冒充跨节点验证。
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
            ?assertEqual(100, length(Results)),
            ?assertMatch({loser, ?ERR_QR_LOGIN_ALREADY_USED}, poll_once(ST)),
            assert_consumed(ST)
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
            assert_consumed(ST)
        after
            catch meck:unload(cowboy_req),
            cleanup_session(ST)
        end
    end).

%% ===================================================================
%% 验收 3：50 SSE + 50 Polling；逐入口检查失败结果，不把异常算作正常败者
%% ===================================================================

mixed_fifty_sse_fifty_polling_single_claim_test_() ->
    ?ACCEPT_WITH_DB(60, fun() ->
        ST = unique_token(<<"st_acc_mix">>),
        setup_confirmed_session(ST),
        try
            install_transport_observer(),
            Funs =
                lists:duplicate(50, fun(S) -> {sse, sse_worker(S)} end) ++
                    lists:duplicate(50, fun(S) -> {poll, poll_worker(S)} end),
            Results = run_barrier_workers(100, Funs, [ST]),
            Winners = [R || {_, {winner, _, _}} = R <- Results],
            Losers = [R || R <- Results, expected_mixed_loser(R)],
            ?assertEqual(100, length(Results)),
            ?assertEqual(50, length([ok || {sse, _} <- Results])),
            ?assertEqual(50, length([ok || {poll, _} <- Results])),
            ?assertEqual([], Results -- (Winners ++ Losers)),
            ?assertEqual(1, length(Winners)),
            [{_, {winner, Token, ?UID}}] = Winners,
            ?assertEqual(?TOKEN, Token),
            ?assertEqual(99, length(Losers)),
            assert_consumed(ST)
        after
            catch meck:unload(cowboy_req),
            cleanup_session(ST)
        end
    end).

expected_mixed_loser({sse, {loser, claimed_elsewhere}}) -> true;
expected_mixed_loser({poll, {loser, ?ERR_QR_LOGIN_ALREADY_USED}}) -> true;
expected_mixed_loser(_) -> false.

%% ===================================================================
%% 真实 epgsql 死连接：查询、claim、SSE、confirm CAS 明确失败且无凭据
%% ===================================================================

pg_failure_fail_closed_real_dead_connection_test_() ->
    ?ACCEPT_WITH_DB(60, fun() ->
        ST = unique_token(<<"st_acc_dead">>),
        ScannedST = unique_token(<<"st_acc_dead_confirm">>),
        setup_confirmed_session(ST),
        setup_scanned_session(ScannedST),
        {ok, Conn} = open_pg_conn(),
        epgsql:close(Conn),
        try
            meck:new(pooler, [no_link, passthrough]),
            meck:expect(pooler, take_member, fun(pgsql) -> Conn end),
            meck:expect(pooler, return_member, fun(_, _) -> ok end),
            install_transport_observer(),
            ?assertMatch(
                {error, {pg_error, _}},
                qr_login_session_repo:claim_login_token(ST)
            ),
            ?assertMatch(
                {error, {pg_error, _}},
                qr_login_session_repo:transition_confirm(ScannedST, ?UID, ?TOKEN)
            ),
            ?assertEqual({loser, ?ERR_SERVICE_UNAVAILABLE}, poll_once(ST)),
            ?assertEqual({loser, service_unavailable}, sse_once(ST)),
            meck:unload(pooler),
            %% 故障发生在写入之前：恢复后状态未被伪造/消费，仍只能领取一次。
            ?assertMatch(
                #{<<"status">> := <<"scanned">>},
                qr_login_session_repo:find_by_session_token(ScannedST)
            ),
            {ok, _} = qr_login_session_repo:transition_confirm(ScannedST, ?UID, ?TOKEN),
            lists:foreach(fun(S) ->
                ?assertMatch({ok, #{login_token := ?TOKEN}}, qr_login_session_repo:claim_login_token(S)),
                assert_consumed(S)
            end, [ST, ScannedST])
        after
            catch meck:unload(pooler),
            catch meck:unload(cowboy_req),
            cleanup_session(ST),
            cleanup_session(ScannedST)
        end
    end).

%% SELECT 查找真实成功后，在 claim SQL 边界注入存储错误。
%% A separate test above exercises a real dead epgsql connection; this test
%% isolates the second-query failure without depending on pooler retry counts.
poll_claim_sql_failure_after_lookup_test_() ->
    ?ACCEPT_WITH_DB(60, fun() ->
        ST = unique_token(<<"st_acc_claim_dead">>),
        setup_confirmed_session(ST),
        ClaimFailures = atomics:new(1, []),
        try
            install_transport_observer(),
            meck:new(elib_pg, [no_link, passthrough]),
            meck:expect(elib_pg, query, fun(Sql, Params) ->
                case binary:match(iolist_to_binary(Sql),
                    <<"UPDATE public.qr_login_session SET login_token = NULL">>) of
                    {0, _} ->
                        atomics:add(ClaimFailures, 1, 1),
                        {error, synthetic_claim_query_failure};
                    nomatch ->
                        meck:passthrough([Sql, Params])
                end
            end),
            %% The status lookup is a real PG SELECT; only the subsequent
            %% atomic claim statement is failed at the storage boundary.
            ?assertEqual({loser, ?ERR_SERVICE_UNAVAILABLE}, poll_once(ST)),
            ?assertEqual(1, atomics:get(ClaimFailures, 1)),
            meck:unload(elib_pg),
            %% No write occurred: after storage recovers the still-unconsumed
            %% credential may be claimed once, and only through the PG CAS.
            ?assertEqual({winner, ?TOKEN, ?UID}, poll_once(ST)),
            assert_consumed(ST)
        after
            catch meck:unload(elib_pg),
            catch meck:unload(cowboy_req),
            cleanup_session(ST)
        end
    end).

%% 真 PG 已提交 claim；仅丢弃返回值（边界故障注入，不声称网络/主从切换演练）。
committed_claim_reply_loss_never_reissues_test_() ->
    ?ACCEPT_WITH_DB(60, fun() ->
        ST = unique_token(<<"st_acc_lost_reply">>),
        setup_confirmed_session(ST),
        LostReplies = atomics:new(1, []),
        try
            install_transport_observer(),
            meck:new(elib_pg, [no_link, passthrough]),
            meck:expect(elib_pg, query, fun(Sql, Params) ->
                Result = meck:passthrough([Sql, Params]),
                case {Sql, Params, Result} of
                    {<<"UPDATE public.qr_login_session SET login_token = NULL,", _/binary>>,
                        [ST], {ok, [_]}} ->
                        atomics:add(LostReplies, 1, 1),
                        {error, synthetic_lost_claim_reply};
                    _ -> Result
                end
            end),
            ?assertEqual({loser, ?ERR_SERVICE_UNAVAILABLE}, poll_once(ST)),
            ?assertEqual(1, atomics:get(LostReplies, 1)),
            meck:unload(elib_pg),
            assert_consumed(ST),
            ?assertEqual({loser, ?ERR_QR_LOGIN_ALREADY_USED}, poll_once(ST)),
            ?assertEqual({loser, claimed_elsewhere}, sse_once(ST))
        after
            catch meck:unload(elib_pg),
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
    setup_scanned_session(ST),
    {ok, _} = qr_login_session_repo:transition_confirm(ST, ?UID, ?TOKEN),
    ok.

setup_scanned_session(ST) ->
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
    ok.

assert_consumed(ST) ->
    {ok, [#{<<"cleared">> := true, <<"consumed">> := true}]} = elib_pg:query(
        <<"SELECT login_token IS NULL AS cleared, consumed_at IS NOT NULL AS consumed "
          "FROM public.qr_login_session WHERE session_token = $1">>, [ST]
    ),
    ?assertEqual({error, not_claimable}, qr_login_session_repo:claim_login_token(ST)).

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

%% N 个 worker 同时起跑；每个结果必须有明确分类，异常不能当作正常败者。
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

%% Polling 入口：真实响应信封；错误必须为空 payload，不得携带凭据。
poll_worker(ST) ->
    Req = #{qs => [{<<"session_token">>, ST}]},
    {stop, ReqOut, _} = qr_login_handler:handle_request(Req, #{action => status}),
    case maps:get(observed_json, ReqOut, undefined) of
        #{<<"code">> := 0, <<"payload">> := #{<<"token">> := ?TOKEN, <<"uid">> := ?UID}} ->
            {winner, ?TOKEN, ?UID};
        #{<<"code">> := Code} = Json when
            Code =:= ?ERR_QR_LOGIN_ALREADY_USED; Code =:= ?ERR_SERVICE_UNAVAILABLE
        ->
            ?assertEqual(#{}, maps:get(<<"payload">>, Json)),
            ?assertEqual(nomatch, binary:match(jsone:encode(Json), ?TOKEN)),
            {loser, Code};
        #{<<"code">> := Code} = Json ->
            {unexpected_code, Code, Json};
        Other ->
            {unexpected_shape, Other}
    end.

poll_once(ST) ->
    poll_worker(ST).

%% Read the actual UID/frame; do not manufacture UID or accept extra credential frames.
sse_worker(ST) ->
    erase(observed_chunks),
    {stop, _, _} = qr_login_sse_handler:info(
        #{<<"status">> => <<"confirmed_ready">>}, #{}, #{session_token => ST}
    ),
    Chunks = lists:reverse(get_chunks()),
    case Chunks of
        [<<"data: ", Json/binary>>] ->
            Body = jsone:decode(binary:part(Json, 0, byte_size(Json) - 2)),
            case Body of
                #{<<"status">> := <<"confirmed">>, <<"token">> := ?TOKEN,
                    <<"uid">> := Uid, <<"uid_str">> := UidStr} ->
                    ?assertEqual(?UID, Uid),
                    ?assertEqual(integer_to_binary(?UID), UidStr),
                    {winner, ?TOKEN, Uid};
                _ -> {unexpected, {payload, Body}}
            end;
        [<<": claimed_elsewhere\n\n">>] -> {loser, claimed_elsewhere};
        [<<": service_unavailable\n\n">>] -> {loser, service_unavailable};
        _ -> {unexpected, {chunks, Chunks}}
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
