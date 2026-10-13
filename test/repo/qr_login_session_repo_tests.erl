%%%-------------------------------------------------------------------
%%% @doc G-QR 原子消费测试（收口 C08）
%%%
%%% 真实 PG（专属临时库 imboy_cache_closeout_v1，迁移 00000170 已 apply）；
%%% 并发 claim 竞争 oracle（g-qr-atomic-consumption-proposal.md 第 6 节）：
%%% - 并发 2×N status 领取：恰好 1 个拿到 token；
%%% - SSE（服务端代领）与 status 轮询竞争：同一 token 恰好领取一次；
%%% - scan/confirm/cancel 状态机 CAS：乱序与并发下状态转移唯一；
%%% - 过期会话一律拒绝；消费后表内无 token 残留。
%%%
%%% 测试日志仅记录合成标识/计数，不记录可用凭据（C08 红线）。
%%% 运行：EUNIT_CONFIG=config/sys.closeout-local make eunit-local t=qr_login_session_repo_tests
%%% @end
%%%-------------------------------------------------------------------
-module(qr_login_session_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

-define(UID, 990001).

%% ===================================================================
%% 基础流程：create → scan → confirm → claim
%% ===================================================================

pg_session_roundtrip_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        ST = unique_token(<<"st_rt">>),
        QT = unique_token(<<"qt_rt">>),
        ok = qr_login_session_repo:create(#{
            session_token => ST,
            qr_token => QT,
            device_id => <<"dev_closeout">>,
            device_name => <<"Closeout Test">>,
            platform => <<"web">>,
            purpose => <<"human">>,
            expires_at_ms => future_ms(60000)
        }),
        %% 初始 waiting
        #{<<"status">> := <<"waiting">>} =
            qr_login_session_repo:find_by_session_token(ST),
        %% scan CAS
        {ok, #{<<"status">> := <<"scanned">>, <<"scanned_by">> := ?UID}} =
            qr_login_session_repo:transition_scan(ST, ?UID),
        %% 非 scan 人不可 confirm（权限）
        {error, not_transitioned} =
            qr_login_session_repo:transition_confirm(ST, ?UID + 1, <<"tok">>),
        %% 扫码人 confirm（原子写 token）
        {ok, _} = qr_login_session_repo:transition_confirm(ST, ?UID, <<"tok_secret">>),
        %% claim：恰好一次成功，随后 not_claimable
        {ok, #{login_token := <<"tok_secret">>, scanned_by := ?UID}} =
            qr_login_session_repo:claim_login_token(ST),
        {error, not_claimable} = qr_login_session_repo:claim_login_token(ST),
        cleanup_session(ST)
    end).

%% ===================================================================
%% G-QR 核心 oracle 1：并发 status 领取恰好一次（N 轮 × 2+ 并发）
%% ===================================================================

concurrent_claim_exactly_one_test_() ->
    ?TEST_WITH_DB_TIMEOUT(60, fun() ->
        Rounds = 100,
        Winners = concurrent_claim_rounds(Rounds),
        ?assertEqual(Rounds, Winners)
    end).

-spec concurrent_claim_rounds(non_neg_integer()) -> non_neg_integer().
concurrent_claim_rounds(0) ->
    0;
concurrent_claim_rounds(N) ->
    ST = unique_token(<<"st_cc">>),
    setup_confirmed_session(ST),
    %% 4 个并发消费者（两个后端节点的 status/status + SSE/SSE 等价竞争：
    %% PG 行锁是唯一仲裁点，多连接竞争即跨节点语义）
    Results = concurrent_claim(ST, 4),
    Winners = length([ok || {ok, _} <- Results]),
    Losers = length([error || {error, _} <- Results]),
    %% 恰好 1 个赢家、其余明确失败；无 token 残留
    ?assertEqual(1, Winners),
    ?assertEqual(3, Losers),
    #{<<"token_claimable">> := false} =
        qr_login_session_repo:find_by_session_token(ST),
    cleanup_session(ST),
    1 + concurrent_claim_rounds(N - 1).

-spec concurrent_claim(binary(), pos_integer()) -> [{ok, map()} | {error, term()}].
concurrent_claim(SessionToken, N) ->
    Parent = self(),
    Pids = [
        spawn_link(fun() ->
            Parent ! {self(), qr_login_session_repo:claim_login_token(SessionToken)}
        end)
     || _ <- lists:seq(1, N)
    ],
    [
        receive
            {Pid, R} -> R
        end
     || Pid <- Pids
    ].

%% ===================================================================
%% G-QR 核心 oracle 2：SSE 代领与 status 轮询竞争（同一 token 一次）
%% ===================================================================

sse_vs_status_race_single_claim_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        ST = unique_token(<<"st_sse">>),
        setup_confirmed_session(ST),
        %% SSE 服务端代领（qr_login_sse_handler 的 claim 路径）与
        %% status 轮询领取并发：两个调用同一 claim_login_token
        Results = concurrent_claim(ST, 2),
        Winners = length([ok || {ok, _} <- Results]),
        ?assertEqual(1, Winners),
        ?assertEqual(1, length([error || {error, _} <- Results])),
        cleanup_session(ST)
    end).

%% ===================================================================
%% 状态机 CAS：并发 scan（不同用户）/ 乱序 confirm / 并发 cancel
%% ===================================================================

concurrent_scan_single_winner_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        ST = unique_token(<<"st_cscan">>),
        QT = unique_token(<<"qt_cscan">>),
        ok = qr_login_session_repo:create(#{
            session_token => ST,
            qr_token => QT,
            device_id => <<"dev">>,
            device_name => <<"d">>,
            platform => <<"web">>,
            purpose => <<"human">>,
            expires_at_ms => future_ms(60000)
        }),
        %% 两个不同用户并发扫码：恰好一人成为 scanned_by（赢家与
        %% 返回顺序均不确定，按计数断言，与 sse_vs_status_race 同口径）
        {Results, _} =
            race_two(
                fun() -> qr_login_session_repo:transition_scan(ST, ?UID) end,
                fun() -> qr_login_session_repo:transition_scan(ST, ?UID + 1) end
            ),
        ?assertEqual(1, length([ok || {ok, _} <- Results])),
        ?assertEqual(
            1,
            length([E || {error, _} = E <- Results])
        ),
        #{<<"scanned_by">> := Winner} =
            qr_login_session_repo:find_by_session_token(ST),
        ?assert(Winner =:= ?UID orelse Winner =:= ?UID + 1),
        cleanup_session(ST)
    end).

concurrent_confirm_cancel_unique_outcome_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        ST = unique_token(<<"st_cc2">>),
        setup_scanned_session(ST),
        %% 并发 confirm 与 cancel：恰好一个成功（状态转移唯一）
        {[ConfirmR, CancelR], _} = race_two(
            fun() -> qr_login_session_repo:transition_confirm(ST, ?UID, <<"tok2">>) end,
            fun() -> qr_login_session_repo:transition_cancel(ST) end
        ),
        ExactlyOne =
            (is_tuple(ConfirmR) andalso element(1, ConfirmR) =:= ok) xor
                (CancelR =:= ok),
        ?assert(ExactlyOne),
        %% 已 confirmed 后 cancel 被拒（结果唯一：已确认即生效）
        case ConfirmR of
            {ok, _} ->
                {error, not_transitioned} =
                    qr_login_session_repo:transition_cancel(ST);
            _ ->
                ok
        end,
        cleanup_session(ST)
    end).

duplicate_confirm_is_idempotent_reject_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        ST = unique_token(<<"st_dc">>),
        setup_scanned_session(ST),
        {ok, _} = qr_login_session_repo:transition_confirm(ST, ?UID, <<"tok3">>),
        %% 重复 confirm：CAS 拒绝（不是再次写入）
        {error, not_transitioned} =
            qr_login_session_repo:transition_confirm(ST, ?UID, <<"tok3">>),
        %% token 仍可被领取恰好一次
        {ok, #{login_token := <<"tok3">>}} =
            qr_login_session_repo:claim_login_token(ST),
        {error, not_claimable} =
            qr_login_session_repo:claim_login_token(ST),
        cleanup_session(ST)
    end).

%% ===================================================================
%% 过期：全部操作一律拒绝
%% ===================================================================

expired_session_all_rejected_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        ST = unique_token(<<"st_exp">>),
        QT = unique_token(<<"qt_exp">>),
        ok = qr_login_session_repo:create(#{
            session_token => ST,
            qr_token => QT,
            device_id => <<"dev">>,
            device_name => <<"d">>,
            platform => <<"web">>,
            purpose => <<"human">>,
            %% 已过期（expires_at 在过去）
            expires_at_ms => erlang:system_time(millisecond) - 1000
        }),
        undefined = qr_login_session_repo:find_by_session_token(ST),
        {error, not_transitioned} =
            qr_login_session_repo:transition_scan(ST, ?UID),
        {error, not_transitioned} =
            qr_login_session_repo:transition_confirm(ST, ?UID, <<"tok">>),
        {error, not_claimable} =
            qr_login_session_repo:claim_login_token(ST),
        cleanup_session(ST)
    end).

%% ===================================================================
%% Helpers
%% ===================================================================

-spec unique_token(binary()) -> binary().
unique_token(Prefix) ->
    Rand = crypto:strong_rand_bytes(8),
    <<Prefix/binary, "_", (base64:encode(Rand, #{mode => urlsafe}))/binary>>.

-spec future_ms(non_neg_integer()) -> non_neg_integer().
future_ms(Sec) ->
    erlang:system_time(millisecond) + Sec * 1000.

-spec setup_scanned_session(binary()) -> ok.
setup_scanned_session(ST) ->
    QT = unique_token(<<"qt">>),
    ok = qr_login_session_repo:create(#{
        session_token => ST,
        qr_token => QT,
        device_id => <<"dev">>,
        device_name => <<"d">>,
        platform => <<"web">>,
        purpose => <<"human">>,
        expires_at_ms => future_ms(60000)
    }),
    {ok, _} = qr_login_session_repo:transition_scan(ST, ?UID),
    ok.

-spec setup_confirmed_session(binary()) -> ok.
setup_confirmed_session(ST) ->
    ok = setup_scanned_session(ST),
    {ok, _} = qr_login_session_repo:transition_confirm(ST, ?UID, <<"tok_race">>),
    ok.

%% 两函数并发执行（同一时刻发起，PG 行锁仲裁），返回两个结果。
-spec race_two(fun(() -> term()), fun(() -> term())) -> {[term()], reference()}.
race_two(F1, F2) ->
    Parent = self(),
    Ref = erlang:make_ref(),
    P1 = spawn_link(fun() -> Parent ! {Ref, 1, F1()} end),
    P2 = spawn_link(fun() -> Parent ! {Ref, 2, F2()} end),
    R1 =
        receive
            {Ref, 1, RA} -> RA
        end,
    R2 =
        receive
            {Ref, 2, RB} -> RB
        end,
    erlang:unlink(P1),
    erlang:unlink(P2),
    {[R1, R2], Ref}.

-spec cleanup_session(binary()) -> ok.
cleanup_session(ST) ->
    {ok, _} = elib_pg:query(
        <<"DELETE FROM public.qr_login_session WHERE session_token = $1">>, [ST]
    ),
    ok.
