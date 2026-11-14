%%%-------------------------------------------------------------------
%%% @doc G-QR / LEE-5 原子消费测试（收口 C08）
%%%
%%% 【测试分类】除显式标注 MOCK 的用例外，本文件全部为**真实 PostgreSQL
%%% 集成测试**（专属临时库 imboy_cache_closeout_v1，迁移 00000170 已 apply，
%%% 多进程经 pooler 多连接并发——跨连接行锁仲裁即跨节点语义）。
%%%
%%% 并发 claim 竞争 oracle（g-qr-atomic-consumption-proposal.md 第 6 节 +
%%% LEE-5 最终实施补充要求第 1/3 项）：
%%% - 并发 2×N status 领取：恰好 1 个拿到 token；单轮 100 路并发同口径；
%%% - SSE（服务端代领）与 status 轮询竞争：同一 token 恰好领取一次；
%%% - 状态竞争六场景（confirm/cancel、confirm/expire、claim/cancel、
%%%   scan/cancel、重复 confirm、claim/expire）：断言**最终数据库状态与
%%%   唯一裁决规则**，非仅"不崩溃"；已取消/过期会话绝不允许成功领取。
%%% - cleanup vs claim 并发：过期安全由 claim 内联谓词保证，绝不交付过期
%%%   凭据；清扫只删 expires_at <= now()，批量限量。
%%% - PG18 门禁语义（显式标注 MOCK：meck elib_pg）：PG18 通过 / PG17 拒绝 /
%%%   PG 故障明确错误且不缓存 / 已缓存零重复查询。
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
    [{ok, #{login_token := Token, scanned_by := ?UID}}] =
        [Result || {ok, _} = Result <- Results],
    ?assertEqual(<<"tok_race">>, Token),
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
        ?assertMatch(
            [{ok, #{login_token := <<"tok_race">>, scanned_by := ?UID}}],
            [Result || {ok, _} = Result <- Results]
        ),
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
%% LEE-5 补充 1：单轮 100 路并发领取恰好一个赢家（真实 PG）
%% ===================================================================

single_round_100_way_claim_exactly_one_test_() ->
    ?TEST_WITH_DB_TIMEOUT(60, fun() ->
        ST = unique_token(<<"st_c100">>),
        setup_confirmed_session(ST),
        Results = concurrent_claim(ST, 100),
        Winners = length([ok || {ok, _} <- Results]),
        Losers = length([error || {error, _} <- Results]),
        %% 恰好 1 个赢家、99 个明确失败；无 token 残留；二次领取拒绝
        ?assertEqual(1, Winners),
        ?assertEqual(99, Losers),
        [{ok, #{login_token := <<"tok_race">>, scanned_by := ?UID}}] =
            [Result || {ok, _} = Result <- Results],
        #{<<"token_claimable">> := false} =
            qr_login_session_repo:find_by_session_token(ST),
        {error, not_claimable} = qr_login_session_repo:claim_login_token(ST),
        cleanup_session(ST)
    end).

%% ===================================================================
%% LEE-5 补充 1：状态竞争六场景——断言最终数据库状态与唯一裁决规则
%% ===================================================================

%% 竞争 1：confirm vs cancel（已有用例断言恰一成功；此处补最终状态裁决）
confirm_vs_cancel_final_state_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        ST = unique_token(<<"st_fc">>),
        setup_scanned_session(ST),
        {[ConfirmR, _CancelR], _} = race_two(
            fun() -> qr_login_session_repo:transition_confirm(ST, ?UID, <<"tok_fc">>) end,
            fun() -> qr_login_session_repo:transition_cancel(ST) end
        ),
        case ConfirmR of
            {ok, _} ->
                %% 裁决规则 A：confirm 赢——status=confirmed，token 恰好可领取一次，
                %% cancel 不可逆（已确认即生效）
                #{<<"status">> := <<"confirmed">>, <<"token_claimable">> := true} =
                    qr_login_session_repo:find_by_session_token(ST),
                {error, not_transitioned} =
                    qr_login_session_repo:transition_cancel(ST),
                {ok, #{login_token := <<"tok_fc">>}} =
                    qr_login_session_repo:claim_login_token(ST),
                {error, not_claimable} =
                    qr_login_session_repo:claim_login_token(ST);
            _ ->
                %% 裁决规则 B：cancel 赢——status=cancelled，token 永不可领取
                #{<<"status">> := <<"cancelled">>, <<"token_claimable">> := false} =
                    qr_login_session_repo:find_by_session_token(ST),
                {error, not_claimable} =
                    qr_login_session_repo:claim_login_token(ST)
        end,
        cleanup_session(ST)
    end).

%% 竞争 2：confirm vs expire（expire = 仅 status='scanned' 时强制过期的
%% 竞争 UPDATE——过期只能阻止 confirm，不能撤销已完成的 confirm）
confirm_vs_expire_unique_outcome_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        ST = unique_token(<<"st_ce">>),
        setup_scanned_session(ST),
        {[ConfirmR, ExpireR], _} = race_two(
            fun() -> qr_login_session_repo:transition_confirm(ST, ?UID, <<"tok_ce">>) end,
            fun() -> force_expire_scanned(ST) end
        ),
        {ok, ExpireCount} = ExpireR,
        ConfirmWon = is_tuple(ConfirmR) andalso element(1, ConfirmR) =:= ok,
        %% 唯一裁决：双方对 status='scanned' CAS，恰好一个赢家
        ?assert(ConfirmWon =/= (ExpireCount =:= 1)),
        case ConfirmWon of
            true ->
                %% confirm 赢：confirmed + 未过期 + token 恰好领取一次
                0 = ExpireCount,
                #{<<"status">> := <<"confirmed">>, <<"token_claimable">> := true} =
                    qr_login_session_repo:find_by_session_token(ST),
                {ok, #{login_token := <<"tok_ce">>}} =
                    qr_login_session_repo:claim_login_token(ST),
                {error, not_claimable} =
                    qr_login_session_repo:claim_login_token(ST);
            false ->
                %% expire 赢：confirm 被内联 expires_at 谓词拒绝；行已过期，
                %% 任何后续操作（含 claim）一律拒绝——绝不交付过期凭据
                {error, not_transitioned} = ConfirmR,
                undefined = qr_login_session_repo:find_by_session_token(ST),
                {error, not_claimable} =
                    qr_login_session_repo:claim_login_token(ST)
        end,
        cleanup_session(ST)
    end).

%% 竞争 3：claim vs cancel——confirmed 会话上 cancel 必败（CAS 仅
%% waiting|scanned），claim 恒可领取；已取消会话绝不允许领取（负向断言）
claim_vs_cancel_and_negative_states_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        ST = unique_token(<<"st_clc">>),
        setup_confirmed_session(ST),
        {[ClaimR, CancelR], _} = race_two(
            fun() -> qr_login_session_repo:claim_login_token(ST) end,
            fun() -> qr_login_session_repo:transition_cancel(ST) end
        ),
        %% 裁决规则：confirmed 即生效——cancel 必败，claim 恰好成功一次
        {error, not_transitioned} = CancelR,
        {ok, #{login_token := <<"tok_race">>}} = ClaimR,
        {error, not_claimable} = qr_login_session_repo:claim_login_token(ST),
        cleanup_session(ST),

        %% 负向状态 1：已取消会话绝不发放凭据
        ST2 = unique_token(<<"st_clc2">>),
        setup_scanned_session(ST2),
        ok = qr_login_session_repo:transition_cancel(ST2),
        {error, not_claimable} = qr_login_session_repo:claim_login_token(ST2),
        cleanup_session(ST2),

        %% 负向状态 2：已过期会话（含过期前已 confirm）绝不发放凭据
        ST3 = unique_token(<<"st_clc3">>),
        setup_confirmed_session(ST3),
        {ok, 1} = force_expire_confirmed(ST3),
        {error, not_claimable} = qr_login_session_repo:claim_login_token(ST3),
        cleanup_session(ST3)
    end).

%% 竞争 4：scan vs cancel。注意两者**不是互斥 CAS**：cancel 的前置集
%% （waiting|scanned）是 scan 后状态的超集——"扫码后再取消"是产品合法
%% 转移。合法结局空间（裁决规则）：
%%   A) cancel 先锁行 → scan 被拒（waiting 已变 cancelled）；
%%   B) scan 先锁行 → cancel 仍可从 scanned 取消，两者都成功。
%% 任一交错：cancel 恒成功、终态恒 cancelled、login_token 永不可领取
%% ——这才是安全不变式（间歇暴露过我此前的假 XOR 断言）。
scan_vs_cancel_unique_outcome_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        ST = unique_token(<<"st_sxc">>),
        QT = unique_token(<<"qt_sxc">>),
        ok = qr_login_session_repo:create(#{
            session_token => ST,
            qr_token => QT,
            device_id => <<"dev">>,
            device_name => <<"d">>,
            platform => <<"web">>,
            purpose => <<"human">>,
            expires_at_ms => future_ms(60000)
        }),
        {[ScanR, CancelR], _} = race_two(
            fun() -> qr_login_session_repo:transition_scan(ST, ?UID) end,
            fun() -> qr_login_session_repo:transition_cancel(ST) end
        ),
        %% 裁决规则 1：cancel 恒成功（前置集覆盖 scan 后状态；幂等回查兜底）
        ?assertEqual(ok, CancelR),
        %% 裁决规则 2：终态恒 cancelled
        Row = qr_login_session_repo:find_by_session_token(ST),
        ?assertEqual(<<"cancelled">>, maps:get(<<"status">>, Row)),
        %% 裁决规则 3：scan 至多成功一次；成功时 scanned_by=赢家（cancelled
        %% 只改 status，scanned_by 保留扫码痕迹），失败时 scanned_by 非本用户
        ScanWon = is_tuple(ScanR) andalso element(1, ScanR) =:= ok,
        case ScanWon of
            true ->
                ?assertEqual(?UID, maps:get(<<"scanned_by">>, Row));
            false ->
                ?assertEqual({error, not_transitioned}, ScanR),
                ?assert(maps:get(<<"scanned_by">>, Row) =/= ?UID)
        end,
        %% 裁决规则 4（安全不变式）：已取消会话绝不发放凭据
        {error, not_claimable} = qr_login_session_repo:claim_login_token(ST),
        cleanup_session(ST)
    end).

%% 竞争 6：claim vs expire——双方对 (confirmed, token 未消费) CAS，恰一赢家；
%% 任何交错都绝不交付过期凭据
claim_vs_expire_never_expired_credential_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        ST = unique_token(<<"st_cle">>),
        setup_confirmed_session(ST),
        {[ClaimR, ExpireR], _} = race_two(
            fun() -> qr_login_session_repo:claim_login_token(ST) end,
            fun() -> force_expire_confirmed(ST) end
        ),
        {ok, ExpireCount} = ExpireR,
        ClaimWon = is_tuple(ClaimR) andalso element(1, ClaimR) =:= ok,
        %% 唯一裁决：claim 赢则行已消费（expire 0 行）；expire 赢则行过期
        %% （claim 0 行 not_claimable）——恰一赢家
        ?assert(ClaimWon =/= (ExpireCount =:= 1)),
        case ClaimWon of
            true ->
                {ok, #{login_token := <<"tok_race">>}} = ClaimR,
                0 = ExpireCount;
            false ->
                {error, not_claimable} = ClaimR,
                1 = ExpireCount
        end,
        %% 无论谁赢：领取窗口已关闭（已消费或已过期），绝不二次交付
        {error, not_claimable} = qr_login_session_repo:claim_login_token(ST),
        cleanup_session(ST)
    end).

%% ===================================================================
%% LEE-5 补充 3：cleanup vs claim 并发 + 只删过期 + 批量上限（真实 PG）
%% ===================================================================

%% 活跃（未过期）会话：清扫与 claim 并发——清扫 0 行（谓词严格分区），
%% claim 恰好成功一次；过期安全从不依赖清扫
cleanup_vs_claim_live_session_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        ST = unique_token(<<"st_cul">>),
        setup_confirmed_session(ST),
        {[ClaimR, CleanupR], _} = race_two(
            fun() -> qr_login_session_repo:claim_login_token(ST) end,
            fun() -> qr_login_session_repo:delete_expired(1) end
        ),
        {ok, #{login_token := <<"tok_race">>}} = ClaimR,
        {ok, 0} = CleanupR,
        {error, not_claimable} = qr_login_session_repo:claim_login_token(ST),
        cleanup_session(ST)
    end).

%% 已过期会话：清扫与 claim 并发——claim 不论交错一律拒绝，绝不交付过期凭据
cleanup_vs_claim_expired_session_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        ST = unique_token(<<"st_cue">>),
        setup_confirmed_session(ST),
        {ok, 1} = force_expire_confirmed(ST),
        {[ClaimR, _CleanupR], _} = race_two(
            fun() -> qr_login_session_repo:claim_login_token(ST) end,
            fun() -> qr_login_session_repo:delete_expired(1) end
        ),
        {error, not_claimable} = ClaimR,
        %% 行可能已被清扫删除或仍在（过期态）——两者对领取语义等价
        {error, not_claimable} = qr_login_session_repo:claim_login_token(ST),
        cleanup_session(ST)
    end).

%% 只删 expires_at <= now() 的行；单批 LIMIT 生效；活跃行保留。
%% 前置基线排空：共享库可能有前轮失败用例泄漏的过期行（60s 后过期），
%% 先清到 0 再插桩，计数断言才不受残留影响。
delete_expired_only_expired_and_batch_limit_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        drain_expired(),
        Live = [unique_token(<<"st_dxl">>) || _ <- lists:seq(1, 2)],
        Expired = [unique_token(<<"st_dxe">>) || _ <- lists:seq(1, 3)],
        [
            begin
                ok = qr_login_session_repo:create(#{
                    session_token => ST,
                    qr_token => unique_token(<<"qt_dx">>),
                    device_id => <<"dev">>,
                    device_name => <<"d">>,
                    platform => <<"web">>,
                    purpose => <<"human">>,
                    expires_at_ms => future_ms(60000)
                })
            end
         || ST <- Live
        ],
        [
            begin
                ok = qr_login_session_repo:create(#{
                    session_token => ST,
                    qr_token => unique_token(<<"qt_dx">>),
                    device_id => <<"dev">>,
                    device_name => <<"d">>,
                    platform => <<"web">>,
                    purpose => <<"human">>,
                    expires_at_ms => erlang:system_time(millisecond) - 1000
                })
            end
         || ST <- Expired
        ],
        %% 单批 2：只删 2 行过期行（按 expires_at ASC），活跃行不动
        {ok, 2} = qr_login_session_repo:delete_expired(2),
        [
            begin
                #{<<"status">> := <<"waiting">>} =
                    qr_login_session_repo:find_by_session_token(ST)
            end
         || ST <- Live
        ],
        %% 批量放宽后清完剩余 1 行过期行；活跃行仍在
        {ok, 1} = qr_login_session_repo:delete_expired(500),
        [
            begin
                #{<<"status">> := <<"waiting">>} =
                    qr_login_session_repo:find_by_session_token(ST)
            end
         || ST <- Live
        ],
        {ok, 0} = qr_login_session_repo:delete_expired(500),
        [cleanup_session(ST) || ST <- Live]
    end).

%% ===================================================================
%% LEE-5 补充 2：PG18 门禁语义——【MOCK：meck elib_pg】
%% 仅证门禁逻辑（通过/拒绝/不缓存错误/记忆零重复查询），非跨节点证明。
%% ===================================================================

pg18_gate_semantics_mock_test_() ->
    {setup,
        fun() ->
            persistent_term:erase({imboy, qr_login_pg_version_num}),
            meck:new(elib_pg, [no_link])
        end,
        fun(_) ->
            meck:unload(elib_pg),
            persistent_term:erase({imboy, qr_login_pg_version_num})
        end,
        [
            fun pg18_passes_and_memoizes_once/0,
            fun pg17_rejected_at_gate_claim_and_create/0,
            fun pg_query_failure_returns_error_and_is_not_cached/0
        ]}.

pg18_passes_and_memoizes_once() ->
    meck:expect(elib_pg, query, fun(_, _) -> {ok, [#{<<"version_num">> => 180000}]} end),
    ok = qr_login_session_repo:ensure_claim_compatible(),
    ok = qr_login_session_repo:ensure_claim_compatible(),
    %% 已缓存版本：两次过闸只发生一次真实查询
    1 = meck:num_calls(elib_pg, query, 2),
    ok.

pg17_rejected_at_gate_claim_and_create() ->
    persistent_term:erase({imboy, qr_login_pg_version_num}),
    meck:expect(elib_pg, query, fun(_, _) -> {ok, [#{<<"version_num">> => 170004}]} end),
    {error, {pg_version_unsupported, 170004}} =
        qr_login_session_repo:ensure_claim_compatible(),
    %% claim 入口被门禁拦截（可区分错误），create 入口同样 fail closed
    {error, {claim_gate, {pg_version_unsupported, 170004}}} =
        qr_login_session_repo:claim_login_token(<<"st_pg17">>),
    {error, {pg_version_unsupported, 170004}} =
        qr_login_session_repo:create(#{
            session_token => <<"st_pg17">>,
            qr_token => <<"qt_pg17">>,
            device_id => <<"dev">>,
            device_name => <<"d">>,
            platform => <<"web">>,
            purpose => <<"human">>,
            expires_at_ms => 1
        }),
    ok.

pg_query_failure_returns_error_and_is_not_cached() ->
    persistent_term:erase({imboy, qr_login_pg_version_num}),
    meck:expect(elib_pg, query, fun(_, _) -> {error, closed} end),
    {error, closed} = qr_login_session_repo:ensure_claim_compatible(),
    %% 查询失败不缓存：PG 恢复后下一次过闸重新查询并成功
    meck:expect(elib_pg, query, fun(_, _) -> {ok, [#{<<"version_num">> => 180001}]} end),
    ok = qr_login_session_repo:ensure_claim_compatible(),
    %% 且此后记忆，不再发查询
    ok = qr_login_session_repo:ensure_claim_compatible(),
    ok.

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

%% 共享库基线排空：循环清扫至 0 行（前轮失败用例可能泄漏已过期行）
-spec drain_expired() -> ok.
drain_expired() ->
    case qr_login_session_repo:delete_expired(500) of
        {ok, 0} ->
            ok;
        {ok, _} ->
            drain_expired();
        {error, Reason} ->
            erlang:error({drain_expired_failed, Reason})
    end.

-spec cleanup_session(binary()) -> ok.
cleanup_session(ST) ->
    {ok, _} = elib_pg:query(
        <<"DELETE FROM public.qr_login_session WHERE session_token = $1">>, [ST]
    ),
    ok.

%% 仅当 status='scanned' 时强制过期（竞争注入：模拟"到期事件"与 confirm
%% 对同一行的竞争。过期只能阻止 confirm，不能撤销已完成的 confirm——
%% 与生产语义一致：真实过期由 expires_at 时间戳承载，不存在撤销操作）。
-spec force_expire_scanned(binary()) -> {ok, non_neg_integer()}.
force_expire_scanned(ST) ->
    elib_pg:execute(
        <<
            "UPDATE public.qr_login_session SET expires_at = now() - interval '1 second' "
            "WHERE session_token = $1 AND status = 'scanned'"
        >>,
        [ST]
    ).

%% 仅当 confirmed 且 token 未消费时强制过期（竞争注入：与 claim 对同一
%% (status, login_token, consumed_at) 目标集 CAS 竞争）。
-spec force_expire_confirmed(binary()) -> {ok, non_neg_integer()}.
force_expire_confirmed(ST) ->
    elib_pg:execute(
        <<
            "UPDATE public.qr_login_session SET expires_at = now() - interval '1 second' "
            "WHERE session_token = $1 AND status = 'confirmed' "
            "AND login_token IS NOT NULL AND consumed_at IS NULL"
        >>,
        [ST]
    ).
