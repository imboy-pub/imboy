-module(login_security_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("log.hrl").
-include("common.hrl").

%%%===================================================================
%%% @doc
%%% login_security_logic 模块的 EUnit 测试（Limiter V1 L3 接线版）
%%%
%%% 目标：验证 S 层作为 login_attempt_ds 权威状态的无状态薄转发
%%% 覆盖：新 Identity 导出（check/failure/reset/get_lock_info）、
%%%       deprecated 兼容转发、unavailable 不得默认放行（F2）、
%%%       reset 确认语义（AC-17）、admin_unlock、快照一致性（AC-05）
%%%
%%% 纪律：D/S 本体不 mock（真实 DS owner + ETS）；仅故障注入场景
%%% meck login_attempt_ds 单函数（AC-12 许可的模拟方式）。
%%% ETS 为 owner 持有的 protected 表，测试进程不可写：清表 = 重启
%%% owner（新 owner 新空表，A1 同款装配）。
%%%===================================================================

-define(UID, 915001).
-define(ID_USER, {user, ?UID}).
-define(ID_UNK, {unknown, email, <<"limiter-s@e2e.test">>}).
-define(L_UNK_BIN, <<"limiter-s-compat@e2e.test">>).

ensure_ds() ->
    ok = limiter_test_support:ensure_owner(),
    ok.

cleanup_ds() ->
    try
        limiter_test_support:ensure_owner()
    catch
        _:_ ->
            ok
    end,
    ok.

%% 带清理的测试包装：断言失败也保证 owner 重建（清空共享 ETS）
with_ds(Fun) ->
    fun() ->
        ok = ensure_ds(),
        try
            Fun()
        after
            cleanup_ds()
        end
    end.

%% ===================================================================
%% check_login_allowed/1（新导出）
%% ===================================================================

check_login_allowed_allows_unknown_identity_test_() ->
    ?TEST_WITH_APP(
        with_ds(fun() ->
            ?assertEqual({ok, true}, login_security_logic:check_login_allowed(?ID_UNK))
        end)
    ).

check_login_allowed_rejects_when_locked_test_() ->
    ?TEST_WITH_APP(
        with_ds(fun() ->
            %% 真实累计到锁定阈值（F=5）
            lists:foreach(
                fun(_) ->
                    {ok, _} = login_security_logic:record_login_failure(?ID_USER)
                end,
                lists:seq(1, 5)
            ),
            Result = login_security_logic:check_login_allowed(?ID_USER),
            %% F2 修复 oracle：锁定必须以显式三元组返回，不得放行
            ?assertMatch({error, locked, #{<<"is_locked">> := true}}, Result),
            {error, locked, LockInfo} = Result,
            ?assertEqual(5, maps:get(<<"attempts">>, LockInfo)),
            ?assertEqual(0, maps:get(<<"remaining_attempts">>, LockInfo)),
            ?assertEqual(5, maps:get(<<"max_attempts">>, LockInfo))
        end)
    ).

check_login_allowed_unavailable_is_not_pass_through_test_() ->
    %% 故障注入（AC-12 许可方式）：meck DS 单函数模拟 owner 不可用。
    %% F2 反例：若实现回退为默认放行，此断言首先变红。
    ?WITH_MECK(
        login_attempt_ds,
        [
            {'check', 1, fun(_Identity) -> {error, unavailable} end}
        ],
        fun() ->
            ?assertEqual(
                {error, unavailable},
                login_security_logic:check_login_allowed(?ID_USER)
            )
        end
    ).

%% ===================================================================
%% record_login_failure/1（新导出：单次快照含 locked 判断）
%% ===================================================================

record_login_failure_increments_and_reports_locked_test_() ->
    ?TEST_WITH_APP(
        with_ds(fun() ->
            {ok, Snap1} = login_security_logic:record_login_failure(?ID_USER),
            ?assertEqual(1, maps:get(count, Snap1)),
            ?assertEqual(false, maps:get(locked, Snap1)),
            {ok, Snap2} = login_security_logic:record_login_failure(?ID_USER),
            ?assertEqual(2, maps:get(count, Snap2)),
            %% 累计 2 次后到第 F 次：locked 在同一快照内为 true（不二次读状态）
            lists:foreach(
                fun(_) ->
                    {ok, _} = login_security_logic:record_login_failure(?ID_USER)
                end,
                lists:seq(1, 2)
            ),
            {ok, SnapF} = login_security_logic:record_login_failure(?ID_USER),
            ?assertEqual(5, maps:get(count, SnapF)),
            ?assertEqual(true, maps:get(locked, SnapF)),
            ?assertEqual(0, maps:get(remaining, SnapF)),
            %% 达阈值后在途失败仍记账（不以 locked 拒收，C.7）
            {ok, Snap6} = login_security_logic:record_login_failure(?ID_USER),
            ?assertEqual(6, maps:get(count, Snap6)),
            ?assertEqual(true, maps:get(locked, Snap6))
        end)
    ).

record_login_failure_capacity_lost_propagates_test_() ->
    ?WITH_MECK(
        login_attempt_ds,
        [
            {'failure', 1, fun(_Identity) -> {error, capacity_lost} end}
        ],
        fun() ->
            ?assertEqual(
                {error, capacity_lost},
                login_security_logic:record_login_failure(?ID_USER)
            )
        end
    ).

%% ===================================================================
%% record_login_success/1（AC-17：不再无条件 ok）
%% ===================================================================

record_login_success_resets_count_test_() ->
    ?TEST_WITH_APP(
        with_ds(fun() ->
            {ok, _} = login_security_logic:record_login_failure(?ID_USER),
            ok = login_security_logic:record_login_success(?ID_USER),
            {ok, Snap} = login_attempt_ds:snapshot(?ID_USER),
            ?assertEqual(0, maps:get(count, Snap))
        end)
    ).

record_login_success_may_fail_unavailable_test_() ->
    %% AC-17 反例：若实现回退为无条件 ok（忽略 reset 结果），
    %% 下层 reset 返回 unavailable 时此断言首先变红。
    ?WITH_MECK(
        login_attempt_ds,
        [
            {'reset', 1, fun(_Identity) -> {error, unavailable} end}
        ],
        fun() ->
            ?assertEqual(
                {error, unavailable},
                login_security_logic:record_login_success(?ID_USER)
            )
        end
    ).

%% ===================================================================
%% get_lock_info/1（单一快照构造，字段形状兼容）
%% ===================================================================

get_lock_info_shape_from_real_snapshot_test_() ->
    ?TEST_WITH_APP(
        with_ds(fun() ->
            lists:foreach(
                fun(_) ->
                    {ok, _} = login_security_logic:record_login_failure(?ID_USER)
                end,
                lists:seq(1, 2)
            ),
            {ok, Info} = login_security_logic:get_lock_info(?ID_USER),
            ?assertEqual(false, maps:get(<<"is_locked">>, Info)),
            ?assertEqual(2, maps:get(<<"attempts">>, Info)),
            ?assertEqual(3, maps:get(<<"remaining_attempts">>, Info)),
            ?assertEqual(5, maps:get(<<"max_attempts">>, Info)),
            ?assertEqual(undefined, maps:get(<<"locked_until">>, Info))
        end)
    ).

get_lock_info_locked_carries_expiry_test_() ->
    ?TEST_WITH_APP(
        with_ds(fun() ->
            lists:foreach(
                fun(_) ->
                    {ok, _} = login_security_logic:record_login_failure(?ID_USER)
                end,
                lists:seq(1, 5)
            ),
            {ok, Info} = login_security_logic:get_lock_info(?ID_USER),
            ?assertEqual(true, maps:get(<<"is_locked">>, Info)),
            %% locked_until 来自同一快照的 expires_at_ms（单调毫秒或
            %% undefined），不再伪造 wall 时间（F4：跨 Cache 伪 expiry 修复）
            ?assert(is_integer(maps:get(<<"locked_until">>, Info))),
            {ok, Snap} = login_attempt_ds:snapshot(?ID_USER),
            ?assertEqual(maps:get(expires_at_ms, Snap), maps:get(<<"locked_until">>, Info))
        end)
    ).

get_lock_info_unavailable_test_() ->
    ?WITH_MECK(
        login_attempt_ds,
        [
            {'snapshot', 1, fun(_Identity) -> {error, unavailable} end}
        ],
        fun() ->
            ?assertEqual(
                {error, unavailable},
                login_security_logic:get_lock_info(?ID_USER)
            )
        end
    ).

%% ===================================================================
%% admin_unlock（AC-05：未授权不改状态，授权清规范身份）
%% ===================================================================

admin_unlock_unauthorized_keeps_state_test_() ->
    ?TEST_WITH_APP(
        with_ds(fun() ->
            lists:foreach(
                fun(_) ->
                    {ok, _} = login_security_logic:record_login_failure(?ID_USER)
                end,
                lists:seq(1, 5)
            ),
            OldAdminUids = application:get_env(imboy, admin_uids),
            application:unset_env(imboy, admin_uids),
            try
                %% 普通 Uid（>= 10000）不授权
                ?assertEqual(
                    {error, unauthorized},
                    login_security_logic:admin_unlock(999999, ?ID_USER)
                ),
                {ok, Snap} = login_attempt_ds:snapshot(?ID_USER),
                ?assertEqual(true, maps:get(locked, Snap))
            after
                restore_env(admin_uids, OldAdminUids)
            end
        end)
    ).

admin_unlock_authorized_clears_identity_test_() ->
    ?TEST_WITH_APP(
        with_ds(fun() ->
            lists:foreach(
                fun(_) ->
                    {ok, _} = login_security_logic:record_login_failure(?ID_USER)
                end,
                lists:seq(1, 5)
            ),
            OldAdminUids = application:get_env(imboy, admin_uids),
            application:set_env(imboy, admin_uids, [4242]),
            try
                ?assertEqual(
                    {ok, unlocked},
                    login_security_logic:admin_unlock(4242, ?ID_USER)
                ),
                {ok, Snap} = login_attempt_ds:snapshot(?ID_USER),
                ?assertEqual(0, maps:get(count, Snap)),
                ?assertEqual(false, maps:get(locked, Snap))
            after
                restore_env(admin_uids, OldAdminUids)
            end
        end)
    ).

restore_env(_Key, undefined) ->
    application:unset_env(imboy, admin_uids);
restore_env(_Key, {ok, V}) ->
    application:set_env(imboy, admin_uids, V).

%% ===================================================================
%% deprecated 兼容导出（旧 (Identifier, Ip) 键；Ip 忽略——账户域跨 IP）
%% ===================================================================

compat_check_login_allowed_empty_identifier_test_() ->
    ?_assertEqual({ok, true}, login_security_logic:check_login_allowed(<<>>, <<>>)).

compat_record_login_failure_shapes_test_() ->
    ?TEST_WITH_APP(
        with_ds(fun() ->
            {ok, 1} = login_security_logic:record_login_failure(?L_UNK_BIN, <<>>),
            lists:foreach(
                fun(_) ->
                    _ = login_security_logic:record_login_failure(?L_UNK_BIN, <<"9.9.9.9">>)
                end,
                lists:seq(1, 3)
            ),
            %% 第 5 次（阈值）→ {ok, Count, locked}
            ?assertMatch(
                {ok, 5, locked}, login_security_logic:record_login_failure(?L_UNK_BIN, <<>>)
            ),
            %% 非法输入
            ?assertEqual(
                {error, invalid_input}, login_security_logic:record_login_failure(<<>>, <<>>)
            )
        end)
    ).

compat_record_login_success_passthrough_test_() ->
    ?TEST_WITH_APP(
        with_ds(fun() ->
            {ok, _} = login_security_logic:record_login_failure(?L_UNK_BIN, <<>>),
            ok = login_security_logic:record_login_success(?L_UNK_BIN, <<>>),
            {ok, Snap} = login_attempt_ds:snapshot({unknown, account, ?L_UNK_BIN}),
            ?assertEqual(0, maps:get(count, Snap))
        end)
    ).

compat_check_ip_rate_limit_invalid_ip_test_() ->
    ?_assertEqual({error, invalid_ip}, login_security_logic:check_ip_rate_limit(<<>>)).

%% ===================================================================
%% 集成：失败 → 锁定 → reset → 放行（真实 DS，账户域跨 IP 合并）
%% ===================================================================

login_failure_flow_locks_then_resets_test_() ->
    ?TEST_WITH_APP(
        with_ds(fun() ->
            %% 5 次失败（键同、IP 不同也合并——账户域跨 IP，AC-05/09 语义）
            lists:foreach(
                fun(N) ->
                    {ok, SnapN} = login_security_logic:record_login_failure(?ID_USER),
                    ?assertEqual(N, maps:get(count, SnapN))
                end,
                lists:seq(1, 5)
            ),
            {error, locked, _} = login_security_logic:check_login_allowed(?ID_USER),
            %% 成功 reset 后放行
            ok = login_security_logic:record_login_success(?ID_USER),
            ?assertEqual({ok, true}, login_security_logic:check_login_allowed(?ID_USER))
        end)
    ).
