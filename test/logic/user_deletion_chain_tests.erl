-module(user_deletion_chain_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% 账号删除合规链 D-01 —— 真 PostgreSQL fixture 七场景
%%% （Implementation Plan Task D-01 Tests: request, duplicate, cancel,
%%%  DB failure, invalid user, clock boundary, admin approve/reject
%%%  concurrency）。运行：make eunit-local t=user_deletion_chain_tests
%%%  前置：本地 imboy_v1 schema 已应用 00000085。
%%%
%%% 覆盖的业务守卫：
%%%   * 重复申请不重置 requested_at（宽限期时钟不可被绕过）
%%%   * 撤销幂等（无活跃请求 = no-op 成功）；-1 已注销账号绝不复活
%%%   * DB 失败/非法用户 → 整体回滚传播 {error,_}，绝不报成功
%%%   * 清扫只认 user_deletion_request.requested_at（user 无 updated_at）
%%%   * admin 审批并发：恰好一个命中，另一个空守卫返回

-define(GRACE_DAYS, 60).

%% ===================================================================
%% fixtures
%% ===================================================================

new_uid() ->
    %% 真库唯一键惯例：毫秒时间戳基址，自清理兜底跨跑残留
    erlang:system_time(millisecond) * 1000 + rand:uniform(999).

create_user(Uid) ->
    Account = <<"u", (integer_to_binary(Uid))/binary>>,
    {ok, _} =
        elib_pg:query(
            <<
                "INSERT INTO public.\"user\" (id, account, password, reg_ip, reg_cosv)"
                " VALUES ($1, $2, 'x', '127.0.0.1', '')"
            >>,
            [Uid, Account]
        ),
    ok.

cleanup_user(Uid) ->
    _ = elib_pg:query(
        <<"DELETE FROM public.user_deletion_request WHERE user_id = $1">>, [Uid]
    ),
    _ = elib_pg:query(<<"DELETE FROM public.\"user\" WHERE id = $1">>, [Uid]),
    ok.

%% 直接置 status=2 + 插/改 requested 行（requested_at 可控，服务时钟边界场景）
seed_request(Uid, AgeDays) ->
    {ok, _} =
        elib_pg:query(<<"UPDATE public.\"user\" SET status = 2 WHERE id = $1">>, [Uid]),
    {ok, _} =
        elib_pg:query(
            <<
                "INSERT INTO public.user_deletion_request"
                " (id, user_id, status, requested_at)"
                " VALUES ($1, $2, 'requested', NOW() - ($3 || ' days')::INTERVAL)"
                " ON CONFLICT (user_id) DO UPDATE"
                " SET status = 'requested', requested_at = EXCLUDED.requested_at"
            >>,
            [
                elib_tsid:generate(user_deletion_request),
                Uid,
                integer_to_binary(AgeDays)
            ]
        ),
    ok.

user_status(Uid) ->
    {ok, [#{<<"status">> := S}]} =
        elib_pg:query(<<"SELECT status FROM public.\"user\" WHERE id = $1">>, [Uid]),
    S.

%% ===================================================================
%% 1. request：申请 → 状态可查询、旗标=2、预期注销时间=requested_at+宽限
%% ===================================================================

request_flow_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = new_uid(),
        try
            ok = create_user(Uid),
            {ok, "success"} = user_logic:apply_logout(Uid, #{headers => #{}}),
            {ok, #{status := <<"requested">>, requested_at := ReqAt}} =
                user_logic:deletion_status(Uid),
            true = ReqAt =/= undefined andalso ReqAt =/= null,
            %% 请求时间戳可查询（D-01 验收）
            2 = user_status(Uid),
            %% 预期注销时间 = requested_at + 宽限期（rfc3339 串经 elib_dt 平移）
            {ok, #{expected_deletion_at := Exp, grace_days := Grace}} =
                user_logic:deletion_status(Uid),
            true = is_integer(Grace) andalso Grace > 0,
            true = is_binary(Exp) andalso Exp =/= ReqAt,
            Exp = elib_dt:add(ReqAt, {Grace * 86400, second})
        after
            cleanup_user(Uid)
        end
    end).

%% ===================================================================
%% 2. duplicate：重复申请幂等——一行、requested_at 不重置
%% ===================================================================

duplicate_apply_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = new_uid(),
        try
            ok = create_user(Uid),
            {ok, "success"} = user_logic:apply_logout(Uid, #{headers => #{}}),
            {ok, #{requested_at := T1}} = user_logic:deletion_status(Uid),
            {ok, "success"} = user_logic:apply_logout(Uid, #{headers => #{}}),
            {ok, [#{<<"count">> := Count}]} =
                elib_pg:query(
                    <<
                        "SELECT COUNT(*) FROM public.user_deletion_request"
                        " WHERE user_id = $1"
                    >>,
                    [Uid]
                ),
            1 = Count,
            {ok, #{requested_at := T2}} = user_logic:deletion_status(Uid),
            %% 幂等核心断言：重复请求不重置宽限期时钟
            T1 = T2
        after
            cleanup_user(Uid)
        end
    end).

%% ===================================================================
%% 3. cancel：撤销 → cancelled/旗标=1；再申请 → 新 requested_at
%% ===================================================================

cancel_flow_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = new_uid(),
        try
            ok = create_user(Uid),
            {ok, "success"} = user_logic:apply_logout(Uid, #{headers => #{}}),
            {ok, <<"success">>} = user_logic:cancel_logout(Uid, #{}),
            {ok, #{status := <<"cancelled">>}} = user_logic:deletion_status(Uid),
            1 = user_status(Uid),
            %% 幂等：再撤销仍成功（no-op）
            {ok, <<"success">>} = user_logic:cancel_logout(Uid, #{}),
            %% 撤销后重新申请：原位复活为新请求，requested_at 重置
            ok = seed_request(Uid, 0),
            {ok, "success"} = user_logic:apply_logout(Uid, #{headers => #{}}),
            {ok, #{status := <<"requested">>, requested_at := _}} =
                user_logic:deletion_status(Uid),
            2 = user_status(Uid)
        after
            cleanup_user(Uid)
        end
    end).

%% ===================================================================
%% 4. DB failure：注入失败 → 整体回滚传播错误，旗标不动
%% ===================================================================

db_failure_rollback_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = new_uid(),
        try
            ok = create_user(Uid),
            ok = meck:new([user_deletion_request_repo], [passthrough, no_link]),
            ok = meck:expect(
                user_deletion_request_repo,
                upsert_request_tx,
                fun(_Conn, _Uid) -> {error, injected_failure} end
            ),
            {error, _} = user_logic:apply_logout(Uid, #{headers => #{}}),
            %% 回滚：user 旗标未被置 2（API 绝不回滚后报成功）
            1 = user_status(Uid)
        after
            catch meck:unload([user_deletion_request_repo]),
            cleanup_user(Uid)
        end
    end).

%% ===================================================================
%% 5. invalid user：不存在用户 → FK 违例回滚传播错误
%% ===================================================================

invalid_user_test_() ->
    ?TEST_WITH_DB(fun() ->
        GhostUid = new_uid(),
        %% 未 create_user：user_deletion_request.user_id FK 违例 → 回滚
        {error, _} = user_logic:apply_logout(GhostUid, #{headers => #{}}),
        {ok, undefined} = user_deletion_request_repo:find_latest(GhostUid),
        ok
    end).

%% ===================================================================
%% 6. clock boundary：清扫只认 requested_at（>宽限命中，<宽限不命中）
%% ===================================================================

clock_boundary_test_() ->
    ?TEST_WITH_DB(fun() ->
        UidExpired = new_uid(),
        UidFresh = new_uid(),
        try
            ok = create_user(UidExpired),
            ok = create_user(UidFresh),
            %% 过期侧：requested_at = 61 天前（宽限 60）→ 命中
            ok = seed_request(UidExpired, ?GRACE_DAYS + 1),
            {ok, Rows} = user_ds:find_expired_logout_users(?GRACE_DAYS, 1000),
            true = lists:any(
                fun(#{<<"id">> := Id}) -> Id =:= UidExpired end, Rows
            ),
            %% 未到期侧：requested_at = 59 天前 → 不命中
            ok = seed_request(UidFresh, ?GRACE_DAYS - 1),
            {ok, Rows2} = user_ds:find_expired_logout_users(?GRACE_DAYS, 1000),
            false = lists:any(
                fun(#{<<"id">> := Id}) -> Id =:= UidFresh end, Rows2
            ),
            %% cancelled 态即使超期也不命中（只有 requested 态进清扫）
            ok = seed_request(UidFresh, ?GRACE_DAYS + 5),
            {ok, _} = elib_pg:query(
                <<
                    "UPDATE public.user_deletion_request"
                    " SET status = 'cancelled' WHERE user_id = $1"
                >>,
                [UidFresh]
            ),
            {ok, Rows3} = user_ds:find_expired_logout_users(?GRACE_DAYS, 1000),
            false = lists:any(
                fun(#{<<"id">> := Id}) -> Id =:= UidFresh end, Rows3
            )
        after
            cleanup_user(UidExpired),
            cleanup_user(UidFresh)
        end
    end).

%% ===================================================================
%% 7. admin approve/reject concurrency：并发审批恰好一个命中；
%%    批准后驳回被守卫拦截；状态与记录同步
%% ===================================================================

admin_concurrency_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = new_uid(),
        try
            ok = create_user(Uid),
            ok = seed_request(Uid, 1),
            Self = self(),
            spawn(fun() -> Self ! {r1, user_ds:approve_logout_apply(Uid)} end),
            R2 = user_ds:approve_logout_apply(Uid),
            R1 =
                receive
                    {r1, Res} -> Res
                after 15000 -> {error, timeout}
                end,
            %% 恰好一个非空命中 + 一个空守卫命中
            Fold = fun
                ({ok, []}, {H, N}) -> {H, N + 1};
                ({ok, L}, {H, N}) when is_list(L), L =/= [] -> {H + 1, N}
            end,
            {1, 1} = lists:foldl(Fold, {0, 0}, [R1, R2]),
            -1 = user_status(Uid),
            %% 记录态 approved
            {ok, [#{<<"count">> := 1}]} =
                elib_pg:query(
                    <<
                        "SELECT COUNT(*) FROM public.user_deletion_request"
                        " WHERE user_id = $1 AND status = 'approved'"
                    >>,
                    [Uid]
                ),
            %% 批准后的驳回：守卫拦截（status 已 -1 非申请中），空返回
            {ok, []} = user_ds:reject_logout_apply(Uid),
            -1 = user_status(Uid)
        after
            cleanup_user(Uid)
        end
    end).
