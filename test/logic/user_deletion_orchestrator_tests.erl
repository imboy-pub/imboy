-module(user_deletion_orchestrator_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%% L-03 shell 演练钩子（eunit 不自动发现；见文末第 7 节注释）
-export([l03_seed_drill_user/0, l03_delete_drill_user/1, l03_state/1]).

%%% 账号删除合规链 D-03 —— 编排器真 PostgreSQL 测试
%%% （Implementation Plan Task D-03 Tests：多域种子用户、外部失败重试、
%%%  并发 worker 认领、支付余额门、E2EE 密钥、Garage 对象删除 mock）
%%% 运行：make eunit-local t=user_deletion_orchestrator_tests
%%% 前置：schema 已应用 00000085/00000086。

-define(GRACE, 60).

%% ===================================================================
%% fixtures
%% ===================================================================

new_uid() ->
    elib_tsid:generate(user).

create_user(Uid) ->
    {ok, _} = elib_pg:query(
        <<
            "INSERT INTO public.\"user\" (id, account, password, reg_ip, reg_cosv)"
            " VALUES ($1, $2, 'x', '127.0.0.1', '')"
        >>,
        [Uid, <<"u", (integer_to_binary(Uid))/binary>>]
    ),
    ok.

seed_expired_request(Uid) ->
    {ok, _} = elib_pg:query(
        <<"UPDATE public.\"user\" SET status = 2 WHERE id = $1">>, [Uid]
    ),
    {ok, _} = elib_pg:query(
        <<
            "INSERT INTO public.user_deletion_request (id, user_id, status, requested_at)"
            " VALUES ($1, $2, 'requested', NOW() - interval '61 days')"
        >>,
        [new_uid(), Uid]
    ),
    ok.

cleanup_user(Uid) ->
    lists:foreach(fun(Sql) -> _ = elib_pg:query(Sql, [Uid]) end, [
        <<"DELETE FROM public.user_deletion_job WHERE user_id = $1">>,
        <<"DELETE FROM public.user_deletion_request WHERE user_id = $1">>,
        <<"DELETE FROM public.msg_c2c WHERE from_id = $1 OR to_id = $1">>,
        <<"DELETE FROM public.moment_post WHERE author_uid = $1">>,
        <<"DELETE FROM public.attachment WHERE creator_user_id = $1">>,
        <<"DELETE FROM public.push_token WHERE user_id = $1">>,
        <<"DELETE FROM public.user_device WHERE user_id = $1">>,
        <<"DELETE FROM public.wallet WHERE user_id = $1">>,
        <<"DELETE FROM public.group_member WHERE user_id = $1">>,
        <<"DELETE FROM public.\"group\" WHERE id = $1">>,
        <<"DELETE FROM public.olm_identity WHERE user_id = $1">>,
        <<"DELETE FROM public.\"user\" WHERE id = $1">>
    ]).

ensure_sweeper() ->
    ensure_sweeper(unavailable).

ensure_sweeper(Behavior) ->
    application:set_env(imboy, user_deletion_enabled, true),
    application:set_env(imboy, user_deletion_retention_days, ?GRACE),
    application:set_env(imboy, user_deletion_batch_size, 10),
    application:set_env(imboy, user_deletion_max_attempts, 5),
    %% [语境固化] EB/CS/Agent 域 provider 已登记（0c53bc17 ORG-08、
    %% d62faaf2 ORG02-AGENT-DOMAIN），preflight 不再走「缺域=拒」分支，
    %% 原断言依赖的 ORG-02 裁决 A 语境（DEPENDENCY_FACTS_UNAVAILABLE →
    %% 同轮重试至 failed）需显式复现：注入五域 stub 注册表并置
    %% unavailable 行为（同款用法见 organization_preflight_tests）。
    %% 用例 after 中 unset，防跨模块并发污染共享 app env。
    ok = application:set_env(
        imboy,
        deletion_preflight_providers,
        organization_preflight_stub_providers:full_registry()
    ),
    ok = application:set_env(imboy, preflight_stub_behavior, Behavior),
    case whereis(user_deletion_logic) of
        undefined ->
            {ok, _} = user_deletion_logic:start_link(),
            ok;
        _Pid ->
            ok
    end.

reset_preflight_stub() ->
    application:unset_env(imboy, deletion_preflight_providers),
    application:unset_env(imboy, preflight_stub_behavior),
    ok.

%% ===================================================================
%% 1. 多域端到端：消息/朋友圈/附件/E2EE 密钥/群所有权转移
%% ===================================================================

multi_domain_e2e_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = new_uid(),
        Member = new_uid(),
        GroupId = new_uid(),
        MsgId = new_uid(),
        PostId = new_uid(),
        try
            ok = create_user(Uid),
            ok = create_user(Member),
            %% 群（Uid 拥有，Member 是成员）→ 应转移给 Member 而非删群
            {ok, _} = elib_pg:query(
                <<
                    "INSERT INTO public.\"group\" (id, owner_uid, creator_uid)"
                    " VALUES ($1, $2, $1)"
                >>,
                [GroupId, Uid]
            ),
            {ok, _} = elib_pg:query(
                <<
                    "INSERT INTO public.group_member (id, group_id, user_id)"
                    " VALUES ($1, $2, $3)"
                >>,
                [new_uid(), GroupId, Member]
            ),
            %% 个人消息/朋友圈/附件/E2EE 密钥
            MsgKey = <<"m1-", (integer_to_binary(MsgId))/binary>>,
            {ok, 1} = personal_message_order_repo:execute_formal(
                Uid,
                Member,
                MsgKey,
                <<
                    "INSERT INTO public.msg_c2c (id, from_id, to_id, msg_id, msg_type, payload, conv_seq)"
                    " VALUES ($1, $2, $3, $4, 't', 'p', $5)"
                >>,
                [MsgId, Uid, Member, MsgKey]
            ),
            {ok, _} = elib_pg:query(
                <<
                    "INSERT INTO public.moment_post (id, author_uid, content)"
                    " VALUES ($1, $2, 'hello')"
                >>,
                [PostId, Uid]
            ),
            {ok, _} = elib_pg:query(
                <<
                    "INSERT INTO public.attachment (id, creator_user_id, path)"
                    " VALUES ($1, $2, $3)"
                >>,
                [new_uid(), Uid, <<"objects/u/", (integer_to_binary(Uid))/binary, "/att1.bin">>]
            ),
            {ok, _} = elib_pg:query(
                <<
                    "INSERT INTO public.olm_identity (id, user_id, device_id,"
                    " ed25519_key, curve25519_key, signature)"
                    " VALUES ($1, $2, 'd1', 'ek', 'ck', 'sig')"
                >>,
                [new_uid(), Uid]
            ),
            %% 注销申请（过期）
            ok = seed_expired_request(Uid),
            ok = ensure_sweeper(),
            %% [ORG-02 裁决A] 计数语义：preflight 拒后 mark_failed 回 pending，
            %% 同一轮重试至 failed，失败尝试不计入完成数。
            {ok, 0} = user_deletion_logic:cleanup_now(),

            %% [ORG-02 裁决A] 期望翻转：计划 §1.6 冻结原文「禁止给未实现域
            %% 默认空 blocker（未注册域=facts 不可得=拒）」——EB/CS/Agent 域
            %% provider 未注册 → preflight 整体拒
            %% （DEPENDENCY_FACTS_UNAVAILABLE），删除事务回滚，零部分删除。
            %% 依据：control/ruling-ORG02-plan1-orchestrator-tests.md（裁决A）。
            %% 原期望（Plan1）：用户主行/消息/朋友圈/E2EE 密钥已删、群 owner
            %% 已转移、任务 completed —— 让位于冻结合同。
            {ok, [_]} = elib_pg:query(
                <<"SELECT id FROM public.\"user\" WHERE id = $1">>, [Uid]
            ),
            {ok, [_]} = elib_pg:query(
                <<"SELECT id FROM public.msg_c2c WHERE id = $1">>, [MsgId]
            ),
            {ok, [_]} = elib_pg:query(
                <<"SELECT id FROM public.moment_post WHERE id = $1">>, [PostId]
            ),
            {ok, [_]} = elib_pg:query(
                <<"SELECT id FROM public.olm_identity WHERE user_id = $1">>, [Uid]
            ),
            %% 群幸存且 owner 未转移（转移与删除同事务，整体回滚）
            {ok, [#{<<"owner_uid">> := Uid}]} = elib_pg:query(
                <<"SELECT owner_uid FROM public.\"group\" WHERE id = $1">>, [GroupId]
            ),
            %% 任务被稳定拒绝：pending（attempts<max 待重试），last_error 含
            %% 稳定原因 DEPENDENCY_FACTS_UNAVAILABLE
            {ok, #{
                <<"status">> := <<"failed">>, <<"attempts">> := 5, <<"last_error">> := LastError
            }} =
                user_deletion_job_repo:find_by_user(Uid),
            {_Pos, _Len} = binary:match(LastError, <<"DEPENDENCY_FACTS_UNAVAILABLE">>)
        after
            reset_preflight_stub(),
            cleanup_user(Uid),
            cleanup_user(Member),
            {ok, _} = elib_pg:execute(
                <<"DELETE FROM public.msg_store_seq WHERE conv_key=$1">>,
                [msg_archive_repo:conv_key(<<"c2c">>, Uid, Member)]
            ),
            _ = elib_pg:query(<<"DELETE FROM public.\"group\" WHERE id = $1">>, [GroupId])
        end
    end).

%% ===================================================================
%% 2. 支付余额门 + 失败重试：余额非 0 → failed；清零后重试完成
%% ===================================================================

balance_gate_and_retry_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = new_uid(),
        try
            ok = create_user(Uid),
            {ok, _} = elib_pg:query(
                <<
                    "INSERT INTO public.wallet (id, user_id, balance)"
                    " VALUES ($1, $2, 100)"
                >>,
                [new_uid(), Uid]
            ),
            ok = seed_expired_request(Uid),
            ok = ensure_sweeper(),
            %% 余额门（认领 SQL 跳过）：任务保持 pending，不烧 attempts
            {ok, 0} = user_deletion_logic:cleanup_now(),
            {ok, #{<<"status">> := <<"pending">>}} =
                user_deletion_job_repo:find_by_user(Uid),
            2 = user_status(Uid),
            %% 运营清零后重试
            {ok, _} = elib_pg:query(
                <<"UPDATE public.wallet SET balance = 0 WHERE user_id = $1">>, [Uid]
            ),
            %% 同轮重试至 failed，完成数仍为 0。
            {ok, 0} = user_deletion_logic:cleanup_now(),
            %% [ORG-02 裁决A] 期望翻转：计划 §1.6「未注册域=facts 不可得=拒」
            %% （EB/CS/Agent provider 未注册）→ preflight 整体拒
            %% （DEPENDENCY_FACTS_UNAVAILABLE），删除事务回滚，零部分删除。
            %% 依据：control/ruling-ORG02-plan1-orchestrator-tests.md（裁决A）。
            %% 原期望（Plan1）：用户主行已删、任务 completed —— 让位于冻结合同。
            {ok, [_]} = elib_pg:query(
                <<"SELECT id FROM public.\"user\" WHERE id = $1">>, [Uid]
            ),
            {ok, #{
                <<"status">> := <<"failed">>, <<"attempts">> := 5, <<"last_error">> := LastError
            }} =
                user_deletion_job_repo:find_by_user(Uid),
            {_Pos, _Len} = binary:match(LastError, <<"DEPENDENCY_FACTS_UNAVAILABLE">>)
        after
            reset_preflight_stub(),
            cleanup_user(Uid)
        end
    end).

user_status(Uid) ->
    {ok, [#{<<"status">> := S}]} =
        elib_pg:query(<<"SELECT status FROM public.\"user\" WHERE id = $1">>, [Uid]),
    S.

%% ===================================================================
%% 3. 并发 worker 认领：同一任务只被一个 worker 拿到
%% ===================================================================

%% 放行路径：五域 stub 全部放行（ok_all）→ preflight 通过 → 删除事务真执行
%% → 任务 completed、用户主行消失。provider 登记完成（0c53bc17/d62faaf2）后
%% 该语义此前无用例覆盖（stable-failures-root-cause-20260929.md 处置建议）。
all_providers_allow_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = new_uid(),
        try
            ok = create_user(Uid),
            ok = seed_expired_request(Uid),
            {ok, _} = elib_pg:query(
                <<"INSERT INTO public.user_log (type, uid, body) VALUES (102, $1, '{}'::jsonb)">>,
                [Uid]
            ),
            assert_deletion_log_status(Uid, <<"requested">>, <<"not_started">>, null),
            assert_approval_waits_for_grace_period(Uid),
            {ok, 1} = user_deletion_logic:cleanup_now(),
            {ok, []} = elib_pg:query(
                <<"SELECT id FROM public.\"user\" WHERE id = $1">>, [Uid]
            ),
            {ok, []} = elib_pg:query(
                <<"SELECT id FROM public.user_deletion_request WHERE user_id = $1">>, [Uid]
            ),
            {ok, #{<<"status">> := <<"completed">>, <<"finished_at">> := FinishedAt}} =
                user_deletion_job_repo:find_by_user(Uid),
            ?assert(FinishedAt =/= null),
            ?assert(FinishedAt =/= undefined),
            assert_deletion_log_status(Uid, <<"approved">>, <<"completed">>, FinishedAt),
            {ok, #{
                execution := #{
                    status := <<"completed">>,
                    finished_at := FinishedAt,
                    request_status_at_execution := <<"approved">>,
                    request_approved_at := ApprovalAt
                }
            }} =
                user_logic:deletion_status(Uid),
            ?assert(is_binary(ApprovalAt)),
            {error, <<"deletion_cancellation_unavailable">>} = user_logic:cancel_logout(Uid, #{})
        after
            {ok, _} = elib_pg:execute(<<"DELETE FROM public.user_log WHERE uid = $1">>, [Uid]),
            reset_preflight_stub(),
            cleanup_user(Uid)
        end
    end).

assert_approval_waits_for_grace_period(Uid) ->
    {ok, _} = elib_pg:execute(
        <<"UPDATE public.user_deletion_request SET requested_at = NOW() WHERE user_id = $1">>, [Uid]
    ),
    {ok, [#{<<"id">> := Uid}]} = user_ds:approve_logout_apply(Uid),
    {ok, []} = user_ds:approve_logout_apply(Uid),
    {ok, []} = user_ds:reject_logout_apply(Uid),
    {error, <<"deletion_cancellation_unavailable">>} = user_logic:cancel_logout(Uid, #{}),
    assert_deletion_log_status(Uid, <<"approved">>, <<"pending">>, null),
    {ok, #{status := <<"approved">>, execution := #{status := <<"pending">>}}} =
        user_logic:deletion_status(Uid),
    ok = ensure_sweeper(ok_all),
    {ok, 0} = user_deletion_logic:cleanup_now(),
    {ok, _} = elib_pg:execute(
        <<"UPDATE public.user_deletion_request SET requested_at = NOW() - interval '61 days' WHERE user_id = $1">>,
        [Uid]
    ),
    ok.

assert_deletion_log_status(Uid, Request, Execution, FinishedAt) ->
    Filter = <<" AND l.uid = $1">>,
    {ok, #{total := 1, list := [Row]}} = user_log_ds:page_logout_apply_log(Filter, [Uid], 1, 20),
    ?assertMatch(
        #{
            <<"request_status">> := Request,
            <<"execution_status">> := Execution,
            <<"execution_finished_at">> := FinishedAt
        },
        Row
    ),
    ?assertNot(maps:is_key(<<"last_error">>, Row)),
    {ok, [Export]} = user_log_ds:list_logout_apply_log_chunk(Filter, [Uid], 20, 0),
    ?assertNot(maps:is_key(<<"execution_status">>, Export)).

approve_without_formal_request_rolls_back_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = new_uid(),
        try
            ok = create_user(Uid),
            {ok, 1} = elib_pg:execute(
                <<"UPDATE public.\"user\" SET status = 2 WHERE id = $1">>, [Uid]
            ),
            {error, formal_deletion_request_not_pending} = user_ds:approve_logout_apply(Uid),
            2 = user_status(Uid),
            {ok, undefined} = user_deletion_request_repo:find_latest(Uid),
            {ok, undefined} = user_deletion_job_repo:find_by_user(Uid)
        after
            cleanup_user(Uid)
        end
    end).

complete_job_only_while_running_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = new_uid(),
        JobId = new_uid(),
        try
            {ok, _} = elib_pg:query(
                <<"INSERT INTO public.user_deletion_job (id, user_id, status) VALUES ($1, $2, 'pending')">>,
                [JobId, Uid]
            ),
            Complete = fun() ->
                elib_pg:with_tx(fun(Conn) ->
                    user_deletion_job_repo:mark_completed_tx(
                        Conn, JobId, #{<<"status">> => <<"requested">>, <<"approved_at">> => null}
                    )
                end)
            end,
            {error, job_not_running} = Complete(),
            {ok, _} = elib_pg:query(
                <<"UPDATE public.user_deletion_job SET status = 'running' WHERE id = $1">>,
                [JobId]
            ),
            ok = Complete(),
            {error, job_not_running} = Complete(),
            {error, job_not_running} = user_deletion_job_repo:mark_failed(JobId, <<"stale">>, 5),
            {ok, #{<<"status">> := <<"completed">>}} = user_deletion_job_repo:find_by_user(Uid)
        after
            cleanup_user(Uid)
        end
    end).

cancel_after_claim_preserves_user_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = new_uid(),
        try
            ok = create_user(Uid),
            ok = seed_expired_request(Uid),
            {ok, #{requested_at := PreviousRequestAt}} = user_logic:deletion_status(Uid),
            {ok, _} = elib_pg:with_tx(fun(Conn) ->
                user_deletion_job_repo:ensure_job_tx(Conn, Uid, <<"fixture">>)
            end),
            {ok, #{<<"user_id">> := Uid} = Job} =
                user_deletion_job_repo:claim_pending_expired(?GRACE, <<"cancel-race">>),
            Parent = self(),
            Ref = make_ref(),
            Worker = spawn(fun() ->
                receive
                    {execute, Ref} -> Parent ! {Ref, user_deletion_logic:execute_job(Job, ?GRACE)}
                end
            end),
            try
                ok = elib_pg:with_tx(fun(Conn) ->
                    {ok, 1} = user_ds:unmark_logout_apply_in_tx(Conn, Uid),
                    Worker ! {execute, Ref},
                    {ok, 1} = user_deletion_request_repo:cancel_request_tx(Conn, Uid),
                    ok
                end),
                receive
                    {Ref, Result} -> ?assertEqual(failed_attempt, Result)
                after 3000 -> error(deletion_worker_timeout)
                end,
                1 = user_status(Uid),
                {ok, #{status := <<"cancelled">>}} = user_logic:deletion_status(Uid),
                {ok, #{<<"status">> := <<"pending">>, <<"attempts">> := 0}} =
                    user_deletion_job_repo:find_by_user(Uid),
                {ok, none} = user_deletion_job_repo:claim_pending_expired(?GRACE, <<"next">>),
                {ok, "success"} = user_logic:apply_logout(Uid, #{headers => #{}}),
                {ok, #{status := <<"requested">>, requested_at := NewRequestAt}} =
                    user_logic:deletion_status(Uid),
                ?assert(PreviousRequestAt =/= NewRequestAt),
                {ok, none} = user_deletion_job_repo:claim_pending_expired(?GRACE, <<"reapply">>)
            after
                exit(Worker, kill)
            end
        after
            {ok, _} = elib_pg:execute(<<"DELETE FROM public.user_log WHERE uid = $1">>, [Uid]),
            cleanup_user(Uid)
        end
    end).

concurrent_claim_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = new_uid(),
        try
            ok = create_user(Uid),
            ok = seed_expired_request(Uid),
            %% 直接插 pending job（绕过周期执行，保持 user status=2 供认领）
            {ok, _} = elib_pg:query(
                <<
                    "INSERT INTO public.user_deletion_job (id, user_id, account, status)"
                    " VALUES ($1, $2, 'probe', 'pending')"
                >>,
                [new_uid(), Uid]
            ),
            Self = self(),
            Claim = fun() ->
                Self ! {claimed, user_deletion_job_repo:claim_pending_expired(60, <<"probe">>)}
            end,
            spawn(Claim),
            R2 = user_deletion_job_repo:claim_pending_expired(60, <<"probe2">>),
            R1 =
                receive
                    {claimed, Res} -> Res
                after 15000 -> {error, timeout}
                end,
            Winners = [M || {ok, M} <- [R1, R2], is_map(M)],
            Losers = [R || {ok, none} = R <- [R1, R2]],
            1 = length(Winners),
            1 = length(Losers)
        after
            reset_preflight_stub(),
            cleanup_user(Uid)
        end
    end).

%% ===================================================================
%% 4. Garage 对象删除（真实 Garage 集成：不存在的 key 由 Garage 403
%% 拒绝，best-effort 记日志不阻塞 completed——multi 场景已覆盖该路径）
%% ===================================================================

garage_enqueue_best_effort_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = new_uid(),
        try
            ok = create_user(Uid),
            {ok, _} = elib_pg:query(
                <<
                    "INSERT INTO public.attachment (id, creator_user_id, path)"
                    " VALUES ($1, $2, $3)"
                >>,
                [new_uid(), Uid, <<"objects/u/", (integer_to_binary(Uid))/binary, "/probe.bin">>]
            ),
            ok = seed_expired_request(Uid),
            ok = ensure_sweeper(),
            %% 同轮重试至 failed，完成数仍为 0。
            {ok, 0} = user_deletion_logic:cleanup_now(),
            %% [ORG-02 裁决A] 期望翻转：计划 §1.6「未注册域=facts 不可得=拒」
            %% （EB/CS/Agent provider 未注册）→ preflight 整体拒
            %% （DEPENDENCY_FACTS_UNAVAILABLE），事务回滚：附件行与用户行俱在。
            %% 依据：control/ruling-ORG02-plan1-orchestrator-tests.md（裁决A）。
            %% 原期望（Plan1）：附件行已清、任务 completed、用户已删 ——
            %% 让位于冻结合同。
            {ok, [_]} = elib_pg:query(
                <<"SELECT id FROM public.attachment WHERE creator_user_id = $1">>,
                [Uid]
            ),
            {ok, #{
                <<"status">> := <<"failed">>, <<"attempts">> := 5, <<"last_error">> := LastError
            }} =
                user_deletion_job_repo:find_by_user(Uid),
            {_Pos, _Len} = binary:match(LastError, <<"DEPENDENCY_FACTS_UNAVAILABLE">>),
            {ok, [_]} = elib_pg:query(
                <<"SELECT id FROM public.\"user\" WHERE id = $1">>, [Uid]
            )
        after
            reset_preflight_stub(),
            cleanup_user(Uid)
        end
    end).

%% ===================================================================
%% 5. L-03:LOCAL:01 备份恢复不复活已删除数据或旧权限（模拟旧备份
%%    恢复 → 墓碑重放）。旧备份是删除前时点快照：恢复后、重放前
%%    旧数据/旧令牌在场是**如实残留**（不宣称备份不含已删数据）；
%%    防线是「先重放删除编排再开放访问」——重放后被删数据不复现、
%%    撤权（设备绑定/推送令牌）不复活。
%% ===================================================================

l03_insert_grants(Uid) ->
    %% 旧权限载体：设备绑定 + 推送令牌（都在删除清单内）
    {ok, _} = elib_pg:query(
        <<
            "INSERT INTO public.user_device (id, user_id, device_id, device_type)"
            " VALUES ($1, $2, 'l03-dev', 'phone')"
        >>,
        [new_uid(), Uid]
    ),
    {ok, _} = elib_pg:query(
        <<
            "INSERT INTO public.push_token"
            " (id, user_id, device_id, device_type, platform, token)"
            " VALUES ($1, $2, 'l03-dev', 'android', 'fcm', 'l03tok')"
        >>,
        [new_uid(), Uid]
    ),
    ok.

l03_insert_message(Uid, Member, MsgId) ->
    MsgKey = <<"l03-", (integer_to_binary(MsgId))/binary>>,
    personal_message_order_repo:execute_formal(
        Uid,
        Member,
        MsgKey,
        <<
            "INSERT INTO public.msg_c2c (id, from_id, to_id, msg_id, msg_type, payload, conv_seq)"
            " VALUES ($1, $2, $3, $4, 't', 'p', $5)"
        >>,
        [MsgId, Uid, Member, MsgKey]
    ).

l03_insert_olm(Uid) ->
    {ok, _} = elib_pg:query(
        <<
            "INSERT INTO public.olm_identity (id, user_id, device_id,"
            " ed25519_key, curve25519_key, signature)"
            " VALUES ($1, $2, 'd1', 'ek', 'ck', 'sig')"
        >>,
        [new_uid(), Uid]
    ),
    ok.

l03_assert_all_gone(Uid, MsgId) ->
    {ok, []} = elib_pg:query(<<"SELECT id FROM public.\"user\" WHERE id = $1">>, [Uid]),
    {ok, []} = elib_pg:query(
        <<"SELECT id FROM public.user_device WHERE user_id = $1">>, [Uid]
    ),
    {ok, []} = elib_pg:query(
        <<"SELECT id FROM public.push_token WHERE user_id = $1">>, [Uid]
    ),
    {ok, []} = elib_pg:query(<<"SELECT id FROM public.msg_c2c WHERE id = $1">>, [MsgId]),
    {ok, []} = elib_pg:query(
        <<"SELECT id FROM public.olm_identity WHERE user_id = $1">>, [Uid]
    ),
    {ok, []} = elib_pg:query(
        <<"SELECT id FROM public.user_deletion_request WHERE user_id = $1">>, [Uid]
    ),
    ok.

restore_replay_no_revival_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = new_uid(),
        Member = new_uid(),
        MsgId = new_uid(),
        ResurrectedMsgId = new_uid(),
        try
            %% --- 删除前（= 旧备份时点）：数据 + 权限载体 + 过期注销申请 ---
            ok = create_user(Uid),
            ok = create_user(Member),
            ok = seed_expired_request(Uid),
            ok = l03_insert_grants(Uid),
            {ok, 1} = l03_insert_message(Uid, Member, MsgId),
            ok = l03_insert_olm(Uid),
            %% --- 首次删除编排（BE-18 语义，五域放行）---
            ok = ensure_sweeper(ok_all),
            {ok, 1} = user_deletion_logic:cleanup_now(),
            ok = l03_assert_all_gone(Uid, MsgId),
            {ok, #{<<"status">> := <<"completed">>}} =
                user_deletion_job_repo:find_by_user(Uid),
            %% --- 模拟恢复旧备份（删除前时点）：被删行原样回到库里 ---
            %% 旧备份先于任务行存在：清掉 completed 任务，恢复 request/
            %% 数据/权限行，等价于 pg_restore 一份删除前 dump 的内容效果。
            {ok, _} = elib_pg:query(
                <<"DELETE FROM public.user_deletion_job WHERE user_id = $1">>, [Uid]
            ),
            ok = create_user(Uid),
            {ok, _} = elib_pg:query(
                <<"UPDATE public.\"user\" SET status = 2 WHERE id = $1">>, [Uid]
            ),
            ok = seed_expired_request(Uid),
            ok = l03_insert_grants(Uid),
            {ok, _} = elib_pg:query(
                <<
                    "INSERT INTO public.msg_c2c (id, from_id, to_id, msg_id, msg_type, payload, conv_seq)"
                    " VALUES ($1, $2, $3, $4, 't', 'p', 2)"
                >>,
                [
                    ResurrectedMsgId,
                    Uid,
                    Member,
                    <<"l03r-", (integer_to_binary(ResurrectedMsgId))/binary>>
                ]
            ),
            ok = l03_insert_olm(Uid),
            %% 重放前如实断言：旧数据/旧令牌确实随恢复回来了（残留可见，
            %% 这是备份时点语义，不是缺陷；防线在下面的重放）
            {ok, [_]} = elib_pg:query(
                <<"SELECT id FROM public.\"user\" WHERE id = $1">>, [Uid]
            ),
            {ok, [_]} = elib_pg:query(
                <<"SELECT id FROM public.user_device WHERE user_id = $1">>, [Uid]
            ),
            {ok, [_]} = elib_pg:query(
                <<"SELECT id FROM public.push_token WHERE user_id = $1">>, [Uid]
            ),
            {ok, [_]} = elib_pg:query(
                <<"SELECT id FROM public.msg_c2c WHERE from_id = $1">>, [Uid]
            ),
            %% --- 墓碑重放：恢复后、开放访问前，同一编排再执行 ---
            {ok, 1} = user_deletion_logic:cleanup_now(),
            %% --- 防复活断言：被删数据不复现、撤权不复活 ---
            ok = l03_assert_all_gone(Uid, ResurrectedMsgId),
            {ok, #{<<"status">> := <<"completed">>, <<"finished_at">> := FinishedAt}} =
                user_deletion_job_repo:find_by_user(Uid),
            ?assert(FinishedAt =/= null),
            ?assert(FinishedAt =/= undefined)
        after
            reset_preflight_stub(),
            cleanup_user(Uid),
            cleanup_user(Member),
            {ok, _} = elib_pg:execute(
                <<"DELETE FROM public.msg_store_seq WHERE conv_key=$1">>,
                [msg_archive_repo:conv_key(<<"c2c">>, Uid, Member)]
            )
        end
    end).

%% ===================================================================
%% 6. L-03:LOCAL:02 Legal Hold 不支持时不宣称支持
%%    注册表显式 unsupported；导出面显式 supported=false（不静默
%%    省略、不宣称支持）；无隐藏开关可翻转；合成演练 ≠ 生产恢复
%%    验收（口径见 scripts/restore_pg.sh 恢复完成提示）。
%% ===================================================================

l03_retention_policy_path() ->
    Cwd = filename:absname(""),
    Candidates = [
        filename:join(Cwd, "docs/compliance/retention-policy.yml"),
        filename:join(filename:dirname(Cwd), "docs/compliance/retention-policy.yml")
    ],
    case [P || P <- Candidates, filelib:is_regular(P)] of
        [P | _] -> P;
        [] -> erlang:error({retention_policy_not_found, Candidates})
    end.

legal_hold_unsupported_not_claimed_test_() ->
    ?TEST_WITH_APP(fun() ->
        %% 注册表口径：counsel 设计落地前全仓不支持
        {ok, Bin} = file:read_file(l03_retention_policy_path()),
        {_, _} = binary:match(Bin, <<"legal_hold: unsupported">>),
        %% 运行时导出面：显式 supported=false，不静默省略也不宣称支持
        #{<<"supported">> := false} = user_export_logic:legal_hold_status(),
        %% 不存在能把「不支持」翻转成「支持」的隐藏开关
        undefined = application:get_env(imboy, legal_hold),
        undefined = application:get_env(imboy, legal_hold_enabled)
    end).

%% ===================================================================
%% 7. L-03 shell 演练钩子（eunit 不自动发现非 _test 函数；仅供
%%    .task-state 演练脚本以 erl -eval 驱动真实编排的种子/删除/
%%    重放/状态探针。绝不在 eunit 轨道内调用——它们会 halt/写标记）
%% ===================================================================

-define(L03_STATE_QUERIES, [
    {user, <<"SELECT count(*) FROM public.\"user\" WHERE id = $1">>},
    {user_device, <<"SELECT count(*) FROM public.user_device WHERE user_id = $1">>},
    {push_token, <<"SELECT count(*) FROM public.push_token WHERE user_id = $1">>},
    {msg_c2c, <<"SELECT count(*) FROM public.msg_c2c WHERE from_id = $1 OR to_id = $1">>},
    {olm_identity, <<"SELECT count(*) FROM public.olm_identity WHERE user_id = $1">>},
    {deletion_request, <<"SELECT count(*) FROM public.user_deletion_request WHERE user_id = $1">>},
    {deletion_job, <<"SELECT count(*) FROM public.user_deletion_job WHERE user_id = $1">>}
]).

l03_count(Sql, Uid) ->
    {ok, [#{<<"count">> := N}]} = elib_pg:query(Sql, [Uid]),
    N.

l03_run(Fun) ->
    try
        ok =
            case persistent_term:get({?MODULE, l03_booted}, false) of
                true ->
                    ok;
                false ->
                    {ok, _} = eunit_runner:eunit_setup_with_db(),
                    persistent_term:put({?MODULE, l03_booted}, true),
                    ok
            end,
        Fun()
    catch
        Class:Reason:Stack ->
            io:format("L03_ERROR ~p:~p~n~p~n", [Class, Reason, Stack]),
            halt(2)
    end.

%% 演练种子：可证明归属的合成用户（数据 + 权限 + 过期注销申请），
%% 无任务行 = 删除前时点（等价于“即将被备份的旧状态”）。
l03_seed_drill_user() ->
    l03_run(fun() ->
        Uid = new_uid(),
        Member = new_uid(),
        MsgId = new_uid(),
        ok = create_user(Uid),
        ok = create_user(Member),
        {ok, _} = elib_pg:query(
            <<"UPDATE public.\"user\" SET status = 2 WHERE id = $1">>, [Uid]
        ),
        ok = l03_insert_grants(Uid),
        {ok, 1} = l03_insert_message(Uid, Member, MsgId),
        ok = l03_insert_olm(Uid),
        {ok, _} = elib_pg:query(
            <<
                "INSERT INTO public.user_deletion_request (id, user_id, status, requested_at)"
                " VALUES ($1, $2, 'requested', NOW() - interval '61 days')"
            >>,
            [new_uid(), Uid]
        ),
        io:format("L03_SEEDED uid=~b member=~b msg=~b~n", [Uid, Member, MsgId]),
        ok
    end),
    halt(0).

%% 演练删除/墓碑重放：真实 BE-18 编排（五域放行），成功以退出码表达。
l03_delete_drill_user(Uid) when is_integer(Uid) ->
    l03_run(fun() ->
        ok = ensure_sweeper(ok_all),
        Result = user_deletion_logic:cleanup_now(),
        Job = user_deletion_job_repo:find_by_user(Uid),
        lists:foreach(
            fun({K, Sql}) ->
                io:format("L03_STATE ~b ~s=~b~n", [Uid, K, l03_count(Sql, Uid)])
            end,
            ?L03_STATE_QUERIES
        ),
        io:format("L03_DELETE uid=~b result=~p job=~p~n", [Uid, Result, Job]),
        case {Result, Job} of
            {{ok, N}, {ok, #{<<"status">> := <<"completed">>}}} when N >= 1 ->
                ok;
            Other ->
                io:format("L03_DELETE_UNEXPECTED ~p~n", [Other]),
                halt(3)
        end
    end),
    halt(0).

%% 演练状态探针：逐表打印 key=value，供 bash 断言。
l03_state(Uid) when is_integer(Uid) ->
    l03_run(fun() ->
        lists:foreach(
            fun({K, Sql}) ->
                io:format("L03_STATE ~b ~s=~b~n", [Uid, K, l03_count(Sql, Uid)])
            end,
            ?L03_STATE_QUERIES
        ),
        ok
    end),
    halt(0).
