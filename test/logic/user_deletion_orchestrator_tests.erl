-module(user_deletion_orchestrator_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

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
    erlang:system_time(millisecond) * 1000 + rand:uniform(999).

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
        <<"DELETE FROM public.wallet WHERE user_id = $1">>,
        <<"DELETE FROM public.group_member WHERE user_id = $1">>,
        <<"DELETE FROM public.\"group\" WHERE id = $1">>,
        <<"DELETE FROM public.olm_identity WHERE user_id = $1">>,
        <<"DELETE FROM public.\"user\" WHERE id = $1">>
    ]).

ensure_sweeper() ->
    application:set_env(imboy, user_deletion_enabled, true),
    application:set_env(imboy, user_deletion_retention_days, ?GRACE),
    application:set_env(imboy, user_deletion_batch_size, 10),
    application:set_env(imboy, user_deletion_max_attempts, 5),
    case whereis(user_deletion_logic) of
        undefined ->
            {ok, _} = user_deletion_logic:start_link(),
            ok;
        _Pid ->
            ok
    end.

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
            {ok, _} = elib_pg:query(
                <<
                    "INSERT INTO public.msg_c2c (id, from_id, to_id, msg_id, msg_type, payload)"
                    " VALUES ($1, $2, $3, 'm1', 't', 'p')"
                >>,
                [MsgId, Uid, Member]
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
                    " VALUES ($1, $2, 'objects/u/att1.bin')"
                >>,
                [new_uid(), Uid]
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
            R = user_deletion_logic:cleanup_now(),
            io:format(user, "~nDBG_CLEANUP=~p~n", [R]),
            io:format(
                user,
                "DBG_USER_ROWS=~p~n",
                [elib_pg:query(<<"SELECT id, status FROM public.\"user\" WHERE id = $1">>, [Uid])]
            ),
            {ok, 1} = R,

            %% 用户主行/消息/E2EE 密钥已删；群已转移
            {ok, []} = elib_pg:query(
                <<"SELECT id FROM public.\"user\" WHERE id = $1">>, [Uid]
            ),
            {ok, []} = elib_pg:query(
                <<"SELECT id FROM public.msg_c2c WHERE id = $1">>, [MsgId]
            ),
            {ok, []} = elib_pg:query(
                <<"SELECT id FROM public.moment_post WHERE id = $1">>, [PostId]
            ),
            {ok, []} = elib_pg:query(
                <<"SELECT id FROM public.olm_identity WHERE user_id = $1">>, [Uid]
            ),
            %% 群幸存且 owner 已转移（D-02 转移决策）
            {ok, [#{<<"owner_uid">> := Member}]} = elib_pg:query(
                <<"SELECT owner_uid FROM public.\"group\" WHERE id = $1">>, [GroupId]
            ),
            %% 任务墓碑：completed
            {ok, #{<<"status">> := <<"completed">>}} =
                user_deletion_job_repo:find_by_user(Uid)
        after
            io:format(
                user,
                "~nDBG_JOB=~p~n",
                [user_deletion_job_repo:find_by_user(Uid)]
            ),
            cleanup_user(Uid),
            cleanup_user(Member),
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
            %% 运营清零后重试 → 完成
            {ok, _} = elib_pg:query(
                <<"UPDATE public.wallet SET balance = 0 WHERE user_id = $1">>, [Uid]
            ),
            {ok, 1} = user_deletion_logic:cleanup_now(),
            {ok, []} = elib_pg:query(
                <<"SELECT id FROM public.\"user\" WHERE id = $1">>, [Uid]
            ),
            {ok, #{<<"status">> := <<"completed">>}} =
                user_deletion_job_repo:find_by_user(Uid)
        after
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
                    " VALUES ($1, $2, 'objects/u/probe.bin')"
                >>,
                [new_uid(), Uid]
            ),
            ok = seed_expired_request(Uid),
            ok = ensure_sweeper(),
            {ok, 1} = user_deletion_logic:cleanup_now(),
            %% 附件行已清；对象删除对外尝试失败（403）仅记日志，任务仍完成
            {ok, []} = elib_pg:query(
                <<"SELECT id FROM public.attachment WHERE creator_user_id = $1">>,
                [Uid]
            ),
            {ok, #{<<"status">> := <<"completed">>}} =
                user_deletion_job_repo:find_by_user(Uid),
            {ok, []} = elib_pg:query(
                <<"SELECT id FROM public.\"user\" WHERE id = $1">>, [Uid]
            )
        after
            cleanup_user(Uid)
        end
    end).
