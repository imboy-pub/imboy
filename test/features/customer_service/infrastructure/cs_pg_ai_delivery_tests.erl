%%% @doc P0-3（v4 02-B）：B 组锁序 + 租约 + 投递统一协议 focused 套件。
%%%
%%% 覆盖 B-1…B-6 + I3/I4/I5（04-task-contracts P0-3）：
%%%   * B-1 陈旧 version 提交影响 0 行（A 认领 → B 接管 → A 旧栅栏失效）；
%%%   * B-2 同 inbound 幂等建单（uq_csaij_org_inbound）；
%%%   * B-3 并发投递只产生 1 条 delivery（canonical 串行 + uq_csaid_reply）；
%%%   * B-4 投递前输入变更 ⇒ {skipped, superseded} 且 delivery = 0；
%%%   * B-5 无 L1 时 input_version 不稳定（探针 B2 复现）+ 方案甲锁静态断言；
%%%   * B-6 混合并发（claim/renew/takeover/deliver）无 40P01 死锁。
%%%
%%% 事务结构（与实现互证）：L1 = canonical 的 `fetch_conversation FOR NO
%%% KEY UPDATE`（方案甲，conversation 行锁；cs 会话↔conversation 一对一）；
%%% deadline/input_version 复核 + L4 + job CAS 在 ai_delivery_hook 内同
%%% canonical 事务（cs_session_app:compose_ai_hook 组合，失败整体回滚）。
%%%
%%% 隔离：`cs_pg_test_fixture:new_scope/0` 随机 TSID scope；交错用例持
%%% 原生 epgsql 连接；环境不可用 ⇒ `erlang:error/1`（不是 skip）。
-module(cs_pg_ai_delivery_tests).

-include_lib("eunit/include/eunit.hrl").

-define(FIX, cs_pg_test_fixture).

cs_pg_ai_delivery_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun cases/1}.

setup() ->
    case eunit_runner:eunit_setup_with_db() of
        {ok, Conn} ->
            Previous = application:get_env(imboy, eb_enterprise_keyring),
            ok = application:set_env(imboy, eb_enterprise_keyring, #{
                active_version => 1,
                keys => #{1 => binary:encode_hex(crypto:strong_rand_bytes(32), lowercase)}
            }),
            {ok, Conn, Previous};
        {error, Reason} ->
            {error, Reason}
    end.

cleanup({ok, Conn, Previous}) ->
    case Previous of
        undefined -> application:unset_env(imboy, eb_enterprise_keyring);
        {ok, Value} -> application:set_env(imboy, eb_enterprise_keyring, Value)
    end,
    eunit_runner:eunit_cleanup_db(Conn);
cleanup(Other) ->
    Other.

cases({ok, _Conn, _Previous}) ->
    [
        {timeout, 60, fun b1_stale_version_commits_zero_rows/0},
        {timeout, 60, fun b2_same_inbound_creates_single_job/0},
        {timeout, 90, fun b3_concurrent_delivery_single_row/0},
        {timeout, 60, fun b4_new_input_supersedes_delivery/0},
        {timeout, 60, fun b5_input_version_stability_and_l1/0},
        {timeout, 90, fun b6_mixed_concurrency_no_deadlock/0},
        {timeout, 60, fun skipped_status_mapping/0}
    ];
cases({error, Reason}) ->
    erlang:error({csai_b_group_suite_db_unavailable, Reason}).

%% ===================================================================
%% B-1：陈旧 version 提交影响 0 行（I4）
%% ===================================================================

b1_stale_version_commits_zero_rows() ->
    Scene = seed_ai_scene(),
    try
        #{org_id := Org, job_id := JobId} = Scene,
        {ok, #{version := V1}} = cs_pg_ai:claim_job(Org, JobId, <<"worker-a">>, 60),
        %% 强制租约过期后 B 接管（接管条件含 lease_until < now()，02-B.5）。
        ok = expire_lease(Org, JobId),
        ok = cs_pg_ai:takeover_job(Org, JobId, <<"worker-b">>, V1, 60),
        %% A 用旧 version 续租/完成 ⇒ 全部 0 行（lease_lost）。
        ?assertEqual(
            {error, lease_lost},
            cs_pg_ai:renew_lease(
                Org, JobId, <<"worker-a">>, V1, 60
            )
        ),
        %% A 以旧栅栏投递 ⇒ 无任何副作用（delivery = 0，消息 = 0）。
        Result = cs_ai_delivery_app:deliver(Org, maps:get(workspace_id, Scene), #{
            job_id => JobId,
            worker_id => <<"worker-a">>,
            seat_identity_id => maps:get(seat_identity_id, Scene),
            reply => <<"stale">>
        }),
        ?assertMatch({error, lease_lost}, Result),
        ?assertEqual(0, delivery_count(Org)),
        ?assertEqual(0, ai_message_count(Scene))
    after
        cleanup_scene(Scene)
    end.

%% ===================================================================
%% B-2：同 inbound 只建一个 job（DB 幂等键）
%% ===================================================================

b2_same_inbound_creates_single_job() ->
    Scene = seed_ai_scene(),
    try
        #{org_id := Org, workspace_id := Ws, job_id := JobId} = Scene,
        %% 直接重复 INSERT（模拟 sweep/重放双建单）⇒ 第二条 0 行。
        Duplicate = [
            ?FIX:id(),
            Org,
            Ws,
            maps:get(session_id, Scene),
            maps:get(conversation_id, Scene),
            maps:get(inbound_id, Scene),
            1,
            maps:get(inbound_id, Scene),
            <<"pending">>
        ],
        %% fixture:exec 透传 epgsql 原始错误形状（未经 cs_pg_common 归一）。
        {error, {error, error, <<"23505">>, unique_violation, _, _}} =
            ?FIX:exec(
                <<
                    "INSERT INTO customer_service_ai_job"
                    " (id, organization_id, workspace_id, session_id, conversation_id,"
                    "  inbound_message_id, generation_epoch, context_watermark, status,"
                    "  deadline_at)"
                    " VALUES ($1,$2,$3,$4,$5,$6,$7,$8,$9,"
                    "         now() + make_interval(secs => 300))"
                >>,
                Duplicate
            ),
        ?assertEqual(1, job_count(Org)),
        ?assert(is_integer(JobId))
    after
        cleanup_scene(Scene)
    end.

%% ===================================================================
%% B-3：并发投递只产生 1 条 delivery（I3）
%% ===================================================================

b3_concurrent_delivery_single_row() ->
    Scene = seed_ai_scene(),
    try
        #{org_id := Org, workspace_id := Ws, job_id := JobId} = Scene,
        {ok, _} = cs_pg_ai:claim_job(Org, JobId, <<"worker-a">>, 60),
        Params = #{
            job_id => JobId,
            worker_id => <<"worker-a">>,
            seat_identity_id => maps:get(seat_identity_id, Scene),
            reply => <<"concurrent answer">>
        },
        %% 双进程并发投递（同 owner 同栅栏——canonical L1 串行化后一成一拒）。
        Parent = self(),
        lists:foreach(
            fun(I) ->
                _ = spawn(fun() ->
                    Parent ! {I, (catch cs_ai_delivery_app:deliver(Org, Ws, Params))}
                end)
            end,
            [1, 2]
        ),
        Results = [
            receive
                {I, R} -> R
            end
         || I <- [1, 2]
        ],
        %% 至少一个 delivered；重复投递经 client_msg_id 幂等键命中既有成功
        %% （canonical 无半态重试安全，02-B.4 幂等语义）或被 hook 拒绝——
        %% 两种路径都不产生新副作用。硬断言：delivery 恰 1 行、消息恰 1 条。
        Delivered = length([ok || {ok, delivered} <- Results]),
        ?assert(Delivered >= 1),
        ?assertEqual(1, delivery_count(Org)),
        ?assertMatch({ok, #{status := completed}}, cs_pg_ai:fetch_job(Org, JobId)),
        %% 再投（陈旧）⇒ 冲突而非重复 delivery。
        ?assertMatch(
            {error, {conflict, completed}},
            cs_ai_delivery_app:deliver(Org, Ws, Params)
        ),
        ?assertEqual(1, delivery_count(Org))
    after
        cleanup_scene(Scene)
    end.

%% ===================================================================
%% B-4：投递前新 contact 消息 ⇒ superseded 且不投递（I5）
%% ===================================================================

b4_new_input_supersedes_delivery() ->
    Scene = seed_ai_scene(),
    try
        #{org_id := Org, workspace_id := Ws, job_id := JobId} = Scene,
        {ok, _} = cs_pg_ai:claim_job(Org, JobId, <<"worker-a">>, 60),
        %% 新 contact 消息超越 job 的 context_watermark。
        Later = seed_contact_message(Scene),
        _ = Later,
        Result = cs_ai_delivery_app:deliver(Org, Ws, #{
            job_id => JobId,
            worker_id => <<"worker-a">>,
            seat_identity_id => maps:get(seat_identity_id, Scene),
            reply => <<"stale answer">>
        }),
        ?assertEqual({skipped, superseded}, Result),
        ?assertEqual(0, delivery_count(Org)),
        ?assertEqual(0, ai_message_count(Scene)),
        %% job 落终态：cancelled + last_error_code=superseded（02-B.4 形状）。
        {ok, Job} = cs_pg_ai:fetch_job(Org, JobId),
        ?assertEqual(cancelled, maps:get(status, Job)),
        ?assertEqual(<<"superseded">>, job_error_code(Org, JobId))
    after
        cleanup_scene(Scene)
    end.

%% ===================================================================
%% B-5：无 L1 时 input_version 不稳定（探针 B2 复现）+ 方案甲锁断言
%% ===================================================================

b5_input_version_stability_and_l1() ->
    Scene = seed_ai_scene(),
    C1 = raw_conn(),
    try
        #{org_id := Org, conversation_id := ConvId, inbound_id := Inbound} = Scene,
        %% 探针 B2 复现（对照组）：C1 插入 contact 消息未提交时，无锁读
        %% max(id) 拿不到它（可见性缺口 = 不稳定窗口的实证）。
        Later = ?FIX:id(),
        ok = sql(C1, <<"BEGIN">>),
        ok = insert_message_tx(C1, Scene, Later, contact),
        Before = read_input_version(Org, ConvId),
        ?assertEqual(Inbound, Before),
        ok = sql(C1, <<"COMMIT">>),
        After = read_input_version(Org, ConvId),
        ?assertEqual(Later, After),
        %% 方案甲静态断言：canonical 的 fetch_conversation 带 FOR NO KEY
        %% UPDATE（入站与投递共用 canonical ⇒ 同 conversation 串行，
        %% 复核在持锁事务内稳定——B-3 的串行行为为其运行时佐证）。
        ?assert(
            binary:match(eb_pg_store_sql:sql(fetch_conversation), <<"FOR NO KEY UPDATE">>) =/=
                nomatch
        ),
        ?FIX:exec(
            <<"DELETE FROM enterprise_message WHERE organization_id=$1 AND id=$2">>,
            [Org, Later]
        )
    after
        close_conn(C1),
        cleanup_scene(Scene)
    end.

%% ===================================================================
%% B-6：混合并发（claim/renew/takeover/deliver）无 40P01
%% ===================================================================

b6_mixed_concurrency_no_deadlock() ->
    Scene = seed_ai_scene(),
    try
        #{org_id := Org, workspace_id := Ws, job_id := JobId} = Scene,
        {ok, _} = cs_pg_ai:claim_job(Org, JobId, <<"worker-a">>, 60),
        Parent = self(),
        %% 三路并发：续租、（过期后）接管、投递——锁序 L1→L2 固定，
        %% 任何 40P01 都说明出现逆序。
        Work =
            fun
                (renew) ->
                    cs_pg_ai:renew_lease(Org, JobId, <<"worker-a">>, 2, 60);
                (takeover) ->
                    cs_pg_ai:takeover_job(Org, JobId, <<"worker-b">>, 2, 60);
                (deliver) ->
                    cs_ai_delivery_app:deliver(Org, Ws, #{
                        job_id => JobId,
                        worker_id => <<"worker-a">>,
                        seat_identity_id => maps:get(seat_identity_id, Scene),
                        reply => <<"race">>
                    })
            end,
        lists:foreach(
            fun(Kind) ->
                _ = spawn(fun() ->
                    Rounds = [Work(Kind) || _ <- lists:seq(1, 5)],
                    Parent ! {Kind, Rounds}
                end)
            end,
            [renew, takeover, deliver]
        ),
        All = [
            (receive
                {K, Rs} -> {K, Rs}
            end)
         || K <- [renew, takeover, deliver]
        ],
        Deadlocks = [
            R
         || {_K, Rounds} <- All,
            R <- Rounds,
            element(1, R) =:= error,
            is_tuple(element(2, R)),
            element(1, element(2, R)) =:= sql,
            element(2, element(2, R)) =:= <<"40P01">>
        ],
        ?assertEqual([], Deadlocks)
    after
        cleanup_scene(Scene)
    end.

%% ===================================================================
%% 附加：mark_job_skipped 的终态映射（02-B.4 返回形状 × 180 七态 CHECK）
%% ===================================================================

skipped_status_mapping() ->
    Scene = seed_ai_scene(),
    try
        #{org_id := Org, job_id := JobId} = Scene,
        ?assertEqual({ok, skipped, expired}, cs_pg_ai:mark_job_skipped(Org, JobId, expired)),
        {ok, #{status := dead}} = cs_pg_ai:fetch_job(Org, JobId),
        ?assertEqual(<<"expired">>, job_error_code(Org, JobId)),
        OtherScene = seed_ai_scene(),
        Other = maps:get(job_id, OtherScene),
        ?assertEqual(
            {ok, skipped, superseded},
            cs_pg_ai:mark_job_skipped(
                maps:get(org_id, OtherScene), Other, superseded
            )
        ),
        ?assertEqual(<<"superseded">>, job_error_code(maps:get(org_id, OtherScene), Other)),
        cleanup_scene(OtherScene)
    after
        cleanup_scene(Scene)
    end.

%% ===================================================================
%% 种子与断言辅助
%% ===================================================================

org(S) -> maps:get(org_id, S).
ws(S) -> maps:get(workspace_id, S).

%% AI 投递最小场景：随机租户 + active 会话绑定 AI 身份（sender_shape 的
%% business_identity 分支要求会话 active 且自报 identity 逐字一致）+
%% 一条 contact 入站消息 + 一个 pending job（context_watermark = 入站 id）。
seed_ai_scene() ->
    Scope = ?FIX:new_scope(),
    Scene = seed_ai_scene_in(Scope),
    Scene#{scope => Scope}.

seed_ai_scene_in(Scope) ->
    SessionId = ?FIX:id(),
    Identity = maps:get(service_identity_id, Scope),
    ok = ?FIX:exec(
        <<
            "INSERT INTO customer_service_session"
            " (id, organization_id, workspace_id, contact_id, conversation_id,"
            "  business_identity_id, status)"
            " VALUES ($1,$2,$3,$4,$5,$6,'active')"
        >>,
        [
            SessionId,
            org(#{org_id => maps:get(org_id, Scope)}),
            maps:get(workspace_id, Scope),
            maps:get(contact_id, Scope),
            maps:get(conversation_id, Scope),
            Identity
        ]
    ),
    Inbound = ?FIX:id(),
    ok = ?FIX:exec(
        <<
            "INSERT INTO enterprise_message"
            " (id, organization_id, workspace_id, conversation_id, sender_type,"
            "  sender_contact_id, client_msg_id, retention_days, retain_until,"
            "  visibility, created_at)"
            " VALUES ($1,$2,$3,$4,'contact',$5,$6,1095,to_timestamp(1900000000),"
            "         'visible',to_timestamp(1700000000))"
        >>,
        [
            Inbound,
            maps:get(org_id, Scope),
            maps:get(workspace_id, Scope),
            maps:get(conversation_id, Scope),
            maps:get(contact_id, Scope),
            <<"csb-", (integer_to_binary(Inbound))/binary>>
        ]
    ),
    JobId = ?FIX:id(),
    ok = ?FIX:exec(
        <<
            "INSERT INTO customer_service_ai_job"
            " (id, organization_id, workspace_id, session_id, conversation_id,"
            "  inbound_message_id, generation_epoch, context_watermark, status, deadline_at)"
            " VALUES ($1,$2,$3,$4,$5,$6,1,$7,'pending', now() + make_interval(secs => 300))"
        >>,
        [
            JobId,
            maps:get(org_id, Scope),
            maps:get(workspace_id, Scope),
            SessionId,
            maps:get(conversation_id, Scope),
            Inbound,
            Inbound
        ]
    ),
    #{
        scope => Scope,
        org_id => maps:get(org_id, Scope),
        workspace_id => maps:get(workspace_id, Scope),
        session_id => SessionId,
        conversation_id => maps:get(conversation_id, Scope),
        seat_identity_id => Identity,
        inbound_id => Inbound,
        job_id => JobId
    }.

seed_contact_message(Scene) ->
    Scope = maps:get(scope, Scene),
    MessageId = ?FIX:id(),
    ok = ?FIX:exec(
        <<
            "INSERT INTO enterprise_message"
            " (id, organization_id, workspace_id, conversation_id, sender_type,"
            "  sender_contact_id, client_msg_id, retention_days, retain_until,"
            "  visibility, created_at)"
            " VALUES ($1,$2,$3,$4,'contact',$5,$6,1095,to_timestamp(1900000000),"
            "         'visible',to_timestamp(1700000000))"
        >>,
        [
            MessageId,
            maps:get(org_id, Scope),
            maps:get(workspace_id, Scope),
            maps:get(conversation_id, Scope),
            maps:get(contact_id, Scope),
            <<"csb-", (integer_to_binary(MessageId))/binary>>
        ]
    ),
    MessageId.

expire_lease(Org, JobId) ->
    ok = ?FIX:exec(
        <<
            "UPDATE customer_service_ai_job SET lease_until = now() - interval '10s'"
            " WHERE organization_id=$1 AND id=$2"
        >>,
        [Org, JobId]
    ).

insert_message_tx(Conn, Scene, MessageId, contact) ->
    Scope = maps:get(scope, Scene),
    case
        elib_pg:execute(
            Conn,
            <<
                "INSERT INTO enterprise_message"
                " (id, organization_id, workspace_id, conversation_id, sender_type,"
                "  sender_contact_id, client_msg_id, retention_days, retain_until,"
                "  visibility, created_at)"
                " VALUES ($1,$2,$3,$4,'contact',$5,$6,1095,to_timestamp(1900000000),"
                "         'visible',to_timestamp(1700000000))"
            >>,
            [
                MessageId,
                maps:get(org_id, Scope),
                maps:get(workspace_id, Scope),
                maps:get(conversation_id, Scope),
                maps:get(contact_id, Scope),
                <<"csb-", (integer_to_binary(MessageId))/binary>>
            ]
        )
    of
        {ok, _} -> ok;
        {ok, _, _} -> ok;
        Other -> erlang:error({message_insert_failed, Other})
    end.

read_input_version(Org, ConvId) ->
    ?FIX:scalar(
        <<
            "SELECT COALESCE(max(m.id), 0) FROM enterprise_message m"
            " WHERE m.organization_id=$1 AND m.conversation_id=$2"
            "   AND m.sender_type='contact'"
        >>,
        [Org, ConvId]
    ).

delivery_count(Org) ->
    ?FIX:scalar(
        <<"SELECT count(*) FROM customer_service_ai_delivery WHERE organization_id=$1">>,
        [Org]
    ).

job_count(Org) ->
    ?FIX:scalar(
        <<"SELECT count(*) FROM customer_service_ai_job WHERE organization_id=$1">>,
        [Org]
    ).

job_error_code(Org, JobId) ->
    ?FIX:scalar(
        <<
            "SELECT last_error_code FROM customer_service_ai_job"
            " WHERE organization_id=$1 AND id=$2"
        >>,
        [Org, JobId]
    ).

%% 本场景 AI 侧对外消息（client_msg_id 前缀 ai-job- 的 canonical 真源行）。
ai_message_count(Scene) ->
    ?FIX:scalar(
        <<
            "SELECT count(*) FROM enterprise_message"
            " WHERE organization_id=$1 AND client_msg_id LIKE 'ai-job-%'"
        >>,
        [org(Scene)]
    ).

%% 套件清场：AI 表先删（FK RESTRICT 链），再 fixture 全清。
cleanup_scene(Scene) ->
    Scope = maps:get(scope, Scene),
    O = maps:get(org_id, Scope),
    lists:foreach(
        fun(Table) ->
            _ = ?FIX:exec(
                <<"DELETE FROM ", Table/binary, " WHERE organization_id=$1">>,
                [O]
            )
        end,
        [
            <<"customer_service_ai_delivery">>,
            <<"customer_service_ai_job">>,
            <<"customer_service_ai_consume_seen">>,
            <<"customer_service_ai_consume_cursor">>,
            <<"customer_service_ai_session">>
        ]
    ),
    ?FIX:cleanup(Scope).

sql(Conn, Statement) ->
    case epgsql:squery(Conn, Statement) of
        {ok, _, _} -> ok;
        {ok, _} -> ok;
        Ok when element(1, Ok) =:= ok -> ok;
        Error -> erlang:error({sql_failed, Statement, Error})
    end.

raw_conn() ->
    #{start_mfa := {epgsql, connect, [Params]}} = config_ds:env(pg_conf),
    case epgsql:connect(Params) of
        {ok, Conn} -> Conn;
        {error, Reason} -> erlang:error({raw_conn_failed, Reason})
    end.

close_conn(Conn) ->
    try
        epgsql:close(Conn)
    catch
        _:_ -> ok
    end,
    ok.
