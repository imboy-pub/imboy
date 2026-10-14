%%% @doc P0-5（v4 02-D）：D 组证据 gate focused 套件（真库）。
%%%
%%% 覆盖 D-1/D-2/D-3/D-5 + I9/I10（04-task-contracts P0-5）：
%%%   * D-1 三层字段独立存在（schema 静态断言，无合成列）；
%%%   * D-2/I9 无据不投递：faq_require_hit=true 且无引用 ⇒ {skipped,
%%%     no_grounding}，delivery=0；
%%%   * D-3/I10 引用过期不投递：FAQ 置 paused / 版本失配 ⇒ {skipped,
%%%     stale_citation}（job → cancelled，B.4 冻结语义）；
%%%   * D-5 自报置信度不参与放行：静态（gate 代码不含该字段）+ 运行时
%%%     （高置信度 + 无据 turn 仍不投递）；
%%%   * gate_1 mode_ok：disabled / shadow / canary 未覆盖 / canary 覆盖；
%%%   * gate_7 session_open：closed 会话不投递。
%%%
%%% 隔离：cs_pg_test_fixture 随机 TSID scope；环境不可用 ⇒ erlang:error。
-module(cs_ai_grounding_gate_tests).

-include_lib("eunit/include/eunit.hrl").

-define(FIX, cs_pg_test_fixture).

cs_ai_grounding_gate_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun cases/1}.

setup() ->
    case eunit_runner:eunit_setup_with_db() of
        {ok, Conn} -> {ok, Conn};
        {error, Reason} -> {error, Reason}
    end.

cleanup({ok, Conn}) ->
    eunit_runner:eunit_cleanup_db(Conn);
cleanup(Other) ->
    Other.

cases({ok, _Conn}) ->
    [
        {timeout, 60, fun d1_three_layers_independent/0},
        {timeout, 60, fun d2_i9_no_citation_no_delivery/0},
        {timeout, 60, fun d3_i10_stale_citation_no_delivery/0},
        {timeout, 60, fun d5_confidence_not_a_gate/0},
        {timeout, 60, fun gate1_mode_and_canary/0},
        {timeout, 60, fun gate7_session_open/0}
    ];
cases({error, Reason}) ->
    erlang:error({csai_d_group_suite_db_unavailable, Reason}).

%% ===================================================================
%% D-1：三层字段独立存在且无合成列
%% ===================================================================

d1_three_layers_independent() ->
    %% L-Grounding / L-Answerability / L-Confidence 三层物理分离（02-D.2）。
    ok = ?FIX:exec(
        <<
            "SELECT 1 FROM information_schema.columns"
            " WHERE table_name='customer_service_ai_turn'"
            "   AND column_name IN ('cited_faq_ids','cited_faq_versions','answerable',"
            "                       'model_reported_confidence') HAVING count(*)=4"
        >>,
        []
    ),
    ok = ?FIX:exec(
        <<
            "SELECT 1 FROM information_schema.columns"
            " WHERE table_name='customer_service_ai_delivery'"
            "   AND column_name='grounding' HAVING count(*)=1"
        >>,
        []
    ),
    ok.

%% ===================================================================
%% D-2 + I9：无据不投递
%% ===================================================================

d2_i9_no_citation_no_delivery() ->
    Scene = seed_scene(),
    try
        #{org_id := Org, workspace_id := Ws, job_id := JobId} = Scene,
        seed_config(Org, #{auto_reply_mode => enabled, faq_require_hit => true}),
        {ok, _} = cs_pg_ai:claim_job(Org, JobId, <<"worker-a">>, 60),
        %% 无引用 + answerable（模型说能答）⇒ gate_3 拒。
        Gate = cs_ai_app:grounding_gate(Org, #{
            cited_faq_ids => [], cited_faq_versions => [], answerable => true
        }),
        Result =
            cs_ai_app:deliver(Org, Ws, #{
                job_id => JobId,
                worker_id => <<"worker-a">>,
                seat_identity_id => maps:get(seat_identity_id, Scene),
                reply => <<"guess">>,
                gate_fun => Gate
            }),
        ?assertEqual({skipped, no_grounding}, Result),
        ?assertEqual(0, delivery_count(Org)),
        ?assertEqual(0, ai_message_count(Org))
    after
        cleanup_ai(maps:get(scope, Scene))
    end.

%% ===================================================================
%% D-3 + I10：引用过期/停更不投递
%% ===================================================================

d3_i10_stale_citation_no_delivery() ->
    Scene = seed_scene(),
    try
        #{org_id := Org, workspace_id := Ws, job_id := JobId} = Scene,
        seed_config(Org, #{auto_reply_mode => enabled, faq_require_hit => true}),
        {FaqId, Version} = seed_faq(Org, maps:get(workspace_id, Scene)),
        {ok, _} = cs_pg_ai:claim_job(Org, JobId, <<"worker-a">>, 60),
        %% 引用有效 ⇒ 通过 gate（delivery=1）。
        Gate = cs_ai_app:grounding_gate(Org, #{
            cited_faq_ids => [FaqId],
            cited_faq_versions => [Version],
            answerable => true
        }),
        Result =
            cs_ai_app:deliver(Org, Ws, #{
                job_id => JobId,
                worker_id => <<"worker-a">>,
                seat_identity_id => maps:get(seat_identity_id, Scene),
                reply => <<"grounded">>,
                gate_fun => Gate
            }),
        ?assertEqual({ok, delivered}, Result),
        ?assertEqual(1, delivery_count(Org)),
        %% D-3 主张：投递前把 FAQ 置 paused ⇒ 新 job 复核 superseded。
        JobId2 = seed_second_job(Scene),
        {ok, _} = cs_pg_ai:claim_job(Org, JobId2, <<"worker-b">>, 60),
        ok = ?FIX:exec(
            <<
                "UPDATE customer_service_faq SET status='paused'"
                " WHERE organization_id=$1 AND id=$2"
            >>,
            [Org, FaqId]
        ),
        Result2 =
            cs_ai_app:deliver(Org, Ws, #{
                job_id => JobId2,
                worker_id => <<"worker-b">>,
                seat_identity_id => maps:get(seat_identity_id, Scene),
                reply => <<"stale">>,
                gate_fun =>
                    cs_ai_app:grounding_gate(Org, #{
                        cited_faq_ids => [FaqId],
                        cited_faq_versions => [Version],
                        answerable => true
                    })
            }),
        ?assertEqual({skipped, stale_citation}, Result2),
        ?assertEqual(1, delivery_count(Org))
    after
        cleanup_ai(maps:get(scope, Scene))
    end.

%% ===================================================================
%% D-5：自报置信度不参与放行
%% ===================================================================

d5_confidence_not_a_gate() ->
    Scene = seed_scene(),
    try
        #{org_id := Org, workspace_id := Ws, job_id := JobId} = Scene,
        seed_config(Org, #{auto_reply_mode => enabled, faq_require_hit => true}),
        {ok, _} = cs_pg_ai:claim_job(Org, JobId, <<"worker-a">>, 60),
        %% 高自报置信度 + 无据（answerable=false）⇒ 仍不投递。
        Gate = cs_ai_app:grounding_gate(Org, #{
            cited_faq_ids => [],
            cited_faq_versions => [],
            answerable => false,
            model_reported_confidence => 0.99
        }),
        Result =
            cs_ai_app:deliver(Org, Ws, #{
                job_id => JobId,
                worker_id => <<"worker-a">>,
                seat_identity_id => maps:get(seat_identity_id, Scene),
                reply => <<"confident guess">>,
                gate_fun => Gate
            }),
        ?assertEqual({skipped, no_grounding}, Result),
        ?assertEqual(0, delivery_count(Org)),
        %% 静态面：gate 代码路径不含 model_reported_confidence。
        {ok, Bin} = file:read_file(src_path("cs_ai_app.erl")),
        GateSeg = gate_segment(Bin),
        ?assert(binary:match(GateSeg, <<"model_reported_confidence">>) =:= nomatch)
    after
        cleanup_ai(maps:get(scope, Scene))
    end.

%% ===================================================================
%% gate_1：mode_ok（disabled / shadow / canary）
%% ===================================================================

gate1_mode_and_canary() ->
    Scene = seed_scene(),
    try
        #{org_id := Org, workspace_id := Ws, job_id := JobId} = Scene,
        FaqId = seed_faq_simple(Org, Ws),
        {ok, _} = cs_pg_ai:claim_job(Org, JobId, <<"worker-a">>, 60),
        Evidence = #{
            cited_faq_ids => [FaqId], cited_faq_versions => [1], answerable => true
        },
        %% disabled ⇒ 拒（B.4：每次跳过都落 job 终态——后续场景用独立 job）。
        seed_config(Org, #{auto_reply_mode => disabled, faq_require_hit => false}),
        {skipped, superseded} =
            deliver_with(Org, Ws, Scene, JobId, Evidence),
        %% shadow ⇒ 拒（完整跑但不投递）。
        seed_config(Org, #{auto_reply_mode => shadow, faq_require_hit => false}),
        Job2 = seed_second_job(Scene),
        {ok, _} = cs_pg_ai:claim_job(Org, Job2, <<"worker-a">>, 60),
        {skipped, superseded} =
            deliver_with(Org, Ws, Scene, Job2, Evidence),
        %% canary 无白名单（空集）⇒ 拒。
        seed_config(Org, #{auto_reply_mode => canary, faq_require_hit => false}),
        Job3 = seed_second_job(Scene),
        {ok, _} = cs_pg_ai:claim_job(Org, Job3, <<"worker-a">>, 60),
        {skipped, superseded} =
            deliver_with(Org, Ws, Scene, Job3, Evidence),
        %% canary + workspace 白名单 ⇒ 过。
        ok = ?FIX:exec(
            <<
                "INSERT INTO customer_service_ai_canary"
                " (organization_id, scope_kind, scope_id) VALUES ($1,'workspace',$2)"
            >>,
            [Org, Ws]
        ),
        Job4 = seed_second_job(Scene),
        {ok, _} = cs_pg_ai:claim_job(Org, Job4, <<"worker-a">>, 60),
        {ok, delivered} = deliver_with(Org, Ws, Scene, Job4, Evidence)
    after
        cleanup_ai(maps:get(scope, Scene))
    end.

%% ===================================================================
%% gate_7：session_open
%% ===================================================================

gate7_session_open() ->
    Scene = seed_scene(),
    try
        #{org_id := Org, workspace_id := Ws, job_id := JobId} = Scene,
        FaqId = seed_faq_simple(Org, Ws),
        seed_config(Org, #{auto_reply_mode => enabled, faq_require_hit => false}),
        {ok, _} = cs_pg_ai:claim_job(Org, JobId, <<"worker-a">>, 60),
        %% 会话 closed ⇒ 不投递。
        ok = ?FIX:exec(
            <<
                "UPDATE customer_service_session SET status='closed', closed_at=now()"
                " WHERE organization_id=$1 AND id=$2"
            >>,
            [Org, maps:get(session_id, Scene)]
        ),
        {skipped, superseded} =
            deliver_with(Org, Ws, Scene, JobId, #{
                cited_faq_ids => [FaqId], cited_faq_versions => [1], answerable => true
            }),
        ?assertEqual(0, delivery_count(Org))
    after
        cleanup_ai(maps:get(scope, Scene))
    end.

%% ===================================================================
%% 辅助
%% ===================================================================

deliver_with(Org, Ws, Scene, JobId, Evidence) ->
    cs_ai_app:deliver(Org, Ws, #{
        job_id => JobId,
        worker_id => <<"worker-a">>,
        seat_identity_id => maps:get(seat_identity_id, Scene),
        reply => <<"x">>,
        gate_fun => cs_ai_app:grounding_gate(Org, Evidence)
    }).

seed_scene() ->
    Scope = ?FIX:new_scope(),
    Scene = seed_scene_in(Scope),
    Scene#{scope => Scope}.

seed_scene_in(Scope) ->
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
            maps:get(org_id, Scope),
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
            <<"csd-", (integer_to_binary(Inbound))/binary>>
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
        org_id => maps:get(org_id, Scope),
        workspace_id => maps:get(workspace_id, Scope),
        session_id => SessionId,
        conversation_id => maps:get(conversation_id, Scope),
        seat_identity_id => Identity,
        job_id => JobId
    }.

seed_second_job(Scene) ->
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
            maps:get(org_id, Scene),
            maps:get(workspace_id, Scene),
            maps:get(session_id, Scene),
            maps:get(conversation_id, Scene),
            ?FIX:id(),
            ?FIX:id()
        ]
    ),
    JobId.

%% v4 配置行（v4 列名；P0-8 前的测试侧先对齐）。
%% 一 org 一配置（生产 config_set 同契约）：复用既有行 id，防止多行后
%% fetch_gate_config 的 ORDER BY id LIMIT 1 读到陈旧 mode。
seed_config(Org, Opts) ->
    Mode = maps:get(auto_reply_mode, Opts, disabled),
    RequireHit = maps:get(faq_require_hit, Opts, true),
    Id = config_id(Org),
    ok = ?FIX:exec(
        <<
            "INSERT INTO customer_service_ai_agent"
            " (organization_id, id, auto_reply_mode, faq_require_hit)"
            " VALUES ($1,$2,$3,$4)"
            " ON CONFLICT (organization_id, id) DO UPDATE SET"
            "  auto_reply_mode = EXCLUDED.auto_reply_mode,"
            "  faq_require_hit = EXCLUDED.faq_require_hit"
        >>,
        [Org, Id, atom_to_binary(Mode), RequireHit]
    ),
    ok.

config_id(Org) ->
    case
        ?FIX:scalar(
            <<"SELECT id FROM customer_service_ai_agent WHERE organization_id=$1 LIMIT 1">>,
            [Org]
        )
    of
        undefined -> ?FIX:id();
        Existing -> Existing
    end.

%% 已发布 FAQ（返回 {id, version}）。
seed_faq(Org, Ws) ->
    Id = ?FIX:id(),
    ok = ?FIX:exec(
        <<
            "INSERT INTO customer_service_faq"
            " (organization_id, id, question, answer, status, scope_workspace_ids)"
            " VALUES ($1,$2,'退款政策是什么','7 天无理由退款','published',ARRAY[$3::bigint])"/utf8
        >>,
        [Org, Id, Ws]
    ),
    {Id, 1}.

seed_faq_simple(Org, _Ws) ->
    element(1, seed_faq(Org, _Ws)).

delivery_count(Org) ->
    ?FIX:scalar(
        <<"SELECT count(*) FROM customer_service_ai_delivery WHERE organization_id=$1">>,
        [Org]
    ).

ai_message_count(Org) ->
    ?FIX:scalar(
        <<
            "SELECT count(*) FROM enterprise_message"
            " WHERE organization_id=$1 AND client_msg_id LIKE 'ai-job-%'"
        >>,
        [Org]
    ).

%% 从模块源码截取 gate 代码段（%% D 组 标题到下一节标题）。
gate_segment(Bin) ->
    Start = binary:match(Bin, <<"D 组（v4 02-D）：七道放行 gate"/utf8>>),
    End = binary:match(Bin, <<"历史（多轮上下文）"/utf8>>),
    case {Start, End} of
        {{S, _}, {E, _}} when S < E -> binary:part(Bin, S, E - S);
        _ -> Bin
    end.

src_path(Base) ->
    filename:join(
        [code:lib_dir(imboy), <<"src/features/customer_service/application/ai">>, Base]
    ).

cleanup_ai(Scope) ->
    O = maps:get(org_id, Scope),
    lists:foreach(
        fun(Table) ->
            _ = ?FIX:exec(
                <<"DELETE FROM ", Table/binary, " WHERE organization_id=$1">>, [O]
            )
        end,
        [
            <<"customer_service_ai_delivery">>,
            <<"customer_service_ai_job">>,
            <<"customer_service_ai_consume_seen">>,
            <<"customer_service_ai_consume_cursor">>,
            <<"customer_service_ai_session">>,
            <<"customer_service_faq">>,
            <<"customer_service_ai_canary">>,
            <<"customer_service_ai_agent">>
        ]
    ),
    ?FIX:cleanup(Scope).
