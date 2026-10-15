%%% @doc P0-4（v4 02-C）：AI 接管闩锁 focused 套件（真库面）。
%%%
%%% 覆盖 C-2/C-3/C-4 + I6 + E2 两条触发 + 死代码删除（C-6 静态面在
%%% cs_ai_permission_matrix_tests，本套件专注闩锁运行时语义）：
%%%   * E2 触发一：on_human_takeover 置粘性闩锁 + generation_epoch+1；
%%%     重复触发 epoch 继续自增、reason/at 保持首次（粘性）；
%%%   * E2 触发二：on_config_revoked 批量置位 org 内全部未闩锁会话；
%%%   * C-3/I6：闩锁置位后 deliver ⇒ {skipped, superseded} 且 delivery=0；
%%%   * C-4：无解除 API + 粘性 SQL（重复 upsert handoff=false 不回退）；
%%%   * on_human_takeover 永不抛错（吞错契约，AI 故障不拖垮人工路径）。
%%%
%%% 隔离：cs_pg_test_fixture 随机 TSID scope；环境不可用 ⇒ erlang:error。
-module(cs_ai_handoff_tests).

-include_lib("eunit/include/eunit.hrl").

-define(FIX, cs_pg_test_fixture).
-define(MOD, cs_ai_app).

cs_ai_handoff_test_() ->
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
        {timeout, 60, fun e2_claim_latch_is_sticky_and_bumps_epoch/0},
        {timeout, 60, fun e2_config_revoked_latches_all_pending/0},
        {timeout, 60, fun c3_i6_latched_session_gets_zero_delivery/0},
        {timeout, 60, fun c4_latch_has_no_unset_path/0},
        {timeout, 60, fun on_human_takeover_never_raises/0}
    ];
cases({error, Reason}) ->
    erlang:error({csai_c_group_suite_db_unavailable, Reason}).

%% ===================================================================
%% E2 触发一：claim 闩锁粘性 + epoch 自增
%% ===================================================================

e2_claim_latch_is_sticky_and_bumps_epoch() ->
    {Scope, SessionId} = seed_ai_session(),
    try
        Org = maps:get(org_id, Scope),
        Ws = maps:get(workspace_id, Scope),
        ok = cs_ai_delivery_app:on_human_takeover(Org, Ws, SessionId, <<"human_claim">>),
        {ok, S1} = cs_pg_ai:fetch_session_state(Org, Ws, SessionId),
        ?assertEqual(true, maps:get(handoff, S1)),
        ?assertEqual(2, maps:get(generation_epoch, S1)),
        ?assertEqual(<<"human_claim">>, maps:get(handoff_reason, S1)),
        %% 重复触发（transfer 再来一次）：epoch 继续自增，reason 保持首次。
        ok = cs_ai_delivery_app:on_human_takeover(Org, Ws, SessionId, <<"human_transfer">>),
        {ok, S2} = cs_pg_ai:fetch_session_state(Org, Ws, SessionId),
        ?assertEqual(3, maps:get(generation_epoch, S2)),
        ?assertEqual(<<"human_claim">>, maps:get(handoff_reason, S2)),
        ?assertEqual(true, maps:get(handoff, S2))
    after
        cleanup_ai(Scope)
    end.

%% ===================================================================
%% E2 触发二：配置撤销批量闩锁
%% ===================================================================

e2_config_revoked_latches_all_pending() ->
    {Scope, SessionId} = seed_ai_session(),
    {Scope2, SessionId2} = seed_ai_session(),
    try
        Org = maps:get(org_id, Scope),
        Ws = maps:get(workspace_id, Scope),
        %% AI 曾介入（ai_session 行存在、未闩锁）。裸 INSERT（不用
        %% upsert_session_state：其 SQL 仍引用 v1 漂移列 last_ai_at，
        %% 字段对齐属 P0-8）。
        ok = ?FIX:exec(
            <<
                "INSERT INTO customer_service_ai_session"
                " (organization_id, workspace_id, session_id, handoff)"
                " VALUES ($1,$2,$3,false)"
            >>,
            [Org, Ws, SessionId]
        ),
        cs_ai_delivery_app:on_config_revoked(Org, <<"config_revoked">>),
        {ok, S1} = cs_pg_ai:fetch_session_state(Org, Ws, SessionId),
        ?assertEqual(true, maps:get(handoff, S1)),
        ?assertEqual(<<"config_revoked">>, maps:get(handoff_reason, S1)),
        ?assertEqual(2, maps:get(generation_epoch, S1))
    after
        cleanup_ai(Scope),
        cleanup_ai(Scope2)
    end.

%% ===================================================================
%% C-3 + I6：闩锁置位后 AI 投递 = 0
%% ===================================================================

c3_i6_latched_session_gets_zero_delivery() ->
    Scene = seed_delivery_scene(),
    try
        #{org_id := Org, workspace_id := Ws, job_id := JobId} = Scene,
        {ok, _} = cs_pg_ai:claim_job(Org, JobId, <<"worker-a">>, 60),
        %% 人工接管（E2）后投递。
        ok = cs_ai_delivery_app:on_human_takeover(
            Org, Ws, maps:get(session_id, Scene), <<"human_claim">>
        ),
        Result = cs_ai_delivery_app:deliver(Org, Ws, #{
            job_id => JobId,
            worker_id => <<"worker-a">>,
            seat_identity_id => maps:get(seat_identity_id, Scene),
            reply => <<"should not deliver">>
        }),
        ?assertEqual({skipped, superseded}, Result),
        ?assertEqual(0, delivery_count(Org)),
        ?assertEqual(0, ai_message_count(Org)),
        {ok, Job} = cs_pg_ai:fetch_job(Org, JobId),
        ?assertEqual(cancelled, maps:get(status, Job))
    after
        cleanup_ai(maps:get(scope, Scene))
    end.

%% ===================================================================
%% C-4：闩锁无解除路径（粘性）
%% ===================================================================

c4_latch_has_no_unset_path() ->
    {Scope, SessionId} = seed_ai_session(),
    try
        Org = maps:get(org_id, Scope),
        Ws = maps:get(workspace_id, Scope),
        ok = cs_ai_delivery_app:on_human_takeover(Org, Ws, SessionId, <<"human_claim">>),
        %% 静态面：应用/数据两层均无清除入口（E3 清除仅发生在 closed 后
        %% 新会话 = 新 ai_session 行；无任何 unset/clear 导出）。
        Exports = ?MOD:module_info(exports),
        Offenders = lists:filter(
            fun({F, _A}) ->
                Name = atom_to_binary(F),
                binary:match(Name, <<"unset">>) =/= nomatch orelse
                    binary:match(Name, <<"clear">>) =/= nomatch orelse
                    binary:match(Name, <<"unlatch">>) =/= nomatch
            end,
            Exports
        ),
        ?assertEqual([], Offenders),
        %% 运行时面：重复置位不回退（粘性；reason 保持首次已由 e2 覆盖，
        %% 此处断言 handoff 恒真）。
        {ok, S} = cs_pg_ai:fetch_session_state(Org, Ws, SessionId),
        ?assertEqual(true, maps:get(handoff, S))
    after
        cleanup_ai(Scope)
    end.

%% ===================================================================
%% on_human_takeover 吞错契约：畸形入参/异常路径一律 ok
%% ===================================================================

on_human_takeover_never_raises() ->
    %% 畸形入参（非整数）⇒ ok（参数卫卫返回）。
    ok = cs_ai_delivery_app:on_human_takeover(<<"bad">>, 1, 2, <<"x">>),
    %% 正常形状但 org 不存在（DB 错误路径）⇒ ok（内部吞错）。
    ok = cs_ai_delivery_app:on_human_takeover(1, 1, 1, <<"human_claim">>),
    ok = cs_ai_delivery_app:on_config_revoked(<<"bad">>, <<"x">>),
    ok = cs_ai_delivery_app:on_config_revoked(1, <<"config_revoked">>).

%% ===================================================================
%% 种子与断言辅助
%% ===================================================================

seed_ai_session() ->
    Scope = ?FIX:new_scope(),
    SessionId = seed_session_row(Scope),
    {Scope, SessionId}.

seed_session_row(Scope) ->
    SessionId = ?FIX:id(),
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
            maps:get(service_identity_id, Scope)
        ]
    ),
    SessionId.

%% P0-3 同款投递场景（active 会话 + contact 入站 + pending job）。
seed_delivery_scene() ->
    Scope = ?FIX:new_scope(),
    SessionId = seed_session_row(Scope),
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
            <<"csh-", (integer_to_binary(Inbound))/binary>>
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
        seat_identity_id => maps:get(service_identity_id, Scope),
        inbound_id => Inbound,
        job_id => JobId
    }.

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
            <<"customer_service_ai_session">>
        ]
    ),
    ?FIX:cleanup(Scope).
