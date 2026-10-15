%%% @doc P0-2（v4 02-A）：A 组事件消费三件套 focused 套件。
%%%
%%% 覆盖 A-1…A-6 + I1/I2（见 04-task-contracts P0-2）：
%%%   * A-1 迟提交不丢：复现探针 A1 交错（低 xid 事务晚于高 id 提交），
%%%     断言 xmin-horizon 闸门下迟提交事件仍被消费；
%%%   * A-2 闸门不跳过：交错窗口内 horizon 停在活跃最小 xid（不越过
%%%     飞行中事务），两事件提交后齐全（eligible=2 语义）；
%%%   * A-3 重复扫描幂等：同批重跑，副作用（seen / job）不增；
%%%   * A-4 horizon 与 rows 同快照：冻结谓词单语句（gate CTE）静态断言
%%%     + 交错行为佐证（时序精确注入在 eunit 内不确定，见各用例注释）；
%%%   * A-5 sweep 修复人为漏行：手工删一条 seen，sweep 补回且 job 不重复；
%%%   * A-6 窗口不匹配 fast-fail：BACKSCAN_WINDOW > RETENTION ⇒ 拒启。
%%%
%%% 副作用载体：`cs_ai_consume_app:apply_event/5`（visitor `message.appended` →
%%% 幂等建 `customer_service_ai_job`，DB 幂等键 `uq_csaij_org_inbound`）。
%%%
%%% 隔离：`cs_pg_test_fixture:new_scope/0` 随机 TSID scope；交错用例持
%%% 两条原生 epgsql 连接（同库参数取自 `config_ds:env(pg_conf)`）；
%%% 环境不可用 ⇒ `erlang:error/1`（不是 skip）。
-module(cs_pg_ai_consume_tests).

-include_lib("eunit/include/eunit.hrl").

-define(FIX, cs_pg_test_fixture).
-define(CONSUMER, <<"ai_job_derive">>).

cs_pg_ai_consume_test_() ->
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
        {timeout, 60, fun a1_a2_late_commit_not_skipped/0},
        {timeout, 60, fun a3_rescan_is_idempotent/0},
        {timeout, 60, fun a4_gate_predicate_single_statement/0},
        {timeout, 60, fun a5_sweep_repairs_manual_seen_hole/0},
        {timeout, 60, fun a6_window_mismatch_fails_fast/0},
        {timeout, 60, fun i1_i2_full_coverage_and_unique_effects/0},
        {timeout, 60, fun apply_event_filters_non_visitor_and_bad_shapes/0},
        {timeout, 60, fun apply_failure_rolls_back_whole_batch/0}
    ];
cases({error, Reason}) ->
    erlang:error({csai_a_group_suite_db_unavailable, Reason}).

%% ===================================================================
%% A-1 + A-2：迟提交交错（探针 A1 时序）
%% ===================================================================

a1_a2_late_commit_not_skipped() ->
    Scope = ?FIX:new_scope(),
    C1 = raw_conn(),
    C2 = raw_conn(),
    try
        Seed = seed_session(Scope),
        %% 低 id 事件（E1）在 C1 持有未提交；高 id 事件（E2）在 C2 先提交。
        E1 = ev_id(),
        E2 = ev_id(),
        M1 = msg_id(),
        M2 = msg_id(),
        seed_message(Scope, Seed, M1, contact),
        seed_message(Scope, Seed, M2, contact),
        ok = sql(C1, <<"BEGIN">>),
        insert_event_tx(C1, Scope, Seed, E1, M1, <<"visitor">>),
        insert_event_tx(C2, Scope, Seed, E2, M2, <<"visitor">>),
        %% 第一轮：闸门停在飞行事务的 xid（pg_snapshot_xmin = 快照最小
        %% 活跃 xid）。E1 未提交不可见；E2 虽已提交但其 xid 更大，
        %% `xmin < horizon` 同样不越过——两行都等（A-2「不越过飞行事务」；
        %% 这是闸门不漏的代价：宁可延迟，不可越过后永久跳过）。
        {ok, S1} = cs_ai_consume_app:consume_batch(org(Scope), ws(Scope), #{}),
        ?assertEqual(0, maps:get(applied, S1)),
        ?assertEqual(0, maps:get(seen, S1)),
        ?assertEqual(false, maps:get(regressed, S1)),
        %% 迟提交（探针 A1 时序）：E1 的 xid 小于 E2，id 单调游标在
        %% 「先消费 E2 再提交 E1」下会永久跳过 E1；xmin 闸门下水位从未
        %% 越过 E1 的 xid ⇒ E1 提交后必被选中。
        ok = sql(C1, <<"COMMIT">>),
        {ok, S2} = cs_ai_consume_app:consume_batch(org(Scope), ws(Scope), #{}),
        ?assertEqual(2, maps:get(applied, S2)),
        %% 两事件齐全（eligible=2 语义，A-2）：seen 全覆盖 + 各恰一条 job。
        ?assertEqual(
            2,
            ?FIX:scalar(
                <<
                    "SELECT count(*) FROM customer_service_ai_consume_seen"
                    " WHERE organization_id=$1 AND workspace_id=$2 AND consumer=$3"
                >>,
                [org(Scope), ws(Scope), ?CONSUMER]
            )
        ),
        ?assertEqual(2, job_count(Scope))
    after
        close_conn(C1),
        close_conn(C2),
        cleanup_scope(Scope)
    end.

%% ===================================================================
%% A-3：重复扫描幂等
%% ===================================================================

a3_rescan_is_idempotent() ->
    Scope = ?FIX:new_scope(),
    try
        Seed = seed_session(Scope),
        [_Ev] = seed_visitor_event(Scope, Seed),
        {ok, S1} = cs_ai_consume_app:consume_batch(org(Scope), ws(Scope), #{}),
        ?assertEqual(1, maps:get(applied, S1)),
        {ok, S2} = cs_ai_consume_app:consume_batch(org(Scope), ws(Scope), #{}),
        {ok, S3} = cs_ai_consume_app:consume_batch(org(Scope), ws(Scope), #{}),
        ?assertEqual(0, maps:get(applied, S2)),
        ?assertEqual(0, maps:get(applied, S3)),
        %% 副作用不随重复扫描增长（seen=1、job=1）。
        ?assertEqual(1, seen_count(Scope)),
        ?assertEqual(1, job_count(Scope))
    after
        cleanup_scope(Scope)
    end.

%% ===================================================================
%% A-4：冻结谓词单语句（horizon 与 rows 同快照的静态面）
%% ===================================================================

a4_gate_predicate_single_statement() ->
    [Pick, Upsert, SeenInsert] = cs_pg_ai:sql_statements(),
    %% gate CTE 与 picked CTE 在同一语句（同快照的前提），且 horizon
    %% 出现在最终 SELECT（rows 为空时 LEFT JOIN ON true 仍能输出 horizon）。
    ?assert(
        binary:match(Pick, <<"WITH gate AS (SELECT pg_snapshot_xmin(pg_current_snapshot())">>) =/=
            nomatch
    ),
    ?assert(binary:match(Pick, <<"AND e.xmin::text::bigint >= $3">>) =/= nomatch),
    %% gate.horizon 是 xid8（pg_snapshot_xmin 返回类型），比较前显式
    %% ::text::bigint（bigint < xid8 无操作符；与 02-A.2.3 语义一致的落地 cast）。
    ?assert(
        binary:match(Pick, <<"AND e.xmin::text::bigint <  gate.horizon::text::bigint">>) =/=
            nomatch
    ),
    ?assert(binary:match(Pick, <<"FROM gate LEFT JOIN picked ON true">>) =/= nomatch),
    %% 非 vacuum 断言（对照）：id 单调游标形状必须不存在。
    ?assert(binary:match(Pick, <<"e.id > $3">>) =:= nomatch),
    %% cursor 推进不改写 horizon（倒退防护，A.4）与 seen 幂等键。
    ?assert(
        binary:match(Upsert, <<"GREATEST(customer_service_ai_consume_cursor.last_xmin_horizon">>) =/=
            nomatch
    ),
    ?assert(binary:match(SeenInsert, <<"ON CONFLICT DO NOTHING">>) =/= nomatch).

%% ===================================================================
%% A-5：sweep 修复人为 seen 漏行
%% ===================================================================

a5_sweep_repairs_manual_seen_hole() ->
    Scope = ?FIX:new_scope(),
    try
        Seed = seed_session(Scope),
        [_Ev] = seed_visitor_event(Scope, Seed),
        {ok, _} = cs_ai_consume_app:consume_batch(org(Scope), ws(Scope), #{}),
        ?assertEqual(1, seen_count(Scope)),
        %% 人为挖洞：seen 表无 append-only 守卫，直接 DELETE。
        ok = ?FIX:exec(
            <<
                "DELETE FROM customer_service_ai_consume_seen"
                " WHERE organization_id=$1 AND workspace_id=$2 AND consumer=$3"
            >>,
            [org(Scope), ws(Scope), ?CONSUMER]
        ),
        ?assertEqual(0, seen_count(Scope)),
        {ok, [#{result := {ok, Sweep}}]} =
            cs_ai_consume_app:reconcile_sweep(#{consumer => ?CONSUMER}),
        ?assertEqual(1, maps:get(missing, Sweep)),
        ?assertEqual(1, maps:get(repaired, Sweep)),
        ?assertEqual(0, maps:get(remaining, Sweep)),
        ?assertEqual(1, seen_count(Scope)),
        %% job 不因 sweep 重复（uq_csaij_org_inbound 幂等）。
        ?assertEqual(1, job_count(Scope))
    after
        cleanup_scope(Scope)
    end.

%% ===================================================================
%% A-6：窗口不匹配 fast-fail
%% ===================================================================

a6_window_mismatch_fails_fast() ->
    ok = cs_ai_consume_app:validate_consume_config(),
    try
        application:set_env(imboy, cs_ai_backscan_window_seconds, 604801),
        ?assertError({consume_window_mismatch, _}, cs_ai_consume_app:validate_consume_config())
    after
        application:unset_env(imboy, cs_ai_backscan_window_seconds)
    end,
    %% 恢复后放行。
    ok = cs_ai_consume_app:validate_consume_config().

%% ===================================================================
%% I1 + I2：全事件覆盖、副作用唯一（含非消息事件与坐席消息）
%% ===================================================================

i1_i2_full_coverage_and_unique_effects() ->
    Scope = ?FIX:new_scope(),
    try
        Seed = seed_session(Scope),
        %% 3 条 visitor 消息事件 + 1 条坐席消息事件 + 1 条非消息事件。
        seed_visitor_events(Scope, Seed, 3),
        seed_seat_message_event(Scope, Seed),
        seed_opened_event(Scope, Seed),
        {ok, S1} = cs_ai_consume_app:consume_batch(org(Scope), ws(Scope), #{}),
        %% 全部 5 条都被消费（I1：seen 是消费证明），其中 3 条产生 job。
        ?assertEqual(5, maps:get(applied, S1)),
        ?assertEqual(5, seen_count(Scope)),
        ?assertEqual(3, job_count(Scope)),
        %% I2：重扫不重。
        {ok, _} = cs_ai_consume_app:consume_batch(org(Scope), ws(Scope), #{}),
        ?assertEqual(3, job_count(Scope))
    after
        cleanup_scope(Scope)
    end.

%% ===================================================================
%% apply_event 过滤面：非 visitor / 无 message_id 的畸形事件
%% ===================================================================

apply_event_filters_non_visitor_and_bad_shapes() ->
    Scope = ?FIX:new_scope(),
    try
        Seed = seed_session(Scope),
        %% visitor 事件但 detail 缺 message_id（payload_limits 合同外）：
        %% 跳过建 job（ok），seen 仍记录。
        Ev = ev_id(),
        insert_event(?FIX, Scope, Seed, Ev, <<"visitor">>, <<"message.appended">>, #{}),
        {ok, S1} = cs_ai_consume_app:consume_batch(org(Scope), ws(Scope), #{}),
        ?assertEqual(1, maps:get(applied, S1)),
        ?assertEqual(1, seen_count(Scope)),
        ?assertEqual(0, job_count(Scope))
    after
        cleanup_scope(Scope)
    end.

%% ===================================================================
%% A.4 失败路径：apply_event 失败 ⇒ 整批回滚（seen 与 cursor 一并回滚）
%% ===================================================================

apply_failure_rolls_back_whole_batch() ->
    Scope = ?FIX:new_scope(),
    try
        Seed = seed_session(Scope),
        [_Ev] = seed_visitor_event(Scope, Seed),
        %% 注入恒败 apply：模拟业务副作用故障。
        Bad = fun(_Conn, _Event) -> {error, injected_failure} end,
        Result = cs_pg_ai:consume_batch(org(Scope), ws(Scope), ?CONSUMER, Bad, #{}),
        %% elib_pg:with_tx(reraise=false) 把 throw 收敛为 {rollback, Reason}；
        %% 「整批回滚」的实质由下方 seen/job/cursor 三断言验证。
        ?assertMatch({rollback, {apply_event_failed, injected_failure}}, Result),
        %% 整批回滚：seen / cursor / job 全部无痕（A.4）。
        ?assertEqual(0, seen_count(Scope)),
        ?assertEqual(0, job_count(Scope)),
        {error, not_found} = cs_pg_ai:fetch_consume_cursor(org(Scope), ws(Scope), ?CONSUMER)
    after
        cleanup_scope(Scope)
    end.

%% ===================================================================
%% 种子与断言辅助（scope 内随机 TSID，无真实数据）
%% ===================================================================

org(Scope) -> maps:get(org_id, Scope).
ws(Scope) -> maps:get(workspace_id, Scope).

%% 套件级清场：180 新表（fixture 清单外）先删——ai_session 对
%% customer_service_session 是 RESTRICT FK，须在 fixture 删 session 之前。
cleanup_scope(Scope) ->
    lists:foreach(
        fun(Table) ->
            _ = ?FIX:exec(
                <<"DELETE FROM ", Table/binary, " WHERE organization_id=$1">>,
                [org(Scope)]
            )
        end,
        [
            <<"customer_service_ai_job">>,
            <<"customer_service_ai_consume_seen">>,
            <<"customer_service_ai_consume_cursor">>,
            <<"customer_service_ai_session">>
        ]
    ),
    ?FIX:cleanup(Scope).

ev_id() -> ?FIX:id().
msg_id() -> ?FIX:id().

%% 会话行（fixture 未覆盖 customer_service_session，本套件自建；active 态）。
seed_session(Scope) ->
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
            org(Scope),
            ws(Scope),
            maps:get(contact_id, Scope),
            maps:get(conversation_id, Scope),
            maps:get(service_identity_id, Scope)
        ]
    ),
    #{session_id => SessionId}.

seed_message(Scope, _Seed, MessageId, SenderType) ->
    {Contact, Identity, Actor} =
        case SenderType of
            contact ->
                {maps:get(contact_id, Scope), null, null};
            business_identity ->
                {null, maps:get(service_identity_id, Scope), maps:get(actor_user_id, Scope)}
        end,
    ok = ?FIX:exec(
        <<
            "INSERT INTO enterprise_message"
            " (id, organization_id, workspace_id, conversation_id, sender_type,"
            "  sender_contact_id, sender_business_identity_id, actor_user_id,"
            "  client_msg_id, retention_days, retain_until, visibility, created_at)"
            " VALUES ($1,$2,$3,$4,$5,$6,$7,$8,$9,1095,to_timestamp(1900000000),'visible',"
            "         to_timestamp(1700000000))"
        >>,
        [
            MessageId,
            org(Scope),
            ws(Scope),
            maps:get(conversation_id, Scope),
            atom_to_binary(SenderType),
            Contact,
            Identity,
            Actor,
            <<"csai-", (integer_to_binary(MessageId))/binary>>
        ]
    ),
    MessageId.

%% visitor 消息事件（含对应 contact 消息行）。
seed_visitor_event(Scope, Seed) ->
    seed_visitor_events(Scope, Seed, 1).

seed_visitor_events(Scope, Seed, N) ->
    [
        begin
            MessageId = msg_id(),
            _ = seed_message(Scope, Seed, MessageId, contact),
            Ev = ev_id(),
            insert_event(?FIX, Scope, Seed, Ev, <<"visitor">>, <<"message.appended">>, #{
                <<"message_id">> => MessageId
            }),
            Ev
        end
     || _ <- lists:seq(1, N)
    ].

seed_seat_message_event(Scope, Seed) ->
    MessageId = seed_message(Scope, Seed, msg_id(), business_identity),
    insert_event(?FIX, Scope, Seed, ev_id(), <<"seat">>, <<"message.appended">>, #{
        <<"message_id">> => MessageId
    }).

seed_opened_event(Scope, Seed) ->
    insert_event(?FIX, Scope, Seed, ev_id(), <<"visitor">>, <<"session.opened">>, #{}).

insert_event(Mod, Scope, Seed, EvId, ActorKind, Action, Detail) ->
    ok = Mod:exec(
        <<
            "INSERT INTO customer_service_event"
            " (id, organization_id, workspace_id, session_id, business_identity_id,"
            "  actor_kind, action, detail)"
            " VALUES ($1,$2,$3,$4,$5,$6,$7,$8::jsonb)"
        >>,
        [
            EvId,
            org(Scope),
            ws(Scope),
            maps:get(session_id, Seed),
            maps:get(service_identity_id, Scope),
            ActorKind,
            Action,
            jsone:encode(Detail)
        ]
    ),
    EvId.

%% 交错用例：在指定原生连接的事务内插事件（不提交；C2 无 BEGIN 时单语句
%% 自动提交）。RETURNING 形状容错（{ok,N} / {ok,N,Rows} 两种实测形态）。
insert_event_tx(Conn, Scope, Seed, EvId, MessageId, ActorKind) ->
    case
        elib_pg:execute(
            Conn,
            <<
                "INSERT INTO customer_service_event"
                " (id, organization_id, workspace_id, session_id, business_identity_id,"
                "  actor_kind, action, detail)"
                " VALUES ($1,$2,$3,$4,$5,$6,'message.appended',$7::jsonb)"
            >>,
            [
                EvId,
                org(Scope),
                ws(Scope),
                maps:get(session_id, Seed),
                maps:get(service_identity_id, Scope),
                ActorKind,
                jsone:encode(#{<<"message_id">> => MessageId})
            ]
        )
    of
        {ok, N} when N > 0 -> ok;
        {ok, N, _} when N > 0 -> ok;
        {ok, N, _, _} when N > 0 -> ok;
        Other -> erlang:error({event_insert_failed, Other})
    end.

%% C2（自动提交连接）由 epgsql 默认行为单语句提交，无死代码分支。

sql(Conn, Statement) ->
    case epgsql:squery(Conn, Statement) of
        {ok, _, _} -> ok;
        {ok, _} -> ok;
        Ok when element(1, Ok) =:= ok -> ok;
        Error -> erlang:error({sql_failed, Statement, Error})
    end.

seen_count(Scope) ->
    ?FIX:scalar(
        <<
            "SELECT count(*) FROM customer_service_ai_consume_seen"
            " WHERE organization_id=$1 AND workspace_id=$2 AND consumer=$3"
        >>,
        [org(Scope), ws(Scope), ?CONSUMER]
    ).

job_count(Scope) ->
    ?FIX:scalar(
        <<"SELECT count(*) FROM customer_service_ai_job WHERE organization_id=$1">>,
        [org(Scope)]
    ).

%% 原生连接（交错时序控制）：参数取自 pg_conf（同库同凭据）。
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
