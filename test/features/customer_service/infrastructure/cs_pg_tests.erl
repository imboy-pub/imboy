%%% @doc CS-01 真库套件：A01/A02 的 DB 级约束、真并发 CAS，以及
%%% transfer/close/rating/visit token/rebind 的落库语义。
%%%
%%% 依计划 CS-01-A01..A05 与 §4.3：
%%%   * A01：seat 引用 sales identity 在**数据库层**被复合 FK 拒绝（23503 /
%%%     fk_css_identity_function），绕过应用也进不来；
%%%   * A02：spawn N 进程并发 claim 同一会话 ⇒ **恰好一个** {ok, _}，其余
%%%     conflict；seat 行锁内复核 max_concurrent ⇒ active 不超上限；
%%%     enabled=false 即时拒绝；事件同事务恰一条；
%%%   * A03：客服消息全栈只写 enterprise_message（真源），库里**不存在**任何
%%%     customer_service_message 类副本表；
%%%   * A04：identity rebind（assignment 换人）前后 session 行、消息行数、
%%%     org/contact/conversation 完全不变；
%%%   * A05：visit token 过期/吊销即失效；作用域只绑定 (Org, contact)。
%%%
%%% 隔离：`cs_pg_test_fixture:new_scope/1` 的随机 TSID scope；无真实数据。
%%% 环境不可用 ⇒ `erlang:error/1`（不是 skip）：环境问题不得被当成 PASS。
%%% 本套件由 A0 在 scratch PG 队列独占运行；本地门禁只做编译自检。
-module(cs_pg_tests).

%% Reuse these real-store cases inside the existing owned marker DB harness.
-export([cases/1]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

-define(FIX, cs_pg_test_fixture).
-define(AUDIT_GUARD, <<"trg_customer_service_event_append_only">>).

cs_pg_test_() ->
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
        {timeout, 60, fun a01_seat_fk_rejects_sales_identity_at_db/0},
        {timeout, 60, fun a01_duplicate_seat_pk_conflicts/0},
        {timeout, 120, fun a02_concurrent_claim_exactly_one_wins/0},
        {timeout, 120, fun a02_concurrent_claims_respect_max_concurrent/0},
        {timeout, 60, fun a02_suspended_seat_rejects_claim_at_db/0},
        {timeout, 60, fun a02_dispatch_claim_picks_real_seat/0},
        {timeout, 60, fun transfer_close_rate_db_semantics/0},
        {timeout, 60, fun rating_out_of_range_rejected_by_db_check/0},
        {timeout, 60, fun a04_rebind_keeps_session_and_messages/0},
        {timeout, 60, fun a05_visit_token_expiry_and_revocation_at_db/0},
        {timeout, 60, fun a03_messages_land_only_in_enterprise_tables/0},
        {timeout, 60, fun cs_message_lifecycle_pg_checks:closed_visitor/0},
        {timeout, 60, fun cs_message_lifecycle_pg_checks:close_during_send/0},
        {timeout, 60, fun cs_message_lifecycle_pg_checks:claim_during_send/0},
        {timeout, 60, fun cs_message_lifecycle_pg_checks:suspended_seat/0},
        {timeout, 60, fun cs_message_lifecycle_pg_checks:suspension_during_send/0},
        {timeout, 60, fun cs_message_lifecycle_pg_checks:installation_revocation_during_send/0},
        {timeout, 60, fun cs_message_lifecycle_pg_checks:widget_token_revocation_during_send/0},
        {timeout, 60, fun cs_message_lifecycle_pg_checks:widget_token_expiry_during_send/0},
        {timeout, 60, fun cross_org_session_is_not_found/0},
        {timeout, 60, fun event_table_is_append_only/0},
        %% BE-S01b（A07）：admin provisioning 单事务 + 审计 + 幂等 + 回滚（真库）。
        {timeout, 120, fun bes01b_provision_seat_pg_tx_audit_idempotent_rollback/0},
        %% C1~C4（contracts-w2）：键集分页下推 + 租户隔离（真库）。
        {timeout, 60, fun c1_sessions_page_desc_keyset_and_cross_org/0},
        {timeout, 60, fun c2_shop_keys_page_and_cross_org/0},
        {timeout, 60, fun c3_visit_tokens_page_and_cross_org/0},
        {timeout, 60, fun c4_seats_page_keyset/0}
    ];
cases({error, Reason}) ->
    erlang:error({cs01_pg_suite_db_unavailable, Reason}).

%% ===================================================================
%% A01：DB 级 function 约束
%% ===================================================================

a01_seat_fk_rejects_sales_identity_at_db() ->
    Scope = ?FIX:new_scope(),
    Org = org(Scope),
    Sales = maps:get(sales_identity_id, Scope),
    try
        Before = ?FIX:count(Org, seats),
        %% 绕过应用直接 INSERT：sales identity 必须被复合 FK 拒绝
        {error, Err} = ?FIX:exec(
            <<
                "INSERT INTO customer_service_seat"
                " (organization_id, business_identity_id, function_key, enabled, max_concurrent)"
                " VALUES ($1, $2, 'customer_service', true, 1)"
            >>,
            [Org, Sales]
        ),
        ?assertEqual(<<"23503">>, error_code(Err)),
        ?assertEqual(<<"fk_css_identity_function">>, error_constraint(Err)),
        ?assertEqual(Before, ?FIX:count(Org, seats))
    after
        ?FIX:cleanup(Scope)
    end.

a01_duplicate_seat_pk_conflicts() ->
    Scope = ?FIX:new_scope(),
    Org = org(Scope),
    %% 夹具已为 service_identity 建过 seat（R2 修复）：这里造一个全新客服 identity
    Service = insert_service_identity(Scope, 9),
    try
        {ok, _} = cs_pg_store:insert_seat(Org, #{
            organization_id => Org,
            business_identity_id => Service,
            function_key => <<"customer_service">>,
            enabled => true,
            max_concurrent => 1
        }),
        {error, {sql, <<"23505">>, _Constraint}} = cs_pg_store:insert_seat(Org, #{
            organization_id => Org,
            business_identity_id => Service,
            function_key => <<"customer_service">>,
            enabled => true,
            max_concurrent => 1
        })
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% A02：真并发 CAS（spawn N 进程，恰好一个成功）
%% ===================================================================

a02_concurrent_claim_exactly_one_wins() ->
    Scope = ?FIX:new_scope(),
    Org = org(Scope),
    Ws = ws(Scope),
    Service = maps:get(service_identity_id, Scope),
    try
        SessionId = open_session_via_app(Scope),
        Params = #{
            workspace_id => Ws,
            session_id => SessionId,
            business_identity_id => Service,
            expected_version => 1,
            at => 1700000000,
            actor_user_id => maps:get(actor_user_id, Scope)
        },
        Results = parallel(8, fun() -> cs_session_app:claim(Org, Params) end),
        Winners = [R || {ok, _} = R <- Results],
        Losers = [R || {error, _} = R <- Results],
        ?assertEqual(1, length(Winners)),
        ?assertEqual(7, length(Losers)),
        [{ok, Active}] = Winners,
        ?assertEqual(active, maps:get(status, Active)),
        ?assertEqual(Service, maps:get(business_identity_id, Active)),
        %% 恰好推进一次：version 1 → 2
        ?assertEqual(2, maps:get(version, Active)),
        %% 其余全部是可区分的并发结论：胜者提交前进锁的看到 CAS 冲突（conflict），
        %% 胜者提交后进锁的看到容量已满（seat_at_capacity）——都不是笼统内部错误
        ?assert(
            lists:all(
                fun
                    ({error, conflict}) -> true;
                    %% 胜者提交后才进入的进程：应用层 domain CAS 预检先拒（更精确的结论）
                    ({error, {cas_mismatch, _}}) -> true;
                    %% 在胜者事务提交后进 seat 行锁的：容量复核先拒
                    ({error, seat_at_capacity}) -> true;
                    (_) -> false
                end,
                Losers
            )
        ),
        %% 审计恰一条（claim 状态推进与事件同事务）
        ?assertEqual(
            1,
            ?FIX:scalar(
                <<
                    "SELECT count(*) AS n FROM customer_service_event"
                    " WHERE organization_id=$1 AND session_id=$2 AND action='session.claimed'"
                >>,
                [Org, SessionId],
                -1
            )
        )
    after
        ?FIX:cleanup(Scope)
    end.

a02_concurrent_claims_respect_max_concurrent() ->
    %% max_concurrent=1：两个 queued 会话被 8 进程并发抢同一坐席 ⇒ 全局恰 1 个 active
    Scope = ?FIX:new_scope(#{max_concurrent => 1}),
    Org = org(Scope),
    Ws = ws(Scope),
    Service = maps:get(service_identity_id, Scope),
    try
        S1 = open_session_via_app(Scope),
        %% 第二会话必须换 conversation（uq_csss_org_conv_open：一会话一开放 session）
        S2 = open_session_via_app(Scope, #{conversation_id => second_conversation(Scope)}),
        ClaimOf = fun(SessionId) ->
            fun() ->
                cs_session_app:claim(Org, #{
                    workspace_id => Ws,
                    session_id => SessionId,
                    business_identity_id => Service,
                    expected_version => 1,
                    at => 1700000001
                })
            end
        end,
        Results =
            parallel(4, ClaimOf(S1)) ++ parallel(4, ClaimOf(S2)),
        Winners = [R || {ok, _} = R <- Results],
        ?assertEqual(1, length(Winners)),
        Losers = [R || {error, R} <- Results],
        %% 落败原因只有两种：CAS（被抢先的会话）或容量（同一会话第二人 / 满员）
        ?assert(
            lists:all(
                fun
                    (conflict) -> true;
                    (seat_at_capacity) -> true;
                    (_) -> false
                end,
                Losers
            )
        ),
        ActiveCount =
            ?FIX:scalar(
                <<
                    "SELECT count(*) AS n FROM customer_service_session"
                    " WHERE organization_id=$1 AND business_identity_id=$2 AND status='active'"
                >>,
                [Org, Service],
                -1
            ),
        ?assertEqual(1, ActiveCount)
    after
        ?FIX:cleanup(Scope)
    end.

a02_suspended_seat_rejects_claim_at_db() ->
    Scope = ?FIX:new_scope(),
    Org = org(Scope),
    Service = maps:get(service_identity_id, Scope),
    try
        {ok, SuspendedSeat} = cs_seat_app:suspend_seat(Org, #{
            workspace_id => ws(Scope),
            business_identity_id => Service,
            at => 1700000000,
            reason => <<"cs01-suspend">>,
            actor_user_id => maps:get(owner_user_id, Scope)
        }),
        ?assertEqual(false, maps:get(enabled, SuspendedSeat)),
        SessionId = open_session_via_app(Scope),
        {error, seat_disabled} = cs_session_app:claim(Org, #{
            workspace_id => ws(Scope),
            session_id => SessionId,
            business_identity_id => Service,
            expected_version => 1,
            at => 1700000001
        }),
        {ok, Seat} = cs_pg_store:fetch_seat(Org, Service),
        ?assertEqual(false, maps:get(enabled, Seat))
    after
        ?FIX:cleanup(Scope)
    end.

a02_dispatch_claim_picks_real_seat() ->
    Scope = ?FIX:new_scope(),
    Org = org(Scope),
    Service = maps:get(service_identity_id, Scope),
    try
        %% CS-BE-05（845cd9b1 起默认派单先做 presence 派生）：无 presence 行
        %% = 从未上报心跳 = offline，从严过滤 → no_seat_available。
        %% 派单前为该坐席种新鲜心跳（at 与 claim 注入时钟一致 → online）。
        {ok, _} = cs_seat_app:seat_heartbeat(Org, #{
            workspace_id => ws(Scope),
            business_identity_id => Service,
            at => 1700000000
        }),
        SessionId = open_session_via_app(Scope),
        %% 不指定坐席：走 list_dispatchable_seats + least-active（真库计数）
        {ok, Active} = cs_session_app:claim(Org, #{
            workspace_id => ws(Scope),
            session_id => SessionId,
            expected_version => 1,
            at => 1700000000
        }),
        ?assertEqual(Service, maps:get(business_identity_id, Active)),
        ?assertEqual(active, maps:get(status, Active))
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% transfer / close / rating 落库语义
%% ===================================================================

transfer_close_rate_db_semantics() ->
    Scope = ?FIX:new_scope(),
    Org = org(Scope),
    Ws = ws(Scope),
    Service = maps:get(service_identity_id, Scope),
    try
        SessionId = open_session_via_app(Scope),
        {ok, Active} = cs_session_app:claim(Org, #{
            workspace_id => Ws,
            session_id => SessionId,
            business_identity_id => Service,
            expected_version => 1,
            at => 1700000000
        }),
        %% transfer 需要第二个坐席：给 sales identity 也开一个 seat？不行——
        %% seat 只能引用 customer_service identity（A01）。这里造第二个客服 identity。
        Service2 = insert_service_identity(Scope, 2),
        {ok, _} = cs_pg_store:insert_seat(Org, #{
            organization_id => Org,
            business_identity_id => Service2,
            function_key => <<"customer_service">>,
            enabled => true,
            max_concurrent => 1
        }),
        {ok, Transferred} = cs_session_app:transfer(Org, #{
            workspace_id => Ws,
            session_id => SessionId,
            to_identity_id => Service2,
            expected_version => maps:get(version, Active),
            at => 1700000005
        }),
        ?assertEqual(Service2, maps:get(business_identity_id, Transferred)),
        ?assertEqual(SessionId, maps:get(id, Transferred)),
        ?assertEqual(maps:get(version, Active) + 1, maps:get(version, Transferred)),
        %% close（带原因）
        {ok, Closed} = cs_session_app:close(Org, #{
            workspace_id => Ws,
            session_id => SessionId,
            expected_version => maps:get(version, Transferred),
            at => 1700000010,
            reason => <<"cs01-solved">>
        }),
        ?assertEqual(closed, maps:get(status, Closed)),
        ?assertEqual(<<"cs01-solved">>, maps:get(close_reason, Closed)),
        ClosedVersion = maps:get(version, Closed),
        %% 重复 close：domain 终态判定先拒
        {error, session_already_closed} = cs_session_app:close(Org, #{
            workspace_id => Ws,
            session_id => SessionId,
            expected_version => ClosedVersion,
            at => 1700000011
        }),
        %% rating 落库
        {ok, Rated} = cs_session_app:rate(Org, #{
            workspace_id => Ws,
            session_id => SessionId,
            rating => 4,
            expected_version => ClosedVersion,
            at => 1700000012
        }),
        ?assertEqual(4, maps:get(rating, Rated)),
        %% DB 复核：状态与 version
        {ok, Row} = cs_pg_store:fetch_session(Org, Ws, SessionId),
        ?assertEqual(closed, maps:get(status, Row)),
        ?assertEqual(4, maps:get(rating, Row)),
        %% 事件链完整
        ?assertEqual(
            1,
            ?FIX:scalar(
                <<
                    "SELECT count(*) AS n FROM customer_service_event"
                    " WHERE organization_id=$1 AND session_id=$2 AND action='session.transferred'"
                >>,
                [Org, SessionId],
                -1
            )
        )
    after
        ?FIX:cleanup(Scope)
    end.

rating_out_of_range_rejected_by_db_check() ->
    Scope = ?FIX:new_scope(),
    Org = org(Scope),
    try
        SessionId = open_session_via_app(Scope),
        %% 绕过应用直接把 closed 会话的 rating 改为 6：DB CHECK 拒绝
        ok = ?FIX:exec(
            <<
                "UPDATE customer_service_session SET status='closed', closed_at=now(),"
                " close_reason='cs01-db', version=version+1 WHERE organization_id=$1 AND id=$2"
            >>,
            [Org, SessionId]
        ),
        {error, Err} = ?FIX:exec(
            <<
                "UPDATE customer_service_session SET rating=6"
                " WHERE organization_id=$1 AND id=$2 AND status='closed'"
            >>,
            [Org, SessionId]
        ),
        ?assertEqual(<<"23514">>, error_code(Err)),
        ?assertEqual(<<"ck_csss_rating_range">>, error_constraint(Err)),
        ok
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% A04：rebind（assignment 换人）前后 session 与历史连续
%% ===================================================================

a04_rebind_keeps_session_and_messages() ->
    Scope = ?FIX:new_scope(),
    Org = org(Scope),
    Ws = ws(Scope),
    Service = maps:get(service_identity_id, Scope),
    try
        SessionId = open_session_via_app(Scope),
        {ok, Active} = cs_session_app:claim(Org, #{
            workspace_id => Ws,
            session_id => SessionId,
            business_identity_id => Service,
            expected_version => 1,
            at => 1700000000
        }),
        %% 经 facade 写两条企业消息（真源）
        {ok, M1} = cs_session_app:append_session_message(Org, #{
            workspace_id => Ws,
            session_id => SessionId,
            business_identity_id => Service,
            actor_user_id => maps:get(actor_user_id, Scope),
            client_msg_id => <<"cs01-a04-1">>,
            body => <<"before rebind">>,
            key_ref => ?FIX:key_ref(),
            accepted_at => 1700000001,
            notify => fun(_) -> ok end
        }),
        ?assertEqual(true, maps:get(accepted, M1)),
        MessagesBefore = ?FIX:count(Org, messages),
        %% rebind：identity A 的经办人从 actor 换成 peer（EB 侧语义；数据零迁移）
        ok = rebind_assignment(
            Scope, Service, maps:get(actor_user_id, Scope), maps:get(peer_user_id, Scope)
        ),
        %% rebind 后：会话行原样可读，主体字段不变
        {ok, After} = cs_pg_store:fetch_session(Org, Ws, SessionId),
        ?assertEqual(maps:get(id, Active), maps:get(id, After)),
        ?assertEqual(Service, maps:get(business_identity_id, After)),
        ?assertEqual(maps:get(contact_id, Scope), maps:get(contact_id, After)),
        %% rebind 后新用户（同一 identity）继续发消息成功
        {ok, M2} = cs_session_app:append_session_message(Org, #{
            workspace_id => Ws,
            session_id => SessionId,
            business_identity_id => Service,
            actor_user_id => maps:get(peer_user_id, Scope),
            client_msg_id => <<"cs01-a04-2">>,
            body => <<"after rebind">>,
            key_ref => ?FIX:key_ref(),
            accepted_at => 1700000002,
            notify => fun(_) -> ok end
        }),
        ?assertEqual(true, maps:get(accepted, M2)),
        %% 消息行数 +1、旧行零改写（数量连续性）
        MessagesAfter = ?FIX:count(Org, messages),
        ?assertEqual(MessagesBefore + 1, MessagesAfter)
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% A05：visit token 落库语义
%% ===================================================================

a05_visit_token_expiry_and_revocation_at_db() ->
    Scope = ?FIX:new_scope(),
    Org = org(Scope),
    Contact = maps:get(contact_id, Scope),
    Secret = <<"cs01-visit-secret-1">>,
    %% ck_csvt_expiry 要求 expires_at > 行的 created_at（DB CURRENT_TIMESTAMP），
    %% 因此一切时间基准取**当前墙钟**（R2 修复：固定 2023 时间戳必撞 CHECK 23514）
    Now = erlang:system_time(second),
    try
        {ok, Issued} = cs_access_app:issue_visit_token(Org, #{
            workspace_id => ws(Scope),
            contact_id => Contact,
            secret => Secret,
            expires_at => Now + 100,
            created_by_user_id => maps:get(owner_user_id, Scope)
        }),
        ?assertEqual(Secret, maps:get(secret, Issued)),
        %% DB 只存 digest
        DigestInDb =
            ?FIX:scalar(
                <<
                    "SELECT token_digest FROM customer_service_visit_token"
                    " WHERE organization_id=$1 AND id=$2"
                >>,
                [Org, maps:get(id, Issued)],
                undefined
            ),
        ?assertNotEqual(Secret, DigestInDb),
        %% 有效 → 只返回 (Org, contact) 作用域
        {ok, Scope0} = cs_access_app:verify_visit_token(Org, #{secret => Secret, at => Now + 1}),
        ?assertEqual(Contact, maps:get(contact_id, Scope0)),
        ?assertEqual(visit, maps:get(scope, Scope0)),
        %% 过期即失效
        {error, token_expired} = cs_access_app:verify_visit_token(Org, #{
            secret => Secret, at => Now + 100
        }),
        %% 吊销后（未到期）立即失效
        ok = cs_access_app:revoke_visit_token(Org, #{
            workspace_id => ws(Scope), id => maps:get(id, Issued), at => Now + 2
        }),
        {error, token_revoked} = cs_access_app:verify_visit_token(Org, #{
            secret => Secret, at => Now + 3
        }),
        %% 跨 Org 命中不了（无枚举）
        {error, not_found} = cs_access_app:verify_visit_token(maps:get(other_org_id, Scope), #{
            secret => Secret, at => Now + 3
        })
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% A03：消息只落 enterprise 真源；无客服消息副本表
%% ===================================================================

a03_messages_land_only_in_enterprise_tables() ->
    Scope = ?FIX:new_scope(),
    Org = org(Scope),
    Ws = ws(Scope),
    Service = maps:get(service_identity_id, Scope),
    try
        SessionId = open_session_via_app(Scope),
        {ok, _} = cs_session_app:claim(Org, #{
            workspace_id => Ws,
            session_id => SessionId,
            business_identity_id => Service,
            expected_version => 1,
            at => 1700000000
        }),
        {ok, Result} = cs_session_app:append_session_message(Org, #{
            workspace_id => Ws,
            session_id => SessionId,
            business_identity_id => Service,
            actor_user_id => maps:get(actor_user_id, Scope),
            client_msg_id => <<"cs01-a03-1">>,
            body => <<"seat outbound via facade">>,
            key_ref => ?FIX:key_ref(),
            accepted_at => 1700000001,
            notify => fun(_) -> ok end
        }),
        ?assertEqual(true, maps:get(accepted, Result)),
        ?assertEqual(1, ?FIX:count(Org, messages)),
        %% 会话行不携带任何消息副本键（body/body_cipher 不在表结构里）
        Columns =
            ?FIX:scalar(
                <<
                    "SELECT count(*) AS n FROM information_schema.columns"
                    " WHERE table_name='customer_service_session'"
                    "   AND column_name IN ('body','body_cipher','payload')"
                >>,
                [],
                -1
            ),
        ?assertEqual(0, Columns),
        %% 数据库里不存在客服私有消息/附件副本表
        CopyTables =
            ?FIX:scalar(
                <<
                    "SELECT count(*) AS n FROM information_schema.tables"
                    " WHERE table_name LIKE 'customer_service_message%'"
                    "    OR table_name LIKE 'customer_service_attachment%'"
                >>,
                [],
                -1
            ),
        ?assertEqual(0, CopyTables)
    after
        ?FIX:cleanup(Scope)
    end.

cross_org_session_is_not_found() ->
    Scope = ?FIX:new_scope(),
    Org = org(Scope),
    OtherOrg = maps:get(other_org_id, Scope),
    try
        SessionId = open_session_via_app(Scope),
        {error, not_found} = cs_pg_store:fetch_session(OtherOrg, ws(Scope), SessionId),
        {error, not_found} = cs_pg_store:fetch_session(
            Org, maps:get(other_workspace_id, Scope), SessionId
        ),
        ?assert(is_integer(SessionId))
    after
        ?FIX:cleanup(Scope)
    end.

event_table_is_append_only() ->
    Scope = ?FIX:new_scope(),
    Org = org(Scope),
    try
        SessionId = open_session_via_app(Scope),
        {ok, _EventId} = cs_pg_store:append_event(Org, #{
            session_id => SessionId,
            action => <<"cs01.probe">>,
            detail => #{<<"probe">> => true},
            workspace_id => ws(Scope)
        }),
        EventId =
            ?FIX:scalar(
                <<
                    "SELECT id FROM customer_service_event"
                    " WHERE organization_id=$1 AND action='cs01.probe' LIMIT 1"
                >>,
                [Org],
                undefined
            ),
        ?assert(is_integer(EventId)),
        %% UPDATE / DELETE 一律被触发器拒绝（23514）
        {error, ErrU} = ?FIX:exec(
            <<
                "UPDATE customer_service_event SET action='tampered'"
                " WHERE organization_id=$1 AND id=$2"
            >>,
            [Org, EventId]
        ),
        ?assertEqual(<<"23514">>, error_code(ErrU)),
        ?assertEqual(?AUDIT_GUARD, error_constraint(ErrU)),
        {error, ErrD} = ?FIX:exec(
            <<"DELETE FROM customer_service_event WHERE organization_id=$1 AND id=$2">>,
            [Org, EventId]
        ),
        ?assertEqual(<<"23514">>, error_code(ErrD))
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% C1~C4（contracts-w2）：键集分页下推（真库边界：空 / 不足一页 / 翻页游标）
%% ===================================================================

c1_sessions_page_desc_keyset_and_cross_org() ->
    Scope = ?FIX:new_scope(),
    Org = org(Scope),
    Ws = ws(Scope),
    try
        S1 = open_session_via_app(Scope),
        S2 = open_session_via_app(Scope, #{conversation_id => second_conversation(Scope)}),
        S3 = open_session_via_app(Scope, #{conversation_id => second_conversation(Scope)}),
        %% 空租户：空列表（跨 Org 隔离：别的 Org 行命中不了）。
        {ok, []} = cs_pg_store:list_sessions_page(999999, Ws, undefined, 0, 5),
        %% status 过滤（queued 全命中）。
        {ok, All} = cs_pg_store:list_sessions_page(Org, Ws, <<"queued">>, 0, 50),
        ?assertEqual(lists:sort([S1, S2, S3]), lists:sort([maps:get(id, R) || R <- All])),
        %% DESC 键集：首页 2 行（id 大者在前）。
        {ok, Page1} = cs_pg_store:list_sessions_page(Org, Ws, undefined, 0, 2),
        ?assertEqual(2, length(Page1)),
        ?assertEqual(S3, maps:get(id, hd(Page1))),
        Cursor = maps:get(id, lists:last(Page1)),
        %% 翻页：after_id=上页尾 → 剩余行。
        {ok, Page2} = cs_pg_store:list_sessions_page(Org, Ws, undefined, Cursor, 2),
        ?assertEqual(1, length(Page2)),
        %% 游标不重复：两页 id 集合恰好覆盖全部。
        Ids = [maps:get(id, R) || R <- Page1] ++ [maps:get(id, R) || R <- Page2],
        ?assertEqual(lists:sort([S1, S2, S3]), lists:sort(Ids)),
        %% LIMIT 下推：limit=1 只回 1 行。
        {ok, One} = cs_pg_store:list_sessions_page(Org, Ws, undefined, 0, 1),
        ?assertEqual(1, length(One)),
        %% application 视图组装：满页 → next_after_id=本页尾；不足一页 → 结束。
        {ok, #{sessions := VRows1, next_after_id := Next1}} =
            cs_session_app:list_sessions(Org, #{workspace_id => Ws, limit => 2}),
        ?assertEqual([S3, Cursor], [maps:get(id, R) || R <- VRows1]),
        ?assertEqual(Cursor, Next1),
        {ok, #{sessions := VRows2, next_after_id := undefined}} =
            cs_session_app:list_sessions(Org, #{workspace_id => Ws, limit => 2, after_id => Cursor}),
        ?assertEqual(1, length(VRows2))
    after
        ?FIX:cleanup(Scope)
    end.

c2_shop_keys_page_and_cross_org() ->
    Scope = ?FIX:new_scope(),
    Org = org(Scope),
    try
        K1 = ?FIX:id(),
        K2 = ?FIX:id(),
        lists:foreach(
            fun(Id) ->
                {ok, _} = cs_pg_store:insert_shop_key(Org, #{
                    id => Id,
                    organization_id => Org,
                    key_digest => <<"cs01-key-digest-", (integer_to_binary(Id))/binary>>,
                    display_hint => <<"shop-", (integer_to_binary(Id))/binary>>
                })
            end,
            [K1, K2]
        ),
        %% 空租户（跨 Org 隔离：别的 Org 的行命中不了）。
        {ok, []} = cs_pg_store:list_shop_keys_page(999999, 0, 10),
        %% DESC 键集 + 不足一页。
        {ok, Rows} = cs_pg_store:list_shop_keys_page(Org, 0, 10),
        ?assertEqual([K2, K1], [maps:get(id, R) || R <- Rows]),
        %% 翻页游标。
        {ok, Rows2} = cs_pg_store:list_shop_keys_page(Org, K2, 10),
        ?assertEqual([K1], [maps:get(id, R) || R <- Rows2]),
        %% LIMIT 下推 + 满页。
        {ok, Rows3} = cs_pg_store:list_shop_keys_page(Org, 0, 1),
        ?assertEqual([K2], [maps:get(id, R) || R <- Rows3]),
        %% application 视图：白名单投影 + next_after_id。
        {ok, #{shop_keys := VRows, next_after_id := Next}} =
            cs_access_app:list_shop_keys(Org, #{workspace_id => ws(Scope), limit => 1}),
        ?assertEqual([K2], [maps:get(id, R) || R <- VRows]),
        ?assertEqual(K2, Next),
        ?assertNot(is_map_key(key_digest, hd(VRows)))
    after
        ?FIX:cleanup(Scope)
    end.

c3_visit_tokens_page_and_cross_org() ->
    Scope = ?FIX:new_scope(),
    Org = org(Scope),
    Contact = maps:get(contact_id, Scope),
    try
        T1 = ?FIX:id(),
        T2 = ?FIX:id(),
        lists:foreach(
            fun(Id) ->
                {ok, _} = cs_pg_store:insert_visit_token(Org, #{
                    id => Id,
                    organization_id => Org,
                    contact_id => Contact,
                    token_digest => <<"cs01-token-digest-", (integer_to_binary(Id))/binary>>,
                    expires_at => 1800000000
                })
            end,
            [T1, T2]
        ),
        {ok, []} = cs_pg_store:list_visit_tokens_page(999999, 0, 10),
        {ok, Rows} = cs_pg_store:list_visit_tokens_page(Org, 0, 10),
        ?assertEqual([T2, T1], [maps:get(id, R) || R <- Rows]),
        {ok, Rows2} = cs_pg_store:list_visit_tokens_page(Org, T2, 1),
        ?assertEqual([T1], [maps:get(id, R) || R <- Rows2]),
        %% application 视图：白名单投影不含 token_digest。
        {ok, #{visit_tokens := VRows, next_after_id := Next}} =
            cs_access_app:list_visit_tokens(Org, #{workspace_id => ws(Scope), limit => 10}),
        ?assertEqual([T2, T1], [maps:get(id, R) || R <- VRows]),
        ?assertEqual(undefined, Next),
        ?assertNot(is_map_key(token_digest, hd(VRows)))
    after
        ?FIX:cleanup(Scope)
    end.

c4_seats_page_keyset() ->
    Scope = ?FIX:new_scope(),
    Org = org(Scope),
    try
        Base = maps:get(service_identity_id, Scope),
        Extra1 = insert_service_identity(Scope, 31),
        Extra2 = insert_service_identity(Scope, 32),
        lists:foreach(
            fun(IdentityId) ->
                {ok, _} = cs_pg_store:insert_seat(Org, #{
                    organization_id => Org,
                    business_identity_id => IdentityId,
                    function_key => <<"customer_service">>,
                    enabled => true,
                    max_concurrent => 1
                })
            end,
            [Extra1, Extra2]
        ),
        %% ASC 键集（business_identity_id），模板口径；enabled=true 才出现在列表。
        {ok, All} = cs_pg_store:list_dispatchable_seats_page(Org, 0, 200),
        Ids = [maps:get(business_identity_id, R) || R <- All],
        ?assertEqual(lists:sort([Base, Extra1, Extra2]), Ids),
        %% 翻页：after_id=最小坐席 → 剩两行。
        {ok, Page2} = cs_pg_store:list_dispatchable_seats_page(Org, Base, 200),
        ?assertEqual(lists:sort([Extra1, Extra2]), [maps:get(business_identity_id, R) || R <- Page2]),
        %% LIMIT 下推：limit=1 只回首行。
        {ok, Page1} = cs_pg_store:list_dispatchable_seats_page(Org, 0, 1),
        ?assertEqual([Base], [maps:get(business_identity_id, R) || R <- Page1]),
        %% active_count 真实计数仍在（既有投影字段不变）。
        ?assert(is_map_key(active_count, hd(All))),
        %% application 视图：{seats, next_after_id}，满页游标=本页最大 identity。
        {ok, #{seats := VRows, next_after_id := Next}} =
            cs_seat_app:list_dispatchable_seats(Org, #{workspace_id => ws(Scope), limit => 1}),
        ?assertEqual([Base], [maps:get(business_identity_id, R) || R <- VRows]),
        ?assertEqual(Base, Next)
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% 辅助
%% ===================================================================

org(Scope) -> maps:get(org_id, Scope).
ws(Scope) -> maps:get(workspace_id, Scope).

open_session_via_app(Scope) ->
    open_session_via_app(Scope, #{}).

open_session_via_app(Scope, Opts) ->
    ConversationId = maps:get(conversation_id, Opts, maps:get(conversation_id, Scope)),
    {ok, S} = cs_session_app:open_session(org(Scope), #{
        workspace_id => ws(Scope),
        contact_id => maps:get(contact_id, Scope),
        conversation_id => ConversationId,
        at => 1700000000,
        created_by_user_id => maps:get(peer_user_id, Scope)
    }),
    maps:get(id, S).

%% 造第二个带 synthetic consent 的 conversation（uq_csss_org_conv_open 要求
%% 一会话同时最多一个开放客服 session，多会话测试必须换 conversation）。
second_conversation(Scope) ->
    Org = org(Scope),
    ConversationId = ?FIX:id(),
    ok = ?FIX:exec(
        <<
            "INSERT INTO enterprise_conversation"
            " (id,organization_id,workspace_id,contact_id,business_identity_id,status,version,"
            "  notice_version,consent_at,consent_subject,consent_evidence_kind)"
            " VALUES ($1,$2,$3,$4,$5,'active',1,'cs01-notice-v1',CURRENT_TIMESTAMP,$6,'synthetic')"
        >>,
        [
            ConversationId,
            Org,
            ws(Scope),
            maps:get(contact_id, Scope),
            maps:get(service_identity_id, Scope),
            <<"cs01-consent-2-", (integer_to_binary(ConversationId))/binary>>
        ]
    ),
    ConversationId.

insert_service_identity(Scope, N) ->
    Org = org(Scope),
    Id = ?FIX:id(),
    ok = ?FIX:exec(
        <<
            "INSERT INTO organization_business_identity"
            " (id,organization_id,function_key,display_name,status,version,created_by_user_id)"
            " VALUES ($1,$2,'customer_service',$3,'active',1,$4)"
        >>,
        [
            Id,
            Org,
            <<"cs01-service-", (integer_to_binary(N))/binary, "-", (integer_to_binary(Id))/binary>>,
            maps:get(owner_user_id, Scope)
        ]
    ),
    Id.

%% rebind：结束 identity 的旧 assignment，插入指向同一 identity 的新 active assignment。
rebind_assignment(Scope, IdentityId, _FromUser, ToUser) ->
    Org = org(Scope),
    ok = ?FIX:exec(
        <<
            "UPDATE organization_business_identity_assignment SET status='ended', ended_at=now()"
            " WHERE organization_id=$1 AND business_identity_id=$2 AND status='active'"
        >>,
        [Org, IdentityId]
    ),
    ok = ?FIX:exec(
        <<
            "INSERT INTO organization_business_identity_assignment"
            " (id,organization_id,business_identity_id,function_key,user_id,status,assigned_by,version)"
            " SELECT $3, i.organization_id, i.id, i.function_key, $4, 'active', $4, 1"
            "   FROM organization_business_identity i WHERE i.organization_id=$1 AND i.id=$2"
        >>,
        [Org, IdentityId, ?FIX:id(), ToUser]
    ),
    ok.

%% epgsql #error{} 的取值辅助（形状同 eb_offboarding_concurrency_tests）。
error_code({error, _Severity, Code, _Codename, _Message, _Extra}) -> Code;
error_code(_Other) -> undefined.

error_constraint({error, _Severity, _Code, _Codename, _Message, Extra}) when is_list(Extra) ->
    case lists:keyfind(constraint_name, 1, Extra) of
        {constraint_name, Name} -> Name;
        false -> undefined
    end;
error_constraint(_Other) ->
    undefined.

%% 真并发：全部进程就绪后同时放行（同一时刻持同一 expected_version 冲 CAS）。
parallel(N, Fun) ->
    Parent = self(),
    Go = make_ref(),
    Pids = [
        spawn(fun() ->
            receive
                Go -> ok
            after 30000 -> ok
            end,
            Parent ! {self(), run(Fun)}
        end)
     || _ <- lists:seq(1, N)
    ],
    [Pid ! Go || Pid <- Pids],
    [
        receive
            {Pid, Result} -> Result
        after 60000 -> timeout
        end
     || Pid <- Pids
    ].

run(Fun) ->
    try
        Fun()
    catch
        Class:Reason -> {crashed, Class, Reason}
    end.

%% ===================================================================
%% BE-S01b（A07）：admin provisioning 的 PG 证据
%% 单事务开通（identity+assignment+seat+审计同事务）、重试幂等（identity
%% 不重建 + seat 修复 enabled）、审计行 actor/target/before/after、
%% ck_css_max_concurrent 触发 seat upsert 失败 ⇒ 全回滚零残留。
%% ===================================================================

bes01b_provision_seat_pg_tx_audit_idempotent_rollback() ->
    Scope = ?FIX:new_scope(),
    Org = maps:get(org_id, Scope),
    Ws = maps:get(workspace_id, Scope),
    Peer = maps:get(peer_user_id, Scope),
    Owner = maps:get(owner_user_id, Scope),
    Adm = Owner,
    %% peer 目前不是 member：补一条 active member（provisioning 的「成员变坐席」前提）。
    ok = ?FIX:exec(
        <<
            "INSERT INTO organization_member(organization_id,user_id,role,status)"
            " VALUES ($1,$2,'member','active')"
        >>,
        [Org, Peer]
    ),
    %% 审计事件基线（此后新增的 platform.provisioned 都归本用例）。
    Baseline = ?FIX:scalar(
        <<
            "SELECT count(*) AS n FROM customer_service_event"
            " WHERE organization_id=$1 AND action='platform.provisioned'"
        >>,
        [Org],
        -1
    ),
    try
        %% ① 全新开通：单事务四写（identity/assignment/seat/审计）。
        {ok, First} = cs_pg_seat:provision_seat(Org, Ws, #{
            user_id => Peer,
            display_name => <<"pg-provisioned-seat">>,
            max_concurrent => 2,
            adm_user_id => Adm
        }),
        ?assertEqual(true, maps:get(identity_created, First)),
        IdentityId = maps:get(business_identity_id, First),
        ?assertEqual(true, maps:get(enabled, maps:get(seat, First))),
        %% ② 审计 PG 证据：platform_admin actor + adm/target/before/after。
        ?assertEqual(
            1,
            ?FIX:scalar(
                <<
                    "SELECT count(*) AS n FROM customer_service_event"
                    " WHERE organization_id=$1 AND action='platform.provisioned'"
                    " AND actor_kind='platform_admin'"
                    " AND detail->>'adm_user_id' = $2::text"
                    " AND detail->>'target_user_id' = $3::text"
                    " AND detail->>'before' = 'absent'"
                    " AND detail->>'after' = 'enabled'"
                >>,
                [Org, integer_to_binary(Adm), integer_to_binary(Peer)],
                -1
            )
        ),
        %% ③ 重试幂等：identity 不重建（同 id），disabled 的 seat 被修复 enabled。
        ok = ?FIX:exec(
            <<
                "UPDATE customer_service_seat SET enabled = false"
                " WHERE organization_id=$1 AND business_identity_id=$2"
            >>,
            [Org, IdentityId]
        ),
        {ok, Retry} = cs_pg_seat:provision_seat(Org, Ws, #{
            user_id => Peer,
            display_name => <<"pg-provisioned-seat">>,
            max_concurrent => 2,
            adm_user_id => Adm
        }),
        ?assertEqual(false, maps:get(identity_created, Retry)),
        ?assertEqual(IdentityId, maps:get(business_identity_id, Retry)),
        ?assertEqual(true, maps:get(enabled, maps:get(seat, Retry))),
        %% ④ 部分失败回滚（真事务证据）：max_concurrent=0 命中
        %% ck_css_max_concurrent，seat upsert 失败 ⇒ identity/assignment/审计
        %% 全部回滚（分配行数与审计行数零增长）。
        {error, _Rollback} = cs_pg_seat:provision_seat(Org, Ws, #{
            user_id => Peer,
            display_name => <<"rollback-probe">>,
            max_concurrent => 0,
            adm_user_id => Adm
        }),
        ?assertEqual(
            0,
            ?FIX:scalar(
                <<
                    "SELECT count(*) AS n FROM organization_business_identity"
                    " WHERE organization_id=$1 AND display_name='rollback-probe'"
                >>,
                [Org],
                -1
            )
        ),
        ?assertEqual(
            Baseline + 2,
            ?FIX:scalar(
                <<
                    "SELECT count(*) AS n FROM customer_service_event"
                    " WHERE organization_id=$1 AND action='platform.provisioned'"
                >>,
                [Org],
                -1
            )
        ),
        %% ⑤ 越权面：非 member / 作用域外 workspace 在 store 首语句即拒。
        ?assertMatch(
            {error, {not_found, member}},
            cs_pg_seat:provision_seat(Org, Ws, #{
                user_id => ?FIX:id(),
                display_name => <<"X">>,
                adm_user_id => Adm
            })
        ),
        ?assertMatch(
            {error, {not_found, workspace}},
            cs_pg_seat:provision_seat(Org, maps:get(other_workspace_id, Scope), #{
                user_id => Peer,
                display_name => <<"X">>,
                adm_user_id => Adm
            })
        )
    after
        ?FIX:cleanup(Scope)
    end.
