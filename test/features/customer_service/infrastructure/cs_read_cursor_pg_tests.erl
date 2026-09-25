%%% @doc CS-BE-04（durable read cursor / unread）真库 focused 套件。
%%%
%%% 冻结决策 CS-DEC-02（evidence/CS-DEC-02/decision.md）：
%%%   * read cursor 是 **per-assignment**（坐席-会话绑定级）——绑定
%%%     (org, session, 经办 identity)，不是全局 user；
%%%   * 新坐席接手（transfer）时以 transfer 时刻的最后消息为起点——历史可读，
%%%     转接前历史不算新未读（默认 0 unread）；
%%%   * ACK 单调前进、不可回退：重复/乱序后到的旧 ACK 幂等（零副作用）；
%%%   * 未读数由 cursor 与 enterprise_message 事实**同语句现算**（visible ∧
%%%     sender_type='contact' ∧ id > cursor），无冗余计数表；
%%%   * 时钟可注入：`at` 由调用方显式传入（固定值），写路径零 now() 依赖。
%%%
%%% 覆盖（CS-RUNTIME-01 验收）：
%%%   1. 重复/乱序 ACK 幂等（后到的旧 cursor 不得覆盖新值）；
%%%   2. 跨 Seat ACK 403 语义（not_session_owner）/ 跨 Org not_found；
%%%   3. transfer 前历史默认 0 unread（含 A→B→A 重受让边界重置）；
%%%   4. 新消息精确增加 unread（出站/hidden 消息不计）；
%%%   5. 迁移回滚(down)+重放(up) cycle（单独证据 migration_147_down_up_cycle.log）；
%%%   6. 时钟/时间可注入（重复 ACK 不同 at 不刷新 updated_at；写值 = 注入 at）。
%%%
%%% 隔离：`cs_pg_test_fixture:new_scope/0` 随机 TSID scope；无真实数据；
%%% 环境不可用 ⇒ `erlang:error/1`（不是 skip）。与共享 scratch DB 上只做
%%% 随机 scope 的 INSERT/UPDATE/DELETE，无 DROP/TRUNCATE。
-module(cs_read_cursor_pg_tests).

-include_lib("eunit/include/eunit.hrl").

-define(FIX, cs_pg_test_fixture).

cs_read_cursor_pg_test_() ->
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
        {timeout, 60, fun duplicate_and_out_of_order_ack_is_idempotent/0},
        {timeout, 60, fun cross_seat_ack_rejected_and_cross_org_not_found/0},
        {timeout, 60, fun transfer_history_defaults_zero_unread/0},
        {timeout, 60, fun retransfer_resets_grantee_cursor_to_boundary/0},
        {timeout, 60, fun new_message_precisely_increments_unread/0},
        {timeout, 60, fun ack_without_any_message_converges_to_zero/0},
        {timeout, 60, fun injected_clock_noop_keeps_updated_at/0}
    ];
cases({error, Reason}) ->
    erlang:error({csbe04_pg_suite_db_unavailable, Reason}).

%% ===================================================================
%% 验收 1：重复/乱序 ACK 幂等——后到的旧 cursor 不得覆盖新值
%% ===================================================================

duplicate_and_out_of_order_ack_is_idempotent() ->
    Scope = ?FIX:new_scope(),
    try
        Seed = owned_session(Scope),
        [M1, M2, M3] = seed_contact_messages(Scope, Seed, 3),
        SessionId = maps:get(session_id, Seed),
        Identity = seat_identity(Scope),
        %% 前进到 M3：unread 清零。
        {ok, State3} = ack(Scope, Seed, M3),
        ?assertEqual(M3, maps:get(last_read_message_id, State3)),
        ?assertEqual(0, maps:get(unread_count, State3)),
        %% 乱序后到的旧 ACK：M1 / M2 均不得回退 M3（幂等 no-op）。
        ?assertEqual(
            {ok, State3#{session_id => SessionId}},
            ack(Scope, Seed, M1)
        ),
        ?assertEqual(
            {ok, State3#{session_id => SessionId}},
            ack(Scope, Seed, M2)
        ),
        %% 重复 ACK M3：同样 no-op。
        ?assertEqual(
            {ok, State3#{session_id => SessionId}},
            ack(Scope, Seed, M3)
        ),
        %% DB 行事实仍是 M3（单行、单值——无任何回退痕迹）。
        ?assertEqual(M3, stored_cursor(Scope, SessionId, Identity)),
        %% ACK 越界（未来/不存在的消息 id）收敛到已存在事实上界（M3）。
        ?assertEqual(
            {ok, State3#{session_id => SessionId}},
            ack(Scope, Seed, huge_id(Scope))
        ),
        ?assertEqual(M3, stored_cursor(Scope, SessionId, Identity))
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% 验收 2：跨 Seat ACK 拒绝（403 面）/ 跨 Org not_found
%% ===================================================================

cross_seat_ack_rejected_and_cross_org_not_found() ->
    Scope = ?FIX:new_scope(),
    try
        Seed = owned_session(Scope),
        [_M1] = seed_contact_messages(Scope, Seed, 1),
        SessionId = maps:get(session_id, Seed),
        %% 同 Org 的另一坐席：不是当前经办 ⇒ 403 面 not_session_owner。
        OtherSeat = make_seat(Scope),
        ?assertEqual(
            {error, {not_session_owner, OtherSeat, seat_identity(Scope)}},
            cs_seat_app:ack_read(org(Scope), #{
                workspace_id => ws(Scope),
                session_id => SessionId,
                business_identity_id => OtherSeat,
                last_read_message_id => huge_id(Scope),
                at => 1700000200
            })
        ),
        %% 读面同门：他坐席也读不到本会话读状态。
        ?assertEqual(
            {error, {not_session_owner, OtherSeat, seat_identity(Scope)}},
            cs_seat_app:read_state(org(Scope), #{
                workspace_id => ws(Scope),
                session_id => SessionId,
                business_identity_id => OtherSeat
            })
        ),
        %% 跨 Org：store 同语句裁决 not_found（不区分不存在与跨租户，防枚举）。
        OtherIdentity = make_seat_in(Scope, maps:get(other_org_id, Scope)),
        ?assertEqual(
            {error, not_found},
            cs_seat_app:ack_read(maps:get(other_org_id, Scope), #{
                workspace_id => maps:get(other_workspace_id, Scope),
                session_id => SessionId,
                business_identity_id => OtherIdentity,
                last_read_message_id => huge_id(Scope),
                at => 1700000201
            })
        ),
        %% 撤权（suspend）后本经办 ACK 立即被拒（seat_disabled）。
        {ok, _} = cs_seat_app:suspend_seat(org(Scope), #{
            workspace_id => ws(Scope),
            business_identity_id => seat_identity(Scope),
            at => 1700000202
        }),
        ?assertEqual(
            {error, seat_disabled},
            cs_seat_app:ack_read(org(Scope), #{
                workspace_id => ws(Scope),
                session_id => SessionId,
                business_identity_id => seat_identity(Scope),
                last_read_message_id => huge_id(Scope),
                at => 1700000203
            })
        )
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% 验收 3：transfer 前的历史对受让人默认 0 unread
%% ===================================================================

transfer_history_defaults_zero_unread() ->
    Scope = ?FIX:new_scope(),
    try
        Seed = owned_session(Scope),
        SessionId = maps:get(session_id, Seed),
        [M1, M2] = seed_contact_messages(Scope, Seed, 2),
        %% 转接前：原经办 A 的未读是真实未读（M1/M2），默认从 0 起算。
        {ok, Before} = read_state(Scope, Seed),
        ?assertEqual(0, maps:get(last_read_message_id, Before)),
        ?assertEqual(2, maps:get(unread_count, Before)),
        %% A → B 转接：受让人 B 的游标 = transfer 边界（M2），历史 0 unread。
        SeatB = make_seat(Scope),
        {ok, _} = cs_session_app:transfer(org(Scope), #{
            workspace_id => ws(Scope),
            session_id => SessionId,
            to_identity_id => SeatB,
            expected_version => 2,
            at => 1700000200
        }),
        {ok, AfterTransfer} = read_state(Scope, #{
            session_id => SessionId, business_identity_id => SeatB
        }),
        ?assertEqual(M2, maps:get(last_read_message_id, AfterTransfer)),
        ?assertEqual(0, maps:get(unread_count, AfterTransfer)),
        %% transfer 后新消息精确计入受让人（B）：未读从 0 → 1。
        [M3] = seed_contact_messages(Scope, Seed, 1),
        {ok, AfterNew} = read_state(Scope, #{session_id => SessionId, business_identity_id => SeatB}),
        ?assertEqual(M2, maps:get(last_read_message_id, AfterNew)),
        ?assertEqual(1, maps:get(unread_count, AfterNew)),
        _ = [M1, M3],
        %% 原经办 A 的游标行独立留在原地（per-assignment，互不污染）。
        %% 注意：A 已失去 ownership，read_state 被授权门拒绝（403 语义，
        %% 与 CS-BE-03 context 端点同门）——游标独立性只能用 DB 事实探针。
        {error, {not_session_owner, _, _}} =
            read_state(Scope, Seed),
        ?assertEqual(0, stored_cursor(Scope, SessionId, seat_identity(Scope)))
    after
        ?FIX:cleanup(Scope)
    end.

%% A→B→A 重受让：A 的旧游标被新 transfer 边界覆盖（transfer 是新 assignment
%% 边界的确立事件，不是 ACK——CS-DEC-02 字面语义「以 transfer 时刻的最后
%% 消息为起点」）。
retransfer_resets_grantee_cursor_to_boundary() ->
    Scope = ?FIX:new_scope(),
    try
        Seed = owned_session(Scope),
        SessionId = maps:get(session_id, Seed),
        [M1] = seed_contact_messages(Scope, Seed, 1),
        %% A 读到 M1。
        {ok, _} = ack(Scope, Seed, M1),
        SeatB = make_seat(Scope),
        {ok, _} = cs_session_app:transfer(org(Scope), #{
            workspace_id => ws(Scope),
            session_id => SessionId,
            to_identity_id => SeatB,
            expected_version => 2,
            at => 1700000200
        }),
        %% B 期间新消息（contact 入站）到 M2；B ACK 到 M2。
        [M2] = seed_contact_messages(Scope, Seed, 1),
        SeedB = #{session_id => SessionId, business_identity_id => SeatB},
        {ok, _} = ack(Scope, SeedB, M2),
        %% B → A 回转：A 的旧游标（M1）被 transfer 边界（M2）覆盖——
        %% transfer 前的历史对受让人 A 默认 0 unread。
        {ok, _} = cs_session_app:transfer(org(Scope), #{
            workspace_id => ws(Scope),
            session_id => SessionId,
            to_identity_id => seat_identity(Scope),
            expected_version => 3,
            at => 1700000300
        }),
        {ok, StateA} = read_state(Scope, Seed),
        ?assertEqual(M2, maps:get(last_read_message_id, StateA)),
        ?assertEqual(0, maps:get(unread_count, StateA))
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% 验收 4：新消息精确增加 unread（出站 / hidden 不计）
%% ===================================================================

new_message_precisely_increments_unread() ->
    Scope = ?FIX:new_scope(),
    try
        Seed = owned_session(Scope),
        %% 空会话：unread = 0。
        {ok, Empty} = read_state(Scope, Seed),
        ?assertEqual(0, maps:get(unread_count, Empty)),
        %% 客户消息 M1 → unread 1；M2 → 2（精确 1:1 递增）。
        [M1, M2] = seed_contact_messages(Scope, Seed, 2),
        {ok, AfterTwo} = read_state(Scope, Seed),
        ?assertEqual(2, maps:get(unread_count, AfterTwo)),
        %% ACK M1：M2 仍未读（精确减 1，不是清零）。
        {ok, AfterAck} = ack(Scope, Seed, M1),
        ?assertEqual(M1, maps:get(last_read_message_id, AfterAck)),
        ?assertEqual(1, maps:get(unread_count, AfterAck)),
        %% 坐席出站消息不计入自己的未读（sender_type='business_identity'）。
        Outbound = seed_identity_message(Scope, Seed),
        {ok, AfterOutbound} = read_state(Scope, Seed),
        ?assertEqual(1, maps:get(unread_count, AfterOutbound)),
        _ = Outbound,
        %% 撤回（hidden）的 contact 消息不计未读。
        Hidden = seed_hidden_contact_message(Scope, Seed),
        {ok, AfterHidden} = read_state(Scope, Seed),
        ?assertEqual(1, maps:get(unread_count, AfterHidden)),
        _ = Hidden,
        %% ACK 到 M2 后全部清零（hidden/出站行不产生幽灵未读）。
        {ok, Cleared} = ack(Scope, Seed, M2),
        ?assertEqual(0, maps:get(unread_count, Cleared))
    after
        ?FIX:cleanup(Scope)
    end.

%% 无任何消息的会话 ACK（候选收敛 0）与 ACK 值 0 的幂等边界。
ack_without_any_message_converges_to_zero() ->
    Scope = ?FIX:new_scope(),
    try
        Seed = owned_session(Scope),
        SessionId = maps:get(session_id, Seed),
        Identity = seat_identity(Scope),
        {ok, State} = ack(Scope, Seed, 0),
        ?assertEqual(0, maps:get(last_read_message_id, State)),
        ?assertEqual(0, maps:get(unread_count, State)),
        ?assertEqual(0, stored_cursor(Scope, SessionId, Identity)),
        %% ACK 指向不存在的消息 id：收敛到 0（会话内无更早消息）。
        {ok, _} = ack(Scope, Seed, huge_id(Scope)),
        ?assertEqual(0, stored_cursor(Scope, SessionId, Identity))
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% 验收 6：时钟/时间可注入——写值 = 注入 at；重复 ACK 不同 at 不刷新
%% ===================================================================

injected_clock_noop_keeps_updated_at() ->
    Scope = ?FIX:new_scope(),
    try
        Seed = owned_session(Scope),
        SessionId = maps:get(session_id, Seed),
        Identity = seat_identity(Scope),
        [M1] = seed_contact_messages(Scope, Seed, 1),
        %% 注入 at = 1700000100：updated_at 必须等于注入值（to_timestamp 秒），
        %% 不是运行时 now()。
        {ok, _} = cs_seat_app:ack_read(org(Scope), #{
            workspace_id => ws(Scope),
            session_id => SessionId,
            business_identity_id => Identity,
            last_read_message_id => M1,
            at => 1700000100
        }),
        {ok, UpdatedAt1} = stored_cursor_updated_at(Scope, SessionId, Identity),
        ?assertEqual(1700000100, UpdatedAt1),
        %% 之后换一个注入时钟重复 ACK（乱序旧值 no-op）：updated_at 不被刷新。
        sleep(1100),
        {ok, _} = cs_seat_app:ack_read(org(Scope), #{
            workspace_id => ws(Scope),
            session_id => SessionId,
            business_identity_id => Identity,
            last_read_message_id => M1,
            at => 1700009999
        }),
        {ok, UpdatedAt2} = stored_cursor_updated_at(Scope, SessionId, Identity),
        ?assertEqual(UpdatedAt1, UpdatedAt2)
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% 夹具辅助（随机 TSID scope 内自建事实；无 DROP/TRUNCATE）
%% ===================================================================

sleep(Ms) -> timer:sleep(Ms).

org(Scope) -> maps:get(org_id, Scope).

ws(Scope) -> maps:get(workspace_id, Scope).

seat_identity(Scope) -> maps:get(service_identity_id, Scope).

%% 越界 ACK 候选：TSID 域内 + 10 亿（约 32 年时间偏移）的未存在 id——
%% 仍在 bigint 范围内（乘法版会溢出 int8 打爆连接，历史教训见注释），
%% 测试窗口内不会有任何真实消息命中该值。
huge_id(Scope) -> maps:get(org_id, Scope) + 1000000000.

%% ACK / 读状态直驱 application 用例（HTTP 认证派生键在此显式给出）。
ack(Scope, Seed, MessageId) ->
    cs_seat_app:ack_read(org(Scope), #{
        workspace_id => ws(Scope),
        session_id => maps:get(session_id, Seed),
        business_identity_id => maps:get(business_identity_id, Seed, seat_identity(Scope)),
        last_read_message_id => MessageId,
        at => 1700000100
    }).

read_state(Scope, Seed) ->
    cs_seat_app:read_state(org(Scope), #{
        workspace_id => ws(Scope),
        session_id => maps:get(session_id, Seed),
        business_identity_id => maps:get(business_identity_id, Seed, seat_identity(Scope))
    }).

%% 建一条「已 claim、有 ownership」的锚定会话；返回直驱读参数。
owned_session(Scope) ->
    SessionId = open_session(Scope),
    ok = claim(Scope, SessionId, seat_identity(Scope), 1700000000),
    #{
        session_id => SessionId,
        business_identity_id => seat_identity(Scope),
        workspace_id => ws(Scope)
    }.

open_session(Scope) ->
    {ok, Session} = cs_session_app:open_session(org(Scope), #{
        workspace_id => ws(Scope),
        contact_id => maps:get(contact_id, Scope),
        conversation_id => maps:get(conversation_id, Scope),
        at => 1699999900
    }),
    maps:get(id, Session).

claim(Scope, SessionId, IdentityId, At) ->
    {ok, _} = cs_session_app:claim(org(Scope), #{
        workspace_id => ws(Scope),
        session_id => SessionId,
        business_identity_id => IdentityId,
        expected_version => 1,
        at => At
    }),
    ok.

%% seed N 条 id 递增的 contact 入站消息（未读语义的事实源）。
%% id 用排序后的 TSID：全局唯一（无跨 scope 主键冲突），同批严格递增；
%% 跨批单调由 elib_tsid 的严格递增生成保证（后批 min > 前批 max）——
%% 未读/幂等断言依赖的 M2 < M3 顺序因此确定。
seed_contact_messages(Scope, Seed, N) ->
    Ids = lists:sort([?FIX:id() || _ <- lists:seq(1, N)]),
    [seed_message(Scope, Seed, Id, contact, visible) || Id <- Ids].

seed_identity_message(Scope, Seed) ->
    seed_message(Scope, Seed, ?FIX:id(), business_identity, visible).

seed_hidden_contact_message(Scope, Seed) ->
    seed_message(Scope, Seed, ?FIX:id(), contact, hidden).

%% enterprise_message 行（canonical 真源事实；出站必须带 actor_user_id）。
seed_message(Scope, Seed, MessageId, SenderType, Visibility) ->
    SessionId = maps:get(session_id, Seed),
    {ok, #{conversation_id := ConvId}} = cs_session_app:fetch_session(
        org(Scope), #{workspace_id => ws(Scope), session_id => SessionId}
    ),
    {SenderContact, SenderIdentity, ActorUser} =
        case SenderType of
            contact -> {maps:get(contact_id, Scope), null, null};
            business_identity -> {null, seat_identity(Scope), maps:get(owner_user_id, Scope)}
        end,
    CMsgId = <<"csbe04-", (integer_to_binary(MessageId))/binary>>,
    ok = ?FIX:exec(
        <<
            "INSERT INTO enterprise_message"
            " (id, organization_id, workspace_id, conversation_id, sender_type,"
            "  sender_contact_id, sender_business_identity_id, actor_user_id,"
            "  client_msg_id, retention_days, retain_until, visibility, created_at)"
            " VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, 1095,"
            "         to_timestamp(1900000000), $10, to_timestamp(1700000000))"
        >>,
        [
            MessageId,
            org(Scope),
            ws(Scope),
            ConvId,
            atom_to_binary(SenderType),
            SenderContact,
            SenderIdentity,
            ActorUser,
            CMsgId,
            atom_to_binary(Visibility)
        ]
    ),
    MessageId.

%% DB 事实探针：游标行当前值（不存在 = 0）。
stored_cursor(Scope, SessionId, Identity) ->
    V = ?FIX:scalar(
        <<
            "SELECT last_read_message_id FROM customer_service_read_cursor"
            " WHERE organization_id = $1 AND session_id = $2 AND business_identity_id = $3"
        >>,
        [org(Scope), SessionId, Identity],
        0
    ),
    case is_integer(V) of
        true -> V;
        false -> 0
    end.

stored_cursor_updated_at(Scope, SessionId, Identity) ->
    case
        ?FIX:scalar(
            <<
                "SELECT extract(epoch from updated_at)::bigint FROM customer_service_read_cursor"
                " WHERE organization_id = $1 AND session_id = $2 AND business_identity_id = $3"
            >>,
            [org(Scope), SessionId, Identity]
        )
    of
        N when is_integer(N) -> {ok, N};
        _ -> {error, not_found}
    end.

%% 新开一个 customer_service identity + enabled seat；返回 identity id。
make_seat(Scope) ->
    make_seat_in(Scope, org(Scope)).

make_seat_in(Scope, TargetOrg) ->
    Identity = ?FIX:id(),
    Owner = maps:get(owner_user_id, Scope),
    ok = ?FIX:exec(
        <<
            "INSERT INTO organization_business_identity"
            " (id, organization_id, function_key, display_name, status, version, created_by_user_id)"
            " VALUES ($1, $2, 'customer_service', $3, 'active', 1, $4)"
        >>,
        [Identity, TargetOrg, integer_to_binary(?FIX:id()), Owner]
    ),
    ok = ?FIX:exec(
        <<
            "INSERT INTO customer_service_seat"
            " (organization_id, business_identity_id, function_key, enabled, max_concurrent,"
            "  created_by_user_id)"
            " VALUES ($1, $2, 'customer_service', true, 1, $3)"
        >>,
        [TargetOrg, Identity, Owner]
    ),
    Identity.
