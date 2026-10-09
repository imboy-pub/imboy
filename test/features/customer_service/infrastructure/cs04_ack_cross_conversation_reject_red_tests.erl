%%% @doc CX-04「拒绝跨会话消息 ID」合同的 failing regression（故意 RED）。
%%%
%%% 同一候选上两份有效合同对「ACK 非本会话消息 ID」的要求相反：
%%%
%%%   * CS-DEC-02（已冻结决策；cs_read_cursor_pg_tests 验收 1/5 的现有断言）：
%%%     候选游标收敛到本会话已存在消息的事实上界（clamp 语义）。
%%%   * CX-04（docs/design/2026-10-08-customer-service-experience-production/
%%%     plan-v1.md Step 5，plan SHA256 14289f917fb3e273e495b0e91de27f393844
%%%     eb7f4c9cdb717bced1d94407f7fc）：「Backend 拒绝非本会话消息 ID，
%%%     游标单调且不越 scope」。
%%%
%%% 本文件按 CX-04 合同写验收：ACK 一个真实存在、但属于另一 conversation
%%% 的消息 id 时，必须返回 error 且不得推进本会话游标。当前实现
%%% （cs_pg_session:effective_cursor_in/5 的 `id <= Candidate` 收敛）会把它
%%% clamp 到本会话上界并返回 ok——因此本测试**故意失败**，如实暴露
%%% CONTRACT_CONFLICT（验收状态表用该枚举，不是 PASS/FAIL 二值）。
%%%
%%% 处置约束（CX-02 交接 nextUnlock 既定方针）：
%%%   * 保留冲突——不修改 CS-DEC-02 现有断言、不放宽本文件断言、不加
%%%     skip、不删测试制造通过；
%%%   * scoped fix 仅在用户或被授权的合同负责人裁决语义（reject / clamp /
%%%     折中）之后进行；届时本文件与 cs_read_cursor_pg_tests 的对应断言
%%%     一起按裁决对齐；
%%%   * 本文件的红灯不参与任何绿灯声明；它失败 = 合同冲突仍待裁决。
%%%
%%% 隔离与 cs_inbox_pg_tests 同款：随机 TSID scope + fixture 直驱（不自起
%%% eunit_runner/app config；由本 run 私有 runtime 的 pgsql pool 承载）；
%%% 无真实数据；环境不可用 ⇒ 直接抛错（不是 skip）。
-module(cs04_ack_cross_conversation_reject_red_tests).

-include_lib("eunit/include/eunit.hrl").

-define(FIX, cs_pg_test_fixture).

cross_conversation_ack_must_be_rejected_test() ->
    Scope = ?FIX:new_scope(),
    try
        %% 会话 A：本坐席持有；消息 Ma（本会话事实）。
        SeedA = owned_session(Scope),
        SessionA = maps:get(session_id, SeedA),
        {ok, #{conversation_id := ConvA}} = cs_session_app:fetch_session(
            org(Scope), #{workspace_id => ws(Scope), session_id => SessionA}
        ),
        Ma = seed_message_in(Scope, ConvA, contact, visible),
        %% 另一个 conversation（同 org/workspace/contact/identity 绑定，
        %% 新随机 TSID）：其消息 Mb 真实存在但不属于会话 A 的 conversation。
        ConvB = insert_second_conversation(Scope),
        Mb = seed_message_in(Scope, ConvB, contact, visible),
        ?assert(Mb > Ma),
        %% 先合法 ACK 到 Ma（两种合同语义下都成功），使游标有确定初值：
        %% 之后无论裁决为 reject（不动）还是 clamp（收敛回 Ma），探针的
        %% 期望值都是 Ma——游标推进断言不预编码任何一方的语义。
        {ok, _} = cs_seat_app:ack_read(org(Scope), #{
            workspace_id => ws(Scope),
            session_id => SessionA,
            business_identity_id => seat_identity(Scope),
            last_read_message_id => Ma,
            at => 1700000050
        }),
        %% CX-04：对会话 A ACK 会话 B 的消息 id ⇒ 必须 error。
        %% 当前实现返回 {ok, #{last_read_message_id => Ma}}（clamp）——
        %% 本断言失败即合同冲突的 RED 证据。
        ?assertMatch(
            {error, _},
            cs_seat_app:ack_read(org(Scope), #{
                workspace_id => ws(Scope),
                session_id => SessionA,
                business_identity_id => seat_identity(Scope),
                last_read_message_id => Mb,
                at => 1700000100
            })
        ),
        %% 且游标不得被该非法 ACK 推进到 Ma 之外（DB 事实探针）。
        ?assertEqual(Ma, stored_cursor(Scope, SessionA, seat_identity(Scope)))
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% 夹具辅助（镜像 cs_read_cursor_pg_tests 的最小子集 + 第二 conversation）
%% ===================================================================

org(Scope) -> maps:get(org_id, Scope).

ws(Scope) -> maps:get(workspace_id, Scope).

seat_identity(Scope) -> maps:get(service_identity_id, Scope).

owned_session(Scope) ->
    SessionId = open_session(Scope, maps:get(conversation_id, Scope)),
    {ok, _} = cs_session_app:claim(org(Scope), #{
        workspace_id => ws(Scope),
        session_id => SessionId,
        business_identity_id => seat_identity(Scope),
        expected_version => 1,
        at => 1700000000
    }),
    #{
        session_id => SessionId,
        business_identity_id => seat_identity(Scope),
        workspace_id => ws(Scope)
    }.

open_session(Scope, ConversationId) ->
    {ok, Session} = cs_session_app:open_session(org(Scope), #{
        workspace_id => ws(Scope),
        contact_id => maps:get(contact_id, Scope),
        conversation_id => ConversationId,
        at => 1699999900
    }),
    maps:get(id, Session).

%% 同 org/workspace/contact/service-identity 绑定的第二个 conversation
%% （镜像 fixture 的 enterprise_conversation 插入，随机 TSID 主键）。
insert_second_conversation(Scope) ->
    Conversation = ?FIX:id(),
    ok = ?FIX:exec(
        <<
            "INSERT INTO enterprise_conversation"
            " (id,organization_id,workspace_id,contact_id,business_identity_id,status,version,"
            " notice_version,consent_at,consent_subject,consent_evidence_kind)"
            " VALUES ($1,$2,$3,$4,$5,'active',1,'cs01-notice-v1',CURRENT_TIMESTAMP,$6,'synthetic')"
        >>,
        [
            Conversation,
            org(Scope),
            ws(Scope),
            maps:get(contact_id, Scope),
            seat_identity(Scope),
            integer_to_binary(Conversation)
        ]
    ),
    Conversation.

%% 指定 conversation 内的一条 enterprise_message（canonical 真源事实）。
seed_message_in(Scope, ConversationId, SenderType, Visibility) ->
    MessageId = ?FIX:id(),
    {SenderContact, SenderIdentity, ActorUser} =
        case SenderType of
            contact -> {maps:get(contact_id, Scope), null, null};
            business_identity -> {null, seat_identity(Scope), maps:get(owner_user_id, Scope)}
        end,
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
            ConversationId,
            atom_to_binary(SenderType),
            SenderContact,
            SenderIdentity,
            ActorUser,
            <<"cs04red-", (integer_to_binary(MessageId))/binary>>,
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
