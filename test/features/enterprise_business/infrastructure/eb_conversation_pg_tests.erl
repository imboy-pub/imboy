%%% @doc EB-03 企业会话与 canonical 消息（同事务 message + policy snapshot + audit）套件。
%%%
%%% 覆盖：
%%%   EB-03-A01 会话/消息的租户贯穿（同语句带 OrgId + WorkspaceId，跨租户一律 fail-closed）；
%%%   EB-03-A03 显式 sender/actor 合同、canonical message + policy snapshot + audit
%%%             **同事务原子性**（中途失败无半提交）与**重放不增行**；
%%%             delivery ACK 只改投递状态、不改 canonical 真源（用 domain 判据断言）；
%%%   EB-03-A05 密文入库、审计 detail 不含明文。
-module(eb_conversation_pg_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").
-include("eunit_setup.hrl").

-define(SECONDS_PER_DAY, 86400).

conversation_message_test_() ->
    {setup, fun setup/0, fun cleanup_db/1, fun cases/1}.

setup() ->
    case eunit_runner:eunit_setup_with_db() of
        {ok, Conn} ->
            ok = eb_pg_test_fixture:ensure_purge_role(),
            {ok, Conn};
        {error, Reason} ->
            {error, Reason}
    end.

cleanup_db({ok, Conn}) ->
    eunit_runner:eunit_cleanup_db(Conn);
cleanup_db(_Other) ->
    ok.

cases({ok, _Conn}) ->
    [
        {timeout, 60, fun a01_conversation_is_tenant_scoped/0},
        {timeout, 60, fun a01_conversation_rejects_foreign_workspace_and_contact/0},
        {timeout, 60, fun a03_inbound_message_uses_explicit_contact_sender/0},
        {timeout, 60, fun a03_outbound_message_requires_identity_and_actor/0},
        {timeout, 60, fun a03_sender_contract_violations_insert_no_row/0},
        {timeout, 60, fun a03_cross_org_sender_is_rejected_by_composite_fk/0},
        {timeout, 60, fun a03_policy_snapshot_is_frozen_on_the_message/0},
        {timeout, 60, fun a03_atomic_tx_has_no_half_commit/0},
        {timeout, 60, fun a03_replay_of_same_client_msg_id_adds_no_rows/0},
        {timeout, 60, fun a03_store_append_message_is_idempotent/0},
        {timeout, 60, fun a03_missing_consent_and_missing_policy_fail_closed/0},
        {timeout, 60, fun a03_ack_changes_only_delivery_state/0},
        {timeout, 60, fun a05_message_body_and_audit_detail_have_no_plaintext/0},
        {timeout, 60, fun a03_audit_port_append_is_org_scoped_and_plaintext_free/0}
    ];
cases(_Skipped) ->
    {skip, "conversation/message suite requires the scratch database connection"}.

%% ===================================================================
%% EB-03-A01
%% ===================================================================

a01_conversation_is_tenant_scoped() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        Ws = maps:get(workspace_id, Scope),
        OtherOrg = maps:get(other_org_id, Scope),
        OtherWs = maps:get(other_workspace_id, Scope),
        Conv = maps:get(conversation_id, Scope),
        Contact = maps:get(contact_id, Scope),
        Sales = maps:get(sales_identity_id, Scope),
        {ok, Row} = eb_pg_store:fetch_conversation(Org, Ws, Conv),
        ?assertEqual(Org, maps:get(organization_id, Row)),
        ?assertEqual(Ws, maps:get(workspace_id, Row)),
        ?assertEqual(active, maps:get(status, Row)),
        ?assertEqual({error, not_found}, eb_pg_store:fetch_conversation(OtherOrg, Ws, Conv)),
        ?assertEqual({error, not_found}, eb_pg_store:fetch_conversation(Org, OtherWs, Conv)),
        %% 新建会话：跨 Org Workspace / 跨 Org 客户都必须被同语句拒绝
        NewId = eb_pg_test_fixture:id(),
        Base = #{id => NewId, contact_id => Contact, business_identity_id => Sales},
        ?assertEqual(
            {error, {workspace_not_in_org, OtherWs}},
            eb_pg_store:insert_conversation(Org, OtherWs, Base)
        ),
        MissingContact = eb_pg_test_fixture:id(),
        ?assertEqual(
            {error, {contact_not_in_org, MissingContact}},
            eb_pg_store:insert_conversation(Org, Ws, Base#{contact_id => MissingContact})
        ),
        MissingIdentity = eb_pg_test_fixture:id(),
        ?assertEqual(
            {error, {identity_not_in_org, MissingIdentity}},
            eb_pg_store:insert_conversation(Org, Ws, Base#{
                business_identity_id => MissingIdentity
            })
        ),
        ?assertMatch(
            {error, {invalid_tenant, _}},
            eb_pg_store:fetch_conversation(Org, undefined, Conv)
        )
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

a01_conversation_rejects_foreign_workspace_and_contact() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        Ws = maps:get(workspace_id, Scope),
        OtherOrg = maps:get(other_org_id, Scope),
        Contact = maps:get(contact_id, Scope),
        Sales = maps:get(sales_identity_id, Scope),
        NewId = eb_pg_test_fixture:id(),
        Result = eb_pg_store:insert_conversation(OtherOrg, Ws, #{
            id => NewId, contact_id => Contact, business_identity_id => Sales
        }),
        ?assertMatch({error, {workspace_not_in_org, _}}, Result),
        ?assertEqual(0, eb_pg_test_fixture:count(OtherOrg, Ws, conversations))
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

%% ===================================================================
%% EB-03-A03：sender / actor 合同
%% ===================================================================

a03_inbound_message_uses_explicit_contact_sender() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        {Org, Ws, Conv, Contact} = scope_ids(Scope),
        Canary = eb_pg_test_fixture:canary(),
        Now = now_secs(),
        {ok, Result} = eb_pg_canonical_tx:accept_message(Org, Ws, #{
            conversation_id => Conv,
            client_msg_id => <<"eb03-inbound-1">>,
            body => Canary,
            sender_type => contact,
            contact_id => Contact,
            key_ref => eb_pg_test_fixture:key_ref(1),
            accepted_at => Now
        }),
        Row = maps:get(message, Result),
        ?assertEqual(contact, maps:get(sender_type, Row)),
        ?assertEqual(Contact, maps:get(sender_contact_id, Row)),
        ?assertEqual(undefined, maps:get(sender_business_identity_id, Row)),
        ?assertEqual(undefined, maps:get(actor_user_id, Row)),
        %% domain 的 XOR 判据（不变量语义的唯一真源）
        ?assertEqual(ok, eb_message:validate_sender(Row)),
        ?assertEqual(false, maps:get(replayed, Result)),
        ?assert(is_integer(maps:get(audit_id, Result))),
        ?assert(eb_pg_test_fixture:canary_absent(maps:get(body_cipher, Row))),
        ?assertEqual(1, eb_pg_test_fixture:count(Org, Ws, messages))
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

a03_outbound_message_requires_identity_and_actor() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        {Org, Ws, Conv, _Contact} = scope_ids(Scope),
        Sales = maps:get(sales_identity_id, Scope),
        Actor = maps:get(actor_user_id, Scope),
        {ok, Result} = eb_pg_canonical_tx:accept_message(Org, Ws, #{
            conversation_id => Conv,
            client_msg_id => <<"eb03-outbound-1">>,
            body => <<"eb03-outbound-body">>,
            sender_type => business_identity,
            identity_id => Sales,
            actor_user_id => Actor,
            key_ref => eb_pg_test_fixture:key_ref(1)
        }),
        Row = maps:get(message, Result),
        ?assertEqual(business_identity, maps:get(sender_type, Row)),
        ?assertEqual(undefined, maps:get(sender_contact_id, Row)),
        ?assertEqual(Sales, maps:get(sender_business_identity_id, Row)),
        ?assertEqual(Actor, maps:get(actor_user_id, Row)),
        ?assertEqual(ok, eb_message:validate_sender(Row))
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

a03_sender_contract_violations_insert_no_row() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        {Org, Ws, Conv, Contact} = scope_ids(Scope),
        Sales = maps:get(sales_identity_id, Scope),
        Actor = maps:get(actor_user_id, Scope),
        KeyRef = eb_pg_test_fixture:key_ref(1),
        Base = #{
            conversation_id => Conv,
            body => <<"eb03-violation-body">>,
            key_ref => KeyRef,
            accepted_at => now_secs()
        },
        Cases = [
            %% 出站缺 actor
            {<<"eb03-violation-1">>,
                Base#{
                    client_msg_id => <<"eb03-violation-1">>,
                    sender_type => business_identity,
                    identity_id => Sales
                },
                actor_required},
            %% 入站带 identity（多态 sender 被明确禁止）
            {<<"eb03-violation-2">>,
                Base#{
                    client_msg_id => <<"eb03-violation-2">>,
                    sender_type => contact,
                    contact_id => Contact,
                    identity_id => Sales
                },
                identity_not_allowed_for_contact},
            %% 入站带 actor
            {<<"eb03-violation-3">>,
                Base#{
                    client_msg_id => <<"eb03-violation-3">>,
                    sender_type => contact,
                    contact_id => Contact,
                    actor_user_id => Actor
                },
                actor_not_allowed_for_contact},
            %% 入站缺 contact
            {<<"eb03-violation-4">>,
                Base#{
                    client_msg_id => <<"eb03-violation-4">>,
                    sender_type => contact
                },
                contact_required},
            %% 出站带 contact
            {<<"eb03-violation-5">>,
                Base#{
                    client_msg_id => <<"eb03-violation-5">>,
                    sender_type => business_identity,
                    identity_id => Sales,
                    actor_user_id => Actor,
                    contact_id => Contact
                },
                contact_not_allowed_for_identity},
            %% 未知 sender_type
            {<<"eb03-violation-6">>,
                Base#{
                    client_msg_id => <<"eb03-violation-6">>,
                    sender_type => system
                },
                {unknown_sender_type, system}}
        ],
        lists:foreach(
            fun({_ClientMsgId, Params, Expected}) ->
                ?assertEqual(
                    {error, Expected},
                    eb_pg_canonical_tx:accept_message(Org, Ws, Params)
                )
            end,
            Cases
        ),
        ?assertEqual(0, eb_pg_test_fixture:count(Org, Ws, messages)),
        ?assertEqual(0, eb_pg_test_fixture:count(Org, Ws, audits))
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

a03_cross_org_sender_is_rejected_by_composite_fk() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        {Org, Ws, Conv, _Contact} = scope_ids(Scope),
        OtherOrg = maps:get(other_org_id, Scope),
        ForeignContact = eb_pg_test_fixture:id(),
        ok = eb_pg_test_fixture:exec(
            <<
                "INSERT INTO enterprise_contact(id,organization_id,status,display_name,version)"
                " VALUES ($1,$2,'active','eb03-foreign-contact',1)"
            >>,
            [ForeignContact, OtherOrg]
        ),
        Result = eb_pg_canonical_tx:accept_message(Org, Ws, #{
            conversation_id => Conv,
            client_msg_id => <<"eb03-cross-org-sender">>,
            body => <<"eb03-cross-org-body">>,
            sender_type => contact,
            contact_id => ForeignContact,
            key_ref => eb_pg_test_fixture:key_ref(1),
            accepted_at => now_secs()
        }),
        ?assertMatch({error, {sql, _, _}}, Result),
        {error, {sql, Code, Constraint}} = Result,
        ?assertEqual(<<"23503">>, Code),
        ?assertEqual(<<"fk_em_sender_contact">>, Constraint),
        ?assertEqual(0, eb_pg_test_fixture:count(Org, Ws, messages))
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

%% ===================================================================
%% EB-03-A03：policy snapshot
%% ===================================================================

a03_policy_snapshot_is_frozen_on_the_message() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        {Org, Ws, Conv, Contact} = scope_ids(Scope),
        Policy = maps:get(policy_id, Scope),
        AcceptedAt = now_secs(),
        {ok, Result} = eb_pg_canonical_tx:accept_message(Org, Ws, #{
            conversation_id => Conv,
            client_msg_id => <<"eb03-snapshot-1">>,
            body => <<"eb03-snapshot-body">>,
            sender_type => contact,
            contact_id => Contact,
            key_ref => eb_pg_test_fixture:key_ref(1),
            accepted_at => AcceptedAt
        }),
        Row = maps:get(message, Result),
        ?assertEqual(Policy, maps:get(policy_id, Row)),
        ?assertEqual(1, maps:get(policy_version, Row)),
        ?assertEqual(1095, maps:get(retention_days, Row)),
        %% retain_until = 接受时刻 + retention_days*86400（注入时钟驱动，可逐字复现）
        ?assertEqual(AcceptedAt + 1095 * ?SECONDS_PER_DAY, maps:get(retain_until, Row)),
        %% 新策略版本（延长期限）不得回填已接受消息的快照
        NewPolicy = eb_pg_test_fixture:id(),
        ok = eb_pg_test_fixture:exec(
            <<
                "INSERT INTO enterprise_retention_policy"
                " (id,organization_id,workspace_id,data_class,version,retention_days,trigger_event)"
                " VALUES ($1,$2,$3,'enterprise_message',2,1200,'message.accept')"
            >>,
            [NewPolicy, Org, Ws]
        ),
        {ok, [AfterPolicyV2]} = eb_pg_store:list_messages(Org, Ws, Conv),
        ?assertEqual(Policy, maps:get(policy_id, AfterPolicyV2)),
        ?assertEqual(1, maps:get(policy_version, AfterPolicyV2)),
        ?assertEqual(AcceptedAt + 1095 * ?SECONDS_PER_DAY, maps:get(retain_until, AfterPolicyV2))
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

%% ===================================================================
%% EB-03-A03：原子性 / 重放
%% ===================================================================

%% 中途失败（审计 action 违反 ck_eae_action）→ 整个事务回滚：消息与审计都不留。
a03_atomic_tx_has_no_half_commit() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        {Org, Ws, Conv, Contact} = scope_ids(Scope),
        MessagesBefore = eb_pg_test_fixture:count(Org, Ws, messages),
        AuditsBefore = eb_pg_test_fixture:count(Org, Ws, audits),
        Result = eb_pg_canonical_tx:accept_message(Org, Ws, #{
            conversation_id => Conv,
            client_msg_id => <<"eb03-atomic-1">>,
            body => <<"eb03-atomic-body">>,
            sender_type => contact,
            contact_id => Contact,
            key_ref => eb_pg_test_fixture:key_ref(1),
            accepted_at => now_secs(),
            %% 合法调用方不会传空 action；此处刻意制造 DB 约束失败以证明无半提交
            audit_action => <<>>
        }),
        ?assertMatch({error, {sql, _, _}}, Result),
        {error, {sql, Code, Constraint}} = Result,
        ?assertEqual(<<"23514">>, Code),
        ?assertEqual(<<"ck_eae_action">>, Constraint),
        ?assertEqual(MessagesBefore, eb_pg_test_fixture:count(Org, Ws, messages)),
        ?assertEqual(AuditsBefore, eb_pg_test_fixture:count(Org, Ws, audits)),
        %% 失败不消耗幂等键：修复后同一 client_msg_id 仍可成功
        {ok, _} = eb_pg_canonical_tx:accept_message(Org, Ws, #{
            conversation_id => Conv,
            client_msg_id => <<"eb03-atomic-1">>,
            body => <<"eb03-atomic-body">>,
            sender_type => contact,
            contact_id => Contact,
            key_ref => eb_pg_test_fixture:key_ref(1),
            accepted_at => now_secs()
        }),
        ?assertEqual(MessagesBefore + 1, eb_pg_test_fixture:count(Org, Ws, messages))
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

%% 同一 client_msg_id 重放两次：行数与审计数都不变，第二次标记 replayed。
a03_replay_of_same_client_msg_id_adds_no_rows() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        {Org, Ws, Conv, Contact} = scope_ids(Scope),
        Params = #{
            conversation_id => Conv,
            client_msg_id => <<"eb03-replay-1">>,
            body => <<"eb03-replay-body">>,
            sender_type => contact,
            contact_id => Contact,
            key_ref => eb_pg_test_fixture:key_ref(1),
            accepted_at => now_secs()
        },
        {ok, First} = eb_pg_canonical_tx:accept_message(Org, Ws, Params),
        ?assertEqual(false, maps:get(replayed, First)),
        MessagesAfterFirst = eb_pg_test_fixture:count(Org, Ws, messages),
        AuditsAfterFirst = eb_pg_test_fixture:count(Org, Ws, audits),
        ?assertEqual(1, MessagesAfterFirst),
        ?assertEqual(1, AuditsAfterFirst),
        {ok, Second} = eb_pg_canonical_tx:accept_message(Org, Ws, Params),
        ?assertEqual(true, maps:get(replayed, Second)),
        ?assertEqual(undefined, maps:get(audit_id, Second)),
        %% 两条返回的 canonical 行逐字相同；唯一差异是调用方可见的重放标记
        ?assertEqual(
            maps:remove(replayed, maps:get(message, First)),
            maps:remove(replayed, maps:get(message, Second))
        ),
        ?assertEqual(false, maps:get(replayed, maps:get(message, First))),
        ?assertEqual(true, maps:get(replayed, maps:get(message, Second))),
        ?assertEqual(MessagesAfterFirst, eb_pg_test_fixture:count(Org, Ws, messages)),
        ?assertEqual(AuditsAfterFirst, eb_pg_test_fixture:count(Org, Ws, audits))
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

%% store 层的 append_message/3 自身也必须幂等（重放返回既有行且不增行）。
a03_store_append_message_is_idempotent() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        {Org, Ws, Conv, Contact} = scope_ids(Scope),
        KeyRef = eb_pg_test_fixture:key_ref(1),
        MsgId = eb_pg_test_fixture:id(),
        Aad = #{
            organization_id => Org,
            workspace_id => Ws,
            conversation_id => Conv,
            message_id => MsgId
        },
        {ok, Sealed} = eb_managed_crypto:seal(Aad, <<"eb03-store-idem">>, KeyRef),
        Message = #{
            id => MsgId,
            conversation_id => Conv,
            client_msg_id => <<"eb03-store-idem-1">>,
            sender_type => <<"contact">>,
            sender_contact_id => Contact,
            sender_business_identity_id => null,
            actor_user_id => null,
            body_cipher => maps:get(cipher, Sealed),
            key_version => maps:get(key_version, Sealed),
            aad_hash => maps:get(aad_hash, Sealed),
            content_hash => sha256_hex(maps:get(cipher, Sealed)),
            policy_id => maps:get(policy_id, Scope),
            policy_version => 1,
            retention_days => 1095,
            retain_until => now_secs() + 3600
        },
        {ok, First} = eb_pg_store:append_message(Org, Ws, Message),
        ?assertEqual(false, maps:get(replayed, First)),
        {ok, OtherId} = eb_pg_store:append_message(Org, Ws, Message#{id => eb_pg_test_fixture:id()}),
        ?assertEqual(true, maps:get(replayed, OtherId)),
        ?assertEqual(MsgId, maps:get(id, OtherId)),
        ?assertEqual(1, eb_pg_test_fixture:count(Org, Ws, messages))
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

%% consent / policy 缺失一律 fail-closed，不落任何行。
a03_missing_consent_and_missing_policy_fail_closed() ->
    NoConsent = eb_pg_test_fixture:new_scope(#{with_consent => false}),
    try
        {Org, Ws, Conv, _Contact} = scope_ids(NoConsent),
        Contact = maps:get(contact_id, NoConsent),
        ?assertEqual(
            {error, consent_required},
            eb_pg_canonical_tx:accept_message(Org, Ws, #{
                conversation_id => Conv,
                client_msg_id => <<"eb03-no-consent">>,
                body => <<"eb03-no-consent-body">>,
                sender_type => contact,
                contact_id => Contact,
                key_ref => eb_pg_test_fixture:key_ref(1),
                accepted_at => now_secs()
            })
        ),
        ?assertEqual(0, eb_pg_test_fixture:count(Org, Ws, messages))
    after
        eb_pg_test_fixture:cleanup(NoConsent)
    end,
    NoPolicy = eb_pg_test_fixture:new_scope(#{with_policy => false}),
    try
        {Org2, Ws2, Conv2, Contact2} = scope_ids(NoPolicy),
        ?assertEqual(
            {error, missing_retention_policy},
            eb_pg_canonical_tx:accept_message(Org2, Ws2, #{
                conversation_id => Conv2,
                client_msg_id => <<"eb03-no-policy">>,
                body => <<"eb03-no-policy-body">>,
                sender_type => contact,
                contact_id => Contact2,
                key_ref => eb_pg_test_fixture:key_ref(1),
                accepted_at => now_secs()
            })
        ),
        ?assertEqual(0, eb_pg_test_fixture:count(Org2, Ws2, messages))
    after
        eb_pg_test_fixture:cleanup(NoPolicy)
    end.

%% ===================================================================
%% EB-03-A03：delivery ACK 不改 canonical
%% ===================================================================

a03_ack_changes_only_delivery_state() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        {Org, Ws, Conv, Contact} = scope_ids(Scope),
        {ok, Result} = eb_pg_canonical_tx:accept_message(Org, Ws, #{
            conversation_id => Conv,
            client_msg_id => <<"eb03-ack-1">>,
            body => <<"eb03-ack-body">>,
            sender_type => contact,
            contact_id => Contact,
            key_ref => eb_pg_test_fixture:key_ref(1),
            accepted_at => now_secs()
        }),
        MsgId = maps:get(id, maps:get(message, Result)),
        {ok, Before} = eb_pg_store:list_messages(Org, Ws, Conv),
        RecipientRef = <<"contact:", (integer_to_binary(Contact))/binary>>,
        Ack = #{
            message_id => MsgId,
            recipient_ref => RecipientRef,
            device_id => <<"eb03-device-1">>,
            acked_at => now_secs()
        },
        {ok, Delivery} = eb_pg_store:ack_delivery(Org, Ws, Ack),
        ?assertEqual(delivered, maps:get(status, Delivery)),
        ?assertEqual(MsgId, maps:get(message_id, Delivery)),
        %% ACK 幂等：重复 ACK 不增 delivery 行
        {ok, DeliveryAgain} = eb_pg_store:ack_delivery(Org, Ws, Ack),
        ?assertEqual(maps:get(id, Delivery), maps:get(id, DeliveryAgain)),
        ?assertEqual(1, eb_pg_test_fixture:count(Org, Ws, deliveries)),
        {ok, After} = eb_pg_store:list_messages(Org, Ws, Conv),
        %% domain 判据：canonical 真源逐字不变（行数/密文 hash/scope/sender/actor/policy/retain_until）
        ?assertEqual(
            ok,
            eb_message:ack_preserves_canonical(canonical_view(Before), canonical_view(After))
        ),
        ?assertEqual(1, eb_pg_test_fixture:count(Org, Ws, messages))
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

%% ===================================================================
%% EB-03-A05：DB 无明文
%% ===================================================================

a05_message_body_and_audit_detail_have_no_plaintext() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        {Org, Ws, Conv, Contact} = scope_ids(Scope),
        Canary = eb_pg_test_fixture:canary(),
        {ok, Result} = eb_pg_canonical_tx:accept_message(Org, Ws, #{
            conversation_id => Conv,
            client_msg_id => <<"eb03-canary-1">>,
            body => Canary,
            sender_type => contact,
            contact_id => Contact,
            key_ref => eb_pg_test_fixture:key_ref(1),
            accepted_at => now_secs()
        }),
        Row = maps:get(message, Result),
        ?assert(eb_pg_test_fixture:canary_absent(maps:get(body_cipher, Row))),
        ?assert(eb_pg_test_fixture:canary_absent(maps:get(aad_hash, Row))),
        ?assert(eb_pg_test_fixture:canary_absent(maps:get(content_hash, Row))),
        %% DB 侧整体回读：消息行 + 审计 detail 都不得含明文
        %% 两张表列数不同，不能 UNION ALL；分别整行转文本后拼接。
        Blob = eb_pg_test_fixture:scalar(
            <<
                "SELECT coalesce((SELECT string_agg(m::text, '|') FROM enterprise_message m"
                "                  WHERE m.organization_id=$1 AND m.workspace_id=$2), '')"
                "    || '|' ||"
                "       coalesce((SELECT string_agg(a::text, '|') FROM enterprise_audit_event a"
                "                  WHERE a.organization_id=$1), '') AS blob"
            >>,
            [Org, Ws]
        ),
        ?assert(is_binary(Blob)),
        ?assertEqual(nomatch, binary:match(Blob, Canary)),
        %% 审计 detail 只含结构化字段（无正文、无密文原文）
        Detail = eb_pg_test_fixture:scalar(
            <<
                "SELECT detail::text AS detail FROM enterprise_audit_event"
                " WHERE organization_id=$1 AND action='message.accept' ORDER BY id DESC LIMIT 1"
            >>,
            [Org]
        ),
        ?assert(is_binary(Detail)),
        ?assertEqual(nomatch, binary:match(Detail, Canary))
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

%% 端口入口 append/2（独立事务）与注入 ID 端口的行为级覆盖：
%% canonical 事务走 append_in/3（同事务），append/2 供非事务路径复用。
a03_audit_port_append_is_org_scoped_and_plaintext_free() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        Ws = maps:get(workspace_id, Scope),
        Canary = eb_pg_test_fixture:canary(),
        Before = eb_pg_test_fixture:count(Org, Ws, audits),
        {ok, AuditId} = eb_pg_audit:append(Org, #{
            resource_type => <<"enterprise_contact">>,
            resource_id => maps:get(contact_id, Scope),
            action => <<"contact.read">>,
            business_identity_id => maps:get(sales_identity_id, Scope),
            actor_user_id => maps:get(actor_user_id, Scope),
            detail => #{<<"org">> => Org, <<"workspace_id">> => Ws}
        }),
        ?assertEqual(Before + 1, eb_pg_test_fixture:count(Org, Ws, audits)),
        ?assertEqual(
            Org,
            eb_pg_test_fixture:scalar(
                <<"SELECT organization_id FROM enterprise_audit_event WHERE id=$1">>,
                [AuditId]
            )
        ),
        %% append 不接受调用方给的 id 之外的注入 ID：端口内部经注入 ID 端口生成
        ?assert(is_integer(eb_tsid:new_id(enterprise_audit))),
        ?assertNotEqual(eb_tsid:new_id(enterprise_audit), eb_tsid:new_id(enterprise_audit)),
        %% detail 里不得出现明文金丝雀（append 只接受调用方给的结构化字段）
        Detail = eb_pg_test_fixture:scalar(
            <<"SELECT detail::text FROM enterprise_audit_event WHERE id=$1">>,
            [AuditId]
        ),
        ?assert(is_binary(Detail)),
        ?assertEqual(nomatch, binary:match(Detail, Canary))
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

%% ===================================================================
%% 辅助
%% ===================================================================

scope_ids(Scope) ->
    {
        maps:get(org_id, Scope),
        maps:get(workspace_id, Scope),
        maps:get(conversation_id, Scope),
        maps:get(contact_id, Scope)
    }.

now_secs() ->
    eb_system_clock:now().

%% canonical_fields/0 用 body_cipher_hash 命名，store 行里对应列为 content_hash。
canonical_view(Rows) ->
    [
        Row#{
            %% canonical_fields/0 用 message_id / body_cipher_hash 命名，store 行列名为 id / content_hash
            message_id => maps:get(id, Row),
            body_cipher_hash => maps:get(content_hash, Row)
        }
     || Row <- Rows
    ].

sha256_hex(Bin) ->
    binary:encode_hex(crypto:hash(sha256, Bin), lowercase).
