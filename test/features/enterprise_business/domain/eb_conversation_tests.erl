%%% @doc 企业会话与消息 domain 纯函数测试（零 mock / 零 I/O / 零隐式时钟）。
%%%
%%% 覆盖（EB-02-A02 / EB-02-A05 的 sender 与 consent 分支）：
%%%   * eb_message:validate_sender/1 —— plan EB-D05 的 sender XOR 全枚举
%%%     （四条入站违约 + 四条出站违约 + 未知/缺失 sender_type）；
%%%   * eb_message:inbound/1 / outbound/1 构造函数与 validate_sender 自洽；
%%%   * eb_message:handover_preserves_sender/2 —— handover 不得重写历史
%%%     message 的 sender/actor；
%%%   * eb_conversation:open/1 —— conversation 的 Org/Workspace 必须同源
%%%     （plan §2.1 #15）；
%%%   * eb_conversation:handover/2 —— 只换当前经办 identity，owner/resource id 不变；
%%%   * eb_consent:gate/2 —— 无 consent fail-closed；classify/1 严禁把
%%%     synthetic 映射为 real（plan §2.1 #14 / §5.4）。
-module(eb_conversation_tests).

-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% eb_message:validate_sender/1 —— 入站（contact）
%% ===================================================================

sender_inbound_ok_test() ->
    ?assertEqual(ok, eb_message:validate_sender(inbound_sender())).

sender_inbound_contact_empty_test() ->
    Sender = (inbound_sender())#{sender_contact_id => undefined},
    ?assertEqual({error, contact_required}, eb_message:validate_sender(Sender)).

sender_inbound_identity_set_test() ->
    Sender = (inbound_sender())#{sender_business_identity_id => 3},
    ?assertEqual(
        {error, identity_not_allowed_for_contact},
        eb_message:validate_sender(Sender)
    ).

sender_inbound_actor_set_test() ->
    Sender = (inbound_sender())#{actor_user_id => 9},
    ?assertEqual(
        {error, actor_not_allowed_for_contact},
        eb_message:validate_sender(Sender)
    ).

%% ===================================================================
%% eb_message:validate_sender/1 —— 出站（business_identity）
%% ===================================================================

sender_outbound_ok_test() ->
    ?assertEqual(ok, eb_message:validate_sender(outbound_sender())).

sender_outbound_contact_set_test() ->
    Sender = (outbound_sender())#{sender_contact_id => 7},
    ?assertEqual(
        {error, contact_not_allowed_for_identity},
        eb_message:validate_sender(Sender)
    ).

sender_outbound_identity_empty_test() ->
    Sender = (outbound_sender())#{sender_business_identity_id => undefined},
    ?assertEqual({error, identity_required}, eb_message:validate_sender(Sender)).

sender_outbound_actor_empty_test() ->
    Sender = (outbound_sender())#{actor_user_id => undefined},
    ?assertEqual({error, actor_required}, eb_message:validate_sender(Sender)).

sender_unknown_type_test() ->
    ?assertEqual(
        {error, {unknown_sender_type, bot}},
        eb_message:validate_sender((outbound_sender())#{sender_type => bot})
    ).

sender_missing_type_test() ->
    ?assertEqual(
        {error, missing_sender_type},
        eb_message:validate_sender(#{sender_contact_id => 7})
    ).

sender_not_a_map_test() ->
    ?assertEqual({error, missing_sender_type}, eb_message:validate_sender(undefined)).

%% ===================================================================
%% eb_message 构造函数
%% ===================================================================

inbound_constructor_ok_test() ->
    {ok, Msg} = eb_message:inbound(#{
        organization_id => 1,
        workspace_id => 2,
        conversation_id => 3,
        message_id => 4,
        contact_id => 7,
        body_cipher => <<"cipher">>
    }),
    ?assertEqual(contact, maps:get(sender_type, Msg)),
    ?assertEqual(7, maps:get(sender_contact_id, Msg)),
    ?assertEqual(undefined, maps:get(sender_business_identity_id, Msg)),
    ?assertEqual(undefined, maps:get(actor_user_id, Msg)),
    %% 构造函数必须自带 XOR 自洽性。
    ?assertEqual(ok, eb_message:validate_sender(Msg)).

inbound_constructor_missing_contact_test() ->
    ?assertEqual(
        {error, contact_required},
        eb_message:inbound(#{
            organization_id => 1,
            workspace_id => 2,
            conversation_id => 3,
            message_id => 4
        })
    ).

inbound_constructor_not_a_map_test() ->
    ?assertEqual({error, invalid_params}, eb_message:inbound(<<"1">>)).

outbound_constructor_ok_test() ->
    {ok, Msg} = eb_message:outbound(#{
        organization_id => 1,
        workspace_id => 2,
        conversation_id => 3,
        message_id => 4,
        identity_id => 3,
        actor_user_id => 9,
        body_cipher => <<"cipher">>
    }),
    ?assertEqual(business_identity, maps:get(sender_type, Msg)),
    ?assertEqual(3, maps:get(sender_business_identity_id, Msg)),
    ?assertEqual(undefined, maps:get(sender_contact_id, Msg)),
    ?assertEqual(9, maps:get(actor_user_id, Msg)),
    ?assertEqual(ok, eb_message:validate_sender(Msg)).

outbound_constructor_missing_actor_test() ->
    ?assertEqual(
        {error, actor_required},
        eb_message:outbound(#{
            organization_id => 1,
            workspace_id => 2,
            conversation_id => 3,
            message_id => 4,
            identity_id => 3
        })
    ).

outbound_constructor_missing_identity_test() ->
    ?assertEqual(
        {error, identity_required},
        eb_message:outbound(#{
            organization_id => 1,
            workspace_id => 2,
            conversation_id => 3,
            message_id => 4,
            actor_user_id => 9
        })
    ).

outbound_constructor_not_a_map_test() ->
    ?assertEqual({error, invalid_params}, eb_message:outbound(42)).

%% ===================================================================
%% handover 不重写历史 message
%% ===================================================================

handover_preserves_sender_ok_test() ->
    {ok, In} = eb_message:inbound(inbound_params(4, 7)),
    {ok, Out} = eb_message:outbound(outbound_params(5, 3, 9)),
    ?assertEqual(ok, eb_message:handover_preserves_sender([In, Out], [In, Out])).

handover_preserves_sender_reordered_ok_test() ->
    %% 只比较集合语义，handover 不应改变任何 message 的 sender 指纹。
    {ok, In} = eb_message:inbound(inbound_params(4, 7)),
    {ok, Out} = eb_message:outbound(outbound_params(5, 3, 9)),
    ?assertEqual(ok, eb_message:handover_preserves_sender([In, Out], [Out, In])).

handover_preserves_sender_actor_rewritten_test() ->
    {ok, In} = eb_message:inbound(inbound_params(4, 7)),
    {ok, Out} = eb_message:outbound(outbound_params(5, 3, 9)),
    Rewritten = Out#{actor_user_id => 10},
    ?assertEqual(
        {error, sender_rewritten_by_handover},
        eb_message:handover_preserves_sender([In, Out], [In, Rewritten])
    ).

handover_preserves_sender_identity_rewritten_test() ->
    {ok, In} = eb_message:inbound(inbound_params(4, 7)),
    {ok, Out} = eb_message:outbound(outbound_params(5, 3, 9)),
    Rewritten = Out#{sender_business_identity_id => 33},
    ?assertEqual(
        {error, sender_rewritten_by_handover},
        eb_message:handover_preserves_sender([In, Out], [In, Rewritten])
    ).

handover_preserves_sender_message_dropped_test() ->
    {ok, In} = eb_message:inbound(inbound_params(4, 7)),
    {ok, Out} = eb_message:outbound(outbound_params(5, 3, 9)),
    ?assertEqual(
        {error, message_set_changed_by_handover},
        eb_message:handover_preserves_sender([In, Out], [In])
    ).

handover_preserves_sender_single_map_ok_test() ->
    {ok, Out} = eb_message:outbound(outbound_params(5, 3, 9)),
    ?assertEqual(ok, eb_message:handover_preserves_sender(Out, Out)).

handover_preserves_sender_invalid_input_test() ->
    ?assertEqual(
        {error, invalid_messages},
        eb_message:handover_preserves_sender([], not_a_list)
    ).

%% ===================================================================
%% delivery ACK 不改 canonical 真源（plan §2.1 #16）
%% ===================================================================

ack_preserves_canonical_ok_test() ->
    Canonical = canonical_message(),
    %% ACK 只写独立 delivery 行；canonical 行必须逐字不变。
    ?assertEqual(ok, eb_message:ack_preserves_canonical([Canonical], [Canonical])),
    ?assertEqual(ok, eb_message:ack_preserves_canonical(Canonical, Canonical)).

ack_preserves_canonical_visibility_tombstone_is_allowed_test() ->
    %% 客户端隐藏/未来撤回只改可见性，不属于 canonical 字段集 → 不算改真源。
    Canonical = canonical_message(),
    Tombstoned = Canonical#{visibility => tombstoned},
    ?assertEqual(ok, eb_message:ack_preserves_canonical(Canonical, Tombstoned)),
    ?assertNot(lists:member(visibility, eb_message:canonical_fields())).

ack_preserves_canonical_retain_until_mutation_rejected_test() ->
    Canonical = canonical_message(),
    Mutated = Canonical#{retain_until => 4_000},
    ?assertEqual(
        {error, {canonical_message_mutated_by_ack, retain_until}},
        eb_message:ack_preserves_canonical(Canonical, Mutated)
    ).

ack_preserves_canonical_cipher_hash_mutation_rejected_test() ->
    Canonical = canonical_message(),
    Mutated = Canonical#{body_cipher_hash => <<"h2">>},
    ?assertEqual(
        {error, {canonical_message_mutated_by_ack, body_cipher_hash}},
        eb_message:ack_preserves_canonical(Canonical, Mutated)
    ).

ack_preserves_canonical_scope_mutation_rejected_test() ->
    %% 改写 WorkspaceId 等于跨 Workspace 搬动 canonical 行 → 必须判红。
    Canonical = canonical_message(),
    Mutated = Canonical#{workspace_id => 99},
    ?assertEqual(
        {error, {canonical_message_mutated_by_ack, workspace_id}},
        eb_message:ack_preserves_canonical(Canonical, Mutated)
    ).

ack_preserves_canonical_sender_mutation_rejected_test() ->
    Canonical = canonical_message(),
    Mutated = Canonical#{sender_business_identity_id => 33},
    ?assertEqual(
        {error, {canonical_message_mutated_by_ack, sender_business_identity_id}},
        eb_message:ack_preserves_canonical(Canonical, Mutated)
    ).

ack_preserves_canonical_actor_mutation_rejected_test() ->
    Canonical = canonical_message(),
    Mutated = Canonical#{actor_user_id => 77},
    ?assertEqual(
        {error, {canonical_message_mutated_by_ack, actor_user_id}},
        eb_message:ack_preserves_canonical(Canonical, Mutated)
    ).

ack_preserves_canonical_row_dropped_rejected_test() ->
    %% ACK/客户端隐藏不得删除 canonical 行。
    ?assertEqual(
        {error, message_count_changed_by_ack},
        eb_message:ack_preserves_canonical([canonical_message()], [])
    ).

ack_preserves_canonical_invalid_input_test() ->
    ?assertEqual(
        {error, invalid_messages},
        eb_message:ack_preserves_canonical(undefined, [])
    ).

%% ===================================================================
%% eb_conversation:open/1 —— Org / Workspace 同源
%% ===================================================================

conversation_open_ok_test() ->
    {ok, Conv} = eb_conversation:open(conversation_params(1, 2, 1, 7, 3)),
    ?assertEqual(1, maps:get(organization_id, Conv)),
    ?assertEqual(2, maps:get(workspace_id, Conv)),
    ?assertEqual(7, maps:get(contact_id, Conv)),
    ?assertEqual(3, maps:get(business_identity_id, Conv)),
    ?assertEqual(100, maps:get(conversation_id, Conv)),
    ?assertEqual(100, maps:get(resource_id, Conv)),
    ?assertEqual(active, maps:get(status, Conv)),
    ?assertEqual(1, maps:get(version, Conv)).

conversation_open_cross_workspace_rejected_test() ->
    %% Workspace 属于另一个 Org → 硬拒绝（plan §2.1 #15）。
    ?assertEqual(
        {error, {workspace_org_mismatch, 1, 2}},
        eb_conversation:open(conversation_params(1, 2, 2, 7, 3))
    ).

conversation_open_missing_workspace_org_test() ->
    Params = maps:remove(workspace_organization_id, conversation_params(1, 2, 1, 7, 3)),
    ?assertEqual(
        {error, {missing_field, workspace_organization_id}},
        eb_conversation:open(Params)
    ).

conversation_open_missing_organization_id_test() ->
    %% 省略 organization_id 不得被默认成任何值。
    Params = maps:remove(organization_id, conversation_params(1, 2, 1, 7, 3)),
    ?assertEqual(
        {error, {missing_field, organization_id}},
        eb_conversation:open(Params)
    ).

conversation_open_missing_contact_test() ->
    Params = maps:remove(contact_id, conversation_params(1, 2, 1, 7, 3)),
    ?assertEqual({error, {missing_field, contact_id}}, eb_conversation:open(Params)).

conversation_open_missing_identity_test() ->
    Params = maps:remove(business_identity_id, conversation_params(1, 2, 1, 7, 3)),
    ?assertEqual(
        {error, {missing_field, business_identity_id}},
        eb_conversation:open(Params)
    ).

conversation_open_not_a_map_test() ->
    ?assertEqual({error, invalid_params}, eb_conversation:open([])).

%% ===================================================================
%% eb_conversation:handover/2 —— owner/resource id 不变
%% ===================================================================

conversation_handover_changes_identity_only_test() ->
    {ok, Conv} = eb_conversation:open(conversation_params(1, 2, 1, 7, 3)),
    {ok, New} = eb_conversation:handover(Conv, 4),
    ?assertEqual(4, maps:get(business_identity_id, New)),
    ?assertEqual(1, maps:get(organization_id, New)),
    ?assertEqual(2, maps:get(workspace_id, New)),
    ?assertEqual(7, maps:get(contact_id, New)),
    ?assertEqual(100, maps:get(conversation_id, New)),
    %% 资源 id 与 owner 在 handover 前后必须逐字不变。
    ?assertEqual(maps:get(resource_id, Conv), maps:get(resource_id, New)),
    ?assertEqual(100, maps:get(resource_id, New)),
    ?assertEqual(2, maps:get(version, New)).

conversation_handover_same_identity_rejected_test() ->
    {ok, Conv} = eb_conversation:open(conversation_params(1, 2, 1, 7, 3)),
    ?assertEqual({error, same_identity_handover}, eb_conversation:handover(Conv, 3)).

conversation_handover_invalid_identity_test() ->
    {ok, Conv} = eb_conversation:open(conversation_params(1, 2, 1, 7, 3)),
    ?assertEqual(
        {error, {invalid_identity, undefined}},
        eb_conversation:handover(Conv, undefined)
    ).

conversation_caretaker_test() ->
    {ok, Conv} = eb_conversation:open(conversation_params(1, 2, 1, 7, 3)),
    ?assertEqual({ok, 3}, eb_conversation:caretaker(Conv)).

conversation_caretaker_missing_test() ->
    ?assertEqual(
        {error, {missing_field, business_identity_id}},
        eb_conversation:caretaker(#{organization_id => 1})
    ).

%% ===================================================================
%% eb_consent:gate/2 —— fail-closed
%% ===================================================================

consent_gate_ok_test() ->
    ?assertEqual(ok, eb_consent:gate(consent(), <<"notice-v1">>)).

consent_gate_without_expected_version_ok_test() ->
    ?assertEqual(ok, eb_consent:gate(consent(), any)).

consent_gate_missing_consent_test() ->
    ?assertEqual({error, consent_required}, eb_consent:gate(undefined, <<"notice-v1">>)).

consent_gate_not_a_map_test() ->
    ?assertEqual({error, consent_required}, eb_consent:gate(<<"yes">>, <<"notice-v1">>)).

consent_gate_empty_notice_version_test() ->
    C = (consent())#{notice_version => <<>>},
    ?assertEqual({error, consent_required}, eb_consent:gate(C, <<"notice-v1">>)).

consent_gate_undefined_notice_version_test() ->
    C = (consent())#{notice_version => undefined},
    ?assertEqual({error, consent_required}, eb_consent:gate(C, <<"notice-v1">>)).

consent_gate_missing_consent_at_test() ->
    C = (consent())#{consent_at => undefined},
    ?assertEqual({error, consent_required}, eb_consent:gate(C, <<"notice-v1">>)).

consent_gate_missing_consent_subject_test() ->
    C = (consent())#{consent_subject => <<>>},
    ?assertEqual({error, consent_required}, eb_consent:gate(C, <<"notice-v1">>)).

consent_gate_version_mismatch_test() ->
    ?assertEqual(
        {error, {notice_version_mismatch, <<"notice-v2">>, <<"notice-v1">>}},
        eb_consent:gate(consent(), <<"notice-v2">>)
    ).

%% ===================================================================
%% eb_consent:classify/1 / is_synthetic/1 —— synthetic 绝不映射为 real
%% ===================================================================

consent_classify_synthetic_test() ->
    ?assertEqual(synthetic, eb_consent:classify(synthetic_consent())).

consent_classify_real_test() ->
    ?assertEqual(real, eb_consent:classify(#{kind => real})).

consent_classify_synthetic_flag_test() ->
    ?assertEqual(synthetic, eb_consent:classify(#{synthetic => true})).

consent_classify_unknown_test() ->
    ?assertEqual(unknown, eb_consent:classify(undefined)),
    ?assertEqual(unknown, eb_consent:classify(#{})).

consent_synthetic_never_classified_as_real_test() ->
    %% 本卡最关键的负例：synthetic fixture 不得被当作真实客户同意。
    ?assertNotEqual(real, eb_consent:classify(synthetic_consent())).

consent_synthetic_predicate_test() ->
    ?assert(eb_consent:is_synthetic(synthetic_consent())),
    ?assertNot(eb_consent:is_synthetic(#{kind => real})),
    ?assertNot(eb_consent:is_synthetic(undefined)).

consent_synthetic_predicate_alias_test() ->
    %% plan v4.1 §5.1 写作 `synthetic?/1`；Erlang 未加引号的 atom 不允许 '?'，
    %% 故主 API 为 is_synthetic/1，并保留同义字面别名 'synthetic?'/1 供合同核对。
    ?assertEqual(true, eb_consent:'synthetic?'(synthetic_consent())),
    ?assertEqual(false, eb_consent:'synthetic?'(undefined)),
    ?assertEqual(
        eb_consent:is_synthetic(synthetic_consent()),
        eb_consent:'synthetic?'(synthetic_consent())
    ),
    ?assertEqual(eb_consent:is_synthetic(undefined), eb_consent:'synthetic?'(undefined)).

consent_report_label_never_claims_compliance_test() ->
    %% synthetic 只能报告为本地合成，不得报告为真实同意/合规。
    Label = eb_consent:report_label(synthetic_consent()),
    ?assertEqual(synthetic_fixture_verified, Label),
    ?assertNotEqual(real_customer_consent, Label),
    ?assertNotEqual(legal_compliance, Label),
    %% 即便 kind=real（本地不可能产生），也必须留在外部 Gate 之后。
    ?assertEqual(requires_external_gate, eb_consent:report_label(#{kind => real})),
    ?assertEqual(requires_external_gate, eb_consent:report_label(undefined)).

%% ===================================================================
%% 辅助
%% ===================================================================

inbound_sender() ->
    #{
        sender_type => contact,
        sender_contact_id => 7,
        sender_business_identity_id => undefined,
        actor_user_id => undefined
    }.

outbound_sender() ->
    #{
        sender_type => business_identity,
        sender_contact_id => undefined,
        sender_business_identity_id => 3,
        actor_user_id => 9
    }.

inbound_params(MessageId, ContactId) ->
    #{
        organization_id => 1,
        workspace_id => 2,
        conversation_id => 3,
        message_id => MessageId,
        contact_id => ContactId,
        body_cipher => <<"cipher">>
    }.

outbound_params(MessageId, IdentityId, ActorUserId) ->
    #{
        organization_id => 1,
        workspace_id => 2,
        conversation_id => 3,
        message_id => MessageId,
        identity_id => IdentityId,
        actor_user_id => ActorUserId,
        body_cipher => <<"cipher">>
    }.

conversation_params(OrgId, WorkspaceId, WorkspaceOrgId, ContactId, IdentityId) ->
    #{
        organization_id => OrgId,
        workspace_id => WorkspaceId,
        workspace_organization_id => WorkspaceOrgId,
        contact_id => ContactId,
        business_identity_id => IdentityId,
        conversation_id => 100
    }.

consent() ->
    #{
        notice_version => <<"notice-v1">>,
        consent_at => 1_700_000_000,
        consent_subject => <<"enterprise_contact:7">>,
        kind => real
    }.

synthetic_consent() ->
    (consent())#{kind => synthetic}.

%% canonical enterprise_message（含 policy snapshot 与 retain_until）。
canonical_message() ->
    #{
        organization_id => 1,
        workspace_id => 2,
        conversation_id => 3,
        message_id => 5,
        client_msg_id => <<"c-5">>,
        body_cipher_hash => <<"h1">>,
        sender_type => business_identity,
        sender_contact_id => undefined,
        sender_business_identity_id => 3,
        actor_user_id => 9,
        policy_id => 7,
        policy_version => 1,
        retain_until => 5_000,
        visibility => visible
    }.
