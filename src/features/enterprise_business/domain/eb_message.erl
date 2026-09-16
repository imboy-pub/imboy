%%% @doc 企业消息（enterprise_message）sender 与 handover 的领域纯函数。
%%%
%%% 依据：plan v4.1 EB-D05、§2.1 #16/#18、§5.4。
%%%
%%% 纯净性（铁律 4）：无 I/O、无进程、无隐式时间/随机源。
%%%
%%% 冻结合同（sender XOR）：
%%%   * 客户入站：`sender_type = contact`，`sender_contact_id` 非空，
%%%     `sender_business_identity_id` 与 `actor_user_id` 必须为空；
%%%   * 员工出站：`sender_type = business_identity`，`sender_contact_id` 必须为空，
%%%     `sender_business_identity_id` 与 `actor_user_id` 必须非空；
%%%   * 不使用多态 `sender_id`；两个显式 nullable 复合 FK 恰好一个非空。
%%%
%%% handover 只允许改变 conversation 的当前经办 identity，**不得**重写任何历史
%%% message 的 sender/actor；`handover_preserves_sender/2` 是该不变量的纯函数判据。
-module(eb_message).

-export([
    validate_sender/1,
    inbound/1,
    outbound/1,
    handover_preserves_sender/2,
    ack_preserves_canonical/2,
    canonical_fields/0
]).

-type sender_type() :: contact | business_identity.
-type message() :: map().
-type params() :: map().

-export_type([sender_type/0, message/0]).

%% ===================================================================
%% sender XOR
%% ===================================================================

%% @doc 判定 message 的 sender 组合是否满足 EB-D05 的 XOR 合同。
%%
%% 违约 Reason 逐类可区分（便于审计与错误码映射）：
%%   {error, missing_sender_type}
%%   {error, {unknown_sender_type, Type}}
%%   {error, contact_required}                   入站缺 contact
%%   {error, identity_not_allowed_for_contact}   入站带 identity
%%   {error, actor_not_allowed_for_contact}      入站带 actor
%%   {error, contact_not_allowed_for_identity}   出站带 contact
%%   {error, identity_required}                  出站缺 identity
%%   {error, actor_required}                     出站缺 actor
-spec validate_sender(map()) -> ok | {error, term()}.
validate_sender(#{sender_type := contact} = Msg) ->
    validate_contact_sender(
        present(maps:get(sender_contact_id, Msg, undefined)),
        present(maps:get(sender_business_identity_id, Msg, undefined)),
        present(maps:get(actor_user_id, Msg, undefined))
    );
validate_sender(#{sender_type := business_identity} = Msg) ->
    validate_identity_sender(
        present(maps:get(sender_contact_id, Msg, undefined)),
        present(maps:get(sender_business_identity_id, Msg, undefined)),
        present(maps:get(actor_user_id, Msg, undefined))
    );
validate_sender(#{sender_type := Type}) ->
    {error, {unknown_sender_type, Type}};
validate_sender(_NotAMap) ->
    {error, missing_sender_type}.

validate_contact_sender(true, false, false) ->
    ok;
validate_contact_sender(false, _Identity, _Actor) ->
    {error, contact_required};
validate_contact_sender(true, true, _Actor) ->
    {error, identity_not_allowed_for_contact};
validate_contact_sender(true, false, true) ->
    {error, actor_not_allowed_for_contact}.

validate_identity_sender(false, true, true) ->
    ok;
validate_identity_sender(true, _Identity, _Actor) ->
    {error, contact_not_allowed_for_identity};
validate_identity_sender(false, false, _Actor) ->
    {error, identity_required};
validate_identity_sender(false, true, false) ->
    {error, actor_required}.

%% @doc 「非空」判据：undefined / <<>> / "" 一律视为空。
present(undefined) -> false;
present(<<>>) -> false;
present("") -> false;
present(_Value) -> true.

%% ===================================================================
%% 构造函数（自带 XOR 自洽）
%% ===================================================================

%% @doc 构造客户入站 message。
-spec inbound(params()) -> {ok, message()} | {error, term()}.
inbound(Params) when is_map(Params) ->
    Msg = #{
        organization_id => maps:get(organization_id, Params, undefined),
        workspace_id => maps:get(workspace_id, Params, undefined),
        conversation_id => maps:get(conversation_id, Params, undefined),
        message_id => maps:get(message_id, Params, undefined),
        client_msg_id => maps:get(client_msg_id, Params, undefined),
        body_cipher => maps:get(body_cipher, Params, undefined),
        sender_type => contact,
        sender_contact_id => maps:get(contact_id, Params, undefined),
        sender_business_identity_id => undefined,
        actor_user_id => undefined
    },
    case validate_sender(Msg) of
        ok -> {ok, Msg};
        {error, _} = Err -> Err
    end;
inbound(_NotAMap) ->
    {error, invalid_params}.

%% @doc 构造员工出站 message。
-spec outbound(params()) -> {ok, message()} | {error, term()}.
outbound(Params) when is_map(Params) ->
    Msg = #{
        organization_id => maps:get(organization_id, Params, undefined),
        workspace_id => maps:get(workspace_id, Params, undefined),
        conversation_id => maps:get(conversation_id, Params, undefined),
        message_id => maps:get(message_id, Params, undefined),
        client_msg_id => maps:get(client_msg_id, Params, undefined),
        body_cipher => maps:get(body_cipher, Params, undefined),
        sender_type => business_identity,
        sender_contact_id => undefined,
        sender_business_identity_id => maps:get(identity_id, Params, undefined),
        actor_user_id => maps:get(actor_user_id, Params, undefined)
    },
    case validate_sender(Msg) of
        ok -> {ok, Msg};
        {error, _} = Err -> Err
    end;
outbound(_NotAMap) ->
    {error, invalid_params}.

%% ===================================================================
%% handover 不重写历史 message
%% ===================================================================

%% @doc 断言 handover 前后 message 集合的 sender/actor 指纹逐字不变。
%%
%% 指纹 = `{message_id, sender_type, sender_contact_id,
%%          sender_business_identity_id, actor_user_id}`；
%% 以集合语义比较，故 handover 导致的重排不算改写，但任何字段改写、
%% 条数变化（丢弃/新增）都会被抓到。
-spec handover_preserves_sender(term(), term()) -> ok | {error, term()}.
handover_preserves_sender(Old, New) when is_list(Old), is_list(New) ->
    compare_fingerprints(sender_fingerprints(Old), sender_fingerprints(New));
handover_preserves_sender(Old, New) when is_map(Old), is_map(New) ->
    compare_fingerprints([sender_fingerprint(Old)], [sender_fingerprint(New)]);
handover_preserves_sender(_Old, _New) ->
    {error, invalid_messages}.

sender_fingerprints(Messages) ->
    lists:usort([sender_fingerprint(M) || M <- Messages]).

sender_fingerprint(Msg) ->
    {
        maps:get(message_id, Msg, undefined),
        maps:get(sender_type, Msg, undefined),
        maps:get(sender_contact_id, Msg, undefined),
        maps:get(sender_business_identity_id, Msg, undefined),
        maps:get(actor_user_id, Msg, undefined)
    }.

compare_fingerprints(Old, New) when length(Old) =/= length(New) ->
    {error, message_set_changed_by_handover};
compare_fingerprints(Old, New) when Old =:= New ->
    ok;
compare_fingerprints(_Old, _New) ->
    {error, sender_rewritten_by_handover}.

%% ===================================================================
%% delivery ACK 不改 canonical 真源
%% ===================================================================

%% @doc canonical message 的字段集合（plan §2.1 #16 的逐字口径）。
%%
%% 注意 `visibility` **不在**此列：客户端隐藏/未来撤回只改可见性，
%% 属于允许的操作；而 OrgId/Workspace/发送者/actor/密文 hash/policy
%% snapshot/`retain_until` 一旦接受即不可被 ACK 改写。
-spec canonical_fields() -> [atom()].
canonical_fields() ->
    [
        organization_id,
        workspace_id,
        conversation_id,
        message_id,
        client_msg_id,
        body_cipher_hash,
        sender_type,
        sender_contact_id,
        sender_business_identity_id,
        actor_user_id,
        policy_id,
        policy_version,
        retain_until
    ].

%% @doc 断言 delivery ACK 前后 canonical message 真源逐字不变。
%%
%% ACK 只写独立的 `enterprise_message_delivery`（投递状态，可独立压缩/清理），
%% 不得 `DELETE`/`UPDATE` canonical 行，也不得改变行数、密文 hash、scope、
%% 发送者/actor、policy snapshot 或 `retain_until`。
%%
%% 失败：
%%   {error, {canonical_message_mutated_by_ack, Field}}
%%   {error, message_count_changed_by_ack}
%%   {error, message_set_changed_by_ack}
-spec ack_preserves_canonical(term(), term()) -> ok | {error, term()}.
ack_preserves_canonical(Before, After) when is_list(Before), is_list(After) ->
    compare_canonical(canonical_fingerprints(Before), canonical_fingerprints(After));
ack_preserves_canonical(Before, After) when is_map(Before), is_map(After) ->
    compare_canonical([canonical_fingerprint(Before)], [canonical_fingerprint(After)]);
ack_preserves_canonical(_Before, _After) ->
    {error, invalid_messages}.

canonical_fingerprints(Messages) ->
    lists:usort([canonical_fingerprint(M) || M <- Messages]).

canonical_fingerprint(Msg) ->
    {
        maps:get(message_id, Msg, undefined),
        maps:from_list([{Field, maps:get(Field, Msg, undefined)} || Field <- canonical_fields()])
    }.

compare_canonical(Before, After) when Before =:= After ->
    ok;
compare_canonical(Before, After) ->
    case length(Before) =:= length(After) of
        false -> {error, message_count_changed_by_ack};
        true -> first_canonical_difference(lists:sort(Before), lists:sort(After))
    end.

first_canonical_difference([{MessageId, BeforeFields} | RestBefore], [
    {MessageId, AfterFields} | RestAfter
]) ->
    case first_field_difference(canonical_fields(), BeforeFields, AfterFields) of
        ok -> first_canonical_difference(RestBefore, RestAfter);
        {error, _} = Err -> Err
    end;
first_canonical_difference(_Before, _After) ->
    {error, message_set_changed_by_ack}.

first_field_difference([], _BeforeFields, _AfterFields) ->
    ok;
first_field_difference([Field | Rest], BeforeFields, AfterFields) ->
    case maps:get(Field, BeforeFields) =:= maps:get(Field, AfterFields) of
        true -> first_field_difference(Rest, BeforeFields, AfterFields);
        false -> {error, {canonical_message_mutated_by_ack, Field}}
    end.
