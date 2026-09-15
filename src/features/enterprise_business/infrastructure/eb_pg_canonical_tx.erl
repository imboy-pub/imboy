%%% @doc 企业 canonical 消息接受的事务编排（message + policy snapshot + audit 同事务）。
%%%
%%% 依据：plan v4.1 EB-D05、§2.1 #9/#16/#18、§4.1、§4.3；EB-03 §3.1/§3.3（A03/A05）。
%%%
%%% 「服务端接受」的语义（§2.1 #18）：canonical message、固化到消息上的 policy snapshot
%%% 与 `message.accept` 审计**必须在同一事务内提交**；任一环失败一律整体回滚——
%%% 不得返回服务端接受成功、不得留下半提交（见套件 a03_atomic_tx_has_no_half_commit）。
%%%
%%% 幂等（§2.1 #18 末句）：同一 `client_msg_id` 重放只产生一条消息与一条接受审计。
%%% 重放判定由 DB 唯一键 `(organization_id, conversation_id, client_msg_id)` 裁决
%%% （store 的 `ON CONFLICT DO NOTHING` + 回读），因此并发重放也不会双写。
%%%
%%% sender/actor 合同：先由 `eb_message:validate_sender/1` 判定（domain 是唯一真源），
%%% 再由两个显式 nullable 复合 FK（`sender_contact_id` / `sender_business_identity_id`）
%%% 在 DB 层兜底：跨 Org 的 sender 即使过了应用层，也会被 23503 拒绝。
%%%
%%% 时间：`accepted_at` 由调用方注入（默认取注入时钟端口 `eb_system_clock`）；
%%% 本模块不读系统时间，也不读全局配置（密钥经 `key_ref` 传入）。
-module(eb_pg_canonical_tx).

-export([accept_message/3, audit_detail_keys/0]).

-define(DEFAULT_AUDIT_ACTION, <<"message.accept">>).
-define(DATA_CLASS, <<"enterprise_message">>).

%% @doc 接受一条企业消息（同事务：message + policy snapshot + audit）。
%%
%% Params（原子键 map）：
%%   conversation_id   必填；消息所属会话
%%   client_msg_id     必填；调用方幂等键（同一会话内唯一）
%%   body              必填；明文正文（本事务内加密封装，绝不落库）
%%   sender_type       必填；`contact` | `business_identity`（二进制或原子）
%%   contact_id        入站必填；出站必须缺省
%%   identity_id       出站必填；入站必须缺省
%%   actor_user_id     出站必填；入站必须缺省
%%   key_ref           必填；企业托管主密钥引用（装配层解析）
%%   accepted_at       可选；注入时钟（Unix 秒），缺省取 `eb_system_clock:now/0`
%%   message_id        可选；缺省由注入 ID 端口生成
%%   audit_action      可选；审计 action 名（缺省 `message.accept`）
%%   enforce_consent   可选；默认 true（无合成 consent 一律 fail-closed，§2.1 #9）
%%
%% 返回 `{ok, #{message, audit_id, replayed, sealed}}` 或 `{error, Reason}`。
-spec accept_message(integer(), integer(), map()) -> {ok, map()} | {error, term()}.
accept_message(OrgId, WorkspaceId, Params) when
    is_integer(OrgId), is_integer(WorkspaceId), is_map(Params)
->
    case
        elib_pg:with_tx(
            fun(Conn) -> do_accept(Conn, OrgId, WorkspaceId, Params) end,
            [{reraise, false}]
        )
    of
        {rollback, Reason} -> {error, Reason};
        Result -> Result
    end;
accept_message(OrgId, WorkspaceId, _Params) ->
    {error, {invalid_tenant, {OrgId, WorkspaceId}}}.

do_accept(Conn, OrgId, WorkspaceId, Params) ->
    ConversationId = maps:get(conversation_id, Params, undefined),
    case eb_pg_store:fetch_conversation_in(Conn, OrgId, WorkspaceId, ConversationId) of
        {ok, Conversation} ->
            accept_in_conversation(Conn, OrgId, WorkspaceId, Conversation, Params);
        {error, not_found} ->
            {error, not_found};
        {error, _} = Err ->
            Err
    end.

accept_in_conversation(Conn, OrgId, WorkspaceId, Conversation, Params) ->
    case consent_gate(Conversation, Params) of
        ok ->
            case latest_policy(Conn, OrgId, WorkspaceId) of
                {ok, Policy} ->
                    accept_with_policy(Conn, OrgId, WorkspaceId, Policy, Params);
                {error, not_found} ->
                    {error, missing_retention_policy};
                {error, _} = Err ->
                    Err
            end;
        {error, _} = Err ->
            Err
    end.

consent_gate(Conversation, Params) ->
    case maps:get(enforce_consent, Params, true) of
        false ->
            ok;
        true ->
            Consent = #{
                notice_version => maps:get(notice_version, Conversation, undefined),
                consent_at => maps:get(consent_at, Conversation, undefined),
                consent_subject => maps:get(consent_subject, Conversation, undefined)
            },
            eb_consent:gate(Consent, any)
    end.

%% 策略版本由 store 在同一事务内读取（SQL 全部集中在 eb_pg_store，避免两处真源）。
latest_policy(Conn, OrgId, WorkspaceId) ->
    case eb_pg_store:latest_policy_in(Conn, OrgId, WorkspaceId, ?DATA_CLASS) of
        {ok, Policy} ->
            {ok, #{
                policy_id => maps:get(id, Policy),
                policy_version => maps:get(version, Policy),
                retention_days => maps:get(retention_days, Policy)
            }};
        {error, _} = Err ->
            Err
    end.

accept_with_policy(Conn, OrgId, WorkspaceId, Policy, Params) ->
    Clock = eb_system_clock:now(),
    AcceptedAt = maps:get(accepted_at, Params, Clock),
    case eb_retention:policy_snapshot(Policy, AcceptedAt, Clock) of
        {ok, Snapshot} ->
            build_and_seal(Conn, OrgId, WorkspaceId, Policy, Snapshot, Params, AcceptedAt);
        {error, _} = Err ->
            Err
    end.

build_and_seal(Conn, OrgId, WorkspaceId, Policy, Snapshot, Params, AcceptedAt) ->
    ConversationId = maps:get(conversation_id, Params),
    MessageId = maps:get(message_id, Params, eb_tsid:new_id(enterprise_message)),
    Body = maps:get(body, Params, undefined),
    Candidate = sender_candidate(Params),
    case eb_message:validate_sender(Candidate) of
        ok ->
            case seal_body(OrgId, WorkspaceId, ConversationId, MessageId, Body, Params) of
                {ok, Sealed} ->
                    Message =
                        Candidate#{
                            id => MessageId,
                            conversation_id => ConversationId,
                            client_msg_id => maps:get(client_msg_id, Params, undefined),
                            body_cipher => maps:get(cipher, Sealed),
                            key_version => maps:get(key_version, Sealed),
                            aad_hash => maps:get(aad_hash, Sealed),
                            content_hash => sha256_hex(maps:get(cipher, Sealed)),
                            policy_id => maps:get(policy_id, Policy),
                            policy_version => maps:get(policy_version, Policy),
                            retention_days => maps:get(retention_days, Snapshot),
                            retain_until => maps:get(retain_until, Snapshot)
                        },
                    persist(
                        Conn, OrgId, WorkspaceId, Message, Snapshot, Params, AcceptedAt, Sealed
                    );
                {error, _} = Err ->
                    Err
            end;
        {error, _} = Err ->
            Err
    end.

persist(Conn, OrgId, WorkspaceId, Message, _Snapshot, Params, AcceptedAt, Sealed) ->
    case eb_pg_store:append_message_in(Conn, OrgId, WorkspaceId, Message) of
        {ok, Stored} ->
            case maps:get(replayed, Stored, false) of
                true ->
                    %% 重放：不追加第二条接受审计（§2.1 #18）
                    {ok, #{
                        message => Stored,
                        audit_id => undefined,
                        replayed => true,
                        sealed => Sealed
                    }};
                false ->
                    append_accept_audit(
                        Conn, OrgId, WorkspaceId, Stored, Params, AcceptedAt, Sealed
                    )
            end;
        {error, _} = Err ->
            Err
    end.

append_accept_audit(Conn, OrgId, WorkspaceId, Stored, Params, AcceptedAt, Sealed) ->
    Action = maps:get(audit_action, Params, ?DEFAULT_AUDIT_ACTION),
    Event = #{
        id => eb_tsid:new_id(enterprise_audit),
        resource_type => <<"enterprise_message">>,
        resource_id => maps:get(id, Stored),
        action => Action,
        business_identity_id => maps:get(sender_business_identity_id, Stored, undefined),
        actor_user_id => maps:get(actor_user_id, Stored, undefined),
        actor_role => maps:get(actor_role, Params, undefined),
        detail => audit_detail(OrgId, WorkspaceId, Stored, AcceptedAt, Sealed)
    },
    case eb_pg_audit:append_in(Conn, OrgId, Event) of
        {ok, AuditId} ->
            {ok, #{
                message => Stored,
                audit_id => AuditId,
                replayed => false,
                sealed => Sealed
            }};
        {error, _} = Err ->
            Err
    end.

%% 审计 detail 只放**结构化摘要**：无正文、无密文原文、无密钥、无 Authorization。
%% 白名单见 audit_detail_keys/0（静态可核对）。
-spec audit_detail_keys() -> [atom()].
audit_detail_keys() ->
    [
        <<"workspace_id">>,
        <<"conversation_id">>,
        <<"client_msg_id">>,
        <<"sender_type">>,
        <<"policy_id">>,
        <<"policy_version">>,
        <<"retention_days">>,
        <<"retain_until">>,
        <<"accepted_at">>,
        <<"aad_hash">>,
        <<"content_hash">>,
        <<"key_version">>
    ].

audit_detail(_OrgId, WorkspaceId, Stored, AcceptedAt, Sealed) ->
    #{
        <<"workspace_id">> => WorkspaceId,
        <<"conversation_id">> => maps:get(conversation_id, Stored),
        <<"client_msg_id">> => maps:get(client_msg_id, Stored),
        <<"sender_type">> => atom_to_binary(maps:get(sender_type, Stored), utf8),
        <<"policy_id">> => maps:get(policy_id, Stored),
        <<"policy_version">> => maps:get(policy_version, Stored),
        <<"retention_days">> => maps:get(retention_days, Stored),
        <<"retain_until">> => maps:get(retain_until, Stored),
        <<"accepted_at">> => AcceptedAt,
        <<"aad_hash">> => maps:get(aad_hash, Sealed),
        <<"content_hash">> => maps:get(content_hash, Stored),
        <<"key_version">> => maps:get(key_version, Sealed)
    }.

%% ===================================================================
%% sender / seal
%% ===================================================================

%% 构造 domain 判定所需的候选 map：显式两个 sender 列，绝不使用多态 sender_id。
sender_candidate(Params) ->
    #{
        sender_type => sender_type_atom(maps:get(sender_type, Params, undefined)),
        sender_contact_id => maps:get(contact_id, Params, undefined),
        sender_business_identity_id => maps:get(identity_id, Params, undefined),
        actor_user_id => maps:get(actor_user_id, Params, undefined)
    }.

%% 只识别两个受控二进制值；其余原样交给 domain 判定为 unknown_sender_type
%% （刻意不做 binary_to_atom，避免用外部输入制造原子）。
sender_type_atom(<<"contact">>) -> contact;
sender_type_atom(<<"business_identity">>) -> business_identity;
sender_type_atom(Other) -> Other.

seal_body(OrgId, WorkspaceId, ConversationId, MessageId, Body, Params) when is_binary(Body) ->
    Aad = #{
        organization_id => OrgId,
        workspace_id => WorkspaceId,
        conversation_id => ConversationId,
        message_id => MessageId
    },
    case eb_managed_crypto:seal(Aad, Body, maps:get(key_ref, Params, undefined)) of
        {ok, Sealed} ->
            {ok, Sealed#{aad => Aad}};
        {error, Reason} ->
            {error, {seal_failed, Reason}}
    end;
seal_body(_OrgId, _WorkspaceId, _ConversationId, _MessageId, _Body, _Params) ->
    {error, invalid_body}.

sha256_hex(Bin) ->
    binary:encode_hex(crypto:hash(sha256, Bin), lowercase).
