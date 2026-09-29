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
%%   persist_hook      可选；REVIEW-3 F-2 事务内旁路写钩子（如客服
%%                     `message.appended` 事件行并轨写）。`fun((Conn, Stored) ->
%%                     ok | {error, Reason})`：非重放路径在消息+附件+审计写毕、
%%                     事务仍开放时调用；返回 `{error, Reason}` ⇒ 整体回滚，
%%                     调用方拿到 `{error, Reason}`（Reason 原样上浮）。缺省
%%                     undefined = 无钩子。
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
        %% REVIEW-3 F-2：persist_hook 失败的专用回滚信号（run_persist_hook 抛
        %% `{persist_hook_failed, HookReason}`，epgsql reraise=false 对其单层
        %% 包裹为 `{rollback, _}`）——在此解包回原始 HookReason，调用方拿到的
        %% 错误形状与钩子自身返回逐字一致（如 `{audit_append_failed, _}`）。
        {rollback, {persist_hook_failed, HookReason}} -> {error, HookReason};
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

accept_in_conversation(Conn, OrgId, WorkspaceId, Conversation, Params0) ->
    case sender_attribution_gate(Conversation, Params0) of
        {error, _} = Err ->
            Err;
        {ok, Params} ->
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
            end
    end.

%% F-SEC-01：发送者归属权威校验。canonical tx 是唯一同时握有「会话事实 +
%% 全部 sender 声明」的位置，两类声明在此锚定：
%%   * business_identity：HTTP 租户面由 handler 注入 caller_identity_id（认证
%%     事实派生，客户端不可报）——自报 identity_id 与之不符即
%%     `{sender_identity_unauthorized, _, _}`（403），不写 canonical 真源；
%%     无 caller_identity_id 的内部/测试显式注入合同原样放行（domain XOR 与
%%     DB 复合 FK 仍兜底）。
%%   * contact：contact_id 必须等于会话绑定的 contact——防持有读权限的成员
%%     指认本 Org 任意 contact 伪造入站（`{sender_contact_mismatch, _, _}`，
%%     403）。CS 访客入站的 contact 与会话天然一致，不受影响。
sender_attribution_gate(Conversation, Params) ->
    case maps:get(sender_type, Params, undefined) of
        business_identity ->
            authorize_business_sender(Params);
        contact ->
            authorize_contact_sender(Conversation, Params);
        _ ->
            %% 未知 sender_type 交给 domain validate_sender 报错，不在重复判。
            {ok, Params}
    end.

authorize_business_sender(Params) ->
    case maps:get(caller_identity_id, Params, undefined) of
        undefined ->
            {ok, Params};
        CallerId ->
            case maps:get(identity_id, Params, undefined) of
                Requested when Requested =:= undefined; Requested =:= CallerId ->
                    %% `=>`：客户端可省略 identity_id，由认证事实补齐。
                    {ok, Params#{identity_id => CallerId}};
                Requested ->
                    {error, {sender_identity_unauthorized, Requested, CallerId}}
            end
    end.

authorize_contact_sender(Conversation, Params) ->
    case maps:get(contact_id, Params, undefined) of
        undefined ->
            %% 缺 contact 是形状错误，交 domain validate_sender 报 contact_required。
            {ok, Params};
        ContactId ->
            ConversationContact = maps:get(contact_id, Conversation, undefined),
            case is_integer(ContactId) andalso ContactId =:= ConversationContact of
                true ->
                    {ok, Params};
                false ->
                    {error, {sender_contact_mismatch, ContactId, ConversationContact}}
            end
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
                    %% 重放：不追加第二条接受审计（§2.1 #18），也不触发
                    %% persist_hook——原事务提交时消息与钩子写入（如客服事件
                    %% 行）已原子落库，重放再写会产生重复事件帧。asset_ids
                    %% 奇偶校验（BE-PATCH-01，attachment-state-machine 幂等
                    %% 口径）：同一 client_msg_id + 同一 asset_ids 重放 =
                    %% 同一 message；其他重放 = 409 conflict。
                    case asset_replay_gate(Conn, OrgId, WorkspaceId, Stored, Params) of
                        ok ->
                            replay_result(Conn, OrgId, WorkspaceId, Stored, Sealed);
                        {error, _} = Err ->
                            Err
                    end;
                false ->
                    append_accept_audit(
                        Conn, OrgId, WorkspaceId, Stored, Params, AcceptedAt, Sealed
                    )
            end;
        {error, _} = Err ->
            Err
    end.

%% CS-BE-01：重放回显同样带资产白名单投影（与首发同一形状——同一 client_msg_id
%% 重放返回同一 message，含 assets）。
replay_result(Conn, OrgId, WorkspaceId, Stored, Sealed) ->
    case message_with_assets(Conn, OrgId, WorkspaceId, Stored) of
        {error, _} = Err ->
            Err;
        WithAssets ->
            {ok, #{
                message => WithAssets,
                audit_id => undefined,
                replayed => true,
                sealed => Sealed
            }}
    end.

%% ===================================================================
%% 消息-附件事务绑定（BE-PATCH-01，attachment-state-machine append_message）
%% ===================================================================

%% 绑定发生在接受审计**之前**、同一事务内：任一资产校验/绑定失败 → 整体回滚
%% （不产生半提交、不产生「无附件的文本消息」假成功）。
append_accept_audit(Conn, OrgId, WorkspaceId, Stored, Params, AcceptedAt, Sealed) ->
    case bind_assets(Conn, OrgId, WorkspaceId, Stored, Params) of
        ok ->
            do_append_accept_audit(Conn, OrgId, WorkspaceId, Stored, Params, AcceptedAt, Sealed);
        {error, _} = Err ->
            Err
    end.

do_append_accept_audit(Conn, OrgId, WorkspaceId, Stored, Params, AcceptedAt, Sealed) ->
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
            case run_persist_hook(Conn, OrgId, WorkspaceId, Stored, Params) of
                ok ->
                    accept_result(Conn, OrgId, WorkspaceId, Stored, AuditId, Sealed);
                {error, _} = HookErr ->
                    %% 不可达兜底（run_persist_hook 以 throw 裁决回滚），保类型诚实。
                    HookErr
            end;
        {error, _} = Err ->
            Err
    end.

%% REVIEW-3 F-2：事务内旁路写钩子（如客服 message.appended 事件行并轨写）。
%% 在消息+附件绑定+接受审计写毕、事务仍开放时执行；写失败 ⇒ `throw(
%% {persist_hook_failed, HookReason})`，由 with_tx 的 catch 裁决 ROLLBACK。
%% **不得**改成以 `{error, _}` 正常返回结束：epgsql:with_transaction 只对
%% 异常回滚，正常返回一律 COMMIT，那会留下"消息已提交、钩子未写"的半提交，
%% 恰是 F-2 要消灭的形状。
%%
%% 标签刻意**不**用 `{rollback, _}` 形：本路径 with_tx 是 reraise=false，
%% epgsql 会把抛出的 Reason 再包一层 `{rollback, _}`（对 `{rollback, X}` 形
%% throw 实测得 `{rollback, {rollback, X}}`）；用专用标签让 accept_message
%% 单点解包回原始 HookReason。
run_persist_hook(Conn, _OrgId, _WorkspaceId, Stored, Params) ->
    case maps:get(persist_hook, Params, undefined) of
        undefined ->
            ok;
        Hook when is_function(Hook, 2) ->
            case Hook(Conn, Stored) of
                ok ->
                    ok;
                {error, HookReason} ->
                    throw({persist_hook_failed, HookReason})
            end
    end.

%% CS-BE-01：POST 回显补资产白名单投影——发送后立即读回绑定资产，与消息写入/
%% 绑定/审计**同一事务**（本读不走独立连接，回滚即整体回滚）。读失败 = 事务
%% 失败（fail-closed：绝不返回「无附件」的假成功回显）。投影列集与历史读面
%% 逐字同款（冻结契约 {id,mime,size_bytes,file_name,status}）。
accept_result(Conn, OrgId, WorkspaceId, Stored, AuditId, Sealed) ->
    case message_with_assets(Conn, OrgId, WorkspaceId, Stored) of
        {error, _} = Err ->
            Err;
        WithAssets ->
            {ok, #{
                message => WithAssets,
                audit_id => AuditId,
                replayed => false,
                sealed => Sealed
            }}
    end.

message_with_assets(Conn, OrgId, WorkspaceId, Stored) ->
    case eb_pg_store:message_assets_in(Conn, OrgId, WorkspaceId, maps:get(id, Stored)) of
        {ok, Assets} ->
            Stored#{assets => Assets};
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

%% 请求的 asset_ids（已由 eb_message_app 归一校验：pos int 且去重；缺省 = 空集）。
requested_asset_ids(Params) ->
    case maps:get(asset_ids, Params, undefined) of
        Ids when is_list(Ids) -> Ids;
        _Other -> []
    end.

%% 重放奇偶校验：请求 asset_ids 与该消息已绑定资产集合逐字相等 ⇒ 同一 message
%% 重放（幂等成功）；不等 ⇒ 其他重放（409 conflict）。
asset_replay_gate(Conn, OrgId, WorkspaceId, Stored, Params) ->
    case eb_pg_store:message_asset_ids_in(Conn, OrgId, WorkspaceId, maps:get(id, Stored)) of
        {error, _} = Err ->
            Err;
        {ok, BoundIds} ->
            case lists:sort(BoundIds) =:= lists:sort(requested_asset_ids(Params)) of
                true -> ok;
                false -> {error, conflict}
            end
    end.

%% 绑定主流程：FOR UPDATE 锁行 → 逐行校验 → 逐条写 message_id。任一步失败
%% 即返回错误，由外层 with_tx 整体回滚（attachment-state-machine：
%% 「单数据库事务内锁定 active+unbound asset … 任一步失败全部回滚」）。
bind_assets(Conn, OrgId, WorkspaceId, Stored, Params) ->
    AssetIds = requested_asset_ids(Params),
    case AssetIds =:= [] of
        true ->
            ok;
        false ->
            RetainUntilSec = maps:get(retain_until, Stored, undefined),
            case is_integer(RetainUntilSec) of
                false ->
                    %% 消息无保留期快照 ⇒ 附件无从继承（迁移 118 触发器同样拒绝）：
                    %% fail-closed，不默认任何值。
                    {error, {invalid_argument, asset_retain_until}};
                true ->
                    ConversationId = maps:get(conversation_id, Stored),
                    case eb_pg_store:lock_assets_in(Conn, OrgId, WorkspaceId, AssetIds) of
                        {error, _} = Err ->
                            Err;
                        {ok, Rows} ->
                            bind_validated(
                                Conn,
                                OrgId,
                                WorkspaceId,
                                Stored,
                                AssetIds,
                                Rows,
                                ConversationId,
                                RetainUntilSec
                            )
                    end
            end
    end.

%% 存在性/作用域不泄露：缺行（含跨租户/不存在）与跨会话同为 not_found。
bind_validated(Conn, OrgId, WorkspaceId, Stored, AssetIds, Rows, ConversationId, RetainUntilSec) ->
    LockedIds = [maps:get(id, Row) || Row <- Rows],
    Missing = [Id || Id <- AssetIds, not lists:member(Id, LockedIds)],
    case Missing of
        [_ | _] ->
            {error, not_found};
        [] ->
            case lists:all(fun(Row) -> bindable(Row, Stored, ConversationId) end, Rows) of
                false ->
                    {error, not_found};
                true ->
                    bind_each(
                        Conn, OrgId, WorkspaceId, AssetIds, maps:get(id, Stored), RetainUntilSec
                    )
            end
    end.

%% 逐行可绑校验（attachment-state-machine：Org/Workspace 由语句作用域保证；
%% 这里裁决 unbound / 会话一致 / 上传人主体一致；retain_until 由 SQL
%% GREATEST + 迁移 118 触发器裁决）。
bindable(Row, Stored, ConversationId) ->
    maps:get(status, Row) =:= active andalso
        maps:get(message_id, Row, undefined) =:= undefined andalso
        maps:get(conversation_id, Row, undefined) =:= ConversationId andalso
        uploader_matches(Row, Stored).

%% 上传人主体一致：访客消息只绑「contact 上传」（uploaded_by_user_id 为空，
%% 访客凭证不落 user 列——上传人在上传链路由会话归属门裁决）；成员消息只绑
%% 本人上传的资产。不符 ⇒ not_found（不区分「不存在」与「不属于你」）。
uploader_matches(Row, Stored) ->
    case maps:get(sender_contact_id, Stored, undefined) of
        ContactId when is_integer(ContactId) ->
            maps:get(uploaded_by_user_id, Row, undefined) =:= undefined;
        _Other ->
            maps:get(uploaded_by_user_id, Row, undefined) =:=
                maps:get(actor_user_id, Stored, undefined)
    end.

bind_each(_Conn, _OrgId, _WorkspaceId, [], _MessageId, _RetainUntilSec) ->
    ok;
bind_each(Conn, OrgId, WorkspaceId, [AssetId | Rest], MessageId, RetainUntilSec) ->
    case
        eb_pg_store:bind_asset_message_in(
            Conn, OrgId, WorkspaceId, AssetId, MessageId, RetainUntilSec
        )
    of
        ok -> bind_each(Conn, OrgId, WorkspaceId, Rest, MessageId, RetainUntilSec);
        {error, _} = Err -> Err
    end.

%% ===================================================================
%% sender / seal（候选构造与加密封装）
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
