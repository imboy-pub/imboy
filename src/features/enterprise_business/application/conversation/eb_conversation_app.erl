%%% @doc 企业会话（enterprise conversation）的应用层用例：建立会话与经办交接。
%%%
%%% 依据：plan v4.1 EB-D01/D03/D05、§2.1 #9/#15、§8 EB-06；作业书 §4 A02/A04、§5。
%%%
%%% ## 建立会话（`open_conversation/2`）
%%%
%%%   1. **服务端解析默认 Workspace**：`workspace_id` 必须等于服务端解析出的该 Org
%%%      默认 Workspace。事实源是 EB-03R 的**最小只读事实 Port**
%%%      `eb_member_fact_port:default_workspace/2`（经 `eb_infra_ports:resolve(member_fact)`
%%%      装配），调用方身份取 `member_user_id`（缺省 `actor_user_id`）；事实源缺失或
%%%      成员无默认 Workspace 一律 fail-closed（**不**隐式推断、**不**任选一个）。
%%%      解析结果再经 **BC-19 `workspace_resolver:resolve_workspace/1`** 复核，确认它是
%%%      一个真实存在的 workspace 资源（`personal` / 解析失败一律拒绝）。调用方无法
%%%      任意指定或切换 Workspace，也无法自报 `workspace_organization_id`（派生字段由
%%%      服务端解析）。
%%%   2. **同 Org 校验**：用 store 的 `list_assignments/2`（同语句带 Org + Workspace）
%%%      证明该 Workspace 属于该 Org，再把已验证事实作为 domain 的
%%%      `workspace_organization_id` 传入 `eb_conversation:open/1`；跨 Org 一律拒绝。
%%%   3. **合成 consent**：consent 三列只能来自 `eb_consent_app:synthetic_fields/2`，
%%%      写入前经 `eb_consent:gate/2` 自检。真实告知文本属人工 Gate（plan §9），
%%%      本卡只用合成版本验证状态机。
%%%   4. 企业会话**只写 enterprise 表**，绝不写个人 `conversation` / `msg_c2c` /
%%%      `msg_store` / `user_friend`。
%%%
%%% ## 经办交接（`handover_identity/2`）
%%%
%%% 交接走 `eb_store_port:update_conversation_assignee/4`（EB-03R P5 交付的 CAS 形状：
%%% 只改当前 identity，`owner` / `resource id` / 历史 message 的 sender/actor 一律不变），
%%% 并**必须留审计**：审计经**用例级事务扩展点** `eb_tx_port:append_conversation_audit/3`
%%% 写入（action = `conversation.handover`，含 from/to identity）。**不使用**
%%% `eb_audit_port:append/2` 旁的独立写路径 —— 交接的审计必须属于会话生命周期的
%%% 用例级事务通道。
%%%
%%% **失败原子性（如实登记）**：冻结契约没有「assignee 变更 + 审计」的单事务 callback
%%% （`eb_tx_port` 只有 `accept_message/3` 与 `append_conversation_audit/3` 两个具名用例），
%%% 因此本模块用「先 CAS 变更 → 再写审计 → 审计失败即补偿回原经办」实现**可观测原子性**：
%%% 「交接生效」与「审计存在」互相蕴含（审计失败时返回 error 且 assignee 回到原值；
%%% 若补偿本身也失败，返回体显式带 `compensation_failed`，不静默）。
%%%
%%% 本模块**绝不**用「改写历史 message 的 sender/actor」来伪装交接——那是 A04 明确
%%% 禁止的、也是 `eb_message:handover_preserves_sender/2` 会当场抓住的反模式。
%%%
%%% ## 数据访问
%%%
%%% 一律经扩展点（`eb_store_port` → `eb_infra_ports:resolve(store)`；`eb_member_fact_port`
%%% 只读事实；`eb_tx_port` 用例级事务；`eb_id_port`）。本模块零 SQL、零 `elib_pg`、
%%% 不触 `*_repo` / `*_ds`，也不出现任何持久化实现模块名。
%%% 端口可在 `Params` 里用同键覆盖（`store` / `audit` / `id` / `clock` / `member_fact` / `tx`），
%%% 供测试与装配。
-module(eb_conversation_app).

-moduledoc "企业会话应用层用例 —— 建立会话与经办交接。".
-export([
    open_conversation/2,
    handover_identity/2
]).

%% ===================================================================
%% 建立企业会话
%% ===================================================================

%% @doc 建立一条企业会话（归 Org；显式绑定同 Org 的默认 Workspace；带合成 consent）。
%%
%% Params：
%%   workspace_id         必填整数（必须等于服务端解析的默认 Workspace）
%%   contact_id           必填整数（企业客户，须属同 Org）
%%   business_identity_id 必填整数（当前经办身份，须属同 Org）
%%   default_workspace    必填 fun/1：`fun(OrgId) -> {ok, WorkspaceId} | WorkspaceId`
%%                        （服务端只读事实；缺省即 fail-closed）
%%   consent_at           可选（Unix 秒；缺省经注入时钟端口）
%%   actor_user_id        可选审计快照
%%   store / audit / id / clock 可选端口覆盖
%%
%% 返回 `{ok, #{conversation, consent, evidence, audit_id, workspace_id,
%% conversation_id}}` 或 `{error, Reason}`。
-spec open_conversation(integer(), map()) -> {ok, map()} | {error, term()}.
open_conversation(OrgId, Params) when is_map(Params) ->
    case tenant(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, WorkspaceId} ->
            open_args(OrgId, WorkspaceId, Params)
    end;
open_conversation(_OrgId, _Params) ->
    {error, {invalid_argument, open_conversation}}.

open_args(OrgId, WorkspaceId, Params) ->
    ContactId = maps:get(contact_id, Params, undefined),
    IdentityId = maps:get(business_identity_id, Params, undefined),
    case is_pos_int(ContactId) of
        false ->
            {error, {invalid_contact_id, ContactId}};
        true ->
            case is_pos_int(IdentityId) of
                false ->
                    {error, {invalid_business_identity_id, IdentityId}};
                true ->
                    open_default_workspace(OrgId, WorkspaceId, ContactId, IdentityId, Params)
            end
    end.

%% 服务端解析默认 Workspace：调用方给的 workspace_id 必须与服务端事实一致，
%% 且解析结果必须能被 BC-19 `workspace_resolver` 认成一个真实 workspace 资源。
open_default_workspace(OrgId, WorkspaceId, ContactId, IdentityId, Params) ->
    case resolve_default_workspace(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, WorkspaceId} ->
            case resolve_workspace_resource(WorkspaceId) of
                ok ->
                    open_scope(OrgId, WorkspaceId, ContactId, IdentityId, Params);
                {error, _} = Err ->
                    Err
            end;
        {ok, OtherWorkspaceId} ->
            {error, {not_default_workspace, WorkspaceId, OtherWorkspaceId}}
    end.

open_scope(OrgId, WorkspaceId, ContactId, IdentityId, Params) ->
    %% 同语句租户自检：Workspace 不属于该 Org 时 store 返回
    %% `{error, {workspace_not_in_org, Ws}}`（跨 Org 默认 Workspace 在此被拒）。
    case with_store(Params, fun(Store) -> Store:list_assignments(OrgId, WorkspaceId) end) of
        {error, {workspace_not_in_org, _} = Reason} ->
            {error, Reason};
        {error, _} = Err ->
            Err;
        {ok, _Assignments} ->
            open_with_consent(OrgId, WorkspaceId, ContactId, IdentityId, Params)
    end.

open_with_consent(OrgId, WorkspaceId, ContactId, IdentityId, Params) ->
    case new_id(enterprise_conversation, Params) of
        {error, _} = Err ->
            Err;
        {ok, ConversationId} ->
            case eb_consent_app:synthetic_fields(ConversationId, Params) of
                {error, _} = Err ->
                    Err;
                {ok, Consent} ->
                    open_gate(
                        OrgId, WorkspaceId, ContactId, IdentityId, ConversationId, Consent, Params
                    )
            end
    end.

open_gate(OrgId, WorkspaceId, ContactId, IdentityId, ConversationId, Consent, Params) ->
    %% 自检：合成件必须自洽（三列齐全 + 版本一致），否则不写。
    case eb_consent:gate(Consent, eb_consent_app:synthetic_notice_version()) of
        {error, _} = Err ->
            Err;
        ok ->
            open_domain(
                OrgId, WorkspaceId, ContactId, IdentityId, ConversationId, Consent, Params
            )
    end.

open_domain(OrgId, WorkspaceId, ContactId, IdentityId, ConversationId, Consent, Params) ->
    %% `workspace_organization_id` 是**服务端已解析并验证过的派生事实**（见 open_scope），
    %% 调用方传入同名参数在 facade 层已被判非法。
    case
        eb_conversation:open(#{
            organization_id => OrgId,
            workspace_id => WorkspaceId,
            workspace_organization_id => OrgId,
            contact_id => ContactId,
            business_identity_id => IdentityId,
            conversation_id => ConversationId
        })
    of
        {error, _} = Err ->
            Err;
        {ok, Normalized} ->
            Row = Normalized#{
                id => ConversationId,
                notice_version => maps:get(notice_version, Consent),
                consent_at => maps:get(consent_at, Consent),
                consent_subject => maps:get(consent_subject, Consent),
                consent_evidence_kind => eb_consent_app:evidence_kind(Consent)
            },
            insert_conversation(OrgId, WorkspaceId, Row, Consent, Params)
    end.

insert_conversation(OrgId, WorkspaceId, Row, Consent, Params) ->
    case
        with_store(Params, fun(Store) ->
            Store:insert_conversation(OrgId, WorkspaceId, Row)
        end)
    of
        {error, conflict} ->
            {error, {conversation_exists, maps:get(id, Row)}};
        {error, _} = Err ->
            Err;
        {ok, Stored} ->
            conversation_audit(OrgId, WorkspaceId, Stored, Consent, Params)
    end.

%% 审计在写库之后独立追加（store/审计非同一事务——冻结契约没有事务端口，如实登记）。
conversation_audit(OrgId, WorkspaceId, Stored, Consent, Params) ->
    Event = #{
        resource_type => <<"enterprise_conversation">>,
        resource_id => maps:get(id, Stored),
        action => <<"enterprise_conversation.open">>,
        business_identity_id => maps:get(business_identity_id, Stored, undefined),
        actor_user_id => actor_user_id(Params),
        detail => #{
            <<"workspace_id">> => WorkspaceId,
            <<"contact_id">> => maps:get(contact_id, Stored),
            <<"notice_version">> => maps:get(notice_version, Consent),
            <<"synthetic_consent">> => true
        }
    },
    case port(audit, Params) of
        {error, _} = Err ->
            Err;
        {ok, Audit} ->
            case Audit:append(OrgId, Event) of
                {error, Reason} ->
                    {error, {audit_append_failed, Reason}};
                {ok, AuditId} ->
                    {ok, #{
                        conversation => Stored,
                        conversation_id => maps:get(id, Stored),
                        workspace_id => WorkspaceId,
                        consent => Consent,
                        %% 证据只能声明合成状态机 PASS（不构成真实同意/合规）
                        evidence => eb_consent_app:evidence(Consent),
                        audit_id => AuditId
                    }}
            end
    end.

%% ===================================================================
%% 经办交接
%% ===================================================================

%% @doc 把会话的当前经办 identity 交接给同 Org 的另一个 active identity。
%%
%% owner（`organization_id`）、`workspace_id`、`contact_id`、resource id 一律不变，
%% 历史 message 的 sender/actor 也一律不变（A04 的硬不变量）。
%%
%% Params：
%%   conversation_id  必填整数
%%   to_identity_id   必填整数（目标 active 经办）
%%   actor_user_id    必填（审计人；交接属可追责操作）
%%   store / tx 可选端口覆盖
%%
%% 返回 `{ok, #{conversation, conversation_id, from_identity_id, to_identity_id, audit_id}}`
%% 或 `{error, Reason}`。审计**必须**写入（见模块头「失败原子性」）。
-spec handover_identity(integer(), map()) -> {ok, map()} | {error, term()}.
handover_identity(OrgId, Params) when is_map(Params) ->
    case tenant(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, WorkspaceId} ->
            handover_args(OrgId, WorkspaceId, Params)
    end;
handover_identity(_OrgId, _Params) ->
    {error, {invalid_argument, handover_identity}}.

handover_args(OrgId, WorkspaceId, Params) ->
    ConversationId = maps:get(conversation_id, Params, undefined),
    ToIdentityId = maps:get(to_identity_id, Params, undefined),
    case is_pos_int(ConversationId) of
        false ->
            {error, {invalid_conversation_id, ConversationId}};
        true ->
            case is_pos_int(ToIdentityId) of
                false ->
                    {error, {invalid_business_identity_id, ToIdentityId}};
                true ->
                    handover_scope(OrgId, WorkspaceId, ConversationId, ToIdentityId, Params)
            end
    end.

handover_scope(OrgId, WorkspaceId, ConversationId, ToIdentityId, Params) ->
    case with_store(Params, fun(Store) -> Store:list_assignments(OrgId, WorkspaceId) end) of
        {error, {workspace_not_in_org, _} = Reason} ->
            {error, Reason};
        {error, _} = Err ->
            Err;
        {ok, _Assignments} ->
            handover_conversation(OrgId, WorkspaceId, ConversationId, ToIdentityId, Params)
    end.

handover_conversation(OrgId, WorkspaceId, ConversationId, ToIdentityId, Params) ->
    case
        with_store(Params, fun(Store) ->
            Store:fetch_conversation(OrgId, WorkspaceId, ConversationId)
        end)
    of
        {error, not_found} ->
            {error, {conversation_not_found, ConversationId}};
        {error, _} = Err ->
            Err;
        {ok, Conversation} ->
            handover_identity_gate(
                OrgId, WorkspaceId, Conversation, ConversationId, ToIdentityId, Params
            )
    end.

handover_identity_gate(OrgId, WorkspaceId, Conversation, ConversationId, ToIdentityId, Params) ->
    case
        with_store(Params, fun(Store) ->
            Store:fetch_identity(OrgId, WorkspaceId, ToIdentityId)
        end)
    of
        {error, not_found} ->
            {error, {identity_not_in_org, ToIdentityId}};
        {error, _} = Err ->
            Err;
        {ok, Identity} ->
            case maps:get(status, Identity, undefined) of
                active ->
                    handover_domain(
                        OrgId, WorkspaceId, Conversation, ConversationId, ToIdentityId, Params
                    );
                OtherStatus ->
                    {error, {identity_not_active, OtherStatus}}
            end
    end.

handover_domain(OrgId, WorkspaceId, Conversation, ConversationId, ToIdentityId, Params) ->
    %% domain 是交接语义的唯一真源（自交接、owner/resource id 不变）。
    case eb_conversation:handover(Conversation, ToIdentityId) of
        {error, _} = Err ->
            Err;
        {ok, _HandoverTarget} ->
            commit_handover(OrgId, WorkspaceId, Conversation, ConversationId, ToIdentityId, Params)
    end.

%% 交接落库：① CAS 变更经办（只改当前 identity）② 写**用例级事务**审计。
%% 审计失败 ⇒ 补偿回原经办，使「交接生效 ⇔ 审计存在」（见模块头「失败原子性」）。
commit_handover(OrgId, WorkspaceId, Conversation, ConversationId, ToIdentityId, Params) ->
    FromIdentityId = maps:get(business_identity_id, Conversation, undefined),
    case
        with_store(Params, fun(Store) ->
            Store:update_conversation_assignee(OrgId, WorkspaceId, ConversationId, ToIdentityId)
        end)
    of
        {error, conflict} ->
            %% CAS 未命中（并发交接 / 会话非 active）：交给调用方重试，绝不当作成功。
            {error, {handover_conflict, ConversationId}};
        {error, _} = Err ->
            Err;
        {ok, Updated} ->
            handover_with_audit(
                OrgId, WorkspaceId, Updated, ConversationId, FromIdentityId, ToIdentityId, Params
            )
    end.

handover_with_audit(OrgId, WorkspaceId, Updated, ConversationId, From, To, Params) ->
    case handover_audit(OrgId, WorkspaceId, ConversationId, From, To, Params) of
        {error, Reason} ->
            Compensation =
                compensate_handover(OrgId, WorkspaceId, ConversationId, From, Params),
            {error, {handover_audit_failed, Reason, Compensation}};
        {ok, AuditId} ->
            {ok, #{
                conversation => Updated,
                conversation_id => ConversationId,
                workspace_id => WorkspaceId,
                from_identity_id => From,
                to_identity_id => To,
                audit_id => AuditId,
                %% 历史 message 的 sender/actor 未被触碰（A04）
                history_rewritten => false
            }}
    end.

%% 审计经**用例级事务扩展点**（`eb_tx_port:append_conversation_audit/3`）——交接属于
%% 会话生命周期用例，必须走会话审计的用例级事务通道，而不是普通审计端口的独立写。
handover_audit(OrgId, WorkspaceId, ConversationId, From, To, Params) ->
    Event = #{
        resource_type => <<"enterprise_conversation">>,
        resource_id => ConversationId,
        action => <<"conversation.handover">>,
        business_identity_id => To,
        actor_user_id => actor_user_id(Params),
        detail => #{
            <<"from_business_identity_id">> => From,
            <<"to_business_identity_id">> => To
        }
    },
    case tx_port(Params) of
        {error, _} = Err ->
            Err;
        {ok, Tx} ->
            try
                case Tx:append_conversation_audit(OrgId, WorkspaceId, Event) of
                    {ok, AuditId} -> {ok, AuditId};
                    {error, Reason} -> {error, Reason}
                end
            catch
                Class:CatchReason -> {error, {audit_port_failed, {Class, CatchReason}}}
            end
    end.

%% 补偿：把经办改回原值。原值缺失（不应发生）或补偿失败都**显式**报告，不静默成功。
compensate_handover(OrgId, WorkspaceId, ConversationId, From, Params) ->
    case is_pos_int(From) of
        false ->
            {compensation_impossible, missing_previous_identity};
        true ->
            case
                with_store(Params, fun(Store) ->
                    Store:update_conversation_assignee(OrgId, WorkspaceId, ConversationId, From)
                end)
            of
                {ok, _Restored} -> compensated;
                {error, Reason} -> {compensation_failed, Reason}
            end
    end.

%% ===================================================================
%% 内部辅助：端口 / 租户 / 事实源
%% ===================================================================

port(Key, Params) ->
    case maps:get(Key, Params, undefined) of
        Mod when is_atom(Mod), Mod =/= undefined -> {ok, Mod};
        _ -> eb_infra_ports:resolve(Key)
    end.

with_store(Params, Fun) ->
    case port(store, Params) of
        {ok, Store} -> Fun(Store);
        {error, _} = Err -> Err
    end.

%% 用例级事务端口（会话审计同事务提交；实现按装配选择）。
tx_port(Params) ->
    case maps:get(tx, Params, undefined) of
        Mod when is_atom(Mod), Mod =/= undefined -> {ok, Mod};
        _ -> eb_infra_ports:resolve(tx)
    end.

new_id(Kind, Params) ->
    case port(id, Params) of
        {error, _} = Err ->
            Err;
        {ok, IdPort} ->
            try
                {ok, IdPort:new_id(Kind)}
            catch
                Class:Reason -> {error, {id_generation_failed, Kind, {Class, Reason}}}
            end
    end.

%% 默认 Workspace 的**服务端只读事实源**：
%%   1. 显式注入 `default_workspace => fun/1`（上层装配 / 测试）；
%%   2. 否则经 EB-03R 的**最小只读事实 Port** `eb_member_fact_port:default_workspace/2`
%%      逐请求读取（事实 ≠ 授权结论；授权仍走 `eb_auth_port`）。
%% 两者都不可用时 fail-closed，**不**猜、**不**回落、**不**隐式推断。
resolve_default_workspace(OrgId, Params) ->
    case maps:get(default_workspace, Params, undefined) of
        Loader when is_function(Loader, 1) ->
            normalize_default_workspace(Loader(OrgId));
        _MissingSource ->
            default_workspace_from_facts(OrgId, Params)
    end.

default_workspace_from_facts(OrgId, Params) ->
    case member_user_id(Params) of
        {ok, UserId} ->
            case port(member_fact, Params) of
                {error, _} = Err ->
                    Err;
                {ok, Facts} ->
                    try Facts:default_workspace(OrgId, UserId) of
                        Result -> normalize_default_workspace(Result)
                    catch
                        Class:Reason -> {error, {member_fact_unavailable, {Class, Reason}}}
                    end
            end;
        missing ->
            {error, {default_workspace_source_missing, organization}}
    end.

%% 事实读取所需的成员身份：显式 `member_user_id` 优先，缺省用 `actor_user_id`。
member_user_id(Params) ->
    case is_pos_int(maps:get(member_user_id, Params, undefined)) of
        true ->
            {ok, maps:get(member_user_id, Params)};
        false ->
            case is_pos_int(maps:get(actor_user_id, Params, undefined)) of
                true -> {ok, maps:get(actor_user_id, Params)};
                false -> missing
            end
    end.

normalize_default_workspace(WorkspaceId) when is_integer(WorkspaceId), WorkspaceId > 0 ->
    {ok, WorkspaceId};
normalize_default_workspace({ok, WorkspaceId}) when is_integer(WorkspaceId), WorkspaceId > 0 ->
    {ok, WorkspaceId};
normalize_default_workspace({error, _} = Err) ->
    Err;
normalize_default_workspace(Other) ->
    {error, {invalid_default_workspace, Other}}.

%% BC-19 复用：默认 Workspace 必须能被 **`workspace_resolver`**（统一资源归属解析层）
%% 解析成一个真实存在的 workspace 资源——本模块**不另造**隐式归属推断。
%% `personal`（个人域）与 `{error, _}`（不存在 / 不支持）一律拒绝。
resolve_workspace_resource(WorkspaceId) ->
    try workspace_resolver:resolve_workspace({workspace, WorkspaceId}) of
        {ok, Resolved} when Resolved =:= WorkspaceId ->
            ok;
        {ok, _Other} ->
            {error, {default_workspace_not_resolvable, WorkspaceId}};
        personal ->
            {error, {default_workspace_not_resolvable, WorkspaceId}};
        {error, Reason} ->
            {error, {default_workspace_resolver_failed, Reason}}
    catch
        Class:Reason -> {error, {default_workspace_resolver_failed, {Class, Reason}}}
    end.

tenant(OrgId, Params) ->
    case is_pos_int(OrgId) of
        false ->
            {error, {invalid_organization_id, OrgId}};
        true ->
            case maps:get(workspace_id, Params, undefined) of
                WorkspaceId when is_integer(WorkspaceId), WorkspaceId > 0 ->
                    {ok, WorkspaceId};
                Other ->
                    {error, {invalid_workspace_id, Other}}
            end
    end.

actor_user_id(Params) ->
    first_defined([
        maps:get(actor_user_id, Params, undefined),
        maps:get(created_by_user_id, Params, undefined)
    ]).

first_defined([]) ->
    undefined;
first_defined([undefined | Rest]) ->
    first_defined(Rest);
first_defined([Value | _Rest]) ->
    Value.

is_pos_int(Value) ->
    is_integer(Value) andalso Value > 0.
