%%% @doc Enterprise Business Feature 的公开 API（铁律 3：公开 API 只能进 Application）。
%%%
%%% 依据：plan v4.1 §5.1（租户 Enterprise API 的最小动作集）、§2.1、EB-D03、
%%% `docs/architecture/feature-slice-rules.md` 铁律 3。
%%%
%%% 本模块**只做两件事**：参数收敛 + 委派。
%%%
%%%   * 每个对外函数形状统一为 `(OrgId, Params)`，返回委派结果原样透传；
%%%   * 收敛只做「形状与类型」判定（`OrgId` 必为整数；资源级必填键必须存在且
%%%     类型正确），不做任何业务规则、不读库、不写 SQL、不发布事件；
%%%   * 委派目标**只能是本 feature 的 `application/` 用例模块**（`eb_*_app`）。
%%%     本卡不引用任何其他模块——连 OTP 工具模块也不需要，见
%%%     `eb_ports:facade_reference_whitelist/0`（空集）。
%%%   * 客户不可信的派生字段（如 `workspace_organization_id`）由服务端解析，
%%%     调用方传入即判错，避免「客户端决定租户归属」。
%%%
%%% 本卡的委派目标（`application/identity|contact|conversation|message|asset|
%%% member|offboarding|retention`）由 EB-05/06/07/08 落地；本卡先冻结其模块名与
%%% 调用形状，使「facade 只调 application」成为可静态判定的事实（见
%%% `test/features/enterprise_business/application/eb_ports_tests.erl`）。
-module(enterprise_business_facade).

-moduledoc "Enterprise Business Feature 公开 API（铁律 3：公开 API 只能进 Application）。".
-export([
    %% identity / assignment
    create_identity/2,
    list_identities/2,
    bind_assignment/2,
    end_assignment/2,
    %% contact / note
    create_contact/2,
    append_note/2,
    %% contact 读 / 改 / 经办（EB-05 重开新增：§5 GET /contacts、GET /contacts/{id}、
    %% PATCH /contacts/{id}、POST /contacts/{id}/assignment）
    get_contact/2,
    list_contacts/2,
    update_contact/2,
    assign_contact/2,
    %% conversation / handover
    open_conversation/2,
    handover_identity/2,
    %% message / delivery
    append_message/2,
    ack_delivery/2,
    %% asset
    request_presign/2,
    confirm_asset/2,
    put_object/2,
    content_stream/2,
    %% member
    suspend_member/2,
    %% offboarding
    open_offboarding/2,
    execute_offboarding/2,
    verify_offboarding/2,
    finalize_offboarding/2,
    %% offboarding 读（closure §8：查询交接 case —— 列表 / 详情；零写零审计）
    list_offboarding/2,
    offboarding_detail/2,
    %% retention / hold
    open_retention_policy/2,
    create_hold/2,
    release_hold/2,
    %% retention 读 / hold 读（EB-06 重开新增：§4.1 fetch_hold 精确错误语义、
    %% §5.1 策略现值；POLICY-LATEST 此前只是 application 层私有函数）
    latest_retention_policy/2,
    fetch_hold/2,
    %% message 只读历史（EB-06 重开新增：§5.1 GET messages 的 after_id 键集分页、
    %% 单条 canonical 只读读取；两者经契约公开且**零写入**）
    list_messages/2,
    fetch_message/2
]).

%% ===================================================================
%% identity / assignment
%% ===================================================================

-spec create_identity(integer(), map()) -> term().
create_identity(OrgId, #{function_key := FunctionKey, display_name := DisplayName} = Params) when
    is_integer(OrgId), is_binary(FunctionKey), is_binary(DisplayName), is_map(Params)
->
    eb_identity_app:create_identity(OrgId, Params);
create_identity(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_argument, create_identity}};
create_identity(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

-spec list_identities(integer(), map()) -> term().
list_identities(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    eb_identity_app:list_identities(OrgId, Params);
list_identities(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

-spec bind_assignment(integer(), map()) -> term().
bind_assignment(OrgId, #{identity_id := IdentityId, user_id := UserId} = Params) when
    is_integer(OrgId), is_integer(IdentityId), is_integer(UserId), is_map(Params)
->
    eb_identity_app:bind_assignment(OrgId, Params);
bind_assignment(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_argument, bind_assignment}};
bind_assignment(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

-spec end_assignment(integer(), map()) -> term().
end_assignment(
    OrgId, #{identity_id := IdentityId, user_id := UserId, end_reason := EndReason} = Params
) when
    is_integer(OrgId),
    is_integer(IdentityId),
    is_integer(UserId),
    is_binary(EndReason),
    is_map(Params)
->
    eb_identity_app:end_assignment(OrgId, Params);
end_assignment(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_argument, end_assignment}};
end_assignment(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

%% ===================================================================
%% contact / note
%% ===================================================================

-spec create_contact(integer(), map()) -> term().
create_contact(OrgId, #{channel := Channel, subject := Subject} = Params) when
    is_integer(OrgId), is_binary(Channel), is_binary(Subject), is_map(Params)
->
    eb_contact_app:create_contact(OrgId, Params);
create_contact(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_argument, create_contact}};
create_contact(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

-spec append_note(integer(), map()) -> term().
%% FND-5（RULING-2026-09-15 §七）：HTTP 接受业务明文（body_plaintext），
%% 服务端经 provider 加密；客户端密文/密钥版本不再进入本面。
append_note(OrgId, #{contact_id := ContactId, body_plaintext := Plaintext} = Params) when
    is_integer(OrgId), is_integer(ContactId), is_binary(Plaintext), is_map(Params)
->
    eb_contact_app:append_note(OrgId, Params);
append_note(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_argument, append_note}};
append_note(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

%% @doc 客户详情（§5.1 GET /contacts/{id}）：租户作用域由 application 层经 store
%% 的同语句租户键裁决，facade 只做形状收敛，不自行判断归属。
-spec get_contact(integer(), map()) -> term().
get_contact(OrgId, #{contact_id := ContactId} = Params) when
    is_integer(OrgId), is_integer(ContactId), is_map(Params)
->
    eb_contact_app:get_contact(OrgId, Params);
get_contact(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_argument, get_contact}};
get_contact(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

%% @doc 客户列表（§5.1 GET /contacts）。分页参数（`after_id` / `limit`）为键集语义，
%% 由 application 层实现；facade 不解释分页。
-spec list_contacts(integer(), map()) -> term().
list_contacts(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    eb_contact_app:list_contacts(OrgId, Params);
list_contacts(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

%% @doc 客户资料更新（§5.1 PATCH /contacts/{id}）。至少要有 `display_name` 或
%% `profile_plaintext` 或成对的 `profile_cipher` + `profile_key_version` 之一，
%% 否则 application 层以 `empty_patch` 拒绝（facade 只保证 contact_id 形状正确）。
-spec update_contact(integer(), map()) -> term().
update_contact(OrgId, #{contact_id := ContactId} = Params) when
    is_integer(OrgId), is_integer(ContactId), is_map(Params)
->
    eb_contact_app:update_contact(OrgId, Params);
update_contact(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_argument, update_contact}};
update_contact(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

%% @doc 客户 ↔ 业务身份经办（§5.1 POST /contacts/{id}/assignment）。
%% `role` 缺省为 `primary`（由 application 层收敛白名单）。
-spec assign_contact(integer(), map()) -> term().
assign_contact(
    OrgId, #{contact_id := ContactId, business_identity_id := IdentityId} = Params
) when
    is_integer(OrgId), is_integer(ContactId), is_integer(IdentityId), is_map(Params)
->
    eb_contact_app:assign_contact(OrgId, Params);
assign_contact(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_argument, assign_contact}};
assign_contact(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

%% ===================================================================
%% conversation / handover
%% ===================================================================

%% @doc 建立企业会话。V1 只使用 Organization 的默认 Workspace，服务端解析，
%% 故客户端传入 `workspace_organization_id` 视为非法参数。
-spec open_conversation(integer(), map()) -> term().
open_conversation(OrgId, #{workspace_organization_id := _Spoofed} = Params) when
    is_integer(OrgId), is_map(Params)
->
    {error, {unexpected_argument, workspace_organization_id}};
open_conversation(
    OrgId,
    #{workspace_id := WorkspaceId, contact_id := ContactId, business_identity_id := IdentityId} =
        Params
) when
    is_integer(OrgId),
    is_integer(WorkspaceId),
    is_integer(ContactId),
    is_integer(IdentityId),
    is_map(Params)
->
    eb_conversation_app:open_conversation(OrgId, Params);
open_conversation(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_argument, open_conversation}};
open_conversation(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

%% @doc 把会话的当前经办 identity 交接给另一个 active identity；owner 不变。
-spec handover_identity(integer(), map()) -> term().
handover_identity(
    OrgId, #{conversation_id := ConversationId, to_identity_id := ToIdentityId} = Params
) when
    is_integer(OrgId), is_integer(ConversationId), is_integer(ToIdentityId), is_map(Params)
->
    eb_conversation_app:handover_identity(OrgId, Params);
handover_identity(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_argument, handover_identity}};
handover_identity(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

%% ===================================================================
%% message / delivery
%% ===================================================================

-spec append_message(integer(), map()) -> term().
append_message(
    OrgId,
    #{
        workspace_id := WorkspaceId,
        conversation_id := ConversationId,
        client_msg_id := ClientMsgId,
        sender_type := SenderType
    } = Params
) when
    is_integer(OrgId),
    is_integer(WorkspaceId),
    is_integer(ConversationId),
    is_binary(ClientMsgId),
    is_binary(SenderType),
    is_map(Params)
->
    eb_message_app:append_message(OrgId, Params);
append_message(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_argument, append_message}};
append_message(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

%% @doc 幂等确认投递：只写 delivery，不改 canonical message（plan §2.1 #16）。
-spec ack_delivery(integer(), map()) -> term().
ack_delivery(
    OrgId,
    #{workspace_id := WorkspaceId, message_id := MessageId, recipient_ref := RecipientRef} = Params
) when
    is_integer(OrgId),
    is_integer(WorkspaceId),
    is_integer(MessageId),
    is_binary(RecipientRef),
    is_map(Params)
->
    eb_message_app:ack_delivery(OrgId, Params);
ack_delivery(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_argument, ack_delivery}};
ack_delivery(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

%% ===================================================================
%% asset
%% ===================================================================

-spec request_presign(integer(), map()) -> term().
request_presign(
    OrgId,
    #{
        workspace_id := WorkspaceId,
        conversation_id := ConversationId,
        mime := Mime,
        size_bytes := SizeBytes
    } = Params
) when
    is_integer(OrgId),
    is_integer(WorkspaceId),
    is_integer(ConversationId),
    is_binary(Mime),
    is_integer(SizeBytes),
    is_map(Params)
->
    eb_asset_app:request_presign(OrgId, Params);
request_presign(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_argument, request_presign}};
request_presign(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

%% @doc confirm 必须重新鉴权：presign 后被 suspend 的 actor 在此 fail-closed。
-spec confirm_asset(integer(), map()) -> term().
confirm_asset(OrgId, #{workspace_id := WorkspaceId, upload_ref := UploadRef} = Params) when
    is_integer(OrgId), is_integer(WorkspaceId), is_binary(UploadRef), is_map(Params)
->
    eb_asset_app:confirm_asset(OrgId, Params);
confirm_asset(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_argument, confirm_asset}};
confirm_asset(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

%% BE-PATCH-01：字节上传代理（`eb_asset_app:put_object/2` 的 facade 出口）。
%% `payload` = 请求体原始字节（HTTP 面由 handler 线格式层注入，非 JSON 参数）；
%% `actor_user_id` / `actor_contact_id` 二选一（member / contact 主体），由调用
%% 面服务端派生。ref open 验过期/篡改/同上传人 + 主体作用域门 + hash/size/mime
%% 复核，全部既有实现。响应只含 asset 元数据投影，永无对象 URL。
-spec put_object(integer(), map()) -> term().
put_object(
    OrgId,
    #{
        workspace_id := WorkspaceId, upload_ref := UploadRef, payload := Payload
    } = Params
) when
    is_integer(OrgId),
    is_integer(WorkspaceId),
    is_binary(UploadRef),
    is_binary(Payload),
    is_map(Params)
->
    eb_asset_app:put_object(OrgId, Params);
put_object(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_argument, put_object}};
put_object(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

%% @doc 鉴权代理取流。返回内容流，绝不返回对象 key 或任何可下载链接。
-spec content_stream(integer(), map()) -> term().
content_stream(OrgId, #{workspace_id := WorkspaceId, asset_id := AssetId} = Params) when
    is_integer(OrgId), is_integer(WorkspaceId), is_integer(AssetId), is_map(Params)
->
    eb_asset_app:content_stream(OrgId, Params);
content_stream(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_argument, content_stream}};
content_stream(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

%% ===================================================================
%% member
%% ===================================================================

%% @doc 立即撤销该成员的全部企业业务授权（个人 IM 能力不变）。
-spec suspend_member(integer(), map()) -> term().
suspend_member(OrgId, #{member_user_id := MemberUserId, reason := Reason} = Params) when
    is_integer(OrgId), is_integer(MemberUserId), is_binary(Reason), is_map(Params)
->
    %% EB-08 把 suspend_member 的**写路径**实现在 eb_offboarding_app（S1 冻结的第一步）；
    %% 原委派目标 eb_member_app 从未落地（R0 的 D3：facade_targets 列了它但无卡租约）。
    eb_offboarding_app:suspend_member(OrgId, Params);
suspend_member(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_argument, suspend_member}};
suspend_member(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

%% ===================================================================
%% offboarding
%% ===================================================================

-spec open_offboarding(integer(), map()) -> term().
open_offboarding(
    OrgId, #{leaver_user_id := LeaverUserId, successor_user_id := SuccessorUserId} = Params
) when
    is_integer(OrgId), is_integer(LeaverUserId), is_integer(SuccessorUserId), is_map(Params)
->
    eb_offboarding_app:open_offboarding(OrgId, Params);
open_offboarding(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_argument, open_offboarding}};
open_offboarding(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

%% @doc CAS 批量 rebind：`expected_version` 决定并发裁决。
-spec execute_offboarding(integer(), map()) -> term().
execute_offboarding(OrgId, #{case_id := CaseId, expected_version := ExpectedVersion} = Params) when
    is_integer(OrgId), is_integer(CaseId), is_integer(ExpectedVersion), is_map(Params)
->
    eb_offboarding_app:execute_offboarding(OrgId, Params);
execute_offboarding(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_argument, execute_offboarding}};
execute_offboarding(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

-spec verify_offboarding(integer(), map()) -> term().
verify_offboarding(OrgId, #{case_id := CaseId} = Params) when
    is_integer(OrgId), is_integer(CaseId), is_map(Params)
->
    eb_offboarding_app:verify_offboarding(OrgId, Params);
verify_offboarding(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_argument, verify_offboarding}};
verify_offboarding(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

-spec finalize_offboarding(integer(), map()) -> term().
finalize_offboarding(OrgId, #{case_id := CaseId} = Params) when
    is_integer(OrgId), is_integer(CaseId), is_map(Params)
->
    eb_offboarding_app:finalize_offboarding(OrgId, Params);
finalize_offboarding(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_argument, finalize_offboarding}};
finalize_offboarding(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

%% @doc 离职交接 case 列表（closure §8，`GET /offboarding/cases`）。**零写零审计**：
%% 纯读取用例；分页（`after_id` / `limit`）与 `status` 过滤的语义由 application 层
%% 实现，facade 不解释分页。平台面与租户面共用本函数（租户条件在 path org_id）。
-spec list_offboarding(integer(), map()) -> term().
list_offboarding(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    eb_offboarding_app:list_cases(OrgId, Params);
list_offboarding(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

%% @doc 离职交接 case 详情（closure §8，`GET /offboarding/cases/:id`）。
%% 含 items 子表（`items_status` 可选过滤失败项）；同样零写零审计。
-spec offboarding_detail(integer(), map()) -> term().
offboarding_detail(OrgId, #{case_id := CaseId} = Params) when
    is_integer(OrgId), is_integer(CaseId), is_map(Params)
->
    eb_offboarding_app:case_detail(OrgId, Params);
offboarding_detail(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_argument, offboarding_detail}};
offboarding_detail(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

%% ===================================================================
%% retention / hold
%% ===================================================================

-spec open_retention_policy(integer(), map()) -> term().
open_retention_policy(
    OrgId,
    #{workspace_id := WorkspaceId, data_class := DataClass, retention_days := RetentionDays} =
        Params
) when
    is_integer(OrgId),
    is_integer(WorkspaceId),
    is_binary(DataClass),
    is_integer(RetentionDays),
    is_map(Params)
->
    eb_retention_app:open_retention_policy(OrgId, Params);
open_retention_policy(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_argument, open_retention_policy}};
open_retention_policy(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

%% @doc 创建 hold（append-only 事实）。真实 hold 属需担责操作，本仓只用合成 fixture。
-spec create_hold(integer(), map()) -> term().
create_hold(
    OrgId, #{workspace_id := WorkspaceId, scope := Scope, reason_code := ReasonCode} = Params
) when
    is_integer(OrgId),
    is_integer(WorkspaceId),
    is_binary(Scope),
    is_binary(ReasonCode),
    is_map(Params)
->
    eb_retention_app:create_hold(OrgId, Params);
create_hold(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_argument, create_hold}};
create_hold(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

-spec release_hold(integer(), map()) -> term().
release_hold(OrgId, #{hold_id := HoldId} = Params) when
    is_integer(OrgId), is_integer(HoldId), is_map(Params)
->
    eb_retention_app:release_hold(OrgId, Params);
release_hold(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_argument, release_hold}};
release_hold(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

%% @doc hold 详情（EB-06 重开新增，§4.1）。三态精确可区分：不存在 / 已释放 / 跨 Org；
%% facade 只收敛 `hold_id` 的形状，裁决在 application 层（`eb_retention_app:fetch_hold/2`）。
-spec fetch_hold(integer(), map()) -> term().
fetch_hold(OrgId, #{hold_id := HoldId} = Params) when
    is_integer(OrgId), is_integer(HoldId), is_map(Params)
->
    eb_retention_app:fetch_hold(OrgId, Params);
fetch_hold(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_argument, fetch_hold}};
fetch_hold(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

%% @doc 当前生效的保留策略（EB-06 重开新增，`POLICY-LATEST`）。无策略 ⇒ `not_found`
%%（没有默认保留期）。
-spec latest_retention_policy(integer(), map()) -> term().
latest_retention_policy(OrgId, #{data_class := DataClass} = Params) when
    is_integer(OrgId), is_binary(DataClass), is_map(Params)
->
    eb_retention_app:latest_retention_policy(OrgId, Params);
latest_retention_policy(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_argument, latest_retention_policy}};
latest_retention_policy(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

%% @doc 会话历史的键集分页读取（EB-06 重开新增，§5.1 `GET messages`）。
%% `after_id` / `limit` 的语义由 application 层实现（键集，不是 offset）。
-spec list_messages(integer(), map()) -> term().
list_messages(OrgId, #{conversation_id := ConversationId} = Params) when
    is_integer(OrgId), is_integer(ConversationId), is_map(Params)
->
    eb_message_app:list_messages(OrgId, Params);
list_messages(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_argument, list_messages}};
list_messages(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

%% @doc 单条 canonical 消息的只读读取（EB-06 重开新增）。**零写入**：不改 seen /
%% status / version，也不产生 delivery 行。
-spec fetch_message(integer(), map()) -> term().
fetch_message(OrgId, #{message_id := MessageId} = Params) when
    is_integer(OrgId), is_integer(MessageId), is_map(Params)
->
    eb_message_app:fetch_message(OrgId, Params);
fetch_message(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_argument, fetch_message}};
fetch_message(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.
