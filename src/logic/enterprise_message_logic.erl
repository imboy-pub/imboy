-module(enterprise_message_logic).

%%%
% EPGZ-04 INT-09/10 OA 代发消息（企业托管非 E2EE：direct / Workspace 企业群）。
%
% 合同（plan-gz §4.3 / manifest INV-5/6）：
%   * sender 唯一合同字段 sender_user_id（external_user_id 语义，与 A3
%     INT-11 一致）；actor_user_id / as_user_id 别名在触库前拒绝（负例钉死）。
%   * sender_mode=application：以 Application 名义发（from = principal user），
%     scope messages:send；sender_mode=human：以指定 Human 名义发，
%     scope messages:send_as_human。两种模式都固定企业托管非 E2EE。
%   * sender/recipient 解析必须满足：同 org（mapping 按 org+app 过滤）+
%     已映射（resolve active 行）+ active Human（运行时实时校验 member/
%     user/account_type——bind 触发器只保证绑定时点）。任一不满足按
%     stable 码拒绝：identity_not_mapped / organization_boundary_violation。
%   * 落库：msg_c2c / msg_c2g 服务端明文行（e2ee 恒 NULL——bot 域同款
%     先例；不走 msg_c2c_logic human 链、不写 msg_store/msg_store_staging
%     E2EE 离线真源）；payload 顶层 origin 结构（kind/sender_kind/
%     sender_user_id/application_id——A6 Flutter 渲染代发标记的数据源）；
%     enterprise_audit_event append-only 审计行（actor_role=
%     'enterprise_application'，detail 列级持久化 origin_application_id——
%     审计 actor 是 Application，不伪造成 Human 发起了 HTTP 请求）。
%   * direct：收件人必须是同 org 已映射 active Human；group：群必须是
%     granted Workspace 企业群（enterprise_group_repo:find_group_in_org_tx
%     边界判定），human sender 还必须是群 active 成员。
%   * 消息与附件原子绑定：file 消息只能引用本 (org, app) 已 confirm 的
%     attachment 行（confirm 后的 file 才能引用——同一事务内完成绑定校验
%     与消息落库，文件状态在消息事务提交前已确认）。
%%%

-export([direct_tx/3, group_tx/4]).
-export([push_after_commit/2]).

-include("log.hrl").

-define(MAX_CONTENT_BYTES, 4000).
-define(ORIGIN_KIND, <<"enterprise_application">>).

%%%===================================================================
%%% INT-09 direct
%%%===================================================================

%% @doc OA 代发企业个人消息。
%% Input（atom 键）：
%%   sender_mode        必填 application | human
%%   sender_user_id     human 必填 binary（external 语义）
%%   recipient_user_id  必填 binary（external 语义，同 org 已映射 active Human）
%%   msg_type           必填 text | file
%%   content            msg_type=text 必填 binary 1..4000
%%   object_key         msg_type=file 必填 binary（本 org/app 已 confirm 附件）
%%   as_user_id / actor_user_id —— 出现即拒（INV-5）
%% 返回 {ok, #{msg_id, sender_kind, sender_user_id, origin_kind,
%% origin_application_id, accepted_at}}；失败 {error, {Code, Detail}}。
-spec direct_tx(any(), map(), map()) -> {ok, map()} | {error, {binary(), term()}}.
direct_tx(Conn, Ctx, Input) when is_map(Input) ->
    case resolve_sender(Conn, Ctx, Input) of
        {ok, SenderKind, SenderUid} ->
            case build_payload(Conn, Ctx, Input, SenderKind, SenderUid) of
                {ok, MsgType, Payload} ->
                    case
                        resolve_active_human(
                            Conn, Ctx, maps:get(recipient_user_id, Input, undefined)
                        )
                    of
                        {ok, RecipientUid} ->
                            MsgId = next_msg_id(),
                            case
                                enterprise_message_repo:insert_direct_tx(
                                    Conn, MsgId, SenderUid, RecipientUid, MsgType, Payload
                                )
                            of
                                {ok, RowId} ->
                                    finish_accept(
                                        Conn,
                                        Ctx,
                                        SenderKind,
                                        SenderUid,
                                        <<"msg_c2c">>,
                                        RowId,
                                        MsgId
                                    );
                                {error, Reason} ->
                                    {error, {<<"internal_error">>, Reason}}
                            end;
                        {error, {Code, Detail}} ->
                            {error, {Code, Detail}}
                    end;
                {error, _} = E ->
                    E
            end;
        {error, _} = E ->
            E
    end;
direct_tx(_Conn, _Ctx, _Input) ->
    {error, {<<"invalid_request">>, input_not_map}}.

%%%===================================================================
%%% INT-10 group
%%%===================================================================

%% @doc OA 代发 Workspace 企业群消息。
%% 群定位/边界复用 A3：enterprise_group_repo:find_group_in_org_tx（跨 Org/
%% personal 群/disabled/ws archived 统一 resource_not_found）；human sender
%% 必须是群 active 成员（application 模式经 principal，无群成员要求——
%% manifest sender_constraints 的 application 分支）。
-spec group_tx(any(), map(), integer(), map()) -> {ok, map()} | {error, {binary(), term()}}.
group_tx(Conn, Ctx, GroupId, Input) when is_map(Input), is_integer(GroupId), GroupId > 0 ->
    OrgId = maps:get(organization_id, Ctx),
    case resolve_sender(Conn, Ctx, Input) of
        {ok, SenderKind, SenderUid} ->
            case enterprise_group_repo:find_group_in_org_tx(Conn, OrgId, GroupId) of
                {ok, Group} ->
                    WsId = maps:get(<<"workspace_id">>, Group),
                    %% 群在 archived Workspace 上不可发（与 A3 INT-05/06 的
                    %% ws archived -> resource_not_found 口径一致——
                    %% find_group_in_org_tx 不过滤 ws.status，须单独复核）。
                    case enterprise_group_repo:find_workspace_in_org_tx(Conn, OrgId, WsId) of
                        {ok, #{<<"status">> := <<"active">>}} ->
                            case
                                sender_group_authorized(Conn, SenderKind, SenderUid, WsId, GroupId)
                            of
                                ok ->
                                    group_payload_and_insert(
                                        Conn, Ctx, Input, SenderKind, SenderUid, GroupId
                                    );
                                {error, _} = E ->
                                    E
                            end;
                        _ ->
                            {error, {<<"resource_not_found">>, workspace_not_active}}
                    end;
                {error, not_found} ->
                    {error, {<<"resource_not_found">>, group_not_found}};
                {error, Reason} ->
                    {error, {<<"internal_error">>, Reason}}
            end;
        {error, _} = E ->
            E
    end;
group_tx(_Conn, _Ctx, _GroupId, _Input) ->
    {error, {<<"invalid_request">>, input_not_map}}.

group_payload_and_insert(Conn, Ctx, Input, SenderKind, SenderUid, GroupId) ->
    case build_payload(Conn, Ctx, Input, SenderKind, SenderUid) of
        {ok, MsgType, Payload} ->
            MsgId = next_msg_id(),
            case
                enterprise_message_repo:insert_group_tx(
                    Conn, MsgId, SenderUid, GroupId, MsgType, Payload
                )
            of
                {ok, RowId} ->
                    finish_accept(Conn, Ctx, SenderKind, SenderUid, <<"msg_c2g">>, RowId, MsgId);
                {error, Reason} ->
                    {error, {<<"internal_error">>, Reason}}
            end;
        {error, _} = E ->
            E
    end.

%%%===================================================================
%%% 判定链
%%%===================================================================

%% @doc 触库前静态校验 + scope + sender 解析（application=principal /
%% human=external -> active 内部 uid）。
resolve_sender(Conn, Ctx, Input) ->
    case forbidden_alias(Input) of
        true ->
            {error,
                {<<"invalid_request">>, {forbidden_field, sender_field_is_sender_user_id_only}}};
        false ->
            resolve_sender_mode(Conn, Ctx, Input)
    end.

%% INV-5：actor_user_id / as_user_id 一律拒绝（负例钉死）。
forbidden_alias(Input) ->
    maps:is_key(as_user_id, Input) orelse maps:is_key(actor_user_id, Input).

%% @doc sender_mode 分派：scope 由
%% enterprise_internal_boundary:required_scope_for_sender_mode/1 单点给出
%% （handler 的 Grant 资源边界判定读同一处，避免两份映射漂移）。
resolve_sender_mode(Conn, Ctx, Input) ->
    Mode = maps:get(sender_mode, Input, undefined),
    case enterprise_internal_boundary:required_scope_for_sender_mode(Mode) of
        {ok, Scope} ->
            resolve_sender_scoped(Conn, Ctx, Input, Mode, Scope);
        error ->
            {error, {<<"invalid_request">>, invalid_sender_mode}}
    end.

-spec resolve_sender_scoped(any(), map(), map(), binary(), binary()) ->
    {ok, binary(), integer()} | {error, {binary(), term()}}.
resolve_sender_scoped(_Conn, Ctx, _Input, <<"application">>, Scope) ->
    scope_gate(Ctx, Scope, fun() -> application_sender(Ctx) end);
resolve_sender_scoped(Conn, Ctx, Input, <<"human">>, Scope) ->
    case maps:get(sender_user_id, Input, undefined) of
        SenderExt when is_binary(SenderExt), SenderExt =/= <<>> ->
            scope_gate(Ctx, Scope, fun() ->
                case resolve_active_human(Conn, Ctx, SenderExt) of
                    {ok, Uid} -> {ok, <<"human">>, Uid};
                    {error, _} = E -> E
                end
            end);
        _ ->
            {error, {<<"invalid_request">>, sender_user_id_required}}
    end.

scope_gate(Ctx, RequiredScope, Next) ->
    Granted = maps:get(granted_scopes, Ctx, []),
    case enterprise_internal_scope:authorize(RequiredScope, Granted) of
        ok ->
            Next();
        {error, _} ->
            {error, {<<"insufficient_scope">>, RequiredScope}}
    end.

%% application 模式：principal 由 handler 预取放入 Ctx（无绑定时
%% invalid_request fail-closed——以 Application 名义发消息必须有可展示
%% 的内部主体；plan-gz §4.3「内部可绑定可信 service-principal user」）。
application_sender(Ctx) ->
    Principal = maps:get(principal_user_id, Ctx, undefined),
    case is_integer(Principal) andalso Principal > 0 of
        true ->
            {ok, <<"application">>, Principal};
        false ->
            {error, {<<"invalid_request">>, application_principal_required}}
    end.

%%%===================================================================
%%% 解析与边界
%%%===================================================================

%% @doc active Human 运行时校验（mapping active + member active + Human +
%% user 正常）。判定字段与 trg_enterprise_external_identity_member_guard
%% 同源；跨 org 的 external 不在本 (org, app) mapping 内 -> identity_not_mapped。
-spec resolve_active_human(any(), map(), binary()) ->
    {ok, integer()} | {error, {binary(), term()}}.
resolve_active_human(Conn, Ctx, ExternalUserId) when
    is_binary(ExternalUserId), ExternalUserId =/= <<>>
->
    OrgId = maps:get(organization_id, Ctx),
    AppId = maps:get(application_id, Ctx),
    case enterprise_external_identity_repo:resolve_tx(Conn, OrgId, AppId, [ExternalUserId]) of
        {ok, [#{<<"user_id">> := Uid} | _]} ->
            case active_human_fact(Conn, OrgId, Uid) of
                ok -> {ok, Uid};
                {error, Detail} -> {error, {<<"identity_not_mapped">>, Detail}}
            end;
        {ok, []} ->
            {error, {<<"identity_not_mapped">>, {unmapped, ExternalUserId}}};
        {error, Reason} ->
            {error, {<<"internal_error">>, Reason}}
    end;
resolve_active_human(_Conn, _Ctx, _ExternalUserId) ->
    {error, {<<"invalid_request">>, invalid_external_user_id}}.

active_human_fact(Conn, OrgId, Uid) ->
    case enterprise_org_member_repo:find_membership_with_user_tx(Conn, OrgId, Uid) of
        {ok, #{
            <<"member_status">> := <<"active">>,
            <<"account_type">> := 0,
            <<"user_status">> := 1
        }} ->
            ok;
        {ok, Facts} ->
            {error, {not_active_human, Facts}};
        {error, not_found} ->
            {error, {not_org_member, Uid}};
        {error, Reason} ->
            {error, Reason}
    end.

%% human sender：群消息还须是目标 Workspace 权限下的群 active 成员
%% （群成员 ⊆ Workspace 成员，A3 选型 4）。
sender_group_authorized(_Conn, <<"application">>, _SenderUid, _WsId, _GroupId) ->
    ok;
sender_group_authorized(Conn, <<"human">>, SenderUid, WsId, GroupId) ->
    case enterprise_group_repo:non_ws_member_uids_tx(Conn, WsId, [SenderUid]) of
        {ok, []} ->
            case enterprise_group_repo:group_member_status_tx(Conn, GroupId, SenderUid) of
                {ok, #{<<"status">> := 1}} ->
                    ok;
                {ok, _} ->
                    {error, {<<"organization_boundary_violation">>, sender_not_group_member}};
                {error, not_found} ->
                    {error, {<<"organization_boundary_violation">>, sender_not_group_member}}
            end;
        {ok, [_]} ->
            {error,
                {<<"organization_boundary_violation">>, {sender_not_workspace_member, SenderUid}}};
        {error, Reason} ->
            {error, {<<"internal_error">>, Reason}}
    end.

%%%===================================================================
%%% payload / 落库 / 审计 / 事件
%%%===================================================================

%% @doc 构造 payload 子对象（msg_c2c/msg_c2g payload 列形态）。
%% text：content/text + origin；file：file{object_key,name,size,mime_type}
%% + origin（附件字段来自服务端 attachment 行——confirm 后的 file 才能引用，
%% 未 confirm / 跨 org/app 的一律 resource_not_found，不泄露存在性）。
build_payload(Conn, Ctx, Input, SenderKind, SenderUid) ->
    case maps:get(msg_type, Input, undefined) of
        <<"text">> ->
            Content = maps:get(content, Input, undefined),
            case
                is_binary(Content) andalso byte_size(Content) > 0 andalso
                    byte_size(Content) =< ?MAX_CONTENT_BYTES
            of
                true ->
                    Payload = #{
                        <<"content">> => Content,
                        <<"text">> => Content,
                        <<"origin">> => origin(Ctx, SenderKind, SenderUid)
                    },
                    {ok, <<"text">>, Payload};
                false ->
                    {error, {<<"invalid_request">>, invalid_content}}
            end;
        <<"file">> ->
            ObjectKey = maps:get(object_key, Input, undefined),
            case is_binary(ObjectKey) andalso ObjectKey =/= <<>> of
                true ->
                    OrgId = maps:get(organization_id, Ctx),
                    AppId = maps:get(application_id, Ctx),
                    case enterprise_asset_repo:find_confirmed_tx(Conn, OrgId, AppId, ObjectKey) of
                        {ok, Att} ->
                            Payload = #{
                                <<"file">> => #{
                                    <<"file_id">> => maps:get(<<"id">>, Att),
                                    <<"object_key">> => ObjectKey,
                                    <<"name">> => maps:get(<<"name">>, Att, <<>>),
                                    <<"size">> => maps:get(<<"size">>, Att, 0),
                                    <<"mime_type">> => maps:get(<<"mime_type">>, Att, <<>>)
                                },
                                <<"origin">> => origin(Ctx, SenderKind, SenderUid)
                            },
                            {ok, <<"file">>, Payload};
                        {error, not_found} ->
                            {error, {<<"resource_not_found">>, file_not_confirmed}}
                    end;
                false ->
                    {error, {<<"invalid_request">>, object_key_required}}
            end;
        _ ->
            {error, {<<"invalid_request">>, invalid_msg_type}}
    end.

origin(Ctx, SenderKind, SenderUid) ->
    AppId = maps:get(application_id, Ctx),
    Base = #{
        <<"kind">> => ?ORIGIN_KIND,
        <<"application_id">> => AppId,
        <<"sender_kind">> => SenderKind
    },
    case SenderKind of
        <<"human">> when is_integer(SenderUid) ->
            Base#{<<"sender_user_id">> => SenderUid};
        _ ->
            Base
    end.

finish_accept(Conn, Ctx, SenderKind, SenderUid, ResourceType, RowId, MsgId) ->
    OrgId = maps:get(organization_id, Ctx),
    AppId = maps:get(application_id, Ctx),
    Principal = maps:get(principal_user_id, Ctx, null),
    %% FULL-02：origin 一等账本与消息行**同事务**写入（消息主体与 origin 原子；
    %% application_id 恒非空、human sender 必带 sender_user_id、non_e2ee 恒真——
    %% 三者由 migration 00000140 的 CHECK 声明式强制）。
    Kind = conversation_kind(ResourceType),
    case
        enterprise_message_origin_repo:insert_tx(
            Conn,
            Kind,
            OrgId,
            AppId,
            sender_kind_atom(SenderKind),
            origin_sender_uid(SenderKind, SenderUid),
            RowId,
            MsgId
        )
    of
        {ok, _OriginRow} ->
            finish_audit(
                Conn,
                Ctx,
                SenderKind,
                SenderUid,
                ResourceType,
                RowId,
                MsgId,
                OrgId,
                AppId,
                Principal
            );
        {error, Reason} ->
            {error, {<<"internal_error">>, {message_origin, Reason}}}
    end.

-spec conversation_kind(binary()) -> direct | group.
conversation_kind(<<"msg_c2c">>) -> direct;
conversation_kind(<<"msg_c2g">>) -> group;
conversation_kind(_Other) -> direct.

-spec sender_kind_atom(binary()) -> application | human.
sender_kind_atom(<<"human">>) -> human;
sender_kind_atom(_Other) -> application.

%% origin 账本的 sender_user_id 只在 human 模式写入（application 模式的
%% from_id 是 principal，但那是消息主体字段，不是 Human sender 痕迹；
%% DB CHECK `(sender_kind='human') = (sender_user_id IS NOT NULL)` 强制二者一致）。
-spec origin_sender_uid(binary(), term()) -> undefined | integer().
origin_sender_uid(<<"human">>, Uid) when is_integer(Uid) -> Uid;
origin_sender_uid(_Kind, _Uid) -> undefined.

-spec finish_audit(
    any(),
    map(),
    binary(),
    integer(),
    binary(),
    integer(),
    binary(),
    integer(),
    integer(),
    term()
) ->
    {ok, map()} | {error, {binary(), term()}}.
finish_audit(
    Conn, Ctx, SenderKind, SenderUid, ResourceType, RowId, MsgId, OrgId, AppId, Principal
) ->
    Detail = #{
        <<"msg_id">> => MsgId,
        <<"origin_kind">> => ?ORIGIN_KIND,
        <<"origin_application_id">> => AppId,
        <<"sender_kind">> => SenderKind,
        <<"sender_user_id">> => SenderUid,
        <<"resource_type">> => ResourceType,
        %% INT-BE-03 七字段口径补齐：correlation（Idempotency-Key 或请求级
        %% 随机串，middleware 注入 ctx；直调 logic 的测试无该键 → null）。
        <<"correlation_id">> => maps:get(correlation_id, Ctx, null)
    },
    case
        enterprise_message_repo:insert_audit_tx(
            Conn,
            OrgId,
            #{
                resource_type => ResourceType,
                resource_id => RowId,
                action => <<"message.enterprise.accepted">>,
                actor_user_id => Principal
            },
            Detail
        )
    of
        {ok, _AuditId} ->
            %% message.enterprise.accepted 事件与消息同事务原子。
            _ = enterprise_webhook_logic:emit_event_tx(
                Conn,
                Ctx,
                <<"message.enterprise.accepted">>,
                #{resource_type => ResourceType, resource_id => RowId}
            ),
            _ = enterprise_application_usage_repo:bump_tx(
                Conn, OrgId, AppId, <<"message.accepted">>
            ),
            {ok, #{
                <<"msg_id">> => MsgId,
                %% v1.1.1 纯追加：webhook 关联键＝message.enterprise.accepted/failed
                %% 事件 resource.id 的同源值（消息表行 ID，与 msg_id 是两个标识符），
                %% 集成方以此把回调关联回发起响应（信封无 correlation_id 的补位）。
                <<"webhook_resource_id">> => RowId,
                <<"sender_kind">> => SenderKind,
                <<"sender_user_id">> => SenderUid,
                <<"origin_kind">> => ?ORIGIN_KIND,
                <<"origin_application_id">> => AppId,
                <<"accepted_at">> => erlang:system_time(millisecond)
            }};
        {error, Reason} ->
            {error, {<<"internal_error">>, Reason}}
    end.

%%%===================================================================
%%% 提交后离线推送（FULL-07）
%%%===================================================================
%%
%% 增量根因（A0 已核实）：本模块此前**没有任何 push 调用**——`msg_c2c_logic`
%% 与 `msg_c2g_logic` 都有离线推送入口，企业托管消息（INT-09/10）没有。后果是
%% OA 代发消息发给离线用户时完全不推送，企业内部平台的核心通知链是断的。
%%
%% 位置约束（**必须在消息事务 COMMIT 之后**）：
%%   * 推送是不可撤销的外部副作用（FCM/APNs/JPush 第三方通道），事务内触发
%%     会在 rollback 时产生「消息不存在但已推送」的幽灵通知；
%%   * 幂等 replay 分支（`{ok, replay, ...}`）**不**经过本函数，因此同一条
%%     消息不会因客户端重放而二次推送；
%%   * 与 `enterprise_friend_request_handler:notify_after_commit/3` 同款先例
%%     （提交后独立事务 + 失败只记日志，不影响已提交的消息结果）。
%%
%% 收件人真源 = **已提交的消息行**（不重新解析 external id）：
%%   direct: msg_c2c.to_id
%%   group : msg_c2g.to_id 的 active(status=1) 成员（`active_member_uids_tx`）
%% 这样即使 mapping 在提交后被停用，推送目标仍与已落库的收件人一致。
%%
%% 硬边界：企业托管消息固定非 E2EE；推送文案由 `push_notification_logic` 的
%% 常量单点给出，本模块**不传** msg_type/正文/payload（没有入参通道）。
-type push_table() :: binary().

%% @doc 消息提交后离线推送（Table 为 msg_c2c | msg_c2g，MsgId 为 TSID binary）。
%% 恒返回 ok：推送故障不影响已提交的消息（fail-safe，与 C2C/C2G 推送同口径）。
-spec push_after_commit(push_table(), binary()) -> ok.
push_after_commit(Table, MsgId) when is_binary(MsgId) ->
    try
        _ = elib_pg:with_tx(fun(Conn) -> push_after_commit_tx(Conn, Table, MsgId) end),
        ok
    catch
        Class:Reason ->
            ?ERROR_LOG([
                "enterprise_message push_after_commit failed", Table, MsgId, Class, Reason
            ]),
            ok
    end;
push_after_commit(_Table, _MsgId) ->
    ok.

push_after_commit_tx(Conn, <<"msg_c2c">>, MsgId) ->
    case enterprise_message_repo:find_direct_tx(Conn, MsgId) of
        {ok, #{<<"from_id">> := FromId, <<"to_id">> := ToId}} ->
            push_notification_logic:maybe_push_for_enterprise_c2c(FromId, ToId);
        _ ->
            ok
    end;
push_after_commit_tx(Conn, <<"msg_c2g">>, MsgId) ->
    case enterprise_message_repo:find_group_tx(Conn, MsgId) of
        {ok, #{<<"from_id">> := FromId, <<"to_id">> := GroupId}} ->
            case enterprise_group_repo:active_member_uids_tx(Conn, GroupId) of
                {ok, MemberUids} ->
                    %% 发送者剔除与离线判定在 push_notification_logic 内单点实现
                    %% （c2g 的发送者永不自收）。
                    push_notification_logic:maybe_push_for_enterprise_c2g(
                        FromId, GroupId, MemberUids
                    );
                _ ->
                    ok
            end;
        _ ->
            ok
    end;
push_after_commit_tx(_Conn, _Table, _MsgId) ->
    ok.

%%%===================================================================
%%% Internal
%%%===================================================================

next_msg_id() ->
    ensure_tsid(enterprise_message),
    integer_to_binary(elib_tsid:generate(enterprise_message)).

ensure_tsid(Name) ->
    case lists:member(Name, elib_tsid:registered()) of
        true -> ok;
        false -> elib_tsid:register(Name)
    end,
    ok.
