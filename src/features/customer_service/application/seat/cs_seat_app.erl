%%% @doc 客服坐席（seat）的应用层用例：创建 / 开关（suspend）。
%%%
%%% 依据：plan v4.1 §4.2（customer_service_seat）、§5.2、CS-01-A01/A02、EB-D03。
%%%
%%% 职责边界：
%%%   * A01 双重校验的应用侧：创建前先经扩展点读取 identity 的 `function_key`，
%%%     非 `customer_service` 直接拒绝（数据库侧由复合 FK + CHECK 兜底——
%%%     绕过应用也进不来）；不采信调用方自报职能。
%%%   * seat 是 identity 的运营属性：**不含 owner user**；当前用户来自 active
%%%     identity assignment（EB 侧），不在本用例的读写面内（A04 的前提）。
%%%   * 授权（tenant admin / platform admin）由 CS-02 的认证分流判定；
%%%     本模块只判业务前提。
%%%
%%% 数据访问全部经 `cs_store_port`（Params 里 `store` / `id` 键可注入，
%%% 未给则用 `cs_infra_ports` 装配默认）；零 SQL、零 `elib_pg`。
-module(cs_seat_app).

-export([
    create_seat/2,
    suspend_seat/2,
    resume_seat/2,
    fetch_seat/2,
    list_dispatchable_seats/2,
    %% 平台运营面坐席分页（跨企业可选 Org 过滤）
    list_platform_seats/2,
    session_detail/2,
    %% CS-BE-03（CS-DEC-01）：客户上下文只读投影（session ownership 门）。
    session_context/2,
    %% BE-S01a：坐席上下文清单（主体自身作用域）+ 转接目标最小投影
    seat_contexts/1,
    transfer_targets/2,
    %% BE-S01b：平台面事务化开通/修复坐席（/api/adm provisioning）
    provision_seat/2
]).

%% ===================================================================
%% 创建坐席（A01）
%% ===================================================================

%% @doc 为 customer_service 业务身份开通坐席（PK = business_identity_id，重复开通 conflict）。
%%
%% Params：
%%   workspace_id         必填整数（租户操作范围）
%%   business_identity_id 必填整数；其 function_key 必须是 customer_service（应用侧
%%                        经 store 读取后判定，DB 侧复合 FK 兜底）
%%   max_concurrent       可选正整数（默认 1）
%%   enabled              可选布尔（默认 true）
%%   created_by_user_id   可选（审计快照）
%%   store / id           可选端口覆盖
-spec create_seat(integer(), map()) -> {ok, map()} | {error, term()}.
create_seat(OrgId, Params) when is_map(Params) ->
    case cs_app_support:tenant(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, WorkspaceId} ->
            create_seat_in(OrgId, WorkspaceId, Params)
    end;
create_seat(_OrgId, _Params) ->
    {error, {invalid_argument, create_seat}}.

create_seat_in(OrgId, WorkspaceId, Params) ->
    IdentityId = maps:get(business_identity_id, Params, undefined),
    case pos_int(IdentityId) of
        false ->
            {error, {invalid_identity_id, IdentityId}};
        true ->
            case
                with_store(Params, fun(Store) ->
                    Store:fetch_identity_function(OrgId, IdentityId)
                end)
            of
                {error, not_found} ->
                    {error, {identity_not_found, IdentityId}};
                {error, _} = Err ->
                    Err;
                {ok, <<"customer_service">>} ->
                    insert_seat(OrgId, WorkspaceId, IdentityId, Params);
                {ok, OtherFunction} ->
                    {error, {identity_not_customer_service, IdentityId, OtherFunction}}
            end
    end.

insert_seat(OrgId, WorkspaceId, IdentityId, Params) ->
    Enabled = maps:get(enabled, Params, true),
    MaxConcurrent = maps:get(max_concurrent, Params, 1),
    Seat = #{
        organization_id => OrgId,
        business_identity_id => IdentityId,
        function_key => <<"customer_service">>,
        enabled => Enabled,
        max_concurrent => MaxConcurrent,
        created_by_user_id => maps:get(created_by_user_id, Params, undefined)
    },
    case with_store(Params, fun(Store) -> Store:insert_seat(OrgId, Seat) end) of
        {error, _} = Err ->
            Err;
        {ok, Stored} ->
            case
                append_event(Params, OrgId, WorkspaceId, #{
                    business_identity_id => IdentityId,
                    actor_user_id => maps:get(created_by_user_id, Params, undefined),
                    action => <<"seat.created">>,
                    detail => #{
                        <<"max_concurrent">> => MaxConcurrent,
                        <<"enabled">> => Enabled
                    }
                })
            of
                ok -> {ok, Stored#{workspace_id => WorkspaceId}};
                {error, _} = AuditErr -> AuditErr
            end
    end.

%% ===================================================================
%% 坐席开关（suspend / resume）
%% ===================================================================

%% @doc 停用坐席：`enabled=false` 后新 claim 即时被拒；
%% 既有 active 会话不自动迁移（显式 transfer/close 才变化）。
-spec suspend_seat(integer(), map()) -> {ok, map()} | {error, term()}.
suspend_seat(OrgId, Params) ->
    set_enabled(OrgId, Params, false, <<"seat.suspended">>).

%% @doc 恢复坐席。
-spec resume_seat(integer(), map()) -> {ok, map()} | {error, term()}.
resume_seat(OrgId, Params) ->
    set_enabled(OrgId, Params, true, <<"seat.resumed">>).

set_enabled(OrgId, Params, Enabled, Action) when is_map(Params) ->
    case cs_app_support:tenant(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, WorkspaceId} ->
            set_enabled_in(OrgId, WorkspaceId, Enabled, Action, Params)
    end;
set_enabled(_OrgId, _Params, _Enabled, _Action) ->
    {error, {invalid_argument, suspend_seat}}.

set_enabled_in(OrgId, WorkspaceId, Enabled, Action, Params) ->
    IdentityId = maps:get(business_identity_id, Params, undefined),
    At = maps:get(at, Params, undefined),
    case pos_int(IdentityId) andalso pos_int(At) of
        false ->
            {error, {invalid_argument, set_seat_enabled}};
        true ->
            case
                with_store(Params, fun(Store) ->
                    Store:set_seat_enabled(OrgId, IdentityId, Enabled, At)
                end)
            of
                {error, _} = Err ->
                    Err;
                {ok, Seat} ->
                    case
                        append_event(Params, OrgId, WorkspaceId, #{
                            business_identity_id => IdentityId,
                            actor_user_id => maps:get(actor_user_id, Params, undefined),
                            action => Action,
                            detail => #{<<"reason">> => maps:get(reason, Params, undefined)}
                        })
                    of
                        ok -> {ok, Seat#{workspace_id => WorkspaceId}};
                        {error, _} = AuditErr -> AuditErr
                    end
            end
    end.

%% ===================================================================
%% 读取
%% ===================================================================

%% @doc 坐席详情。
-spec fetch_seat(integer(), map()) -> {ok, map()} | {error, term()}.
fetch_seat(OrgId, Params) when is_map(Params) ->
    IdentityId = maps:get(business_identity_id, Params, undefined),
    case pos_int(IdentityId) of
        false ->
            {error, {invalid_identity_id, IdentityId}};
        true ->
            with_store(Params, fun(Store) -> Store:fetch_seat(OrgId, IdentityId) end)
    end;
fetch_seat(_OrgId, _Params) ->
    {error, {invalid_argument, fetch_seat}}.

%% @doc 派单快照（enabled 坐席 + active 计数；least-active 的输入）。
%%
%% C4（contracts-w2）：HTTP 面列表支持键集分页——after_id（TSID，按
%% business_identity_id 游标）/ limit（1..200，缺省 50，越界
%% `{invalid_limit,_}`）；SQL LIMIT 由 `cs_pg_seat:list_dispatchable_seats_page/3`
%% 下推。返回 `{ok, #{seats := Rows, next_after_id := Cursor | undefined}}`；
%% 既有投影字段不变。内部派单（claim 的 least-active）仍走
%% `cs_store_port:list_dispatchable_seats/1` 原快照，不受分页影响。
-spec list_dispatchable_seats(integer(), map()) -> {ok, map()} | {error, term()}.
list_dispatchable_seats(OrgId, Params) when is_map(Params) ->
    case cs_app_support:page_cursor(Params) of
        {error, _} = Err ->
            Err;
        {ok, AfterId, Limit} ->
            case
                with_store(Params, fun(Store) ->
                    Store:list_dispatchable_seats_page(OrgId, AfterId, Limit)
                end)
            of
                {error, _} = Err2 ->
                    Err2;
                {ok, Rows} ->
                    cs_app_support:page_view(
                        seats,
                        identity_projection(),
                        Rows,
                        Limit,
                        business_identity_id
                    )
            end
    end;
list_dispatchable_seats(_OrgId, _Params) ->
    {error, {invalid_argument, list_dispatchable_seats}}.

%% 既有投影字段（contracts-w2 C4：不变）。
identity_projection() ->
    [business_identity_id, function_key, enabled, max_concurrent, active_count].

%% ===================================================================
%% 平台运营面坐席分页（跨企业可选 Org 过滤）
%% ===================================================================

%% @doc 平台运营面坐席分页列表（`p_platform_seats` → `list_platform_seats/2`）。
%%
%% 与 `list_dispatchable_seats/2` 的差别（运营面语义，不是第二套业务逻辑——
%% 复用同一 page_cursor/page_view 组装与 store 端口）：
%%   * OrgId=0（param_optional 缺省）= 跨企业全局；>0 = 收窄到该企业；
%%   * 不按 enabled 过滤——运营面要能定位并恢复已停用坐席；
%%   * 投影带 organization_name / display_name（列表可读）与默认 active
%%     workspace_id（suspend/resume 审计事件的服务端落点）。
-spec list_platform_seats(integer(), map()) -> {ok, map()} | {error, term()}.
list_platform_seats(OrgId, Params) when is_map(Params) ->
    case org_filter(OrgId) of
        {error, _} = Err ->
            Err;
        {ok, OrgFilter} ->
            case cs_app_support:page_cursor(Params) of
                {error, _} = Err2 ->
                    Err2;
                {ok, AfterId, Limit} ->
                    case
                        with_store(Params, fun(Store) ->
                            Store:list_all_seats_page(OrgFilter, AfterId, Limit)
                        end)
                    of
                        {error, _} = Err3 ->
                            Err3;
                        {ok, Rows} ->
                            cs_app_support:page_view(
                                seats,
                                platform_projection(),
                                Rows,
                                Limit,
                                business_identity_id
                            )
                    end
            end
    end;
list_platform_seats(_OrgId, _Params) ->
    {error, {invalid_argument, list_platform_seats}}.

org_filter(0) ->
    %% param_optional 缺省：跨企业全局（占位语义同 self/derived 的 0）。
    {ok, 0};
org_filter(OrgId) when is_integer(OrgId), OrgId > 0 ->
    {ok, OrgId};
org_filter(OrgId) ->
    {error, {invalid_organization_id, OrgId}}.

platform_projection() ->
    [
        organization_id,
        organization_name,
        display_name,
        business_identity_id,
        function_key,
        enabled,
        max_concurrent,
        active_count,
        workspace_id
    ].

%% ===================================================================
%% 坐席会话详情（§12.4 表 2：GET /cs/sessions/:id 的应用用例）
%% ===================================================================

%% @doc 坐席取会话详情（queue 列表已冻结于 `cs_session_app:list_sessions/2`，
%% 本用例只补 detail；业务规则零复制——读取复用既有 fetch 路径）。
%%
%% 业务前提（授权由认证分流判定）：请求者 `business_identity_id`（认证事实
%% 派生，浏览器不可申报）必须是本 Org 的 enabled customer_service 坐席；
%% 会话租户作用域由 store 同语句裁决（跨 Org 一律 not_found）。
-spec session_detail(integer(), map()) -> {ok, map()} | {error, term()}.
session_detail(OrgId, Params) when is_map(Params) ->
    IdentityId = maps:get(business_identity_id, Params, undefined),
    case pos_int(IdentityId) of
        false ->
            {error, {invalid_identity_id, IdentityId}};
        true ->
            case with_store(Params, fun(Store) -> Store:fetch_seat(OrgId, IdentityId) end) of
                {error, not_found} ->
                    {error, {seat_not_found, IdentityId}};
                {error, _} = Err ->
                    Err;
                {ok, Seat} ->
                    case maps:get(enabled, Seat, false) of
                        false -> {error, seat_disabled};
                        true -> fetch_detail(OrgId, Params)
                    end
            end
    end;
session_detail(_OrgId, _Params) ->
    {error, {invalid_argument, session_detail}}.

fetch_detail(OrgId, Params) ->
    SessionId = maps:get(session_id, Params, undefined),
    case pos_int(SessionId) of
        false ->
            {error, {invalid_session_id, SessionId}};
        true ->
            Clean = maps:with([store, id, workspace_id], Params),
            cs_session_app:fetch_session(OrgId, Clean#{session_id => SessionId})
    end.

%% ===================================================================
%% CS-BE-03：客户上下文只读投影（CS-DEC-01 字段白名单，逐字冻结）
%% ===================================================================

%% @doc 会话锚定的客户上下文（只读、零写副作用）。白名单**仅限**：
%% 掩码名 / 来源 / first_seen / last_seen / 同 Org 历史客服会话列表 /
%% 授权备注事实（EB 无 note 读面，密文材料禁出站）；电话、邮箱、原始
%% 外部身份、跨组织资料、任何密文/凭证/object key 永不投影
%% （越界字段需求一律 BLOCKED_SCOPE_EXPANSION）。
%%
%% 授权（沿用 session_detail 的 seat 门）+ **session ownership**：
%% 请求坐席必须是该会话当前经办 identity——转接后新 Seat 可读、原 Seat
%% 立即失去读权；queued 会话无经办 ⇒ 拒绝（不给 customer_service 粗暴
%% 扩展 sales contact scope）。会话租户作用域由 store 同语句裁决
%% （跨 Org 一律 not_found，不区分不存在与跨租户）。
%%
%% Params：workspace_id / session_id / business_identity_id 必填；
%% after_id / limit 走 C1~C4 冻结键集口径（作用于历史会话页）。
-spec session_context(integer(), map()) -> {ok, map()} | {error, term()}.
session_context(OrgId, Params) when is_map(Params) ->
    case cs_app_support:tenant(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, WorkspaceId} ->
            session_context_seat_gate(OrgId, WorkspaceId, Params)
    end;
session_context(_OrgId, _Params) ->
    {error, {invalid_argument, session_context}}.

session_context_seat_gate(OrgId, WorkspaceId, Params) ->
    IdentityId = maps:get(business_identity_id, Params, undefined),
    case pos_int(IdentityId) of
        false ->
            {error, {invalid_identity_id, IdentityId}};
        true ->
            case with_store(Params, fun(Store) -> Store:fetch_seat(OrgId, IdentityId) end) of
                {error, not_found} ->
                    {error, {seat_not_found, IdentityId}};
                {error, _} = Err ->
                    Err;
                {ok, Seat} ->
                    case maps:get(enabled, Seat, false) of
                        false -> {error, seat_disabled};
                        true -> session_context_owner_gate(OrgId, WorkspaceId, IdentityId, Params)
                    end
            end
    end.

session_context_owner_gate(OrgId, WorkspaceId, IdentityId, Params) ->
    SessionId = maps:get(session_id, Params, undefined),
    case pos_int(SessionId) of
        false ->
            {error, {invalid_session_id, SessionId}};
        true ->
            case
                with_store(Params, fun(Store) ->
                    Store:fetch_session(OrgId, WorkspaceId, SessionId)
                end)
            of
                {error, _} = Err ->
                    Err;
                {ok, Session} ->
                    session_context_owned(OrgId, Session, IdentityId, Params)
            end
    end.

%% ownership 复核：queued 会话 business_identity_id = undefined（无经办）。
session_context_owned(OrgId, Session, IdentityId, Params) ->
    case maps:get(business_identity_id, Session, undefined) of
        IdentityId ->
            assemble_session_context(OrgId, Session, Params);
        Owner ->
            {error, {not_session_owner, IdentityId, Owner}}
    end.

%% 客户上下文投影组装（白名单唯一出口）：
%%   contact  = #{masked_name, first_seen, last_seen}
%%   history  = page_view(sessions, …)（同 contact、同 Org、DESC 键集）
%%   notes    = 授权备注事实行（active；软删排除；零正文零密文）
assemble_session_context(OrgId, Session, Params) ->
    case cs_app_support:page_cursor(Params) of
        {error, _} = Err ->
            Err;
        {ok, AfterId, Limit} ->
            assemble_context_facts(OrgId, Session, AfterId, Limit, Params)
    end.

assemble_context_facts(OrgId, Session, AfterId, Limit, Params) ->
    case context_facts(OrgId, Session, Params) of
        {error, _} = Err ->
            Err;
        {ok, Contact, Source} ->
            assemble_context_pages(OrgId, Session, Contact, Source, AfterId, Limit, Params)
    end.

assemble_context_pages(OrgId, Session, Contact, Source, AfterId, Limit, Params) ->
    ContactId = maps:get(contact_id, Session),
    case session_history(OrgId, ContactId, AfterId, Limit, Params) of
        {error, _} = Err ->
            Err;
        {ok, History} ->
            case contact_notes(OrgId, ContactId, Params) of
                {error, _} = Err2 ->
                    Err2;
                {ok, Notes} ->
                    {ok, #{
                        session_id => maps:get(id, Session),
                        workspace_id => maps:get(workspace_id, Session),
                        source => Source,
                        contact => Contact,
                        history => History,
                        notes => Notes
                    }}
            end
    end.

context_facts(OrgId, Session, Params) ->
    Read = fun(Store) ->
        Store:fetch_session_customer_context(
            OrgId, maps:get(workspace_id, Session), maps:get(id, Session)
        )
    end,
    case with_store(Params, Read) of
        {error, _} = Err ->
            Err;
        {ok, Row} ->
            ContactId = maps:get(contact_id, Session),
            {ok, contact_view(Row, ContactId), cs_app_support:source_of(source_facts(Row))}
    end.

source_facts(Row) ->
    #{
        visit_token_id => maps:get(visit_token_id, Row, undefined),
        created_by_user_id => maps:get(created_by_user_id, Row, undefined)
    }.

contact_view(Row, ContactId) ->
    #{
        masked_name =>
            cs_app_support:masked_name(#{
                contact_subject_mask => maps:get(subject_mask, Row, undefined),
                contact_display_name => maps:get(display_name, Row, undefined),
                contact_id => ContactId
            }),
        first_seen => maps:get(first_seen, Row),
        last_seen => maps:get(last_seen, Row)
    }.

-define(CONTEXT_HISTORY_PROJECTION, [
    id,
    conversation_id,
    workspace_id,
    status,
    version,
    rating,
    queued_at,
    claimed_at,
    closed_at
]).

session_history(OrgId, ContactId, AfterId, Limit, Params) ->
    case
        with_store(Params, fun(Store) ->
            Store:list_session_history_page(OrgId, ContactId, AfterId, Limit)
        end)
    of
        {error, _} = Err ->
            Err;
        {ok, Rows} ->
            cs_app_support:page_view(
                sessions, ?CONTEXT_HISTORY_PROJECTION, Rows, Limit, id
            )
    end.

%% 授权备注事实页：每页固定 20 行（id/created_by_identity_id/created_at）。
-define(CONTEXT_NOTES_LIMIT, 20).

contact_notes(OrgId, ContactId, Params) ->
    case
        with_store(Params, fun(Store) ->
            Store:list_contact_notes_page(OrgId, ContactId, ?CONTEXT_NOTES_LIMIT)
        end)
    of
        {error, _} = Err ->
            Err;
        {ok, Rows} ->
            {ok, [
                #{
                    id => maps:get(id, Row),
                    created_by_identity_id => maps:get(business_identity_id, Row, undefined),
                    created_at => maps:get(created_at, Row)
                }
             || Row <- Rows
            ]}
    end.

%% ===================================================================
%% BE-S01a：坐席上下文清单（GET /api/v1/cs/me/seat-contexts）
%% ===================================================================

%% @doc 一次返回当前用户全部可用坐席上下文（api-surface-freeze）。
%%
%% 聚合源是四张事实表（store 单语句同过滤）：organization_member（active）、
%% workspace（同 Org active）、organization_business_identity_assignment
%% （active + customer_service）、customer_service_seat（enabled）。**不复用**
%% 治理 identity 列表——本用例是坐席面自描述端点，客户端不手填 TSID。
%%
%% 投影：每 Org 一行 `#{organization_id, organization_name, workspaces,
%% business_identity_id, seat_enabled, capabilities}`；workspaces 是
%% `#{id, name}` 列表；capabilities 是坐席能力清单（seat_enabled 才非空——
%% 镜像 EB 侧 V1 经办业务能力冻结集 §五，见 eb_pg_auth_facts:
%% business_permission_set；跨 feature 直引其基础设施是被禁的，此处冻结镜像
%% 并以注释锚定真源）。
-spec seat_contexts(map()) -> {ok, map()} | {error, term()}.
seat_contexts(#{user_id := UserId} = Params) when is_integer(UserId) ->
    case with_store(Params, fun(Store) -> Store:list_seat_org_contexts(UserId) end) of
        {error, _} = Err ->
            Err;
        {ok, Rows} ->
            {ok, #{
                contexts => [context_row(Row) || Row <- Rows],
                user_id => UserId
            }}
    end;
seat_contexts(_Params) ->
    {error, {invalid_argument, seat_contexts}}.

context_row(Row) ->
    SeatEnabled = maps:get(seat_enabled, Row, false) =:= true,
    #{
        organization_id => maps:get(organization_id, Row),
        organization_name => maps:get(organization_name, Row),
        workspaces => maps:get(workspaces, Row, []),
        business_identity_id => maps:get(business_identity_id, Row),
        seat_enabled => SeatEnabled,
        capabilities => seat_capabilities(SeatEnabled)
    }.

%% seat enabled 才有坐席能力（suspended seat 的 capabilities 为空——与
%% cs_auth 的 seat_enabled_gate 同口径：停用坐席拿不到任何动作语义）。
seat_capabilities(true) ->
    [
        <<"conversation.read">>,
        <<"conversation.write">>,
        <<"message.write">>,
        <<"asset.read">>,
        <<"asset.write">>
    ];
seat_capabilities(false) ->
    [].

%% ===================================================================
%% BE-S01a：转接目标最小投影（GET .../transfer-targets）
%% ===================================================================

%% @doc 同 Org 其他可用坐席的最小投影（identity id / 显示名 / 可用状态）。
%% 排除调用者本人（IdentityId 是认证派生键）；无 owner/admin 权限要求——
%% 坐席转接是工作台动作，不是治理动作。available = active 会话数未达
%% max_concurrent（store 同语句计数）。after_id/limit 沿用 C1~C4 冻结口径。
-spec transfer_targets(integer(), map()) -> {ok, map()} | {error, term()}.
transfer_targets(OrgId, #{business_identity_id := IdentityId} = Params) when
    is_integer(OrgId), is_integer(IdentityId)
->
    case cs_app_support:page_cursor(Params) of
        {error, _} = Err ->
            Err;
        {ok, AfterId, Limit} ->
            case
                with_store(Params, fun(Store) ->
                    Store:list_transfer_targets_page(OrgId, IdentityId, AfterId, Limit)
                end)
            of
                {error, _} = Err2 ->
                    Err2;
                {ok, Rows} ->
                    cs_app_support:page_view(
                        targets,
                        [business_identity_id, display_name, available],
                        [target_row(Row) || Row <- Rows],
                        Limit,
                        business_identity_id
                    )
            end
    end;
transfer_targets(_OrgId, _Params) ->
    {error, {invalid_argument, transfer_targets}}.

target_row(Row) ->
    ActiveCount = maps:get(active_count, Row, 0),
    MaxConcurrent = maps:get(max_concurrent, Row, 1),
    #{
        business_identity_id => maps:get(business_identity_id, Row),
        display_name => maps:get(display_name, Row),
        available => ActiveCount < MaxConcurrent
    }.

%% ===================================================================
%% BE-S01b：平台面事务化开通/修复坐席（api-surface-freeze admin_provisioning）
%% ===================================================================

%% @doc 为 (Org, Workspace, user) 开通/修复坐席：customer_service identity +
%% active assignment + enabled seat 在**单数据库事务**内创建或修复
%% （store `provision_seat/3`：任一步失败全回滚）。
%%
%% Params：workspace_id / user_id（目标成员）/ display_name（identity 显示名）
%% 必填；max_concurrent 可选（默认 1）；adm_user_id 必填（认证派生键，
%% 进审计 detail 的 actor 记录——事件表 FK 只认 "user"，admin id 不写 actor 列）。
%% 幂等：重复对同一 user+org+workspace 调用返回既有事实，不重复创建。
%%
%% 业务前提：目标 user 必须是本 Org 的 active member（store 首语句同语句
%% 裁决——identity/assignment 的 INSERT 语义即「把成员变坐席」，非成员在
%% 事实层不存在，由 guard 语句显式拒绝，返回 `{error, {not_found, member}}`）。
-spec provision_seat(integer(), map()) -> {ok, map()} | {error, term()}.
provision_seat(OrgId, #{workspace_id := WorkspaceId} = Params) when
    is_integer(OrgId), is_integer(WorkspaceId), is_map(Params)
->
    UserId = maps:get(user_id, Params, undefined),
    DisplayName = maps:get(display_name, Params, undefined),
    AdmId = maps:get(adm_user_id, Params, undefined),
    case
        pos_int(UserId) andalso cs_app_support:non_empty_binary(DisplayName) andalso pos_int(AdmId)
    of
        false ->
            {error, {invalid_argument, provision_seat}};
        true ->
            Provision = #{
                user_id => UserId,
                display_name => DisplayName,
                max_concurrent => max_concurrent(Params),
                adm_user_id => AdmId
            },
            case
                with_store(Params, fun(Store) ->
                    Store:provision_seat(OrgId, WorkspaceId, Provision)
                end)
            of
                {error, {not_found, workspace}} ->
                    %% workspace 不在本 Org / 非 active：作用域不存在（404 面）。
                    {error, {not_found, workspace}};
                {error, {not_found, member}} ->
                    {error, {not_found, member}};
                {error, _} = Err ->
                    Err;
                {ok, Result} ->
                    {ok, Result#{seat_enabled => maps:get(enabled, maps:get(seat, Result), false)}}
            end
    end;
provision_seat(_OrgId, _Params) ->
    {error, {invalid_argument, provision_seat}}.

max_concurrent(Params) ->
    case maps:get(max_concurrent, Params, undefined) of
        N when is_integer(N), N > 0 -> N;
        _ -> 1
    end.

%% ===================================================================
%% 内部辅助
%% ===================================================================

append_event(Params, OrgId, WorkspaceId, Event) ->
    cs_app_support:append_event(Params, OrgId, Event#{workspace_id => WorkspaceId}).

with_store(Params, Fun) ->
    cs_app_support:with_store(Params, Fun).

pos_int(V) ->
    cs_app_support:pos_int(V).
