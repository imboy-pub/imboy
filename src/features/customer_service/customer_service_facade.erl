%%% @doc Customer Service Feature 的公开 API（铁律 3：公开 API 只进 Facade）。
%%%
%%% 依据：plan v4.1 §5.2（客服 API 最小合同）、EB-D08（feature 边界）、
%%% EB-D09（租户/平台两套 Handler 复用同一 application）。
%%%
%%% 本模块**只做两件事**：参数收敛 + 委派。
%%%   * 每个对外函数形状统一为 `(OrgId, Params)`，返回委派结果原样透传；
%%%   * 收敛只做「形状与类型」判定（OrgId 必为整数；资源级必填键存在且类型
%%%     正确），不做任何业务规则、不读库、不写 SQL、不发布事件；
%%%   * 委派目标只能是本 feature 的 `application/` 用例模块（`cs_*_app`）。
%%%
%%% **真源边界（CS-01-A03 / EB-D08）**：客户、消息、备注、附件的写入一律经
%%% `enterprise_business_facade`（由 application 层 `cs_session_app` 调用）；
%%% 本 feature 不建消息/附件副本。本 facade 自身不直接引用 enterprise 模块。
-module(customer_service_facade).

-export([
    %% seat
    create_seat/2,
    suspend_seat/2,
    resume_seat/2,
    fetch_seat/2,
    list_dispatchable_seats/2,
    provision_seat/2,
    %% session
    open_session/2,
    fetch_session/2,
    claim/2,
    transfer/2,
    close/2,
    rate/2,
    append_session_message/2,
    list_contact_sessions/2,
    list_sessions/2,
    %% shop key / visit token
    create_shop_key/2,
    revoke_shop_key/2,
    verify_shop_key/2,
    list_shop_keys/2,
    issue_visit_token/2,
    revoke_visit_token/2,
    verify_visit_token/2,
    list_visit_tokens/2,
    %% widget installation 管理
    list_widget_installations/2,
    create_widget_installation/2,
    revoke_widget_installation/2,
    %% widget（CSB-02：application 合同；HTTP 面归 CSB-03）
    widget_bootstrap/2,
    %% BE-W01 A05：动态 frame HTML 的公开 installation 投影
    widget_frame_html/2,
    widget_identity_exchange/2,
    widget_create_session/2,
    widget_list_sessions/2,
    widget_history_after/2,
    widget_visitor_message/2,
    widget_rate/2,
    widget_asset_upload/2,
    widget_asset_confirm/2,
    widget_asset_content/2,
    %% seat 会话详情（§12.4 表 2 补缺）
    seat_session_detail/2,
    %% CSB-02R：坐席工作台（队列 GET + active/closed 列表，共用 seat_session_page）
    seat_session_queue/2,
    seat_session_list/2,
    %% BE-S01a：坐席上下文清单 / 转接目标最小投影 / SSE 占位
    seat_contexts/2,
    transfer_targets/2,
    seat_events/2
]).

%% ===================================================================
%% seat
%% ===================================================================

-spec create_seat(integer(), map()) -> term().
create_seat(OrgId, #{business_identity_id := IdentityId} = Params) when
    is_integer(OrgId), is_integer(IdentityId), is_map(Params)
->
    cs_seat_app:create_seat(OrgId, Params);
create_seat(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_argument, create_seat}};
create_seat(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

-spec suspend_seat(integer(), map()) -> term().
suspend_seat(OrgId, #{business_identity_id := IdentityId, at := At} = Params) when
    is_integer(OrgId), is_integer(IdentityId), is_integer(At), is_map(Params)
->
    cs_seat_app:suspend_seat(OrgId, Params);
suspend_seat(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_argument, suspend_seat}};
suspend_seat(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

-spec resume_seat(integer(), map()) -> term().
resume_seat(OrgId, #{business_identity_id := IdentityId, at := At} = Params) when
    is_integer(OrgId), is_integer(IdentityId), is_integer(At), is_map(Params)
->
    cs_seat_app:resume_seat(OrgId, Params);
resume_seat(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_argument, resume_seat}};
resume_seat(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

-spec fetch_seat(integer(), map()) -> term().
fetch_seat(OrgId, #{business_identity_id := IdentityId} = Params) when
    is_integer(OrgId), is_integer(IdentityId), is_map(Params)
->
    cs_seat_app:fetch_seat(OrgId, Params);
fetch_seat(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_argument, fetch_seat}};
fetch_seat(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

-spec list_dispatchable_seats(integer(), map()) -> term().
list_dispatchable_seats(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    cs_seat_app:list_dispatchable_seats(OrgId, Params);
list_dispatchable_seats(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

%% ===================================================================
%% session
%% ===================================================================

-spec open_session(integer(), map()) -> term().
open_session(
    OrgId, #{contact_id := ContactId, conversation_id := ConversationId} = Params
) when
    is_integer(OrgId), is_integer(ContactId), is_integer(ConversationId), is_map(Params)
->
    cs_session_app:open_session(OrgId, Params);
open_session(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_argument, open_session}};
open_session(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

-spec fetch_session(integer(), map()) -> term().
fetch_session(OrgId, #{session_id := SessionId} = Params) when
    is_integer(OrgId), is_integer(SessionId), is_map(Params)
->
    cs_session_app:fetch_session(OrgId, Params);
fetch_session(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_argument, fetch_session}};
fetch_session(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

-spec claim(integer(), map()) -> term().
claim(
    OrgId, #{session_id := SessionId, expected_version := ExpectedVersion, at := At} = Params
) when
    is_integer(OrgId),
    is_integer(SessionId),
    is_integer(ExpectedVersion),
    is_integer(At),
    is_map(Params)
->
    cs_session_app:claim(OrgId, Params);
claim(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_argument, claim}};
claim(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

-spec transfer(integer(), map()) -> term().
transfer(
    OrgId,
    #{
        session_id := SessionId,
        to_identity_id := ToIdentityId,
        expected_version := ExpectedVersion,
        at := At
    } = Params
) when
    is_integer(OrgId),
    is_integer(SessionId),
    is_integer(ToIdentityId),
    is_integer(ExpectedVersion),
    is_integer(At),
    is_map(Params)
->
    cs_session_app:transfer(OrgId, Params);
transfer(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_argument, transfer}};
transfer(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

-spec close(integer(), map()) -> term().
close(
    OrgId, #{session_id := SessionId, expected_version := ExpectedVersion, at := At} = Params
) when
    is_integer(OrgId),
    is_integer(SessionId),
    is_integer(ExpectedVersion),
    is_integer(At),
    is_map(Params)
->
    cs_session_app:close(OrgId, Params);
close(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_argument, close}};
close(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

-spec rate(integer(), map()) -> term().
rate(
    OrgId,
    #{
        session_id := SessionId,
        rating := Rating,
        expected_version := ExpectedVersion,
        at := At
    } = Params
) when
    is_integer(OrgId),
    is_integer(SessionId),
    is_integer(Rating),
    is_integer(ExpectedVersion),
    is_integer(At),
    is_map(Params)
->
    cs_session_app:rate(OrgId, Params);
rate(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_argument, rate}};
rate(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

%% @doc 客服会话消息（A03）：委派给 application，后者经
%% `enterprise_business_facade:append_message` 写 enterprise 真源。
-spec append_session_message(integer(), map()) -> term().
append_session_message(
    OrgId, #{session_id := SessionId, client_msg_id := ClientMsgId, body := Body} = Params
) when
    is_integer(OrgId),
    is_integer(SessionId),
    is_binary(ClientMsgId),
    is_binary(Body),
    is_map(Params)
->
    cs_session_app:append_session_message(OrgId, Params);
append_session_message(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_argument, append_session_message}};
append_session_message(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

-spec list_contact_sessions(integer(), map()) -> term().
list_contact_sessions(OrgId, #{contact_id := ContactId} = Params) when
    is_integer(OrgId), is_integer(ContactId), is_map(Params)
->
    cs_session_app:list_contact_sessions(OrgId, Params);
list_contact_sessions(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_argument, list_contact_sessions}};
list_contact_sessions(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

%% @doc C1（contracts-w2）：平台 session 列表（只读；租户/平台共用同一用例，
%% CS-02-A02）。Params：workspace_id 必填（handler 强制的 face 级参数）；
%% status / after_id / limit 可选（application 白名单与范围校验）。
-spec list_sessions(integer(), map()) -> term().
list_sessions(OrgId, #{workspace_id := WorkspaceId} = Params) when
    is_integer(OrgId), is_integer(WorkspaceId), is_map(Params)
->
    cs_session_app:list_sessions(OrgId, Params);
list_sessions(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_argument, list_sessions}};
list_sessions(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

%% ===================================================================
%% shop key / visit token
%% ===================================================================

-spec create_shop_key(integer(), map()) -> term().
create_shop_key(OrgId, #{secret := Secret} = Params) when
    is_integer(OrgId), is_binary(Secret), is_map(Params)
->
    cs_access_app:create_shop_key(OrgId, Params);
create_shop_key(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_argument, create_shop_key}};
create_shop_key(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

-spec revoke_shop_key(integer(), map()) -> term().
revoke_shop_key(OrgId, #{id := KeyId, at := At} = Params) when
    is_integer(OrgId), is_integer(KeyId), is_integer(At), is_map(Params)
->
    cs_access_app:revoke_shop_key(OrgId, Params);
revoke_shop_key(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_argument, revoke_shop_key}};
revoke_shop_key(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

-spec verify_shop_key(integer(), map()) -> term().
verify_shop_key(OrgId, #{secret := Secret} = Params) when
    is_integer(OrgId), is_binary(Secret), is_map(Params)
->
    cs_access_app:verify_shop_key(OrgId, Params);
verify_shop_key(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_argument, verify_shop_key}};
verify_shop_key(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

%% @doc C2（contracts-w2）：shop key 治理列表（投影白名单由 application 裁剪，
%% digest 绝不出 facade）。
-spec list_shop_keys(integer(), map()) -> term().
list_shop_keys(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    cs_access_app:list_shop_keys(OrgId, Params);
list_shop_keys(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

-spec issue_visit_token(integer(), map()) -> term().
issue_visit_token(
    OrgId, #{contact_id := ContactId, secret := Secret, expires_at := ExpiresAt} = Params
) when
    is_integer(OrgId),
    is_integer(ContactId),
    is_binary(Secret),
    is_integer(ExpiresAt),
    is_map(Params)
->
    cs_access_app:issue_visit_token(OrgId, Params);
issue_visit_token(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_argument, issue_visit_token}};
issue_visit_token(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

-spec revoke_visit_token(integer(), map()) -> term().
revoke_visit_token(OrgId, #{id := TokenId, at := At} = Params) when
    is_integer(OrgId), is_integer(TokenId), is_integer(At), is_map(Params)
->
    cs_access_app:revoke_visit_token(OrgId, Params);
revoke_visit_token(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_argument, revoke_visit_token}};
revoke_visit_token(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

-spec verify_visit_token(integer(), map()) -> term().
verify_visit_token(OrgId, #{secret := Secret} = Params) when
    is_integer(OrgId), is_binary(Secret), is_map(Params)
->
    cs_access_app:verify_visit_token(OrgId, Params);
verify_visit_token(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_argument, verify_visit_token}};
verify_visit_token(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

%% @doc C3（contracts-w2）：visit token 治理列表（投影白名单由 application
%% 裁剪，token_digest 绝不出 facade）。
-spec list_visit_tokens(integer(), map()) -> term().
list_visit_tokens(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    cs_access_app:list_visit_tokens(OrgId, Params);
list_visit_tokens(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

%% ===================================================================
%% widget installation 管理
%% ===================================================================

-spec list_widget_installations(integer(), map()) -> term().
list_widget_installations(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    cs_widget_app:list_installations(OrgId, Params);
list_widget_installations(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

-spec create_widget_installation(integer(), map()) -> term().
create_widget_installation(
    OrgId,
    #{display_name := DisplayName, allowed_origins := AllowedOrigins, consent_version := Consent} =
        Params
) when
    is_integer(OrgId),
    is_binary(DisplayName),
    is_list(AllowedOrigins),
    is_binary(Consent),
    is_map(Params)
->
    cs_widget_app:create_installation(OrgId, Params);
create_widget_installation(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_argument, create_widget_installation}};
create_widget_installation(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

-spec revoke_widget_installation(integer(), map()) -> term().
revoke_widget_installation(OrgId, #{id := Id, at := At} = Params) when
    is_integer(OrgId), is_integer(Id), is_integer(At), is_map(Params)
->
    cs_widget_app:revoke_installation(OrgId, Params);
revoke_widget_installation(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_argument, revoke_widget_installation}};
revoke_widget_installation(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

%% ===================================================================
%% widget（CSB-02）：参数收敛只做形状判定；服务端派生事实（contact /
%% conversation / workspace / identity / 时钟 / HMAC key）由 application 从
%% Ctx 注入项与令牌作用域取得，浏览器申报值一律不成为授权事实。
%% ===================================================================

-spec widget_bootstrap(integer(), map()) -> term().
widget_bootstrap(OrgId, #{public_widget_id := PublicId, origin := Origin} = Params) when
    is_integer(OrgId), is_binary(PublicId), is_binary(Origin), is_map(Params)
->
    case cs_widget_env:merge_bootstrap(OrgId, Params) of
        {ok, Merged} -> cs_widget_app:bootstrap(OrgId, Merged);
        {error, _} = Err -> Err
    end;
widget_bootstrap(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_argument, widget_bootstrap}};
widget_bootstrap(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

%% BE-W01 A05：动态 frame HTML 端点的公开 installation 投影——零凭证面
%% （iframe src 导航落点）。Params：installation_id 必填；返回
%% #{id, public_widget_id, allowed_origins}（allowed_origins 已归一化），
%% revoked/不存在一律 {error, not_found}（handler 映射 404，kill switch
%% 不显形）。凭据/接触面零 secret。
-spec widget_frame_html(integer(), map()) -> term().
widget_frame_html(OrgId, #{installation_id := InstallationId} = Params) when
    is_integer(OrgId), is_integer(InstallationId), is_map(Params)
->
    cs_widget_app:public_frame_installation(OrgId, Params);
widget_frame_html(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_argument, widget_frame_html}};
widget_frame_html(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

%% BE-W01 A06：identity/exchange 第一阶段 capability_disabled 收敛
%% （api-surface-freeze.json：返回明确能力状态；签名断言流程保留，
%% 由 cs_widget_identity_exchange_enabled 显式开启，默认 false）。
-spec widget_identity_exchange(integer(), map()) -> term().
widget_identity_exchange(
    OrgId, #{installation_id := InstallationId, assertion := Assertion} = Params
) when
    is_integer(OrgId), is_integer(InstallationId), is_map(Assertion), is_map(Params)
->
    case cs_widget_env:identity_exchange_enabled() of
        true ->
            widget_identity_exchange_enabled(OrgId, InstallationId, Assertion, Params);
        false ->
            %% 第一阶段：签名身份换绑未开放（capability_disabled；HTTP 403 +
            %% envelope tag capability_disabled.identity_exchange）。既有签名
            %% 断言链原样保留在 true 分支，未删除。
            {error, {capability_disabled, identity_exchange}}
    end;
widget_identity_exchange(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_argument, widget_identity_exchange}};
widget_identity_exchange(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

widget_identity_exchange_enabled(OrgId, _InstallationId, _Assertion, Params) ->
    case cs_widget_env:merge_identity_exchange(OrgId, Params) of
        {ok, Merged} -> cs_widget_app:identity_exchange(OrgId, Merged);
        {error, _} = Err -> Err
    end.

-spec widget_create_session(integer(), map()) -> term().
widget_create_session(OrgId, #{installation_id := InstallationId, secret := Secret} = Params) when
    is_integer(OrgId), is_integer(InstallationId), is_binary(Secret), is_map(Params)
->
    case cs_widget_env:merge_session(OrgId, Params) of
        {ok, Merged} -> cs_widget_session_app:create_session(OrgId, Merged);
        {error, _} = Err -> Err
    end;
widget_create_session(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_argument, widget_create_session}};
widget_create_session(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

-spec widget_list_sessions(integer(), map()) -> term().
widget_list_sessions(OrgId, #{installation_id := InstallationId, secret := Secret} = Params) when
    is_integer(OrgId), is_integer(InstallationId), is_binary(Secret), is_map(Params)
->
    case cs_widget_env:merge_visitor(OrgId, Params) of
        {ok, Merged} -> cs_widget_session_app:list_sessions(OrgId, Merged);
        {error, _} = Err -> Err
    end;
widget_list_sessions(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_argument, widget_list_sessions}};
widget_list_sessions(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

-spec widget_history_after(integer(), map()) -> term().
widget_history_after(
    OrgId, #{installation_id := InstallationId, secret := Secret, session_id := SessionId} = Params
) when
    is_integer(OrgId),
    is_integer(InstallationId),
    is_binary(Secret),
    is_integer(SessionId),
    is_map(Params)
->
    case cs_widget_env:merge_visitor(OrgId, Params) of
        {ok, Merged} -> cs_widget_session_app:history_after(OrgId, Merged);
        {error, _} = Err -> Err
    end;
widget_history_after(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_argument, widget_history_after}};
widget_history_after(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

-spec widget_visitor_message(integer(), map()) -> term().
widget_visitor_message(
    OrgId,
    #{
        installation_id := InstallationId,
        secret := Secret,
        session_id := SessionId,
        client_msg_id := ClientMsgId,
        body := Body
    } = Params
) when
    is_integer(OrgId),
    is_integer(InstallationId),
    is_binary(Secret),
    is_integer(SessionId),
    is_binary(ClientMsgId),
    is_binary(Body),
    is_map(Params)
->
    case cs_widget_env:merge_visitor(OrgId, Params) of
        {ok, Merged} -> cs_widget_session_app:visitor_message(OrgId, Merged);
        {error, _} = Err -> Err
    end;
widget_visitor_message(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_argument, widget_visitor_message}};
widget_visitor_message(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

-spec widget_rate(integer(), map()) -> term().
widget_rate(
    OrgId,
    #{
        installation_id := InstallationId,
        secret := Secret,
        session_id := SessionId,
        rating := Rating,
        expected_version := ExpectedVersion
    } = Params
) when
    is_integer(OrgId),
    is_integer(InstallationId),
    is_binary(Secret),
    is_integer(SessionId),
    is_integer(Rating),
    is_integer(ExpectedVersion),
    is_map(Params)
->
    case cs_widget_env:merge_visitor(OrgId, Params) of
        {ok, Merged} -> cs_widget_session_app:rate(OrgId, Merged);
        {error, _} = Err -> Err
    end;
widget_rate(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_argument, widget_rate}};
widget_rate(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

-spec widget_asset_upload(integer(), map()) -> term().
widget_asset_upload(
    OrgId,
    #{
        installation_id := InstallationId,
        secret := Secret,
        session_id := SessionId,
        mime := Mime,
        size_bytes := SizeBytes
    } = Params
) when
    is_integer(OrgId),
    is_integer(InstallationId),
    is_binary(Secret),
    is_integer(SessionId),
    is_binary(Mime),
    is_integer(SizeBytes),
    is_map(Params)
->
    case cs_widget_env:merge_visitor(OrgId, Params) of
        {ok, Merged} -> cs_widget_session_app:asset_presign(OrgId, Merged);
        {error, _} = Err -> Err
    end;
widget_asset_upload(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_argument, widget_asset_upload}};
widget_asset_upload(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

-spec widget_asset_confirm(integer(), map()) -> term().
widget_asset_confirm(
    OrgId, #{installation_id := InstallationId, secret := Secret, upload_ref := UploadRef} = Params
) when
    is_integer(OrgId),
    is_integer(InstallationId),
    is_binary(Secret),
    is_binary(UploadRef),
    is_map(Params)
->
    case cs_widget_env:merge_visitor(OrgId, Params) of
        {ok, Merged} -> cs_widget_session_app:asset_confirm(OrgId, Merged);
        {error, _} = Err -> Err
    end;
widget_asset_confirm(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_argument, widget_asset_confirm}};
widget_asset_confirm(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

%% BE-S01b：访客附件内容代理（GET .../sessions/:session_id/assets/:asset_id/content，
%% api-surface-freeze widget_apis）：逐项校验链（visit token → installation 事实
%% → 默认 Workspace → 会话归属）后经 enterprise 真源的 contact 分支取流。
%% 返回体只含 mime/size/hash/字节（asset content 投影），**永不**暴露 storage
%% URL / object key。asset_id 是路径绑定（服务端解析），浏览器不申报会话外的
%% 任何作用域。
-spec widget_asset_content(integer(), map()) -> term().
widget_asset_content(
    OrgId,
    #{
        installation_id := InstallationId,
        secret := Secret,
        session_id := SessionId,
        asset_id := AssetId
    } = Params
) when
    is_integer(OrgId),
    is_integer(InstallationId),
    is_binary(Secret),
    is_integer(SessionId),
    is_integer(AssetId),
    is_map(Params)
->
    case cs_widget_env:merge_visitor(OrgId, Params) of
        {ok, Merged} -> cs_widget_session_app:asset_content(OrgId, Merged);
        {error, _} = Err -> Err
    end;
widget_asset_content(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_argument, widget_asset_content}};
widget_asset_content(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

%% @doc 坐席会话详情（§12.4 表 2 补缺）：business_identity_id 是认证事实
%% 派生键（HTTP 面服务端注入），queue 列表沿用既有 list_sessions。
-spec seat_session_detail(integer(), map()) -> term().
seat_session_detail(
    OrgId, #{business_identity_id := IdentityId, session_id := SessionId} = Params
) when
    is_integer(OrgId), is_integer(IdentityId), is_integer(SessionId), is_map(Params)
->
    cs_seat_app:session_detail(OrgId, Params);
seat_session_detail(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_argument, seat_session_detail}};
seat_session_detail(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

%% ===================================================================
%% CSB-02R：坐席工作台（队列 GET + active/closed 列表）
%% ===================================================================

%% @doc 坐席队列视图（GET /api/v1/cs/sessions/queue）：status 冻结 queued，
%% 与平台面 list_sessions 共用 `cs_session_app:seat_session_page`（业务规则
%% 零复制）。坐席作用域（Org + customer_service 职能 assignment）由 cs_auth
%% 在进用例之前裁决。
-spec seat_session_queue(integer(), map()) -> term().
seat_session_queue(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    cs_session_app:seat_session_page(OrgId, Params#{status => <<"queued">>});
seat_session_queue(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

%% @doc 坐席 active/closed 两视图（GET /api/v1/cs/seats/sessions）。queued
%% 视图冻结在队列端点——本入口显式拒绝（`{invalid_status,*}` 422），两视图
%% 语义不漂移。
-spec seat_session_list(integer(), map()) -> term().
seat_session_list(OrgId, #{status := Status} = Params) when
    is_integer(OrgId), is_map(Params)
->
    case Status of
        S when S =:= <<"active">>; S =:= <<"closed">>; S =:= active; S =:= closed ->
            cs_session_app:seat_session_page(OrgId, Params);
        Other ->
            {error, {invalid_status, Other}}
    end;
seat_session_list(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_argument, seat_session_list}};
seat_session_list(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

%% ===================================================================
%% BE-S01a：坐席上下文清单 / 转接目标 / SSE 占位（api-surface-freeze）
%% ===================================================================

%% @doc 坐席上下文清单（GET /api/v1/cs/me/seat-contexts）：主体自身作用域——
%% OrgId 形参不读（org 作用域由 actor 派生；handler 的 self 分支传 0 占位）。
%% 一次返回当前用户全部 active member 的 Org、每 Org 的 active Workspace[]、
%% active customer_service identity、seat enabled 与 capabilities。数据从
%% org membership / eb identity / assignment / seat 事实表聚合（store 同语句
%% 过滤 active），不复用治理 identity 列表。
-spec seat_contexts(integer(), map()) -> term().
seat_contexts(_OrgId, #{actor_user_id := UserId} = Params) when
    is_integer(UserId), is_map(Params)
->
    cs_seat_app:seat_contexts(Params#{user_id => UserId});
seat_contexts(_OrgId, Params) when is_map(Params) ->
    {error, {invalid_argument, seat_contexts}};
seat_contexts(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

%% @doc 转接目标最小投影（GET /api/v1/cs/organizations/:org_id/transfer-targets）：
%% 同 Org 其他可用坐席（identity id / 显示名 / 可用状态），排除调用者本人
%% （business_identity_id 是认证派生键，客户端不可申报）；无 owner/admin
%% 权限要求。分页 after_id/limit 沿用 C1~C4 冻结口径。
-spec transfer_targets(integer(), map()) -> term().
transfer_targets(OrgId, #{business_identity_id := IdentityId} = Params) when
    is_integer(OrgId), is_integer(IdentityId), is_map(Params)
->
    cs_seat_app:transfer_targets(OrgId, Params);
transfer_targets(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_argument, transfer_targets}};
transfer_targets(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

%% @doc 坐席 SSE 事件流（GET /api/v1/cs/organizations/:org_id/seats/me/events）：
%% BE-S01b 流式实现。handler 把 Last-Event-ID 头（优先）与 after_id 查询参数
%% 归一为同一 `after_id` 键（缺流首连无该键）；workspace_id 是 handler 强制的
%% face 级必填。每次调用返回一页确定性事实（游标状态 + 合同信封列表），流式
%% 写出/心跳/撤权关流在 handler 分支（see cs_seat_event_app 模块文档）。
-spec seat_events(integer(), map()) -> term().
seat_events(
    OrgId, #{workspace_id := WorkspaceId, business_identity_id := IdentityId} = Params
) when
    is_integer(OrgId), is_integer(WorkspaceId), is_integer(IdentityId), is_map(Params)
->
    cs_seat_event_app:events(OrgId, Params);
seat_events(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_argument, seat_events}};
seat_events(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

%% @doc 平台面事务化开通/修复坐席（POST /api/adm/.../provisioning；
%% api-surface-freeze admin_provisioning）：单事务内 identity + assignment +
%% enabled seat 的创建/修复 + 不可抵赖审计；幂等（重复调用返回既有事实）。
%% adm_user_id 是认证派生键（Admin session），进审计 detail 不进 actor 列。
-spec provision_seat(integer(), map()) -> term().
provision_seat(
    OrgId,
    #{workspace_id := WorkspaceId, user_id := UserId, adm_user_id := AdmId} = Params
) when
    is_integer(OrgId), is_integer(WorkspaceId), is_integer(UserId), is_integer(AdmId), is_map(Params)
->
    cs_seat_app:provision_seat(OrgId, Params);
provision_seat(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_argument, provision_seat}};
provision_seat(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.
