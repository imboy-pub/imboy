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
    widget_identity_exchange/2,
    widget_create_session/2,
    widget_list_sessions/2,
    widget_history_after/2,
    widget_visitor_message/2,
    widget_rate/2,
    widget_asset_upload/2,
    widget_asset_confirm/2,
    %% seat 会话详情（§12.4 表 2 补缺）
    seat_session_detail/2,
    %% CSB-02R：坐席工作台（队列 GET + active/closed 列表，共用 seat_session_page）
    seat_session_queue/2,
    seat_session_list/2
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

-spec widget_identity_exchange(integer(), map()) -> term().
widget_identity_exchange(
    OrgId, #{installation_id := InstallationId, assertion := Assertion} = Params
) when
    is_integer(OrgId), is_integer(InstallationId), is_map(Assertion), is_map(Params)
->
    case cs_widget_env:merge_identity_exchange(OrgId, Params) of
        {ok, Merged} -> cs_widget_app:identity_exchange(OrgId, Merged);
        {error, _} = Err -> Err
    end;
widget_identity_exchange(OrgId, Params) when is_integer(OrgId), is_map(Params) ->
    {error, {invalid_argument, widget_identity_exchange}};
widget_identity_exchange(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

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
