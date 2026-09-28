%%% @doc Widget 接入的机械辅助与跨裁剪调用统一出口（CSB-02；镜像
%%% `cs_app_support` 的「机械动作、零业务规则」与 F-EB10-1 的 ifdef 先例）。
%%%
%%% 本模块**始终编译**，而下述被调模块（cs_app_support / cs_session_app /
%%% enterprise_business_facade）随特性裁剪（ERLC_EXCLUDE）不参与未选中档的
%%% 编译；Erlang 远程调用是运行期解析的——裸调用会在未选中档 `undef`。
%%% 因此每个跨裁剪调用必须包在 `-ifdef(IMBOY_FEATURE_<特性>)` 的活动分支里；
%%% 未选中档 fail-closed（显式不可用错误），不做任何客服侧兜底副本。
%%%
%%% 另承载 widget 用例共享的机械辅助：令牌 digest/校验、服务端事实解析
%%% （默认 Workspace / 接待 identity——缺注入 fail-closed）、enterprise
%%% facade 参数收敛。零业务规则；业务判定在 domain `cs_widget` 与各用例。
-module(cs_widget_support).

-export([
    pos_int/1,
    non_empty_binary/1,
    with_store/2,
    new_id/2,
    append_event/4,
    token_digest/2,
    token_ttl/1,
    new_token_secret/1,
    resolve_default_workspace/2,
    intake_identity/1,
    eb_params/2,
    optional_keys/3,
    fetch_installation/3,
    installation_active/1,
    verify_bootstrap_token/2,
    derive_org_by_token/1,
    token_usable/2,
    session_open/2,
    session_list_contact/2,
    session_fetch/2,
    session_append_message/2,
    session_rate/2,
    eb_create_contact/2,
    eb_open_conversation/2,
    eb_list_messages/2,
    eb_request_presign/2,
    eb_confirm_asset/2,
    %% BE-PATCH-01：访客附件字节上传代理
    eb_put_object/2,
    api_base/0,
    %% BE-S01b：访客附件内容代理
    eb_content_stream/2
]).

-include("generated/imboy_product_features.hrl").

%% bootstrap 令牌缺省有效期（秒）——「短期」的冻结缺省；可注入覆盖。
-define(DEFAULT_TOKEN_TTL, 3600).

%% ===================================================================
%% 机械辅助（纯形状 / 参数收敛）
%% ===================================================================

token_digest(Params, Value) ->
    case maps:get(digest, Params, undefined) of
        Fun when is_function(Fun, 1) -> Fun(Value);
        _ -> binary:encode_hex(crypto:hash(sha256, Value))
    end.

token_ttl(Params) ->
    case maps:get(bootstrap_token_ttl, Params, undefined) of
        Ttl when is_integer(Ttl), Ttl > 0 -> Ttl;
        _ -> ?DEFAULT_TOKEN_TTL
    end.

new_token_secret(Params) ->
    case maps:get(new_secret, Params, undefined) of
        Fun when is_function(Fun, 0) -> Fun();
        _ -> binary:encode_hex(crypto:strong_rand_bytes(32))
    end.

%% 服务端事实：默认 Workspace（EB `default_workspace` fun/1 同款，fail-closed）。
resolve_default_workspace(OrgId, Params) ->
    case maps:get(default_workspace, Params, undefined) of
        Fun when is_function(Fun, 1) ->
            case Fun(OrgId) of
                {ok, WorkspaceId} when is_integer(WorkspaceId), WorkspaceId > 0 ->
                    {ok, WorkspaceId};
                WorkspaceId when is_integer(WorkspaceId), WorkspaceId > 0 ->
                    {ok, WorkspaceId};
                _ ->
                    {error, default_workspace_unresolved}
            end;
        _ ->
            {error, {missing_injection, default_workspace}}
    end.

%% 服务端事实：本 Org 的接待 identity（浏览器不可申报；缺注入 fail-closed）。
intake_identity(Params) ->
    case maps:get(intake_business_identity_id, Params, undefined) of
        Id when is_integer(Id), Id > 0 -> {ok, Id};
        Other -> {error, {missing_injection, {intake_business_identity_id, Other}}}
    end.

%% 只把**有值**的可选键并入 Base（undefined 键不进 enterprise 面，避免下游
%% 把「未提供」误读成非法值）。
optional_keys(Base, Params, Keys) ->
    lists:foldl(
        fun(Key, Acc) ->
            case maps:get(Key, Params, undefined) of
                undefined -> Acc;
                Value -> Acc#{Key => Value}
            end
        end,
        Base,
        Keys
    ).

%% enterprise facade 参数收敛：cs 侧端口键（store/id 等）**绝不**透传给
%% enterprise（两侧端口语义不同）；只透传显式 `eb_*` 注入与 key_ref
%% （密钥材料不经 HTTP 面——F6，缺省由 enterprise 侧 env 装配）。
eb_params(Params, Base) ->
    WithKeyRef =
        case maps:get(key_ref, Params, undefined) of
            undefined -> Base;
            KeyRef -> Base#{key_ref => KeyRef}
        end,
    Rename = [
        {eb_store, store},
        {eb_audit, audit},
        {eb_id, id},
        {eb_clock, clock},
        {eb_crypto, crypto}
    ],
    Merged = lists:foldl(
        fun({From, To}, Acc) ->
            case maps:get(From, Params, undefined) of
                undefined -> Acc;
                Value -> Acc#{To => Value}
            end
        end,
        WithKeyRef,
        Rename
    ),
    %% CSB-02S D6：访客主体（服务端派生自令牌 contact）透传给 enterprise
    %% 附件面的访客作用域分支。
    case maps:get(actor_contact_id, Params, undefined) of
        undefined -> Merged;
        ContactId -> Merged#{actor_contact_id => ContactId}
    end.

%% ===================================================================
%% 令牌与 installation 的机械读取（digest 命中 / 未吊销 / 未过期）
%% ===================================================================

%% 令牌校验：digest 命中 (Org, installation) + 未吊销 + 未过期；
%% 跨 Org / 跨安装的命中不了行（not_found，无存在性枚举）。
verify_bootstrap_token(OrgId, Params) ->
    Secret = maps:get(secret, Params, undefined),
    ShapeOk =
        pos_int(OrgId) andalso
            pos_int(maps:get(installation_id, Params, undefined)) andalso
            pos_int(maps:get(at, Params, undefined)) andalso
            non_empty_binary(Secret),
    case ShapeOk of
        false ->
            {error, {invalid_argument, verify_bootstrap_token}};
        true ->
            fetch_token_by_digest(OrgId, Params, Secret)
    end.

fetch_token_by_digest(OrgId, Params, Secret) ->
    Digest = token_digest(Params, Secret),
    InstallationId = maps:get(installation_id, Params),
    case
        with_store(Params, fun(Store) ->
            Store:fetch_widget_bootstrap_token_by_digest(OrgId, InstallationId, Digest)
        end)
    of
        %% CP-SEC-05（DEC-VISIT-TOKEN=FIX_401_VISIT_TOKEN_INVALID）：digest
        %% 无命中行 = 持有的 secret 不是任何已签发令牌（伪造/跨租户重放）——
        %% 凭证无效语义，翻译为 visit_token_invalid（HTTP 面 401），不把
        %% not_found 泄漏成 404/500。installation 存在性在 frame/bootstrap 面
        %% 更早裁决（installation_unavailable），与此凭证面语义分离。
        {error, not_found} ->
            {error, visit_token_invalid};
        {error, _} = Err ->
            Err;
        {ok, Token} ->
            token_usable(Token, maps:get(at, Params))
    end.

%% @doc 持 token 动作面的 Org 权威派生（CSD-BE-01S，hosted-widget-contract
%% S3 v1.1 零申报面）：(installation_id, secret) 的 digest **全局**命中行本就
%% 绑定 (organization_id, installation)——命中行的 org 即派生租户（facade 在
%% env 事实装配**之前**调用，装配与用例内的令牌复核都以真实 Org 进行）。
%% digest = sha256(secret)：命中前提是持明文 secret，无存在性枚举面。
%% 令牌可用性（吊销/过期）不在此裁决——仍由各用例的
%% `verify_bootstrap_token/2` 以 (Org, installation) 同语句复核（纵深防御：
%% 本函数只做「租户解析」这一件事）。
-spec derive_org_by_token(map()) -> {ok, integer()} | {error, term()}.
derive_org_by_token(Params) when is_map(Params) ->
    InstallationId = maps:get(installation_id, Params, undefined),
    Secret = maps:get(secret, Params, undefined),
    case pos_int(InstallationId) andalso non_empty_binary(Secret) of
        false ->
            {error, {invalid_argument, derive_org_by_token}};
        true ->
            Digest = token_digest(Params, Secret),
            case
                with_store(Params, fun(Store) ->
                    Store:fetch_widget_bootstrap_token_by_digest_global(InstallationId, Digest)
                end)
            of
                %% CP-SEC-05：同 fetch_token_by_digest——digest 无命中 = 伪造
                %% 凭证，翻译 visit_token_invalid（401）。
                {error, not_found} ->
                    {error, visit_token_invalid};
                {error, _} = Err ->
                    Err;
                {ok, Token} ->
                    {ok, maps:get(organization_id, Token)}
            end
    end;
derive_org_by_token(_Params) ->
    {error, {invalid_argument, derive_org_by_token}}.

token_usable(Token, At) ->
    RevokedAt = maps:get(revoked_at, Token, undefined),
    ExpiresAt = maps:get(expires_at, Token, undefined),
    if
        is_integer(RevokedAt), RevokedAt =< At -> {error, token_revoked};
        is_integer(ExpiresAt), At >= ExpiresAt -> {error, token_expired};
        true -> {ok, Token}
    end.

fetch_installation(Params, OrgId, InstallationId) ->
    case
        with_store(Params, fun(Store) ->
            Store:fetch_widget_installation(OrgId, InstallationId)
        end)
    of
        {error, _} = Err ->
            Err;
        {ok, Installation} ->
            installation_active(Installation)
    end.

installation_active(Installation) ->
    case maps:get(status, Installation, undefined) of
        active -> {ok, Installation};
        _ -> {error, installation_revoked}
    end.

%% ===================================================================
%% 跨裁剪出口（customer_service 侧：端口辅助 + session 用例复用）
%% ===================================================================

-ifdef(IMBOY_FEATURE_CUSTOMER_SERVICE).
pos_int(V) ->
    cs_app_support:pos_int(V).
non_empty_binary(V) ->
    cs_app_support:non_empty_binary(V).
with_store(Params, Fun) ->
    cs_app_support:with_store(Params, Fun).
new_id(Kind, Params) ->
    cs_app_support:new_id(Kind, Params).
%% 客服域 append-only 审计（workspace 一并落事件）。
append_event(Params, OrgId, WorkspaceId, Event) ->
    cs_app_support:append_event(Params, OrgId, Event#{workspace_id => WorkspaceId}).
session_open(OrgId, Params) ->
    cs_session_app:open_session(OrgId, Params).
session_list_contact(OrgId, Params) ->
    cs_session_app:list_contact_sessions(OrgId, Params).
session_fetch(OrgId, Params) ->
    cs_session_app:fetch_session(OrgId, Params).
session_append_message(OrgId, Params) ->
    cs_session_app:append_session_message(OrgId, Params).
session_rate(OrgId, Params) ->
    cs_session_app:rate(OrgId, Params).
-else.
pos_int(_V) ->
    false.
non_empty_binary(_V) ->
    false.
with_store(_Params, _Fun) ->
    {error, customer_service_feature_not_selected}.
new_id(_Kind, _Params) ->
    {error, customer_service_feature_not_selected}.
append_event(_Params, _OrgId, _WorkspaceId, _Event) ->
    {error, customer_service_feature_not_selected}.
session_open(_OrgId, _Params) ->
    {error, customer_service_feature_not_selected}.
session_list_contact(_OrgId, _Params) ->
    {error, customer_service_feature_not_selected}.
session_fetch(_OrgId, _Params) ->
    {error, customer_service_feature_not_selected}.
session_append_message(_OrgId, _Params) ->
    {error, customer_service_feature_not_selected}.
session_rate(_OrgId, _Params) ->
    {error, customer_service_feature_not_selected}.
-endif.

%% ===================================================================
%% 跨裁剪出口（enterprise 真源入口：消息/客户/会话/附件的唯一写读路径）
%% ===================================================================

-ifdef(IMBOY_FEATURE_ENTERPRISE_BUSINESS).
eb_create_contact(OrgId, Params) ->
    enterprise_business_facade:create_contact(OrgId, Params).
eb_open_conversation(OrgId, Params) ->
    enterprise_business_facade:open_conversation(OrgId, Params).
eb_list_messages(OrgId, Params) ->
    enterprise_business_facade:list_messages(OrgId, Params).
eb_request_presign(OrgId, Params) ->
    enterprise_business_facade:request_presign(OrgId, Params).
eb_confirm_asset(OrgId, Params) ->
    enterprise_business_facade:confirm_asset(OrgId, Params).
%% BE-PATCH-01：访客附件字节上传（企业真源既有 put_object：ref open 验过期/
%% 篡改/同上传人 + contact 会话归属门 + hash/size/mime 复核，widget 零复制）。
eb_put_object(OrgId, Params) ->
    enterprise_business_facade:put_object(OrgId, Params).
%% BE-PATCH-01：API 绝对 URL 基址（{imboy, base_url}，仓内既有派生方式；
%% 未配置 = 空串，presign 投影据此不加 upload.url——fail-closed）。
api_base() ->
    config_ds:env(base_url, <<>>).
%% BE-S01b：访客附件内容代理（企业真源的 contact 分支，零 URL/key 出站）。
eb_content_stream(OrgId, Params) ->
    enterprise_business_facade:content_stream(OrgId, Params).
-else.
eb_create_contact(_OrgId, _Params) ->
    {error, enterprise_business_feature_not_selected}.
eb_open_conversation(_OrgId, _Params) ->
    {error, enterprise_business_feature_not_selected}.
eb_list_messages(_OrgId, _Params) ->
    {error, enterprise_business_feature_not_selected}.
eb_request_presign(_OrgId, _Params) ->
    {error, enterprise_business_feature_not_selected}.
eb_confirm_asset(_OrgId, _Params) ->
    {error, enterprise_business_feature_not_selected}.
eb_put_object(_OrgId, _Params) ->
    {error, enterprise_business_feature_not_selected}.
api_base() ->
    <<>>.
eb_content_stream(_OrgId, _Params) ->
    {error, enterprise_business_feature_not_selected}.
-endif.
