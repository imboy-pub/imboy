%%% @doc Widget 接入的应用层用例（CSB-02）：bootstrap 与可选签名身份 exchange。
%%%
%%% 依据：plan v4.1 §12.4（Widget API 最小合同）、§12.7 CSB-02、EB-D08。
%%% 访客会话生命周期（create/list/message/history/rating/asset）在
%%% `cs_widget_session_app`；跨裁剪出口与机械辅助在 `cs_widget_support`；
%%% 领域纯判定在 `cs_widget`。
%%%
%%% 安全合同（全部服务端派生，禁止信任浏览器申报值）：
%%%   * Org 解析后的 installation 只在本 Org 内可见（store 同语句裁决）；
%%%     status 非 active（revoked）即拒绝新 bootstrap；
%%%   * Origin 由 domain `cs_widget:origin_allowed/2` 做 scheme+host+port 归一
%%%     后精确匹配 allowlist——无通融、无子域前缀；
%%%   * 匿名 subject 只以 HMAC 形态存在（`cs_widget:subject_hmac/3`，密钥由
%%%     Ctx 注入）；(installation, subject_hmac) → enterprise contact 的映射
%%%     以 contact 的**确定性资源键**（EB-05 幂等锚）裁决：重复映射返回
%%%     `{error, {contact_exists, ContactId}}` 且零新增行 → 应用层复用该 id；
%%%   * bootstrap 令牌走 CSB-01 的 digest 存储路径（复用 visit_token 存储，
%%%     绑定 installation+contact）；明文 secret 只在签发响应返回一次；
%%%   * 签名身份断言：identity_key 只存 digest——签名验证器由 Ctx 注入
%%%     （`assertion_verifier/2`，持有真钥材料并核对 digest），application 只做
%%%     claims 全查 + jti 一次性消费（store nonce 唯一裁决，重放
%%%     `{error, replay}`）。
%%%
%%% 时钟 / ID / HMAC key / token secret / 默认 Workspace 等事实全部显式注入
%%% （照 `cs_app_support` / EB `default_workspace` 既有风格）；缺注入即
%%% fail-closed，不做隐式推断。
-module(cs_widget_app).

-export([
    list_installations/2,
    create_installation/2,
    revoke_installation/2,
    bootstrap/2,
    identity_exchange/2
]).

-define(INSTALLATION_PROJECTION, [
    id,
    organization_id,
    public_widget_id,
    display_name,
    allowed_origins,
    branding,
    consent_version,
    status,
    revoked_at,
    version,
    created_at,
    updated_at
]).

%% ===================================================================
%% Admin installation 管理（公开 id，不签发或返回任何 shop_key）
%% ===================================================================

-spec list_installations(integer(), map()) -> {ok, map()} | {error, term()}.
list_installations(OrgId, Params) when is_map(Params) ->
    %% installation 是 Org 级资源；workspace_id 仅是平台管理接口的显式请求上下文。
    case cs_app_support:tenant(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, _WorkspaceId} ->
            case cs_app_support:page_cursor(Params) of
                {error, _} = Err2 ->
                    Err2;
                {ok, AfterId, Limit} ->
                    list_installations_page(OrgId, AfterId, Limit, Params)
            end
    end;
list_installations(_OrgId, _Params) ->
    {error, {invalid_argument, list_installations}}.

list_installations_page(OrgId, AfterId, Limit, Params) ->
    case
        cs_widget_support:with_store(Params, fun(Store) ->
            Store:list_widget_installations_page(OrgId, AfterId, Limit)
        end)
    of
        {error, _} = Err ->
            Err;
        {ok, Rows} ->
            Views = [installation_view(Row) || Row <- Rows],
            cs_app_support:page_view(
                installations, ?INSTALLATION_PROJECTION, Views, Limit, id
            )
    end.

-spec create_installation(integer(), map()) -> {ok, map()} | {error, term()}.
create_installation(OrgId, Params) when is_map(Params) ->
    case installation_draft(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, WorkspaceId, Draft} ->
            insert_installation(OrgId, WorkspaceId, Draft, Params)
    end;
create_installation(_OrgId, _Params) ->
    {error, {invalid_argument, create_installation}}.

installation_draft(OrgId, Params) ->
    DisplayName = maps:get(display_name, Params, undefined),
    ConsentVersion = maps:get(consent_version, Params, undefined),
    Branding = maps:get(branding, Params, undefined),
    case cs_app_support:tenant(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, WorkspaceId} ->
            case
                cs_widget_support:non_empty_binary(DisplayName) andalso
                    cs_widget_support:non_empty_binary(ConsentVersion) andalso is_map(Branding)
            of
                false ->
                    {error, {invalid_argument, create_installation}};
                true ->
                    installation_origins(WorkspaceId, DisplayName, ConsentVersion, Branding, Params)
            end
    end.

installation_origins(WorkspaceId, DisplayName, ConsentVersion, Branding, Params) ->
    case normalize_origins(maps:get(allowed_origins, Params, undefined), []) of
        {error, _} = Err ->
            Err;
        {ok, []} ->
            {error, {invalid_argument, allowed_origins}};
        {ok, AllowedOrigins} ->
            case cs_widget_support:new_id(cs_widget_installation, Params) of
                {error, _} = Err2 ->
                    Err2;
                {ok, InstallationId} ->
                    {ok, WorkspaceId, #{
                        id => InstallationId,
                        public_widget_id => new_public_widget_id(Params),
                        display_name => DisplayName,
                        allowed_origins => AllowedOrigins,
                        branding => cs_widget:branding_view(Branding),
                        consent_version => ConsentVersion,
                        created_by_user_id => maps:get(actor_user_id, Params, undefined)
                    }}
            end
    end.

insert_installation(OrgId, WorkspaceId, Draft, Params) ->
    case
        cs_widget_support:with_store(Params, fun(Store) ->
            Store:insert_widget_installation(OrgId, Draft)
        end)
    of
        {error, _} = Err ->
            Err;
        {ok, Stored} ->
            case
                installation_event(
                    Params, OrgId, WorkspaceId, <<"widget.installation.created">>, Stored
                )
            of
                ok -> {ok, #{installation => installation_view(Stored)}};
                {error, _} = AuditErr -> AuditErr
            end
    end.

-spec revoke_installation(integer(), map()) -> {ok, map()} | {error, term()}.
revoke_installation(OrgId, Params) when is_map(Params) ->
    Id = maps:get(id, Params, undefined),
    At = maps:get(at, Params, undefined),
    case
        {
            cs_app_support:tenant(OrgId, Params),
            cs_widget_support:pos_int(Id),
            cs_widget_support:pos_int(At)
        }
    of
        {{error, _} = Err, _, _} ->
            Err;
        {{ok, _WorkspaceId}, false, _} ->
            {error, {invalid_argument, revoke_installation}};
        {{ok, _WorkspaceId}, _, false} ->
            {error, {invalid_argument, revoke_installation}};
        {{ok, WorkspaceId}, true, true} ->
            revoke_installation_in(OrgId, WorkspaceId, Id, At, Params)
    end;
revoke_installation(_OrgId, _Params) ->
    {error, {invalid_argument, revoke_installation}}.

revoke_installation_in(OrgId, WorkspaceId, Id, At, Params) ->
    case
        cs_widget_support:with_store(Params, fun(Store) ->
            Store:revoke_widget_installation(OrgId, Id, At)
        end)
    of
        {error, _} = Err ->
            Err;
        ok ->
            case
                cs_widget_support:with_store(Params, fun(Store) ->
                    Store:fetch_widget_installation(OrgId, Id)
                end)
            of
                {error, _} = Err2 ->
                    Err2;
                {ok, Stored} ->
                    case
                        installation_event(
                            Params, OrgId, WorkspaceId, <<"widget.installation.revoked">>, Stored
                        )
                    of
                        ok -> {ok, #{installation => installation_view(Stored)}};
                        {error, _} = AuditErr -> AuditErr
                    end
            end
    end.

installation_event(Params, OrgId, WorkspaceId, Action, Installation) ->
    cs_widget_support:append_event(Params, OrgId, WorkspaceId, #{
        actor_user_id => maps:get(actor_user_id, Params, undefined),
        actor_kind => <<"platform_admin">>,
        action => Action,
        detail => #{<<"installation_id">> => maps:get(id, Installation)}
    }).

installation_view(Installation) ->
    maps:with(
        ?INSTALLATION_PROJECTION,
        Installation#{branding => cs_widget:branding_view(maps:get(branding, Installation, #{}))}
    ).

normalize_origins(Origins, Acc) when is_list(Origins) ->
    normalize_origins_in(Origins, Acc);
normalize_origins(_Origins, _Acc) ->
    {error, {invalid_argument, allowed_origins}}.

normalize_origins_in([], Acc) ->
    {ok, lists:usort(Acc)};
normalize_origins_in([Origin | Rest], Acc) ->
    case cs_widget:normalize_origin(Origin) of
        {ok, Normalized} -> normalize_origins_in(Rest, [Normalized | Acc]);
        {error, _} = Err -> Err
    end.

new_public_widget_id(Params) ->
    case maps:get(new_public_widget_id, Params, undefined) of
        Fun when is_function(Fun, 0) -> Fun();
        _ -> <<"wgt_pub_", (binary:encode_hex(crypto:strong_rand_bytes(16)))/binary>>
    end.

%% ===================================================================
%% bootstrap（安装校验 → Origin 精确匹配 → 匿名 contact 幂等映射 → 签发令牌）
%% ===================================================================

%% @doc Widget 引导。Params：
%%   public_widget_id / origin / subject_id（浏览器随机 ID）/ at / subject_key
%%   （服务端 HMAC 材料，注入）必填；secret（已持有的 bootstrap 令牌，重放
%%   复用）、default_workspace（fun/1 服务端事实，contact 落位用）、
%%   bootstrap_token_ttl、new_secret（fun/0）、digest（fun/1）、store / id
%%   可选；`key_ref` / `eb_*` 透传给 enterprise facade（测试/内部合同）。
%%
%% 重放语义（CSB-02-A01）：
%%   * 携带未过期且 subject 逐字相符的令牌 → 原令牌复用（touch 心跳），
%%     contact 不重建、session 不动；
%%   * 携带过期令牌 / 无令牌 → 同 subject 重新签发（contact 幂等锚保证同
%%     contact），contact_exists 复用既有行；
%%   * 携带已吊销令牌（kill switch）或 subject 不符的令牌 → 显式拒绝。
-spec bootstrap(integer(), map()) -> {ok, map()} | {error, term()}.
bootstrap(OrgId, Params) when is_map(Params) ->
    case bootstrap_args(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, Installation} ->
            bootstrap_origin(OrgId, Installation, Params)
    end;
bootstrap(_OrgId, _Params) ->
    {error, {invalid_argument, bootstrap}}.

bootstrap_args(OrgId, Params) ->
    PublicId = maps:get(public_widget_id, Params, undefined),
    ShapeOk =
        cs_widget_support:pos_int(OrgId) andalso
            cs_widget_support:non_empty_binary(PublicId) andalso
            cs_widget_support:non_empty_binary(maps:get(subject_id, Params, undefined)) andalso
            cs_widget_support:non_empty_binary(maps:get(origin, Params, undefined)) andalso
            cs_widget_support:non_empty_binary(maps:get(subject_key, Params, undefined)) andalso
            cs_widget_support:pos_int(maps:get(at, Params, undefined)),
    case ShapeOk of
        false ->
            {error, {invalid_argument, bootstrap}};
        true ->
            fetch_installation_by_public_id(Params, OrgId, PublicId)
    end.

fetch_installation_by_public_id(Params, OrgId, PublicId) ->
    case
        cs_widget_support:with_store(Params, fun(Store) ->
            Store:fetch_widget_installation_by_public_id(OrgId, PublicId)
        end)
    of
        {error, _} = Err ->
            Err;
        {ok, Installation} ->
            cs_widget_support:installation_active(Installation)
    end.

bootstrap_origin(OrgId, Installation, Params) ->
    Allowed = maps:get(allowed_origins, Installation, []),
    case cs_widget:origin_allowed(maps:get(origin, Params), Allowed) of
        {error, _} = Err ->
            Err;
        ok ->
            bootstrap_replay(OrgId, Installation, Params)
    end.

bootstrap_replay(OrgId, Installation, Params) ->
    SubjectHmac = subject_hmac(Installation, Params),
    case maps:get(secret, Params, undefined) of
        Secret when is_binary(Secret), Secret =/= <<>> ->
            case replay_token(OrgId, Installation, SubjectHmac, Secret, Params) of
                {ok, View} ->
                    {ok, View};
                {error, Reason} when Reason =:= not_found orelse Reason =:= token_expired ->
                    %% 令牌失效但 subject 合法 → 同 subject 重新签发（contact 幂等）。
                    fresh_bootstrap(OrgId, Installation, SubjectHmac, Params);
                {error, _} = Err ->
                    Err
            end;
        _ ->
            fresh_bootstrap(OrgId, Installation, SubjectHmac, Params)
    end.

subject_hmac(Installation, Params) ->
    cs_widget:subject_hmac(
        maps:get(public_widget_id, Installation),
        maps:get(subject_id, Params),
        maps:get(subject_key, Params)
    ).

%% 重放校验：digest 命中 (Org, installation) → subject 逐字比对 → 未吊销/未过期。
replay_token(OrgId, Installation, SubjectHmac, Secret, Params) ->
    Digest = cs_widget_support:token_digest(Params, Secret),
    InstallationId = maps:get(id, Installation),
    case
        cs_widget_support:with_store(Params, fun(Store) ->
            Store:fetch_widget_bootstrap_token_by_digest(OrgId, InstallationId, Digest)
        end)
    of
        {error, not_found} ->
            {error, not_found};
        {error, _} = Err ->
            Err;
        {ok, Token} ->
            case maps:get(anonymous_subject_hmac, Token, undefined) of
                SubjectHmac -> replay_usable(OrgId, Installation, Token, Params);
                _ -> {error, subject_mismatch}
            end
    end.

replay_usable(OrgId, Installation, Token, Params) ->
    At = maps:get(at, Params),
    case cs_widget_support:token_usable(Token, At) of
        {error, _} = Err ->
            Err;
        {ok, _Usable} ->
            ok = cs_widget_support:with_store(Params, fun(Store) ->
                Store:touch_widget_bootstrap_token(
                    OrgId, maps:get(id, Installation), maps:get(id, Token), At
                )
            end),
            {ok, bootstrap_view(Installation, Token#{secret => maps:get(secret, Params)}, true)}
    end.

fresh_bootstrap(OrgId, Installation, SubjectHmac, Params) ->
    case cs_widget_support:resolve_default_workspace(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, WorkspaceId} ->
            case ensure_contact(OrgId, WorkspaceId, SubjectHmac, Params) of
                {error, _} = Err2 ->
                    Err2;
                {ok, ContactId, _Reused} ->
                    issue_token(OrgId, Installation, ContactId, SubjectHmac, WorkspaceId, Params)
            end
    end.

issue_token(OrgId, Installation, ContactId, SubjectHmac, WorkspaceId, Params) ->
    case cs_widget_support:new_id(cs_visit_token, Params) of
        {error, _} = Err ->
            Err;
        {ok, TokenId} ->
            do_issue_token(
                OrgId, Installation, ContactId, SubjectHmac, WorkspaceId, TokenId, Params
            )
    end.

do_issue_token(OrgId, Installation, ContactId, SubjectHmac, WorkspaceId, TokenId, Params) ->
    Secret = cs_widget_support:new_token_secret(Params),
    Token = #{
        id => TokenId,
        contact_id => ContactId,
        token_digest => cs_widget_support:token_digest(Params, Secret),
        expires_at => maps:get(at, Params) + cs_widget_support:token_ttl(Params),
        widget_installation_id => maps:get(id, Installation),
        anonymous_subject_hmac => SubjectHmac
    },
    case
        cs_widget_support:with_store(Params, fun(Store) ->
            Store:insert_widget_bootstrap_token(OrgId, Token)
        end)
    of
        {error, _} = Err ->
            Err;
        {ok, Stored} ->
            cs_widget_support:append_event(Params, OrgId, WorkspaceId, #{
                actor_kind => <<"visitor">>,
                action => <<"widget.bootstrapped">>,
                detail => #{
                    <<"installation_id">> => maps:get(id, Installation),
                    <<"contact_id">> => ContactId
                }
            }),
            {ok, bootstrap_view(Installation, Stored#{secret => Secret}, false)}
    end.

%% bootstrap 令牌签发响应白名单：digest / subject_hmac / installation 内部
%% 字段（allowed_origins 等）一律不出本用例；secret 只在此处出现一次；
%% branding 由 domain `cs_widget:branding_view/1` 白名单裁剪。
bootstrap_view(Installation, Token, Reused) ->
    #{
        installation_id => maps:get(id, Installation),
        public_widget_id => maps:get(public_widget_id, Installation),
        display_name => maps:get(display_name, Installation),
        consent_version => maps:get(consent_version, Installation),
        branding => cs_widget:branding_view(maps:get(branding, Installation, #{})),
        contact_id => maps:get(contact_id, Token),
        secret => maps:get(secret, Token),
        expires_at => maps:get(expires_at, Token),
        reused => Reused
    }.

%% ===================================================================
%% 可选签名身份 exchange（claims 全查 + jti 一次性消费 + 可信 contact 幂等绑定）
%% ===================================================================

%% @doc 商城服务端签名断言换可信身份绑定。Params 在 bootstrap 的令牌面之上：
%%   assertion（`#{key_version => pos_integer(), claims => map()}`）、
%%   assertion_verifier（fun(Assertion, KeyDigest) -> {ok, Claims} | {error, _}，
%%   由 Ctx 注入——identity_key 只存 digest，真钥材料在验证器侧）必填。
%%
%% 无 identity_key 的 installation 调用即 `{error, identity_key_not_configured}`
%% （4xx 语义）。jti 的一次性消费在任何写路径之前由 store nonce 唯一裁决。
-spec identity_exchange(integer(), map()) -> {ok, map()} | {error, term()}.
identity_exchange(OrgId, Params) when is_map(Params) ->
    case exchange_args(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, Token} ->
            exchange_installation(OrgId, Token, Params)
    end;
identity_exchange(_OrgId, _Params) ->
    {error, {invalid_argument, identity_exchange}}.

exchange_args(OrgId, Params) ->
    Assertion = maps:get(assertion, Params, undefined),
    ShapeOk =
        cs_widget_support:pos_int(OrgId) andalso
            cs_widget_support:pos_int(maps:get(installation_id, Params, undefined)) andalso
            cs_widget_support:pos_int(maps:get(at, Params, undefined)) andalso
            is_map(Assertion) andalso
            cs_widget_support:pos_int(maps:get(key_version, Assertion, undefined)) andalso
            is_map(maps:get(claims, Assertion, undefined)) andalso
            is_function(maps:get(assertion_verifier, Params, undefined), 2),
    case ShapeOk of
        false ->
            {error, {invalid_argument, identity_exchange}};
        true ->
            cs_widget_support:verify_bootstrap_token(OrgId, Params)
    end.

exchange_installation(OrgId, Token, Params) ->
    InstallationId = maps:get(installation_id, Params),
    case cs_widget_support:fetch_installation(Params, OrgId, InstallationId) of
        {error, _} = Err ->
            Err;
        {ok, Installation} ->
            exchange_key(OrgId, Installation, Token, Params)
    end.

exchange_key(OrgId, Installation, Token, Params) ->
    Assertion = maps:get(assertion, Params),
    KeyVersion = maps:get(key_version, Assertion),
    case
        cs_widget_support:with_store(Params, fun(Store) ->
            Store:fetch_widget_identity_key(OrgId, maps:get(id, Installation), KeyVersion)
        end)
    of
        {error, not_found} ->
            {error, identity_key_not_configured};
        {error, _} = Err ->
            Err;
        {ok, Key} ->
            exchange_key_usable(OrgId, Installation, Token, Key, Assertion, Params)
    end.

exchange_key_usable(OrgId, Installation, Token, Key, Assertion, Params) ->
    At = maps:get(at, Params),
    Usable =
        case maps:get(status, Key, undefined) of
            active ->
                case maps:get(expires_at, Key, undefined) of
                    ExpiresAt when is_integer(ExpiresAt), ExpiresAt =< At ->
                        {error, identity_key_expired};
                    _ ->
                        ok
                end;
            _ ->
                {error, identity_key_revoked}
        end,
    case Usable of
        {error, _} = Err ->
            Err;
        ok ->
            exchange_verify(OrgId, Installation, Token, Key, Assertion, Params)
    end.

exchange_verify(OrgId, Installation, Token, Key, Assertion, Params) ->
    Verifier = maps:get(assertion_verifier, Params),
    case Verifier(Assertion, maps:get(key_digest, Key)) of
        {error, _} = Err ->
            Err;
        {ok, Claims} ->
            PublicId = maps:get(public_widget_id, Installation),
            Expectation = #{
                aud => PublicId,
                widget_id => PublicId,
                now => maps:get(at, Params)
            },
            case cs_widget:assertion_claims(Claims, Expectation) of
                {error, _} = Err2 ->
                    Err2;
                ok ->
                    consume_nonce(OrgId, Installation, Token, Claims, Params)
            end
    end.

consume_nonce(OrgId, Installation, Token, Claims, Params) ->
    JtiDigest = cs_widget_support:token_digest(Params, maps:get(jti, Claims)),
    case
        cs_widget_support:with_store(Params, fun(Store) ->
            Store:record_widget_nonce(
                OrgId, maps:get(id, Installation), JtiDigest, maps:get(exp, Claims)
            )
        end)
    of
        {error, _} = Err ->
            Err;
        ok ->
            bind_verified_contact(OrgId, Installation, Token, Claims, Params)
    end.

bind_verified_contact(OrgId, Installation, Token, Claims, Params) ->
    PublicId = maps:get(public_widget_id, Installation),
    VerifiedHmac = cs_widget:verified_subject_hmac(
        PublicId, maps:get(sub, Claims), maps:get(subject_key, Params)
    ),
    case cs_widget_support:resolve_default_workspace(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, WorkspaceId} ->
            case ensure_contact(OrgId, WorkspaceId, VerifiedHmac, Params) of
                {error, _} = Err2 ->
                    Err2;
                {ok, ContactId, Reused} ->
                    cs_widget_support:append_event(
                        Params, OrgId, WorkspaceId, #{
                            actor_kind => <<"visitor">>,
                            action => <<"widget.identity_exchanged">>,
                            detail => #{
                                <<"installation_id">> => maps:get(id, Installation),
                                <<"contact_id">> => ContactId
                            }
                        }
                    ),
                    {ok, #{
                        installation_id => maps:get(id, Installation),
                        contact_id => ContactId,
                        anonymous_contact_id => maps:get(contact_id, Token),
                        contact_reused => Reused
                    }}
            end
    end.

%% (installation, subject) 幂等映射的唯一写入口：enterprise contact 的
%% 确定性资源键（EB-05 幂等锚）——重复映射 `{error, {contact_exists, Id}}`
%% 且零新增行，应用层复用该 id（不重复建）。
ensure_contact(OrgId, WorkspaceId, SubjectHmac, Params) ->
    EbParams = cs_widget_support:eb_params(Params, #{
        workspace_id => WorkspaceId,
        channel => <<"other">>,
        subject => SubjectHmac
    }),
    case cs_widget_support:eb_create_contact(OrgId, EbParams) of
        {ok, Result} ->
            {ok, maps:get(id, maps:get(contact, Result)), false};
        {error, {contact_exists, ContactId}} ->
            {ok, ContactId, true};
        {error, _} = Err ->
            Err
    end.
