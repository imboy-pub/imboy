%%% @doc 客服接入凭证的应用层用例：shop key 与访客 visit token。
%%%
%%% 依据：plan v4.1 §4.2、§5.2、EB-D10（cs_shop_key / cs_visit）、CS-01-A05。
%%%
%%% 安全合同：
%%%   * **明文只返回一次**：secret 由调用方生成后传入，本模块只落 digest
%%%     （sha256 hex，注入 `digest` 可覆盖）；数据库与审计永不保存明文。
%%%   * **digest 命中即属主**：查询按 (OrgId, digest) 同语句裁决，跨 Org 的
%%%     secret 命中不了本 Org 行（{error, not_found}，不做存在性枚举）。
%%%   * **visit token 不是换权凭证**（A05）：`verify_visit_token/2` 只返回其
%%%     绑定的 (organization_id, contact_id) 作用域——没有任何 member/seat
%%%     能力键；吊销（revoke）或过期（expires_at）立即失效（domain 判定）。
%%%
%%% 数据访问全部经 `cs_store_port`；零 SQL、零 `elib_pg`。
-module(cs_access_app).

-export([
    create_shop_key/2,
    revoke_shop_key/2,
    verify_shop_key/2,
    issue_visit_token/2,
    revoke_visit_token/2,
    verify_visit_token/2,
    default_digest/1
]).

%% ===================================================================
%% shop key
%% ===================================================================

%% @doc 创建门店接入密钥：落 digest，明文只在本次响应返回。
%% Params：workspace_id / secret 必填；display_hint / created_by_user_id /
%% digest（fun/1）可选。
-spec create_shop_key(integer(), map()) -> {ok, map()} | {error, term()}.
create_shop_key(OrgId, Params) when is_map(Params) ->
    case cs_app_support:tenant(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, WorkspaceId} ->
            create_shop_key_in(OrgId, WorkspaceId, Params)
    end;
create_shop_key(_OrgId, _Params) ->
    {error, {invalid_argument, create_shop_key}}.

create_shop_key_in(OrgId, WorkspaceId, Params) ->
    Secret = maps:get(secret, Params, undefined),
    case cs_app_support:non_empty_binary(Secret) of
        false ->
            {error, {invalid_secret, Secret}};
        true ->
            insert_shop_key(OrgId, WorkspaceId, Secret, Params)
    end.

insert_shop_key(OrgId, WorkspaceId, Secret, Params) ->
    Digest = digest_of(Params, Secret),
    case cs_app_support:new_id(cs_shop_key, Params) of
        {error, _} = Err ->
            Err;
        {ok, KeyId} ->
            Key = #{
                id => KeyId,
                organization_id => OrgId,
                key_digest => Digest,
                display_hint => maps:get(display_hint, Params, undefined),
                created_by_user_id => maps:get(created_by_user_id, Params, undefined)
            },
            case with_store(Params, fun(Store) -> Store:insert_shop_key(OrgId, Key) end) of
                {error, _} = Err ->
                    Err;
                {ok, Stored} ->
                    case
                        append_event(Params, OrgId, WorkspaceId, #{
                            actor_user_id => maps:get(created_by_user_id, Params, undefined),
                            actor_kind => <<"tenant_admin">>,
                            action => <<"shop_key.created">>,
                            detail => #{<<"shop_key_id">> => KeyId},
                            workspace_id => WorkspaceId
                        })
                    of
                        ok -> {ok, Stored#{workspace_id => WorkspaceId, secret => Secret}};
                        {error, _} = AuditErr -> AuditErr
                    end
            end
    end.

%% @doc 吊销门店密钥（digest 行保留以审计；吊销后校验立即失败）。
-spec revoke_shop_key(integer(), map()) -> ok | {error, term()}.
revoke_shop_key(OrgId, Params) when is_map(Params) ->
    case cs_app_support:tenant(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, WorkspaceId} ->
            revoke_in(shop_key, OrgId, WorkspaceId, Params, <<"shop_key.revoked">>)
    end;
revoke_shop_key(_OrgId, _Params) ->
    {error, {invalid_argument, revoke_shop_key}}.

%% @doc 用明文校验 shop key（digest 命中 + 未吊销即有效；返回不含明文/digest 的行）。
-spec verify_shop_key(integer(), map()) -> {ok, map()} | {error, not_found | revoked | term()}.
verify_shop_key(OrgId, Params) when is_map(Params) ->
    Secret = maps:get(secret, Params, undefined),
    case cs_app_support:non_empty_binary(Secret) of
        false ->
            {error, {invalid_secret, Secret}};
        true ->
            case
                with_store(Params, fun(Store) ->
                    Store:fetch_shop_key_by_digest(OrgId, digest_of(Params, Secret))
                end)
            of
                {error, _} = Err ->
                    Err;
                {ok, Key} ->
                    case maps:get(status, Key, undefined) of
                        active -> {ok, maps:without([key_digest], Key)};
                        _ -> {error, revoked}
                    end
            end
    end;
verify_shop_key(_OrgId, _Params) ->
    {error, {invalid_argument, verify_shop_key}}.

%% ===================================================================
%% visit token（A05：访客 key 单独不能换权）
%% ===================================================================

%% @doc 签发访客令牌：绑定 (Org, contact) + 过期时间；明文只返回一次。
%% Params：workspace_id / contact_id / secret / expires_at 必填；
%% created_by_business_identity_id / created_by_user_id / digest 可选。
-spec issue_visit_token(integer(), map()) -> {ok, map()} | {error, term()}.
issue_visit_token(OrgId, Params) when is_map(Params) ->
    case cs_app_support:tenant(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, WorkspaceId} ->
            issue_visit_token_in(OrgId, WorkspaceId, Params)
    end;
issue_visit_token(_OrgId, _Params) ->
    {error, {invalid_argument, issue_visit_token}}.

issue_visit_token_in(OrgId, WorkspaceId, Params) ->
    ContactId = maps:get(contact_id, Params, undefined),
    Secret = maps:get(secret, Params, undefined),
    ExpiresAt = maps:get(expires_at, Params, undefined),
    ArgsOk =
        cs_app_support:pos_int(ContactId) andalso
            cs_app_support:non_empty_binary(Secret) andalso
            cs_app_support:pos_int(ExpiresAt),
    case ArgsOk of
        false ->
            {error, {invalid_argument, issue_visit_token}};
        true ->
            insert_visit_token(OrgId, WorkspaceId, ContactId, Secret, ExpiresAt, Params)
    end.

insert_visit_token(OrgId, WorkspaceId, ContactId, Secret, ExpiresAt, Params) ->
    Digest = digest_of(Params, Secret),
    case cs_app_support:new_id(cs_visit_token, Params) of
        {error, _} = Err ->
            Err;
        {ok, TokenId} ->
            Token = #{
                id => TokenId,
                organization_id => OrgId,
                contact_id => ContactId,
                token_digest => Digest,
                expires_at => ExpiresAt,
                created_by_business_identity_id =>
                    maps:get(created_by_business_identity_id, Params, undefined),
                created_by_user_id => maps:get(created_by_user_id, Params, undefined)
            },
            case with_store(Params, fun(Store) -> Store:insert_visit_token(OrgId, Token) end) of
                {error, _} = Err ->
                    Err;
                {ok, Stored} ->
                    case
                        append_event(Params, OrgId, WorkspaceId, #{
                            business_identity_id =>
                                maps:get(created_by_business_identity_id, Params, undefined),
                            actor_user_id => maps:get(created_by_user_id, Params, undefined),
                            actor_kind => <<"tenant_admin">>,
                            action => <<"visit_token.issued">>,
                            detail => #{
                                <<"visit_token_id">> => TokenId, <<"contact_id">> => ContactId
                            },
                            workspace_id => WorkspaceId
                        })
                    of
                        ok -> {ok, Stored#{workspace_id => WorkspaceId, secret => Secret}};
                        {error, _} = AuditErr -> AuditErr
                    end
            end
    end.

%% @doc 吊销访客令牌：吊销后即使未过期也立即失效（domain `token_revoked`）。
-spec revoke_visit_token(integer(), map()) -> ok | {error, term()}.
revoke_visit_token(OrgId, Params) when is_map(Params) ->
    case cs_app_support:tenant(OrgId, Params) of
        {error, _} = Err ->
            Err;
        {ok, WorkspaceId} ->
            revoke_in(visit_token, OrgId, WorkspaceId, Params, <<"visit_token.revoked">>)
    end;
revoke_visit_token(_OrgId, _Params) ->
    {error, {invalid_argument, revoke_visit_token}}.

%% @doc 校验访客令牌：digest 命中 + 未吊销 + 未过期后，只返回其
%% (organization_id, contact_id) 作用域——**没有**任何 member/seat 能力键（A05）。
%%
%% 返回 `{ok, #{organization_id, contact_id, scope => visit}}` 或
%% `{error, not_found | revoked | token_expired | cross_org | contact_mismatch}`。
-spec verify_visit_token(integer(), map()) -> {ok, map()} | {error, term()}.
verify_visit_token(OrgId, Params) when is_map(Params) ->
    Secret = maps:get(secret, Params, undefined),
    Now = maps:get(at, Params, undefined),
    case cs_app_support:non_empty_binary(Secret) of
        false ->
            {error, {invalid_secret, Secret}};
        true ->
            verify_token_digest(OrgId, digest_of(Params, Secret), Now, Params)
    end;
verify_visit_token(_OrgId, _Params) ->
    {error, {invalid_argument, verify_visit_token}}.

verify_token_digest(OrgId, Digest, Now, Params) ->
    case
        with_store(Params, fun(Store) ->
            Store:fetch_visit_token_by_digest(OrgId, Digest)
        end)
    of
        {error, _} = Err ->
            Err;
        {ok, Token} ->
            case cs_session:assert_visitor_scope(Token, OrgId, maps:get(contact_id, Token), Now) of
                ok ->
                    {ok, #{
                        organization_id => maps:get(organization_id, Token),
                        contact_id => maps:get(contact_id, Token),
                        scope => visit
                    }};
                {error, _} = Err ->
                    Err
            end
    end.

%% ===================================================================
%% 内部辅助
%% ===================================================================

revoke_in(Kind, OrgId, WorkspaceId, Params, Action) ->
    Id = maps:get(id, Params, undefined),
    At = maps:get(at, Params, undefined),
    case cs_app_support:pos_int(Id) andalso cs_app_support:pos_int(At) of
        false ->
            {error, {invalid_argument, revoke}};
        true ->
            revoke_row(Kind, OrgId, WorkspaceId, Id, At, Action, Params)
    end.

revoke_row(Kind, OrgId, WorkspaceId, Id, At, Action, Params) ->
    Revoke = fun(Store) ->
        case Kind of
            shop_key -> Store:revoke_shop_key(OrgId, Id, At);
            visit_token -> Store:revoke_visit_token(OrgId, Id, At)
        end
    end,
    case with_store(Params, Revoke) of
        {error, _} = Err ->
            Err;
        ok ->
            case
                append_event(Params, OrgId, WorkspaceId, #{
                    actor_user_id => maps:get(actor_user_id, Params, undefined),
                    actor_kind => <<"tenant_admin">>,
                    action => Action,
                    detail => #{<<"id">> => Id},
                    workspace_id => WorkspaceId
                })
            of
                ok -> ok;
                {error, _} = AuditErr -> AuditErr
            end
    end.

digest_of(Params, Secret) ->
    case maps:get(digest, Params, undefined) of
        Fun when is_function(Fun, 1) -> Fun(Secret);
        _ -> default_digest(Secret)
    end.

%% @doc 默认摘要：sha256 hex（确定性；密钥明文永不落库）。
-spec default_digest(binary()) -> binary().
default_digest(Secret) when is_binary(Secret) ->
    binary:encode_hex(crypto:hash(sha256, Secret)).

append_event(Params, OrgId, WorkspaceId, Event) ->
    cs_app_support:append_event(Params, OrgId, Event#{workspace_id => WorkspaceId}).

with_store(Params, Fun) ->
    cs_app_support:with_store(Params, Fun).
