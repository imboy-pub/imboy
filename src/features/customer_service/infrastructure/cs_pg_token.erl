%%% @doc 门店密钥 / 访客令牌的 PG 实现（`cs_store_port` 的 key/token 段）。
%%%
%%% 安全不变量：只存 digest（sha256 hex），**任何** SELECT 都不返回明文
%%% （表里也没有明文列）；digest 查找按 (organization_id, digest) 同语句裁决，
%%% 跨 Org 的 secret 命中不了行（not_found，不做存在性枚举）。
%%% 铁律 6：每条 SQL 同语句带 `organization_id`。
-module(cs_pg_token).

-export([
    insert_shop_key/2,
    fetch_shop_key/2,
    fetch_shop_key_by_digest/2,
    revoke_shop_key/3,
    insert_visit_token/2,
    fetch_visit_token/2,
    fetch_visit_token_by_digest/2,
    revoke_visit_token/3,
    sql_statements/0
]).

-define(SHOP_KEY_KEYS, [
    id,
    organization_id,
    key_digest,
    display_hint,
    status,
    revoked_at,
    version,
    created_at,
    updated_at
]).

-define(VISIT_TOKEN_KEYS, [
    id,
    organization_id,
    contact_id,
    token_digest,
    display_hint,
    expires_at,
    revoked_at,
    created_by_business_identity_id,
    version,
    created_at,
    updated_at
]).

-define(SQL_INSERT_SHOP_KEY, <<
    "INSERT INTO customer_service_shop_key"
    " (id, organization_id, key_digest, display_hint, created_by_user_id)"
    " VALUES ($1, $2, $3, $4, $5)"
>>).

-define(SQL_FETCH_SHOP_KEY, <<
    "SELECT id, organization_id, key_digest, display_hint, status,"
    "       extract(epoch from revoked_at)::bigint AS revoked_at, version,"
    "       extract(epoch from created_at)::bigint AS created_at,"
    "       extract(epoch from updated_at)::bigint AS updated_at"
    "  FROM customer_service_shop_key"
    " WHERE organization_id = $1 AND id = $2"
>>).

-define(SQL_FETCH_SHOP_KEY_BY_DIGEST, <<
    "SELECT id, organization_id, key_digest, display_hint, status,"
    "       extract(epoch from revoked_at)::bigint AS revoked_at, version,"
    "       extract(epoch from created_at)::bigint AS created_at,"
    "       extract(epoch from updated_at)::bigint AS updated_at"
    "  FROM customer_service_shop_key"
    " WHERE organization_id = $1 AND key_digest = $2"
>>).

-define(SQL_REVOKE_SHOP_KEY, <<
    "UPDATE customer_service_shop_key"
    "   SET status = 'revoked', revoked_at = to_timestamp($3),"
    "       version = version + 1, updated_at = to_timestamp($3)"
    " WHERE organization_id = $1 AND id = $2 AND status = 'active'"
>>).

-define(SQL_INSERT_VISIT_TOKEN, <<
    "INSERT INTO customer_service_visit_token"
    " (id, organization_id, contact_id, token_digest, expires_at,"
    "  created_by_business_identity_id, created_by_user_id)"
    " VALUES ($1, $2, $3, $4, to_timestamp($5), $6, $7)"
>>).

-define(SQL_FETCH_VISIT_TOKEN, <<
    "SELECT id, organization_id, contact_id, token_digest, display_hint,"
    "       extract(epoch from expires_at)::bigint AS expires_at,"
    "       extract(epoch from revoked_at)::bigint AS revoked_at,"
    "       created_by_business_identity_id, version,"
    "       extract(epoch from created_at)::bigint AS created_at,"
    "       extract(epoch from updated_at)::bigint AS updated_at"
    "  FROM customer_service_visit_token"
    " WHERE organization_id = $1 AND id = $2"
>>).

-define(SQL_FETCH_VISIT_TOKEN_BY_DIGEST, <<
    "SELECT id, organization_id, contact_id, token_digest, display_hint,"
    "       extract(epoch from expires_at)::bigint AS expires_at,"
    "       extract(epoch from revoked_at)::bigint AS revoked_at,"
    "       created_by_business_identity_id, version,"
    "       extract(epoch from created_at)::bigint AS created_at,"
    "       extract(epoch from updated_at)::bigint AS updated_at"
    "  FROM customer_service_visit_token"
    " WHERE organization_id = $1 AND token_digest = $2"
>>).

-define(SQL_REVOKE_VISIT_TOKEN, <<
    "UPDATE customer_service_visit_token"
    "   SET revoked_at = to_timestamp($3), version = version + 1,"
    "       updated_at = to_timestamp($3)"
    " WHERE organization_id = $1 AND id = $2 AND revoked_at IS NULL"
>>).

%% @doc 冻结语句（供 cs_pg_tests 的租户键机械断言）。
-spec sql_statements() -> [binary()].
sql_statements() ->
    [
        ?SQL_INSERT_SHOP_KEY,
        ?SQL_FETCH_SHOP_KEY,
        ?SQL_FETCH_SHOP_KEY_BY_DIGEST,
        ?SQL_REVOKE_SHOP_KEY,
        ?SQL_INSERT_VISIT_TOKEN,
        ?SQL_FETCH_VISIT_TOKEN,
        ?SQL_FETCH_VISIT_TOKEN_BY_DIGEST,
        ?SQL_REVOKE_VISIT_TOKEN
    ].

%% ===================================================================
%% shop key
%% ===================================================================

-spec insert_shop_key(integer(), map()) -> {ok, map()} | {error, term()}.
insert_shop_key(OrgId, Key) when is_map(Key) ->
    KeyId = maps:get(id, Key),
    Params = [
        KeyId,
        OrgId,
        maps:get(key_digest, Key),
        cs_pg_common:nullify(maps:get(display_hint, Key, undefined)),
        cs_pg_common:nullify(maps:get(created_by_user_id, Key, undefined))
    ],
    case elib_pg:execute(?SQL_INSERT_SHOP_KEY, Params) of
        {ok, 1} -> fetch_shop_key(OrgId, KeyId);
        {ok, 0} -> {error, no_row};
        {error, Reason} -> {error, cs_pg_common:normalize_error(Reason)}
    end;
insert_shop_key(_OrgId, _Key) ->
    {error, invalid_shop_key}.

-spec fetch_shop_key(integer(), integer()) -> {ok, map()} | {error, term()}.
fetch_shop_key(OrgId, KeyId) ->
    to_status_row(cs_pg_common:fetch_one(?SQL_FETCH_SHOP_KEY, [OrgId, KeyId], ?SHOP_KEY_KEYS)).

-spec fetch_shop_key_by_digest(integer(), binary()) -> {ok, map()} | {error, term()}.
fetch_shop_key_by_digest(OrgId, Digest) ->
    to_status_row(
        cs_pg_common:fetch_one(?SQL_FETCH_SHOP_KEY_BY_DIGEST, [OrgId, Digest], ?SHOP_KEY_KEYS)
    ).

%% fetch_one/fetch_many 只做行归一化（atom 键），status 列按 cs_pg_common 契约
%% 由调用方转 atom；cs_access_app:verify_shop_key/2 以 atom `active` 判定，
%% 漏转会让所有有效 shop key 被误判 revoked（DEFECT-1，CS-04 E2E 发现）。
to_status_row({ok, Row}) -> {ok, maps:update_with(status, fun cs_pg_common:to_status/1, Row)};
to_status_row({error, _} = Err) -> Err.

-spec revoke_shop_key(integer(), integer(), integer()) -> ok | {error, term()}.
revoke_shop_key(OrgId, KeyId, At) ->
    revoke(?SQL_REVOKE_SHOP_KEY, [OrgId, KeyId, At]).

%% ===================================================================
%% visit token
%% ===================================================================

-spec insert_visit_token(integer(), map()) -> {ok, map()} | {error, term()}.
insert_visit_token(OrgId, Token) when is_map(Token) ->
    TokenId = maps:get(id, Token),
    Params = [
        TokenId,
        OrgId,
        maps:get(contact_id, Token),
        maps:get(token_digest, Token),
        maps:get(expires_at, Token),
        cs_pg_common:nullify(maps:get(created_by_business_identity_id, Token, undefined)),
        cs_pg_common:nullify(maps:get(created_by_user_id, Token, undefined))
    ],
    case elib_pg:execute(?SQL_INSERT_VISIT_TOKEN, Params) of
        {ok, 1} -> fetch_visit_token(OrgId, TokenId);
        {ok, 0} -> {error, no_row};
        {error, Reason} -> {error, cs_pg_common:normalize_error(Reason)}
    end;
insert_visit_token(_OrgId, _Token) ->
    {error, invalid_visit_token}.

-spec fetch_visit_token(integer(), integer()) -> {ok, map()} | {error, term()}.
fetch_visit_token(OrgId, TokenId) ->
    cs_pg_common:fetch_one(?SQL_FETCH_VISIT_TOKEN, [OrgId, TokenId], ?VISIT_TOKEN_KEYS).

-spec fetch_visit_token_by_digest(integer(), binary()) -> {ok, map()} | {error, term()}.
fetch_visit_token_by_digest(OrgId, Digest) ->
    cs_pg_common:fetch_one(
        ?SQL_FETCH_VISIT_TOKEN_BY_DIGEST, [OrgId, Digest], ?VISIT_TOKEN_KEYS
    ).

-spec revoke_visit_token(integer(), integer(), integer()) -> ok | {error, term()}.
revoke_visit_token(OrgId, TokenId, At) ->
    revoke(?SQL_REVOKE_VISIT_TOKEN, [OrgId, TokenId, At]).

%% ===================================================================
%% 内部辅助
%% ===================================================================

revoke(Sql, Params) ->
    case elib_pg:execute(Sql, Params) of
        {ok, 1} -> ok;
        {ok, 0} -> {error, not_found};
        {error, Reason} -> {error, cs_pg_common:normalize_error(Reason)}
    end.
