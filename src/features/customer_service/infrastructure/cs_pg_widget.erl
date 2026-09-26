%%% @doc Widget 安装 / 身份密钥 / bootstrap 令牌 / JTI nonce 的 PG 实现
%%% （`cs_store_port` 的 widget 段；CSB-01）。
%%%
%%% 安全不变量（CSB-01-A03/A05）：
%%%   * 明文 secret / signing key / JTI 绝不落库——本模块只接收与存储
%%%     digest（sha256 hex，由 application 以既有 helper 计算）；任何 SELECT
%%%     都不含明文列（表结构上也不存在）。
%%%   * bootstrap 令牌**复用** customer_service_visit_token（只追加
%%%     widget_installation_id / anonymous_subject_hmac / last_seen_at 列），
%%%     不复制新表；digest / expiry / revoke 口径与既有 visit token 完全一致。
%%%   * public_widget_id 是公开标识（非 secret），但解析必须同语句携带
%%%     organization_id——跨 Org 命中不了行（not_found，CSB-01-A02）。
%%%   * JTI 重放由 uq_cswn_install_jti 复合唯一裁决：23505 → `{error, replay}`。
%%% 铁律 6：每条 SQL 同语句带 `organization_id`（`$1`）。
-module(cs_pg_widget).

-export([
    insert_widget_installation/2,
    fetch_widget_installation/2,
    fetch_widget_installation_by_public_id/2,
    fetch_widget_installation_by_public_id_global/1,
    list_widget_installations_page/3,
    revoke_widget_installation/3,
    update_widget_installation/4,
    insert_widget_identity_key/3,
    fetch_widget_identity_key/3,
    revoke_widget_identity_key/4,
    insert_widget_bootstrap_token/2,
    fetch_widget_bootstrap_token_by_digest/3,
    fetch_widget_bootstrap_token_by_digest_global/2,
    touch_widget_bootstrap_token/4,
    revoke_widget_bootstrap_token/4,
    record_widget_nonce/4,
    default_workspace/1,
    sql_statements/0
]).

-define(INSTALLATION_KEYS, [
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

-define(IDENTITY_KEY_KEYS, [
    id,
    organization_id,
    installation_id,
    key_digest,
    key_version,
    display_hint,
    status,
    expires_at,
    revoked_at,
    created_at,
    updated_at
]).

%% bootstrap 令牌行落在 customer_service_visit_token 上（复用存储）；
%% 不返回 token_digest（投影由 application 白名单裁剪，digest 永不出 store）。
-define(BOOTSTRAP_KEYS, [
    id,
    organization_id,
    contact_id,
    widget_installation_id,
    anonymous_subject_hmac,
    expires_at,
    revoked_at,
    last_seen_at,
    version,
    created_at
]).

-define(SQL_INSERT_INSTALLATION, <<
    "INSERT INTO customer_service_widget_installation"
    " (id, organization_id, public_widget_id, display_name,"
    "  allowed_origins, branding, consent_version, created_by_user_id)"
    " VALUES ($1, $2, $3, $4, $5::jsonb, $6::jsonb, $7, $8)"
>>).

-define(SQL_FETCH_INSTALLATION, <<
    "SELECT id, organization_id, public_widget_id, display_name,"
    "       allowed_origins, branding, consent_version, status,"
    "       extract(epoch from revoked_at)::bigint AS revoked_at, version,"
    "       extract(epoch from created_at)::bigint AS created_at,"
    "       extract(epoch from updated_at)::bigint AS updated_at"
    "  FROM customer_service_widget_installation"
    " WHERE organization_id = $1 AND id = $2"
>>).

-define(SQL_FETCH_INSTALLATION_BY_PUBLIC_ID, <<
    "SELECT id, organization_id, public_widget_id, display_name,"
    "       allowed_origins, branding, consent_version, status,"
    "       extract(epoch from revoked_at)::bigint AS revoked_at, version,"
    "       extract(epoch from created_at)::bigint AS created_at,"
    "       extract(epoch from updated_at)::bigint AS updated_at"
    "  FROM customer_service_widget_installation"
    " WHERE organization_id = $1 AND public_widget_id = $2"
>>).

%% CSD-BE-01（hosted-widget-contract S3）：public_widget_id **全局**反查——
%% 与铁律 6 的租户语句**不同类**：输入只有公开 ID（单占位符 $1 =
%% public_widget_id），`organization_id` 只出现在 SELECT 投影（从行**输出**，
%% 权威派生租户），谓词零 Org——错 Org 的调用方根本不存在（调用方是浏览器，
%% 无 Org 可带）。uq_cswi_public_widget_id 全局唯一约束保证单行。
%% 本语句**不进** sql_statements/0（那是「同语句带 Org」的机械断言集，
%% 语义上不适用）；其形状由 cs_pg_widget_tests 的专属机械断言单独冻结：
%% 恰一个占位符且谓词为 public_widget_id = $1。
-define(SQL_FETCH_INSTALLATION_BY_PUBLIC_ID_GLOBAL, <<
    "SELECT id, organization_id, public_widget_id, display_name,"
    "       allowed_origins, branding, consent_version, status,"
    "       extract(epoch from revoked_at)::bigint AS revoked_at, version,"
    "       extract(epoch from created_at)::bigint AS created_at,"
    "       extract(epoch from updated_at)::bigint AS updated_at"
    "  FROM customer_service_widget_installation"
    " WHERE public_widget_id = $1"
>>).

-define(SQL_LIST_INSTALLATIONS_PAGE, <<
    "SELECT id, organization_id, public_widget_id, display_name,"
    "       allowed_origins, branding, consent_version, status,"
    "       extract(epoch from revoked_at)::bigint AS revoked_at, version,"
    "       extract(epoch from created_at)::bigint AS created_at,"
    "       extract(epoch from updated_at)::bigint AS updated_at"
    "  FROM customer_service_widget_installation"
    " WHERE organization_id = $1 AND ($2::bigint = 0 OR id < $2)"
    " ORDER BY id DESC LIMIT $3"
>>).

-define(SQL_REVOKE_INSTALLATION, <<
    "UPDATE customer_service_widget_installation"
    "   SET status = 'revoked', revoked_at = to_timestamp($3),"
    "       version = version + 1, updated_at = to_timestamp($3)"
    " WHERE organization_id = $1 AND id = $2 AND status = 'active'"
>>).

%% 可编辑投影只含配置键：public_widget_id / status / revoked_at 不在 SET 内。
%% status='active' 谓词与 revoke 同口径——已吊销行零命中（0 行 → 应用层
%% fetch 区分 not_found / installation_revoked，无存在性枚举差异）。
-define(SQL_UPDATE_INSTALLATION, <<
    "UPDATE customer_service_widget_installation"
    "   SET display_name = $3, allowed_origins = $4::jsonb, branding = $5::jsonb,"
    "       consent_version = $6, version = version + 1, updated_at = to_timestamp($7)"
    " WHERE organization_id = $1 AND id = $2 AND status = 'active'"
>>).

-define(SQL_INSERT_IDENTITY_KEY, <<
    "INSERT INTO customer_service_widget_identity_key"
    " (id, organization_id, installation_id, key_digest, key_version, display_hint, expires_at)"
    " VALUES ($1, $2, $3, $4, $5, $6, to_timestamp($7))"
>>).

-define(SQL_FETCH_IDENTITY_KEY, <<
    "SELECT id, organization_id, installation_id, key_digest, key_version, display_hint,"
    "       status,"
    "       extract(epoch from expires_at)::bigint AS expires_at,"
    "       extract(epoch from revoked_at)::bigint AS revoked_at,"
    "       extract(epoch from created_at)::bigint AS created_at,"
    "       extract(epoch from updated_at)::bigint AS updated_at"
    "  FROM customer_service_widget_identity_key"
    " WHERE organization_id = $1 AND installation_id = $2 AND key_version = $3"
>>).

-define(SQL_REVOKE_IDENTITY_KEY, <<
    "UPDATE customer_service_widget_identity_key"
    "   SET status = 'revoked', revoked_at = to_timestamp($4),"
    "       updated_at = to_timestamp($4)"
    " WHERE organization_id = $1 AND installation_id = $2 AND key_version = $3"
    "   AND status = 'active'"
>>).

%% bootstrap 令牌落 customer_service_visit_token（复用存储；见模块 doc）。
-define(SQL_INSERT_BOOTSTRAP_TOKEN, <<
    "INSERT INTO customer_service_visit_token"
    " (id, organization_id, contact_id, token_digest, expires_at,"
    "  widget_installation_id, anonymous_subject_hmac)"
    " VALUES ($1, $2, $3, $4, to_timestamp($5), $6, $7)"
>>).

-define(SQL_FETCH_BOOTSTRAP_BY_DIGEST, <<
    "SELECT id, organization_id, contact_id, widget_installation_id,"
    "       anonymous_subject_hmac, display_hint,"
    "       extract(epoch from expires_at)::bigint AS expires_at,"
    "       extract(epoch from revoked_at)::bigint AS revoked_at,"
    "       extract(epoch from last_seen_at)::bigint AS last_seen_at, version,"
    "       extract(epoch from created_at)::bigint AS created_at"
    "  FROM customer_service_visit_token"
    " WHERE organization_id = $1 AND widget_installation_id = $2"
    "   AND token_digest = $3"
>>).

%% CSD-BE-01S（hosted-widget-contract S3 v1.1）：持 token 动作面的 Org 权威
%% 派生——与铁律 6 的租户语句**不同类**（fetch_installation_by_public_id_global
%% 同款先例）：输入只有 (installation_id, digest)，谓词零 Org，
%% `organization_id` 只出现在 SELECT 投影（从命中行**输出**，token 行本就
%% 绑定 (org, installation)）。digest = sha256(secret)，命中前提是持明文
%% secret——无存在性枚举面。本语句不进 sql_statements/0（「同语句带 Org」
%% 机械断言语义上不适用），形状由 cs_pg_widget_tests 专属断言单独冻结。
-define(SQL_FETCH_BOOTSTRAP_BY_DIGEST_GLOBAL, <<
    "SELECT id, organization_id, contact_id, widget_installation_id,"
    "       anonymous_subject_hmac, display_hint,"
    "       extract(epoch from expires_at)::bigint AS expires_at,"
    "       extract(epoch from revoked_at)::bigint AS revoked_at,"
    "       extract(epoch from last_seen_at)::bigint AS last_seen_at, version,"
    "       extract(epoch from created_at)::bigint AS created_at"
    "  FROM customer_service_visit_token"
    " WHERE widget_installation_id = $1 AND token_digest = $2"
>>).

-define(SQL_TOUCH_BOOTSTRAP_TOKEN, <<
    "UPDATE customer_service_visit_token"
    "   SET last_seen_at = to_timestamp($4)"
    " WHERE organization_id = $1 AND widget_installation_id = $2 AND id = $3"
    "   AND revoked_at IS NULL"
>>).

-define(SQL_REVOKE_BOOTSTRAP_TOKEN, <<
    "UPDATE customer_service_visit_token"
    "   SET revoked_at = to_timestamp($4), version = version + 1,"
    "       updated_at = to_timestamp($4)"
    " WHERE organization_id = $1 AND widget_installation_id = $2 AND id = $3"
    "   AND revoked_at IS NULL"
>>).

-define(SQL_INSERT_NONCE, <<
    "INSERT INTO customer_service_widget_nonce"
    " (id, organization_id, installation_id, jti_digest, expires_at)"
    " VALUES ($1, $2, $3, $4, to_timestamp($5))"
>>).

%% CSB-02R：widget 装配的本 Org 缺省 Workspace 解析——org 作用域 active 且
%% id 最小（确定性规则，与 EB 成员面同口径但**不做成员连接**：访客没有
%% membership）。规则冻结于 SQL，调用方不可指定或切换。
-define(SQL_DEFAULT_WORKSPACE, <<
    "SELECT w.id AS workspace_id"
    "  FROM workspace w"
    " WHERE w.organization_id = $1 AND w.status = 'active'"
    " ORDER BY w.id"
    " LIMIT 1"
>>).

-spec default_workspace(integer()) -> {ok, integer()} | {error, not_found | term()}.
default_workspace(OrgId) when is_integer(OrgId) ->
    case cs_pg_common:fetch_one(?SQL_DEFAULT_WORKSPACE, [OrgId], [workspace_id]) of
        {ok, #{workspace_id := Ws}} when is_integer(Ws) -> {ok, Ws};
        {error, _} = Err -> Err
    end;
default_workspace(_OrgId) ->
    {error, {invalid_argument, default_workspace}}.

%% @doc 冻结语句（供租户键机械断言：每条都同语句带 organization_id）。
-spec sql_statements() -> [binary()].
sql_statements() ->
    [
        ?SQL_INSERT_INSTALLATION,
        ?SQL_FETCH_INSTALLATION,
        ?SQL_FETCH_INSTALLATION_BY_PUBLIC_ID,
        ?SQL_LIST_INSTALLATIONS_PAGE,
        ?SQL_REVOKE_INSTALLATION,
        ?SQL_UPDATE_INSTALLATION,
        ?SQL_INSERT_IDENTITY_KEY,
        ?SQL_FETCH_IDENTITY_KEY,
        ?SQL_REVOKE_IDENTITY_KEY,
        ?SQL_INSERT_BOOTSTRAP_TOKEN,
        ?SQL_FETCH_BOOTSTRAP_BY_DIGEST,
        ?SQL_TOUCH_BOOTSTRAP_TOKEN,
        ?SQL_REVOKE_BOOTSTRAP_TOKEN,
        ?SQL_INSERT_NONCE,
        ?SQL_DEFAULT_WORKSPACE
    ].

%% ===================================================================
%% widget installation
%% ===================================================================

-spec insert_widget_installation(integer(), map()) -> {ok, map()} | {error, term()}.
insert_widget_installation(OrgId, Installation) when is_map(Installation) ->
    InstallationId = maps:get(id, Installation),
    Params = [
        InstallationId,
        OrgId,
        maps:get(public_widget_id, Installation),
        maps:get(display_name, Installation),
        cs_pg_common:jsonb(maps:get(allowed_origins, Installation, [])),
        cs_pg_common:jsonb(maps:get(branding, Installation, #{})),
        maps:get(consent_version, Installation),
        cs_pg_common:nullify(maps:get(created_by_user_id, Installation, undefined))
    ],
    case elib_pg:execute(?SQL_INSERT_INSTALLATION, Params) of
        {ok, 1} -> fetch_widget_installation(OrgId, InstallationId);
        {ok, 0} -> {error, no_row};
        {error, Reason} -> {error, cs_pg_common:normalize_error(Reason)}
    end;
insert_widget_installation(_OrgId, _Installation) ->
    {error, invalid_widget_installation}.

-spec fetch_widget_installation(integer(), integer()) -> {ok, map()} | {error, term()}.
fetch_widget_installation(OrgId, InstallationId) ->
    to_status_row(
        decode_installation_jsonb(
            cs_pg_common:fetch_one(
                ?SQL_FETCH_INSTALLATION, [OrgId, InstallationId], ?INSTALLATION_KEYS
            )
        )
    ).

%% @doc 公开标识解析：错 Org 的查询命中不了行（not_found；无存在性枚举）。
-spec fetch_widget_installation_by_public_id(integer(), binary()) ->
    {ok, map()} | {error, term()}.
fetch_widget_installation_by_public_id(OrgId, PublicWidgetId) ->
    to_status_row(
        decode_installation_jsonb(
            cs_pg_common:fetch_one(
                ?SQL_FETCH_INSTALLATION_BY_PUBLIC_ID, [OrgId, PublicWidgetId], ?INSTALLATION_KEYS
            )
        )
    ).

%% @doc 全局反查（CSD-BE-01，hosted-widget-contract S3）：无 Org 输入，
%% organization_id 从命中行输出。不存在 → not_found（application 归一为
%% installation_unavailable，三态不区分、无枚举）。
-spec fetch_widget_installation_by_public_id_global(binary()) ->
    {ok, map()} | {error, term()}.
fetch_widget_installation_by_public_id_global(PublicWidgetId) when is_binary(PublicWidgetId) ->
    to_status_row(
        decode_installation_jsonb(
            cs_pg_common:fetch_one(
                ?SQL_FETCH_INSTALLATION_BY_PUBLIC_ID_GLOBAL,
                [PublicWidgetId],
                ?INSTALLATION_KEYS
            )
        )
    );
fetch_widget_installation_by_public_id_global(_PublicWidgetId) ->
    {error, {invalid_argument, public_widget_id}}.

-spec list_widget_installations_page(integer(), non_neg_integer(), pos_integer()) ->
    {ok, [map()]} | {error, term()}.
list_widget_installations_page(OrgId, AfterId, Limit) ->
    case
        cs_pg_common:fetch_many(
            ?SQL_LIST_INSTALLATIONS_PAGE, [OrgId, AfterId, Limit], ?INSTALLATION_KEYS
        )
    of
        {ok, Rows} ->
            {ok, [finish_installation_row(Row) || Row <- Rows]};
        {error, _} = Err ->
            Err
    end.

-spec revoke_widget_installation(integer(), integer(), integer()) -> ok | {error, term()}.
revoke_widget_installation(OrgId, InstallationId, At) ->
    update_exactly_one(?SQL_REVOKE_INSTALLATION, [OrgId, InstallationId, At]).

-spec update_widget_installation(integer(), integer(), integer(), map()) ->
    {ok, map()} | {error, term()}.
update_widget_installation(OrgId, InstallationId, At, Updates) when is_map(Updates) ->
    Params = [
        OrgId,
        InstallationId,
        maps:get(display_name, Updates),
        cs_pg_common:jsonb(maps:get(allowed_origins, Updates, [])),
        cs_pg_common:jsonb(maps:get(branding, Updates, #{})),
        maps:get(consent_version, Updates),
        At
    ],
    case elib_pg:execute(?SQL_UPDATE_INSTALLATION, Params) of
        {ok, 1} ->
            fetch_widget_installation(OrgId, InstallationId);
        {ok, 0} ->
            %% 0 行 = 不存在或已吊销（status='active' 谓词零命中）：fetch 区分，
            %% 管理面 403 installation_revoked 与 404 not_found 语义不混装。
            case fetch_widget_installation(OrgId, InstallationId) of
                {ok, #{status := Status}} when Status =/= active ->
                    {error, installation_revoked};
                {ok, _} ->
                    {error, not_found};
                {error, _} = Err ->
                    Err
            end;
        {error, Reason} ->
            {error, cs_pg_common:normalize_error(Reason)}
    end;
update_widget_installation(_OrgId, _InstallationId, _At, _Updates) ->
    {error, invalid_widget_installation}.

%% ===================================================================
%% widget identity signing key（只存 digest）
%% ===================================================================

-spec insert_widget_identity_key(integer(), integer(), map()) ->
    {ok, map()} | {error, term()}.
insert_widget_identity_key(OrgId, InstallationId, Key) when is_map(Key) ->
    KeyId = maps:get(id, Key),
    Params = [
        KeyId,
        OrgId,
        InstallationId,
        maps:get(key_digest, Key),
        maps:get(key_version, Key),
        cs_pg_common:nullify(maps:get(display_hint, Key, undefined)),
        maps:get(expires_at, Key)
    ],
    case elib_pg:execute(?SQL_INSERT_IDENTITY_KEY, Params) of
        {ok, 1} ->
            fetch_widget_identity_key(OrgId, InstallationId, maps:get(key_version, Key));
        {ok, 0} ->
            {error, no_row};
        {error, Reason} ->
            {error, cs_pg_common:normalize_error(Reason)}
    end;
insert_widget_identity_key(_OrgId, _InstallationId, _Key) ->
    {error, invalid_widget_identity_key}.

-spec fetch_widget_identity_key(integer(), integer(), pos_integer()) ->
    {ok, map()} | {error, term()}.
fetch_widget_identity_key(OrgId, InstallationId, KeyVersion) ->
    to_status_row(
        cs_pg_common:fetch_one(
            ?SQL_FETCH_IDENTITY_KEY, [OrgId, InstallationId, KeyVersion], ?IDENTITY_KEY_KEYS
        )
    ).

-spec revoke_widget_identity_key(integer(), integer(), pos_integer(), integer()) ->
    ok | {error, term()}.
revoke_widget_identity_key(OrgId, InstallationId, KeyVersion, At) ->
    update_exactly_one(?SQL_REVOKE_IDENTITY_KEY, [OrgId, InstallationId, KeyVersion, At]).

%% ===================================================================
%% widget bootstrap token（复用 customer_service_visit_token 存储）
%% ===================================================================

-spec insert_widget_bootstrap_token(integer(), map()) -> {ok, map()} | {error, term()}.
insert_widget_bootstrap_token(OrgId, Token) when is_map(Token) ->
    TokenId = maps:get(id, Token),
    Params = [
        TokenId,
        OrgId,
        maps:get(contact_id, Token),
        maps:get(token_digest, Token),
        maps:get(expires_at, Token),
        maps:get(widget_installation_id, Token),
        cs_pg_common:nullify(maps:get(anonymous_subject_hmac, Token, undefined))
    ],
    case elib_pg:execute(?SQL_INSERT_BOOTSTRAP_TOKEN, Params) of
        {ok, 1} ->
            fetch_widget_bootstrap_token_by_digest(
                OrgId, maps:get(widget_installation_id, Token), maps:get(token_digest, Token)
            );
        {ok, 0} ->
            {error, no_row};
        {error, Reason} ->
            {error, cs_pg_common:normalize_error(Reason)}
    end;
insert_widget_bootstrap_token(_OrgId, _Token) ->
    {error, invalid_widget_bootstrap_token}.

%% @doc digest 校验：同语句绑定 (Org, installation)——跨 Org / 跨安装命中不了行。
-spec fetch_widget_bootstrap_token_by_digest(integer(), integer(), binary()) ->
    {ok, map()} | {error, term()}.
fetch_widget_bootstrap_token_by_digest(OrgId, InstallationId, Digest) ->
    cs_pg_common:fetch_one(
        ?SQL_FETCH_BOOTSTRAP_BY_DIGEST, [OrgId, InstallationId, Digest], ?BOOTSTRAP_KEYS
    ).

%% @doc digest **全局**命中（CSD-BE-01S，hosted-widget-contract S3 v1.1）：
%% 无 Org 输入，organization_id 从命中行输出——持 token 动作面的租户派生
%% 真源（token 行本就绑定 (org, installation)）。不存在 → not_found。
-spec fetch_widget_bootstrap_token_by_digest_global(integer(), binary()) ->
    {ok, map()} | {error, term()}.
fetch_widget_bootstrap_token_by_digest_global(InstallationId, Digest) when
    is_integer(InstallationId), is_binary(Digest)
->
    cs_pg_common:fetch_one(
        ?SQL_FETCH_BOOTSTRAP_BY_DIGEST_GLOBAL, [InstallationId, Digest], ?BOOTSTRAP_KEYS
    );
fetch_widget_bootstrap_token_by_digest_global(_InstallationId, _Digest) ->
    {error, {invalid_argument, widget_bootstrap_token}}.

-spec touch_widget_bootstrap_token(integer(), integer(), integer(), integer()) ->
    ok | {error, term()}.
touch_widget_bootstrap_token(OrgId, InstallationId, TokenId, At) ->
    update_exactly_one(?SQL_TOUCH_BOOTSTRAP_TOKEN, [OrgId, InstallationId, TokenId, At]).

-spec revoke_widget_bootstrap_token(integer(), integer(), integer(), integer()) ->
    ok | {error, term()}.
revoke_widget_bootstrap_token(OrgId, InstallationId, TokenId, At) ->
    update_exactly_one(?SQL_REVOKE_BOOTSTRAP_TOKEN, [OrgId, InstallationId, TokenId, At]).

%% ===================================================================
%% widget JTI nonce（重放防护；DB 唯一裁决）
%% ===================================================================

-spec record_widget_nonce(integer(), integer(), binary(), integer()) ->
    ok | {error, replay | term()}.
record_widget_nonce(OrgId, InstallationId, JtiDigest, ExpiresAt) ->
    Params = [
        cs_tsid:new_id(cs_widget_nonce),
        OrgId,
        InstallationId,
        JtiDigest,
        ExpiresAt
    ],
    case elib_pg:execute(?SQL_INSERT_NONCE, Params) of
        {ok, 1} ->
            ok;
        {error, Reason} ->
            case cs_pg_common:normalize_error(Reason) of
                {sql, <<"23505">>, _Constraint} -> {error, replay};
                Normalized -> {error, Normalized}
            end
    end.

%% ===================================================================
%% 内部辅助
%% ===================================================================

%% fetch_one/fetch_many 只做行归一化；status 列按 cs_pg_common 契约由调用方
%% 转 atom（`active` → active；`revoked` 等 fail-closed 保留 binary）。
to_status_row({ok, Row}) ->
    {ok, maps:update_with(status, fun cs_pg_common:to_status/1, Row)};
to_status_row({error, _} = Err) ->
    Err.

%% installation 行终处理：status 归一 + jsonb 列读归一。
%% jsonb 读归一（codec 无关，见 cs_pg_common:jsonb_read/1）：无 json codec 的池
%% 读回文本 binary，须还原为 term（origin 校验只认 list）。
finish_installation_row(Row0) ->
    Row = maps:update_with(status, fun cs_pg_common:to_status/1, Row0),
    Row#{
        allowed_origins := cs_pg_common:jsonb_read(maps:get(allowed_origins, Row, [])),
        branding := cs_pg_common:jsonb_read(maps:get(branding, Row, #{}))
    }.

decode_installation_jsonb({ok, Row}) ->
    {ok, finish_installation_row(Row)};
decode_installation_jsonb({error, _} = Err) ->
    Err.

%% 恰写入 1 行才 ok；0 行 = 目标不在本 Org / 不存在 / 已终态 → not_found。
update_exactly_one(Sql, Params) ->
    case elib_pg:execute(Sql, Params) of
        {ok, 1} -> ok;
        {ok, 0} -> {error, not_found};
        {error, Reason} -> {error, cs_pg_common:normalize_error(Reason)}
    end.
