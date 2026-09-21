-module(enterprise_oa_sso_code_repo).

%%%
% enterprise_oa_sso_code_repo 是 OA 一次性 SSO code 仓储层（迁移 00000136，
% EPGZ-01 / plan-gz §7.2）。code 只存 SHA-256 digest（全局唯一即消费定位键），
% 绑定 org/app/user/redirect_uri/nonce；exchange 是原子单次消费（CAS）。
%
% 表结构：enterprise_oa_sso_code(id TSID PK, organization_id, application_id,
%   user_id, code_digest 全局唯一 64hex, redirect_uri https, nonce_digest 64hex,
%   expires_at NOT NULL, consumed_at 可空, created_at)。
%%%

-export([
    tablename/0,
    next_id/0,
    digest_hex/1,
    issue_tx/8,
    find_by_digest_tx/2,
    consume_tx/2,
    consume_tx/3
]).

-define(COLUMNS, <<
    "id, organization_id, application_id, user_id, code_digest, redirect_uri, "
    "nonce_digest, expires_at, consumed_at, created_at"
>>).

%% ===================================================================
%% API functions
%% ===================================================================

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"enterprise_oa_sso_code">>).

%% @doc sso code 命名空间 TSID（惰性注册，镜像 agent_grant_pg 口径）。
-spec next_id() -> pos_integer().
next_id() ->
    case lists:member(enterprise_oa_sso_code, elib_tsid:registered()) of
        true -> ok;
        false -> elib_tsid:register(enterprise_oa_sso_code)
    end,
    elib_tsid:generate(enterprise_oa_sso_code).

%% @doc opaque code / nonce 的 SHA-256 小写 hex（64 字符；镜像 bot_repo:digest_hex/1）。
-spec digest_hex(binary()) -> binary().
digest_hex(Value) when is_binary(Value) ->
    binary:encode_hex(crypto:hash(sha256, Value), lowercase).

%% @doc 事务内签发一次性 code（HUMAN-SSO-01：60 秒；ExpiresAt 二进制 RFC3339）。
%% CodeDigest/NonceDigest 由上层先 digest_hex/1 计算，明文 code 只在签发响应
%% 出现一次。redirect_uri 必须是 exact HTTPS origin（ck_eosc_redirect_https）。
-spec issue_tx(any(), integer(), integer(), integer(), binary(), binary(), binary(), binary()) ->
    {ok, map()} | {error, term()}.
issue_tx(Conn, OrgId, AppId, UserId, CodeDigest, RedirectUri, NonceDigest, ExpiresAt) when
    is_integer(OrgId),
    is_integer(AppId),
    is_integer(UserId),
    is_binary(CodeDigest),
    is_binary(RedirectUri),
    is_binary(NonceDigest),
    is_binary(ExpiresAt)
->
    Tb = tablename(),
    Id = next_id(),
    Now = elib_dt:now(),
    Sql =
        <<"INSERT INTO ", Tb/binary, " (id, organization_id, application_id, user_id, code_digest,",
            " redirect_uri, nonce_digest, expires_at, created_at)",
            " VALUES ($1, $2, $3, $4, $5, $6, $7, $8::timestamptz, $9)", " RETURNING ",
            ?COLUMNS/binary>>,
    case
        elib_pg:query(Conn, Sql, [
            Id, OrgId, AppId, UserId, CodeDigest, RedirectUri, NonceDigest, ExpiresAt, Now
        ])
    of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {error, insert_empty_result};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 事务内按 digest 取行（任意状态；过期/已消费判定在 consume_tx/2）。
-spec find_by_digest_tx(any(), binary()) -> {ok, map()} | {error, not_found | term()}.
find_by_digest_tx(Conn, CodeDigest) when is_binary(CodeDigest), CodeDigest =/= <<>> ->
    Sql =
        <<"SELECT ", ?COLUMNS/binary, ", (expires_at < CURRENT_TIMESTAMP) AS expired", " FROM ",
            (tablename())/binary, " WHERE code_digest = $1 LIMIT 1">>,
    case elib_pg:query(Conn, Sql, [CodeDigest]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 事务内原子消费一次性 code（INT-14：单次消费 CAS）。
%% 单条 UPDATE 的行锁天然串行化并发 exchange：WHERE consumed_at IS NULL AND
%% expires_at > CURRENT_TIMESTAMP，首个事务命中 1 行并写入 consumed_at，
%% 并发第二个事务同 UPDATE 只能命中 0 行（已被写锁排除且重扫不可见）。
%% 成功返回 {ok, Row}；失败分类：not_found / already_consumed（重放拒绝，
%% 不是重放响应）/ expired。
-spec consume_tx(any(), binary()) ->
    {ok, map()} | {error, not_found | already_consumed | expired | term()}.
consume_tx(Conn, CodeDigest) when is_binary(CodeDigest), CodeDigest =/= <<>> ->
    Tb = tablename(),
    Now = elib_dt:now(),
    Sql =
        <<"UPDATE ", Tb/binary, " SET consumed_at = $1::timestamptz",
            " WHERE code_digest = $2 AND consumed_at IS NULL",
            " AND expires_at > CURRENT_TIMESTAMP", " RETURNING ", ?COLUMNS/binary>>,
    case elib_pg:query(Conn, Sql, [Now, CodeDigest]) of
        {ok, [Row | _]} ->
            {ok, Row};
        {ok, []} ->
            %% 0 行命中：行不存在 / 已消费 / 已过期三分支分类（只读，不写）
            case find_by_digest_tx(Conn, CodeDigest) of
                {ok, Row} ->
                    case maps:get(<<"consumed_at">>, Row) of
                        null ->
                            {error, expired};
                        _ConsumedAt when is_binary(_ConsumedAt) ->
                            {error, already_consumed}
                    end;
                {error, not_found} ->
                    {error, not_found};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc consume_tx/2 的校验变体：exchange 还必须回传与签发一致的
%% redirect_uri（exact 匹配）。不匹配归一 {error, redirect_mismatch}。
-spec consume_tx(any(), binary(), binary()) ->
    {ok, map()} | {error, not_found | already_consumed | expired | redirect_mismatch | term()}.
consume_tx(Conn, CodeDigest, RedirectUri) when is_binary(RedirectUri) ->
    case consume_tx(Conn, CodeDigest) of
        {ok, Row} ->
            case maps:get(<<"redirect_uri">>, Row) =:= RedirectUri of
                true -> {ok, Row};
                false -> {error, redirect_mismatch}
            end;
        Other ->
            Other
    end.
