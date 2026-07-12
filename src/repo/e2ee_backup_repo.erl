-module(e2ee_backup_repo).
%%%===================================================================
%%% @doc E2EE 加密密钥备份 Repo 层（4S 模式）
%%%
%%% 服务端只存客户端加密后的密文包与 KDF 参数（零信任），
%%% 本模块只做参数化 SQL，不含任何密码学逻辑。
%%%===================================================================

%%===================================================================
%%% API Functions Export
%%===================================================================
-export([save/1]).
-export([latest/1]).
-export([delete_by_uid/1]).

%%===================================================================
%%% Query Functions
%%===================================================================

%% @doc 写入一个新版本的加密备份（版本唯一性由 UNIQUE(uid, backup_version) 兜底）
-spec save(map()) -> {ok, integer()} | {error, term()}.
save(Params) ->
    Uid = maps:get(<<"uid">>, Params),
    BackupVersion = maps:get(<<"backup_version">>, Params),
    Algo = maps:get(<<"algo">>, Params),
    KdfSalt = maps:get(<<"kdf_salt">>, Params),
    KdfIterations = maps:get(<<"kdf_iterations">>, Params),
    EncryptedPayload = maps:get(<<"encrypted_payload">>, Params),
    PayloadHash = maps:get(<<"payload_hash">>, Params),

    Id = elib_tsid:generate(e2ee_key_backup),
    Sql1 =
        <<"INSERT INTO e2ee_key_backups ",
            "(id, uid, backup_version, algo, kdf_salt, kdf_iterations, ",
            "encrypted_payload, payload_hash) "
            "VALUES ($1, $2, $3, $4, $5, $6, $7, $8)">>,
    case
        elib_pg:execute(Sql1, [
            Id,
            Uid,
            BackupVersion,
            Algo,
            KdfSalt,
            KdfIterations,
            EncryptedPayload,
            PayloadHash
        ])
    of
        {ok, _Count} -> {ok, Id};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 取用户最新版本的备份（含密文，恢复端用）
-spec latest(integer()) -> {ok, map()} | {error, not_found} | {error, term()}.
latest(Uid) ->
    Sql1 =
        <<"SELECT id, uid, backup_version, algo, kdf_salt, kdf_iterations, ",
            "encrypted_payload, payload_hash, created_at ", "FROM e2ee_key_backups ",
            "WHERE uid = $1 ", "ORDER BY backup_version DESC LIMIT 1">>,
    case elib_pg:query(Sql1, [Uid]) of
        {ok, []} -> {error, not_found};
        {ok, [Row | _]} -> {ok, Row};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 删除用户全部备份版本，返回删除行数
-spec delete_by_uid(integer()) -> {ok, non_neg_integer()} | {error, term()}.
delete_by_uid(Uid) ->
    Sql1 = <<"DELETE FROM e2ee_key_backups WHERE uid = $1">>,
    elib_pg:execute(Sql1, [Uid]).
