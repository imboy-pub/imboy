-module(compliance_key_repo).
%%%
% compliance_key_repo 是合规密钥数据仓库层
% 提供三层加密架构中合规密钥对的 CRUD 操作
%%%

-include("log.hrl").

-export([tablename/0]).
-export([create/4]).
-export([find_active/0]).
-export([find_by_key_id/1]).
-export([list_all/0]).
-export([revoke/2]).

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc 获取合规密钥表的表名
-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"compliance_key">>).

%% @doc 创建合规密钥
%% @param KeyId 密钥标识符
%% @param PublicKey 公钥（PEM 格式）
%% @param PrivateKeyEncrypted 加密后的私钥
%% @param CreatedBy 创建者管理员ID
%% @return {ok, KeyId} | {error, Reason}
-spec create(binary(), binary(), binary(), integer()) -> {ok, binary()} | {error, term()}.
create(KeyId, PublicKey, PrivateKeyEncrypted, CreatedBy) ->
    Tb = tablename(),
    Data = #{
        key_id => KeyId,
        public_key => PublicKey,
        private_key_encrypted => PrivateKeyEncrypted,
        created_by => CreatedBy,
        status => 1
    },
    case elib_pg:insert(Tb, Data, <<>>) of
        {ok, _} -> {ok, KeyId};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 查找当前活跃的合规密钥
%% @return {ok, Row} | {error, not_found}
-spec find_active() -> {ok, map()} | {error, not_found | term()}.
find_active() ->
    Tb = tablename(),
    Sql = <<"SELECT key_id, public_key, algorithm, created_at "
            "FROM ", Tb/binary,
            " WHERE status = 1"
            " ORDER BY created_at DESC LIMIT 1">>,
    case elib_pg:query(Sql, []) of
        {ok, [Row]} when is_map(Row) -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 根据 key_id 查找密钥
%% @param KeyId 密钥标识符
%% @return {ok, Row} | {error, not_found}
-spec find_by_key_id(binary()) -> {ok, map()} | {error, not_found | term()}.
find_by_key_id(KeyId) ->
    Tb = tablename(),
    Sql = <<"SELECT key_id, public_key, private_key_encrypted, algorithm, status, created_at "
            "FROM ", Tb/binary,
            " WHERE key_id = $1 LIMIT 1">>,
    case elib_pg:query(Sql, [KeyId]) of
        {ok, [Row]} when is_map(Row) -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 列出所有合规密钥
%% @return {ok, [Row]}
-spec list_all() -> {ok, list(map())} | {error, term()}.
list_all() ->
    Tb = tablename(),
    Sql = <<"SELECT key_id, algorithm, status, created_by, created_at, revoked_at "
            "FROM ", Tb/binary,
            " ORDER BY created_at DESC">>,
    elib_pg:query(Sql, []).

%% @doc 撤销密钥
%% @param KeyId 密钥标识符
%% @param RevokedBy 撤销者管理员ID
%% @return {ok, AffectedRows} | {error, Reason}
-spec revoke(binary(), integer()) -> {ok, non_neg_integer()} | {error, term()}.
revoke(KeyId, RevokedBy) ->
    Tb = tablename(),
    Sql = <<"UPDATE ", Tb/binary,
           " SET status = 0, revoked_at = CURRENT_TIMESTAMP, revoked_by = $1, updated_at = CURRENT_TIMESTAMP"
           " WHERE key_id = $2 AND status = 1">>,
    case elib_pg:execute(Sql, [RevokedBy, KeyId]) of
        {ok, AffectedRows} -> {ok, AffectedRows};
        {error, Reason} -> {error, Reason}
    end.
