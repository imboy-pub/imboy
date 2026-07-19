-module(compliance_key_repo).
%%%
% compliance_key_repo 是合规密钥数据仓库层
% 提供合规公钥的 CRUD 操作（仅存公钥侧；合规私钥由审计方本地保管，服务端零接触）。
%%%

-include("log.hrl").

-export([tablename/0]).
-export([create/3]).
-export([find_active/0]).
-export([list_all/0]).
-export([revoke/2]).

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc 获取合规密钥表的表名
-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"compliance_key">>).

%% @doc 创建合规密钥（仅公钥）
%% 零信任改造（线 A）：合规私钥永不上传服务端，由审计方在本地（HSM / 离线介质）
%% 生成与保管。服务端只存公钥供客户端在 compliance 模式下额外 wrap 一份 AES key。
%%
%% @param KeyId 密钥标识符
%% @param PublicKey 公钥（PEM 格式）
%% @param CreatedBy 创建者管理员ID
%% @return {ok, KeyId} | {error, Reason}
-spec create(binary(), binary(), integer()) -> {ok, binary()} | {error, term()}.
create(KeyId, PublicKey, CreatedBy) ->
    Tb = tablename(),
    Data = #{
        key_id => KeyId,
        public_key => PublicKey,
        created_by => CreatedBy,
        status => 1
    },
    Id = elib_tsid:generate(compliance_key),
    Data2 = Data#{id => Id},
    {Sql, Params} = elib_pg_sql:insert(Tb, Data2),
    case elib_pg:query(Sql, Params) of
        {ok, _} -> {ok, KeyId};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 查找当前活跃的合规密钥
%% @return {ok, Row} | {error, not_found}
-spec find_active() -> {ok, map()} | {error, not_found | term()}.
find_active() ->
    Tb = tablename(),
    Sql = <<
        "SELECT key_id, public_key, algorithm, created_at "
        "FROM ",
        Tb/binary,
        " WHERE status = 1"
        " ORDER BY created_at DESC LIMIT 1"
    >>,
    case elib_pg:query(Sql, []) of
        {ok, [Row]} when is_map(Row) -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 列出所有合规密钥
%% @return {ok, [Row]}
-spec list_all() -> {ok, list(map())} | {error, term()}.
list_all() ->
    Tb = tablename(),
    Sql = <<
        "SELECT key_id, algorithm, status, created_by, created_at, revoked_at "
        "FROM ",
        Tb/binary,
        " ORDER BY created_at DESC"
    >>,
    elib_pg:query(Sql, []).

%% @doc 撤销密钥
%% @param KeyId 密钥标识符
%% @param RevokedBy 撤销者管理员ID
%% @return {ok, AffectedRows} | {error, Reason}
-spec revoke(binary(), integer()) -> {ok, non_neg_integer()} | {error, term()}.
revoke(KeyId, RevokedBy) ->
    Tb = tablename(),
    Sql =
        <<"UPDATE ", Tb/binary,
            " SET status = 0, revoked_at = CURRENT_TIMESTAMP, revoked_by = $1, updated_at = CURRENT_TIMESTAMP"
            " WHERE key_id = $2 AND status = 1">>,
    case elib_pg:execute(Sql, [RevokedBy, KeyId]) of
        {ok, AffectedRows} -> {ok, AffectedRows};
        {error, Reason} -> {error, Reason}
    end.
