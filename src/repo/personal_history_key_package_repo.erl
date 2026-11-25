-module(personal_history_key_package_repo).
%%%
%%% personal_history_key_package_repo —— 个人历史密钥包（迁移 279，§2.5/§10.4）
%%%
%%% 自 personal_history_repo 拆出（elvis no_god_modules：主仓 37 函数超 30）。
%%% 本模块零业务策略：key_version/generation 校验与 409/422 裁决在 ds/logic
%%% 层；此处只提供行级读写。uq_phk_version 唯一冲突映射 unique_violation。
%%%

-export([key_package_tablename/0]).
-export([find_key_package/3, find_key_package_tx/4, insert_key_package/2]).
-export([find_key_package_for_generation/4, delete_key_packages_by_account_tx/3]).

%% ===================================================================
%% Tablename
%% ===================================================================

-spec key_package_tablename() -> binary().
key_package_tablename() ->
    elib_pg_sql:public_tablename(<<"personal_history_key_package">>).

%% ===================================================================
%% Key package（279 密钥包）
%% ===================================================================

%% @doc 读指定版本密钥包（upload 的 422 key_package_missing 校验源）。
-spec find_key_package(integer(), binary(), integer()) -> {ok, map() | undefined} | {error, term()}.
find_key_package(AccountId, Domain, KeyVersion) ->
    Sql =
        <<"SELECT key_version, generation, wrapped_key, kdf_meta, integrity_hash,",
            " retired_at, created_at FROM ", (key_package_tablename())/binary,
            " WHERE account_id = $1 AND domain = $2 AND key_version = $3">>,
    single_or_undefined(Sql, [AccountId, Domain, KeyVersion]).

%% @doc 事务内读指定版本密钥包（upload W1 事务同快照校验）。
-spec find_key_package_tx(pid(), integer(), binary(), integer()) ->
    {ok, map() | undefined} | {error, term()}.
find_key_package_tx(Conn, AccountId, Domain, KeyVersion) ->
    Sql =
        <<"SELECT key_version, generation, retired_at FROM ", (key_package_tablename())/binary,
            " WHERE account_id = $1 AND domain = $2 AND key_version = $3">>,
    single_or_undefined_tx(Conn, Sql, [AccountId, Domain, KeyVersion]).

%% @doc 按当前代次读密钥包（mh04-key-package-read-addendum：查询必须同时
%% 含 account_id/domain/generation/key_version，不能以不含 generation 的
%% uq_phk_version 唯一索引代替当前代次条件）。retired_at 不否定可读性。
-spec find_key_package_for_generation(integer(), binary(), integer(), integer()) ->
    {ok, map() | undefined} | {error, term()}.
find_key_package_for_generation(AccountId, Domain, KeyVersion, Generation) ->
    Sql =
        <<"SELECT key_version, generation, wrapped_key, kdf_meta, integrity_hash,",
            " retired_at, created_at FROM ", (key_package_tablename())/binary,
            " WHERE account_id = $1 AND domain = $2 AND key_version = $3 AND generation = $4">>,
    single_or_undefined(Sql, [AccountId, Domain, KeyVersion, Generation]).

%% 单行判定共享：0 行 = undefined（无行合法），错误透传。
%% （tx 版子句序反转区分 elvis DYT 同构——三支模式两两不相交，序不影响语义。）
single_or_undefined(Sql, Params) ->
    case elib_pg:query(Sql, Params) of
        {ok, [Row]} -> {ok, Row};
        {ok, []} -> {ok, undefined};
        {error, Reason} -> {error, Reason}
    end.

single_or_undefined_tx(Conn, Sql, Params) ->
    case elib_pg:query(Conn, Sql, Params) of
        {error, Reason} -> {error, Reason};
        {ok, []} -> {ok, undefined};
        {ok, [Row]} -> {ok, Row}
    end.

%% @doc 插入密钥包；uq_phk_version 冲突 → {error, unique_violation}（409）。
-spec insert_key_package(pid(), map()) -> {ok, map()} | {error, unique_violation} | {error, term()}.
insert_key_package(Conn, Row) ->
    Sql =
        <<"INSERT INTO ", (key_package_tablename())/binary,
            " (account_id, domain, key_version, generation, wrapped_key, kdf_meta, integrity_hash)"
            " VALUES ($1,$2,$3,$4,$5,$6::jsonb,$7)"
            " RETURNING key_version, created_at">>,
    Params = [
        maps:get(account_id, Row),
        maps:get(domain, Row),
        maps:get(key_version, Row),
        maps:get(generation, Row),
        maps:get(wrapped_key, Row),
        jsone:encode(maps:get(kdf_meta, Row)),
        maps:get(integrity_hash, Row)
    ],
    case elib_pg:query(Conn, Sql, Params) of
        {ok, [Inserted]} -> {ok, Inserted};
        {error, {error, _, <<"23505">>, unique_violation, _, _}} -> {error, unique_violation};
        {error, Reason} -> {error, Reason}
    end.

%% @doc purge：事务内销毁本用途密钥封装（只毁本域材料，§4 轮换边界）。
-spec delete_key_packages_by_account_tx(pid(), integer(), binary()) ->
    {ok, non_neg_integer()} | {error, term()}.
delete_key_packages_by_account_tx(Conn, AccountId, Domain) ->
    Sql =
        <<"DELETE FROM ", (key_package_tablename())/binary,
            " WHERE account_id = $1 AND domain = $2">>,
    elib_pg:execute_count(Conn, Sql, [AccountId, Domain]).
