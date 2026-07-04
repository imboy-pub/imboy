-module(sso_config_repo).

%%%
% SSO 外部认证配置数据仓库模块
% SSO external authentication config repository module
%%%

-export([tablename/0]).
-export([select_all/0]).
-export([upsert/3]).

-include_lib("kernel/include/logger.hrl").
-include("common.hrl").
-include("log.hrl").

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"sso_config">>).

%% @doc 读取全部 provider 配置行（provider, enabled, config[jsonb binary]）
-spec select_all() -> {ok, [map()]} | {error, term()}.
select_all() ->
    Tb = tablename(),
    Sql = <<"SELECT provider, enabled, config FROM ", Tb/binary>>,
    elib_pg:query(Sql, []).

%% @doc 按 provider upsert（ON CONFLICT(provider) DO UPDATE）
%%      ConfigJson 为已编码的 JSON 二进制；SQL 全参数化。
-spec upsert(binary(), boolean(), binary()) -> {ok, [map()]} | {error, term()}.
upsert(Provider, Enabled, ConfigJson) ->
    Tb = tablename(),
    Id = elib_tsid:generate(),
    Sql =
        <<"INSERT INTO ", Tb/binary,
            " (id, provider, enabled, config, created_at, updated_at)"
            " VALUES ($1, $2, $3, $4::jsonb, NOW(), NOW())"
            " ON CONFLICT (provider) DO UPDATE SET"
            " enabled = EXCLUDED.enabled,"
            " config = EXCLUDED.config,"
            " updated_at = NOW()"
            " RETURNING id">>,
    case elib_pg:query(Sql, [Id, Provider, Enabled, ConfigJson]) of
        {ok, Rows} ->
            {ok, Rows};
        {error, Reason} ->
            ?LOG_ERROR("sso_config_repo:upsert error ~p", [Reason]),
            {error, Reason}
    end.
