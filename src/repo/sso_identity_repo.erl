-module(sso_identity_repo).

%%%
% SSO 身份映射数据仓库模块
% SSO identity mapping repository: (provider, subject) -> uid
%%%

-export([tablename/0]).
-export([find_by_subject/2]).
-export([upsert/4]).

-include_lib("kernel/include/logger.hrl").
-include("common.hrl").
-include("log.hrl").

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"sso_identity">>).

%% @doc 按 (provider, subject) 查身份映射行
-spec find_by_subject(binary(), binary()) -> {ok, [map()]} | {error, term()}.
find_by_subject(Provider, Subject) ->
    Tb = tablename(),
    Sql =
        <<"SELECT id, provider, subject, uid, email FROM ", Tb/binary,
            " WHERE provider = $1 AND subject = $2 LIMIT 1">>,
    elib_pg:query(Sql, [Provider, Subject]).

%% @doc upsert 身份映射（ON CONFLICT(provider, subject) DO UPDATE）
-spec upsert(binary(), binary(), integer(), binary()) -> {ok, [map()]} | {error, term()}.
upsert(Provider, Subject, Uid, Email) ->
    Tb = tablename(),
    Id = elib_tsid:generate(sso_identity),
    Sql =
        <<"INSERT INTO ", Tb/binary,
            " (id, provider, subject, uid, email, created_at, updated_at)"
            " VALUES ($1, $2, $3, $4, $5, NOW(), NOW())"
            " ON CONFLICT (provider, subject) DO UPDATE SET"
            " uid = EXCLUDED.uid,"
            " email = EXCLUDED.email,"
            " updated_at = NOW()"
            " RETURNING id">>,
    case elib_pg:query(Sql, [Id, Provider, Subject, Uid, Email]) of
        {ok, Rows} ->
            {ok, Rows};
        {error, Reason} ->
            ?LOG_ERROR("sso_identity_repo:upsert error ~p", [Reason]),
            {error, Reason}
    end.
