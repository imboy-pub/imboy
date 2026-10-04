-module(sso_identity_repo).

-moduledoc "SSO 身份映射数据仓库模块 / repository。".
%%%
% SSO 身份映射数据仓库模块
% SSO identity mapping repository: (provider, subject) -> uid
%%%

-export([tablename/0]).
-export([find_by_subject/2, find_by_subject_tx/3]).
-export([find_subject_by_uid/2]).
-export([upsert/4, upsert_tx/5]).

-include_lib("kernel/include/logger.hrl").
-include("common.hrl").
-include("log.hrl").

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"sso_identity">>).

%% @doc 按 (provider, subject) 查身份映射行
-spec find_by_subject(binary(), binary()) -> {ok, [map()]} | {error, term()}.
find_by_subject(Provider, Subject) ->
    elib_pg:query(find_subject_sql(), [Provider, Subject]).

%% @doc 事务内查身份映射行（DS 层 with_tx Fun(Conn) 内调用）
-spec find_by_subject_tx(any(), binary(), binary()) -> {ok, [map()]} | {error, term()}.
find_by_subject_tx(Conn, Provider, Subject) ->
    elib_pg:query(Conn, find_subject_sql(), [Provider, Subject]).

%% @doc 按 (provider, uid) 反查 subject（如 wechat_mini 的 openid）
%% 订阅消息下发用：uid → openid 后才能调 subscribeMessage.send。
%% 返回值含 subject（openid，PII）——只允许下发链路内联使用，禁止入日志。
-spec find_subject_by_uid(binary(), integer()) -> {ok, [map()]} | {error, term()}.
find_subject_by_uid(Provider, Uid) ->
    Tb = tablename(),
    elib_pg:query(
        <<"SELECT id, provider, subject, uid, email FROM ", Tb/binary,
            " WHERE provider = $1 AND uid = $2 LIMIT 1">>,
        [Provider, Uid]
    ).

%% @doc upsert 身份映射（ON CONFLICT(provider, subject) DO UPDATE）
-spec upsert(binary(), binary(), integer(), binary()) -> {ok, [map()]} | {error, term()}.
upsert(Provider, Subject, Uid, Email) ->
    {Sql, Params} = upsert_sql(Provider, Subject, Uid, Email),
    log_upsert_result(elib_pg:query(Sql, Params)).

%% @doc 事务内 upsert 身份映射（DS 层 with_tx Fun(Conn) 内调用）
%% 与 upsert/4 的唯一差异是连接来源。把它单独留出来的原因：moya 首登
%% 「建 user 行 + 绑 sso_identity」必须是同一连接上的同一事务，否则中途失败
%% 会留下一个有 user 行却没有身份映射的孤儿账号（重试时因为查不到映射会再建
%% 一个，无限堆积），而 upsert/4 走的是池连接、无法参与调用方的事务。
-spec upsert_tx(any(), binary(), binary(), integer(), binary()) ->
    {ok, [map()]} | {error, term()}.
upsert_tx(Conn, Provider, Subject, Uid, Email) ->
    {Sql, Params} = upsert_sql(Provider, Subject, Uid, Email),
    log_upsert_result(elib_pg:query(Conn, Sql, Params)).

%%%===================================================================
%%% Internal
%%%===================================================================

-spec find_subject_sql() -> binary().
find_subject_sql() ->
    Tb = tablename(),
    <<"SELECT id, provider, subject, uid, email FROM ", Tb/binary,
        " WHERE provider = $1 AND subject = $2 LIMIT 1">>.

%% SQL 只有一份：pool 路径与 tx 路径共用，避免两处实现各自漂移
%% （历史上 upsert 的 ON CONFLICT 列清单被改过一次，双份实现必漏改其一）
-spec upsert_sql(binary(), binary(), integer(), binary()) -> {binary(), [term()]}.
upsert_sql(Provider, Subject, Uid, Email) ->
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
    {Sql, [Id, Provider, Subject, Uid, Email]}.

-spec log_upsert_result({ok, [map()]} | {error, term()}) -> {ok, [map()]} | {error, term()}.
log_upsert_result({ok, Rows}) ->
    {ok, Rows};
log_upsert_result({error, Reason} = Error) ->
    ?LOG_ERROR("sso_identity_repo:upsert error ~p", [Reason]),
    Error.
