-module(imboy_pg_connection).

-moduledoc "PostgreSQL 连接建立封装 —— epgsql:connect 后统一设置 statement_timeout。".
-export([connect/1]).

-include_lib("epgsql/include/epgsql.hrl").

-define(DEFAULT_STATEMENT_TIMEOUT_MS, 15000).

-spec connect(map()) -> {ok, pid()} | {error, term()}.
connect(Options) ->
    case epgsql:connect(Options) of
        {ok, Conn} ->
            Timeout = statement_timeout_ms(),
            Sql = [<<"SET statement_timeout TO ">>, integer_to_binary(Timeout)],
            case epgsql:squery(Conn, Sql) of
                {ok, _, _} ->
                    warm_search_dictionary(Conn);
                {error, Reason} ->
                    ok = epgsql:close(Conn),
                    {error, {statement_timeout_setup_failed, Reason}}
            end;
        Error ->
            Error
    end.

%% Initialise Jieba in this PostgreSQL backend before a message holds its
%% acceptance locks. The catalog lookup returns no row if Jieba is absent.
warm_search_dictionary(Conn) ->
    Sql =
        <<"SELECT pg_catalog.to_tsvector(c.oid::pg_catalog.regconfig, '消息搜索初始化') "/utf8,
            "FROM pg_catalog.pg_ts_config AS c JOIN pg_catalog.pg_namespace AS n ",
            "ON n.oid = c.cfgnamespace WHERE n.nspname = 'public' AND c.cfgname = 'jiebacfg'">>,
    try epgsql:squery(Conn, Sql) of
        {ok, _, _} ->
            connected(Conn);
        {error, #error{code = <<"08", _/binary>>} = Reason} ->
            warmup_connection_failed(Conn, Reason);
        {error, #error{severity = error}} ->
            %% An optional warm-up can time out under the configured statement
            %% limit. The standalone statement leaves a live session usable.
            connected(Conn);
        {error, Reason} ->
            warmup_connection_failed(Conn, Reason)
    catch
        exit:Reason -> warmup_connection_failed(Conn, Reason)
    end.

connected(Conn) ->
    case is_process_alive(Conn) of
        true -> {ok, Conn};
        false -> {error, {search_warmup_connection_failed, closed}}
    end.

warmup_connection_failed(Conn, Reason) ->
    _ = epgsql:close(Conn),
    {error, {search_warmup_connection_failed, Reason}}.

statement_timeout_ms() ->
    case config_ds:env(statement_timeout_ms, ?DEFAULT_STATEMENT_TIMEOUT_MS) of
        Value when is_integer(Value), Value >= 100, Value =< 300000 -> Value;
        _ -> ?DEFAULT_STATEMENT_TIMEOUT_MS
    end.
