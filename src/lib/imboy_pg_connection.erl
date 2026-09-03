-module(imboy_pg_connection).

-export([connect/1]).

-define(DEFAULT_STATEMENT_TIMEOUT_MS, 15000).

-spec connect(map()) -> {ok, pid()} | {error, term()}.
connect(Options) ->
    case epgsql:connect(Options) of
        {ok, Conn} ->
            Timeout = statement_timeout_ms(),
            Sql = [<<"SET statement_timeout TO ">>, integer_to_binary(Timeout)],
            case epgsql:squery(Conn, Sql) of
                {ok, _, _} ->
                    {ok, Conn};
                {error, Reason} ->
                    ok = epgsql:close(Conn),
                    {error, {statement_timeout_setup_failed, Reason}}
            end;
        Error ->
            Error
    end.

statement_timeout_ms() ->
    case config_ds:env(statement_timeout_ms, ?DEFAULT_STATEMENT_TIMEOUT_MS) of
        Value when is_integer(Value), Value >= 100, Value =< 300000 -> Value;
        _ -> ?DEFAULT_STATEMENT_TIMEOUT_MS
    end.
