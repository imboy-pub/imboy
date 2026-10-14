#!/usr/bin/env escript
%%! -noshell
%% W0-03 strict-ledger bootstrap: replay frozen original Up set through the
%% REAL erlang_migrate API (strict=>true, strict_bootstrap=>fail) into a fresh
%% run-owned database, then verify dirty=false and exact history version set.
%% Env: V5_MIG_DIR V5_PG_HOST V5_PG_PORT V5_PG_USER V5_PG_DB V5_PG_PASSWORD
%%      V5_EXPECTED_VERSIONS (comma-separated, optional cross-check)
-module(attv5_strict_ledger).
-export([main/1]).

main(_Args) ->
    Dir = env_bin("V5_MIG_DIR"),
    Host = binary_to_list(env_bin("V5_PG_HOST")), % OTP29 gen_tcp rejects binary hosts
    Port = env_int("V5_PG_PORT"),
    User = env_bin("V5_PG_USER"),
    Db = env_bin("V5_PG_DB"),
    Password = env_bin("V5_PG_PASSWORD"),
    Pa = code:add_patha("deps/erlang_migrate/ebin"),
    Pb = code:add_patha("deps/epgsql/ebin"),
    io:format(standard_error, "code paths: ~p ~p~n", [Pa, Pb]),
    {ok, Conn} = epgsql:connect(#{host => Host, port => Port, username => User,
                                  password => Password, database => Db,
                                  timeout => 15000}),
    try
        %% Fresh ledger: create tables via a first call on an empty dir would
        %% not ensure tables; call up/1 directly — ensure_table runs inside.
        Config = #{conn => Conn, dir => Dir, strict => true,
                   strict_bootstrap => fail},
        case erlang_migrate:up(Config) of
            ok ->
                verify(Conn, Db);
            {error, Reason} ->
                io:format("STRICT_UP_ERROR ~200p~n", [Reason]),
                halt(30)
        end
    after
        epgsql:close(Conn)
    end.

verify(Conn, Db) ->
    {ok, _, [{Cur, Dirty}]} = epgsql:squery(
        Conn, "SELECT version, dirty FROM schema_migrations"),
    {ok, _, Rows} = epgsql:squery(
        Conn, "SELECT version FROM schema_migrations_history ORDER BY version"),
    Versions = [binary_to_integer(V) || {V} <- Rows],
    Expected = case os:getenv("V5_EXPECTED_VERSIONS") of
                   false -> Versions;
                   Raw -> [list_to_integer(string:strip(S))
                           || S <- string:tokens(Raw, ",")]
               end,
    FileCount = length(Versions),
    io:format("current_version=~s dirty=~s history_rows=~p~n",
              [Cur, Dirty, FileCount]),
    case {Dirty, Versions =:= Expected, FileCount > 0} of
        {Df, true, true} when Df =:= <<"f">>; Df =:= <<"false">>; Df =:= false ->
            io:format("STRICT_LEDGER_OK db=~s versions=~p~n", [Db, Versions]),
            halt(0);
        {_D, false, _} ->
            io:format("STRICT_LEDGER_VERSION_MISMATCH expected=~p got=~p~n",
                      [Expected, Versions]),
            halt(20);
        {Df2, _, _} when Df2 =:= <<"f">>; Df2 =:= <<"false">>; Df2 =:= false ->
            io:format("STRICT_LEDGER_EMPTY~n"), halt(30);
        _ ->
            io:format("STRICT_LEDGER_DIRTY~n"), halt(20)
    end.

env_bin(K) ->
    case os:getenv(K) of
        false -> erlang:error({missing_env, K});
        V -> unicode:characters_to_binary(V)
    end.

env_int(K) ->
    binary_to_integer(env_bin(K)).
