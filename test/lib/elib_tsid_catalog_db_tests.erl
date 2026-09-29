%%% elib_tsid_catalog_db_tests — TSID catalog <-> scratch-DB closed loop
%%%
%%% Gated suite: runs against a throwaway PostgreSQL database ONLY when the
%%% environment variable IMBOY_TSID_CATALOG_CHECK_DSN is set and non-empty
%%% (e.g. postgres://user:password@host:5432/scratch). When unset the whole
%%% suite conditional-skips (generator mode, repo convention). Connection or
%%% setup failures print the reason and skip — they are never test failures.
%%%
%%% What is verified (contract of src/lib/elib_tsid_scan.erl):
%%%   1. check_schema(elib_tsid_catalog:primary_keys(), <scratch conn>) -> ok
%%%      With the current empty v1 catalog this validates the plumbing on an
%%%      isolated empty schema; once catalog entries appear, the scratch DB
%%%      pointed at by the DSN must contain those tables (migrated schema).
%%%   2. Reverse discovery: a scratch table with a single-column bigint
%%%      primary key that is absent from an injected (minimal, empty)
%%%      catalog must yield {error, {unclassified_primary_keys, L}} with the
%%%      table binary in L.
%%%   3. A composite-primary-key table injected INTO the catalog must yield
%%%      {error, {catalog_mismatch, #{composite_pk := _}}}.
%%%   4. elib_tsid_catalog:digest() is stable across calls and 32 bytes.
%%%
%%% Isolation: all DDL happens inside a dedicated schema (tsid_chk_scratch)
%%% selected via SET search_path on the test connection, so the scan's
%%% default_schema_fun (information_schema ... current_schema()) only ever
%%% observes test tables. The schema is dropped (CASCADE) on setup and
%%% teardown; per-test tables are additionally dropped in an after block.
-module(elib_tsid_catalog_db_tests).

-include_lib("eunit/include/eunit.hrl").

-define(ENV_DSN, "IMBOY_TSID_CATALOG_CHECK_DSN").
-define(SCRATCH_SCHEMA, "tsid_chk_scratch").
-define(T_UNCLASSIFIED, "tsid_chk_unclassified").
-define(T_COMPOSITE, "tsid_chk_composite").

catalog_db_test_() ->
    case connect_scratch() of
        {ok, Conn} ->
            {setup, fun() -> Conn end, fun teardown/1, [
                {"catalog matches scratch schema", fun() -> t_catalog_ok(Conn) end},
                {"unclassified bigint PK is discovered", fun() -> t_unclassified(Conn) end},
                {"composite PK yields catalog_mismatch", fun() -> t_composite(Conn) end},
                {"digest is stable and 32 bytes", fun() -> t_digest() end}
            ]};
        {skip, Reason} ->
            {skip, Reason}
    end.

%%% ------------------------------------------------------------------
%%% Case 1: catalog -> schema direction must hold
%%% ------------------------------------------------------------------

t_catalog_ok(Conn) ->
    Catalog = elib_tsid_catalog:primary_keys(),
    ?assertEqual(ok, elib_tsid_scan:check_schema(Catalog, #{conn_fun => conn_fun(Conn)})).

%%% ------------------------------------------------------------------
%%% Case 2: reverse discovery — single-column bigint PK not in catalog
%%% ------------------------------------------------------------------

t_unclassified(Conn) ->
    ok = ddl(Conn, "CREATE TABLE " ?T_UNCLASSIFIED " (id bigint PRIMARY KEY)"),
    try
        Result = elib_tsid_scan:check_schema([], #{conn_fun => conn_fun(Conn)}),
        ?assertMatch({error, {unclassified_primary_keys, _}}, Result),
        {error, {unclassified_primary_keys, Found}} = Result,
        ?assert(lists:member(list_to_binary(?T_UNCLASSIFIED), Found))
    after
        ok = ddl(Conn, "DROP TABLE IF EXISTS " ?T_UNCLASSIFIED)
    end.

%%% ------------------------------------------------------------------
%%% Case 3: catalog entry whose actual PK is composite -> catalog_mismatch
%%% ------------------------------------------------------------------

t_composite(Conn) ->
    ok = ddl(
        Conn,
        "CREATE TABLE " ?T_COMPOSITE " (id bigint, rid bigint, PRIMARY KEY (id, rid))"
    ),
    try
        Result =
            elib_tsid_scan:check_schema(
                [{tsid_chk_composite, id}], #{conn_fun => conn_fun(Conn)}
            ),
        ?assertMatch({error, {catalog_mismatch, #{composite_pk := _}}}, Result)
    after
        ok = ddl(Conn, "DROP TABLE IF EXISTS " ?T_COMPOSITE)
    end.

%%% ------------------------------------------------------------------
%%% Case 4: digest contract (pure, no DB)
%%% ------------------------------------------------------------------

t_digest() ->
    D1 = elib_tsid_catalog:digest(),
    D2 = elib_tsid_catalog:digest(),
    ?assertEqual(D1, D2),
    ?assertEqual(32, byte_size(D1)).

%%% ==================================================================
%%% Connection / fixture plumbing
%%% ==================================================================

connect_scratch() ->
    case parse_dsn(os:getenv(?ENV_DSN)) of
        {error, not_set} ->
            {skip, ?ENV_DSN " not set; TSID catalog <-> scratch-DB check skipped"};
        {error, Reason} ->
            {skip, lists:flatten(io_lib:format("invalid " ?ENV_DSN ": ~tp", [Reason]))};
        {ok, ConnMap} ->
            try open_scratch(ConnMap) of
                {ok, Conn} ->
                    {ok, Conn};
                {error, R} ->
                    skip_on_connect_failure({scratch_setup_failed, R})
            catch
                _:R ->
                    skip_on_connect_failure({scratch_setup_failed, R})
            end
    end.

skip_on_connect_failure(Reason) ->
    Msg = io_lib:format("scratch DB unavailable (~tp); suite skipped", [Reason]),
    io:format("~ts~n", [Msg]),
    {skip, lists:flatten(Msg)}.

open_scratch(ConnMap) ->
    case epgsql:connect(ConnMap) of
        {ok, Conn} ->
            case scratch_ready(Conn) of
                ok ->
                    {ok, Conn};
                {error, R} ->
                    epgsql:close(Conn),
                    {error, R}
            end;
        {error, R} ->
            {error, {connect_failed, R}}
    end.

%% Fresh isolated schema on every run; search_path makes current_schema()
%% resolve to the scratch schema for the scan's information_schema reads.
scratch_ready(Conn) ->
    ok = ddl(Conn, "DROP SCHEMA IF EXISTS " ?SCRATCH_SCHEMA " CASCADE"),
    ok = ddl(Conn, "CREATE SCHEMA " ?SCRATCH_SCHEMA),
    ok = ddl(Conn, "SET search_path = " ?SCRATCH_SCHEMA),
    ok.

teardown(Conn) ->
    _ = epgsql:squery(Conn, "DROP SCHEMA IF EXISTS " ?SCRATCH_SCHEMA " CASCADE"),
    _ = epgsql:close(Conn),
    ok.

conn_fun(Conn) ->
    fun(F) -> F(Conn) end.

ddl(Conn, Sql) ->
    case epgsql:squery(Conn, Sql) of
        {ok, _, _} -> ok;
        {ok, _} -> ok;
        {error, Reason} -> error({ddl_failed, Sql, Reason})
    end.

%%% ==================================================================
%%% DSN parsing: postgres://user:password@host:port/database
%%% ==================================================================

parse_dsn(undefined) ->
    {error, not_set};
parse_dsn("") ->
    {error, not_set};
parse_dsn(Dsn) when is_list(Dsn) ->
    try uri_string:parse(Dsn) of
        #{scheme := Scheme} = Uri when Scheme =:= "postgres"; Scheme =:= "postgresql" ->
            assemble(Uri);
        #{scheme := Other} ->
            {error, {bad_scheme, Other}};
        _ ->
            {error, {bad_dsn, Dsn}}
    catch
        _:_ ->
            {error, {bad_dsn, Dsn}}
    end.

assemble(Uri) ->
    case maps:get(host, Uri, "") of
        "" ->
            {error, {missing_host, Uri}};
        Host ->
            case string:trim(maps:get(path, Uri, ""), leading, "/") of
                "" ->
                    {error, {missing_database, Uri}};
                Db ->
                    {User, Pass} = userinfo(maps:get(userinfo, Uri, "")),
                    {ok, #{
                        host => Host,
                        port => maps:get(port, Uri, 5432),
                        username => User,
                        password => Pass,
                        database => Db,
                        timeout => 10000
                    }}
            end
    end.

userinfo("") ->
    {"postgres", ""};
userinfo(Encoded) ->
    Plain = percent_decode(Encoded),
    case string:split(Plain, ":", leading) of
        [User] -> {User, ""};
        [User, Pass] -> {User, Pass}
    end.

percent_decode(S) ->
    try
        uri_string:percent_decode(S)
    catch
        _:_ -> S
    end.
