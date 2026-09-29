#!/usr/bin/env escript
%%! -pa ebin
%% tsid_catalog_check.escript — CI gate: TSID catalog <-> database check.
%%
%% Thin CLI shell around the single authoritative implementation
%% src/lib/elib_tsid_scan.erl (check_schema/2). Verifies that every
%% elib_tsid_catalog:primary_keys() entry exists in the target schema as a
%% bigint column (schema_drift otherwise), and reverse-discovers
%% single-column bigint primary-key tables missing from the catalog
%% (unclassified_primary_keys). Catalog entries whose actual primary key is
%% not the declared single column fail as catalog_mismatch. Never connect
%% to production: point this at a scratch/clone database only.
%%
%% Usage:
%%   escript tsid_catalog_check.escript
%%   escript tsid_catalog_check.escript <dsn>
%%   escript tsid_catalog_check.escript -- host port user password database
%%
%%   <dsn>  postgres://user:password@host:port/database
%%   Resolution order: CLI argument > IMBOY_TSID_CATALOG_CHECK_DSN env var
%%   > default elib_pg pool path (only sane when the app env already points
%%   the pool at the scratch/clone database, same as
%%   tsid_bootstrap_floor.escript). The '--' positional form matches
%%   scripts/tsid/tsid_scanner.escript ('-' as password means empty).
%%
%% Beams must be built first: run `make compile` in the repository root
%% (repo-root ebin/ + deps/epgsql/ebin; both cwd=repo-root and
%% cwd=scripts/tsid invocation styles work).
%%
%% Exit codes:
%%   0  check passed: TSID-CATALOG-CHECK-OK version=<v> digest=<64 hex lc>
%%      tables=<n>
%%   2  check failed: schema_drift / unclassified_primary_keys /
%%      catalog_mismatch / invalid catalog (structured reason printed);
%%      also used for usage errors
%%   3  connection / transaction failure (scratch DB unreachable etc.)
%%   4  required beams missing (run make compile first)
-module(tsid_catalog_check_main).
-export([main/1]).

-define(ENV_DSN, "IMBOY_TSID_CATALOG_CHECK_DSN").

main(Args) ->
    ensure_paths(),
    case missing_beams() of
        [] ->
            case conn_opts(Args) of
                {ok, Opts} -> run(Opts);
                usage -> print_usage(), halt(2)
            end;
        Missing ->
            io:format(
              "TSID-CATALOG-CHECK-BEAMS-MISSING: ~p~n"
              "hint: run 'make compile' in the repository root first~n",
              [Missing]),
            halt(4)
    end.

run(Opts) ->
    Catalog = elib_tsid_catalog:primary_keys(),
    try elib_tsid_scan:check_schema(Catalog, Opts) of
        ok ->
            Digest = binary:encode_hex(elib_tsid_catalog:digest(), lowercase),
            io:format(
              "TSID-CATALOG-CHECK-OK version=~p digest=~s tables=~p~n",
              [elib_tsid_catalog:version(), Digest, length(Catalog)]),
            halt(0);
        {error, Reason} ->
            report(Reason)
    catch
        %% conn_fun tags its own connect failures; anything else escaping
        %% the check is treated as infrastructure failure (exit 3), never
        %% as catalog drift.
        throw:{connection_failed, R} -> conn_failed(R);
        _:R -> conn_failed({unexpected, R})
    end.

%% Catalog-contract failures exit 2; everything else (rollback, schema
%% read failure, pooler errors, ...) is connection/infra and exits 3.
report(Reason) ->
    case check_failure(Reason) of
        true ->
            io:format("TSID-CATALOG-CHECK-FAILED reason=~tp~n", [Reason]),
            halt(2);
        false ->
            conn_failed(Reason)
    end.

check_failure({schema_drift, _}) -> true;
check_failure({unclassified_primary_keys, _}) -> true;
check_failure({catalog_mismatch, _}) -> true;
check_failure({invalid_identifier, _}) -> true;
check_failure({invalid_catalog, _}) -> true;
check_failure({bad_schema_map, _}) -> true;
check_failure(_) -> false.

conn_failed(Reason) ->
    io:format("TSID-CATALOG-CHECK-CONNECTION-FAILED reason=~tp~n", [Reason]),
    halt(3).

%%% ------------------------------------------------------------------
%%% Connection source resolution (CLI arg > env > default pool)
%%% ------------------------------------------------------------------

conn_opts([]) ->
    case os:getenv(?ENV_DSN) of
        Dsn when is_list(Dsn), Dsn =/= "" ->
            case parse_dsn(Dsn) of
                {ok, ConnMap} -> {ok, #{conn_fun => conn_fun(ConnMap)}};
                {error, R} ->
                    io:format("invalid " ?ENV_DSN ": ~tp~n", [R]),
                    usage
            end;
        _ ->
            io:put_chars(
              standard_error,
              "note: no DSN given; using the default elib_pg pool path "
              "(app env must point at the scratch DB)~n"),
            {ok, #{}}
    end;
conn_opts([Dsn]) when Dsn =/= "--" ->
    case parse_dsn(Dsn) of
        {ok, ConnMap} -> {ok, #{conn_fun => conn_fun(ConnMap)}};
        {error, R} ->
            io:format("invalid DSN ~ts: ~tp~n", [Dsn, R]),
            usage
    end;
conn_opts(["--", Host, Port, User, Password, Database]) ->
    try list_to_integer(Port) of
        P ->
            {ok, #{conn_fun =>
                       conn_fun(#{
                           host => Host,
                           port => P,
                           username => User,
                           password =>
                               case Password of
                                   "-" -> "";
                                   _ -> Password
                               end,
                           database => Database})}}
    catch
        _:_ ->
            io:format("invalid port: ~ts~n", [Port]),
            usage
    end;
conn_opts(_) ->
    usage.

%% Direct-connection transport over an epgsql read-only snapshot — same
%% semantics as elib_tsid_scan's default path (epgsql:with_transaction
%% builds "BEGIN " ++ begin_opts itself, so begin_opts must NOT repeat the
%% BEGIN keyword). No scanning SQL lives here; check semantics are wholly
%% in elib_tsid_scan.
conn_fun(ConnMap) ->
    fun(F) ->
        case epgsql:connect(ConnMap#{timeout => 10000}) of
            {ok, Pid} ->
                try
                    epgsql:with_transaction(Pid, F, [
                        {reraise, true},
                        {begin_opts, <<"ISOLATION LEVEL REPEATABLE READ READ ONLY">>}
                    ])
                after
                    epgsql:close(Pid)
                end;
            {error, Reason} ->
                throw({connection_failed, Reason})
        end
    end.

print_usage() ->
    io:format(
      "usage: tsid_catalog_check.escript [dsn | -- host port user password database]~n"
      "  dsn: postgres://user:password@host:port/database~n"
      "       (falls back to " ?ENV_DSN "; with neither, the default~n"
      "       elib_pg pool path is used)~n"
      "  '--' form matches tsid_scanner.escript ('-' password = empty)~n"
      "exit: 0 ok | 2 check failed / usage | 3 connection failed | 4 beams missing~n").

%%% ------------------------------------------------------------------
%%% DSN parsing: postgres://user:password@host:port/database
%%% ------------------------------------------------------------------

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
                        database => Db
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
    try uri_string:percent_decode(S) catch _:_ -> S end.

%%% ------------------------------------------------------------------
%%% Beam discovery (repo-root ebin + deps/epgsql/ebin; make compile)
%%% ------------------------------------------------------------------

missing_beams() ->
    [M || M <- [elib_tsid_scan, elib_tsid_catalog, elib_pg, epgsql],
          not beam_present(M)].

beam_present(M) ->
    case code:which(M) of
        Path when is_list(Path) -> filelib:is_file(Path);
        _ -> false
    end.

%% Beams live in repo-root ebin (make compile). Resolve relative to the
%% caller's working directory and to this script's own location so both
%% invocation styles (cwd=scripts/tsid, repo-root cwd) keep working.
ensure_paths() ->
    add_path("ebin"),
    add_path("deps/epgsql/ebin"),
    ScriptDir = filename:dirname(escript:script_name()),
    add_path(filename:join(ScriptDir, "../../ebin")),
    add_path(filename:join(ScriptDir, "../../deps/epgsql/ebin")),
    ok.

add_path(Path) ->
    case filelib:is_dir(Path) of
        true -> code:add_pathz(Path);
        false -> ok
    end.
