%%% elib_tsid_catalog_db_tests — TSID catalog <-> scratch-DB closed loop
%%%
%%% Gated suite: runs against a throwaway PostgreSQL database ONLY when the
%%% environment variable IMBOY_TSID_CATALOG_CHECK_DSN is set and non-empty
%%% (e.g. postgres://user:password@host:5432/scratch). When unset the whole
%%% suite conditional-skips (generator mode, repo convention). Connection or
%%% setup failures print the reason and skip — they are never test failures.
%%%
%%% !!! DESTRUCTIVE !!! The DSN MUST point at a dedicated throwaway database:
%%% the fixture DROP SCHEMA ... CASCADE + recreates the scratch schema, and
%%% the scratch schema IS "public" (migration DDL hardcodes public.-qualified
%%% identifiers, so a differently-named schema would silently receive none of
%%% the migrated objects). Never point IMBOY_TSID_CATALOG_CHECK_DSN at a
%%% business database.
%%%
%%% Fixture base: instead of an empty hand-made schema, the scratch database
%%% is brought up by replaying ALL of priv/migrations/*.up.sql in numeric
%%% filename order (statement-split with dollar-quote / string-literal /
%%% line-comment awareness). The comparison therefore runs against the REAL
%%% migrated schema, exactly as a fresh deployment would build it.
%%%   - CREATE EXTENSION failures are tolerated with a warning (the scratch
%%%     DB may lack postgis / timescaledb / pg_jieba / pgcrypto); today's
%%%     migrations contain no CREATE EXTENSION of their own, so in practice
%%%     the scratch DB must have the extensions pre-installed.
%%%   - any other statement failure is collected; after the replay finishes
%%%     the suite skips with the collected reasons rather than comparing a
%%%     catalog against a half-built schema (no false green).
%%%
%%% What is verified (contract of src/lib/elib_tsid_scan.erl):
%%%   1. check_schema(elib_tsid_catalog:primary_keys(), <scratch conn>) -> ok
%%%      against the fully-migrated schema: every catalog table exists with
%%%      its declared primary-key column, the column is bigint, and the
%%%      reverse discovery finds no single-column bigint primary key that
%%%      the catalog fails to classify. A red here is a real audit finding
%%%      (catalog drift), not a fixture artifact.
%%%   2. Reverse discovery: a scratch table with a single-column bigint
%%%      primary key that is absent from an injected (minimal, empty)
%%%      catalog must yield {error, {unclassified_primary_keys, L}} with the
%%%      table binary in L.
%%%   3. A composite-primary-key table injected INTO the catalog must yield
%%%      {error, {catalog_mismatch, #{composite_pk := _}}}.
%%%   4. elib_tsid_catalog:digest() is stable across calls and 32 bytes.
%%%
%%% Isolation: all DDL happens inside the scratch schema (public) of the
%%% throwaway database, selected via SET search_path on the test connection,
%%% so the scan's default_schema_fun (information_schema ...
%%% current_schema()) only ever observes test tables. The schema is dropped
%%% (CASCADE) on setup and teardown; per-test tables are additionally
%%% dropped in an after block.
-module(elib_tsid_catalog_db_tests).

-include_lib("eunit/include/eunit.hrl").

%% Exported only for the sibling pure-unit suite
%% elib_tsid_catalog_db_plumb_tests (ordering / statement-splitting /
%% tolerance plumbing, meck-mocked epgsql — never a real database).
-export([
    connect_scratch/0,
    migration_files/1,
    apply_migrations/2,
    split_sql/1,
    is_create_extension/1
]).

-define(ENV_DSN, "IMBOY_TSID_CATALOG_CHECK_DSN").
%% The scratch schema must be public: migration DDL hardcodes
%% public.-qualified identifiers (2000+ references), so any other schema
%% name would leave the migrated objects invisible to the comparison.
-define(SCRATCH_SCHEMA, "public").
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
            %% generator 不能直接返回裸 {skip, _}（非合法 eunit fixture 表示，
            %% eunit_data 会把整个元组当错误打出 **{skip,...} 并 Error 2）；
            %% 包成测试函数体返回 {skip, Reason} 才是 eunit 一等语义。
            fun() -> {skip, Reason} end
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
    case parse_dsn(dsn_env()) of
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

%% os:getenv/1 returns the atom false when the variable is unset; normalize
%% to undefined at the read site so parse_dsn/1 stays a pure string parser
%% that never has to know getenv's sentinel value (an unnormalized false
%% used to crash the whole suite with function_clause).
dsn_env() ->
    case os:getenv(?ENV_DSN) of
        false -> undefined;
        Value -> Value
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

%% Fresh isolated schema on every run, then replay ALL migrations so the
%% comparison runs against the real migrated schema (see module header for
%% why the scratch schema must be public and why the DSN must point at a
%% throwaway database). search_path makes current_schema() resolve to the
%% scratch schema for the scan's information_schema reads.
scratch_ready(Conn) ->
    ok = ddl(Conn, "DROP SCHEMA IF EXISTS " ?SCRATCH_SCHEMA " CASCADE"),
    ok = ddl(Conn, "CREATE SCHEMA " ?SCRATCH_SCHEMA),
    ok = ddl(Conn, "SET search_path = " ?SCRATCH_SCHEMA),
    run_migrations(Conn).

run_migrations(Conn) ->
    case migration_files() of
        {ok, Files} ->
            apply_migrations(Conn, Files);
        {error, Reason} ->
            {error, {migrations_unavailable, Reason}}
    end.

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

%%% ==================================================================
%%% Migration plumbing: ordered discovery of priv/migrations/*.up.sql
%%% (fixture plumbing — exported for elib_tsid_catalog_db_plumb_tests)
%%% ==================================================================

migration_files() ->
    Dir = migrations_dir(),
    case filelib:is_dir(Dir) of
        true -> migration_files(Dir);
        false -> {error, {no_migrations_dir, Dir}}
    end.

%% All *.up.sql under Dir, sorted by the numeric filename prefix
%% (00000001_foundation.up.sql -> 1). A prefix-less name or a duplicated
%% prefix is an error, never a silent skip — the comparison must not run
%% against a partially-replayed migration set.
-spec migration_files(file:filename_all()) ->
    {ok, [{non_neg_integer(), file:filename_all()}]} | {error, term()}.
migration_files(Dir) ->
    Wildcard = filename:join(Dir, "*.up.sql"),
    case parse_migration_names(filelib:wildcard(Wildcard)) of
        {ok, Numbered} ->
            ensure_distinct_prefixes(lists:sort(Numbered));
        {error, _} = Error ->
            Error
    end.

parse_migration_names(Files) ->
    parse_migration_names(Files, []).

parse_migration_names([], Acc) ->
    {ok, lists:reverse(Acc)};
parse_migration_names([Path | Rest], Acc) ->
    case migration_prefix(Path) of
        {ok, N} ->
            parse_migration_names(Rest, [{N, Path} | Acc]);
        {error, _} = Error ->
            Error
    end.

migration_prefix(Path) ->
    Base = filename:basename(Path, ".up.sql"),
    case re:run(Base, <<"^[0-9]+">>, [{capture, first, binary}]) of
        {match, [Digits]} ->
            {ok, binary_to_integer(Digits)};
        nomatch ->
            {error, {bad_migration_name, Path}}
    end.

ensure_distinct_prefixes(Sorted) ->
    Prefixes = [N || {N, _} <- Sorted],
    case length(Prefixes) =:= length(lists:usort(Prefixes)) of
        true -> {ok, Sorted};
        false -> {error, duplicate_migration_prefix}
    end.

%% App root via this module's beam location: the eunit layout keeps beams
%% under <app>/.eunit, so the beam's grandparent directory is the app root
%% (same convention as feature_composition_compat_tests).
app_root() ->
    Ebin = code:which(?MODULE),
    filename:dirname(filename:dirname(Ebin)).

migrations_dir() ->
    filename:join(app_root(), "priv/migrations").

%%% ==================================================================
%%% Migration plumbing: statement splitting
%%% (dollar-quote / string-literal / line-comment aware)
%%% ==================================================================

%% Split SQL text into top-level statements on ';'. Only TOP-LEVEL
%% semicolons split: a ';' inside a dollar-quoted body ($tag$ ... $tag$,
%% including $$ ... $$), inside a single-quoted literal ('' is the escape),
%% or inside a -- line comment is literal text. Line comments are dropped
%% from the emitted statements; whitespace-only results are filtered out.
%% epgsql:squery runs one statement per call, so multi-statement batches
%% must be split here — a naive binary:split on <<";">> would shred every
%% plpgsql function body in priv/migrations.
-spec split_sql(binary()) -> [binary()].
split_sql(Sql) when is_binary(Sql) ->
    split_top(Sql, [], []).

%% Cur — reversed iodata segments of the statement being accumulated;
%% Out — reversed trimmed statements already cut.
split_top(Bin, Cur, Out) ->
    case binary:match(Bin, [<<";">>, <<"'">>, <<"$">>, <<"--">>]) of
        nomatch ->
            emit_statement(Bin, Cur, Out);
        {Pos, _Len} ->
            Before = binary:part(Bin, 0, Pos),
            Rest = binary:part(Bin, Pos, byte_size(Bin) - Pos),
            case binary:at(Bin, Pos) of
                $; ->
                    cut_statement(Before, Rest, Cur, Out);
                $' ->
                    {Literal, After} = take_quoted(binary:part(Rest, 1, byte_size(Rest) - 1)),
                    split_top(After, [Literal, Before | Cur], Out);
                $$ ->
                    split_at_dollar(Before, Rest, Cur, Out);
                _ ->
                    %% Len = 2: the matched pattern is the "--" comment.
                    split_top(skip_line_comment(Rest), [Before | Cur], Out)
            end
    end.

cut_statement(Before, Rest, Cur, Out) ->
    Out2 =
        case trim_statement([Before | Cur]) of
            <<>> -> Out;
            Stmt -> [Stmt | Out]
        end,
    split_top(binary:part(Rest, 1, byte_size(Rest) - 1), [], Out2).

emit_statement(Bin, Cur, Out) ->
    case trim_statement([Bin | Cur]) of
        <<>> -> lists:reverse(Out);
        Stmt -> lists:reverse([Stmt | Out])
    end.

split_at_dollar(Before, Rest, Cur, Out) ->
    case take_dollar_quoted(Rest) of
        {ok, Body, After} ->
            split_top(After, [Body, Before | Cur], Out);
        not_dollar_quote ->
            %% $1-style placeholder (ordinary text; only appears inside
            %% literals / function bodies in this repo, but stay defensive).
            Tail = binary:part(Rest, 1, byte_size(Rest) - 1),
            split_top(Tail, [<<"$">>, Before | Cur], Out)
    end.

%% Bin is the text just after the opening quote. Returns the full literal
%% INCLUDING both quotes plus the remainder after the closing quote; a
%% '' inside is an escaped quote; an unterminated literal swallows the
%% rest so splitting never crashes (PostgreSQL rejects the statement).
take_quoted(Bin) ->
    take_quoted(Bin, []).

take_quoted(Bin, Prefix) ->
    case binary:match(Bin, <<"'">>) of
        nomatch ->
            {iolist_to_binary([$' | lists:reverse(Prefix, [Bin])]), <<>>};
        {Pos, _} ->
            Next = Pos + 1,
            case Next < byte_size(Bin) andalso binary:at(Bin, Next) =:= $' of
                true ->
                    Seg = binary:part(Bin, 0, Next + 1),
                    Tail = binary:part(Bin, Next + 1, byte_size(Bin) - Next - 1),
                    take_quoted(Tail, [Seg | Prefix]);
                false ->
                    Seg = binary:part(Bin, 0, Next),
                    Tail = binary:part(Bin, Next, byte_size(Bin) - Next),
                    {iolist_to_binary([$' | lists:reverse(Prefix, [Seg])]), Tail}
            end
    end.

%% Bin starts with '$'; a dollar quote opens only for $tag$ (tag =
%% [A-Za-z_0-9]*, the empty tag is $$). Returns the whole quoted body
%% (delimiters included) and the remainder; an unterminated body swallows
%% the rest so splitting never crashes.
take_dollar_quoted(Bin) ->
    case re:run(Bin, <<"^\\$[A-Za-z_0-9]*\\$">>, [{capture, first, index}]) of
        {match, [{0, TagLen}]} ->
            Tag = binary:part(Bin, 0, TagLen),
            Scope = {TagLen, byte_size(Bin) - TagLen},
            case binary:match(Bin, Tag, [{scope, Scope}]) of
                {End, _} ->
                    Stop = End + TagLen,
                    {ok, binary:part(Bin, 0, Stop), binary:part(Bin, Stop, byte_size(Bin) - Stop)};
                nomatch ->
                    {ok, Bin, <<>>}
            end;
        nomatch ->
            not_dollar_quote
    end.

%% Drop the comment up to (not including) the newline — statements keep
%% their line structure; a comment without a trailing newline runs to EOF.
skip_line_comment(Bin) ->
    case binary:match(Bin, <<"\n">>) of
        {Pos, _} -> binary:part(Bin, Pos, byte_size(Bin) - Pos);
        nomatch -> <<>>
    end.

trim_statement(Segments) ->
    string:trim(iolist_to_binary(lists:reverse(Segments))).

%%% ==================================================================
%%% Migration plumbing: statement execution + failure semantics
%%% ==================================================================

%% Replay every migration file in order. Failure semantics:
%%   - CREATE EXTENSION statements: failure is tolerated with a printed
%%     warning and execution continues (the scratch DB may lack
%%     postgis / timescaledb / pg_jieba / pgcrypto);
%%   - everything else: failures are collected (execution continues so the
%%     report is complete) and returned as {error, Failures} at the end —
%%     the caller must skip the suite instead of comparing a catalog
%%     against a half-built schema.
-spec apply_migrations(term(), [{non_neg_integer(), file:filename_all()}]) ->
    ok | {error, [{binary(), non_neg_integer(), binary(), term()}]}.
apply_migrations(Conn, Files) ->
    {_Applied, Failures} =
        lists:foldl(
            fun({_N, Path}, Acc) -> apply_one_migration(Conn, Path, Acc) end,
            {0, []},
            Files
        ),
    case Failures of
        [] -> ok;
        _ -> {error, lists:reverse(Failures)}
    end.

apply_one_migration(Conn, Path, {Applied, Failures}) ->
    File = unicode:characters_to_binary(filename:basename(Path)),
    case file:read_file(Path) of
        {ok, Bin} ->
            exec_statements(Conn, File, split_sql(Bin), {Applied, Failures});
        {error, Reason} ->
            {Applied, [{File, 0, <<"read_failed">>, Reason} | Failures]}
    end.

exec_statements(Conn, File, Statements, {Applied, Failures}) ->
    Indexed = lists:zip(lists:seq(1, length(Statements)), Statements),
    lists:foldl(
        fun({Index, Stmt}, Acc) -> exec_statement(Conn, File, Index, Stmt, Acc) end,
        {Applied, Failures},
        Indexed
    ).

exec_statement(Conn, File, Index, Stmt, {Applied, Failures}) ->
    case squery_ok(Conn, Stmt) of
        ok ->
            {Applied + 1, Failures};
        {error, Reason} ->
            case is_create_extension(Stmt) of
                true ->
                    warn_extension_failure(File, Index, Reason),
                    {Applied + 1, Failures};
                false ->
                    {Applied, [{File, Index, stmt_head(Stmt), Reason} | Failures]}
            end
    end.

is_create_extension(Stmt) ->
    re:run(
        Stmt,
        <<"^[[:space:]]*CREATE[[:space:]]+EXTENSION">>,
        [caseless, {capture, none}]
    ) =/= nomatch.

squery_ok(Conn, Sql) ->
    case epgsql:squery(Conn, Sql) of
        {ok, _} -> ok;
        {ok, _, _} -> ok;
        {ok, _, _, _} -> ok;
        [{ok, _} | _] -> ok;
        {error, Reason} -> {error, Reason};
        Other -> {error, {unexpected_squery_result, Other}}
    end.

warn_extension_failure(File, Index, Reason) ->
    io:format(
        "[tsid-catalog-db] WARNING: tolerated CREATE EXTENSION failure in ~s stmt #~b: ~tp~n",
        [File, Index, Reason]
    ).

stmt_head(Stmt) ->
    binary:part(Stmt, 0, min(byte_size(Stmt), 160)).
