%%% elib_tsid_catalog_db_plumb_tests — pure-unit coverage for the
%%% migration plumbing exported by elib_tsid_catalog_db_tests.
%%%
%%% Never contacts a database: inttest_marker_db:safe_connect/1 and
%%% epgsql:squery/2 are meck-mocked for failure/apply-path cases; everything
%%% else is pure text / ordering logic.
%%% Covers:
%%%   - numeric-prefix ordering (non-padded names sort numerically;
%%%     prefix-less and duplicated prefixes are rejected, never skipped);
%%%   - dollar-quote / string-literal / line-comment aware statement
%%%     splitting ($$ bodies, $tag$ bodies, '' escapes, ';' and '--'
%%%     inside literals, comments containing ';');
%%%   - CREATE EXTENSION failure tolerance vs critical failure
%%%     collection (execution continues, replay returns {error, Fs});
%%%   - only an unset DSN yields {skip, _}; invalid DSN and configured-but-
%%%     unavailable scratch DB fail closed.
-module(elib_tsid_catalog_db_plumb_tests).

-include_lib("eunit/include/eunit.hrl").

-define(SUT, elib_tsid_catalog_db_tests).
-define(ENV_DSN, "IMBOY_TSID_CATALOG_CHECK_DSN").

%%% ==================================================================
%%% Ordering: numeric filename prefix
%%% ==================================================================

migration_ordering_test_() ->
    {setup, fun ordering_fixture/0, fun cleanup_dir/1, fun(Dir) ->
        ?_test(begin
            {ok, Files} = ?SUT:migration_files(Dir),
            %% numeric order, NOT lexicographic ("2" before "10")
            ?assertEqual([1, 2, 10], [N || {N, _} <- Files]),
            ?assertEqual(<<"10_late.up.sql">>, last_basename(Files)),
            %% non-up files never enter the replay set
            ?assertEqual(3, length(Files))
        end)
    end}.

ordering_fixture() ->
    Dir = tmp_dir("ordering"),
    _ = write_file(Dir, "10_late.up.sql", <<"SELECT 10;">>),
    _ = write_file(Dir, "2_mid.up.sql", <<"SELECT 2;">>),
    _ = write_file(Dir, "00000001_early.up.sql", <<"SELECT 1;">>),
    %% noise that must be ignored / rejected
    _ = write_file(Dir, "5_noise.down.sql", <<"SELECT 'down';">>),
    _ = write_file(Dir, "README.txt", <<"not a migration">>),
    Dir.

prefixless_name_is_an_error_test_() ->
    {setup, fun prefixless_fixture/0, fun cleanup_dir/1, fun(Dir) ->
        ?_assertMatch({error, {bad_migration_name, _}}, ?SUT:migration_files(Dir))
    end}.

prefixless_fixture() ->
    Dir = tmp_dir("prefixless"),
    _ = write_file(Dir, "foundation.up.sql", <<"SELECT 1;">>),
    Dir.

duplicate_prefix_is_an_error_test_() ->
    {setup, fun duplicate_fixture/0, fun cleanup_dir/1, fun(Dir) ->
        ?_assertMatch({error, duplicate_migration_prefix}, ?SUT:migration_files(Dir))
    end}.

duplicate_fixture() ->
    Dir = tmp_dir("duplicate"),
    _ = write_file(Dir, "1_a.up.sql", <<"SELECT 1;">>),
    _ = write_file(Dir, "3_b.up.sql", <<"SELECT 3;">>),
    _ = write_file(Dir, "3_c.up.sql", <<"SELECT 33;">>),
    Dir.

%%% ==================================================================
%%% Splitting: dollar-quote / literal / comment aware
%%% ==================================================================

split_sql_test_() ->
    [
        ?_assertEqual([<<"SELECT 1">>, <<"SELECT 2">>], ?SUT:split_sql(<<"SELECT 1; SELECT 2;">>)),
        ?_assertEqual([<<"SELECT ';' FROM t">>], ?SUT:split_sql(<<"SELECT ';' FROM t;">>)),
        ?_assertEqual([<<"SELECT 'a'';b'">>], ?SUT:split_sql(<<"SELECT 'a'';b';">>)),
        ?_assertEqual([<<"SELECT 'a--b' AS c">>], ?SUT:split_sql(<<"SELECT 'a--b' AS c;">>)),
        ?_assertEqual([<<"SELECT '$$' || '$x$'">>], ?SUT:split_sql(<<"SELECT '$$' || '$x$';">>)),
        ?_assertEqual([<<"SELECT 1">>], ?SUT:split_sql(<<"-- dropped; comment\nSELECT 1;">>)),
        ?_assertEqual(
            [<<"SELECT 1">>, <<"SELECT 2">>],
            ?SUT:split_sql(<<"--; noise\nSELECT 1;\n-- x;\nSELECT 2;">>)
        ),
        ?_assertEqual([<<"SELECT 42">>], ?SUT:split_sql(<<"  \n SELECT 42 ; \n">>)),
        ?_assertEqual([<<"SELECT 1">>, <<"SELECT 2">>], ?SUT:split_sql(<<"SELECT 1;;SELECT 2;">>)),
        ?_assertEqual([], ?SUT:split_sql(<<"">>)),
        ?_assertEqual([], ?SUT:split_sql(<<"-- only; comments\n-- more;">>))
    ].

split_sql_dollar_quote_test_() ->
    Fn =
        <<
            "CREATE FUNCTION f() RETURNS void LANGUAGE plpgsql AS $$\n"
            "BEGIN\n  PERFORM note('semi; colon');\nEND;\n$$"
        >>,
    Tagged =
        <<"DO $constraints$\nBEGIN\n  PERFORM 1; PERFORM 2;\nEND $constraints$">>,
    [
        ?_assertEqual([Fn], ?SUT:split_sql(<<Fn/binary, ";">>)),
        ?_assertEqual([Fn, <<"SELECT 1">>], ?SUT:split_sql(<<Fn/binary, ";\nSELECT 1;">>)),
        ?_assertEqual([Tagged], ?SUT:split_sql(<<Tagged/binary, ";">>)),
        %% a $$ body followed by more statements keeps them separate
        ?_assertEqual(
            [Tagged, <<"SELECT 'tail'">>],
            ?SUT:split_sql(<<Tagged/binary, ";\nSELECT 'tail';">>)
        ),
        %% $ without a closing $ is ordinary text (e.g. $1 placeholder
        %% outside a literal): the text survives verbatim
        ?_assertEqual(
            [<<"SELECT $1">>],
            ?SUT:split_sql(<<"SELECT $1;">>)
        )
    ].

split_sql_unterminated_test_() ->
    [
        %% unterminated dollar quote / literal must not crash or loop;
        %% the statement stays whole and PostgreSQL rejects it later
        ?_assertEqual(
            [<<"DO $$ BEGIN PERFORM 1">>],
            ?SUT:split_sql(<<"DO $$ BEGIN PERFORM 1">>)
        ),
        ?_assertEqual(
            [<<"SELECT 'open">>],
            ?SUT:split_sql(<<"SELECT 'open">>)
        )
    ].

%%% ==================================================================
%%% Real migrations: ordering + splitting smoke (read-only, no DB)
%%% ==================================================================

real_migrations_smoke_test_() ->
    ?_test(begin
        {ok, Files} = ?SUT:migration_files(migrations_dir()),
        Prefixes = [N || {N, _} <- Files],
        ?assert(length(Files) >= 100),
        %% strictly increasing numeric order, no duplicates, starts at 1
        ?assert(hd(Prefixes) =:= 1),
        ?assertEqual(Prefixes, lists:usort(Prefixes)),
        %% every file splits into at least one statement; first statement
        %% of the foundation migration is a full CREATE FUNCTION whose
        %% $$ body (with inner ';') survived as ONE statement
        {Empty, Total} = split_all(Files),
        ?assertEqual([], Empty),
        ?assert(Total > 1000),
        {ok, First} = file:read_file(filename:join(migrations_dir(), "00000001_foundation.up.sql")),
        [FirstStmt | _] = ?SUT:split_sql(First),
        ?assertEqual(
            <<"CREATE FUNCTION">>,
            binary:part(FirstStmt, 0, byte_size(<<"CREATE FUNCTION">>))
        ),
        %% the function body's inner semicolons did not shred the statement
        ?assert(byte_size(FirstStmt) > 1000)
    end).

split_all(Files) ->
    lists:foldl(
        fun({_N, Path}, {Empty, Total}) ->
            {ok, Bin} = file:read_file(Path),
            case ?SUT:split_sql(Bin) of
                [] -> {[Path | Empty], Total};
                Stmts -> {Empty, Total + length(Stmts)}
            end
        end,
        {[], 0},
        Files
    ).

%%% ==================================================================
%%% Execution: CREATE EXTENSION tolerance vs critical failures (meck)
%%% ==================================================================

apply_tolerates_extension_failures_test_() ->
    {setup,
        fun() ->
            mock_squery(fun(Sql) -> ?SUT:is_create_extension(Sql) end),
            extension_fixture()
        end,
        fun(Dir) ->
            meck:unload(epgsql),
            cleanup_dir(Dir)
        end,
        fun(Dir) ->
            ?_test(begin
                {ok, Files} = ?SUT:migration_files(Dir),
                ?assertEqual(ok, ?SUT:apply_migrations(fake_conn, Files)),
                %% extension statements failed but execution continued:
                %% both CREATE TABLE statements still ran (2 files x 2)
                Sqls = squery_history(),
                ?assertEqual(4, length(Sqls)),
                ?assert(lists:member(<<"CREATE TABLE t_ok (id bigint PRIMARY KEY)">>, Sqls)),
                ?assertEqual(2, length([S || S <- Sqls, ?SUT:is_create_extension(S)]))
            end)
        end}.

extension_fixture() ->
    Dir = tmp_dir("ext"),
    _ = write_file(
        Dir,
        "00000001_a.up.sql",
        <<
            "CREATE EXTENSION IF NOT EXISTS postgis;\n"
            "CREATE TABLE t_ok (id bigint PRIMARY KEY);\n"
        >>
    ),
    _ = write_file(
        Dir,
        "00000002_b.up.sql",
        <<
            "CREATE EXTENSION postgis;\n"
            "CREATE TABLE t_ok (id bigint PRIMARY KEY);\n"
        >>
    ),
    Dir.

apply_collects_critical_failures_test_() ->
    {setup,
        fun() ->
            mock_squery(fun(Sql) -> Sql =:= <<"THIS IS NOT VALID SQL">> end),
            failure_fixture()
        end,
        fun(Dir) ->
            meck:unload(epgsql),
            cleanup_dir(Dir)
        end,
        fun(Dir) ->
            ?_test(begin
                {ok, Files} = ?SUT:migration_files(Dir),
                {error, Failures} = ?SUT:apply_migrations(fake_conn, Files),
                %% exactly the bad statement failed, with file / index /
                %% statement head; execution continued past it
                [{<<"00000001_bad.up.sql">>, 2, Head, Reason}] = Failures,
                ?assertEqual(<<"THIS IS NOT VALID SQL">>, Head),
                ?assertMatch({error, error, _}, Reason),
                Sqls = squery_history(),
                ?assertEqual(3, length(Sqls)),
                ?assert(lists:member(<<"CREATE TABLE t_after (id bigint PRIMARY KEY)">>, Sqls))
            end)
        end}.

failure_fixture() ->
    Dir = tmp_dir("failing"),
    _ = write_file(
        Dir,
        "00000001_bad.up.sql",
        <<
            "CREATE TABLE t_first (id bigint PRIMARY KEY);\n"
            "THIS IS NOT VALID SQL;\n"
            "CREATE TABLE t_after (id bigint PRIMARY KEY);\n"
        >>
    ),
    Dir.

%%% ==================================================================
%%% Unset DSN must skip the gated suite (defect-1 regression lock)
%%% ==================================================================

unset_dsn_skips_suite_test_() ->
    {setup,
        fun() ->
            Old = os:getenv(?ENV_DSN),
            os:unsetenv(?ENV_DSN),
            Old
        end,
        fun
            (false) -> ok;
            (Old) -> os:putenv(?ENV_DSN, Old)
        end, fun(_Old) ->
            ?_assertMatch({skip, _}, ?SUT:connect_scratch())
        end}.

invalid_dsn_fails_suite_test_() ->
    {setup, fun() -> set_dsn("invalid://dsn") end, fun restore_dsn/1, fun(_Old) ->
        ?_assertMatch({error, {invalid_dsn, _}}, ?SUT:connect_scratch())
    end}.

configured_dsn_connect_failure_fails_suite_test_() ->
    {setup,
        fun() ->
            Old = set_dsn("postgres://postgres@127.0.0.1:5432/tsid_scratch"),
            meck:new(inttest_marker_db, [no_link]),
            meck:expect(inttest_marker_db, safe_connect, 1, {error, econnrefused}),
            Old
        end,
        fun(Old) ->
            meck:unload(inttest_marker_db),
            restore_dsn(Old)
        end,
        fun(_Old) ->
            ?_assertEqual(
                {error, {scratch_setup_failed, {connect_failed, econnrefused}}},
                ?SUT:connect_scratch()
            )
        end}.

%%% ==================================================================
%%% Helpers
%%% ==================================================================

mock_squery(FailOn) ->
    meck:new(epgsql, [no_link]),
    meck:expect(
        epgsql,
        squery,
        2,
        fun(_Conn, Sql) ->
            case FailOn(Sql) of
                true -> {error, {error, error, <<"forced failure">>}};
                false -> {ok, [], []}
            end
        end
    ),
    ok.

squery_history() ->
    %% 本仓 meck 版本的 history 元素为三元组 {CallerPid, {M, F, Args}, Result}
    %% （对齐 test/lib/log_redact_tests.erl 的既有解析口径）。
    [Sql || {_Pid, {_M, _F, [_Conn, Sql]}, _Result} <- meck:history(epgsql)].

set_dsn(Value) ->
    Old = os:getenv(?ENV_DSN),
    true = os:putenv(?ENV_DSN, Value),
    Old.

restore_dsn(false) ->
    true = os:unsetenv(?ENV_DSN),
    ok;
restore_dsn(Old) ->
    true = os:putenv(?ENV_DSN, Old),
    ok.

tmp_dir(Label) ->
    %% os:tmpdir/0 在 eunit 环境的 OTP 版本不可用（error:undef），
    %% 手工解析 TMPDIR 与 /tmp 缺省。
    TmpBase =
        case os:getenv("TMPDIR") of
            false -> "/tmp";
            [] -> "/tmp";
            Tmpdir0 -> Tmpdir0
        end,
    Dir = filename:join([
        TmpBase,
        "imboy_tsid_plumb",
        Label ++ "_" ++ integer_to_list(erlang:unique_integer([positive, monotonic]))
    ]),
    ok = filelib:ensure_dir(filename:join(Dir, "keep")),
    Dir.

write_file(Dir, Name, Content) ->
    Path = filename:join(Dir, Name),
    ok = file:write_file(Path, Content),
    Path.

cleanup_dir(Dir) ->
    _ = file:del_dir_r(Dir),
    ok.

last_basename(Files) ->
    {_N, Path} = lists:last(Files),
    unicode:characters_to_binary(filename:basename(Path)).

migrations_dir() ->
    %% Same beam-location convention as elib_tsid_catalog_db_tests.
    Ebin = code:which(?MODULE),
    filename:join(filename:dirname(filename:dirname(Ebin)), "priv/migrations").
