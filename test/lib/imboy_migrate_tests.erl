-module(imboy_migrate_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

get_scripts_path_from_string_config_test_() ->
    ?WITH_MECK(config_ds, [
        {'env', 1, fun(scripts_path) -> "/path/to/migrations" end}
    ], fun() ->
        ?assertEqual("/path/to/migrations", imboy_migrate:get_scripts_path())
    end).

get_scripts_path_from_binary_config_test_() ->
    ?WITH_MECK(config_ds, [
        {'env', 1, fun(scripts_path) -> <<"/binary/path/migrations">> end}
    ], fun() ->
        ?assertEqual("/binary/path/migrations", imboy_migrate:get_scripts_path())
    end).

get_scripts_path_expands_project_dir_placeholder_test_() ->
    ?WITH_MECK(config_ds, [
        {'env', 1, fun(scripts_path) -> "$PROJECT_DIR/migrations" end}
    ], fun() ->
        ?assertEqual(
            filename:join(code:priv_dir(imboy), "migrations"),
            imboy_migrate:get_scripts_path()
        )
    end).

get_scripts_path_falls_back_to_priv_dir_test_() ->
    ?WITH_MECK(config_ds, [
        {'env', 1, fun(scripts_path) -> undefined end}
    ], fun() ->
        ?assertEqual(
            filename:join(code:priv_dir(imboy), "migrations"),
            imboy_migrate:get_scripts_path()
        )
    end).

set_max_id_seq_counts_successes_and_failures_test_() ->
    ScriptsPath = filename:join(code:priv_dir(imboy), "migrations"),
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun
                (super_account) -> #{host => "localhost", database => "test"};
                (scripts_path) -> ScriptsPath
            end}
        ]},
        {file, [
            {'read_file', 1, fun(_File) ->
                {ok, <<"-- comment\nSELECT 1;\nINVALID SQL;">>}
            end}
        ]},
        {epgsql, [
            {'connect', 1, fun(_Conf) -> {ok, self()} end},
            {'squery', 2, fun(_Conn, Sql) ->
                case Sql of
                    "SELECT 1;" -> {ok, [], []};
                    _ -> {error, syntax_error}
                end
            end},
            {'close', 1, fun(_Conn) -> ok end}
        ]}
    ], fun() ->
        ?assertEqual(
            #{success => 1, fail => 1},
            imboy_migrate:set_max_id_seq()
        )
    end).

set_max_id_seq_ignores_comments_and_empty_lines_test_() ->
    ScriptsPath = filename:join(code:priv_dir(imboy), "migrations"),
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun
                (super_account) -> #{host => "localhost"};
                (scripts_path) -> ScriptsPath
            end}
        ]},
        {file, [
            {'read_file', 1, fun(_File) ->
                {ok, <<"# shell comment\n-- sql comment\n\nSELECT 1;">>}
            end}
        ]},
        {epgsql, [
            {'connect', 1, fun(_Conf) -> {ok, self()} end},
            {'squery', 2, fun(_Conn, "SELECT 1;") -> {ok, [], []} end},
            {'close', 1, fun(_Conn) -> ok end}
        ]}
    ], fun() ->
        ?assertEqual(
            #{success => 1, fail => 0},
            imboy_migrate:set_max_id_seq()
        )
    end).

migrate_msg_payload_plaintext_aggregates_per_table_results_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(postgre_aes_key) -> <<"test_key">> end}
        ]},
        {msg_c2c_repo, [
            {'tablename', 0, fun() -> <<"msg_c2c">> end}
        ]},
        {msg_c2g_repo, [
            {'tablename', 0, fun() -> <<"msg_c2g">> end}
        ]},
        {msg_c2s_repo, [
            {'tablename', 0, fun() -> <<"msg_c2s">> end}
        ]},
        {msg_s2c_repo, [
            {'tablename', 0, fun() -> <<"msg_s2c">> end}
        ]},
        {msg_store_repo, [
            {'tablename', 0, fun() -> <<"msg_store">> end}
        ]},
        {elib_pg, [
            {'execute', 2, fun(_Sql, [<<"test_key">>]) -> {ok, 5} end}
        ]}
    ], fun() ->
        Result = imboy_migrate:migrate_msg_payload_plaintext(),
        ?assertEqual({ok, 5}, maps:get(<<"msg_c2c">>, Result)),
        ?assertEqual({ok, 5}, maps:get(<<"msg_c2g">>, Result)),
        ?assertEqual({ok, 5}, maps:get(<<"msg_c2s">>, Result)),
        ?assertEqual({ok, 5}, maps:get(<<"msg_s2c">>, Result)),
        ?assertEqual({ok, 5}, maps:get(<<"msg_store">>, Result))
    end).
