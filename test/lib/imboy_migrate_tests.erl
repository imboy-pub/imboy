-module(imboy_migrate_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% imboy_migrate 模块的 EUnit 测试
%%%
%%% 目标：验证数据库迁移功能
%%% 覆盖：路径获取、项目目录展开、迁移执行
%%%===================================================================

%% ===================================================================
%% get_scripts_path/0 测试
%% ===================================================================

get_scripts_path_from_config_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(scripts_path) ->
                "/path/to/migrations"
            end}
        ]},
        {code, [
            {'priv_dir', 1, fun(imboy) -> "/priv" end}
        ]}
    ], fun() ->
        Result = imboy_migrate:get_scripts_path(),
        ?assertEqual("/path/to/migrations", Result)
    end).

get_scripts_path_with_project_dir_placeholder_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(scripts_path) ->
                "$PROJECT_DIR/migrations"
            end}
        ]},
        {code, [
            {'priv_dir', 1, fun(imboy) -> "/app/priv" end}
        ]}
    ], fun() ->
        Result = imboy_migrate:get_scripts_path(),
        ?assertEqual("/app/migrations", Result)
    end).

get_scripts_path_with_project_dir_and_leading_slash_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(scripts_path) ->
                "$PROJECT_DIR/sub/dir"
            end}
        ]},
        {code, [
            {'priv_dir', 1, fun(imboy) -> "/priv" end}
        ]}
    ], fun() ->
        Result = imboy_migrate:get_scripts_path(),
        ?assertEqual("/priv/sub/dir", Result)
    end).

get_scripts_path_with_binary_config_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(scripts_path) ->
                <<"/binary/path/migrations">>
            end}
        ]},
        {code, [
            {'priv_dir', 1, fun(imboy) -> "/priv" end}
        ]}
    ], fun() ->
        Result = imboy_migrate:get_scripts_path(),
        ?assertEqual("/binary/path/migrations", Result)
    end).

get_scripts_path_with_invalid_config_test_() ->
    ?WITH_MECK(code, [
        {'priv_dir', 1, fun(imboy) -> "/default/priv" end}
    ], fun() ->
        ?WITH_MECK(config_ds, [
            {'env', 1, fun(scripts_path) -> undefined end}
        ], fun() ->
            Result = imboy_migrate:get_scripts_path(),
            ?assertEqual("/default/priv/migrations", Result)
        end)
    end).

%% ===================================================================
%% expand_project_dir/1 测试
%% ===================================================================

expand_project_dir_with_only_placeholder_test_() ->
    ?WITH_MECK(code, [
        {'priv_dir', 1, fun(imboy) -> "/app/priv" end}
    ], fun() ->
        Result = imboy_migrate:expand_project_dir("$PROJECT_DIR"),
        ?assertEqual("/app/priv", Result)
    end).

expand_project_dir_with_relative_path_test_() ->
    ?WITH_MECK(code, [
        {'priv_dir', 1, fun(imboy) -> "/app/priv" end}
    ], fun() ->
        Result = imboy_migrate:expand_project_dir("$PROJECT_DIR/migrations"),
        ?assertEqual("/app/priv/migrations", Result)
    end).

expand_project_dir_with_leading_slash_test_() ->
    ?WITH_MECK(code, [
        {'priv_dir', 1, fun(imboy) -> "/priv" end}
    ], fun() ->
        Result = imboy_migrate:expand_project_dir("$PROJECT_DIR/sub"),
        ?assertEqual("/priv/sub", Result)
    end).

expand_project_dir_without_placeholder_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = imboy_migrate:expand_project_dir("/absolute/path"),
        ?assertEqual("/absolute/path", Result)
    end).

expand_project_dir_with_windows_path_test_() ->
    ?WITH_MECK(code, [
        {'priv_dir', 1, fun(imboy) -> "C:\\app\\priv" end}
    ], fun() ->
        Result = imboy_migrate:expand_project_dir("$PROJECT_DIR\\sub"),
        ?assertEqual("C:\\app\\priv\\sub", Result)
    end).

%% ===================================================================
%% priv_is_valid/1 测试
%% ===================================================================

priv_is_valid_with_ok_results_test_() ->
    ?TEST_SIMPLE(fun() ->
        List = [{ok, 1}, {ok, 2}, {ok, 3}],
        Result = imboy_migrate:priv_is_valid(List),
        ?assert(Result)
    end).

priv_is_valid_with_three_element_ok_tuples_test_() ->
    ?TEST_SIMPLE(fun() ->
        List = [{ok, [], []}, {ok, [{col, <<"a">>}], []}],
        Result = imboy_migrate:priv_is_valid(List),
        ?assert(Result)
    end).

priv_is_valid_with_four_element_ok_tuples_test_() ->
    ?TEST_SIMPLE(fun() ->
        List = [{ok, 1, [], []}, {ok, 2, [{col}], []}],
        Result = imboy_migrate:priv_is_valid(List),
        ?assert(Result)
    end).

priv_is_valid_with_mixed_results_test_() ->
    ?TEST_SIMPLE(fun() ->
        List = [{ok, 1}, {error, reason}],
        Result = imboy_migrate:priv_is_valid(List),
        ?assertNot(Result)
    end).

priv_is_valid_with_error_results_test_() ->
    ?TEST_SIMPLE(fun() ->
        List = [{error, reason1}, {error, reason2}],
        Result = imboy_migrate:priv_is_valid(List),
        ?assertNot(Result)
    end).

priv_is_valid_with_empty_list_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = imboy_migrate:priv_is_valid([]),
        ?assert(Result)
    end).

priv_is_valid_with_invalid_results_test_() ->
    ?TEST_SIMPLE(fun() ->
        List = [invalid, {error, reason}],
        Result = imboy_migrate:priv_is_valid(List),
        ?assertNot(Result)
    end).

%% ===================================================================
%% set_max_id_seq/0 测试
%% ===================================================================

set_max_id_seq_success_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(super_account) ->
                #{host => "localhost", database => "test"}
            end},
            {'env', 1, fun(scripts_path) ->
                "/priv/migrations"
            end}
        ]},
        {file, [
            {'read_file', 1, fun(_File) ->
                {ok, <<"-- Comment\nSELECT 1;\nSELECT 2;">>}
            end}
        ]},
        {epgsql, [
            {'connect', 1, fun(_Conf) -> {ok, self()} end},
            {'squery', 2, fun(_Conn, _Sql) -> {ok, [], []} end},
            {'close', 1, fun(_Conn) -> ok end}
        ]}
    ], fun() ->
        Result = imboy_migrate:set_max_id_seq(),
        ?assertMatch(#{success := _, fail := _}, Result)
    end).

set_max_id_seq_with_sql_errors_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(super_account) ->
                #{host => "localhost"}
            end},
            {'env', 1, fun(scripts_path) ->
                "/priv/migrations"
            end}
        ]},
        {file, [
            {'read_file', 1, fun(_File) ->
                {ok, <<"SELECT 1;\nINVALID SQL;">>}
            end}
        ]},
        {epgsql, [
            {'connect', 1, fun(_Conf) -> {ok, self()} end},
            {'squery', 2, fun(_Conn, Sql) ->
                case Sql of
                    <<"SELECT 1;">> -> {ok, [], []};
                    _ -> {error, syntax_error}
                end
            end},
            {'close', 1, fun(_Conn) -> ok end}
        ]}
    ], fun() ->
        Result = imboy_migrate:set_max_id_seq(),
        ?assertMatch(#{success := 1, fail := 1}, Result)
    end).

set_max_id_seq_with_comments_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(super_account) -> #{} end},
            {'env', 1, fun(scripts_path) -> "/priv" end}
        ]},
        {file, [
            {'read_file', 1, fun(_File) ->
                {ok, <<"# Shell comment\n-- SQL comment\nSELECT 1;">>}
            end}
        ]},
        {epgsql, [
            {'connect', 1, fun(_Conf) -> {ok, self()} end},
            {'squery', 2, fun(_Conn, _Sql) -> {ok, [], []} end},
            {'close', 1, fun(_Conn) -> ok end}
        ]}
    ], fun() ->
        Result = imboy_migrate:set_max_id_seq(),
        ?assertMatch(#{success := 1, fail := 0}, Result)
    end).

%% ===================================================================
%% migrate_msg_payload_plaintext/0 测试
%% ===================================================================

migrate_msg_payload_plaintext_success_test_() ->
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
            {'execute', 2, fun(_Sql, _Key) -> {ok, 5} end}
        ]}
    ], fun() ->
        Result = imboy_migrate:migrate_msg_payload_plaintext(),
        ?assert(is_map(Result)),
        ?assert(maps:is_key(<<"msg_c2c">>, Result)),
        ?assert(maps:is_key(<<"msg_c2g">>, Result))
    end).

migrate_msg_payload_plaintext_single_table_test_() ->
    ?WITH_MECK(lib_pg, [
        {'execute', 2, fun(_Sql, _Key) -> {ok, 10} end}
    ], fun() ->
        Result = imboy_migrate:migrate_msg_payload_plaintext(<<"test_table">>, <<"key">>),
        ?assertEqual({ok, 10}, Result)
    end).

%% ===================================================================
%% read_sql_file/1 测试
%% ===================================================================

read_sql_file_success_test_() ->
    ?WITH_MECK(file, [
        {'read_file', 1, fun("/path/to/file.sql") ->
            {ok, <<"CREATE TABLE test (id INT);">>}
        end}
    ], fun() ->
        Result = imboy_migrate:read_sql_file("/path/to/file.sql"),
        ?assertEqual({ok, <<"CREATE TABLE test (id INT);">>}, Result)
    end).

read_sql_file_error_test_() ->
    ?WITH_MECK(file, [
        {'read_file', 1, fun("/missing/file.sql") ->
            {error, enoent}
        end}
    ], fun() ->
        Result = imboy_migrate:read_sql_file("/missing/file.sql"),
        ?assertMatch({error, {file_read_error, "/missing/file.sql", _}}, Result)
    end).

%% ===================================================================
%% split_sql_statements/1 测试
%% ===================================================================

split_sql_statements_with_multiple_statements_test_() ->
    ?TEST_SIMPLE(fun() ->
        SqlContent = <<"SELECT 1; SELECT 2; SELECT 3;">>,
        Result = imboy_migrate:split_sql_statements(SqlContent),
        ?assertEqual(3, length(Result))
    end).

split_sql_statements_filters_comments_test_() ->
    ?TEST_SIMPLE(fun() ->
        SqlContent = <<"-- Comment\nSELECT 1;\n-- Another comment\nSELECT 2;">>,
        Result = imboy_migrate:split_sql_statements(SqlContent),
        ?assertEqual(2, length(Result))
    end).

split_sql_statements_filters_empty_test_() ->
    ?TEST_SIMPLE(fun() ->
        SqlContent = <<"SELECT 1;;   \n;\nSELECT 2;">>,
        Result = imboy_migrate:split_sql_statements(SqlContent),
        ?assertEqual(2, length(Result))
    end).

split_sql_statements_with_whitespace_test_() ->
    ?TEST_SIMPLE(fun() ->
        SqlContent = <<"  \n  SELECT 1;  \n  ">>,
        Result = imboy_migrate:split_sql_statements(SqlContent),
        ?assertEqual(1, length(Result))
    end).

%% ===================================================================
%% trim_sql_whitespace/1 测试
%% ===================================================================

trim_sql_whitespace_leading_test_() ->
    ?TEST_SIMPLE(fun() ->
        Sql = <<"   \n\tSELECT * FROM test">>,
        Result = imboy_migrate:trim_sql_whitespace(Sql),
        ?assertEqual(<<"SELECT * FROM test">>, Result)
    end).

trim_sql_whitespace_trailing_test_() ->
    ?TEST_SIMPLE(fun() ->
        Sql = <<"SELECT * FROM test   \n\t">>,
        Result = imboy_migrate:trim_sql_whitespace(Sql),
        ?assertEqual(<<"SELECT * FROM test">>, Result)
    end).

trim_sql_whitespace_both_test_() ->
    ?TEST_SIMPLE(fun() ->
        Sql = <<"   \nSELECT 1;\n   ">>,
        Result = imboy_migrate:trim_sql_whitespace(Sql),
        ?assertEqual(<<"SELECT 1;">>, Result)
    end).

%% ===================================================================
%% execute_sql_statements/2 测试
%% ===================================================================

execute_sql_statements_with_empty_list_test_() ->
    ?WITH_MECK(epgsql, [
        {'squery', 2, fun(_Conn, _Sql) -> {ok, [], []} end}
    ], fun() ->
        Conn = self(),
        Result = imboy_migrate:execute_sql_statements(Conn, []),
        ?assertEqual(ok, Result)
    end).

execute_sql_statements_success_test_() ->
    ?WITH_MECK(epgsql, [
        {'squery', 2, fun(_Conn, _Sql) -> {ok, [], []} end}
    ], fun() ->
        Conn = self(),
        Statements = [<<"SELECT 1;">>, <<"SELECT 2;">>],
        Result = imboy_migrate:execute_sql_statements(Conn, Statements),
        ?assertEqual(ok, Result)
    end).

execute_sql_statements_with_error_test_() ->
    ?WITH_MECK(epgsql, [
        {'squery', 2, fun(_Conn, Sql) ->
            case Sql of
                <<"SELECT 1;">> -> {ok, [], []};
                _ -> {error, "syntax error"}
            end
        end}
    ], fun() ->
        Conn = self(),
        Statements = [<<"SELECT 1;">>, <<"INVALID;">>],
        Result = imboy_migrate:execute_sql_statements(Conn, Statements),
        ?assertMatch({error, {statement_failed, _, _}}, Result)
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

get_scripts_path_with_empty_config_test_() ->
    ?WITH_MECK(code, [
        {'priv_dir', 1, fun(imboy) -> "/priv" end}
    ], fun() ->
        ?WITH_MECK(config_ds, [
            {'env', 1, fun(scripts_path) -> "" end}
        ], fun() ->
            Result = imboy_migrate:get_scripts_path(),
            ?assertEqual("", Result)
        end)
    end).

expand_project_dir_with_double_slash_test_() ->
    ?WITH_MECK(code, [
        {'priv_dir', 1, fun(imboy) -> "/app" end}
    ], fun() ->
        Result = imboy_migrate:expand_project_dir("$PROJECT_DIR//path"),
        ?assertEqual("/app/path", Result)
    end).

split_sql_statements_with_only_comments_test_() ->
    ?TEST_SIMPLE(fun() ->
        SqlContent = <<"-- Comment 1\n-- Comment 2">>,
        Result = imboy_migrate:split_sql_statements(SqlContent),
        ?assertEqual([], Result)
    end).

split_sql_statements_with_empty_content_test_() ->
    ?TEST_SIMPLE(fun() ->
        SqlContent = <<>>,
        Result = imboy_migrate:split_sql_statements(SqlContent),
        ?assertEqual([], Result)
    end).

set_max_id_seq_with_empty_file_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(super_account) -> #{} end},
            {'env', 1, fun(scripts_path) -> "/priv" end}
        ]},
        {file, [
            {'read_file', 1, fun(_File) -> {ok, <<>>} end}
        ]},
        {epgsql, [
            {'connect', 1, fun(_Conf) -> {ok, self()} end},
            {'close', 1, fun(_Conn) -> ok end}
        ]}
    ], fun() ->
        Result = imboy_migrate:set_max_id_seq(),
        ?assertMatch(#{success := 0, fail := 0}, Result)
    end).
