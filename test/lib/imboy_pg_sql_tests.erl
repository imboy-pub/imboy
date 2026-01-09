-module(imboy_pg_sql_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% imboy_pg_sql 模块的 EUnit 测试
%%%
%%% 目标：验证 SQL 构造函数的正确性
%%% 覆盖：insert, update, insert_batch, select, build_select, build_where_clause
%%%===================================================================

%% ===================================================================
%% 测试装置
%% ===================================================================

setup() ->
    % 设置测试环境配置，确保 sql_driver 返回 pgsql
    application:set_env(imboy, sql_driver, pgsql).

cleanup(_) ->
    % 清理测试环境配置
    application:unset_env(imboy, sql_driver).

%% ===================================================================
%% public_tablename/1 测试
%% ===================================================================

public_tablename_without_prefix_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 设置测试环境配置，确保 sql_driver 返回 pgsql
        application:set_env(imboy, sql_driver, pgsql),
        Result = imboy_pg_sql:public_tablename(<<"user">>),
        ?assertEqual(<<"public.user">>, Result)
    end).

public_tablename_with_prefix_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 设置测试环境配置，确保 sql_driver 返回 pgsql
        application:set_env(imboy, sql_driver, pgsql),
        Result = imboy_pg_sql:public_tablename(<<"public.user">>),
        ?assertEqual(<<"public.user">>, Result)
    end).

public_tablename_nested_prefix_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 设置测试环境配置，确保 sql_driver 返回 pgsql
        application:set_env(imboy, sql_driver, pgsql),
        Result = imboy_pg_sql:public_tablename(<<"public.public.user">>),
        ?assertEqual(<<"public.user">>, Result)
    end).

%% ===================================================================
%% insert/2 测试
%% ===================================================================

insert_single_field_test_() ->
    ?TEST_WITH_DB(fun() ->
        Table = <<"user">>,
        Map = #{name => <<"Alice">>},
        {Sql, Params} = imboy_pg_sql:insert(Table, Map),
        ?assertEqual(<<"INSERT INTO user ( name ) VALUES ( $1 )">>, iolist_to_binary(Sql)),
        ?assertEqual([<<"Alice">>], Params)
    end).

insert_multiple_fields_test_() ->
    ?TEST_WITH_DB(fun() ->
        Table = <<"user">>,
        Map = #{name => <<"Bob">>, age => 30, status => 1},
        {Sql, Params} = imboy_pg_sql:insert(Table, Map),
        SqlBin = iolist_to_binary(Sql),
        ?assertMatch({_, _}, binary:match(SqlBin, <<"INSERT INTO user">>)),
        ?assertEqual(3, length(Params))
    end).

insert_empty_map_test_() ->
    ?TEST_WITH_DB(fun() ->
        Table = <<"user">>,
        Map = #{},
        {Sql, Params} = imboy_pg_sql:insert(Table, Map),
        ?assertEqual(<<"INSERT INTO user ( ) VALUES ( )">>, iolist_to_binary(Sql)),
        ?assertEqual([], Params)
    end).

insert_with_binary_key_test_() ->
    ?TEST_WITH_DB(fun() ->
        Table = <<"user">>,
        Map = #{<<"user_id">> => 123},
        {_Sql, Params} = imboy_pg_sql:insert(Table, Map),
        ?assertEqual([123], Params)
    end).

%% ===================================================================
%% update/3 测试
%% ===================================================================

update_single_field_test_() ->
    ?TEST_WITH_DB(fun() ->
        Table = <<"user">>,
        Map = #{name => <<"Charlie">>},
        WhereSql = <<"id = $1">>,
        {Sql, Params} = imboy_pg_sql:update(Table, Map, WhereSql, []),
        SqlBin = iolist_to_binary(Sql),
        ?assertMatch({_, _}, binary:match(SqlBin, <<"UPDATE user SET name = $1 WHERE id = $1">>)),
        ?assertEqual([<<"Charlie">>], Params)
    end).

update_multiple_fields_test_() ->
    ?TEST_WITH_DB(fun() ->
        Table = <<"user">>,
        Map = #{name => <<"David">>, age => 25, status => 1},
        WhereSql = <<"id = $4">>,
        {Sql, Params} = imboy_pg_sql:update(Table, Map, WhereSql, []),
        SqlBin = iolist_to_binary(Sql),
        ?assertMatch({_, _}, binary:match(SqlBin, <<"UPDATE user SET">>)),
        ?assertEqual(3, length(Params))
    end).

update_with_raw_field_test_() ->
    ?TEST_WITH_DB(fun() ->
        Table = <<"user">>,
        Map = #{updated_at => {raw, <<"NOW()">>}},
        WhereSql = <<"id = $1">>,
        {Sql, Params} = imboy_pg_sql:update(Table, Map, WhereSql, []),
        SqlBin = iolist_to_binary(Sql),
        ?assertMatch({_, _}, binary:match(SqlBin, <<"updated_at = NOW()">>)),
        ?assertEqual([], Params)
    end).

update_empty_map_test_() ->
    ?TEST_WITH_DB(fun() ->
        Table = <<"user">>,
        Map = #{},
        WhereSql = <<"id = 1">>,
        {_Sql, Params} = imboy_pg_sql:update(Table, Map, WhereSql, []),
        ?assertEqual([], Params)
    end).

%% ===================================================================
%% update/4 测试（WHERE 参数化）
%% ===================================================================

update_4_single_where_param_test_() ->
    ?TEST_WITH_DB(fun() ->
        Table = <<"user">>,
        Map = #{name => <<"Eve">>, age => 28},
        WhereSql = <<"id = $1">>,
        WhereParams = [123],
        {Sql, Params} = imboy_pg_sql:update(Table, Map, WhereSql, WhereParams),
        SqlBin = iolist_to_binary(Sql),
        % WHERE 参数是 $1，SET 参数从 $2 开始
        ?assertMatch({_, _}, binary:match(SqlBin, <<"UPDATE user SET name = $2, age = $3 WHERE id = $1">>)),
        % 参数顺序：WHERE 参数在前，SET 参数在后
        ?assertEqual([123, <<"Eve">>, 28], Params)
    end).

update_4_multiple_where_params_test_() ->
    ?TEST_WITH_DB(fun() ->
        Table = <<"user">>,
        Map = #{status => 2},
        WhereSql = <<"id = $1 AND status = $2">>,
        WhereParams = [456, 1],
        {Sql, Params} = imboy_pg_sql:update(Table, Map, WhereSql, WhereParams),
        SqlBin = iolist_to_binary(Sql),
        % WHERE 参数是 $1, $2，SET 参数从 $3 开始
        ?assertMatch({_, _}, binary:match(SqlBin, <<"UPDATE user SET status = $3 WHERE id = $1 AND status = $2">>)),
        % 参数顺序：WHERE 参数在前，SET 参数在后
        ?assertEqual([456, 1, 2], Params)
    end).

update_4_complex_where_test_() ->
    ?TEST_WITH_DB(fun() ->
        Table = <<"user">>,
        Map = #{name => <<"Frank">>, age => 35, updated_at => <<"2024-01-01">>},
        WhereSql = <<"id = $1 OR (status = $2 AND created_at > $3)">>,
        WhereParams = [789, 1, <<"2023-01-01">>],
        {Sql, Params} = imboy_pg_sql:update(Table, Map, WhereSql, WhereParams),
        SqlBin = iolist_to_binary(Sql),
        % WHERE 参数是 $1, $2, $3，SET 参数从 $4 开始
        ?assertMatch({_, _}, binary:match(SqlBin, <<"UPDATE user SET name = $4, age = $5, updated_at = $6 WHERE id = $1 OR (status = $2 AND created_at > $3)">>)),
        % 参数顺序：WHERE 参数在前，SET 参数在后
        ?assertEqual([789, 1, <<"2023-01-01">>, <<"Frank">>, 35, <<"2024-01-01">>], Params)
    end).

update_4_with_in_clause_test_() ->
    ?TEST_WITH_DB(fun() ->
        Table = <<"user">>,
        Map = #{status => 0},
        WhereSql = <<"id IN ($1, $2, $3)">>,
        WhereParams = [1, 2, 3],
        {Sql, Params} = imboy_pg_sql:update(Table, Map, WhereSql, WhereParams),
        SqlBin = iolist_to_binary(Sql),
        ?assertMatch({_, _}, binary:match(SqlBin, <<"UPDATE user SET status = $4 WHERE id IN ($1, $2, $3)">>)),
        ?assertEqual([1, 2, 3, 0], Params)
    end).

update_4_empty_where_params_test_() ->
    ?TEST_WITH_DB(fun() ->
        Table = <<"user">>,
        Map = #{name => <<"Grace">>},
        WhereSql = <<"1=1">>,
        WhereParams = [],
        {Sql, Params} = imboy_pg_sql:update(Table, Map, WhereSql, WhereParams),
        SqlBin = iolist_to_binary(Sql),
        % 没有 WHERE 参数，SET 参数从 $1 开始
        ?assertMatch({_, _}, binary:match(SqlBin, <<"UPDATE user SET name = $1 WHERE 1=1">>)),
        ?assertEqual([<<"Grace">>], Params)
    end).

update_4_security_prevention_test_() ->
    ?TEST_WITH_DB(fun() ->
        Table = <<"user">>,
        Map = #{name => <<"Hacker">>},
        % 模拟恶意的 WHERE 参数（包含 SQL 注入尝试）
        WhereSql = <<"id = $1">>,
        WhereParams = [<<"1; DROP TABLE user; --">>],
        {Sql, Params} = imboy_pg_sql:update(Table, Map, WhereSql, WhereParams),
        SqlBin = iolist_to_binary(Sql),
        % 参数化后，恶意输入会被当作普通字符串处理，不会被执行
        ?assertMatch({_, _}, binary:match(SqlBin, <<"UPDATE user SET name = $2 WHERE id = $1">>)),
        % 参数会被正确转义，不会造成 SQL 注入
        ?assertEqual([<<"1; DROP TABLE user; --">>, <<"Hacker">>], Params)
    end).

%% ===================================================================
%% insert_batch/3 测试
%% ===================================================================

insert_batch_two_rows_test_() ->
    ?TEST_WITH_DB(fun() ->
        Table = <<"user">>,
        Cols = [name, age],
        Rows = [[<<"Alice">>, 30], [<<"Bob">>, 25]],
        {Sql, Params} = imboy_pg_sql:insert_batch(Table, Cols, Rows),
        SqlBin = iolist_to_binary(Sql),
        ?assertMatch({_, _}, binary:match(SqlBin, <<"INSERT INTO user ( name,age ) VALUES">>)),
        ?assertEqual([<<"Alice">>, 30, <<"Bob">>, 25], lists:flatten(Params))
    end).

insert_batch_single_row_test_() ->
    ?TEST_WITH_DB(fun() ->
        Table = <<"user">>,
        Cols = [id],
        Rows = [[1]],
        {_Sql, Params} = imboy_pg_sql:insert_batch(Table, Cols, Rows),
        ?assertEqual([1], lists:flatten(Params))
    end).

insert_batch_empty_rows_test_() ->
    ?TEST_WITH_DB(fun() ->
        Table = <<"user">>,
        Cols = [id],
        Rows = [],
        {Sql, _Params} = imboy_pg_sql:insert_batch(Table, Cols, Rows),
        SqlBin = iolist_to_binary(Sql),
        ?assertEqual(nomatch, binary:match(SqlBin, <<"VALUES ">>))
    end).

%% ===================================================================
%% select/2 测试
%% ===================================================================

select_basic_test_() ->
    ?TEST_WITH_DB(fun() ->
        Table = <<"user">>,
        WhereSql = <<"id = 1">>,
        Sql = imboy_pg_sql:select(Table, WhereSql),
        ?assertEqual(<<"SELECT * FROM user WHERE id = 1">>, iolist_to_binary(Sql))
    end).

select_with_empty_where_test_() ->
    ?TEST_WITH_DB(fun() ->
        Table = <<"user">>,
        WhereSql = <<>>,
        Sql = imboy_pg_sql:select(Table, WhereSql),
        Expected = <<"SELECT * FROM user WHERE ">>,
        ?assertEqual(Expected, iolist_to_binary(Sql))
    end).

%% ===================================================================
%% build_select/4 测试
%% ===================================================================

build_select_all_fields_test_() ->
    ?TEST_WITH_DB(fun() ->
        Table = <<"user">>,
        Fields = [<<"*">>],
        Where = #{},
        Opts = #{},
        {_Sql, Params} = imboy_pg_sql:build_select(Table, Fields, Where, Opts),
        ?assertEqual([], Params)
    end).

build_select_specific_fields_test_() ->
    ?TEST_WITH_DB(fun() ->
        Table = <<"user">>,
        Fields = [<<"id">>, <<"name">>],
        Where = #{id => 1},
        Opts = #{},
        {Sql, Params} = imboy_pg_sql:build_select(Table, Fields, Where, Opts),
        SqlBin = iolist_to_binary(Sql),
        ?assertMatch({_, _}, binary:match(SqlBin, <<"SELECT id,name FROM user WHERE id = $1">>)),
        ?assertEqual([1], Params)
    end).

build_select_with_order_by_test_() ->
    ?TEST_WITH_DB(fun() ->
        Table = <<"user">>,
        Fields = [<<"*">>],
        Where = #{},
        Opts = #{order_by => [{id, desc}, {name, asc}]},
        {Sql, _Params} = imboy_pg_sql:build_select(Table, Fields, Where, Opts),
        SqlBin = iolist_to_binary(Sql),
        ?assertMatch({_, _}, binary:match(SqlBin, <<"ORDER BY id DESC, name ASC">>))
    end).

build_select_with_limit_test_() ->
    ?TEST_WITH_DB(fun() ->
        Table = <<"user">>,
        Fields = [<<"*">>],
        Where = #{},
        Opts = #{limit => 10},
        {Sql, Params} = imboy_pg_sql:build_select(Table, Fields, Where, Opts),
        SqlBin = iolist_to_binary(Sql),
        ?assertMatch({_, _}, binary:match(SqlBin, <<"LIMIT $1">>)),
        ?assertEqual([10], Params)
    end).

build_select_with_offset_test_() ->
    ?TEST_WITH_DB(fun() ->
        Table = <<"user">>,
        Fields = [<<"*">>],
        Where = #{},
        Opts = #{offset => 5},
        {Sql, Params} = imboy_pg_sql:build_select(Table, Fields, Where, Opts),
        SqlBin = iolist_to_binary(Sql),
        ?assertMatch({_, _}, binary:match(SqlBin, <<"OFFSET $1">>)),
        ?assertEqual([5], Params)
    end).

build_select_with_limit_and_offset_test_() ->
    ?TEST_WITH_DB(fun() ->
        Table = <<"user">>,
        Fields = [<<"*">>],
        Where = #{},
        Opts = #{limit => 10, offset => 5},
        {Sql, Params} = imboy_pg_sql:build_select(Table, Fields, Where, Opts),
        SqlBin = iolist_to_binary(Sql),
        ?assertMatch({_, _}, binary:match(SqlBin, <<"LIMIT $1 OFFSET $2">>)),
        ?assertEqual([10, 5], Params)
    end).

%% ===================================================================
%% build_where_clause/1 测试
%% ===================================================================

build_where_empty_test_() ->
    ?TEST_WITH_DB(fun() ->
        Where = #{},
        {Sql, Params} = imboy_pg_sql:build_where_clause(Where),
        ?assertEqual(<<>>, Sql),
        ?assertEqual([], Params)
    end).

build_where_single_condition_test_() ->
    ?TEST_WITH_DB(fun() ->
        Where = #{id => 1},
        {Sql, Params} = imboy_pg_sql:build_where_clause(Where),
        ?assertEqual(<<" WHERE id = $1">>, Sql),
        ?assertEqual([1], Params)
    end).

build_where_multiple_conditions_test_() ->
    ?TEST_WITH_DB(fun() ->
        Where = #{id => 1, status => 1},
        {Sql, Params} = imboy_pg_sql:build_where_clause(Where),
        ?assertMatch({_, _}, binary:match(Sql, <<"WHERE id = $1 AND status = $2">>)),
        ?assertEqual([1, 1], Params)
    end).

build_where_with_in_clause_test_() ->
    ?TEST_WITH_DB(fun() ->
        Where = #{id => {in, [1, 2, 3]}},
        {Sql, Params} = imboy_pg_sql:build_where_clause(Where),
        ?assertMatch({_, _}, binary:match(Sql, <<"WHERE id IN ($1,$2,$3)">>)),
        ?assertEqual([1, 2, 3], Params)
    end).

build_where_with_not_in_clause_test_() ->
    ?TEST_WITH_DB(fun() ->
        Where = #{status => {not_in, [0, -1]}},
        {Sql, Params} = imboy_pg_sql:build_where_clause(Where),
        ?assertMatch({_, _}, binary:match(Sql, <<"WHERE status NOT IN ($1,$2)">>)),
        ?assertEqual([0, -1], Params)
    end).

build_where_with_op_test_() ->
    ?TEST_WITH_DB(fun() ->
        Where = #{age => {op, <<">">>, 18}},
        {Sql, Params} = imboy_pg_sql:build_where_clause(Where),
        ?assertMatch({_, _}, binary:match(Sql, <<"WHERE age > $1">>)),
        ?assertEqual([18], Params)
    end).

build_where_with_raw_test_() ->
    ?TEST_WITH_DB(fun() ->
        Where = #{created_at => {raw, <<"NOW()">>}},
        {Sql, Params} = imboy_pg_sql:build_where_clause(Where),
        ?assertMatch({_, _}, binary:match(Sql, <<"WHERE created_at NOW()">>)),
        ?assertEqual([], Params)
    end).

build_where_mixed_conditions_test_() ->
    ?TEST_WITH_DB(fun() ->
        Where = #{
            id => {in, [1, 2]},
            status => {op, <<">">>, 0},
            name => <<"Alice">>
        },
        {Sql, Params} = imboy_pg_sql:build_where_clause(Where),
        ?assertMatch({_, _}, binary:match(Sql, <<"WHERE id IN ($1,$2) AND status > $3 AND name = $4">>)),
        ?assertEqual([1, 2, 0, <<"Alice">>], Params)
    end).

build_where_atom_key_test_() ->
    ?TEST_WITH_DB(fun() ->
        Where = #{id => 1},
        {Sql, Params} = imboy_pg_sql:build_where_clause(Where),
        ?assertMatch({_, _}, binary:match(Sql, <<"WHERE id = $1">>)),
        ?assertEqual([1], Params)
    end).

build_where_with_or_test_() ->
    ?TEST_WITH_DB(fun() ->
        % 测试 OR 连接多个条件组
        Where = #{<<"__or">> => [
            #{a => 1, b => 3},
            #{a => 4, b => 5, c => {op, <<"LIKE">>, <<"%c%">>}}
        ]},
        {Sql, Params} = imboy_pg_sql:build_where_clause(Where),
        ?assertMatch({_, _}, binary:match(Sql, <<"WHERE (a = $1 AND b = $2) OR (a = $3 AND b = $4 AND c LIKE $5)">>)),
        ?assertEqual([1, 3, 4, 5, <<"%c%">>], Params)
    end).

build_where_with_and_test_() ->
    ?TEST_WITH_DB(fun() ->
        % 测试 AND 连接多个条件组
        Where = #{<<"__and">> => [
            #{a => 1},
            #{b => 2},
            #{c => 3}
        ]},
        {Sql, Params} = imboy_pg_sql:build_where_clause(Where),
        ?assertMatch({_, _}, binary:match(Sql, <<"WHERE (a = $1) AND (b = $2) AND (c = $3)">>)),
        ?assertEqual([1, 2, 3], Params)
    end).

build_where_with_nested_or_and_test_() ->
    ?TEST_WITH_DB(fun() ->
        % 测试嵌套 OR/AND: (a=1 AND (b=2 OR b=3)) OR (a=4)
        Where = #{<<"__or">> => [
            #{a => 1, <<"__and">> => [#{b => 2}, #{b => 3}]},
            #{a => 4}
        ]},
        {Sql, Params} = imboy_pg_sql:build_where_clause(Where),
        ?assertMatch({_, _}, binary:match(Sql, <<"WHERE (a = $1 AND (b = $2) AND (b = $3)) OR (a = $4)">>)),
        ?assertEqual([1, 2, 3, 4], Params)
    end).

build_where_with_complex_nested_test_() ->
    ?TEST_WITH_DB(fun() ->
        % 测试复杂嵌套: (a=1 AND b=2) OR (c=3 AND d=4) OR (e=5 AND f=6)
        Where = #{<<"__or">> => [
            #{a => 1, b => 2},
            #{c => 3, d => 4},
            #{e => 5, f => 6}
        ]},
        {Sql, Params} = imboy_pg_sql:build_where_clause(Where),
        ?assertMatch({_, _}, binary:match(Sql, <<"WHERE (a = $1 AND b = $2) OR (c = $3 AND d = $4) OR (e = $5 AND f = $6)">>)),
        ?assertEqual([1, 2, 3, 4, 5, 6], Params)
    end).

%% ===================================================================
%% page/6 测试
%% ===================================================================

page_basic_test_() ->
    ?TEST_WITH_DB(fun() ->
        Table = <<"user">>,
        Column = <<"*">>,
        WhereSql = <<"id > 0">>,
        OrderBy = <<"id DESC">>,
        Limit = 10,
        Offset = 0,
        {Sql, Params} = imboy_pg_sql:page(Table, Column, WhereSql, OrderBy, Limit, Offset),
        SqlBin = iolist_to_binary(Sql),
        ?assertMatch({_, _}, binary:match(SqlBin, <<"SELECT * FROM user WHERE id > 0 ORDER BY id DESC LIMIT $1 OFFSET $2">>)),
        ?assertEqual([10, 0], Params)
    end).

page_custom_column_test_() ->
    ?TEST_WITH_DB(fun() ->
        Table = <<"user">>,
        Column = <<"id, name">>,
        WhereSql = <<"status = 1">>,
        OrderBy = <<"id ASC">>,
        Limit = 5,
        Offset = 10,
        {_Sql, Params} = imboy_pg_sql:page(Table, Column, WhereSql, OrderBy, Limit, Offset),
        ?assertEqual([5, 10], Params)
    end).
