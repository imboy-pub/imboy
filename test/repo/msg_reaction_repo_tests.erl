-module(msg_reaction_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% msg_reaction_repo 模块的 EUnit 测试（基于 meck mock，不需要数据库）
%%%
%%% 目标：验证消息表情回应数据仓库功能
%%% 覆盖：添加、移除、查询、统计、边界条件
%%%===================================================================

%% ===================================================================
%% tablename/0 测试
%% ===================================================================

tablename_returns_binary_test_() ->
    ?WITH_MECKS([
        {config_ds, [
            {'env', 1, fun(sql_driver) -> pgsql end}
        ]}
    ], fun() ->
        TableName = msg_reaction_repo:tablename(),
        ?assertEqual(<<"public.msg_reaction">>, TableName)
    end).

%% ===================================================================
%% add/4 测试
%% ===================================================================

add_with_valid_data_returns_ok_test_() ->
    ?WITH_MECKS([
        {elib_tsid, [
            {'generate', 1, fun(msg_reaction) -> 100001 end}
        ]},
        {elib_pg, [
            {'query', 2, fun(_Sql, _Params) -> {ok, 1} end}
        ]}
    ], fun() ->
        ok = msg_reaction_repo:add(<<"msg_001">>, <<"c2c">>, 999999, <<"👍"/utf8>>)
    end).

add_with_empty_emoji_returns_error_test_() ->
    ?TEST_SIMPLE(fun() ->
        {error, empty_emoji} = msg_reaction_repo:add(<<"msg_001">>, <<"c2c">>, 999999, <<>>)
    end).

add_with_duplicate_returns_ok_test_() ->
    ?WITH_MECKS([
        {elib_tsid, [
            {'generate', 1, fun(msg_reaction) -> 100002 end}
        ]},
        {elib_pg, [
            {'query', 2, fun(_Sql, _Params) -> {ok, 0} end}
        ]}
    ], fun() ->
        % ON CONFLICT DO NOTHING 返回 {ok, 0} 但 repo 仍返回 ok
        ok = msg_reaction_repo:add(<<"msg_002">>, <<"c2c">>, 999999, <<"❤️"/utf8>>)
    end).

%% ===================================================================
%% remove/4 测试
%% ===================================================================

remove_existing_reaction_returns_ok_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) -> {ok, 1} end}
    ], fun() ->
        ok = msg_reaction_repo:remove(<<"msg_003">>, <<"c2c">>, 999999, <<"😄"/utf8>>)
    end).

remove_non_existent_reaction_returns_ok_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) -> {ok, 0} end}
    ], fun() ->
        ok = msg_reaction_repo:remove(<<"msg_003">>, <<"c2c">>, 999999, <<"👍"/utf8>>)
    end).

%% ===================================================================
%% find_by_msg/2 测试
%% ===================================================================

find_by_msg_returns_reactions_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            {ok, [
                #{<<"msg_id">> => <<"msg_004">>, <<"msg_type">> => <<"c2g">>,
                  <<"user_id">> => 999999, <<"emoji">> => <<"👍"/utf8>>},
                #{<<"msg_id">> => <<"msg_004">>, <<"msg_type">> => <<"c2g">>,
                  <<"user_id">> => 999998, <<"emoji">> => <<"❤️"/utf8>>},
                #{<<"msg_id">> => <<"msg_004">>, <<"msg_type">> => <<"c2g">>,
                  <<"user_id">> => 999999, <<"emoji">> => <<"😄"/utf8>>}
            ]}
        end}
    ], fun() ->
        {ok, Reactions} = msg_reaction_repo:find_by_msg(<<"msg_004">>, <<"c2g">>),
        ?assertEqual(3, length(Reactions))
    end).

find_by_msg_with_empty_result_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) -> {ok, []} end}
    ], fun() ->
        {ok, []} = msg_reaction_repo:find_by_msg(<<"non_existent">>, <<"c2c">>)
    end).

%% ===================================================================
%% find_by_msg_emoji/3 测试
%% ===================================================================

find_by_msg_emoji_filters_by_emoji_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            {ok, [
                #{<<"msg_id">> => <<"msg_005">>, <<"msg_type">> => <<"c2c">>,
                  <<"user_id">> => 999999, <<"emoji">> => <<"👍"/utf8>>},
                #{<<"msg_id">> => <<"msg_005">>, <<"msg_type">> => <<"c2c">>,
                  <<"user_id">> => 999998, <<"emoji">> => <<"👍"/utf8>>}
            ]}
        end}
    ], fun() ->
        {ok, Reactions} = msg_reaction_repo:find_by_msg_emoji(<<"msg_005">>, <<"c2c">>, <<"👍"/utf8>>),
        ?assertEqual(2, length(Reactions))
    end).

%% ===================================================================
%% count_by_msg/2 测试
%% ===================================================================

count_by_msg_returns_total_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            {ok, [#{<<"count">> => 3}]}
        end}
    ], fun() ->
        Count = msg_reaction_repo:count_by_msg(<<"msg_006">>, <<"c2g">>),
        ?assertEqual(3, Count)
    end).

count_by_msg_with_no_results_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) -> {ok, []} end}
    ], fun() ->
        Count = msg_reaction_repo:count_by_msg(<<"non_existent">>, <<"c2c">>),
        ?assertEqual(0, Count)
    end).

%% ===================================================================
%% count_by_emoji/3 测试
%% ===================================================================

count_by_emoji_returns_specific_count_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            {ok, [#{<<"count">> => 2}]}
        end}
    ], fun() ->
        Count = msg_reaction_repo:count_by_emoji(<<"msg_007">>, <<"c2c">>, <<"👍"/utf8>>),
        ?assertEqual(2, Count)
    end).

%% ===================================================================
%% find_user_reactions/3 测试
%% ===================================================================

find_user_reactions_returns_paginated_results_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            {ok, [
                #{<<"msg_id">> => <<"msg_008_1">>, <<"msg_type">> => <<"c2c">>,
                  <<"user_id">> => 999999, <<"emoji">> => <<"👍"/utf8>>},
                #{<<"msg_id">> => <<"msg_008_2">>, <<"msg_type">> => <<"c2c">>,
                  <<"user_id">> => 999999, <<"emoji">> => <<"❤️"/utf8>>}
            ]}
        end}
    ], fun() ->
        {ok, Reactions} = msg_reaction_repo:find_user_reactions(999999, 1, 10),
        ?assertEqual(2, length(Reactions))
    end).

find_user_reactions_with_invalid_params_test_() ->
    ?TEST_SIMPLE(fun() ->
        {error, invalid_params} = msg_reaction_repo:find_user_reactions(999999, 0, 10)
    end).

%% ===================================================================
%% remove_all_by_msg/2 测试
%% ===================================================================

remove_all_by_msg_returns_ok_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) -> {ok, 3} end}
    ], fun() ->
        ok = msg_reaction_repo:remove_all_by_msg(<<"msg_009">>, <<"c2g">>)
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

add_with_database_error_returns_error_test_() ->
    ?WITH_MECKS([
        {elib_tsid, [
            {'generate', 1, fun(msg_reaction) -> 100003 end}
        ]},
        {elib_pg, [
            {'query', 2, fun(_Sql, _Params) -> {error, db_error} end}
        ]}
    ], fun() ->
        {error, db_error} = msg_reaction_repo:add(<<"msg_edge">>, <<"c2c">>, 999999, <<"👍"/utf8>>)
    end).

find_by_msg_with_database_error_returns_error_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) -> {error, db_error} end}
    ], fun() ->
        {error, db_error} = msg_reaction_repo:find_by_msg(<<"msg_err">>, <<"c2c">>)
    end).
