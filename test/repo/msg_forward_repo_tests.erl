-module(msg_forward_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% msg_forward_repo 模块的 EUnit 测试
%%%
%%% 目标：验证消息转发记录数据仓库功能
%%% 覆盖：插入、查询、根据原始消息ID查询、根据转发者查询
%%%===================================================================

%% ===================================================================
%% tablename/0 测试
%% ===================================================================

tablename_returns_valid_table_name_test_() ->
    ?TEST_SIMPLE(fun() ->
        TableName = msg_forward_repo:tablename(),
        ?assertMatch(<<"msg_forward">>, TableName)
    end).

%% ===================================================================
%% insert/1 测试
%% ===================================================================

insert_with_valid_data_succeeds_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) -> {ok, 1} end}
    ], fun() ->
        ForwardRecord = #{
            original_msg_id => <<"msg_123">>,
            original_from_id => 123,
            original_to_id => 456,
            original_type => <<"c2c">>,
            forward_msg_id => <<"msg_456">>,
            forward_from_id => 123,
            forward_to_id => 789,
            forward_type => <<"c2c">>
        },

        Result = msg_forward_repo:insert(ForwardRecord),
        ?assertMatch({ok, _}, Result)
    end).

insert_with_database_error_fails_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) -> {error, database_error} end}
    ], fun() ->
        ForwardRecord = #{
            original_msg_id => <<"msg_123">>,
            original_from_id => 123,
            original_to_id => 456,
            original_type => <<"c2c">>,
            forward_msg_id => <<"msg_456">>,
            forward_from_id => 123,
            forward_to_id => 789,
            forward_type => <<"c2c">>
        },

        Result = msg_forward_repo:insert(ForwardRecord),
        ?assertMatch({error, _}, Result)
    end).

%% ===================================================================
%% find_by_original_msg_id/1 测试
%% ===================================================================

find_by_original_msg_id_with_valid_msg_id_returns_records_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            {ok, [#{<<"original_msg_id">> => <<"msg_123">>,
                   <<"original_from_id">> => 123,
                   <<"original_to_id">> => 456,
                   <<"original_type">> => <<"c2c">>,
                   <<"forward_msg_id">> => <<"msg_456">>,
                   <<"forward_from_id">> => 123,
                   <<"forward_to_id">> => 789,
                   <<"forward_type">> => <<"c2c">>}]}
        end}
    ], fun() ->
        MsgId = <<"msg_123">>,

        Result = msg_forward_repo:find_by_original_msg_id(MsgId),
        ?assertMatch({ok, [_]}, Result)
    end).

find_by_original_msg_id_with_non_existent_msg_id_returns_empty_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) -> {ok, []} end}
    ], fun() ->
        MsgId = <<"non_existent_msg">>,

        Result = msg_forward_repo:find_by_original_msg_id(MsgId),
        ?assertMatch({ok, []}, Result)
    end).

find_by_original_msg_id_with_database_error_fails_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) -> {error, database_error} end}
    ], fun() ->
        MsgId = <<"msg_123">>,

        Result = msg_forward_repo:find_by_original_msg_id(MsgId),
        ?assertMatch({error, _}, Result)
    end).

%% ===================================================================
%% find_by_forward_from_id/2 测试
%% ===================================================================

find_by_forward_from_id_with_valid_uid_returns_records_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            {ok, [#{<<"original_msg_id">> => <<"msg_123">>,
                   <<"forward_msg_id">> => <<"msg_456">>}]}
        end}
    ], fun() ->
        FromUid = 123,
        Limit = 10,

        Result = msg_forward_repo:find_by_forward_from_id(FromUid, Limit),
        ?assertMatch({ok, [_]}, Result)
    end).

find_by_forward_from_id_with_limit_zero_returns_all_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            {ok, [#{<<"original_msg_id">> => <<"msg_123">>},
                  #{<<"original_msg_id">> => <<"msg_124">>}]}
        end}
    ], fun() ->
        FromUid = 123,
        Limit = 0,

        Result = msg_forward_repo:find_by_forward_from_id(FromUid, Limit),
        ?assertMatch({ok, [_, _]}, Result)
    end).

%% ===================================================================
%% count_by_original_msg_id/1 测试
%% ===================================================================

count_by_original_msg_id_with_valid_msg_id_returns_count_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            {ok, [#{<<"count">> => 5}]}
        end}
    ], fun() ->
        MsgId = <<"msg_123">>,

        Result = msg_forward_repo:count_by_original_msg_id(MsgId),
        ?assertEqual({ok, 5}, Result)
    end).

count_by_original_msg_id_with_non_existent_msg_id_returns_zero_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            {ok, [#{<<"count">> => 0}]}
        end}
    ], fun() ->
        MsgId = <<"non_existent_msg">>,

        Result = msg_forward_repo:count_by_original_msg_id(MsgId),
        ?assertEqual({ok, 0}, Result)
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

insert_with_missing_required_field_fails_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) -> {error, {constraint, <<"not_null_violation">>}} end}
    ], fun() ->
        % 缺少必需字段
        ForwardRecord = #{
            original_msg_id => <<"msg_123">>,
            original_from_id => 123
            % 缺少其他必需字段
        },

        Result = msg_forward_repo:insert(ForwardRecord),
        ?assertMatch({error, _}, Result)
    end).

find_by_original_msg_id_with_empty_msg_id_returns_empty_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) -> {ok, []} end}
    ], fun() ->
        MsgId = <<>>,

        Result = msg_forward_repo:find_by_original_msg_id(MsgId),
        ?assertMatch({ok, []}, Result)
    end).

%% ===================================================================
%% 数据类型验证测试
%% ===================================================================

insert_with_invalid_type_fails_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) -> {error, {invalid, <<"type_mismatch">>}} end}
    ], fun() ->
        ForwardRecord = #{
            original_msg_id => <<"msg_123">>,
            original_from_id => <<"not_an_integer">>,  % 错误的类型
            original_to_id => 456,
            original_type => <<"c2c">>,
            forward_msg_id => <<"msg_456">>,
            forward_from_id => 123,
            forward_to_id => 789,
            forward_type => <<"c2c">>
        },

        Result = msg_forward_repo:insert(ForwardRecord),
        ?assertMatch({error, _}, Result)
    end).
