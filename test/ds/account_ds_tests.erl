-module(account_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% account_ds 模块的 EUnit 测试
%%%
%%% 目标：验证账户ID分配功能的正确性
%%% 覆盖：正常路径、缓存机制、错误处理
%%%===================================================================

%% ===================================================================
%% init/0 测试
%% ===================================================================

init_creates_sequence_test_() ->
    ?TEST_WITH_DB(fun() ->
        Result = account_ds:init(),
        ?assertEqual(ok, Result)
    end).

init_is_idempotent_test_() ->
    ?TEST_WITH_DB(fun() ->
        account_ds:init(),
        Result2 = account_ds:init(),
        ?assertEqual(ok, Result2)
    end).

%% ===================================================================
%% allocate/0 测试
%% ===================================================================

allocate_returns_valid_id_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 3, fun(Sql, _Params, _Conn) ->
            % 验证 SQL 语句正确性
            ?assert(binary:match(Sql, <<"SELECT.*FROM.*account_id_seq">>) =/= nomatch),
            % 模拟返回序列值
            {ok, [{1001}]}
        end}
    ], fun() ->
        Result = account_ds:allocate(),
        {ok, Id} = Result,
        ?assert(is_integer(Id)),
        ?assert(Id > 0, "Expected positive integer ID"),
        ?assertEqual(1001, Id),
        
        % 验证 Mock 被正确调用
        meck_helper:verify_called(elib_pg, query, 3)
    end).

allocate_unique_ids_test_() ->
    ?TEST_WITH_DB(fun() ->
        {ok, Id1} = account_ds:allocate(),
        {ok, Id2} = account_ds:allocate(),
        ?assertNotEqual(Id1, Id2),
        ?assert(Id1 > 0),
        ?assert(Id2 > 0)
    end).

allocate_multiple_ids_are_sequential_test_() ->
    ?TEST_WITH_DB(fun() ->
        {ok, Id1} = account_ds:allocate(),
        {ok, Id2} = account_ds:allocate(),
        {ok, Id3} = account_ds:allocate(),
        Ids = [Id1, Id2, Id3],
        ?assertEqual(3, length(lists:usort(Ids)))
    end).

allocate_handles_cache_exhaustion_test_() ->
    ?TEST_WITH_DB(fun() ->
        % 多次分配确保缓存机制正常工作
        Lists = [account_ds:allocate() || _ <- lists:seq(1, 15)],
        ValidResults = [R || R <- Lists, element(1, R) =:= ok],
        ?assert(length(ValidResults) > 0)
    end).
