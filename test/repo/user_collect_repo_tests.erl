-module(user_collect_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% user_collect_repo 模块的 EUnit 测试
%%%
%%% 目标：验证用户收藏数据访问层功能
%%% 覆盖：收藏查询、添加、删除
%%%===================================================================

%% ===================================================================
%% tablename/0 测试
%% ===================================================================

tablename_returns_correct_table_test_() ->
    ?WITH_MECK(elib_pg_sql, [
        {'public_tablename', 1, fun(_Table) -> <<"public.user_collect">> end}
    ], fun() ->
        Result = user_collect_repo:tablename(),
        ?assertEqual(<<"public.user_collect">>, Result)
    end).

%% ===================================================================
%% count_by_uid_kind_id/2 测试
%% ===================================================================

count_by_uid_kind_id_with_existing_record_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            {ok, [#{<<"count">> => 1}]}
        end}
    ], fun() ->
        Uid = 12345,
        KindId = <<"msg123">>,
        
        Result = user_collect_repo:count_by_uid_kind_id(Uid, KindId),
        ?assertEqual(1, Result)
    end).

count_by_uid_kind_id_no_existing_record_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            {ok, [#{<<"count">> => 0}]}
        end}
    ], fun() ->
        Uid = 12345,
        KindId = <<"msg456">>,
        
        Result = user_collect_repo:count_by_uid_kind_id(Uid, KindId),
        ?assertEqual(0, Result)
    end).

count_by_uid_kind_id_query_error_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            {error, database_error}
        end}
    ], fun() ->
        Uid = 12345,
        KindId = <<"msg789">>,
        
        Result = user_collect_repo:count_by_uid_kind_id(Uid, KindId),
        ?assertEqual(0, Result)
    end).

%% ===================================================================
%% delete/2 测试
%% ===================================================================

delete_collect_success_test_() ->
    ?WITH_MECK(elib_pg, [
        {'execute', 2, fun(_Sql, _Params) -> {ok, 1} end}
    ], fun() ->
        Uid = 12345,
        KindId = <<"msg123">>,
        
        Result = user_collect_repo:delete(Uid, KindId),
        ?assertMatch({ok, 1}, Result)
    end).

delete_collect_no_affected_rows_test_() ->
    ?WITH_MECK(elib_pg, [
        {'execute', 2, fun(_Sql, _Params) -> {ok, 0} end}
    ], fun() ->
        Uid = 12345,
        KindId = <<"msg456">>,
        
        Result = user_collect_repo:delete(Uid, KindId),
        ?assertMatch({ok, 0}, Result)
    end).

delete_collect_database_error_test_() ->
    ?WITH_MECK(elib_pg, [
        {'execute', 2, fun(_Sql, _Params) -> {error, database_error} end}
    ], fun() ->
        Uid = 12345,
        KindId = <<"msg789">>,

        Result = user_collect_repo:delete(Uid, KindId),
        case Result of
            {error, Reason} when is_atom(Reason); is_binary(Reason) ->
                ?assert(true);
            _ ->
                ?assert(false, "Expected {error, Reason}")
        end
    end).

%% ===================================================================
%% update/3 测试
%% ===================================================================

update_collect_success_test_() ->
    ?WITH_MECK(elib_pg, [
        {'update', 4, fun(_Table, _Data, _Where, _Params) -> {ok, 1} end}
    ], fun() ->
        Uid = 12345,
        KindId = <<"msg123">>,
        Data = [{<<"remark">>, <<"Updated remark">>}],
        
        Result = user_collect_repo:update(Uid, KindId, Data),
        ?assertMatch({ok, 1}, Result)
    end).

update_collect_with_multiple_fields_test_() ->
    ?WITH_MECK(elib_pg, [
        {'update', 4, fun(_Table, _Data, _Where, _Params) -> {ok, 1} end}
    ], fun() ->
        Uid = 12345,
        KindId = <<"msg123">>,
        Data = [
            {<<"remark">>, <<"Updated remark">>},
            {<<"updated_at">>, 1234567890}
        ],
        
        Result = user_collect_repo:update(Uid, KindId, Data),
        ?assertMatch({ok, 1}, Result)
    end).

update_collect_no_affected_rows_test_() ->
    ?WITH_MECK(elib_pg, [
        {'update', 4, fun(_Table, _Data, _Where, _Params) -> {ok, 0} end}
    ], fun() ->
        Uid = 12345,
        KindId = <<"msg456">>,
        Data = [{<<"remark">>, <<"Updated remark">>}],
        
        Result = user_collect_repo:update(Uid, KindId, Data),
        ?assertMatch({ok, 0}, Result)
    end).
