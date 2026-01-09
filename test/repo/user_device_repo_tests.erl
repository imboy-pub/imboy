-module(user_device_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% user_device_repo 模块的 EUnit 测试
%%%
%%% 目标：验证 imboy_db → imboy_pg 迁移的语义正确性
%%%===================================================================

%% ===================================================================
%% 测试装置（仅 tablename 测试使用）
%% ===================================================================

setup_config() ->
    % 设置测试环境配置，确保 sql_driver 返回 pgsql
    application:set_env(imboy, sql_driver, pgsql).

cleanup_config(_) ->
    % 清理测试环境配置
    application:unset_env(imboy, sql_driver).

%% ===================================================================
%% tablename/0 测试
%% ===================================================================

tablename_public_prefix_test_() ->
    {setup,
     fun setup_config/0,
     fun cleanup_config/1,
     ?_test(begin
         Result = user_device_repo:tablename(),
         ?assertEqual(<<"public.user_device">>, Result)
     end)}.

%% ===================================================================
%% page/3 测试
%% ===================================================================

page_basic_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 1,
        Limit = 10,
        Offset = 0,
        Result = user_device_repo:page(Uid, Limit, Offset),
        ?assertMatch({ok, {_, [_|_]}}, Result)
    end).

page_empty_result_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 999999,
        Limit = 10,
        Offset = 0,
        Result = user_device_repo:page(Uid, Limit, Offset),
        ?assertMatch({ok, {_, []}}, Result)
    end).

page_large_offset_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 1,
        Limit = 10,
        Offset = 1000,
        Result = user_device_repo:page(Uid, Limit, Offset),
        ?assertMatch({ok, {_, []}}, Result)
    end).

%% ===================================================================
%% count_by_uid/1 测试
%% ===================================================================

count_by_uid_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 1,
        Result = user_device_repo:count_by_uid(Uid),
        ?assert(is_integer(Result)),
        ?assert(Result >= 0)
    end).

count_by_uid_non_existing_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 999999,
        Result = user_device_repo:count_by_uid(Uid),
        ?assertEqual(0, Result)
    end).

%% ===================================================================
%% device_name/2 测试
%% ===================================================================

device_name_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 1,
        DID = <<"test_device_id">>,
        Result = user_device_repo:device_name(Uid, DID),
        ?assertMatch(<<_/binary>>, Result)
    end).

device_name_non_existing_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 999999,
        DID = <<"nonexistent_device">>,
        Result = user_device_repo:device_name(Uid, DID),
        ?assertEqual(<<>>, Result)
    end).

%% ===================================================================
%% login_count/2 测试
%% ===================================================================

login_count_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 1,
        DID = <<"test_device_id">>,
        Result = user_device_repo:login_count(Uid, DID),
        ?assert(is_integer(Result)),
        ?assert(Result >= 0)
    end).

login_count_non_existing_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 999999,
        DID = <<"nonexistent_device">>,
        Result = user_device_repo:login_count(Uid, DID),
        ?assertEqual(0, Result)
    end).

%% ===================================================================
%% delete/2 测试
%% ===================================================================

delete_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 999999,
        DID = <<"test_delete_device">>,
        Result = user_device_repo:delete(Uid, DID),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% update_by_did/4 测试
%% ===================================================================

update_by_did_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 1,
        DID = <<"test_device">>,
        Set = <<"device_name = $1">>,
        SetArgs = [<<"New Device Name">>],
        Result = user_device_repo:update_by_did(Uid, DID, Set, SetArgs),
        ?assertMatch({ok, UpdatedCount} when is_integer(UpdatedCount), Result)
    end).

update_by_did_no_match_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 999999,
        DID = <<"nonexistent_device">>,
        Set = <<"device_name = $1">>,
        SetArgs = [<<"Test">>],
        Result = user_device_repo:update_by_did(Uid, DID, Set, SetArgs),
        ?assertMatch({ok, UpdatedCount} when is_integer(UpdatedCount), Result)
    end).

%% ===================================================================
%% save/4 测试
%% ===================================================================

save_new_device_test_() ->
    ?TEST_WITH_DB(fun() ->
        Now = imboy_dt:now(),
        Uid = 999999,
        DID = <<"test_save_device">>,
        PostVals = [
            {<<"cos">>, <<"ios">>},
            {<<"dvsn">>, <<"1.0">>},
            {<<"dname">>, <<"Test Device">>},
            {<<"public_key">>, <<"test_key">>},
            {<<"ip">>, <<"127.0.0.1">>}
        ],
        Result = user_device_repo:save(Now, Uid, DID, PostVals),
        ?assertEqual(ok, Result)
    end).

save_existing_device_test_() ->
    ?TEST_WITH_DB(fun() ->
        Now = imboy_dt:now(),
        Uid = 1,
        DID = <<"test_existing_device">>,
        PostVals = [
            {<<"ip">>, <<"192.168.1.1">>},
            {<<"public_key">>, <<"updated_key">>}
        ],
        Result = user_device_repo:save(Now, Uid, DID, PostVals),
        ?assertEqual(ok, Result)
    end).

save_empty_did_test_() ->
    ?TEST_WITH_DB(fun() ->
        Now = imboy_dt:now(),
        Uid = 1,
        DID = <<>>,
        PostVals = [],
        Result = user_device_repo:save(Now, Uid, DID, PostVals),
        % 空 DID 应该返回 ok
        ?assertEqual(ok, Result)
    end).
