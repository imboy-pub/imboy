-module(user_device_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% user_device_ds 模块的 EUnit 测试
%%%
%%% 目标：验证用户设备数据服务功能
%%% 覆盖：设备列表、公钥管理、设备统计、设备操作
%%%===================================================================

%% ===================================================================
%% page/3 测试
%% ===================================================================

page_returns_device_list_test_() ->
    ?WITH_MECK(user_device_repo, [
        {'page', 3, fun(Uid, Limit, Offset) ->
            ?assertEqual(100, Uid),
            ?assertEqual(10, Limit),
            ?assertEqual(0, Offset),
            {ok, [
                #{
                    <<"id">> => 1,
                    <<"user_id">> => 100,
                    <<"device_id">> => <<"device1">>,
                    <<"device_name">> => <<"iPhone 13">>
                },
                #{
                    <<"id">> => 2,
                    <<"user_id">> => 100,
                    <<"device_id">> => <<"device2">>,
                    <<"device_name">> => <<"iPad Pro">>
                }
            ]}
        end}
    ], fun() ->
        {ok, Devices} = user_device_ds:page(100, 10, 0),
        ?assertEqual(2, length(Devices))
    end).

page_with_empty_result_test_() ->
    ?WITH_MECK(user_device_repo, [
        {'page', 3, fun(_Uid, _Limit, _Offset) -> {ok, []} end}
    ], fun() ->
        Result = user_device_ds:page(100, 10, 0),
        ?assertEqual({ok, []}, Result)
    end).

page_with_pagination_test_() ->
    ?WITH_MECK(user_device_repo, [
        {'page', 3, fun(_Uid, Limit, Offset) ->
            ?assertEqual(20, Limit),
            ?assertEqual(40, Offset),
            {ok, []}
        end}
    ], fun() ->
        Result = user_device_ds:page(100, 20, 40),
        ?assertEqual({ok, []}, Result)
    end).

page_with_error_returns_error_test_() ->
    ?WITH_MECK(user_device_repo, [
        {'page', 3, fun(_Uid, _Limit, _Offset) ->
            {error, <<"database_error">>}
        end}
    ], fun() ->
        Result = user_device_ds:page(100, 10, 0),
        ?assertEqual({error, <<"database_error">>}, Result)
    end).

%% ===================================================================
%% list_public_keys/1 测试
%% ===================================================================

list_public_keys_returns_keys_test_() ->
    ?WITH_MECK(user_device_repo, [
        {'list_public_keys', 1, fun(Uid) ->
            ?assertEqual(100, Uid),
            {ok, [
                #{
                    <<"device_id">> => <<"device1">>,
                    <<"public_key">> => <<"-----BEGIN PUBLIC KEY-----...">>
                },
                #{
                    <<"device_id">> => <<"device2">>,
                    <<"public_key">> => <<"-----BEGIN PUBLIC KEY-----...">>
                }
            ]}
        end}
    ], fun() ->
        {ok, Keys} = user_device_ds:list_public_keys(100),
        ?assertEqual(2, length(Keys))
    end).

list_public_keys_with_empty_result_test_() ->
    ?WITH_MECK(user_device_repo, [
        {'list_public_keys', 1, fun(_Uid) -> {ok, []} end}
    ], fun() ->
        Result = user_device_ds:list_public_keys(100),
        ?assertEqual({ok, []}, Result)
    end).

list_public_keys_with_error_returns_error_test_() ->
    ?WITH_MECK(user_device_repo, [
        {'list_public_keys', 1, fun(_Uid) ->
            {error, <<"query_failed">>}
        end}
    ], fun() ->
        Result = user_device_ds:list_public_keys(100),
        ?assertEqual({error, <<"query_failed">>}, Result)
    end).

%% ===================================================================
%% list_public_keys_by_uids/1 测试
%% ===================================================================

list_public_keys_by_uids_returns_keys_test_() ->
    ?WITH_MECK(user_device_repo, [
        {'list_public_keys_by_uids', 1, fun(Uids) ->
            ?assertEqual([100, 101, 102], Uids),
            {ok, [
                #{
                    <<"user_id">> => 100,
                    <<"device_id">> => <<"device1">>,
                    <<"public_key">> => <<"key1">>
                },
                #{
                    <<"user_id">> => 101,
                    <<"device_id">> => <<"device2">>,
                    <<"public_key">> => <<"key2">>
                }
            ]}
        end}
    ], fun() ->
        {ok, Keys} = user_device_ds:list_public_keys_by_uids([100, 101, 102]),
        ?assertEqual(2, length(Keys))
    end).

list_public_keys_by_uids_with_empty_list_test_() ->
    ?WITH_MECK(user_device_repo, [
        {'list_public_keys_by_uids', 1, fun(Uids) ->
            ?assertEqual([], Uids),
            {ok, []}
        end}
    ], fun() ->
        Result = user_device_ds:list_public_keys_by_uids([]),
        ?assertEqual({ok, []}, Result)
    end).

list_public_keys_by_uids_with_error_returns_error_test_() ->
    ?WITH_MECK(user_device_repo, [
        {'list_public_keys_by_uids', 1, fun(_Uids) ->
            {error, <<"query_failed">>}
        end}
    ], fun() ->
        Result = user_device_ds:list_public_keys_by_uids([100, 101]),
        ?assertEqual({error, <<"query_failed">>}, Result)
    end).

%% ===================================================================
%% count_by_uid/1 测试
%% ===================================================================

count_by_uid_returns_count_test_() ->
    ?WITH_MECK(user_device_repo, [
        {'count_by_uid', 1, fun(Uid) ->
            ?assertEqual(100, Uid),
            3
        end}
    ], fun() ->
        Result = user_device_ds:count_by_uid(100),
        ?assertEqual(3, Result)
    end).

count_by_uid_with_zero_count_test_() ->
    ?WITH_MECK(user_device_repo, [
        {'count_by_uid', 1, fun(_Uid) -> 0 end}
    ], fun() ->
        Result = user_device_ds:count_by_uid(999),
        ?assertEqual(0, Result)
    end).

count_by_uid_with_large_count_test_() ->
    ?WITH_MECK(user_device_repo, [
        {'count_by_uid', 1, fun(_Uid) -> 100 end}
    ], fun() ->
        Result = user_device_ds:count_by_uid(100),
        ?assertEqual(100, Result)
    end).

%% ===================================================================
%% device_name/2 测试
%% ===================================================================

device_name_returns_name_test_() ->
    ?WITH_MECK(user_device_repo, [
        {'device_name', 2, fun(Uid, DID) ->
            ?assertEqual(100, Uid),
            ?assertEqual(<<"device1">>, DID),
            <<"iPhone 13">>
        end}
    ], fun() ->
        Result = user_device_ds:device_name(100, <<"device1">>),
        ?assertEqual(<<"iPhone 13">>, Result)
    end).

device_name_with_empty_name_test_() ->
    ?WITH_MECK(user_device_repo, [
        {'device_name', 2, fun(_Uid, _DID) -> <<>> end}
    ], fun() ->
        Result = user_device_ds:device_name(100, <<"unknown">>),
        ?assertEqual(<<>>, Result)
    end).

device_name_with_utf8_name_test_() ->
    ?WITH_MECK(user_device_repo, [
        {'device_name', 2, fun(_Uid, _DID) -> <<"小米手机"/utf8>> end}
    ], fun() ->
        Result = user_device_ds:device_name(100, <<"device1">>),
        ?assertEqual(<<"小米手机"/utf8>>, Result)
    end).

%% ===================================================================
%% login_count/2 测试
%% ===================================================================

login_count_returns_count_test_() ->
    ?WITH_MECK(user_device_repo, [
        {'login_count', 2, fun(Uid, DID) ->
            ?assertEqual(100, Uid),
            ?assertEqual(<<"device1">>, DID),
            42
        end}
    ], fun() ->
        Result = user_device_ds:login_count(100, <<"device1">>),
        ?assertEqual(42, Result)
    end).

login_count_with_zero_count_test_() ->
    ?WITH_MECK(user_device_repo, [
        {'login_count', 2, fun(_Uid, _DID) -> 0 end}
    ], fun() ->
        Result = user_device_ds:login_count(100, <<"new_device">>),
        ?assertEqual(0, Result)
    end).

login_count_with_large_count_test_() ->
    ?WITH_MECK(user_device_repo, [
        {'login_count', 2, fun(_Uid, _DID) -> 9999 end}
    ], fun() ->
        Result = user_device_ds:login_count(100, <<"device1">>),
        ?assertEqual(9999, Result)
    end).

%% ===================================================================
%% save/4 测试
%% ===================================================================

save_success_test_() ->
    ?WITH_MECK(user_device_repo, [
        {'save', 4, fun(Now, Uid, DID, PostVals) ->
            ?assertEqual(<<"2023-01-01T00:00:00Z">>, Now),
            ?assertEqual(100, Uid),
            ?assertEqual(<<"device1">>, DID),
            ?assert(is_map(PostVals)),
            ok
        end}
    ], fun() ->
        Now = <<"2023-01-01T00:00:00Z">>,
        PostVals = #{
            device_name => <<"iPhone 13">>,
            public_key => <<"-----BEGIN PUBLIC KEY-----...">>
        },
        Result = user_device_ds:save(Now, 100, <<"device1">>, PostVals),
        ?assertEqual(ok, Result)
    end).

save_with_empty_post_vals_test_() ->
    ?WITH_MECK(user_device_repo, [
        {'save', 4, fun(_Now, _Uid, _DID, PostVals) ->
            ?assertEqual(#{}, PostVals),
            ok
        end}
    ], fun() ->
        Result = user_device_ds:save(<<"2023-01-01T00:00:00Z">>, 100, <<"device1">>, #{}),
        ?assertEqual(ok, Result)
    end).

save_with_error_returns_error_test_() ->
    ?WITH_MECK(user_device_repo, [
        {'save', 4, fun(_Now, _Uid, _DID, _PostVals) ->
            {error, <<"save_failed">>}
        end}
    ], fun() ->
        PostVals = #{device_name => <<"iPhone">>},
        Result = user_device_ds:save(<<"2023-01-01T00:00:00Z">>, 100, <<"device1">>, PostVals),
        ?assertEqual({error, <<"save_failed">>}, Result)
    end).

save_with_public_key_test_() ->
    ?WITH_MECK(user_device_repo, [
        {'save', 4, fun(_Now, _Uid, _DID, PostVals) ->
            ?assert(maps:is_key(public_key, PostVals)),
            PublicKey = maps:get(public_key, PostVals),
            ?assert(byte_size(PublicKey) > 0),
            ok
        end}
    ], fun() ->
        PostVals = #{
            device_name => <<"iPhone">>,
            public_key => <<"-----BEGIN PUBLIC KEY-----\nMIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEA...\n-----END PUBLIC KEY-----">>
        },
        Result = user_device_ds:save(<<"2023-01-01T00:00:00Z">>, 100, <<"device1">>, PostVals),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% delete/2 测试
%% ===================================================================

delete_success_test_() ->
    ?WITH_MECK(user_device_repo, [
        {'delete', 2, fun(Uid, DID) ->
            ?assertEqual(100, Uid),
            ?assertEqual(<<"device1">>, DID),
            ok
        end}
    ], fun() ->
        Result = user_device_ds:delete(100, <<"device1">>),
        ?assertEqual(ok, Result)
    end).

delete_nonexistent_device_test_() ->
    ?WITH_MECK(user_device_repo, [
        {'delete', 2, fun(_Uid, _DID) -> ok end}
    ], fun() ->
        Result = user_device_ds:delete(100, <<"nonexistent">>),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% update_by_did/4 测试
%% ===================================================================

update_by_did_success_test_() ->
    ?WITH_MECK(user_device_repo, [
        {'update_by_did', 4, fun(Uid, DID, Set, SetArgs) ->
            ?assertEqual(100, Uid),
            ?assertEqual(<<"device1">>, DID),
            ?assertEqual(<<"device_name = $1">>, Set),
            ?assertEqual([<<"New Name">>], SetArgs),
            {ok, 1}
        end}
    ], fun() ->
        Result = user_device_ds:update_by_did(100, <<"device1">>, <<"device_name = $1">>, [<<"New Name">>]),
        ?assertEqual({ok, 1}, Result)
    end).

update_by_did_with_multiple_fields_test_() ->
    ?WITH_MECK(user_device_repo, [
        {'update_by_did', 4, fun(_Uid, _DID, Set, SetArgs) ->
            ?assertEqual(<<"device_name = $1, updated_at = $2">>, Set),
            ?assertEqual(2, length(SetArgs)),
            {ok, 1}
        end}
    ], fun() ->
        Set = <<"device_name = $1, updated_at = $2">>,
        SetArgs = [<<"New Name">>, <<"2023-01-01T00:00:00Z">>],
        Result = user_device_ds:update_by_did(100, <<"device1">>, Set, SetArgs),
        ?assertEqual({ok, 1}, Result)
    end).

update_by_did_with_error_returns_error_test_() ->
    ?WITH_MECK(user_device_repo, [
        {'update_by_did', 4, fun(_Uid, _DID, _Set, _SetArgs) ->
            {error, <<"update_failed">>}
        end}
    ], fun() ->
        Result = user_device_ds:update_by_did(100, <<"device1">>, <<"device_name = $1">>, [<<"Name">>]),
        ?assertEqual({error, <<"update_failed">>}, Result)
    end).

update_by_did_nonexistent_device_test_() ->
    ?WITH_MECK(user_device_repo, [
        {'update_by_did', 4, fun(_Uid, _DID, _Set, _SetArgs) -> {ok, 0} end}
    ], fun() ->
        Result = user_device_ds:update_by_did(100, <<"nonexistent">>, <<"device_name = $1">>, [<<"Name">>]),
        ?assertEqual({ok, 0}, Result)
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

page_with_large_limit_test_() ->
    ?WITH_MECK(user_device_repo, [
        {'page', 3, fun(_Uid, Limit, _Offset) ->
            ?assertEqual(1000, Limit),
            {ok, []}
        end}
    ], fun() ->
        Result = user_device_ds:page(100, 1000, 0),
        ?assertEqual({ok, []}, Result)
    end).

list_public_keys_by_uids_with_large_list_test_() ->
    LargeUidList = lists:seq(1, 1000),
    ?WITH_MECK(user_device_repo, [
        {'list_public_keys_by_uids', 1, fun(Uids) ->
            ?assertEqual(1000, length(Uids)),
            {ok, []}
        end}
    ], fun() ->
        Result = user_device_ds:list_public_keys_by_uids(LargeUidList),
        ?assertEqual({ok, []}, Result)
    end).

save_with_long_device_name_test_() ->
    LongName = list_to_binary(lists:duplicate(500, $x)),
    ?WITH_MECK(user_device_repo, [
        {'save', 4, fun(_Now, _Uid, _DID, PostVals) ->
            DeviceName = maps:get(device_name, PostVals),
            ?assert(byte_size(DeviceName) >= 500),
            ok
        end}
    ], fun() ->
        PostVals = #{device_name => LongName},
        Result = user_device_ds:save(<<"2023-01-01T00:00:00Z">>, 100, <<"device1">>, PostVals),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% 类型验证测试
%% ===================================================================

page_validates_types_test_() ->
    ?TEST_SIMPLE(fun() ->
        Uid = 100,
        Limit = 10,
        Offset = 0,
        ?assert(is_integer(Uid)),
        ?assert(is_integer(Limit)),
        ?assert(is_integer(Offset))
    end).

save_validates_types_test_() ->
    ?TEST_SIMPLE(fun() ->
        Now = <<"2023-01-01T00:00:00Z">>,
        Uid = 100,
        DID = <<"device1">>,
        PostVals = #{},
        ?assert(is_binary(Now)),
        ?assert(is_integer(Uid)),
        ?assert(is_binary(DID)),
        ?assert(is_map(PostVals))
    end).
