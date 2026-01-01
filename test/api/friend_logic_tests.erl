-module(friend_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("include/eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% friend_logic 模块的 EUnit 测试
%%%
%%% 目标：验证好友业务逻辑功能
%%% 覆盖：添加好友、确认好友、删除好友
%%%===================================================================

%% ===================================================================
%% add_friend/4 测试
%% ===================================================================

add_friend_with_undefined_to_returns_error_test_() ->
    ?TEST_SIMPLE(fun() ->
        CurrentUid = 1,
        To = undefined,
        Payload = [{<<"msg">>, <<"test">>}],
        CreatedAt = 1234567890,
        Result = friend_logic:add_friend(CurrentUid, To, Payload, CreatedAt),
        ?assertEqual({error, <<"Parameter error">>, <<"to">>}, Result)
    end).

add_friend_with_undefined_payload_returns_error_test_() ->
    ?TEST_SIMPLE(fun() ->
        CurrentUid = 1,
        To = <<"test_to_2">>,
        Payload = undefined,
        CreatedAt = 1234567890,
        Result = friend_logic:add_friend(CurrentUid, To, Payload, CreatedAt),
        ?assertEqual({error, <<"Parameter error">>, <<"payload">>}, Result)
    end).

add_friend_with_undefined_created_at_returns_error_test_() ->
    ?TEST_SIMPLE(fun() ->
        CurrentUid = 1,
        To = <<"test_to_2">>,
        Payload = [{<<"msg">>, <<"test">>}],
        CreatedAt = undefined,
        Result = friend_logic:add_friend(CurrentUid, To, Payload, CreatedAt),
        ?assertEqual({error, <<"Parameter error">>, <<"created_at">>}, Result)
    end).

%% ===================================================================
%% add_friend/4 成功路径测试 (使用meck模拟依赖)
%% ===================================================================

add_friend_success_test_() ->
    ?WITH_MECKS([
        {imboy_hashids, [
            {'decode_hex', 1, fun(_Hex) -> {ok, 12345} end}
        ]},
        {friend_ds, [
            {'add', 4, fun(_FromUid, _ToUid, _Category, _Payload) -> {ok, 1} end}
        ]},
        {imboy_syn, [
            {'publish', 2, fun(_Topic, _Msg) -> ok end}
        ]}
    ], fun() ->
        CurrentUid = 1,
        From = <<"test_from_2">>,
        To = <<"test_to_3">>,
        Payload = <<"{}">>,
        Result = friend_logic:add_friend(CurrentUid, From, To, Payload),
        ?ASSERT_OK(Result)
    end).

%% ===================================================================
%% confirm_friend/4 测试
%% ===================================================================

confirm_friend_with_undefined_from_returns_error_test_() ->
    ?TEST_SIMPLE(fun() ->
        CurrentUid = 1,
        From = undefined,
        To = <<"test_to_2">>,
        Payload = <<"{}">>,
        Result = friend_logic:confirm_friend(CurrentUid, From, To, Payload),
        ?assertEqual({error, <<"Parameter error">>, <<"from">>}, Result)
    end).

confirm_friend_with_undefined_to_returns_error_test_() ->
    ?TEST_SIMPLE(fun() ->
        CurrentUid = 1,
        From = <<"test_from_2">>,
        To = undefined,
        Payload = <<"{}">>,
        Result = friend_logic:confirm_friend(CurrentUid, From, To, Payload),
        ?assertEqual({error, <<"Parameter error">>, <<"to">>}, Result)
    end).

confirm_friend_with_undefined_payload_returns_error_test_() ->
    ?TEST_SIMPLE(fun() ->
        CurrentUid = 1,
        From = <<"test_from_2">>,
        To = <<"test_to_2">>,
        Payload = undefined,
        Result = friend_logic:confirm_friend(CurrentUid, From, To, Payload),
        ?assertEqual({error, <<"Parameter error">>, <<"payload">>}, Result)
    end).

%% ===================================================================
%% confirm_friend/4 成功路径测试 (使用meck模拟依赖)
%% ===================================================================

confirm_friend_success_test_() ->
    ?WITH_MECKS([
        {imboy_hashids, [
            {'decode_hex', 1, fun(_Hex) -> {ok, 12345} end}
        ]},
        {friend_ds, [
            {'update', 3, fun(_FromUid, _ToUid, _Status) -> {ok, 1} end}
        ]},
        {imboy_syn, [
            {'publish', 2, fun(_Topic, _Msg) -> ok end}
        ]}
    ], fun() ->
        CurrentUid = 1,
        From = <<"test_from_2">>,
        To = <<"test_to_2">>,
        Payload = <<"{}">>,
        Result = friend_logic:confirm_friend(CurrentUid, From, To, Payload),
        ?ASSERT_OK(Result)
    end).

%% ===================================================================
%% delete_friend/2 测试
%% ===================================================================

delete_friend_with_binary_uid_test_() ->
    ?WITH_MECKS([
        {imboy_hashids, [
            {'decode', 1, fun(<<"encoded_uid_2">>) -> 2 end}
        ]},
        {friend_repo, [
            {'delete', 2, fun(_CurrentUid, _TargetUid) -> ok end}
        ]},
        {user_tag_relation_repo, [
            {'delete', 3, fun(_Scene, _CurrentUid, _TargetUid) -> ok end}
        ]},
        {imboy_cache, [
            {'flush', 1, fun(_Key) -> ok end}
        ]}
    ], fun() ->
        CurrentUid = 1,
        Uid = <<"encoded_uid_2">>,
        
        Result = friend_logic:delete_friend(CurrentUid, Uid),
        ?assertEqual(ok, Result)
    end).

delete_friend_with_integer_uid_test_() ->
    ?WITH_MECKS([
        {friend_repo, [
            {'delete', 2, fun(_CurrentUid, _TargetUid) -> ok end}
        ]},
        {user_tag_relation_repo, [
            {'delete', 3, fun(_Scene, _CurrentUid, _TargetUid) -> ok end}
        ]},
        {imboy_cache, [
            {'flush', 1, fun(_Key) -> ok end}
        ]}
    ], fun() ->
        CurrentUid = 1,
        Uid = 2,
        
        Result = friend_logic:delete_friend(CurrentUid, Uid),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% move_to_category/3 测试
%% ===================================================================

move_to_category_success_test_() ->
    ?WITH_MECK(friend_repo, [
        {'move_to_category', 3, fun(_CurrentUid, _TargetUid, _CategoryId) -> ok end}
    ], fun() ->
        CurrentUid = 1,
        Uid = 2,
        CategoryId = 1,
        
        Result = friend_logic:move_to_category(CurrentUid, Uid, CategoryId),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% information/2 测试
%% ===================================================================

information_with_valid_params_test_() ->
    ?TEST_WITH_APP(fun() ->
        CurrentUid = 1,
        Uid = 2,
        
        Result = friend_logic:information(CurrentUid, Uid),
        % 验证函数调用不会崩溃
        ?assert(is_tuple(Result))
    end).

information_with_nonexistent_friend_test_() ->
    ?TEST_WITH_APP(fun() ->
        CurrentUid = 1,
        Uid = 999,
        
        Result = friend_logic:information(CurrentUid, Uid),
        % 验证函数调用不会崩溃
        ?assert(is_tuple(Result))
    end).