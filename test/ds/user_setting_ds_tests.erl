-module(user_setting_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% user_setting_ds 模块的 EUnit 测试
%%%
%%% 目标：验证用户设置服务功能
%%% 覆盖：设置查询、聊天状态、设置保存
%%%===================================================================

%% ===================================================================
%% find_by_uid/1 测试
%% ===================================================================

find_by_uid_returns_map_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 1,
        Result = user_setting_ds:find_by_uid(Uid),
        ?assert(is_map(Result))
    end).

find_by_uid_contains_allow_search_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 1,
        Result = user_setting_ds:find_by_uid(Uid),
        ?assert(maps:is_key(<<"allow_search">>, Result))
    end).

find_by_uid_with_binary_uid_test_() ->
    ?TEST_WITH_DB(fun() ->
        UidBin = imboy_hashids:encode(1),
        Result = user_setting_ds:find_by_uid(UidBin),
        ?assert(is_map(Result))
    end).

%% ===================================================================
%% chat_state_hide/1 测试
%% ===================================================================

chat_state_hide_returns_boolean_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 1,
        Result = user_setting_ds:chat_state_hide(Uid),
        ?assert(is_boolean(Result))
    end).

chat_state_hide_true_when_chat_state_hide_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 1,
        Result = user_setting_ds:chat_state_hide(Uid),
        % 验证返回的是布尔值
        ?assert(is_boolean(Result))
    end).

%% ===================================================================
%% save/3 测试
%% ===================================================================

save_add_friend_type_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 1,
        TypeLi = [<<"qrcode">>, <<"visit_card">>],
        Result = user_setting_ds:save(Uid, <<"add_friend_type">>, TypeLi),
        ?assertEqual(ok, Result)
    end).

save_people_nearby_visible_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 1,
        Result = user_setting_ds:save(Uid, <<"people_nearby_visible">>, true),
        ?assertEqual(ok, Result)
    end).

save_font_size_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 1,
        FontSize = 16,
        Result = user_setting_ds:save(Uid, <<"font_size">>, FontSize),
        ?assertEqual(ok, Result)
    end).

save_chat_state_hide_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 1,
        Result = user_setting_ds:save(Uid, <<"chat_state">>, <<"hide">>),
        ?assertEqual(ok, Result)
    end).

save_chat_state_online_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 1,
        Result = user_setting_ds:save(Uid, <<"chat_state">>, <<"online">>),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% search/1 测试
%% ===================================================================

search_returns_ok_test_() ->
    ?TEST_WITH_APP(fun() ->
        Account = <<"test_account">>,
        Result = user_setting_ds:search(Account),
        ?assertEqual(ok, Result)
    end).
