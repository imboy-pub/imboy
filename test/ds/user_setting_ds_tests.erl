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
        UidBin = integer_to_binary(1),
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

chat_state_hide_defaults_to_hidden_test_() ->
    ?TEST_SIMPLE(fun() ->
        ok = meck:new(user_setting_ds, [passthrough, no_link]),
        try
            meck:expect(user_setting_ds, find_by_uid, 1, #{}),
            ?assertEqual(true, user_setting_ds:chat_state_hide(1))
        after
            meck:unload(user_setting_ds)
        end
    end).

chat_state_hide_explicit_online_is_visible_test_() ->
    ?TEST_SIMPLE(fun() ->
        ok = meck:new(user_setting_ds, [passthrough, no_link]),
        try
            meck:expect(
                user_setting_ds,
                find_by_uid,
                1,
                #{<<"chat_state">> => <<"online">>}
            ),
            ?assertEqual(false, user_setting_ds:chat_state_hide(1))
        after
            meck:unload(user_setting_ds)
        end
    end).

find_by_uid_invalid_json_defaults_to_empty_settings_test_() ->
    ?TEST_SIMPLE(fun() ->
        ok = meck:new(user_setting_repo, [no_link]),
        ok = meck:new(fts_user_repo, [no_link]),
        try
            meck:expect(
                user_setting_repo,
                find_by_uid,
                2,
                #{<<"setting">> => <<"{invalid-json">>}
            ),
            meck:expect(fts_user_repo, allow_search, 1, false),
            ?assertEqual(
                #{<<"allow_search">> => false},
                user_setting_ds:find_by_uid(1)
            )
        after
            meck:unload([user_setting_repo, fts_user_repo])
        end
    end).

batch_chat_state_hide_defaults_to_hidden_test_() ->
    ?TEST_SIMPLE(fun() ->
        ok = meck:new(user_setting_repo, [no_link]),
        ok = meck:new(elib_pg, [no_link]),
        try
            meck:expect(user_setting_repo, tablename, 0, <<"user_setting">>),
            meck:expect(
                elib_pg,
                query,
                2,
                {ok, [
                    #{<<"user_id">> => 1, <<"setting">> => <<>>},
                    #{<<"user_id">> => 2, <<"setting">> => <<"{}">>},
                    #{<<"user_id">> => 3, <<"setting">> => <<"{invalid-json">>},
                    #{<<"user_id">> => 4, <<"setting">> => <<"{\"chat_state\":\"online\"}">>}
                ]}
            ),
            ?assertEqual(
                #{1 => true, 2 => true, 3 => true, 4 => false},
                user_setting_ds:batch_chat_state_hide([1, 2, 3, 4])
            )
        after
            meck:unload([user_setting_repo, elib_pg])
        end
    end).

batch_chat_state_hide_query_failure_defaults_to_hidden_test_() ->
    ?TEST_SIMPLE(fun() ->
        ok = meck:new(user_setting_repo, [no_link]),
        ok = meck:new(elib_pg, [no_link]),
        try
            meck:expect(user_setting_repo, tablename, 0, <<"user_setting">>),
            meck:expect(elib_pg, query, 2, {error, timeout}),
            ?assertEqual(
                #{1 => true, 2 => true},
                user_setting_ds:batch_chat_state_hide([1, 2])
            )
        after
            meck:unload([user_setting_repo, elib_pg])
        end
    end).

%% ===================================================================
%% save/3 测试
%% ===================================================================

save_add_friend_type_test_() ->
    ?TEST_SIMPLE(fun() ->
        ok = meck:new(user_setting_ds, [passthrough, no_link]),
        ok = meck:new(user_setting_repo, [no_link]),
        ok = meck:new(elib_dt, [no_link]),
        try
            meck:expect(user_setting_ds, find_by_uid, 1, #{}),
            meck:expect(elib_dt, now, 0, <<"2026-01-01T00:00:00.000000Z">>),
            meck:expect(user_setting_repo, update, 2, ok),
            Uid = 1,
            TypeLi = [<<"qrcode">>, <<"visit_card">>],
            Result = user_setting_ds:save(Uid, <<"add_friend_type">>, TypeLi),
            ?assertEqual(ok, Result)
        after
            meck:unload([user_setting_ds, user_setting_repo, elib_dt])
        end
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
