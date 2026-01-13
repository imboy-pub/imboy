-module(user_setting_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% user_setting_repo 模块的 EUnit 测试
%%%
%%% 目标：验证用户设置数据访问层功能
%%% 覆盖：设置查询、更新
%%%===================================================================

%% ===================================================================
%% tablename/0 测试
%% ===================================================================

tablename_returns_correct_table_test_() ->
    ?WITH_MECK(elib_pg_sql, [
        {'public_tablename', 1, fun(_Table) -> <<"public.user_setting">> end}
    ], fun() ->
        Result = user_setting_repo:tablename(),
        ?assertEqual(<<"public.user_setting">>, Result)
    end).

%% ===================================================================
%% 设置查询测试
%% ===================================================================

find_settings_by_uid_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(Sql, Params) ->
            % 验证SQL查询包含用户设置查询
            ?assert(binary:match(Sql, <<"SELECT.*FROM.*user_setting">>) =/= nomatch),
            ?assert(binary:match(Sql, <<"WHERE.*user_id">>) =/= nomatch),
            % 验证参数包含用户ID
            ?assert(length(Params) >= 1),
            ?assert(lists:member(1, Params)),
            % 返回模拟的用户设置
            {ok, [{1, <<"notification">>, <<"enabled">>}, {1, <<"theme">>, <<"dark">>}]}
        end}
    ], fun() ->
        Uid = 1,
        Result = user_setting_repo:find_by_uid(Uid),
        ?ASSERT_OK(Result),
        {ok, Settings} = Result,
        % 验证返回的设置列表
        ?assert(length(Settings) >= 2),
        % 验证第一个设置
        [Setting1, _Setting2 | _] = Settings,
        ?assertEqual(1, element(1, Setting1)),
        ?assertEqual(<<"notification">>, element(2, Setting1)),
        ?assertEqual(<<"enabled">>, element(3, Setting1))
    end).

get_setting_by_key_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(Sql, Params) ->
            % 验证SQL查询包含特定设置查询
            ?assert(binary:match(Sql, <<"SELECT.*FROM.*user_setting">>) =/= nomatch),
            ?assert(binary:match(Sql, <<"WHERE.*user_id.*AND.*setting_key">>) =/= nomatch),
            % 验证参数包含用户ID和设置键
            ?assert(length(Params) >= 2),
            ?assert(lists:member(1, Params)),
            ?assert(lists:member(<<"notification">>, Params)),
            % 返回模拟的设置值
            {ok, [{1, <<"notification">>, <<"enabled">>}]}
        end}
    ], fun() ->
        Uid = 1,
        Key = <<"notification">>,
        Result = user_setting_repo:get(Uid, Key),
        ?ASSERT_OK(Result),
        {ok, SettingValue} = Result,
        ?assertEqual(<<"enabled">>, SettingValue)
    end).

%% ===================================================================
%% 设置更新测试
%% ===================================================================

update_setting_value_test_() ->
    ?WITH_MECK(elib_pg, [
        {'execute', 3, fun(Sql, Params) ->
            % 验证SQL包含设置更新
            ?assert(binary:match(Sql, <<"INSERT.*INTO.*user_setting">>) =/= nomatch),
            ?assert(binary:match(Sql, <<"ON CONFLICT.*DO UPDATE">>) =/= nomatch),
            % 验证参数包含用户ID、设置键和值
            ?assert(length(Params) >= 3),
            ?assert(lists:member(1, Params)),
            ?assert(lists:member(<<"theme">>, Params)),
            ?assert(lists:member(<<"dark">>, Params)),
            {ok, 1}
        end}
    ], fun() ->
        Uid = 1,
        Key = <<"theme">>,
        Value = <<"dark">>,
        
        Result = user_setting_repo:save(Uid, Key, Value),
        ?assertEqual(ok, Result)
    end).
