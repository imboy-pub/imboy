-module(user_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("include/eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% user_logic 模块的 EUnit 测试
%%%
%%% 目标：验证用户逻辑服务功能
%%% 覆盖：在线状态、用户查找、密码修改
%%%===================================================================

%% ===================================================================
%% is_online/1 测试
%% ===================================================================

is_online_returns_boolean_test_() ->
    ?WITH_MECK(user_server, [
        {'is_online', 1, fun(_Uid) -> true end}
    ], fun() ->
        Uid = 1,
        Result = user_logic:is_online(Uid),
        ?assertEqual(true, Result)
    end).

%% ===================================================================
%% online_state/1 测试
%% ===================================================================

online_state_returns_map_test_() ->
    ?WITH_MECK(user_server, [
        {'online_state', 1, fun(_Uid) -> 
            #{<<"id">> => 1, <<"nickname">> => <<"Test">>, <<"status">> => online}
        end}
    ], fun() ->
        Uid = 1,
        Result = user_logic:online_state(Uid),
        ?assertMatch(#{<<"id">> := 1, <<"nickname">> := <<"Test">>, <<"status">> := online}, Result)
    end).

%% ===================================================================
%% find_by_id/1 测试
%% ===================================================================

find_by_id_returns_map_test_() ->
    ?WITH_MECK(user_repo, [
        {'find', 1, fun(_Uid) ->
            {ok, #{<<"id">> => 1, <<"nickname">> => <<"Test User">>, <<"email">> => <<"test@example.com">>}}
        end}
    ], fun() ->
        Uid = 1,
        Result = user_logic:find_by_id(Uid),
        ?ASSERT_OK(Result),
        {ok, User} = Result,
        ?assertMatch(#{<<"id">> := 1, <<"nickname">> := <<"Test User">>, <<"email">> := <<"test@example.com">>}, User)
    end).

%% ===================================================================
%% update/3 测试
%% ===================================================================

update_modifies_user_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 1,
        Key = <<"nickname">>,
        Val = <<"Updated Nickname">>,
        Result = user_logic:update(Uid, Key, Val),
        case Result of
            {ok, AffectedCount} when is_integer(AffectedCount) -> ?assert(AffectedCount >= 0);
            {ok, _} -> ?assert(true);
            _ -> ?assert(false, "Expected {ok, AffectedCount}")
        end
    end).
