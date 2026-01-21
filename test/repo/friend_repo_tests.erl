-module(friend_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% friend_repo 模块的 EUnit 测试
%%%
%%% 目标：验证 imboy_db → elib_pg 迁移的语义正确性
%%%===================================================================

%% ===================================================================
%% tablename/0 测试
%% ===================================================================

tablename_public_prefix_test_() ->
    ?TEST_WITH_APP(fun() ->
        Result = friend_repo:tablename(),
        ?assertEqual(<<"public.user_friend">>, Result)
    end).

%% ===================================================================
%% confirm_friend/7 测试
%% ===================================================================

confirm_friend_already_confirmed_test_() ->
    ?TEST_WITH_DB(fun() ->
        % 已确认的好友关系应该直接返回 ok
        Result = friend_repo:confirm_friend(true, 1, 2, <<"remark">>, <<"[]">>, <<"tag">>, elib_dt:now()),
        ?assertEqual(ok, Result)
    end).

confirm_friend_new_friend_test_() ->
    ?TEST_WITH_DB(fun() ->
        % 新好友关系应该插入数据库
        Result = friend_repo:confirm_friend(false, 1, 2, <<"remark">>, <<"[]">>, <<"tag">>, elib_dt:now()),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% friend_field/3 测试
%% ===================================================================

friend_field_existing_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(Sql, Params) ->
            SqlBin = iolist_to_binary(Sql),
            ?assert(re:run(SqlBin, <<"SELECT.*FROM.*user_friend">>) =/= nomatch),
            ?assert(re:run(SqlBin, <<"WHERE.*from_user_id.*AND.*to_user_id">>) =/= nomatch),
            % 验证参数包含用户ID和朋友ID
            ?assert(length(Params) >= 2),
            ?assert(lists:member(1, Params)),
            ?assert(lists:member(2, Params)),
            % 返回模拟的好友数据
            {ok, [#{<<"id">> => 1, <<"user_id">> => 1, <<"friend_id">> => 2, <<"remark">> => <<"remark">>, <<"ext">> => <<"[]">>, <<"tag">> => <<"tag">>, <<"created_at">> => 1640995200}]}
        end}
    ], fun() ->
        FromID = 1,
        ToID = 2,
        Field = <<"id">>,
        {ok, [FriendData]} = friend_repo:friend_field(FromID, ToID, Field),
        % 验证返回的数据结构
        #{<<"id">> := Id} = FriendData,
        ?assertEqual(1, Id),
        % 验证返回的好友数据包含必要的字段
        ?assertEqual(1, maps:get(<<"user_id">>, FriendData)),
        ?assertEqual(2, maps:get(<<"friend_id">>, FriendData)),
        ?assertEqual(<<"remark">>, maps:get(<<"remark">>, FriendData)),
        ?assertEqual(<<"[]">>, maps:get(<<"ext">>, FriendData)),
        ?assertEqual(<<"tag">>, maps:get(<<"tag">>, FriendData)),
        % 验证时间戳在合理范围内（2020年之后）
        CreatedAt = maps:get(<<"created_at">>, FriendData),
        ?assert(is_integer(CreatedAt)),
        ?assert(CreatedAt > 1577836800)
    end).

friend_field_non_existing_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(Sql, Params) ->
            % 验证SQL查询包含好友关系查询
            SqlBin = iolist_to_binary(Sql),
            ?assert(re:run(SqlBin, <<"SELECT.*FROM.*user_friend">>) =/= nomatch),
            % 验证参数包含不存在的用户ID和朋友IDBin
            ?assert(length(Params) >= 2),
            ?assert(lists:member(999999, Params)),
            ?assert(lists:member(888888, Params)),
            % 返回空结果表示不存在的好友关系
            {ok, []}
        end}
    ], fun() ->
        FromID = 999999,
        ToID = 888888,
        Field = <<"id">>,
        Result = friend_repo:friend_field(FromID, ToID, Field),
        ?assertEqual({ok, undefined}, Result)
    end).

%% ===================================================================
%% list_by_uid/2,3 测试
%% ===================================================================

list_by_uid_default_limit_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(Sql, Params) ->
            SqlBin = iolist_to_binary(Sql),
            % 验证SQL查询包含好友关系查询
            ?assert(re:run(SqlBin, <<"SELECT.*FROM.*user_friend">>) =/= nomatch),
            ?assert(re:run(SqlBin, <<"WHERE.*from_user_id">>) =/= nomatch),
            % 验证参数包含用户ID
            ?assert(length(Params) >= 1),
            ?assert(lists:member(1, Params)),
            % 返回模拟的好友列表
            {ok, [
                #{<<"user_id">> => 1, <<"friend_id">> => 2, <<"remark">> => <<"friend1">>, <<"ext">> => <<"[]">>, <<"tag">> => <<"tag1">>, <<"created_at">> => 1640995200},
                #{<<"user_id">> => 1, <<"friend_id">> => 3, <<"remark">> => <<"friend2">>, <<"ext">> => <<"[]">>, <<"tag">> => <<"tag2">>, <<"created_at">> => 1640995201}
            ]}
        end}
    ], fun() ->
        UID = 1,
        Column = <<"id">>,
        Result = friend_repo:list_by_uid(UID, Column),
        ?assertMatch({ok, _}, Result),
        {ok, FriendList} = Result,
        % 验证返回的好友列表
        ?assert(length(FriendList) >= 2),
        % 验证第一个好友的数据
        [Friend1, Friend2 | _] = FriendList,
        ?assertEqual(1, maps:get(<<"user_id">>, Friend1)),
        ?assertEqual(2, maps:get(<<"friend_id">>, Friend1)),
        ?assertEqual(1, maps:get(<<"user_id">>, Friend2)),
        ?assertEqual(3, maps:get(<<"friend_id">>, Friend2))
    end).

list_by_uid_custom_limit_test_() ->
    ?TEST_WITH_DB(fun() ->
        UID = 1,
        Column = <<"id">>,
        Limit = 10,
        Result = friend_repo:list_by_uid(UID, Column, Limit),
        ?assertMatch({ok, [_|_]}, Result)
    end).

list_by_uid_zero_limit_test_() ->
    ?TEST_WITH_DB(fun() ->
        UID = 1,
        Column = <<"id">>,
        Limit = 0,
        Result = friend_repo:list_by_uid(UID, Column, Limit),
        ?assertMatch({ok, []}, Result)
    end).

%% ===================================================================
%% delete/2 测试
%% ===================================================================

delete_test_() ->
    ?TEST_WITH_DB(fun() ->
        FromID = 1,
        ToID = 2,
        Result = friend_repo:delete(FromID, ToID),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% move_to_category/3 测试
%% ===================================================================

move_to_category_test_() ->
    ?TEST_WITH_DB(fun() ->
        FromUID = 1,
        ToUID = 2,
        CategoryID = 5,
        Result = friend_repo:move_to_category(FromUID, ToUID, CategoryID),
        ?assertEqual(ok, Result)
    end).

move_to_category_zero_category_test_() ->
    ?TEST_WITH_DB(fun() ->
        FromUID = 1,
        ToUID = 2,
        CategoryID = 0,
        Result = friend_repo:move_to_category(FromUID, ToUID, CategoryID),
        ?assertEqual(ok, Result)
    end).
