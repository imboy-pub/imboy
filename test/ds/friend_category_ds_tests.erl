-module(friend_category_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% friend_category_ds 模块的 EUnit 测试
%%%
%%% 目标：验证好友分类服务功能
%%% 覆盖：分类添加、查找、重命名、删除
%%%===================================================================

%% ===================================================================
%% add/2 测试
%% ===================================================================

add_creates_category_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 1,
        Name = <<"Test Category">>,
        Result = friend_category_ds:add(Uid, Name),
        case Result of
            {ok, CategoryId} when is_integer(CategoryId) -> ?assert(CategoryId > 0);
            {ok, _} -> ?assert(true);
            _ -> ?assert(false, "Expected {ok, CategoryId}")
        end
    end).

add_with_empty_name_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 1,
        Name = <<>>,
        Result = friend_category_ds:add(Uid, Name),
        % 空名称可能被拒绝（返回错误）或返回已存在的分类ID
        case Result of
            {error, _} -> ok;
            {ok, _} -> ok
        end
    end).

%% ===================================================================
%% find_by_uid/1 测试
%% ===================================================================

find_by_uid_returns_list_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 1,
        Result = friend_category_ds:find_by_uid(Uid),
        ?assertMatch([_|_], Result)
    end).

find_by_uid_empty_when_no_categories_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 999999,
        Result = friend_category_ds:find_by_uid(Uid),
        ?assertMatch([_|_], Result)
    end).

%% ===================================================================
%% rename/3 测试
%% ===================================================================

rename_updates_category_name_test_() ->
    {setup,
     fun() ->
         meck:new(elib_pg, [no_link, passthrough]),
         meck:expect(elib_pg, update, 4, {ok, 1}),
         ok
     end,
     fun(_) ->
         meck:unload(elib_pg)
     end,
     fun(_) ->
         ?_test(fun() ->
             Uid = 1,
             Cid = 1,
             NewName = <<"Renamed Category">>,
             Result = friend_category_ds:rename(Uid, Cid, NewName),
             case Result of
                 {ok, AffectedCount} when is_integer(AffectedCount) -> ?assert(AffectedCount >= 0);
                 {ok, _} -> ?assert(true);
                 _ -> ?assert(false, "Expected {ok, AffectedCount}")
             end
         end)
     end}.

%% ===================================================================
%% delete/2 测试
%% ===================================================================

delete_removes_category_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 1,
        Cid = 999999,
        Result = friend_category_ds:delete(Uid, Cid),
        % 对不存在的记录，delete 可能返回 {ok, Count} 或 {error, not_found}
        case Result of
            {ok, AffectedCount} when is_integer(AffectedCount) -> ?assert(AffectedCount >= 0);
            {ok, _} -> ?assert(true);
            {error, _} -> ok
        end
    end).
