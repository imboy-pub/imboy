-module(friend_category_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% friend_category_logic 模块的 EUnit 测试
%%%
%%% 目标：验证好友分类业务逻辑功能
%%% 覆盖：分类创建、删除
%%%===================================================================

%% ===================================================================
%% add/2 测试
%% ===================================================================

add_category_success_test_() ->
    ?WITH_MECK(friend_category_ds, [
        {'add', 2, fun(_Uid, _Name) -> {ok, 12345} end}
    ], fun() ->
        Uid = 12345,
        Name = <<"Friends">>,
        
        Result = friend_category_logic:add(Uid, Name),
        ?assertEqual({ok, 12345}, Result)
    end).

add_category_with_empty_name_test_() ->
    ?WITH_MECK(friend_category_ds, [
        {'add', 2, fun(_Uid, _Name) -> {ok, 12346} end}
    ], fun() ->
        Uid = 12345,
        Name = <<>>,  % 空名称
        
        Result = friend_category_logic:add(Uid, Name),
        ?assertEqual({ok, 12346}, Result)
    end).

add_category_with_long_name_test_() ->
    ?WITH_MECK(friend_category_ds, [
        {'add', 2, fun(_Uid, _Name) -> {ok, 12347} end}
    ], fun() ->
        Uid = 12345,
        LongName = <<"This is a very long category name that might be used for testing purposes">>,
        
        Result = friend_category_logic:add(Uid, LongName),
        ?assertEqual({ok, 12347}, Result)
    end).

add_category_ds_error_test_() ->
    ?WITH_MECK(friend_category_ds, [
        {'add', 2, fun(_Uid, _Name) -> {error, <<"Database error">>} end}
    ], fun() ->
        Uid = 12345,
        Name = <<"Friends">>,
        
        Result = friend_category_logic:add(Uid, Name),
        ?assertEqual({error, <<"Database error">>}, Result)
    end).

%% ===================================================================
%% delete/2 测试
%% ===================================================================

delete_category_success_test_() ->
    ?WITH_MECK(friend_ds, [
        {'set_category_id', 3, fun(_Uid, _Id, _NewCategoryId) -> {ok, 2} end}
    ], fun() ->
        ?WITH_MECK(friend_category_ds, [
            {'delete', 2, fun(_Uid, _Id) -> ok end}
        ], fun() ->
            Uid = 12345,
            CategoryId = 67890,
            
            Result = friend_category_logic:delete(Uid, CategoryId),
            ?assertEqual(ok, Result)
        end)
    end).

delete_category_set_category_id_error_test_() ->
    ?WITH_MECK(friend_ds, [
        {'set_category_id', 3, fun(_Uid, _Id, _NewCategoryId) -> {error, <<"Friend update failed">>} end}
    ], fun() ->
        Uid = 12345,
        CategoryId = 67890,
        
        Result = friend_category_logic:delete(Uid, CategoryId),
        ?assertEqual({error, <<"Friend update failed">>}, Result)
    end).

delete_category_delete_error_test_() ->
    ?WITH_MECK(friend_ds, [
        {'set_category_id', 3, fun(_Uid, _Id, _NewCategoryId) -> {ok, 2} end}
    ], fun() ->
        ?WITH_MECK(friend_category_ds, [
            {'delete', 2, fun(_Uid, _Id) -> {error, <<"Category delete failed">>} end}
        ], fun() ->
            Uid = 12345,
            CategoryId = 67890,
            
            Result = friend_category_logic:delete(Uid, CategoryId),
            ?assertEqual({error, <<"Category delete failed">>}, Result)
        end)
    end).

delete_category_no_friends_test_() ->
    ?WITH_MECK(friend_ds, [
        {'set_category_id', 3, fun(_Uid, _Id, _NewCategoryId) -> {ok, 0} end}  % 没有好友需要更新
    ], fun() ->
        ?WITH_MECK(friend_category_ds, [
            {'delete', 2, fun(_Uid, _Id) -> ok end}
        ], fun() ->
            Uid = 12345,
            CategoryId = 67890,
            
            Result = friend_category_logic:delete(Uid, CategoryId),
            ?assertEqual(ok, Result)
        end)
    end).

delete_category_nonexistent_test_() ->
    ?WITH_MECK(friend_ds, [
        {'set_category_id', 3, fun(_Uid, _Id, _NewCategoryId) -> {ok, 0} end}
    ], fun() ->
        ?WITH_MECK(friend_category_ds, [
            {'delete', 2, fun(_Uid, _Id) -> ok end}
        ], fun() ->
            Uid = 12345,
            CategoryId = 99999,  % 不存在的分类
            
            Result = friend_category_logic:delete(Uid, CategoryId),
            ?assertEqual(ok, Result)
        end)
    end).
