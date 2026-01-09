-module(user_tag_relation_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% user_tag_relation_logic 模块的 EUnit 测试
%%%
%%% 目标：验证用户标签关系业务逻辑功能
%%% 覆盖：添加标签、移除标签、设置标签
%%%===================================================================

%% ===================================================================
%% remove/4 测试
%% ===================================================================

remove_tag_success_test_() ->
    ?WITH_MECK(user_tag_relation_repo, [
        {'remove_user_tag_relation', 5, fun(_Conn, _Scene, _Uid, _TagId, _ObjectId) -> ok end},
        {'replace_object_tag', 6, fun(_Conn, _Scene, _Uid, _ObjectId, _TagName, _NewTagId) -> ok end},
        {'flush_subtitle', 1, fun(_TagId) -> ok end}
    ], fun() ->
        ?WITH_MECK(imboy_pg, [
            {'with_tx', 1, fun(TxFun) -> TxFun(mock_conn) end}
        ], fun() ->
            Uid = 12345,
            Scene = <<"user">>,
            ObjectId = <<"obj123">>,
            TagId = <<"tag456">>,
            
            Result = user_tag_relation_logic:remove(Uid, Scene, ObjectId, TagId),
            ?assertEqual(ok, Result)
        end)
    end).

remove_tag_with_integer_id_test_() ->
    ?WITH_MECK(user_tag_relation_repo, [
        {'remove_user_tag_relation', 5, fun(_Conn, _Scene, _Uid, _TagId, _ObjectId) -> ok end},
        {'replace_object_tag', 6, fun(_Conn, _Scene, _Uid, _ObjectId, _TagName, _NewTagId) -> ok end},
        {'flush_subtitle', 1, fun(_TagId) -> ok end}
    ], fun() ->
        ?WITH_MECK(imboy_pg, [
            {'with_tx', 1, fun(TxFun) -> TxFun(mock_conn) end}
        ], fun() ->
            Uid = 12345,
            Scene = <<"user">>,
            ObjectId = <<"obj123">>,
            TagId = 789,  % 整数标签ID
            
            Result = user_tag_relation_logic:remove(Uid, Scene, ObjectId, TagId),
            ?assertEqual(ok, Result)
        end)
    end).

%% ===================================================================
%% add/4 测试
%% ===================================================================

add_tag_success_test_() ->
    ?WITH_MECK(user_tag_relation_repo, [
        {'add_user_tag_relation', 5, fun(_Conn, _Scene, _Uid, _TagId, _ObjectId) -> {ok, 1} end},
        {'replace_object_tag', 6, fun(_Conn, _Scene, _Uid, _ObjectId, _TagName, _TagId) -> ok end},
        {'flush_subtitle', 1, fun(_TagId) -> ok end}
    ], fun() ->
        ?WITH_MECK(imboy_pg, [
            {'with_tx', 1, fun(TxFun) -> TxFun(mock_conn) end}
        ], fun() ->
            Uid = 12345,
            Scene = 1,
            ObjectId = <<"obj123">>,
            TagId = <<"tag456">>,
            TagName = <<"Test Tag">>,
            
            Result = user_tag_relation_logic:add(Uid, Scene, ObjectId, TagId, TagName),
            ?assertEqual(ok, Result)
        end)
    end).

%% ===================================================================
%% set/5 测试
%% ===================================================================

set_tag_success_test_() ->
    ?WITH_MECK(user_tag_relation_repo, [
        {'remove_user_tag_relation', 5, fun(_Conn, _Scene, _Uid, _TagId, _ObjectId) -> ok end},
        {'add_user_tag_relation', 5, fun(_Conn, _Scene, _Uid, _TagId, _ObjectId) -> {ok, 1} end},
        {'replace_object_tag', 6, fun(_Conn, _Scene, _Uid, _ObjectId, _TagName, _TagId) -> ok end},
        {'flush_subtitle', 1, fun(_TagId) -> ok end}
    ], fun() ->
        ?WITH_MECK(imboy_pg, [
            {'with_tx', 1, fun(TxFun) -> TxFun(mock_conn) end}
        ], fun() ->
            Uid = 12345,
            Scene = 2,
            ObjectId = <<"obj123">>,
            TagId = <<"tag456">>,
            TagName = <<"Updated Tag">>,
            
            Result = user_tag_relation_logic:set(Uid, Scene, ObjectId, TagId, TagName),
            ?assertEqual(ok, Result)
        end)
    end).
