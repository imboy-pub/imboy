-module(user_tag_relation_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% @doc user_tag_relation_logic 模块测试
set_tag_success_test_() ->
    ?WITH_MECK(user_tag_relation_ds, [
        {'set', 5, fun(_Uid, _Scene, _ObjectIds, _TagId, _TagName) -> ok end}
    ], fun() ->
        Result = user_tag_relation_logic:set(100, 1, [1, 2], 1, <<"标签1"/utf8>>),
        ?assertEqual(ok, Result)
    end).

remove_tag_success_test_() ->
    ?WITH_MECK(user_tag_relation_ds, [
        {'remove', 4, fun(_Uid, _Scene, _ObjectId, _TagId) -> {ok, 1} end}
    ], fun() ->
        Result = user_tag_relation_logic:remove(100, 1, 1, 1),
        ?assertEqual({ok, 1}, Result)
    end).
