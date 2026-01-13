-module(user_tag_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% @doc user_tag_logic 模块测试
save_tag_success_test_() ->
    ?WITH_MECK(user_tag_ds, [
        {'add', 3, fun(_Uid, _Scene, _Tag) -> {ok, 1} end}
    ], fun() ->
        Result = user_tag_logic:save(100, 1, <<"标签1"/utf8>>),
        ?assertEqual({ok, 1}, Result)
    end).

delete_tag_success_test_() ->
    ?WITH_MECK(user_tag_ds, [
        {'delete', 3, fun(_Uid, _Scene, _Tag) -> ok end}
    ], fun() ->
        Result = user_tag_logic:delete(100, 1, <<"标签1"/utf8>>),
        ?assertEqual(ok, Result)
    end).
