-module(user_denylist_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% @doc user_denylist_logic 模块测试
add_to_denylist_success_test_() ->
    ?WITH_MECK(user_denylist_ds, [
        {'add', 3, fun(_Uid, _DeniedUid, _Remark) -> ok end}
    ], fun() ->
        Result = user_denylist_logic:add(100, 101, <<"原因"/utf8>>),
        ?assertEqual(ok, Result)
    end).

remove_from_denylist_success_test_() ->
    ?WITH_MECK(user_denylist_ds, [
        {'remove', 2, fun(_Uid, _DeniedUid) -> ok end}
    ], fun() ->
        Result = user_denylist_logic:remove(100, 101),
        ?assertEqual(ok, Result)
    end).

in_denylist_true_test_() ->
    ?WITH_MECK(user_denylist_ds, [
        {'in_denylist', 2, fun(_Uid, _DeniedUid) -> 1 end}
    ], fun() ->
        Result = user_denylist_logic:in_denylist(100, 101),
        ?assertEqual(true, Result)
    end).
