-module(user_collect_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% @doc user_collect_logic 模块测试
add_collect_success_test_() ->
    ?WITH_MECK(user_collect_ds, [
        {'save', 3, fun(_Uid, _KindId, _Data) -> {ok, 1} end}
    ], fun() ->
        Result = user_collect_logic:add(100, <<"favorite">>, #{<<"kind_id">> => <<"article1">>}),
        ?assertEqual({ok, 1}, Result)
    end).

remove_collect_success_test_() ->
    ?WITH_MECK(user_collect_ds, [
        {'delete', 2, fun(_Uid, _KindId) -> {ok, 1} end}
    ], fun() ->
        Result = user_collect_logic:remove(100, <<"favorite">>),
        ?assertEqual({ok, 1}, Result)
    end).
