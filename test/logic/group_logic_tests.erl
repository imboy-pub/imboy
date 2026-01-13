-module(group_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% @doc group_logic 模块测试
create_success_test_() ->
    ?WITH_MECK(group_ds, [
        {'save', 1, fun(_Data) -> {ok, 1} end}
    ], fun() ->
        Result = group_logic:save(#{<<"name">> => <<"测试群"/utf8>>, <<"creator_id">> => 100}),
        ?assertEqual({ok, 1}, Result)
    end).

dismiss_success_test_() ->
    ?WITH_MECK(group_ds, [
        {'delete', 1, fun(_Id) -> ok end}
    ], fun() ->
        Result = group_logic:dismiss(1, 100),
        ?assertEqual(ok, Result)
    end).
