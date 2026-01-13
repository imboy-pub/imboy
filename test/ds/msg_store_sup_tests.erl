-module(msg_store_sup_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% @doc msg_store_sup 模块测试
tablename_test_() ->
    ?WITH_MECK(msg_store_repo, [
        {'tablename', 0, fun() -> <<"msg_store">> end}
    ], fun() ->
        Result = msg_store_sup:tablename(),
        ?assertEqual(<<"msg_store">>, Result)
    end).
