-module(group_member_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% @doc group_member_logic 模块测试
join_group_success_test_() ->
    ?WITH_MECK(group_member_ds, [
        {'join_group', 5, fun(_Conn, _Mode, _Uid, _Gid, _Data) -> {ok, 1} end}
    ], fun() ->
        Result = group_member_logic:join_group(self(), <<"invite">>, 100, 1, #{}),
        ?assertEqual({ok, 1}, Result)
    end).

kick_member_success_test_() ->
    ?WITH_MECK(group_member_ds, [
        {'leave', 4, fun(_Conn, _Uid, _Gid, _CurrentUid) -> {ok, 1} end}
    ], fun() ->
        Result = group_member_logic:kick_member(self(), 100, 1, 1),
        ?assertEqual({ok, 1}, Result)
    end).
