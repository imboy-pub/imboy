-module(group_notice_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% @doc group_notice_logic 模块测试
save_success_test_() ->
    ?WITH_MECK(group_notice_repo, [
        {'save', 2, fun(_Conn, _Data) -> {ok, 1} end}
    ], fun() ->
        Result = group_notice_logic:save(self(), #{<<"content">> => <<"公告内容"/utf8>>, <<"group_id">> => 1}),
        ?assertEqual({ok, 1}, Result)
    end).

delete_success_test_() ->
    ?WITH_MECK(group_notice_repo, [
        {'delete', 2, fun(_Conn, _Id) -> ok end}
    ], fun() ->
        Result = group_notice_logic:delete(self(), 1),
        ?assertEqual(ok, Result)
    end).
