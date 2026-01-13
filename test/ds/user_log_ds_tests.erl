-module(user_log_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% @doc user_log_ds 模块测试
save_log_success_test_() ->
    ?WITH_MECK(user_log_repo, [
        {'save', 1, fun(_Data) -> {ok, 1} end}
    ], fun() ->
        Result = user_log_ds:save(#{<<"user_id">> => 100, <<"action">> => <<"login">>}),
        ?assertEqual({ok, 1}, Result)
    end).

page_logs_success_test_() ->
    ?WITH_MECK(user_log_repo, [
        {'page', 3, fun(_Uid, _Limit, _Offset) -> {ok, []} end}
    ], fun() ->
        Result = user_log_ds:page(100, 10, 0),
        ?assertEqual({ok, []}, Result)
    end).
