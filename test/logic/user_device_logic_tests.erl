-module(user_device_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% @doc user_device_logic 模块测试
save_device_success_test_() ->
    ?WITH_MECK(user_device_ds, [
        {'save', 4, fun(_Now, _Uid, _Did, _Data) -> ok end}
    ], fun() ->
        Result = user_device_logic:save(100, <<"device1">>, #{<<"device_name">> => <<"iPhone">>}),
        ?assertEqual(ok, Result)
    end).

get_devices_success_test_() ->
    ?WITH_MECK(user_device_ds, [
            {'page', 3, fun(_Uid, _Limit, _Offset) -> {ok, [#{<<"device_id">> => <<"d1">>}]} end}
        ], fun() ->
        Result = user_device_logic:get_devices(100, 10, 0),
        ?assertMatch({ok, [_ | _]}, Result)
    end).
