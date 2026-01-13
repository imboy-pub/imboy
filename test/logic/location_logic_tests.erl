-module(location_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% @doc location_logic 模块测试
save_location_success_test_() ->
    ?WITH_MECK(geo_people_nearby_ds, [
        {'save', 3, fun(_Uid, _Lat, _Lng) -> {ok, 1} end}
    ], fun() ->
        Result = location_logic:save(100, <<"39.9042">>, <<"116.4074">>),
        ?assertEqual({ok, 1}, Result)
    end).

people_nearby_success_test_() ->
    ?WITH_MECK(geo_people_nearby_ds, [
        {'people_nearby', 5, fun(_Lng, _Lat, _Radius, _Unit, _Limit) ->
            {ok, [#{<<"uid">> => 101}]}
        end}
    ], fun() ->
        Result = location_logic:people_nearby(<<"116.4074">>, <<"39.9042">>, 1000, <<"m">>, 10),
        ?assertMatch({ok, [_ | _]}, Result)
    end).
