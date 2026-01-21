-module(location_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% location_logic 模块的 EUnit 测试
%%%
%%% 目标：验证位置服务功能
%%% 覆盖：保存位置、查询附近的人、边界条件
%%%===================================================================

%% ===================================================================
%% save/3 测试
%% ===================================================================

save_location_success_test_() ->
    ?WITH_MECK(geo_people_nearby_ds, [
        {'save', 3, fun(_Uid, _Lat, _Lng) -> {ok, 1} end}
    ], fun() ->
        Uid = 100,
        Lat = <<"39.9042">>,
        Lng = <<"116.4074">>,
        Result = location_logic:save(Uid, Lat, Lng),
        ?assertEqual({ok, 1}, Result)
    end).

save_location_with_string_coordinates_test_() ->
    ?WITH_MECK(geo_people_nearby_ds, [
        {'save', 3, fun(_Uid, _Lat, _Lng) -> {ok, 1} end}
    ], fun() ->
        Uid = 100,
        Lat = <<"39.9042">>,
        Lng = <<"116.4074">>,
        Result = location_logic:save(Uid, Lat, Lng),
        ?assertMatch({ok, _}, Result)
    end).

save_location_with_number_coordinates_test_() ->
    ?WITH_MECK(geo_people_nearby_ds, [
        {'save', 3, fun(_Uid, _Lat, _Lng) -> {ok, 1} end}
    ], fun() ->
        Uid = 100,
        Lat = 39.9042,
        Lng = 116.4074,
        Result = location_logic:save(Uid, Lat, Lng),
        ?assertMatch({ok, _}, Result)
    end).

%% ===================================================================
%% people_nearby/5 测试
%% ===================================================================

people_nearby_success_test_() ->
    ?WITH_MECK(geo_people_nearby_ds, [
        {'people_nearby', 5, fun(_Lng, _Lat, _Radius, _Unit, _Limit) ->
            {ok, [
                #{<<"uid">> => 101, <<"distance">> => 100},
                #{<<"uid">> => 102, <<"distance">> => 200}
            ]}
        end}
    ], fun() ->
        Lng = <<"116.4074">>,
        Lat = <<"39.9042">>,
        Radius = 1000,
        Unit = <<"m">>,
        Limit = 10,
        Result = location_logic:people_nearby(Lng, Lat, Radius, Unit, Limit),
        ?assertMatch({ok, [_, _]}, Result)
    end).

people_nearby_with_empty_result_test_() ->
    ?WITH_MECK(geo_people_nearby_ds, [
        {'people_nearby', 5, fun(_Lng, _Lat, _Radius, _Unit, _Limit) ->
            {ok, []}
        end}
    ], fun() ->
        Lng = <<"116.4074">>,
        Lat = <<"39.9042">>,
        Radius = 10,
        Unit = <<"m">>,
        Limit = 10,
        Result = location_logic:people_nearby(Lng, Lat, Radius, Unit, Limit),
        ?assertEqual({ok, []}, Result)
    end).

people_nearby_with_kilometer_unit_test_() ->
    ?WITH_MECK(geo_people_nearby_ds, [
        {'people_nearby', 5, fun(_Lng, _Lat, _Radius, _Unit, _Limit) ->
            {ok, [#{<<"uid">> => 101}]}
        end}
    ], fun() ->
        Lng = <<"116.4074">>,
        Lat = <<"39.9042">>,
        Radius = 1,
        Unit = <<"km">>,
        Limit = 10,
        Result = location_logic:people_nearby(Lng, Lat, Radius, Unit, Limit),
        ?assertMatch({ok, [_]}, Result)
    end).

people_nearby_with_custom_limit_test_() ->
    ?WITH_MECK(geo_people_nearby_ds, [
        {'people_nearby', 5, fun(_Lng, _Lat, _Radius, _Unit, _Limit) ->
            {ok, lists:duplicate(20, #{<<"uid">> => 101})}
        end}
    ], fun() ->
        Lng = <<"116.4074">>,
        Lat = <<"39.9042">>,
        Radius = 1000,
        Unit = <<"m">>,
        Limit = 20,
        Result = location_logic:people_nearby(Lng, Lat, Radius, Unit, Limit),
        {ok, Users} = Result,
        ?assertEqual(20, length(Users))
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

people_nearby_with_zero_radius_test_() ->
    ?WITH_MECK(geo_people_nearby_ds, [
        {'people_nearby', 5, fun(_Lng, _Lat, _Radius, _Unit, _Limit) ->
            {ok, []}
        end}
    ], fun() ->
        Lng = <<"116.4074">>,
        Lat = <<"39.9042">>,
        Radius = 0,
        Unit = <<"m">>,
        Limit = 10,
        Result = location_logic:people_nearby(Lng, Lat, Radius, Unit, Limit),
        ?assertEqual({ok, []}, Result)
    end).

people_nearby_with_large_radius_test_() ->
    ?WITH_MECK(geo_people_nearby_ds, [
        {'people_nearby', 5, fun(_Lng, _Lat, _Radius, _Unit, _Limit) ->
            {ok, [#{<<"uid">> => 101}]}
        end}
    ], fun() ->
        Lng = <<"116.4074">>,
        Lat = <<"39.9042">>,
        Radius = 50000,
        Unit = <<"m">>,
        Limit = 10,
        Result = location_logic:people_nearby(Lng, Lat, Radius, Unit, Limit),
        ?assertMatch({ok, [_]}, Result)
    end).

save_with_zero_coordinates_test_() ->
    ?WITH_MECK(geo_people_nearby_ds, [
        {'save', 3, fun(_Uid, _Lat, _Lng) -> {ok, 1} end}
    ], fun() ->
        Uid = 100,
        Lat = 0,
        Lng = 0,
        Result = location_logic:save(Uid, Lat, Lng),
        ?assertMatch({ok, _}, Result)
    end).

people_nearby_with_negative_coordinates_test_() ->
    ?WITH_MECK(geo_people_nearby_ds, [
        {'people_nearby', 5, fun(_Lng, _Lat, _Radius, _Unit, _Limit) ->
            {ok, [#{<<"uid">> => 101}]}
        end}
    ], fun() ->
        Lng = -122.4194,
        Lat = -37.7749,
        Radius = 1000,
        Unit = <<"m">>,
        Limit = 10,
        Result = location_logic:people_nearby(Lng, Lat, Radius, Unit, Limit),
        ?assertMatch({ok, [_]}, Result)
    end).
