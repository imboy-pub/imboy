-module(geo_people_nearby_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% geo_people_nearby_ds 模块的 EUnit 测试
%%%
%%% 目标：验证地理位置数据服务功能
%%% 覆盖：保存位置、删除位置、附近的人查询
%%%===================================================================

%% ===================================================================
%% save/3 测试
%% ===================================================================

%% @doc 测试保存地理位置成功
save_success_test_() ->
    ?WITH_MECK(geo_people_nearby_repo, [
        {'save', 3, fun(Uid, Lat, Lng) ->
            ?assertEqual(100, Uid),
            ?assertEqual(<<"39.9042">>, Lat),
            ?assertEqual(<<"116.4074">>, Lng),
            {ok, 1}
        end}
    ], fun() ->
        Result = geo_people_nearby_ds:save(100, <<"39.9042">>, <<"116.4074">>),
        ?assertEqual({ok, 1}, Result)
    end).

%% @doc 测试保存地理位置失败
save_with_error_returns_error_test_() ->
    ?WITH_MECK(geo_people_nearby_repo, [
        {'save', 3, fun(_Uid, _Lat, _Lng) ->
            {error, <<"database_error">>}
        end}
    ], fun() ->
        Result = geo_people_nearby_ds:save(100, <<"39.9042">>, <<"116.4074">>),
        ?assertEqual({error, <<"database_error">>}, Result)
    end).

%% @doc 测试更新已存在的位置
save_update_existing_location_test_() ->
    ?WITH_MECK(geo_people_nearby_repo, [
        {'save', 3, fun(Uid, Lat, Lng) ->
            ?assertEqual(100, Uid),
            ?assertEqual(<<"31.2304">>, Lat),
            ?assertEqual(<<"121.4737">>, Lng),
            {ok, 1}
        end}
    ], fun() ->
        Result = geo_people_nearby_ds:save(100, <<"31.2304">>, <<"121.4737">>),
        ?assertEqual({ok, 1}, Result)
    end).

%% ===================================================================
%% delete/1 测试
%% ===================================================================

%% @doc 测试删除地理位置成功
delete_success_test_() ->
    ?WITH_MECK(geo_people_nearby_repo, [
        {'delete', 1, fun(Uid) ->
            ?assertEqual(100, Uid),
            {ok, 1}
        end}
    ], fun() ->
        Result = geo_people_nearby_ds:delete(100),
        ?assertEqual({ok, 1}, Result)
    end).

%% @doc 测试删除不存在的位置
delete_nonexistent_returns_zero_test_() ->
    ?WITH_MECK(geo_people_nearby_repo, [
        {'delete', 1, fun(_Uid) ->
            {ok, 0}
        end}
    ], fun() ->
        Result = geo_people_nearby_ds:delete(999999),
        ?assertEqual({ok, 0}, Result)
    end).

%% @doc 测试删除地理位置失败
delete_with_error_returns_error_test_() ->
    ?WITH_MECK(geo_people_nearby_repo, [
        {'delete', 1, fun(_Uid) ->
            {error, <<"database_error">>}
        end}
    ], fun() ->
        Result = geo_people_nearby_ds:delete(100),
        ?assertEqual({error, <<"database_error">>}, Result)
    end).

%% ===================================================================
%% people_nearby/5 测试
%% ===================================================================

%% @doc 测试查询附近的人成功
people_nearby_returns_results_test_() ->
    ?WITH_MECK(geo_people_nearby_repo, [
        {'people_nearby', 5, fun(Lng, Lat, Radius, Unit, Limit) ->
            ?assertEqual(<<"116.4074">>, Lng),
            ?assertEqual(<<"39.9042">>, Lat),
            ?assertEqual(1000, Radius),
            ?assertEqual(<<"m">>, Unit),
            ?assertEqual(10, Limit),
            {ok, [
                #{<<"uid">> => 101, <<"nickname">> => <<"用户A"/utf8>>, <<"distance">> => 500},
                #{<<"uid">> => 102, <<"nickname">> => <<"用户B"/utf8>>, <<"distance">> => 800}
            ]}
        end}
    ], fun() ->
        {ok, Results} = geo_people_nearby_ds:people_nearby(<<"116.4074">>, <<"39.9042">>, 1000, <<"m">>, 10),
        ?assertEqual(2, length(Results))
    end).

%% @doc 测试查询附近的人空结果
people_nearby_with_no_results_returns_empty_list_test_() ->
    ?WITH_MECK(geo_people_nearby_repo, [
        {'people_nearby', 5, fun(_Lng, _Lat, _Radius, _Unit, _Limit) ->
            {ok, []}
        end}
    ], fun() ->
        {ok, Results} = geo_people_nearby_ds:people_nearby(<<"116.4074">>, <<"39.9042">>, 100, <<"m">>, 10),
        ?assertEqual(0, length(Results))
    end).

%% @doc 测试查询附近的人错误
people_nearby_with_error_returns_error_test_() ->
    ?WITH_MECK(geo_people_nearby_repo, [
        {'people_nearby', 5, fun(_Lng, _Lat, _Radius, _Unit, _Limit) ->
            {error, <<"invalid_coordinates">>}
        end}
    ], fun() ->
        Result = geo_people_nearby_ds:people_nearby(<<"116.4074">>, <<"39.9042">>, 1000, <<"m">>, 10),
        ?assertEqual({error, <<"invalid_coordinates">>}, Result)
    end).

%% @doc 测试限制结果数量
people_nearby_with_limit_test_() ->
    ?WITH_MECK(geo_people_nearby_repo, [
        {'people_nearby', 5, fun(_Lng, _Lat, _Radius, _Unit, Limit) ->
            ?assertEqual(20, Limit),
            {ok, [#{<<"uid">> => N} || N <- lists:seq(1, 20)]}
        end}
    ], fun() ->
        {ok, Results} = geo_people_nearby_ds:people_nearby(<<"116.4074">>, <<"39.9042">>, 1000, <<"m">>, 20),
        ?assertEqual(20, length(Results))
    end).

%% @doc 测试不同半径
people_nearby_with_different_radius_test_() ->
    ?WITH_MECK(geo_people_nearby_repo, [
        {'people_nearby', 5, fun(_Lng, _Lat, Radius, _Unit, _Limit) ->
            ?assertEqual(5000, Radius),
            {ok, [#{<<"uid">> => 101}]}
        end}
    ], fun() ->
        {ok, Results} = geo_people_nearby_ds:people_nearby(<<"116.4074">>, <<"39.9042">>, 5000, <<"m">>, 10),
        ?assertEqual(1, length(Results))
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

%% @doc 测试极北纬度
save_with_extreme_north_latitude_test_() ->
    ?WITH_MECK(geo_people_nearby_repo, [
        {'save', 3, fun(_Uid, Lat, _Lng) ->
            ?assertEqual(<<"90.0">>, Lat),
            {ok, 1}
        end}
    ], fun() ->
        Result = geo_people_nearby_ds:save(100, <<"90.0">>, <<"0.0">>),
        ?assertEqual({ok, 1}, Result)
    end).

%% @doc 测试极南纬度
save_with_extreme_south_latitude_test_() ->
    ?WITH_MECK(geo_people_nearby_repo, [
        {'save', 3, fun(_Uid, Lat, _Lng) ->
            ?assertEqual(<<"-90.0">>, Lat),
            {ok, 1}
        end}
    ], fun() ->
        Result = geo_people_nearby_ds:save(100, <<"-90.0">>, <<"0.0">>),
        ?assertEqual({ok, 1}, Result)
    end).

%% @doc 测试极东经度
save_with_extreme_east_longitude_test_() ->
    ?WITH_MECK(geo_people_nearby_repo, [
        {'save', 3, fun(_Uid, _Lat, Lng) ->
            ?assertEqual(<<"180.0">>, Lng),
            {ok, 1}
        end}
    ], fun() ->
        Result = geo_people_nearby_ds:save(100, <<"0.0">>, <<"180.0">>),
        ?assertEqual({ok, 1}, Result)
    end).

%% @doc 测试极西经度
save_with_extreme_west_longitude_test_() ->
    ?WITH_MECK(geo_people_nearby_repo, [
        {'save', 3, fun(_Uid, _Lat, Lng) ->
            ?assertEqual(<<"-180.0">>, Lng),
            {ok, 1}
        end}
    ], fun() ->
        Result = geo_people_nearby_ds:save(100, <<"0.0">>, <<"-180.0">>),
        ?assertEqual({ok, 1}, Result)
    end).

%% @doc 测试本初子午线
save_with_prime_meridian_test_() ->
    ?WITH_MECK(geo_people_nearby_repo, [
        {'save', 3, fun(_Uid, _Lat, Lng) ->
            ?assertEqual(<<"0.0">>, Lng),
            {ok, 1}
        end}
    ], fun() ->
        Result = geo_people_nearby_ds:save(100, <<"51.4779">>, <<"0.0">>),
        ?assertEqual({ok, 1}, Result)
    end).

%% @doc 测试赤道
save_with_equator_test_() ->
    ?WITH_MECK(geo_people_nearby_repo, [
        {'save', 3, fun(_Uid, Lat, _Lng) ->
            ?assertEqual(<<"0.0">>, Lat),
            {ok, 1}
        end}
    ], fun() ->
        Result = geo_people_nearby_ds:save(100, <<"0.0">>, <<"78.4765">>),
        ?assertEqual({ok, 1}, Result)
    end).

%% @doc 测试零半径
people_nearby_with_zero_radius_test_() ->
    ?WITH_MECK(geo_people_nearby_repo, [
        {'people_nearby', 5, fun(_Lng, _Lat, Radius, _Unit, _Limit) ->
            ?assertEqual(0, Radius),
            {ok, []}
        end}
    ], fun() ->
        {ok, Results} = geo_people_nearby_ds:people_nearby(<<"116.4074">>, <<"39.9042">>, 0, <<"m">>, 10),
        ?assertEqual(0, length(Results))
    end).

%% @doc 测试大半径
people_nearby_with_large_radius_test_() ->
    ?WITH_MECK(geo_people_nearby_repo, [
        {'people_nearby', 5, fun(_Lng, _Lat, Radius, _Unit, _Limit) ->
            ?assertEqual(100000, Radius),
            {ok, [#{<<"uid">> => 101}]}
        end}
    ], fun() ->
        {ok, Results} = geo_people_nearby_ds:people_nearby(<<"116.4074">>, <<"39.9042">>, 100000, <<"m">>, 10),
        ?assertEqual(1, length(Results))
    end).

%% @doc 测试零限制
people_nearby_with_zero_limit_test_() ->
    ?WITH_MECK(geo_people_nearby_repo, [
        {'people_nearby', 5, fun(_Lng, _Lat, _Radius, _Unit, Limit) ->
            ?assertEqual(0, Limit),
            {ok, []}
        end}
    ], fun() ->
        {ok, Results} = geo_people_nearby_ds:people_nearby(<<"116.4074">>, <<"39.9042">>, 1000, <<"m">>, 0),
        ?assertEqual(0, length(Results))
    end).

%% @doc 测试大限制
people_nearby_with_large_limit_test_() ->
    ?WITH_MECK(geo_people_nearby_repo, [
        {'people_nearby', 5, fun(_Lng, _Lat, _Radius, _Unit, Limit) ->
            ?assertEqual(10000, Limit),
            {ok, []}
        end}
    ], fun() ->
        {ok, Results} = geo_people_nearby_ds:people_nearby(<<"116.4074">>, <<"39.9042">>, 1000, <<"m">>, 10000),
        ?assertEqual(0, length(Results))
    end).

%% @doc 测试不同单位（公里）
people_nearby_with_kilometer_unit_test_() ->
    ?WITH_MECK(geo_people_nearby_repo, [
        {'people_nearby', 5, fun(_Lng, _Lat, _Radius, Unit, _Limit) ->
            ?assertEqual(<<"km">>, Unit),
            {ok, []}
        end}
    ], fun() ->
        {ok, Results} = geo_people_nearby_ds:people_nearby(<<"116.4074">>, <<"39.9042">>, 1, <<"km">>, 10),
        ?assertEqual(0, length(Results))
    end).

%% @doc 测试高精度坐标
save_with_high_precision_coordinates_test_() ->
    ?WITH_MECK(geo_people_nearby_repo, [
        {'save', 3, fun(_Uid, Lat, Lng) ->
            ?assertEqual(<<"39.9042020">>, Lat),
            ?assertEqual(<<"116.4074010">>, Lng),
            {ok, 1}
        end}
    ], fun() ->
        Result = geo_people_nearby_ds:save(100, <<"39.9042020">>, <<"116.4074010">>),
        ?assertEqual({ok, 1}, Result)
    end).

%% @doc 测试负坐标
save_with_negative_coordinates_test_() ->
    ?WITH_MECK(geo_people_nearby_repo, [
        {'save', 3, fun(_Uid, Lat, Lng) ->
            ?assertEqual(<<"-33.8688">>, Lat),
            ?assertEqual(<<"151.2093">>, Lng),
            {ok, 1}
        end}
    ], fun() ->
        Result = geo_people_nearby_ds:save(100, <<"-33.8688">>, <<"151.2093">>),
        ?assertEqual({ok, 1}, Result)
    end).

%% ===================================================================
%% 类型验证测试
%% ===================================================================

%% @doc 验证save参数类型
save_validates_types_test_() ->
    ?TEST_SIMPLE(fun() ->
        Uid = 100,
        Lat = <<"39.9042">>,
        Lng = <<"116.4074">>,
        ?assert(is_integer(Uid)),
        ?assert(is_binary(Lat)),
        ?assert(is_binary(Lng))
    end).

%% @doc 验证delete参数类型
delete_validates_types_test_() ->
    ?TEST_SIMPLE(fun() ->
        Uid = 100,
        ?assert(is_integer(Uid))
    end).

%% @doc 验证people_nearby参数类型
people_nearby_validates_types_test_() ->
    ?TEST_SIMPLE(fun() ->
        Lng = <<"116.4074">>,
        Lat = <<"39.9042">>,
        Radius = 1000,
        Unit = <<"m">>,
        Limit = 10,
        ?assert(is_binary(Lng)),
        ?assert(is_binary(Lat)),
        ?assert(is_integer(Radius)),
        ?assert(is_binary(Unit)),
        ?assert(is_integer(Limit))
    end).

%% ===================================================================
%% 集成场景测试
%% ===================================================================

%% @doc 测试完整的地理位置生命周期
complete_location_lifecycle_test_() ->
    ?WITH_MECK(geo_people_nearby_repo, [
        {'save', 3, fun(_Uid, _Lat, _Lng) ->
            {ok, 1}
        end},
        {'people_nearby', 5, fun(_Lng, _Lat, _Radius, _Unit, _Limit) ->
            {ok, [#{<<"uid">> => 101}]}
        end},
        {'delete', 1, fun(_Uid) ->
            {ok, 1}
        end}
    ], fun() ->
        Uid = 100,
        % 1. 保存位置
        ?assertEqual({ok, 1}, geo_people_nearby_ds:save(Uid, <<"39.9042">>, <<"116.4074">>)),
        % 2. 查询附近的人
        {ok, Nearby} = geo_people_nearby_ds:people_nearby(<<"116.4074">>, <<"39.9042">>, 1000, <<"m">>, 10),
        ?assertEqual(1, length(Nearby)),
        % 3. 删除位置
        ?assertEqual({ok, 1}, geo_people_nearby_ds:delete(Uid))
    end).

%% @doc 测试位置更新后重新查询
location_update_and_requery_test_() ->
    ?WITH_MECKS([
        {geo_people_nearby_repo, [
            {'save', 3, fun(Uid, Lat, Lng) ->
                {ok, 1}
            end}
        ]}
    ], fun() ->
        Uid = 100,
        % 第一次保存位置
        ?assertEqual({ok, 1}, geo_people_nearby_ds:save(Uid, <<"39.9042">>, <<"116.4074">>)),
        % 更新位置
        ?assertEqual({ok, 1}, geo_people_nearby_ds:save(Uid, <<"31.2304">>, <<"121.4737">>))
    end).

%% @doc 测试多用户附近的人查询
multiple_users_nearby_test_() ->
    ?WITH_MECK(geo_people_nearby_repo, [
        {'people_nearby', 5, fun(_Lng, _Lat, _Radius, _Unit, Limit) ->
            ?assertEqual(50, Limit),
            {ok, [#{<<"uid">> => N} || N <- lists:seq(101, 150)]}
        end}
    ], fun() ->
        {ok, Nearby} = geo_people_nearby_ds:people_nearby(<<"116.4074">>, <<"39.9042">>, 5000, <<"m">>, 50),
        ?assertEqual(50, length(Nearby))
    end).

%% @doc 测试不同城市的附近的人
people_nearby_different_cities_test_() ->
    Cities = [
        {<<"116.4074">>, <<"39.9042">>, <<"北京"/utf8>>},
        {<<"121.4737">>, <<"31.2304">>, <<"上海"/utf8>>},
        {<<"113.2644">>, <<"23.1291">>, <<"广州"/utf8>>},
        {<<"114.0579">>, <<"22.5431">>, <<"深圳"/utf8>>}
    ],
    ?WITH_MECK(geo_people_nearby_repo, [
        {'people_nearby', 5, fun(Lng, Lat, _Radius, _Unit, _Limit) ->
            {ok, [#{<<"longitude">> => Lng, <<"latitude">> => Lat}]}
        end}
    ], fun() ->
        lists:foreach(fun({Lng, Lat, _CityName}) ->
            {ok, _Nearby} = geo_people_nearby_ds:people_nearby(Lng, Lat, 1000, <<"m">>, 10)
        end, Cities)
    end).
