-module(location_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% location_logic 模块的 EUnit 测试
%%%
%%% 目标：验证位置业务逻辑功能
%%% 覆盖：设置可见性、位置更新、附近的人查询
%%%===================================================================

%% ===================================================================
%% make_myself_visible/3 测试
%% ===================================================================

make_myself_visible_success_test_() ->
    ?WITH_MECK(user_setting_ds, [
        {'save', 3, fun(_Uid, _Key, _Value) -> ok end}
    ], fun() ->
        ?WITH_MECK(geo_people_nearby_ds, [
            {'save', 3, fun(_Uid, _Lat, _Lng) -> ok end}
        ], fun() ->
            Uid = 12345,
            Lat = <<"39.9042">>,
            Lng = <<"116.4074">>,

            Result = location_logic:make_myself_visible(Uid, Lat, Lng),
            ?assertEqual(ok, Result)
        end)
    end).

make_myself_visible_empty_latitude_test_() ->
    ?_test(begin
        Uid = 12345,
        Lat = <<>>,  % 空纬度
        Lng = <<"116.4074">>,

        Result = location_logic:make_myself_visible(Uid, Lat, Lng),
        ?assertEqual({error, <<"latitude is empty">>}, Result)
    end).

make_myself_visible_empty_longitude_test_() ->
    ?_test(begin
        Uid = 12345,
        Lat = <<"39.9042">>,
        Lng = <<>>,  % 空经度

        Result = location_logic:make_myself_visible(Uid, Lat, Lng),
        ?assertEqual({error, <<"longitude is empty">>}, Result)
    end).

make_myself_visible_both_empty_test_() ->
    ?_test(begin
        Uid = 12345,
        Lat = <<>>,
        Lng = <<>>,

        Result = location_logic:make_myself_visible(Uid, Lat, Lng),
        % 纬度检查先于经度检查
        ?assertEqual({error, <<"latitude is empty">>}, Result)
    end).

%% ===================================================================
%% make_myself_unvisible/1 测试
%% ===================================================================

make_myself_unvisible_success_test_() ->
    ?WITH_MECK(user_setting_ds, [
        {'save', 3, fun(_Uid, _Key, _Value) -> ok end}
    ], fun() ->
        ?WITH_MECK(geo_people_nearby_ds, [
            {'delete', 1, fun(_Uid) -> ok end}
        ], fun() ->
            Uid = 12345,

            Result = location_logic:make_myself_unvisible(Uid),
            ?assertEqual(ok, Result)
        end)
    end).

%% ===================================================================
%% people_nearby/6 测试
%% ===================================================================

people_nearby_kilometers_converts_to_meters_test_() ->
    ?WITH_MECK(geo_people_nearby_ds, [
        {'people_nearby', 5, fun(_Lng, _Lat, Radius, _Unit, _Limit) ->
            % 验证半径从公里转换为米
            ?assertEqual(1000, Radius),
            {ok, [
                #{<<"id">> => 1, <<"account">> => <<"user1">>, <<"nickname">> => <<"User One">>,
                  <<"avatar">> => <<"avatar1.jpg">>, <<"sign">> => <<"sign1">>, <<"gender">> => 1,
                  <<"region">> => <<"Beijing">>, <<"location">> => <<"location1">>, <<"distance">> => 500}
            ]}
        end}
    ], fun() ->
        ?WITH_MECK(friend_ds, [
            {'is_friend_fields', 3, fun(_FromUid, _ToUid, _Fields) -> {false, #{}} end}
        ], fun() ->
            ?WITH_MECK(elib_hashids, [
                {'encode', 1, fun(Id) -> <<"encoded_", (integer_to_binary(Id))/binary>> end}
            ], fun() ->
                Lng = <<"116.4074">>,
                Lat = <<"39.9042">>,
                Radius = <<"1">>,  % 1公里
                Unit = <<"km">>,
                Limit = 10,
                CurrentUid = 12345,

                Result = location_logic:people_nearby(CurrentUid, Lng, Lat, Radius, Unit, Limit),
                ?assertMatch([#{<<"id">> := <<"encoded_1">>} | _], Result)
            end)
        end)
    end).

people_nearby_meters_test_() ->
    ?WITH_MECK(geo_people_nearby_ds, [
        {'people_nearby', 5, fun(_Lng, _Lat, _Radius, _Unit, _Limit) ->
            {ok, [
                #{<<"id">> => 3, <<"account">> => <<"user3">>, <<"nickname">> => <<"User Three">>,
                  <<"avatar">> => <<"avatar3.jpg">>, <<"sign">> => <<"sign3">>, <<"gender">> => 1,
                  <<"region">> => <<"Guangzhou">>, <<"location">> => <<"location3">>, <<"distance">> => 1500}
            ]}
        end}
    ], fun() ->
        ?WITH_MECK(friend_ds, [
            {'is_friend_fields', 3, fun(_FromUid, _ToUid, _Fields) -> {true, #{<<"remark">> => <<"朋友"/utf8>>, <<"created_at">> => 123456}} end}
        ], fun() ->
            ?WITH_MECK(elib_hashids, [
                {'encode', 1, fun(Id) -> <<"encoded_", (integer_to_binary(Id))/binary>> end}
            ], fun() ->
                Lng = <<"113.264385">>,
                Lat = <<"23.129112">>,
                Radius = 2000,  % 2000米（整数）
                Unit = <<"m">>,
                Limit = 5,
                CurrentUid = 12345,

                Result = location_logic:people_nearby(CurrentUid, Lng, Lat, Radius, Unit, Limit),
                [User] = Result,
                ?assertEqual(<<"encoded_3">>, maps:get(<<"id">>, User)),
                ?assertEqual(true, maps:get(<<"is_friend">>, User)),
                ?assertEqual(<<"朋友"/utf8>>, maps:get(<<"remark">>, User)),
                ?assertEqual(123456, maps:get(<<"friend_created_at">>, User))
            end)
        end)
    end).

people_nearby_empty_result_test_() ->
    ?WITH_MECK(geo_people_nearby_ds, [
        {'people_nearby', 5, fun(_Lng, _Lat, _Radius, _Unit, _Limit) ->
            {ok, []}  % 没有找到附近的人
        end}
    ], fun() ->
        Lng = <<"116.4074">>,
        Lat = <<"39.9042">>,
        Radius = 100,
        Unit = <<"m">>,
        Limit = 10,
        CurrentUid = 12345,

        Result = location_logic:people_nearby(CurrentUid, Lng, Lat, Radius, Unit, Limit),
        ?assertEqual([], Result)
    end).

people_nearby_with_friend_test_() ->
    ?WITH_MECK(geo_people_nearby_ds, [
        {'people_nearby', 5, fun(_Lng, _Lat, _Radius, _Unit, _Limit) ->
            {ok, [
                #{<<"id">> => 2, <<"account">> => <<"user2">>, <<"nickname">> => <<"Friend User">>,
                  <<"avatar">> => <<"avatar2.jpg">>, <<"sign">> => <<"sign2">>, <<"gender">> => 2,
                  <<"region">> => <<"Shanghai">>, <<"location">> => <<"location2">>, <<"distance">> => 800}
            ]}
        end}
    ], fun() ->
        ?WITH_MECK(friend_ds, [
            {'is_friend_fields', 3, fun(_FromUid, _ToUid, _Fields) -> {true, #{<<"remark">> => <<"同事"/utf8>>, <<"created_at">> => 1640995200}} end}
        ], fun() ->
            ?WITH_MECK(elib_hashids, [
                {'encode', 1, fun(Id) -> <<"encoded_", (integer_to_binary(Id))/binary>> end}
            ], fun() ->
                Lng = <<"116.4074">>,
                Lat = <<"39.9042">>,
                Radius = 1000,
                Unit = <<"m">>,
                Limit = 10,
                CurrentUid = 12345,

                Result = location_logic:people_nearby(CurrentUid, Lng, Lat, Radius, Unit, Limit),
                [User] = Result,
                ?assertEqual(true, maps:get(<<"is_friend">>, User)),
                ?assertEqual(<<"同事"/utf8>>, maps:get(<<"remark">>, User)),
                ?assertEqual(1640995200, maps:get(<<"friend_created_at">>, User))
            end)
        end)
    end).

people_nearby_with_non_friend_test_() ->
    ?WITH_MECK(geo_people_nearby_ds, [
        {'people_nearby', 5, fun(_Lng, _Lat, _Radius, _Unit, _Limit) ->
            {ok, [
                #{<<"id">> => 3, <<"account">> => <<"user3">>, <<"nickname">> => <<"Stranger">>,
                  <<"avatar">> => <<"avatar3.jpg">>, <<"sign">> => <<"sign3">>, <<"gender">> => 1,
                  <<"region">> => <<"Beijing">>, <<"location">> => <<"location3">>, <<"distance">> => 300}
            ]}
        end}
    ], fun() ->
        ?WITH_MECK(friend_ds, [
            {'is_friend_fields', 3, fun(_FromUid, _ToUid, _Fields) -> {false, #{}} end}
        ], fun() ->
            ?WITH_MECK(elib_hashids, [
                {'encode', 1, fun(Id) -> <<"encoded_", (integer_to_binary(Id))/binary>> end}
            ], fun() ->
                Lng = <<"116.4074">>,
                Lat = <<"39.9042">>,
                Radius = 500,
                Unit = <<"m">>,
                Limit = 10,
                CurrentUid = 12345,

                Result = location_logic:people_nearby(CurrentUid, Lng, Lat, Radius, Unit, Limit),
                [User] = Result,
                ?assertEqual(false, maps:get(<<"is_friend">>, User)),
                ?assertEqual(<<>>, maps:get(<<"remark">>, User)),
                ?assertEqual(0, maps:get(<<"friend_created_at">>, User))
            end)
        end)
    end).

people_nearby_without_id_field_test_() ->
    ?WITH_MECK(geo_people_nearby_ds, [
        {'people_nearby', 5, fun(_Lng, _Lat, _Radius, _Unit, _Limit) ->
            {ok, [
                #{<<"account">> => <<"user1">>, <<"nickname">> => <<"User">>}  % 缺少 id 字段
            ]}
        end}
    ], fun() ->
        ?WITH_MECK(elib_hashids, [
            {'encode', 1, fun(Id) -> <<"encoded_", (integer_to_binary(Id))/binary>> end}
        ], fun() ->
            Lng = <<"116.4074">>,
            Lat = <<"39.9042">>,
            Radius = 100,
            Unit = <<"m">>,
            Limit = 10,
            CurrentUid = 12345,

            Result = location_logic:people_nearby(CurrentUid, Lng, Lat, Radius, Unit, Limit),
            % 没有 id 字段的数据应该原样返回
            [User] = Result,
            ?assertEqual(<<"user1">>, maps:get(<<"account">>, User))
        end)
    end).

