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
%% make_myself_visible/3 测试
%% ===================================================================

save_location_success_test_() ->
    ?WITH_MECKS(
        [
            {user_setting_ds, [
                {'save', 3, fun(_Uid, _Key, _Val) -> ok end}
            ]},
            {geo_people_nearby_ds, [
                {'save', 3, fun(_Uid, _Lat, _Lng) -> {ok, 1} end}
            ]}
        ],
        fun() ->
            Uid = 100,
            Lat = <<"39.9042">>,
            Lng = <<"116.4074">>,
            Result = location_logic:make_myself_visible(Uid, Lat, Lng),
            ?assertEqual(ok, Result)
        end
    ).

save_location_with_string_coordinates_test_() ->
    ?WITH_MECKS(
        [
            {user_setting_ds, [
                {'save', 3, fun(_Uid, _Key, _Val) -> ok end}
            ]},
            {geo_people_nearby_ds, [
                {'save', 3, fun(_Uid, _Lat, _Lng) -> {ok, 1} end}
            ]}
        ],
        fun() ->
            Uid = 100,
            Lat = <<"39.9042">>,
            Lng = <<"116.4074">>,
            Result = location_logic:make_myself_visible(Uid, Lat, Lng),
            ?assertEqual(ok, Result)
        end
    ).

save_location_with_number_coordinates_test_() ->
    ?WITH_MECKS(
        [
            {user_setting_ds, [
                {'save', 3, fun(_Uid, _Key, _Val) -> ok end}
            ]},
            {geo_people_nearby_ds, [
                {'save', 3, fun(_Uid, _Lat, _Lng) -> {ok, 1} end}
            ]}
        ],
        fun() ->
            Uid = 100,
            Lat = <<"39.9042">>,
            Lng = <<"116.4074">>,
            Result = location_logic:make_myself_visible(Uid, Lat, Lng),
            ?assertEqual(ok, Result)
        end
    ).

%% ===================================================================
%% people_nearby/6 测试
%% ===================================================================

people_nearby_success_test_() ->
    ?WITH_MECKS(
        [
            {geo_people_nearby_ds, [
                {'people_nearby', 5, fun(_Lng, _Lat, _Radius, _Unit, _Limit) ->
                    {ok, [
                        #{<<"id">> => 101, <<"distance">> => 100},
                        #{<<"id">> => 102, <<"distance">> => 200}
                    ]}
                end}
            ]},
            {friend_ds, [
                %% 生产已从 is_friend_fields 重构为批量 friends_fields_map（N+1 → 批量）
                {'friends_fields_map', 3, fun(_CurrentUid, UserIds, _Fields) ->
                    maps:from_list([
                        {Uid, #{
                            <<"remark">> => <<"好友"/utf8>>,
                            <<"created_at">> => 1000
                        }}
                     || Uid <- UserIds
                    ])
                end}
            ]}
        ],
        fun() ->
            CurrentUid = 1,
            Lng = <<"116.4074">>,
            Lat = <<"39.9042">>,
            Radius = 1000,
            Unit = <<"m">>,
            Limit = 10,
            Result = location_logic:people_nearby(CurrentUid, Lng, Lat, Radius, Unit, Limit),
            ?assert(is_list(Result))
        end
    ).

people_nearby_with_empty_result_test_() ->
    ?WITH_MECK(
        geo_people_nearby_ds,
        [
            {'people_nearby', 5, fun(_Lng, _Lat, _Radius, _Unit, _Limit) ->
                {ok, []}
            end}
        ],
        fun() ->
            CurrentUid = 1,
            Lng = <<"116.4074">>,
            Lat = <<"39.9042">>,
            Radius = 10,
            Unit = <<"m">>,
            Limit = 10,
            Result = location_logic:people_nearby(CurrentUid, Lng, Lat, Radius, Unit, Limit),
            ?assertEqual([], Result)
        end
    ).

people_nearby_with_kilometer_unit_test_() ->
    ?WITH_MECKS(
        [
            {geo_people_nearby_ds, [
                {'people_nearby', 5, fun(_Lng, _Lat, _Radius, _Unit, _Limit) ->
                    {ok, [#{<<"id">> => 101}]}
                end}
            ]},
            {friend_ds, [
                %% 生产已从 is_friend_fields 重构为批量 friends_fields_map（N+1 → 批量）
                {'friends_fields_map', 3, fun(_CurrentUid, UserIds, _Fields) ->
                    maps:from_list([
                        {Uid, #{
                            <<"remark">> => <<"好友"/utf8>>,
                            <<"created_at">> => 1000
                        }}
                     || Uid <- UserIds
                    ])
                end}
            ]}
        ],
        fun() ->
            CurrentUid = 1,
            Lng = <<"116.4074">>,
            Lat = <<"39.9042">>,
            Radius = <<"1">>,
            Unit = <<"km">>,
            Limit = 10,
            Result = location_logic:people_nearby(CurrentUid, Lng, Lat, Radius, Unit, Limit),
            ?assert(is_list(Result))
        end
    ).

people_nearby_with_custom_limit_test_() ->
    ?WITH_MECKS(
        [
            {geo_people_nearby_ds, [
                {'people_nearby', 5, fun(_Lng, _Lat, _Radius, _Unit, _Limit) ->
                    {ok, lists:duplicate(20, #{<<"id">> => 101})}
                end}
            ]},
            {friend_ds, [
                %% 生产已从 is_friend_fields 重构为批量 friends_fields_map（N+1 → 批量）
                {'friends_fields_map', 3, fun(_CurrentUid, UserIds, _Fields) ->
                    maps:from_list([
                        {Uid, #{
                            <<"remark">> => <<"好友"/utf8>>,
                            <<"created_at">> => 1000
                        }}
                     || Uid <- UserIds
                    ])
                end}
            ]}
        ],
        fun() ->
            CurrentUid = 1,
            Lng = <<"116.4074">>,
            Lat = <<"39.9042">>,
            Radius = 1000,
            Unit = <<"m">>,
            Limit = 20,
            Result = location_logic:people_nearby(CurrentUid, Lng, Lat, Radius, Unit, Limit),
            ?assertEqual(20, length(Result))
        end
    ).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

people_nearby_with_zero_radius_test_() ->
    ?WITH_MECK(
        geo_people_nearby_ds,
        [
            {'people_nearby', 5, fun(_Lng, _Lat, _Radius, _Unit, _Limit) ->
                {ok, []}
            end}
        ],
        fun() ->
            CurrentUid = 1,
            Lng = <<"116.4074">>,
            Lat = <<"39.9042">>,
            Radius = 0,
            Unit = <<"m">>,
            Limit = 10,
            Result = location_logic:people_nearby(CurrentUid, Lng, Lat, Radius, Unit, Limit),
            ?assertEqual([], Result)
        end
    ).

people_nearby_with_large_radius_test_() ->
    ?WITH_MECKS(
        [
            {geo_people_nearby_ds, [
                {'people_nearby', 5, fun(_Lng, _Lat, _Radius, _Unit, _Limit) ->
                    {ok, [#{<<"id">> => 101}]}
                end}
            ]},
            {friend_ds, [
                %% 生产已从 is_friend_fields 重构为批量 friends_fields_map（N+1 → 批量）
                {'friends_fields_map', 3, fun(_CurrentUid, UserIds, _Fields) ->
                    maps:from_list([
                        {Uid, #{
                            <<"remark">> => <<"好友"/utf8>>,
                            <<"created_at">> => 1000
                        }}
                     || Uid <- UserIds
                    ])
                end}
            ]}
        ],
        fun() ->
            CurrentUid = 1,
            Lng = <<"116.4074">>,
            Lat = <<"39.9042">>,
            Radius = 50000,
            Unit = <<"m">>,
            Limit = 10,
            Result = location_logic:people_nearby(CurrentUid, Lng, Lat, Radius, Unit, Limit),
            ?assert(is_list(Result))
        end
    ).

save_with_zero_coordinates_test_() ->
    ?WITH_MECKS(
        [
            {user_setting_ds, [
                {'save', 3, fun(_Uid, _Key, _Val) -> ok end}
            ]},
            {geo_people_nearby_ds, [
                {'save', 3, fun(_Uid, _Lat, _Lng) -> {ok, 1} end}
            ]}
        ],
        fun() ->
            Uid = 100,
            Lat = <<"0">>,
            Lng = <<"0">>,
            Result = location_logic:make_myself_visible(Uid, Lat, Lng),
            ?assertEqual(ok, Result)
        end
    ).

people_nearby_with_negative_coordinates_test_() ->
    ?WITH_MECKS(
        [
            {geo_people_nearby_ds, [
                {'people_nearby', 5, fun(_Lng, _Lat, _Radius, _Unit, _Limit) ->
                    {ok, [#{<<"id">> => 101}]}
                end}
            ]},
            {friend_ds, [
                %% 生产已从 is_friend_fields 重构为批量 friends_fields_map（N+1 → 批量）
                {'friends_fields_map', 3, fun(_CurrentUid, UserIds, _Fields) ->
                    maps:from_list([
                        {Uid, #{
                            <<"remark">> => <<"好友"/utf8>>,
                            <<"created_at">> => 1000
                        }}
                     || Uid <- UserIds
                    ])
                end}
            ]}
        ],
        fun() ->
            CurrentUid = 1,
            Lng = <<"-122.4194">>,
            Lat = <<"-37.7749">>,
            Radius = 1000,
            Unit = <<"m">>,
            Limit = 10,
            Result = location_logic:people_nearby(CurrentUid, Lng, Lat, Radius, Unit, Limit),
            ?assert(is_list(Result))
        end
    ).
