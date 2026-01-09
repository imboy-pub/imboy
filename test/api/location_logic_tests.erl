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
        ?WITH_MECK(geo_people_nearby_repo, [
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

%% ===================================================================
%% make_myself_unvisible/1 测试
%% ===================================================================

make_myself_unvisible_success_test_() ->
    ?WITH_MECK(user_setting_ds, [
        {'save', 3, fun(_Uid, _Key, _Value) -> ok end}
    ], fun() ->
        ?WITH_MECK(geo_people_nearby_repo, [
            {'delete', 1, fun(_Uid) -> ok end}
        ], fun() ->
            Uid = 12345,
            
            Result = location_logic:make_myself_unvisible(Uid),
            ?assertEqual(ok, Result)
        end)
    end).

%% ===================================================================
%% people_nearby/5 测试
%% ===================================================================

people_nearby_kilometers_test_() ->
    ?WITH_MECK(geo_people_nearby_repo, [
        {'people_nearby', 5, fun(_Lng, _Lat, _Radius, _Unit, _Limit) ->
            {ok, 2, [
                {1, <<"user1">>, <<"User One">>, <<"avatar1.jpg">>, <<"sign1">>, 1, <<"Beijing">>, <<"location1">>, 500},
                {2, <<"user2">>, <<"User Two">>, <<"avatar2.jpg">>, <<"sign2">>, 2, <<"Shanghai">>, <<"location2">>, 800}
            ]}
        end}
    ], fun() ->
        ?WITH_MECK(imboy_hashids, [
            {'replace_id', 1, fun(UserData) ->
                % 模拟ID替换
                maps:put(<<"id">>, <<"encoded_1">>, maps:from_list(UserData))
            end}
        ], fun() ->
            Lng = <<"116.4074">>,
            Lat = <<"39.9042">>,
            Radius = <<"1">>,  % 1公里
            Unit = <<"km">>,
            Limit = <<"10">>,
            
            Result = location_logic:people_nearby(Lng, Lat, Radius, Unit, Limit),
            
            % 验证返回结果格式
            ?assertMatch([_|_], Result),
            ?assertEqual(2, length(Result)),
            
            % 验证第一个用户数据
            [User1, _User2] = Result,
            ?assertEqual(<<"encoded_1">>, maps:get(<<"id">>, User1)),
            ?assertEqual(<<"user1">>, maps:get(<<"account">>, User1)),
            ?assertEqual(<<"User One">>, maps:get(<<"nickname">>, User1)),
            ?assertEqual(500, maps:get(<<"distance">>, User1))
        end)
    end).

people_nearby_meters_test_() ->
    ?WITH_MECK(geo_people_nearby_repo, [
        {'people_nearby', 5, fun(_Lng, _Lat, _Radius, _Unit, _Limit) ->
            {ok, 1, [
                {3, <<"user3">>, <<"User Three">>, <<"avatar3.jpg">>, <<"sign3">>, 1, <<"Guangzhou">>, <<"location3">>, 1500}
            ]}
        end}
    ], fun() ->
        ?WITH_MECK(imboy_hashids, [
            {'replace_id', 1, fun(UserData) ->
                maps:put(<<"id">>, <<"encoded_3">>, maps:from_list(UserData))
            end}
        ], fun() ->
            Lng = <<"113.264385">>,
            Lat = <<"23.129112">>,
            Radius = <<"2000">>,  % 2000米
            Unit = <<"m">>,
            Limit = <<"5">>,
            CurrentUid = 12345,
            
            Result = location_logic:people_nearby(CurrentUid, Lng, Lat, Radius, Unit, Limit),
            
            % 验证返回结果
            ?assertMatch([_|_], Result),
            ?assertEqual(1, length(Result)),
            
            [User] = Result,
            ?assertEqual(<<"encoded_3">>, maps:get(<<"id">>, User)),
            ?assertEqual(<<"user3">>, maps:get(<<"account">>, User)),
            ?assertEqual(<<"User Three">>, maps:get(<<"nickname">>, User)),
            ?assertEqual(1500, maps:get(<<"distance">>, User))
        end)
    end).

people_nearby_empty_result_test_() ->
    ?WITH_MECK(geo_people_nearby_repo, [
        {'people_nearby', 5, fun(_Lng, _Lat, _Radius, _Unit, _Limit) ->
            {ok, []}  % 没有找到附近的人
        end}
    ], fun() ->
        ?WITH_MECK(imboy_hashids, [
            {'replace_id', 1, fun(Row) -> Row end}
        ], fun() ->
            ?WITH_MECK(friend_ds, [
                {'is_friend_fields', 3, fun(_FromUid, _ToUid, _Fields) -> {false, #{}} end}
            ], fun() ->
                Lng = <<"116.4074">>,
                Lat = <<"39.9042">>,
                Radius = 100,
                Unit = <<"m">>,
                Limit = <<"10">>,
                CurrentUid = 12345,
                
                Result = location_logic:people_nearby(CurrentUid, Lng, Lat, Radius, Unit, Limit),
                ?assertEqual([], Result)
            end)
        end)
    end).
