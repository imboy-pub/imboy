-module(geo_people_nearby_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% geo_people_nearby_repo 模块的 EUnit 测试
%%%
%%% 目标：验证附近的人数据访问层功能
%%% 覆盖：位置查询、保存、删除
%%%===================================================================

%% ===================================================================
%% tablename/0 测试
%% ===================================================================

tablename_returns_correct_table_test_() ->
    ?WITH_MECK(elib_pg_sql, [
        {'public_tablename', 1, fun(_Table) -> <<"public.geo_people_nearby">> end}
    ], fun() ->
        Result = geo_people_nearby_repo:tablename(),
        ?assertEqual(<<"public.geo_people_nearby">>, Result)
    end).

%% ===================================================================
%% save/3 测试
%% ===================================================================

save_location_new_user_test_() ->
    ?WITH_MECK(elib_pg, [
        {'execute', 2, fun(_Sql, _Params) -> {ok, 1} end}
    ], fun() ->
        Uid = 12345,
        Lat = <<"39.9042">>,
        Lng = <<"116.4074">>,

        Result = geo_people_nearby_repo:save(Uid, Lat, Lng),
        ?assertEqual({ok, 1}, Result)
    end).

save_location_update_existing_test_() ->
    ?WITH_MECK(elib_pg, [
        {'execute', 2, fun(_Sql, _Params) -> {ok, 1} end}
    ], fun() ->
        % 测试更新已存在的用户位置（ON CONFLICT UPDATE）
        Uid = 12345,
        Lat = <<"39.9150">>,
        Lng = <<"116.4040">>,

        Result = geo_people_nearby_repo:save(Uid, Lat, Lng),
        ?assertEqual({ok, 1}, Result)
    end).

save_location_with_different_coordinates_test_() ->
    ?WITH_MECK(elib_pg, [
        {'execute', 2, fun(_Sql, _Params) -> {ok, 1} end}
    ], fun() ->
        % 测试不同的坐标位置
        Uid = 67890,
        Lat = <<"31.2304">>,  % 上海
        Lng = <<"121.4737">>,

        Result = geo_people_nearby_repo:save(Uid, Lat, Lng),
        ?assertEqual({ok, 1}, Result)
    end).

save_location_error_test_() ->
    ?WITH_MECK(elib_pg, [
        {'execute', 2, fun(_Sql, _Params) -> {error, invalid_coordinates} end}
    ], fun() ->
        Uid = 12345,
        Lat = <<"invalid">>,
        Lng = <<"coordinates">>,

        Result = geo_people_nearby_repo:save(Uid, Lat, Lng),
        ?assertEqual({error, invalid_coordinates}, Result)
    end).

%% ===================================================================
%% delete/1 测试
%% ===================================================================

delete_location_success_test_() ->
    ?WITH_MECK(elib_pg, [
        {'execute', 2, fun(_Sql, _Params) -> {ok, 1} end}
    ], fun() ->
        Uid = 12345,

        Result = geo_people_nearby_repo:delete(Uid),
        ?assertEqual({ok, 1}, Result)
    end).

delete_location_user_not_found_test_() ->
    ?WITH_MECK(elib_pg, [
        {'execute', 2, fun(_Sql, _Params) -> {ok, 0} end}
    ], fun() ->
        Uid = 99999,

        Result = geo_people_nearby_repo:delete(Uid),
        ?assertEqual({ok, 0}, Result)
    end).

delete_location_error_test_() ->
    ?WITH_MECK(elib_pg, [
        {'execute', 2, fun(_Sql, _Params) -> {error, connection_failed} end}
    ], fun() ->
        Uid = 12345,

        Result = geo_people_nearby_repo:delete(Uid),
        ?assertEqual({error, connection_failed}, Result)
    end).

%% ===================================================================
%% people_nearby/5 测试
%% ===================================================================

people_nearby_found_users_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            % 返回附近用户的查询结果
            {ok, [{<<"id">>, <<"account">>, <<"nickname">>, <<"avatar">>, <<"sign">>, <<"gender">>, <<"region">>,
                   <<"POINT(116.4074 39.9042)">>, <<"distance">>}]}
        end}
    ], fun() ->
        Lng = <<"116.4074">>,
        Lat = <<"39.9042">>,
        Radius = 1000,
        Unit = <<"m">>,
        Limit = 10,

        Result = geo_people_nearby_repo:people_nearby(Lng, Lat, Radius, Unit, Limit),
        ?assertMatch({ok, _}, Result)
    end).

people_nearby_no_users_found_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            % 返回空结果
            {ok, []}
        end}
    ], fun() ->
        Lng = <<"116.4074">>,
        Lat = <<"39.9042">>,
        Radius = 100,
        Unit = <<"m">>,
        Limit = 10,

        Result = geo_people_nearby_repo:people_nearby(Lng, Lat, Radius, Unit, Limit),
        ?assertMatch({ok, []}, Result)
    end).

people_nearby_with_different_radius_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            {ok, [{<<"id">>, <<"account">>, <<"nickname">>, <<"avatar">>, <<"sign">>, <<"gender">>, <<"region">>,
                   <<"POINT(116.4074 39.9042)">>, <<"distance">>}]}
        end}
    ], fun() ->
        Lng = <<"116.4074">>,
        Lat = <<"39.9042">>,
        Radius = 5000,
        Unit = <<"m">>,
        Limit = 20,

        Result = geo_people_nearby_repo:people_nearby(Lng, Lat, Radius, Unit, Limit),
        ?assertMatch({ok, _}, Result)
    end).

people_nearby_database_error_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(_Sql, _Params) ->
            {error, database_error}
        end}
    ], fun() ->
        Lng = <<"116.4074">>,
        Lat = <<"39.9042">>,
        Radius = 1000,
        Unit = <<"m">>,
        Limit = 10,

        Result = geo_people_nearby_repo:people_nearby(Lng, Lat, Radius, Unit, Limit),
        ?assertEqual({error, database_error}, Result)
    end).

%% ===================================================================
%% 集成测试
%% ===================================================================

save_find_and_delete_location_flow_test_() ->
    ?WITH_MECK(elib_pg, [
        {'execute', 2, fun(_Sql, _Params) -> {ok, 1} end},
        {'query', 2, fun(_Sql, _Params) ->
            {ok, [{<<"id">>, <<"account">>, <<"nickname">>, <<"avatar">>, <<"sign">>, <<"gender">>, <<"region">>,
                   <<"POINT(116.4074 39.9042)">>, <<"distance">>}]}
        end}
    ], fun() ->
        Uid = 12345,
        Lat = <<"39.9042">>,
        Lng = <<"116.4074">>,

        % 1. 保存位置
        ?assertEqual({ok, 1}, geo_people_nearby_repo:save(Uid, Lat, Lng)),

        % 2. 查询附近的人
        Result = geo_people_nearby_repo:people_nearby(Lng, Lat, 1000, <<"m">>, 10),
        ?assertMatch({ok, _}, Result),

        % 3. 删除位置
        ?assertEqual({ok, 1}, geo_people_nearby_repo:delete(Uid))
    end).

update_location_and_query_nearby_flow_test_() ->
    ?WITH_MECK(elib_pg, [
        {'execute', 2, fun(_Sql, _Params) -> {ok, 1} end},
        {'query', 2, fun(_Sql, _Params) ->
            {ok, [{<<"id">>, <<"account">>, <<"nickname">>, <<"avatar">>, <<"sign">>, <<"gender">>, <<"region">>,
                   <<"POINT(121.4737 31.2304)">>, <<"distance">>}]}
        end}
    ], fun() ->
        Uid = 12345,
        Lat1 = <<"39.9042">>,
        Lng1 = <<"116.4074">>,
        Lat2 = <<"31.2304">>,
        Lng2 = <<"121.4737">>,

        % 1. 初始位置（北京）
        ?assertEqual({ok, 1}, geo_people_nearby_repo:save(Uid, Lat1, Lng1)),

        % 2. 更新位置（上海）
        ?assertEqual({ok, 1}, geo_people_nearby_repo:save(Uid, Lat2, Lng2)),

        % 3. 查询附近的人
        Result = geo_people_nearby_repo:people_nearby(Lng2, Lat2, 1000, <<"m">>, 10),
        ?assertMatch({ok, _}, Result)
    end).
