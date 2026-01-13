-module(geo_people_nearby_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% geo_people_nearby_repo 模块的 EUnit 测试
%%%
%%% 目标：验证附近的人数据访问层功能
%%% 覆盖：位置查询、更新
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
%% 位置查询测试
%% ===================================================================

find_nearby_people_test_() ->
    ?WITH_MECK(elib_pg, [
        {'query', 2, fun(Sql, Params) ->
            % 验证SQL查询包含地理位置查询
            ?assert(binary:match(Sql, <<"SELECT.*FROM.*geo_people_nearby">>) =/= nomatch),
            ?assert(binary:match(Sql, <<"WHERE.*ST_DWithin">>) =/= nomatch),
            % 验证参数包含坐标和半径
            ?assert(length(Params) >= 3),
            % 返回模拟的附近的人
            {ok, [{2, <<"user2">>, <<"User Two">>, <<"avatar2.jpg">>, <<"sign2">>, 500},
                  {3, <<"user3">>, <<"User Three">>, <<"avatar3.jpg">>, <<"sign3">>, 800}]}
        end}
    ], fun() ->
        Lat = 39.9042,
        Lng = 116.4074,
        Radius = 1000,
        
        Result = geo_people_nearby_repo:people_nearby(Lng, Lat, Radius, <<"m">>, 10),
        ?ASSERT_OK(Result),
        {ok, Count, People} = Result,
        % 验证返回的附近的人数据
        ?assertEqual(2, Count),
        ?assert(length(People) >= 2),
        % 验证第一个人的数据
        [Person1, _Person2 | _] = People,
        ?assertEqual(2, element(1, Person1)),
        ?assertEqual(<<"user2">>, element(2, Person1)),
        ?assertEqual(<<"User Two">>, element(3, Person1)),
        ?assertEqual(500, element(6, Person1))
    end).

%% ===================================================================
%% 位置更新测试
%% ===================================================================

update_location_test_() ->
    ?WITH_MECK(elib_pg, [
        {'execute', 3, fun(Sql, Params) ->
            % 验证SQL包含位置更新
            ?assert(binary:match(Sql, <<"INSERT.*INTO.*geo_people_nearby">>) =/= nomatch),
            ?assert(binary:match(Sql, <<"ON CONFLICT.*DO UPDATE">>) =/= nomatch),
            % 验证参数包含用户ID和坐标
            ?assert(length(Params) >= 3),
            ?assert(lists:member(1, Params)),
            ?assert(lists:member(39.9042, Params)),
            ?assert(lists:member(116.4074, Params)),
            {ok, 1}
        end}
    ], fun() ->
        Uid = 1,
        Lat = 39.9042,
        Lng = 116.4074,
        
        Result = geo_people_nearby_repo:save(Uid, Lat, Lng),
        ?assertEqual(ok, Result)
    end).
