-module(group_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% group_logic 模块的 EUnit 测试
%%%
%%% 目标：验证群组业务逻辑功能
%%% 覆盖：面对面建群、群组创建、解散、附近群组、数据转换
%%%===================================================================

%% ===================================================================
%% group_transfer/1 测试
%% ===================================================================

group_transfer_returns_input_unchanged_test_() ->
    ?TEST_SIMPLE(fun() ->
        Input = #{
            <<"id">> => 1,
            <<"creator_uid">> => 100,
            <<"owner_uid">> => 100,
            <<"gid">> => 1,
            <<"name">> => <<"测试群"/utf8>>
        },
        Result = group_logic:group_transfer(Input),
        ?assertEqual(Input, Result)
    end).

group_transfer_with_empty_map_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = group_logic:group_transfer(#{}),
        ?assertEqual(#{}, Result)
    end).

%% ===================================================================
%% face2face/4 测试
%% ===================================================================

face2face_with_empty_code_returns_error_test_() ->
    ?TEST_SIMPLE(fun() ->
        Uid = 123,
        Code = <<>>,
        Lng = <<"116.4074">>,
        Lat = <<"39.9042">>,

        Result = group_logic:face2face(Uid, Code, Lng, Lat),
        ?assertEqual({error, <<"Code 必须"/utf8>>}, Result)
    end).

face2face_with_undefined_lng_returns_error_test_() ->
    ?TEST_SIMPLE(fun() ->
        Uid = 123,
        Code = <<"ABC123">>,
        Lng = undefined,
        Lat = <<"39.9042">>,

        Result = group_logic:face2face(Uid, Code, Lng, Lat),
        ?assertEqual({error, <<"longitude 必须"/utf8>>}, Result)
    end).

face2face_with_undefined_lat_returns_error_test_() ->
    ?TEST_SIMPLE(fun() ->
        Uid = 123,
        Code = <<"ABC123">>,
        Lng = <<"116.4074">>,
        Lat = undefined,

        Result = group_logic:face2face(Uid, Code, Lng, Lat),
        ?assertEqual({error, <<"latitude 必须"/utf8>>}, Result)
    end).

face2face_creates_new_group_when_no_nearby_test_() ->
    ?WITH_MECKS([
        {group_ds, [
            {'nearby_gid', 6, fun(_Lng, _Lat, _Radius, _Unit, _Limit, _Code) ->
                {ok, []}
            end},
            {'face2face_create', 5, fun(_Conn, _Uid, _Code, _Lng, _Lat) ->
                {ok, 999}
            end}
        ]},
        {elib_pg, [
            {'with_tx', 1, fun(Fun) -> Fun(self()) end}
        ]}
    ], fun() ->
        Uid = 123,
        Code = <<"ABC123">>,
        Lng = <<"116.4074">>,
        Lat = <<"39.9042">>,

        Result = group_logic:face2face(Uid, Code, Lng, Lat),
        ?assertMatch({ok, 999}, Result)
    end).

face2face_joins_existing_group_test_() ->
    ?WITH_MECKS([
        {group_ds, [
            {'nearby_gid', 6, fun(_Lng, _Lat, _Radius, _Unit, _Limit, _Code) ->
                {ok, [#{<<"group_id">> => 888}]}
            end}
        ]},
        {group_member_logic, [
            {'join_group', 4, fun(_JoinMode, _Uid, _Gid, _Data) -> ok end}
        ]}
    ], fun() ->
        Uid = 123,
        Code = <<"ABC123">>,
        Lng = <<"116.4074">>,
        Lat = <<"39.9042">>,

        Result = group_logic:face2face(Uid, Code, Lng, Lat),
        ?assertMatch({ok, 888}, Result)
    end).

face2face_with_multiple_nearby_groups_fails_test_() ->
    ?WITH_MECK(group_ds, [
        {'nearby_gid', 6, fun(_Lng, _Lat, _Radius, _Unit, _Limit, _Code) ->
            {ok, [
                #{<<"group_id">> => 888},
                #{<<"group_id">> => 999}
            ]}
        end}
    ], fun() ->
        Uid = 123,
        Code = <<"ABC123">>,
        Lng = <<"116.4074">>,
        Lat = <<"39.9042">>,

        Result = group_logic:face2face(Uid, Code, Lng, Lat),
        ?assertEqual({error, "error"}, Result)
    end).

%% ===================================================================
%% face2face_save/3 测试
%% ===================================================================

face2face_save_success_test_() ->
    ?WITH_MECK(group_ds, [
        {'face2face_save', 3, fun(_Code, _Gid, _Uid) -> {ok, <<"saved">>} end}
    ], fun() ->
        Code = <<"ABC123">>,
        Gid = 999,
        Uid = 123,

        Result = group_logic:face2face_save(Code, Gid, Uid),
        ?assertEqual({ok, <<"saved">>}, Result)
    end).

%% ===================================================================
%% add/4 测试
%% ===================================================================

add_with_exceeding_count_limit_returns_error_test_() ->
    ?TEST_SIMPLE(fun() ->
        Count = 101,
        Uid = 123,
        Type = 1,
        MemberUids = [<<"1">>, <<"2">>],

        Result = group_logic:add(Count, Uid, Type, MemberUids),
        ?assertEqual({error, <<"每人最多创建100个群"/utf8>>}, Result)
    end).

add_creates_new_group_success_test_() ->
    ?WITH_MECKS([
        {elib_dt, [
            {'now', 0, fun() -> <<"2023-01-01T00:00:00Z">> end}
        ]},
        {group_ds, [
            {'find_by_creator_and_sum', 2, fun(_Uid, _Sum) -> 0 end},
            {'create_group', 6, fun(_Conn, _Gid, _Uid, _Now, _Type, _Status) -> 999 end}
        ]},
        {user_ds, [
            {'title', 1, fun(_Uid) -> <<"user123">> end}
        ]},
        {group_member_logic, [
            {'join_group', 5, fun(_Conn, _Mode, _Uid, _Gid, _Data) -> ok end}
        ]},
        {elib_pg, [
            {'with_tx', 1, fun(Fun) -> Fun(self()) end}
        ]}
    ], fun() ->
        Count = 5,
        Uid = 123,
        Type = 1,
        MemberUids = [<<"1">>, <<"2">>],

        Result = group_logic:add(Count, Uid, Type, MemberUids),
        ?assertMatch({ok, _Gid}, Result)
    end).

add_with_existing_group_returns_existing_gid_test_() ->
    ?WITH_MECKS([
        {elib_dt, [
            {'now', 0, fun() -> <<"2023-01-01T00:00:00Z">> end}
        ]},
        {group_ds, [
            {'find_by_creator_and_sum', 2, fun(_Uid, _Sum) -> 888 end}
        ]}
    ], fun() ->
        Count = 5,
        Uid = 123,
        Type = 1,
        MemberUids = [<<"1">>],

        Result = group_logic:add(Count, Uid, Type, MemberUids),
        ?assertEqual({ok, 888}, Result)
    end).

add_with_empty_member_list_test_() ->
    ?WITH_MECKS([
        {elib_dt, [
            {'now', 0, fun() -> <<"2023-01-01T00:00:00Z">> end}
        ]},
        {group_ds, [
            {'find_by_creator_and_sum', 2, fun(_Uid, _Sum) -> 0 end},
            {'create_group', 6, fun(_Conn, _Gid, _Uid, _Now, _Type, _Status) -> 999 end}
        ]},
        {user_ds, [
            {'title', 1, fun(_Uid) -> <<"user123">> end}
        ]},
        {elib_pg, [
            {'with_tx', 1, fun(Fun) -> Fun(self()) end}
        ]}
    ], fun() ->
        Count = 5,
        Uid = 123,
        Type = 1,
        MemberUids = [],

        Result = group_logic:add(Count, Uid, Type, MemberUids),
        ?assertMatch({ok, _Gid}, Result)
    end).

add_with_invalid_member_list_test_() ->
    ?WITH_MECKS([
        {elib_dt, [
            {'now', 0, fun() -> <<"2023-01-01T00:00:00Z">> end}
        ]},
        {group_ds, [
            {'find_by_creator_and_sum', 2, fun(_Uid, _Sum) -> 0 end},
            {'create_group', 6, fun(_Conn, _Gid, _Uid, _Now, _Type, _Status) -> 999 end}
        ]},
        {user_ds, [
            {'title', 1, fun(_Uid) -> <<"user123">> end}
        ]},
        {elib_pg, [
            {'with_tx', 1, fun(Fun) -> Fun(self()) end}
        ]}
    ], fun() ->
        Count = 5,
        Uid = 123,
        Type = 1,
        MemberUids = invalid,

        Result = group_logic:add(Count, Uid, Type, MemberUids),
        ?assertMatch({ok, _Gid}, Result)
    end).

%% ===================================================================
%% dissolve/4 测试
%% ===================================================================

dissolve_success_test_() ->
    ?WITH_MECK(group_ds, [
        {'dissolve_group', 4, fun(_Uid, _Gid, _OwnerUid, _G) -> ok end}
    ], fun() ->
        Uid = 123,
        Gid = 999,
        OwnerUid = 123,
        G = #{<<"id">> => 999},

        Result = group_logic:dissolve(Uid, Gid, OwnerUid, G),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% nearby_gid/6 测试
%% ===================================================================

nearby_gid_returns_groups_test_() ->
    ?WITH_MECK(group_ds, [
        {'nearby_gid', 6, fun(_Lng, _Lat, _Radius, _Unit, _Limit, _Code) ->
            {ok, [#{<<"group_id">> => 888, <<"name">> => <<"附近群"/utf8>>}]}
        end}
    ], fun() ->
        Lng = <<"116.4074">>,
        Lat = <<"39.9042">>,
        Radius = <<"50">>,
        Unit = <<"m">>,
        Limit = <<"1">>,
        Code = <<"ABC123">>,

        Result = group_logic:nearby_gid(Lng, Lat, Radius, Unit, Limit, Code),
        ?assertMatch({ok, [_ | _]}, Result)
    end).

nearby_gid_with_no_results_test_() ->
    ?WITH_MECK(group_ds, [
        {'nearby_gid', 6, fun(_Lng, _Lat, _Radius, _Unit, _Limit, _Code) ->
            {ok, []}
        end}
    ], fun() ->
        Lng = <<"116.4074">>,
        Lat = <<"39.9042">>,
        Radius = <<"50">>,
        Unit = <<"m">>,
        Limit = <<"1">>,
        Code = <<"XYZ789">>,

        Result = group_logic:nearby_gid(Lng, Lat, Radius, Unit, Limit, Code),
        ?assertEqual({ok, []}, Result)
    end).
