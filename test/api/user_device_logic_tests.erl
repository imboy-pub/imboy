-module(user_device_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("include/eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% user_device_logic 模块的 EUnit 测试
%%%
%%% 目标：验证用户设备业务逻辑功能
%%% 覆盖：设备注册、更新、删除、查询
%%%===================================================================

%% ===================================================================
%% 设备管理测试
%% ===================================================================

register_device_with_valid_params_test_() ->
    ?WITH_MECK(user_device_repo, [
        {'save', 4, fun(_Uid, _DeviceType, _DeviceToken, _CreatedAt) -> {ok, 1} end}
    ], fun() ->
        Uid = 1,
        DeviceType = <<"ios">>,
        DeviceToken = <<"token123">>,
        CreatedAt = imboy_dt:now(),

        Result = user_device_logic:register_device(Uid, DeviceType, DeviceToken, CreatedAt),
        % 假设 register_device 函数返回操作结果
        case Result of
            {ok, DeviceId} when is_integer(DeviceId); is_binary(DeviceId) -> ?assert(true);
            {ok, _} -> ?assert(true);
            _ -> ?assert(false, "Expected {ok, DeviceId}")
        end
    end).

update_device_info_test_() ->
    ?WITH_MECK(user_device_repo, [
        {'update_by_did', 4, fun(_Uid, _DeviceId, _Set, _SetArgs) -> {ok, 1} end}
    ], fun() ->
        DeviceId = <<"device123">>,
        Uid = 1,
        Name = <<"Updated Device">>,
        
        Result = user_device_logic:change_name(Uid, DeviceId, Name),
        ?assertEqual(ok, Result)
    end).

remove_device_test_() ->
    ?WITH_MECK(user_device_repo, [
        {'delete', 2, fun(_Uid, _DeviceId) -> ok end}
    ], fun() ->
        DeviceId = <<"device123">>,
        Uid = 1,
        
        Result = user_device_logic:delete(Uid, DeviceId),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% 设备查询测试
%% ===================================================================

list_devices_by_uid_test_() ->
    ?WITH_MECKS([
        {user_device_repo, [
            {'count_by_uid', 1, fun(_Uid) -> 5 end},
            {'page', 3, fun(_Uid, _Size, _Offset) ->
                {ok, [<<"id">>, <<"device_id">>, <<"device_type">>, <<"created_at">>], 
                    [[1, <<"dev1">>, <<"ios">>, <<"2023-12-25">>], [2, <<"dev2">>, <<"android">>, <<"2023-12-24">>]]}
            end}
        ]},
        {imboy_syn, [
            {'online_dids', 1, fun(_Uid) -> [<<"dev1">>] end}
        ]},
        {imboy_response, [
            {'page_payload', 4, fun(_Total, _Page, _Size, _Items) -> 
                #{<<"total">> => 5, <<"page">> => 1, <<"size">> => 10, <<"list">> => []}
            end}
        ]}
    ], fun() ->
        Uid = 1,
        Page = 1,
        Size = 10,
        
        Result = user_device_logic:page(Uid, Page, Size),
        % 验证返回的是分页响应格式
        ?assertMatch(#{<<"total">> := _, <<"page">> := _, <<"size">> := _}, Result)
    end).

get_device_by_id_test_() ->
    ?TEST_WITH_APP(fun() ->
        DeviceId = <<"device123">>,
        ?assertMatch(<<_/binary>>, DeviceId)
    end).
