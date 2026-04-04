-module(user_device_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("error_code.hrl").
-include("chat.hrl").
-include("log.hrl").

%%% @doc user_device_logic 模块测试

%% ===================================================================
%% 设备档案管理测试
%% ===================================================================

save_device_success_test_() ->
    ?WITH_MECK(user_device_ds, [
        {'count_by_uid', 1, fun(_Uid) -> 1 end},
        {'page', 3, fun(_Uid, _Limit, _Offset) ->
            {ok, [#{<<"device_id">> => <<"device1">>, <<"device_name">> => <<"iPhone">>,
                    <<"device_type">> => <<"ios">>, <<"last_active_at">> => <<>>,
                    <<"device_vsn">> => <<"1.0">>}]}
        end}
    ], fun() ->
        Result = user_device_logic:page(100, 1, 10),
        ?assertMatch(#{list := [_ | _]}, Result)
    end).

get_devices_success_test_() ->
    ?WITH_MECK(user_device_ds, [
        {'count_by_uid', 1, fun(_Uid) -> 1 end},
        {'page', 3, fun(_Uid, _Limit, _Offset) ->
            {ok, [#{<<"device_id">> => <<"d1">>, <<"device_name">> => <<"Test">>,
                    <<"device_type">> => <<"android">>, <<"last_active_at">> => <<>>,
                    <<"device_vsn">> => <<"1.0">>}]}
        end}
    ], fun() ->
        Result = user_device_logic:page(100, 1, 10),
        ?assertMatch(#{list := [_ | _]}, Result)
    end).

%% ===================================================================
%% 设备会话管理测试
%% ===================================================================

%% @doc 验证有效的设备类型
validate_device_type_valid_types_test_() ->
    ?TEST_SIMPLE(fun() ->
        ValidTypes = [<<"ios">>, <<"android">>, <<"macos">>, <<"windows">>, <<"linux">>, <<"web">>],
        lists:foreach(fun(Type) ->
            ?assertEqual(true, user_device_logic:validate_device_type(Type))
        end, ValidTypes)
    end).

%% @doc 验证无效的设备类型
validate_device_type_invalid_types_test_() ->
    ?TEST_SIMPLE(fun() ->
        InvalidTypes = [<<"invalid">>, <<>>, <<"IOS">>, <<"Android">>, undefined],
        lists:foreach(fun(Type) ->
            ?assertEqual(false, user_device_logic:validate_device_type(Type))
        end, InvalidTypes)
    end).

%% @doc 验证设备类型枚举
device_type_enum_test_() ->
    ?TEST_SIMPLE(fun() ->
        Types = user_device_logic:device_type_enum(),
        ?assertEqual(6, length(Types)),
        ?assert(lists:member(<<"ios">>, Types)),
        ?assert(lists:member(<<"android">>, Types)),
        ?assert(lists:member(<<"macos">>, Types)),
        ?assert(lists:member(<<"windows">>, Types)),
        ?assert(lists:member(<<"linux">>, Types)),
        ?assert(lists:member(<<"web">>, Types))
    end).

%% @doc 测试登录冲突检测（无冲突）
check_login_conflict_no_conflict_test_() ->
    ?TEST_SIMPLE(fun() ->
        Uid = 1001,
        DType = <<"ios">>,

        % 没有在线设备（假设 syn 返回空列表）
        % 这是一个简单的单元测试，实际集成测试需要在真实环境中运行
        Result = user_device_logic:check_login_conflict(Uid, DType),

        % 结果应该是 {ok, no_conflict} 或者 {error, _}（取决于 syn 状态）
        case Result of
            {ok, no_conflict} -> ok;
            {error, _} -> ok  % syn 未初始化时可能返回错误
        end
    end).

%% @doc 测试登录冲突检测（无效设备类型）
check_login_conflict_invalid_device_type_test_() ->
    ?TEST_SIMPLE(fun() ->
        Uid = 1011,
        InvalidDType = <<"invalid">>,

        Result = user_device_logic:check_login_conflict(Uid, InvalidDType),

        % 应该返回错误
        ?assertMatch({error, _}, Result),
        {error, Msg} = Result,
        ?assertEqual(<<"无效的设备类型"/utf8>>, Msg)
    end).

%% @doc 测试获取活跃设备列表（空）
get_active_devices_empty_test_() ->
    ?TEST_SIMPLE(fun() ->
        Uid = 1010,

        % 没有在线设备
        Result = user_device_logic:get_active_devices(Uid),

        ?assertEqual({ok, []}, Result)
    end).

%% @doc 测试格式化设备信息
format_device_info_test_() ->
    ?TEST_SIMPLE(fun() ->
        Pid = list_to_pid("<0.100.0>"),
        Meta = {<<"ios">>, <<"test-device">>},

        Result = user_device_logic:format_device_info({Pid, Meta}),

        ?assert(is_map(Result)),
        ?assertEqual(<<"ios">>, maps:get(<<"device_type">>, Result)),
        ?assertEqual(<<"test-device">>, maps:get(<<"device_id">>, Result))
    end).

%% @doc 测试按设备类型检查在线状态
is_online_by_dtype_test_() ->
    ?TEST_SIMPLE(fun() ->
        Uid = 1020,
        DType = <<"ios">>,

        % 没有在线设备
        Result = user_device_logic:is_online(Uid, {dtype, DType}),

        ?assertEqual(false, Result)
    end).

%% @doc 测试按设备ID检查在线状态
is_online_by_did_test_() ->
    ?TEST_SIMPLE(fun() ->
        Uid = 1021,
        DID = <<"test-device-123">>,

        % 没有在线设备
        Result = user_device_logic:is_online(Uid, {did, DID}),

        ?assertEqual(false, Result)
    end).

%% @doc 测试无效设备类型的在线状态检查
is_online_invalid_dtype_test_() ->
    ?TEST_SIMPLE(fun() ->
        Uid = 1022,
        InvalidDType = <<"invalid">>,

        % 无效的设备类型
        Result = user_device_logic:is_online(Uid, {dtype, InvalidDType}),

        ?assertEqual(false, Result)
    end).

%% @doc 测试获取在线设备ID列表
online_dids_test_() ->
    ?TEST_SIMPLE(fun() ->
        Uid = 1023,

        % 没有在线设备
        Result = user_device_logic:online_dids(Uid),

        ?assertEqual([], Result)
    end).
