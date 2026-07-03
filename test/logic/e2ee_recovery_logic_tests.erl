-module(e2ee_recovery_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("error_code.hrl").

%%%===================================================================
%%% @doc E2EE 自动恢复 Logic 层测试
%%%
%%% 测试目标：
%%% - 验证恢复方式推荐逻辑
%%% - 验证错误处理
%%%===================================================================

%% ===================================================================
%% 恢复方式推荐测试（不需要 Mock）
%% ===================================================================

recommend_method_empty_test() ->
    ?assertEqual(<<"none">>, e2ee_recovery_logic:recommend_method([])).

recommend_method_selects_best_test() ->
    % 注意：recommend_method 假设列表已按优先级排序
    Options = [
        #{<<"method">> => <<"device_transfer">>, <<"priority">> => 1},
        #{<<"method">> => <<"social_recovery">>, <<"priority">> => 2}
    ],
    ?assertEqual(<<"device_transfer">>, e2ee_recovery_logic:recommend_method(Options)).

recommend_method_selects_social_test() ->
    Options = [
        #{<<"method">> => <<"social_recovery">>, <<"priority">> => 2}
    ],
    ?assertEqual(<<"social_recovery">>, e2ee_recovery_logic:recommend_method(Options)).

%% ===================================================================
%% 自动恢复启动测试（不需要 Mock）
%% ===================================================================

%% 【T10/D4】local_backup 恢复方式已删除（孤岛，create 端点从未实现）；
%% 该方式现落入 unsupported 分支，与 start_auto_recovery_unsupported_test 同预期
start_auto_recovery_local_backup_now_unsupported_test() ->
    Uid = 10001,
    DeviceId = <<"device-001">>,
    Method = <<"local_backup">>,

    Result = e2ee_recovery_logic:start_auto_recovery(Uid, DeviceId, Method),

    ?assertMatch({error, {_, ?ERR_E2EE_OPERATION_NOT_SUPPORTED}}, Result).

start_auto_recovery_unsupported_test() ->
    Uid = 10001,
    DeviceId = <<"device-001">>,
    Method = <<"unknown_method">>,

    Result = e2ee_recovery_logic:start_auto_recovery(Uid, DeviceId, Method),

    ?assertMatch({error, {_, ?ERR_E2EE_OPERATION_NOT_SUPPORTED}}, Result).
