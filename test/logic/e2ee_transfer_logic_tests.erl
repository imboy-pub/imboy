-module(e2ee_transfer_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("error_code.hrl").

%%%===================================================================
%%% @doc E2EE 设备传输 Logic 层测试
%%%
%%% 测试目标：
%%% - 验证错误码定义
%%% - 验证基本逻辑
%%%===================================================================

%% ===================================================================
%% 错误码验证测试
%% ===================================================================

error_codes_defined_test() ->
    % 验证设备传输相关错误码已定义
    ?assertEqual(5000, ?ERR_E2EE_TRANSFER_INVALID_SESSION),
    ?assertEqual(5001, ?ERR_E2EE_TRANSFER_SESSION_EXPIRED),
    ?assertEqual(5002, ?ERR_E2EE_TRANSFER_SESSION_NOT_FOUND),
    ?assertEqual(5003, ?ERR_E2EE_TRANSFER_INVALID_DEVICE),
    ?assertEqual(5004, ?ERR_E2EE_TRANSFER_ALREADY_ACCEPTED),
    ?assertEqual(5005, ?ERR_E2EE_TRANSFER_CANNOT_CONFIRM),
    ?assertEqual(5006, ?ERR_E2EE_TRANSFER_FROM_UID_NOT_MATCH),
    ?assertEqual(5007, ?ERR_E2EE_TRANSFER_TO_UID_NOT_MATCH),
    ?assertEqual(5008, ?ERR_E2EE_TRANSFER_CONCURRENT),
    ?assertEqual(5009, ?ERR_E2EE_TRANSFER_ALREADY_CANCELLED),
    ?assertEqual(5010, ?ERR_E2EE_TRANSFER_STATUS_INVALID).

recovery_error_codes_defined_test() ->
    % 验证自动恢复相关错误码已定义
    ?assertEqual(5060, ?ERR_E2EE_RECOVERY_NO_OPTIONS),
    ?assertEqual(5061, ?ERR_E2EE_RECOVERY_IN_PROGRESS),
    ?assertEqual(5062, ?ERR_E2EE_RECOVERY_FAILED),
    ?assertEqual(5063, ?ERR_E2EE_RECOVERY_TIMEOUT),
    ?assertEqual(5064, ?ERR_E2EE_RECOVERY_KEY_MISMATCH).

%% ===================================================================
%% 函数导出验证测试
%% ===================================================================

exports_test() ->
    % 验证所有必要的函数都已导出
    Exports = e2ee_transfer_logic:module_info(exports),
    ?assert(lists:member({create_transfer, 5}, Exports)),
    ?assert(lists:member({accept_transfer, 3}, Exports)),
    ?assert(lists:member({confirm_transfer, 2}, Exports)),
    ?assert(lists:member({cancel_transfer, 2}, Exports)),
    ?assert(lists:member({get_transfer_info, 1}, Exports)),
    ?assert(lists:member({get_pending_transfers, 1}, Exports)).

recovery_logic_exports_test() ->
    % 验证自动恢复逻辑函数都已导出
    Exports = e2ee_recovery_logic:module_info(exports),
    ?assert(lists:member({check_key_status, 2}, Exports)),
    ?assert(lists:member({get_recovery_options, 1}, Exports)),
    ?assert(lists:member({recommend_method, 1}, Exports)),
    ?assert(lists:member({start_auto_recovery, 3}, Exports)).
