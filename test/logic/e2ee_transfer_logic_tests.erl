-module(e2ee_transfer_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("error_code.hrl").
-include("eunit_setup.hrl").

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
    ?assert(lists:member({create_transfer, 4}, Exports)),
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

%% ===================================================================
%% accept_transfer/3 安全回归测试
%% 防止伪造 device_id 冒领他人转移会话 + 并发覆盖 to_device_id
%% ===================================================================

accept_transfer_success_test_() ->
    ?WITH_MECKS(
        [
            {e2ee_transfer_ds, [
                {get_by_session_id, 1, fun(_SessionId) ->
                    {ok, #{
                        <<"session_id">> => <<"sess-1">>,
                        <<"from_uid">> => 10001,
                        <<"to_uid">> => 10002,
                        <<"status">> => <<"pending">>
                    }}
                end},
                {update_status_and_device, 3, fun(_SessionId, _Status, _ToDeviceId) -> ok end}
            ]},
            {user_device_ds, [
                {device_name, 2, fun(_Uid, _Did) -> <<"我的手机"/utf8>> end}
            ]}
        ],
        fun() ->
            Result = e2ee_transfer_logic:accept_transfer(<<"sess-1">>, 10002, <<"device-002">>),
            ?assertMatch({ok, #{<<"status">> := <<"accepted">>}}, Result)
        end
    ).

accept_transfer_rejects_device_not_owned_by_current_user_test_() ->
    ?WITH_MECKS(
        [
            {e2ee_transfer_ds, [
                {get_by_session_id, 1, fun(_SessionId) ->
                    {ok, #{
                        <<"session_id">> => <<"sess-2">>,
                        <<"from_uid">> => 10001,
                        <<"to_uid">> => 10002,
                        <<"status">> => <<"pending">>
                    }}
                end}
            ]},
            {user_device_ds, [
                % 该 device_id 不属于 10002（伪造/不存在的设备）
                {device_name, 2, fun(_Uid, _Did) -> <<>> end}
            ]}
        ],
        fun() ->
            Result = e2ee_transfer_logic:accept_transfer(<<"sess-2">>, 10002, <<"fake-device">>),
            ?assertMatch({error, {_, ?ERR_E2EE_TRANSFER_INVALID_DEVICE}}, Result)
        end
    ).

%% D3 同账号换机：允许 ToUid==FromUid，但目标设备==源设备的无意义转移被拒
accept_transfer_rejects_same_device_as_source_test_() ->
    ?WITH_MECKS(
        [
            {e2ee_transfer_ds, [
                {get_by_session_id, 1, fun(_SessionId) ->
                    {ok, #{
                        <<"session_id">> => <<"sess-same-dev">>,
                        <<"from_uid">> => 10001,
                        %% 同账号换机：to_uid 与 from_uid 相同
                        <<"to_uid">> => 10001,
                        <<"from_device_id">> => <<"device-001">>,
                        <<"status">> => <<"pending">>
                    }}
                end}
            ]},
            {user_device_ds, [
                {device_name, 2, fun(10001, <<"device-001">>) -> <<"旧手机"/utf8>> end}
            ]}
        ],
        fun() ->
            %% accept 到源设备本身
            Result = e2ee_transfer_logic:accept_transfer(
                <<"sess-same-dev">>, 10001, <<"device-001">>
            ),
            ?assertMatch({error, {_, ?ERR_E2EE_TRANSFER_INVALID_DEVICE}}, Result)
        end
    ).

%% ===================================================================
%% create_transfer/4 同账号安全回归测试
%% 设备间传输仅用于「同一账号换机」；缺同账号校验时任意登录用户可用受害者
%% 公钥加密任意 bundle 向其发起转移，诱导受害者导入攻击者可控密钥（E2EE 身份注入）。
%% ===================================================================

%% 跨账号（ToUid =/= FromUid）必须 fail-closed 拒绝，且不得触达建会话
create_transfer_rejects_cross_account_test_() ->
    ?WITH_MECKS(
        [
            {user_ds, [
                {may_exist, 1, fun(_ToUid) -> true end}
            ]},
            {e2ee_transfer_ds, [
                %% 若守卫缺失会走到这里建会话；这里若被调用即视为漏洞未修
                {generate_session_id, 0, fun() -> <<"sess-x">> end},
                {create_raw, 1, fun(_Map) -> {ok, <<"sess-x">>} end}
            ]}
        ],
        fun() ->
            Result = e2ee_transfer_logic:create_transfer(
                10001, <<"device-001">>, 10002, <<"attacker-bundle">>
            ),
            ?assertMatch({error, {_, ?ERR_E2EE_TRANSFER_TO_UID_NOT_MATCH}}, Result),
            %% 且不得创建任何会话
            ?assertNot(meck:called(e2ee_transfer_ds, create_raw, '_'))
        end
    ).

%% 同账号换机（ToUid =:= FromUid）正常放行
create_transfer_allows_same_account_test_() ->
    ?WITH_MECKS(
        [
            {user_ds, [
                {may_exist, 1, fun(_ToUid) -> true end}
            ]},
            {e2ee_transfer_ds, [
                {generate_session_id, 0, fun() -> <<"sess-ok">> end},
                {create_raw, 1, fun(_Map) -> {ok, <<"sess-ok">>} end}
            ]}
        ],
        fun() ->
            Result = e2ee_transfer_logic:create_transfer(
                10001, <<"device-001">>, 10001, <<"my-bundle">>
            ),
            ?assertMatch({ok, #{<<"status">> := <<"pending">>}}, Result)
        end
    ).

accept_transfer_conflict_on_concurrent_accept_test_() ->
    ?WITH_MECKS(
        [
            {e2ee_transfer_ds, [
                {get_by_session_id, 1, fun(_SessionId) ->
                    {ok, #{
                        <<"session_id">> => <<"sess-3">>,
                        <<"from_uid">> => 10001,
                        <<"to_uid">> => 10002,
                        <<"status">> => <<"pending">>
                    }}
                end},
                % 模拟并发场景下 CAS 更新失败（其他请求已抢先 accept）
                {update_status_and_device, 3, fun(_SessionId, _Status, _ToDeviceId) ->
                    {error, conflict}
                end}
            ]},
            {user_device_ds, [
                {device_name, 2, fun(_Uid, _Did) -> <<"我的手机"/utf8>> end}
            ]}
        ],
        fun() ->
            Result = e2ee_transfer_logic:accept_transfer(<<"sess-3">>, 10002, <<"device-002">>),
            ?assertMatch({error, {_, ?ERR_E2EE_TRANSFER_ALREADY_ACCEPTED}}, Result)
        end
    ).
