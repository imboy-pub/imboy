-module(e2ee_recovery_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
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

%% ===================================================================
%% server_backup 恢复方式（P0-B B3）
%% ===================================================================

recovery_options_include_server_backup_test_() ->
    ?WITH_MECKS(
        [
            {e2ee_transfer_ds, [
                {'get_pending_sessions', 1, fun(10001) -> {ok, []} end}
            ]},
            {e2ee_social_ds, [
                {'list_trusted_contacts', 1, fun(10001) -> {ok, []} end}
            ]},
            {e2ee_backup_ds, [
                {'latest', 1, fun(10001) ->
                    {ok, #{
                        <<"backup_version">> => 5,
                        <<"created_at">> => <<"2026-07-12 00:00:00">>
                    }}
                end}
            ]}
        ],
        fun() ->
            Options = e2ee_recovery_logic:get_recovery_options(10001),
            ?assertEqual([<<"server_backup">>], [maps:get(<<"method">>, O) || O <- Options]),
            [Opt] = Options,
            %% 探测详情只含版本与时间，不含密文/盐值（零信任）
            Details = maps:get(<<"details">>, Opt),
            ?assertEqual(5, maps:get(<<"backup_version">>, Details)),
            ?assertNot(maps:is_key(<<"encrypted_payload">>, Details)),
            ?assertNot(maps:is_key(<<"kdf_salt">>, Details))
        end
    ).

recovery_options_no_backup_test_() ->
    ?WITH_MECKS(
        [
            {e2ee_transfer_ds, [
                {'get_pending_sessions', 1, fun(10001) -> {ok, []} end}
            ]},
            {e2ee_social_ds, [
                {'list_trusted_contacts', 1, fun(10001) -> {ok, []} end}
            ]},
            {e2ee_backup_ds, [
                {'latest', 1, fun(10001) -> {error, not_found} end}
            ]}
        ],
        fun() ->
            ?assertEqual([], e2ee_recovery_logic:get_recovery_options(10001))
        end
    ).

%% 设备间传输可用时优先级仍高于 server_backup（1 < 3）
recovery_options_priority_order_test_() ->
    ?WITH_MECKS(
        [
            {e2ee_transfer_ds, [
                {'get_pending_sessions', 1, fun(10001) ->
                    {ok, [#{<<"session_id">> => <<"s1">>}]}
                end}
            ]},
            {e2ee_social_ds, [
                {'list_trusted_contacts', 1, fun(10001) -> {ok, []} end}
            ]},
            {e2ee_backup_ds, [
                {'latest', 1, fun(10001) -> {ok, #{<<"backup_version">> => 1}} end}
            ]}
        ],
        fun() ->
            Options = e2ee_recovery_logic:get_recovery_options(10001),
            ?assertEqual(
                [<<"device_transfer">>, <<"server_backup">>],
                [maps:get(<<"method">>, O) || O <- Options]
            ),
            ?assertEqual(<<"device_transfer">>, e2ee_recovery_logic:recommend_method(Options))
        end
    ).

start_server_backup_recovery_test_() ->
    ?WITH_MECKS(
        [
            {e2ee_backup_ds, [
                {'latest', 1, fun(10001) -> {ok, #{<<"backup_version">> => 5}} end}
            ]}
        ],
        fun() ->
            {ok, Result} =
                e2ee_recovery_logic:start_auto_recovery(10001, <<"dev-1">>, <<"server_backup">>),
            ?assertEqual(<<"fetch_backup">>, maps:get(<<"action">>, Result)),
            ?assertEqual(5, maps:get(<<"backup_version">>, Result))
        end
    ).

start_server_backup_recovery_no_backup_test_() ->
    ?WITH_MECKS(
        [
            {e2ee_backup_ds, [
                {'latest', 1, fun(10001) -> {error, not_found} end}
            ]}
        ],
        fun() ->
            ?assertMatch(
                {error, {_, ?ERR_NOT_FOUND}},
                e2ee_recovery_logic:start_auto_recovery(10001, <<"dev-1">>, <<"server_backup">>)
            )
        end
    ).
