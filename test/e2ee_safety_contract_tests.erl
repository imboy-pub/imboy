-module(e2ee_safety_contract_tests).
%% E-01 E2EE 安全契约汇聚套件。
%% 验收条款：required 模式下明文到达 server logs / admin / push 任一面即测试失败。
%%
%% 三面分工（本套件只补此前无覆盖的缺口，其余面引用专项测试）：
%%   * 日志面（本套件核心增量）：明文哨兵 C2C/C2G 被真实 policy 门拒收后，
%%     lager 全量捕获无哨兵、staging/入队/投递零调用、S2C 错误回执不回显内容。
%%     判定链 effective_capabilities → message_encryption_required →
%%     encrypted_message_body 全程真实执行（仅桩 config_ds 部署配置边界）。
%%   * admin 面：audit 三态契约——metadata/none 置空 payload；full 返回落库
%%     payload 原样（required 下即密文），服务端不存在任何解密通道可喂给 admin。
%%   * push 面：push_notification_logic_tests 的
%%     e2ee_push_body_never_leaks_ciphertext / e2ee_v2_push_body_generic /
%%     e2ee_c2g_push_body_never_leaks 已覆盖（零知识不变量），此处不重复。
%% 已有 policy 真值表防误伤组见 msg_c2c_logic_tests / msg_c2g_logic_tests；
%% PFv3 Olm 信封形态（payload 空串 + e2ee.devices 非空）此前无真实判定覆盖，
%% 见 olm_envelope 用例。

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

-define(SENTINEL, <<"PLAINTEXT_SENTINEL_7f3a9c_绝密内容绝密内容"/utf8>>).
-define(CIPHERTEXT, <<"Q04xMjM0NTY3OGFiY2RlZg==.QWJjRGVmR2hpSmtMbU5vcFFyc1R1dnhZeg==">>).

%% ===================================================================
%% 桩
%% ===================================================================

%% 部署配置桩：required 档。同 msg_c2c_logic_tests:policy_config_meck/1 口径——
%% 只桩 config_ds 配置边界，imboy_policy 判定链全程真实。
required_config_meck() ->
    {config_ds, [
        {'get', 2, fun(_Key, Default) -> Default end},
        {'env', 2, fun
            (product_profile, community) -> community;
            (capabilities, #{}) -> #{e2ee_mode => required};
            (_Key, Default) -> Default
        end}
    ]}.

%% C2C 走通投递管道所需的周边桩（不含 imboy_policy，不含 lager）
c2c_pipeline_mecks() ->
    [
        {friend_ds, [
            {'check_relationship', 2, fun(456, 123) -> {true, false} end}
        ]},
        {ai_agent_ds, [
            {'is_agent', 1, fun(_) -> false end}
        ]},
        {bot_ds, [
            {'is_bot', 1, fun(_) -> false end}
        ]},
        {elib_dt, [
            {'now', 0, fun() -> <<"2026-09-07T10:00:00Z">> end},
            {'rfc3339_to', 2, fun(<<"2026-09-07T10:00:00Z">>, millisecond) ->
                1708768800000
            end},
            {'to_rfc3339', 1, fun(1708768700000) -> <<"2026-09-07T09:58:20Z">> end},
            {'millisecond', 0, fun() -> 1708768800000 end}
        ]},
        {msg_store_ds, [
            {'stage', 11, fun(_, _, _, _, _, _, _, _, _, _, _) -> {ok, new} end},
            {'enqueue', 3, fun(_, _, _) -> ok end}
        ]},
        {elib_async, [
            {'async_retry', 3, fun(Fun, 3, 1000) ->
                Fun(),
                ok
            end},
            {'async', 1, fun(Fun) ->
                Fun(),
                self()
            end}
        ]},
        {push_notification_logic, [
            {'maybe_push_for_c2c', 4, fun(_, _, _, _) -> ok end}
        ]},
        {message_ds, [
            {'assemble_msg', 8, fun(_, _, _, _, MsgId, _, _, _) -> #{<<"id">> => MsgId} end}
        ]},
        {imboy_message_helper, [
            {'encode_and_send', 4, fun(_, _, _, _) -> ok end}
        ]},
        {ai_agent_reply, [
            {'maybe_dispatch', 3, fun(_, _, _) -> ok end}
        ]},
        {billing_meter, [
            {'meter', 2, fun(_, _) -> ok end}
        ]}
    ].

%% C2G 周边桩（不含 imboy_policy，不含 lager）
c2g_pipeline_mecks() ->
    [
        {group_member_logic, [
            {'check_mute', 2, fun(100, 1001) -> false end}
        ]},
        {group_ds, [
            %% 群级 fail-closed 门默认关；部署级 required 门由真实 imboy_policy 承担
            {'e2ee_mode', 1, fun(_) -> {ok, 0} end},
            {'is_member', 2, fun(1001, 100) -> true end},
            {'member_uids', 1, fun(100) -> [1001, 1002, 1003] end},
            {'member_uids_strict', 1, fun(100) -> {ok, [1001, 1002, 1003]} end}
        ]},
        {elib_dt, [
            {'now', 0, fun() -> <<"2026-09-07T10:00:00Z">> end},
            {'rfc3339_to', 2, fun(<<"2026-09-07T10:00:00Z">>, millisecond) ->
                1708768800000
            end},
            {'to_rfc3339', 1, fun(1708768700000) -> <<"2026-09-07T09:58:20Z">> end},
            {'millisecond', 0, fun() -> 1708768800000 end}
        ]},
        {msg_store_ds, [
            {'stage', 11, fun(_, _, _, _, _, _, _, _, _, _, _) -> {ok, new} end},
            {'enqueue', 3, fun(_, _, _) -> ok end}
        ]},
        {elib_async, [
            {'async_retry', 3, fun(Fun, 3, 1000) ->
                Fun(),
                ok
            end},
            {'async', 1, fun(Fun) ->
                Fun(),
                self()
            end}
        ]},
        {push_notification_logic, [
            {'maybe_push_for_c2g', 4, fun(_, _, _, _) -> ok end}
        ]},
        {message_ds, [
            {'send_next', 4, fun(_, _, _, _) -> ok end}
        ]},
        {mention_logic, [
            {'create_mentions', 4, fun(_, _, _, _) -> ok end}
        ]},
        {user_logic, [
            {'is_online', 1, fun(_Uid) -> true end}
        ]}
    ].

%% lager 捕获桩：所有级别日志收进 meck history，供全文哨兵扫描
lager_meck() ->
    {lager, [
        {'log', 3, fun(_Level, _Meta, _Msg) -> ok end}
    ]}.

%% 从 meck history 提取全部日志消息并拼成 binary
captured_log_text() ->
    Msgs = [Msg || {_P, {lager, log, [_L, _M, Msg]}, _R} <- meck:history(lager)],
    unicode:characters_to_binary(io_lib:format("~tp", [Msgs])).

%% ===================================================================
%% 用例
%% ===================================================================

%% 验收·日志面（C2C）：required 部署下明文哨兵消息被真实 policy 门拒收，
%% 全程日志无哨兵、无落库、无投递，S2C 错误回执不回显内容。
c2c_required_plaintext_never_reaches_logs_test_() ->
    ?WITH_MECKS(
        [required_config_meck(), lager_meck() | c2c_pipeline_mecks()],
        fun() ->
            Data = #{
                <<"to">> => <<"456">>,
                <<"payload">> => #{<<"content">> => ?SENTINEL},
                <<"created_at">> => 1708768700000,
                <<"msg_type">> => <<"text">>,
                <<"action">> => <<>>,
                <<"e2ee">> => null
            },
            {reply, Reply} = msg_c2c_logic:c2c(
                <<"msg_e01_c2c_plain_001">>, 123, Data
            ),
            ?assertEqual(<<"S2C">>, maps:get(<<"type">>, Reply)),
            ?assertEqual(<<"policy_violation">>, maps:get(<<"action">>, Reply)),
            ?assertEqual(
                <<"encrypted_message_required">>,
                maps:get(<<"reason">>, maps:get(<<"payload">>, Reply))
            ),
            %% 错误回执不回显消息内容
            ?assertEqual(
                nomatch,
                binary:match(unicode:characters_to_binary(io_lib:format("~tp", [Reply])), [
                    ?SENTINEL
                ])
            ),
            %% 零落库、零入队、零投递
            ?assertEqual(0, meck:num_calls(msg_store_ds, stage, 11)),
            ?assertEqual(0, meck:num_calls(msg_store_ds, enqueue, 3)),
            ?assertEqual(0, meck:num_calls(imboy_message_helper, encode_and_send, 4)),
            %% 验收核心：日志系统全程未见过明文哨兵
            ?assertEqual(nomatch, binary:match(captured_log_text(), [?SENTINEL]))
        end
    ).

%% 验收·日志面（C2G）：同上，群聊路径。
c2g_required_plaintext_never_reaches_logs_test_() ->
    ?WITH_MECKS(
        [required_config_meck(), lager_meck() | c2g_pipeline_mecks()],
        fun() ->
            Data = #{
                <<"to">> => <<"100">>,
                <<"payload">> => #{<<"content">> => ?SENTINEL, <<"mentions">> => []},
                <<"created_at">> => 1708768700000,
                <<"msg_type">> => <<"text">>,
                <<"action">> => <<>>,
                <<"e2ee">> => null
            },
            {reply, Reply} = msg_c2g_logic:c2g(<<"msg_e01_c2g_plain_001">>, 1001, Data),
            ?assertEqual(<<"S2C">>, maps:get(<<"type">>, Reply)),
            ?assertEqual(<<"policy_violation">>, maps:get(<<"action">>, Reply)),
            ?assertEqual(0, meck:num_calls(msg_store_ds, stage, 11)),
            ?assertEqual(0, meck:num_calls(msg_store_ds, enqueue, 3)),
            ?assertEqual(nomatch, binary:match(captured_log_text(), [?SENTINEL]))
        end
    ).

%% PFv3 Olm 信封形态（payload 恒空串 + e2ee.devices 逐设备密文）此前无真实
%% 判定覆盖：required 部署下必须放行（否则全站 Olm 消息被误判明文拒收），
%% 且走通后日志同样无明文哨兵。
c2c_required_olm_envelope_accepted_and_log_clean_test_() ->
    ?WITH_MECKS(
        [required_config_meck(), lager_meck() | c2c_pipeline_mecks()],
        fun() ->
            Data = #{
                <<"to">> => <<"456">>,
                %% PFv3 fan-out：payload 空串，密文全在 e2ee.devices 信封
                <<"payload">> => <<>>,
                <<"created_at">> => 1708768700000,
                <<"msg_type">> => <<"text">>,
                <<"action">> => <<>>,
                <<"e2ee">> => #{
                    <<"e2ee">> => true,
                    <<"e2ee_ver">> => 3,
                    <<"devices">> => #{
                        <<"deviceA">> => #{<<"ek">> => ?CIPHERTEXT}
                    }
                }
            },
            ok = msg_c2c_logic:c2c(<<"msg_e01_olm_001">>, 123, Data),
            ?assertEqual(1, meck:num_calls(msg_store_ds, stage, 11)),
            ?assertEqual(1, meck:num_calls(imboy_message_helper, encode_and_send, 4)),
            ?assertEqual(nomatch, binary:match(captured_log_text(), [?SENTINEL]))
        end
    ).

%% 验收·admin 面：audit 三态契约。metadata/none 一律置空 payload；
%% full 返回落库 payload 原样——required 部署下落库值即密文，因此
%% 即便 policy=full + 内容权限 + 工单门全开，admin 所见也只是密文，
%% 服务端不存在"解密后转发 admin"的通道。
admin_audit_mode_contract_test_() ->
    Row = #{
        scope => <<"c2c">>,
        msg_id => <<"msg_e01_admin_001">>,
        from_id => <<"123">>,
        to_id => <<"456">>,
        msg_type => <<"text">>,
        action => <<>>,
        payload => ?CIPHERTEXT,
        created_at => <<"2026-09-07T10:00:00Z">>,
        server_ts => 1708768800000
    },
    [
        ?_assertEqual(
            ?CIPHERTEXT,
            maps:get(payload, adm_message_handler:sanitize_row_by_audit_mode(Row, full))
        ),
        ?_assertEqual(
            <<>>, maps:get(payload, adm_message_handler:sanitize_row_by_audit_mode(Row, metadata))
        ),
        ?_assertEqual(
            <<>>, maps:get(payload, adm_message_handler:sanitize_row_by_audit_mode(Row, none))
        )
    ].
