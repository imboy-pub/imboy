-module(user_onboarding_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc user_onboarding_logic EUnit 测试（AI 冷启动 M1）
%%% 覆盖：新手三件套（默认好友/默认订阅/欢迎消息）编排；
%%%       is_friend 幂等守卫；config 缺失/停用整体 no-op；
%%%       单步故障隔离（一步崩不拖垮其余）；非法入参无副作用。
%%%===================================================================

%% 配置 meck：按 config 键返回期望值（模拟 config_ds 的 config 表读取）
-define(CFG_MECK(Enabled, AgentUid, Channels),
    {config_ds, [
        {'get', 2, fun
            (<<"onboarding.enabled">>, _) -> Enabled;
            (<<"onboarding.welcome_agent_uid">>, _) -> AgentUid;
            (<<"onboarding.default_channels">>, _) -> Channels;
            (<<"onboarding.welcome_template">>, D) -> D;
            (<<"onboarding.welcome_llm_enabled">>, D) -> D
        end}
    ]}
).

%% 三件套共用 meck（好友链/订阅链/欢迎链全部隔离）
-define(STACK_MECKS(IsFriend), [
    {friend_ds, [
        {'is_friend', 2, fun(_, _) -> IsFriend end},
        {'confirm_friend', 7, fun(_, _, _, _, _, _, _) -> ok end},
        {'invalidate_cache', 2, fun(_, _) -> ok end}
    ]},
    {channel_logic, [{'subscribe', 2, fun(_, _) -> ok end}]},
    {ai_agent_proactive, [{'send_welcome', 4, fun(_, _, _, _) -> ok end}]},
    {elib_log, [{'internal_log', 5, fun(_, _, _, _, _) -> ok end}]}
]).

%% ===================================================================
%% Happy path：三件套各调用一次（好友双边 = confirm_friend ×2）
%% ===================================================================

after_signup_happy_path_test_() ->
    ?WITH_MECKS(
        [?CFG_MECK(true, 42, [<<"ch_daily">>, <<"ch_notice">>]) | ?STACK_MECKS(false)],
        fun() ->
            ?assertEqual(ok, user_onboarding_logic:after_signup(7, <<"小明"/utf8>>)),
            %% 幂等守卫以 (新用户, agent) 查询
            ?assert(meck:called(friend_ds, is_friend, [7, 42])),
            %% 默认好友双边写入：Uid→Agent 与 Agent→Uid 各一行
            ConfirmCalls = [
                Args
             || {_, {_, confirm_friend, Args}, _} <- meck:history(friend_ds)
            ],
            ?assertEqual(2, length(ConfirmCalls)),
            %% 两个默认频道逐个订阅
            ?assert(meck:called(channel_logic, subscribe, [7, <<"ch_daily">>])),
            ?assert(meck:called(channel_logic, subscribe, [7, <<"ch_notice">>])),
            %% 欢迎消息：agent→新用户，携带昵称与配置
            ?assert(meck:called(ai_agent_proactive, send_welcome, [42, 7, <<"小明"/utf8>>, '_']))
        end
    ).

%% ===================================================================
%% 幂等：已是好友 → 三件套全跳过（防重试/重复注册产生重复欢迎）
%% ===================================================================

after_signup_idempotent_when_already_friend_test_() ->
    ?WITH_MECKS(
        [?CFG_MECK(true, 42, [<<"ch_daily">>]) | ?STACK_MECKS(true)],
        fun() ->
            ?assertEqual(ok, user_onboarding_logic:after_signup(7, <<"小明"/utf8>>)),
            ?assertNot(meck:called(friend_ds, confirm_friend, '_')),
            ?assertNot(meck:called(channel_logic, subscribe, '_')),
            ?assertNot(meck:called(ai_agent_proactive, send_welcome, '_'))
        end
    ).

%% ===================================================================
%% 配置门控：disabled / agent_uid=0 → 整体 no-op
%% ===================================================================

after_signup_disabled_no_op_test_() ->
    ?WITH_MECKS(
        [?CFG_MECK(false, 42, [<<"ch_daily">>]) | ?STACK_MECKS(false)],
        fun() ->
            ?assertEqual(ok, user_onboarding_logic:after_signup(7, <<"小明"/utf8>>)),
            ?assertNot(meck:called(friend_ds, is_friend, '_')),
            ?assertNot(meck:called(ai_agent_proactive, send_welcome, '_'))
        end
    ).

after_signup_zero_agent_uid_no_op_test_() ->
    ?WITH_MECKS(
        [?CFG_MECK(true, 0, [<<"ch_daily">>]) | ?STACK_MECKS(false)],
        fun() ->
            ?assertEqual(ok, user_onboarding_logic:after_signup(7, <<"小明"/utf8>>)),
            ?assertNot(meck:called(friend_ds, is_friend, '_')),
            ?assertNot(meck:called(ai_agent_proactive, send_welcome, '_'))
        end
    ).

%% ===================================================================
%% 故障隔离：好友链抛崩 → 订阅与欢迎仍执行；整体恒 ok
%% ===================================================================

after_signup_step_failure_isolated_test_() ->
    ?WITH_MECKS(
        [
            ?CFG_MECK(true, 42, [<<"ch_daily">>]),
            {friend_ds, [
                {'is_friend', 2, fun(_, _) -> false end},
                {'confirm_friend', 7, fun(_, _, _, _, _, _, _) -> error(db_down) end},
                {'invalidate_cache', 2, fun(_, _) -> ok end}
            ]},
            {channel_logic, [{'subscribe', 2, fun(_, _) -> ok end}]},
            {ai_agent_proactive, [{'send_welcome', 4, fun(_, _, _, _) -> ok end}]},
            {elib_log, [{'internal_log', 5, fun(_, _, _, _, _) -> ok end}]}
        ],
        fun() ->
            ?assertEqual(ok, user_onboarding_logic:after_signup(7, <<"小明"/utf8>>)),
            %% 好友链崩了，但订阅与欢迎链未被拖垮
            ?assert(meck:called(channel_logic, subscribe, [7, <<"ch_daily">>])),
            ?assert(meck:called(ai_agent_proactive, send_welcome, [42, 7, '_', '_']))
        end
    ).

%% 整体兜底：配置读取本身抛崩 → 恒 ok 不向上抛（注册链路零影响）
after_signup_config_crash_safe_test_() ->
    ?WITH_MECKS(
        [
            {config_ds, [{'get', 2, fun(_, _) -> error(config_table_down) end}]},
            {elib_log, [{'internal_log', 5, fun(_, _, _, _, _) -> ok end}]}
        ],
        fun() ->
            ?assertEqual(ok, user_onboarding_logic:after_signup(7, <<"小明"/utf8>>))
        end
    ).

%% ===================================================================
%% 非法入参：无副作用恒 ok
%% ===================================================================

after_signup_invalid_uid_no_op_test() ->
    ?assertEqual(ok, user_onboarding_logic:after_signup(0, <<"x">>)),
    ?assertEqual(ok, user_onboarding_logic:after_signup(-1, <<"x">>)),
    ?assertEqual(ok, user_onboarding_logic:after_signup(<<"7">>, <<"x">>)).

%% ===================================================================
%% get_config/0 + put_config/1（管理后台 onboarding 配置读写）
%% ===================================================================

get_config_returns_all_keys_test_() ->
    ?WITH_MECKS(
        [
            {config_ds, [
                {'get', 2, fun
                    (<<"onboarding.enabled">>, _) -> true;
                    (<<"onboarding.welcome_agent_uid">>, _) -> 42;
                    (<<"onboarding.default_channels">>, _) -> [<<"ch_a">>];
                    (<<"onboarding.welcome_template">>, _) -> <<"嗨 {{nickname}}"/utf8>>;
                    (<<"onboarding.welcome_llm_enabled">>, _) -> false
                end}
            ]}
        ],
        fun() ->
            Cfg = user_onboarding_logic:get_config(),
            ?assertEqual(true, maps:get(<<"enabled">>, Cfg)),
            ?assertEqual(42, maps:get(<<"welcome_agent_uid">>, Cfg)),
            ?assertEqual([<<"ch_a">>], maps:get(<<"default_channels">>, Cfg)),
            ?assertEqual(<<"嗨 {{nickname}}"/utf8>>, maps:get(<<"welcome_template">>, Cfg)),
            ?assertEqual(false, maps:get(<<"welcome_llm_enabled">>, Cfg))
        end
    ).

put_config_writes_whitelisted_keys_test_() ->
    ?WITH_MECKS(
        [
            {config_ds, [
                {'get', 2, fun(_, D) -> D end},
                {'set', 2, fun(_, _) -> ok end}
            ]}
        ],
        fun() ->
            %% 半量更新：只写传入键；未知键忽略不报错
            {ok, _} = user_onboarding_logic:put_config(#{
                <<"enabled">> => true,
                <<"welcome_agent_uid">> => 42,
                <<"unknown_key">> => <<"x">>
            }),
            ?assert(meck:called(config_ds, set, [<<"onboarding.enabled">>, true])),
            ?assert(meck:called(config_ds, set, [<<"onboarding.welcome_agent_uid">>, 42])),
            ?assertEqual(2, length(meck:history(config_ds)))
        end
    ).

put_config_binary_uid_normalized_to_int_test_() ->
    ?WITH_MECKS(
        [{config_ds, [{'set', 2, fun(_, _) -> ok end}]}],
        fun() ->
            %% 前端 TSID 序列化为字符串，后端归一化为 integer 存储
            {ok, _} = user_onboarding_logic:put_config(#{
                <<"welcome_agent_uid">> => <<"123456789012345678">>
            }),
            ?assert(
                meck:called(
                    config_ds, set, [<<"onboarding.welcome_agent_uid">>, 123456789012345678]
                )
            )
        end
    ).

put_config_validates_types_test_() ->
    ?WITH_MECKS(
        [{config_ds, [{'set', 2, fun(_, _) -> ok end}]}],
        fun() ->
            %% 非法 boolean
            ?assertMatch(
                {error, _},
                user_onboarding_logic:put_config(#{<<"enabled">> => <<"yes">>})
            ),
            %% 负数 agent uid
            ?assertMatch(
                {error, _},
                user_onboarding_logic:put_config(#{<<"welcome_agent_uid">> => -1})
            ),
            %% 非数字 binary uid
            ?assertMatch(
                {error, _},
                user_onboarding_logic:put_config(#{<<"welcome_agent_uid">> => <<"abc">>})
            ),
            %% 频道列表含非 binary 元素
            ?assertMatch(
                {error, _},
                user_onboarding_logic:put_config(#{<<"default_channels">> => [123]})
            ),
            %% 模板超长（>2000 字节）
            ?assertMatch(
                {error, _},
                user_onboarding_logic:put_config(#{
                    <<"welcome_template">> => binary:copy(<<"a">>, 2001)
                })
            ),
            %% 全部非法：零写入
            ?assertNot(meck:called(config_ds, set, '_'))
        end
    ).
