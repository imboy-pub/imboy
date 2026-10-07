-module(imboy_profile_preset_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

community_profile_defaults_test_() ->
    ?TEST_SIMPLE(fun() ->
        Defaults = imboy_profile_preset:defaults(community),
        Capabilities = maps:get(capabilities, Defaults),
        Features = maps:get(features, Defaults),

        ?assertEqual(archived, maps:get(storage_mode, Capabilities)),
        ?assertEqual(optional, maps:get(e2ee_mode, Capabilities)),
        ?assertEqual(false, maps:get(message_search, Capabilities)),
        ?assertEqual(true, maps:get(channel, Features)),
        ?assertEqual(true, maps:get(moment, Features))
    end).

enterprise_profile_defaults_test_() ->
    ?TEST_SIMPLE(fun() ->
        Defaults = imboy_profile_preset:defaults(enterprise),
        Capabilities = maps:get(capabilities, Defaults),
        Features = maps:get(features, Defaults),

        ?assertEqual(archived, maps:get(storage_mode, Capabilities)),
        ?assertEqual(disabled, maps:get(e2ee_mode, Capabilities)),
        ?assertEqual(true, maps:get(message_search, Capabilities)),
        ?assertEqual(true, maps:get(channel, Features)),
        ?assertEqual(true, maps:get(channel_invitation, Features))
    end).

unknown_profile_falls_back_to_community_test_() ->
    ?TEST_SIMPLE(fun() ->
        Community = imboy_profile_preset:defaults(community),
        Unknown = imboy_profile_preset:defaults(<<"unknown">>),

        ?assertEqual(Community, Unknown)
    end).

overseas_baseline_sensitive_features_disabled_test_() ->
    ?TEST_SIMPLE(fun() ->
        Defaults = imboy_profile_preset:defaults(overseas_baseline),
        Features = maps:get(features, Defaults),

        %% L-01 默认 OFF 清单
        ?assertEqual(false, maps:get(location, Features)),
        ?assertEqual(false, maps:get(channel_discover, Features)),
        ?assertEqual(false, maps:get(channel_order, Features)),
        ?assertEqual(false, maps:get(live_room, Features)),
        ?assertEqual(false, maps:get(ai_marketplace, Features)),
        ?assertEqual(false, maps:get(bot_webhook, Features)),

        %% 核心基线保持可用
        ?assertEqual(true, maps:get(core, Features)),
        ?assertEqual(true, maps:get(e2ee, Features)),
        ?assertEqual(true, maps:get(channel, Features)),
        ?assertEqual(true, maps:get(moment, Features))
    end).

overseas_baseline_capabilities_match_community_test_() ->
    ?TEST_SIMPLE(fun() ->
        Overseas = imboy_profile_preset:defaults(overseas_baseline),
        Community = imboy_profile_preset:defaults(community),

        ?assertEqual(
            maps:get(capabilities, Community),
            maps:get(capabilities, Overseas)
        )
    end).

overseas_baseline_profile_normalize_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(
            [community, enterprise, overseas_baseline, phase1_delivery],
            imboy_profile_preset:supported_profiles()
        ),
        ?assertEqual(overseas_baseline, imboy_profile_preset:normalize_profile(overseas_baseline)),
        ?assertEqual(
            overseas_baseline,
            imboy_profile_preset:normalize_profile(<<"overseas_baseline">>)
        ),
        ?assertEqual(
            overseas_baseline,
            imboy_profile_preset:normalize_profile("overseas_baseline")
        )
    end).

%% Q02（主计划 2026-10-07 §1/§3）：一期交付白名单 profile。
phase1_delivery_whitelist_test_() ->
    ?TEST_SIMPLE(fun() ->
        Defaults = imboy_profile_preset:defaults(phase1_delivery),
        Features = maps:get(features, Defaults),

        %% 一期白名单：Core IM 底座 + 组织/工作区/RBAC + 人工客服 +
        %% 公告频道底座 + 处置申诉（治理）。
        ?assertEqual(true, maps:get(core, Features)),
        ?assertEqual(true, maps:get(channel, Features)),
        ?assertEqual(true, maps:get(appeal, Features)),
        ?assertEqual(true, maps:get(enterprise_business, Features)),
        ?assertEqual(true, maps:get(customer_service, Features)),
        ?assertEqual(true, maps:get(moment, Features)),
        ?assertEqual(true, maps:get(location, Features)),

        %% 二期七域全部关闭。
        lists:foreach(
            fun(K) -> ?assertEqual(false, maps:get(K, Features)) end,
            [e2ee, rtc, live_room, bot_webhook, ai_assistant, agent_hub, mcp]
        ),

        %% 一期默认不承诺域。
        lists:foreach(
            fun(K) -> ?assertEqual(false, maps:get(K, Features)) end,
            [
                wallet,
                moya,
                channel_discover,
                channel_invitation,
                channel_order,
                group_vote,
                group_schedule,
                group_task,
                project
            ]
        ),

        %% 白名单全键枚举：未知/未来新增键不在 map 中即缺省（配合
        %% fail_closed 语义压 false，此处直接断言 known 键之外无 true）。
        TrueKeys = [K || {K, V} <- maps:to_list(Features), V =:= true],
        ?assertEqual(
            lists:sort([
                core, channel, appeal, enterprise_business, customer_service, moment, location
            ]),
            lists:sort(TrueKeys)
        )
    end).

phase1_delivery_capabilities_test_() ->
    ?TEST_SIMPLE(fun() ->
        Defaults = imboy_profile_preset:defaults(phase1_delivery),
        Capabilities = maps:get(capabilities, Defaults),

        %% E2EE OFF 指产品能力：归档（archived）、检索（AC04）保留。
        ?assertEqual(archived, maps:get(storage_mode, Capabilities)),
        ?assertEqual(disabled, maps:get(e2ee_mode, Capabilities)),
        ?assertEqual(true, maps:get(message_search, Capabilities)),
        ?assertEqual(true, maps:get(message_export, Capabilities)),
        ?assertEqual(full, maps:get(audit_mode, Capabilities))
    end).

phase1_delivery_delivery_lock_test_() ->
    ?TEST_SIMPLE(fun() ->
        %% 交付锁：运行时（DB saved / 远端配置）不得重新开启被锁域。
        Locked = imboy_profile_preset:delivery_locked_features(phase1_delivery),
        ?assert(lists:member(e2ee, Locked)),
        ?assert(lists:member(rtc, Locked)),
        ?assert(lists:member(live_room, Locked)),
        ?assert(lists:member(bot_webhook, Locked)),
        ?assert(lists:member(ai_assistant, Locked)),
        ?assert(lists:member(agent_hub, Locked)),
        ?assert(lists:member(mcp, Locked)),
        ?assert(lists:member(wallet, Locked)),
        ?assert(lists:member(moya, Locked)),
        ?assertNot(lists:member(moment, Locked)),
        ?assertNot(lists:member(location, Locked)),

        %% 其它 profile 不受交付锁约束（保持现有行为）。
        ?assertEqual([], imboy_profile_preset:delivery_locked_features(community)),
        ?assertEqual([], imboy_profile_preset:delivery_locked_features(enterprise)),
        ?assertEqual([], imboy_profile_preset:delivery_locked_features(overseas_baseline)),

        %% fail-closed 语义仅 phase1_delivery。
        ?assert(imboy_profile_preset:fail_closed(phase1_delivery)),
        ?assertNot(imboy_profile_preset:fail_closed(community)),
        ?assertNot(imboy_profile_preset:fail_closed(enterprise)),
        ?assertNot(imboy_profile_preset:fail_closed(overseas_baseline))
    end).

phase1_delivery_normalize_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(
            phase1_delivery,
            imboy_profile_preset:normalize_profile(phase1_delivery)
        ),
        ?assertEqual(
            phase1_delivery,
            imboy_profile_preset:normalize_profile(<<"phase1_delivery">>)
        ),
        ?assertEqual(
            phase1_delivery,
            imboy_profile_preset:normalize_profile("phase1_delivery")
        )
    end).
