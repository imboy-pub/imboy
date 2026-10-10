%%% @doc imboy_cache_policy 单元测试（W2-04/W2-05/19.5）
%%%
%%% Registry 投影、fail-closed 复制白名单、put/replicate 校验、
%%% 墓碑公式与 fenced 配置边界。
-module(imboy_cache_policy_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%% --- Registry 投影（W0 47 条的抽样核验）---

registry_policy_classification_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(local, imboy_cache_policy:policy({local_cache, <<"x">>})),
        ?assertEqual(local, imboy_cache_policy:policy({apns_jwt, 1})),
        ?assertEqual(local, imboy_cache_policy:policy({cluster_node_info, node()})),
        ?assertEqual(invalidate, imboy_cache_policy:policy({is_friend2, 1, 2})),
        ?assertEqual(invalidate, imboy_cache_policy:policy({is_friend_fields2, 1, 2, []})),
        ?assertEqual(invalidate, imboy_cache_policy:policy({auth_session_epoch, 1})),
        ?assertEqual(invalidate, imboy_cache_policy:policy({user_device_active, 1, <<"d">>})),
        ?assertEqual(invalidate, imboy_cache_policy:policy({in_denylist, 1, 2})),
        %% G-QR_BLOCKED：QR 两类 Key（19.2）
        ?assertEqual(blocked, imboy_cache_policy:policy({qr_login, <<"t">>})),
        ?assertEqual(blocked, imboy_cache_policy:policy({qr_login_qr, <<"q">>})),
        %% 未分类 → legacy（3.3 迁移期合同）
        ?assertEqual(legacy, imboy_cache_policy:policy({some_unknown_ns, 1})),
        ?assertEqual(legacy, imboy_cache_policy:policy(<<"binary_key">>)),
        ?assertEqual(legacy, imboy_cache_policy:policy(undefined))
    end).

unclassified_keys_are_sensitive_fail_closed_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assert(imboy_cache_policy:is_sensitive({some_unknown_ns, 1})),
        ?assert(imboy_cache_policy:is_sensitive(<<"binary_key">>)),
        ?assertNot(imboy_cache_policy:is_sensitive({cluster_node_info, node()})),
        ?assertNot(imboy_cache_policy:is_sensitive({friend_rel_dep, 1, 2})),
        ?assert(imboy_cache_policy:is_sensitive({qr_login, <<"t">>}))
    end).

namespace_of_variants_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(local_cache, imboy_cache_policy:namespace_of({local_cache, sub, <<"x">>})),
        ?assertEqual(is_friend2, imboy_cache_policy:namespace_of({is_friend2, 1, 2, <<"f">>})),
        ?assertEqual(
            user_device_active,
            imboy_cache_policy:namespace_of({user_device_active, 1, <<"d">>})
        ),
        ?assertEqual(undefined, imboy_cache_policy:namespace_of(<<"plain">>)),
        ?assertEqual(undefined, imboy_cache_policy:namespace_of({1, 2}))
    end).

%% --- 复制白名单 fail-closed（19.5/AC-11）---

replicate_denied_by_default_empty_whitelist_test_() ->
    ?TEST_SIMPLE(fun() ->
        %% 生产默认：白名单为空，任何 Key（含非敏感候选）都拒绝
        ?assertNot(imboy_cache_policy:replicate_allowed({cluster_node_info, node()})),
        ?assertNot(imboy_cache_policy:replicate_allowed({some_unknown_ns, 1})),
        ?assertNot(imboy_cache_policy:replicate_allowed({is_friend2, 1, 2})),
        %% QR 两类 Key 即使误加白名单也必须被 blocked 拒绝（19.2，不因配置绕过）
        application:set_env(imboy, cache_replication, #{whitelist => [qr_login, qr_login_qr]}),
        try
            ?assertNot(imboy_cache_policy:replicate_allowed({qr_login, <<"t">>}))
        after
            application:unset_env(imboy, cache_replication)
        end
    end).

replicate_requires_registry_entry_and_whitelist_test_() ->
    ?TEST_SIMPLE(fun() ->
        %% 仅白名单（无 Registry replicate 条目）→ 拒绝
        application:set_env(imboy, cache_replication, #{whitelist => [cache_v2_test_rep]}),
        try
            ?assertNot(imboy_cache_policy:replicate_allowed({cache_v2_test_rep, 1})),
            %% 显式 override 登记 replicate 且非敏感 + 白名单 → 允许
            application:set_env(
                imboy,
                cache_registry_overrides,
                #{cache_v2_test_rep => {replicate, false}}
            ),
            ?assert(imboy_cache_policy:replicate_allowed({cache_v2_test_rep, 1})),
            %% override 敏感 → 拒绝
            application:set_env(
                imboy,
                cache_registry_overrides,
                #{cache_v2_test_rep => {replicate, true}}
            ),
            ?assertNot(imboy_cache_policy:replicate_allowed({cache_v2_test_rep, 1})),
            %% override 非 replicate 策略 → 拒绝
            application:set_env(
                imboy,
                cache_registry_overrides,
                #{cache_v2_test_rep => {invalidate, false}}
            ),
            ?assertNot(imboy_cache_policy:replicate_allowed({cache_v2_test_rep, 1}))
        after
            application:unset_env(imboy, cache_replication),
            application:unset_env(imboy, cache_registry_overrides)
        end
    end).

%% --- validate_replicate（W2-06/W2-02 fail-closed）---

validate_replicate_bounds_test_() ->
    ?TEST_SIMPLE(fun() ->
        application:set_env(imboy, cache_replication, #{whitelist => [cache_v2_test_rep]}),
        application:set_env(
            imboy,
            cache_registry_overrides,
            #{cache_v2_test_rep => {replicate, false}}
        ),
        try
            Key = {cache_v2_test_rep, bounds},
            ?assertEqual(ok, imboy_cache_policy:validate_replicate(Key, <<"v">>, 600)),
            %% TTL 超上界
            ?assertEqual(
                {error, ttl_out_of_range},
                imboy_cache_policy:validate_replicate(Key, <<"v">>, 601)
            ),
            %% 非法 TTL
            ?assertEqual(
                {error, invalid_ttl},
                imboy_cache_policy:validate_replicate(Key, <<"v">>, <<"x">>)
            ),
            %% 值类型禁止（PID/ref/fun）
            ?assertEqual(
                {error, value_type_forbidden},
                imboy_cache_policy:validate_replicate(Key, self(), 60)
            ),
            ?assertEqual(
                {error, value_type_forbidden},
                imboy_cache_policy:validate_replicate(Key, make_ref(), 60)
            ),
            ?assertEqual(
                {error, value_type_forbidden},
                imboy_cache_policy:validate_replicate(Key, fun() -> ok end, 60)
            ),
            ?assertEqual(
                {error, value_type_forbidden},
                imboy_cache_policy:validate_replicate(Key, #{pid => self()}, 60)
            ),
            ?assertEqual(
                {error, value_type_forbidden},
                imboy_cache_policy:validate_replicate(Key, [make_ref()], 60)
            )
        after
            application:unset_env(imboy, cache_replication),
            application:unset_env(imboy, cache_registry_overrides)
        end
    end).

validate_replicate_oversize_value_test_() ->
    ?TEST_SIMPLE(fun() ->
        application:set_env(imboy, cache_replication, #{whitelist => [cache_v2_test_rep]}),
        application:set_env(
            imboy,
            cache_registry_overrides,
            #{cache_v2_test_rep => {replicate, false}}
        ),
        try
            %% 32 KiB 上界（计划 6.1）：构造 33 KiB 二进制
            Oversize = binary:copy(<<$a>>, 33 * 1024),
            ?assertEqual(
                {error, value_too_large},
                imboy_cache_policy:validate_replicate({cache_v2_test_rep, big}, Oversize, 60)
            ),
            InBounds = binary:copy(<<$b>>, 31 * 1024),
            ?assertEqual(
                ok,
                imboy_cache_policy:validate_replicate({cache_v2_test_rep, big2}, InBounds, 60)
            )
        after
            application:unset_env(imboy, cache_replication),
            application:unset_env(imboy, cache_registry_overrides)
        end
    end).

%% --- 墓碑与预算公式（6.3/执行补充第 5 节）---

tombstone_ttl_meets_formula_floor_test_() ->
    ?TEST_SIMPLE(fun() ->
        %% 默认参数：TombstoneTTL >= max(T+C, R+S+D+2C) = max(605, 400)，取 720
        TTL = imboy_cache_policy:tombstone_ttl_sec(),
        ?assert(TTL >= 605),
        ?assertEqual(720, TTL),
        ?assertEqual(600, imboy_cache_policy:max_ttl_sec()),
        ?assertEqual(32768, imboy_cache_policy:max_value_bytes()),
        ?assertEqual(256, imboy_cache_policy:batch_size()),
        ?assertEqual(300, imboy_cache_policy:recovery_window_sec())
    end).

protocol_defaults_to_legacy_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(legacy, imboy_cache_policy:protocol()),
        application:set_env(imboy, cache_sync_protocol, v2),
        try
            ?assertEqual(v2, imboy_cache_policy:protocol())
        after
            application:unset_env(imboy, cache_sync_protocol)
        end
    end).

fenced_namespaces_default_empty_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertNot(imboy_cache_policy:is_fenced({is_friend2, 1, 2})),
        ?assertNot(imboy_cache_policy:is_fenced({some_unknown, 1})),
        application:set_env(imboy, cache_fenced_namespaces, [cache_v2_test]),
        try
            ?assert(imboy_cache_policy:is_fenced({cache_v2_test, 1}))
        after
            application:unset_env(imboy, cache_fenced_namespaces)
        end
    end).
