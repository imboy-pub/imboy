%%% @doc imboy_cache_policy 单元测试（W2-04/W2-05/19.5）
%%%
%%% Registry 投影、fail-closed 复制白名单、put/replicate 校验、
%%% 墓碑公式与 fenced 配置边界。
-module(imboy_cache_policy_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("imboy_cache_sync_state.hrl").

%% --- Registry 投影（W0 47 条的抽样核验）---

registry_policy_classification_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(local, imboy_cache_policy:policy({local_cache, <<"x">>})),
        ?assertEqual(local, imboy_cache_policy:policy({apns_jwt, 1})),
        ?assertEqual(local, imboy_cache_policy:policy({fcm_access_token_dyn})),
        ?assertEqual(local, imboy_cache_policy:policy(qianfan_access_token)),
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
        ?assertEqual(undefined, imboy_cache_policy:namespace_of({})),
        ?assertEqual(unknown_namespace, imboy_cache_policy:namespace_of(unknown_namespace)),
        ?assertEqual(group, imboy_cache_policy:namespace_of({group, 1, 2, 3, 4, 5})),
        ?assertEqual(undefined, imboy_cache_policy:namespace_of({1, 2}))
    end).

budget_digest_tracks_resource_limits_test() ->
    Previous = application:get_env(imboy, cache_replication),
    try
        application:set_env(imboy, cache_replication, #{p_max => 10, gen_max_bytes => 128}),
        D0 = imboy_cache_policy:budget_digest(),
        application:set_env(imboy, cache_replication, #{p_max => 11, gen_max_bytes => 128}),
        D1 = imboy_cache_policy:budget_digest(),
        ?assertNotEqual(D0, D1),
        application:set_env(imboy, cache_replication, #{p_max => 11, gen_max_bytes => 129}),
        ?assertNotEqual(D1, imboy_cache_policy:budget_digest()),
        lists:foreach(
            fun(Key) ->
                application:set_env(imboy, cache_replication, #{}),
                Base = imboy_cache_policy:budget_digest(),
                application:set_env(imboy, cache_replication, #{Key => 12345}),
                ?assertNotEqual(Base, imboy_cache_policy:budget_digest())
            end,
            [n_max_bytes, t_max_bytes]
        )
    after
        case Previous of
            undefined -> application:unset_env(imboy, cache_replication);
            {ok, Value} -> application:set_env(imboy, cache_replication, Value)
        end
    end.

credential_keys_never_broadcast_test() ->
    Previous = application:get_env(imboy, dsync_enabled),
    application:set_env(imboy, dsync_enabled, true),
    try
        meck:new(depcache, [unstick, passthrough, no_link]),
        meck:new(imboy_cache_sync, [unstick, passthrough, no_link]),
        meck:expect(depcache, set, 5, fun(_, _, _, _, _) -> ok end),
        meck:expect(imboy_cache_sync, broadcast, 1, fun(_) -> {ok, 1} end),
        Keys = [{fcm_access_token_dyn}, qianfan_access_token, {apns_jwt, synthetic}],
        lists:foreach(
            fun(Key) ->
                ?assertEqual(local, imboy_cache_policy:policy(Key)),
                ?assertNot(imboy_cache_policy:replicate_allowed(Key)),
                ?assertEqual(ok, imboy_cache:set(Key, <<"synthetic-credential">>, 60))
            end,
            Keys
        ),
        ?assertEqual(3, meck:num_calls(depcache, set, 5)),
        ?assertEqual(0, meck:num_calls(imboy_cache_sync, broadcast, 1)),
        %% 未分类旧业务保持既有广播合同，防止通过关闭全部广播掩盖问题。
        ?assertEqual(ok, imboy_cache:set({unknown_legacy, synthetic}, <<"value">>, 60)),
        ?assertEqual(1, meck:num_calls(imboy_cache_sync, broadcast, 1))
    after
        lists:foreach(
            fun(Module) ->
                try
                    meck:unload(Module)
                catch
                    error:{not_mocked, _} -> ok
                end
            end,
            [imboy_cache_sync, depcache]
        ),
        case Previous of
            undefined -> application:unset_env(imboy, dsync_enabled);
            {ok, Value} -> application:set_env(imboy, dsync_enabled, Value)
        end
    end.

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

legacy_set_cannot_overwrite_registered_or_managed_keys_test() ->
    %% The full suite owns the named ETS table; keep this mock fixture in its own peer.
    pure_mock_fixture:run(fun legacy_set_guard_checks/0).

legacy_set_guard_checks() ->
    Old = application:get_env(imboy, cache_registry_overrides),
    application:set_env(imboy, cache_registry_overrides, #{
        legacy_guard_replica => {replicate, false}
    }),
    ets:new(imboy_cache_meta, [named_table, public, set]),
    Indexed = {legacy_guard_indexed, key},
    ets:insert(imboy_cache_meta, [{{rindex, Indexed}, true}, {sentinel, preserved}]),
    TombOnly = {legacy_guard_deleted, key},
    ok = imboy_cache_sync_apply:store_revision(
        TombOnly, {1, node()}, erlang:system_time(millisecond) + 60000, deleted
    ),
    meck:new(depcache, [passthrough, no_link]),
    meck:expect(depcache, set, 5, fun(_, _, _, _, _) -> ok end),
    meck:new(syn, [non_strict, no_link]),
    meck:expect(syn, publish, 3, fun(_, _, _) -> {ok, 1} end),
    try
        State = #state{status = ready, boot_id = <<1:128>>},
        Keys = [
            {fcm_access_token_dyn},
            qianfan_access_token,
            {apns_jwt, 1},
            {config5, key},
            {qr_login, key},
            {legacy_guard_replica, key},
            Indexed,
            TombOnly
        ],
        [
            ?assertEqual(
                {noreply, State},
                imboy_cache_sync:handle_info(
                    {cache_sync, {set, K, synthetic_credential, 60, []}}, State
                )
            )
         || K <- Keys
        ],
        [
            ?assertEqual(
                {error, policy_denied},
                imboy_cache:broadcast(
                    {set, K, synthetic_credential, 60, []}
                )
            )
         || K <- Keys
        ],
        ?assertEqual(0, meck:num_calls(syn, publish, 3)),
        ?assertEqual(0, meck:num_calls(depcache, set, 5)),
        ?assertNot(ets:member(imboy_cache_meta, {rindex, TombOnly})),
        ?assertEqual(undefined, imboy_cache:get(TombOnly)),
        ?assertError({unsupported_replicate_api, set}, imboy_cache:set(TombOnly, value, 60)),
        ?assertEqual([{{rindex, Indexed}, true}], ets:lookup(imboy_cache_meta, {rindex, Indexed})),
        ?assertEqual([{sentinel, preserved}], ets:lookup(imboy_cache_meta, sentinel)),
        ?assertEqual(
            {noreply, State},
            imboy_cache_sync:handle_info(
                {cache_sync, {set, {legacy_guard_unknown, key}, value, 60, []}}, State
            )
        ),
        ?assertEqual(1, meck:num_calls(depcache, set, 5)),
        ?assertEqual(
            {ok, 1},
            imboy_cache:broadcast(
                {set, {legacy_guard_unknown, key}, value, 60, []}
            )
        ),
        ?assertEqual({ok, 1}, imboy_cache:broadcast({flush, {fcm_access_token_dyn}})),
        ?assertEqual(2, meck:num_calls(syn, publish, 3))
    after
        meck:unload(syn),
        meck:unload(depcache),
        ets:delete(imboy_cache_meta),
        case Old of
            undefined -> application:unset_env(imboy, cache_registry_overrides);
            {ok, V} -> application:set_env(imboy, cache_registry_overrides, V)
        end
    end.
