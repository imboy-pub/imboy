%%% @doc imboy_cache_sync V2 协议测试（W3-01..W3-10 + legacy 兼容）
%%%
%%% 采用真实 sync 进程（TEST_WITH_APP 后 app 常驻，imboy_cache_sync 无条件启动）
%%% 定向注入 V2 事件 / legacy 消息，验证版本比较、幂等、乱序收敛、墓碑、
%%% TTL 剩余、自环过滤、白名单 fail-closed 与恢复状态机。
%%% 隔离测试 namespace（cache_v2_test_rep）不进入生产默认配置。
-module(imboy_cache_sync_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("cache.hrl").
-include("chat.hrl").

%% V2 事件直发 sync 进程（绕过 syn，等价远端消息送达；自环过滤用 origin≠node()）
send_v2(Event) ->
    Sync = whereis(imboy_cache_sync),
    Sync ! {cache_sync, Event},
    %% 串行化：ping sync 确保消息已处理
    pong = gen_server:call(imboy_cache_sync, ping_ok, 5000),
    ok.

v2_set(Key, Value, Counter, OriginNode, Seq, BootId, ExpiresAt) ->
    #{
        protocol => 2,
        operation => set,
        key => Key,
        value => Value,
        expires_at_ms => ExpiresAt,
        revision => {Counter, OriginNode},
        origin_seq => Seq,
        origin_node => OriginNode,
        origin_boot_id => BootId
    }.

remote_boot() ->
    crypto:strong_rand_bytes(16).

setup_replicate_env() ->
    application:set_env(
        imboy,
        cache_registry_overrides,
        #{cache_v2_test_rep => {replicate, false}}
    ),
    application:set_env(imboy, cache_replication, #{whitelist => [cache_v2_test_rep]}).

teardown_replicate_env() ->
    application:unset_env(imboy, cache_registry_overrides),
    application:unset_env(imboy, cache_replication).

future_ms(Sec) ->
    erlang:system_time(millisecond) + Sec * 1000.

%% ===================================================================
%% V2 SET/DELETE 应用与版本收敛（W3-02/03/05/09）
%% ===================================================================

v2_set_applies_newer_revision_test_() ->
    ?TEST_WITH_APP(fun() ->
        ready = wait_ready(100),
        setup_replicate_env(),
        try
            Key = {cache_v2_test_rep, set1},
            Boot = remote_boot(),
            ok = send_v2(v2_set(Key, <<"v1">>, 10, 'remote@x', 1, Boot, future_ms(120))),
            ?assertEqual({ok, <<"v1">>}, imboy_cache:get(Key)),
            %% 更新版本覆盖
            ok = send_v2(v2_set(Key, <<"v2">>, 20, 'remote@x', 2, Boot, future_ms(120))),
            ?assertEqual({ok, <<"v2">>}, imboy_cache:get(Key)),
            %% 旧版本不能覆盖新版本（5.2：低版本不得覆盖高版本）
            ok = send_v2(v2_set(Key, <<"stale">>, 15, 'remote@x', 3, Boot, future_ms(120))),
            ?assertEqual({ok, <<"v2">>}, imboy_cache:get(Key))
        after
            teardown_replicate_env(),
            imboy_cache:put_delete({cache_v2_test_rep, set1})
        end
    end).

v2_equal_revision_is_idempotent_test_() ->
    ?TEST_WITH_APP(fun() ->
        ready = wait_ready(100),
        setup_replicate_env(),
        try
            Key = {cache_v2_test_rep, idem1},
            Boot = remote_boot(),
            ok = send_v2(v2_set(Key, <<"v1">>, 10, 'remote@x', 1, Boot, future_ms(120))),
            %% 完全相同事件重复送达：状态不变（W3-08）
            ok = send_v2(v2_set(Key, <<"v1">>, 10, 'remote@x', 1, Boot, future_ms(120))),
            ?assertEqual({ok, <<"v1">>}, imboy_cache:get(Key)),
            %% 同 Counter 不同 NodeId：确定性比较（5.2）
            OtherNode =
                case node() of
                    'a@x' -> 'b@y';
                    _ -> 'zz_remote@x'
                end,
            ok = send_v2(
                v2_set(
                    Key,
                    <<"same_counter_higher_node">>,
                    10,
                    OtherNode,
                    4,
                    remote_boot(),
                    future_ms(120)
                )
            ),
            ?assertEqual({ok, <<"same_counter_higher_node">>}, imboy_cache:get(Key))
        after
            teardown_replicate_env(),
            imboy_cache:put_delete({cache_v2_test_rep, idem1})
        end
    end).

v2_out_of_order_events_converge_test_() ->
    ?TEST_WITH_APP(fun() ->
        ready = wait_ready(100),
        setup_replicate_env(),
        try
            Key = {cache_v2_test_rep, order1},
            Boot = remote_boot(),
            %% 乱序送达：v3 → v1 → v2，最终收敛到 v3（W3-09）
            ok = send_v2(v2_set(Key, <<"v3">>, 30, 'remote@x', 3, Boot, future_ms(120))),
            ok = send_v2(v2_set(Key, <<"v1">>, 10, 'remote@x', 1, Boot, future_ms(120))),
            ok = send_v2(v2_set(Key, <<"v2">>, 20, 'remote@x', 2, Boot, future_ms(120))),
            ?assertEqual({ok, <<"v3">>}, imboy_cache:get(Key))
        after
            teardown_replicate_env(),
            imboy_cache:put_delete({cache_v2_test_rep, order1})
        end
    end).

v2_delete_tombstone_blocks_resurrection_test_() ->
    ?TEST_WITH_APP(fun() ->
        ready = wait_ready(100),
        setup_replicate_env(),
        try
            Key = {cache_v2_test_rep, tomb1},
            Boot = remote_boot(),
            ok = send_v2(v2_set(Key, <<"v1">>, 10, 'remote@x', 1, Boot, future_ms(120))),
            %% DELETE（更高版本）→ 墓碑
            ok = send_v2(#{
                protocol => 2,
                operation => delete,
                key => Key,
                revision => {20, 'remote@x'},
                origin_seq => 2,
                origin_node => 'remote@x',
                origin_boot_id => Boot
            }),
            ?assertEqual(undefined, imboy_cache:get(Key)),
            %% 延迟旧 SET（低于墓碑版本）不能复活（AC-15）
            ok = send_v2(v2_set(Key, <<"zombie">>, 11, 'remote@x', 1, Boot, future_ms(120))),
            ?assertEqual(undefined, imboy_cache:get(Key)),
            %% 更新版本的合法 SET 仍可覆盖（统一版本序列，5.2）
            ok = send_v2(v2_set(Key, <<"legit_new">>, 30, 'remote@x', 3, Boot, future_ms(120))),
            ?assertEqual({ok, <<"legit_new">>}, imboy_cache:get(Key))
        after
            teardown_replicate_env(),
            imboy_cache:put_delete({cache_v2_test_rep, tomb1})
        end
    end).

%% ===================================================================
%% TTL 传播（W3-07/6.2：剩余生命周期，不重置）
%% ===================================================================

v2_set_expired_event_is_rejected_test_() ->
    ?TEST_WITH_APP(fun() ->
        setup_replicate_env(),
        try
            Key = {cache_v2_test_rep, expired1},
            Boot = remote_boot(),
            %% 截止时间已过（含时钟偏差扣减）→ 不缓存
            Past = erlang:system_time(millisecond) - 1000,
            ok = send_v2(v2_set(Key, <<"expired">>, 10, 'remote@x', 1, Boot, Past)),
            ?assertEqual(undefined, imboy_cache:get(Key))
        after
            teardown_replicate_env()
        end
    end).

v2_set_near_expiry_applies_shrunk_ttl_test_() ->
    ?TEST_WITH_APP(fun() ->
        ready = wait_ready(100),
        setup_replicate_env(),
        try
            Key = {cache_v2_test_rep, shrink1},
            Boot = remote_boot(),
            %% 剩余约 10s（扣 5s 偏差后 ~5s）：接收端按剩余缓存，不重置完整 TTL
            ExpiresAt = future_ms(10),
            ok = send_v2(v2_set(Key, <<"short">>, 10, 'remote@x', 1, Boot, ExpiresAt)),
            ?assertEqual({ok, <<"short">>}, imboy_cache:get(Key)),
            %% 墓碑元数据的 expires_at 与事件一致（未延长）
            [{{rev, Key}, {_Rev, _C, StoredExp, live}}] =
                ets:lookup(imboy_cache_meta, {rev, Key}),
            ?assertEqual(ExpiresAt, StoredExp)
        after
            teardown_replicate_env(),
            imboy_cache:put_delete({cache_v2_test_rep, shrink1})
        end
    end).

%% ===================================================================
%% 自环、白名单、校验 fail-closed（W3-10/19.5/AC-11）
%% ===================================================================

v2_self_emitted_event_is_not_reapplied_test_() ->
    ?TEST_WITH_APP(fun() ->
        ready = wait_ready(100),
        setup_replicate_env(),
        try
            Key = {cache_v2_test_rep, selfloop1},
            %% 本地 publish：同步等 ACK 后值与 revision 就位
            ?assertEqual(ok, imboy_cache:put(Key, <<"local_set">>, 120, replicate)),
            [{{rev, Key}, {Rev0, _C0, _E0, live}}] =
                ets:lookup(imboy_cache_meta, {rev, Key}),
            %% syn 组回流自环事件（origin_node=node() + 本 boot_id）：
            %% 通过 ets 读 boot_id 构造完整自环事件
            {_, Boot} = imboy_cache_sync:node_boot(),
            Seq = imboy_cache_sync:meta_snapshot(),
            _ = Seq,
            SelfEvent = v2_set(
                Key,
                <<"selfloop_overwrite">>,
                999,
                node(),
                999,
                Boot,
                future_ms(120)
            ),
            ok = send_v2(SelfEvent),
            %% 值未被自环事件覆盖（W3-10）；revision 未被 999 污染
            ?assertEqual({ok, <<"local_set">>}, imboy_cache:get(Key)),
            [{{rev, Key}, {Rev1, _C1, _E1, live}}] =
                ets:lookup(imboy_cache_meta, {rev, Key}),
            ?assertEqual(Rev0, Rev1)
        after
            teardown_replicate_env(),
            imboy_cache:put_delete({cache_v2_test_rep, selfloop1})
        end
    end).

v2_unauthorized_key_is_rejected_receiver_side_test_() ->
    ?TEST_WITH_APP(fun() ->
        ready = wait_ready(100),
        try
            %% 无白名单：接收端独立校验拒绝（发送/接收两端独立校验，19.5）
            Key = {cache_v2_test_rep, unauthorized_1},
            Before = entries_for(Key),
            ok = send_v2(
                v2_set(
                    Key,
                    <<"sneak">>,
                    10,
                    'remote@x',
                    1,
                    remote_boot(),
                    future_ms(120)
                )
            ),
            ?assertEqual(undefined, imboy_cache:get(Key)),
            ?assertEqual(Before, entries_for(Key)),
            %% 敏感/QR Key 同样拒绝
            QRKey = {qr_login, <<"session">>},
            ok = send_v2(
                v2_set(
                    QRKey,
                    <<"token_sneak">>,
                    10,
                    'remote@x',
                    1,
                    remote_boot(),
                    future_ms(120)
                )
            ),
            ?assertEqual(undefined, imboy_cache:get(QRKey))
        after
            ok
        end
    end).

v2_malformed_events_are_rejected_test_() ->
    ?TEST_WITH_APP(fun() ->
        try
            %% 缺字段/类型错误 → 结构化拒绝（计数，不崩溃）
            ok = send_v2(#{protocol => 2, operation => set, key => <<"k">>}),
            ok = send_v2(#{
                protocol => 2,
                operation => unknown_op,
                key => <<"k">>,
                revision => {1, x},
                origin_seq => 1,
                origin_node => x,
                origin_boot_id => <<>>
            }),
            ok = send_v2(not_a_map),
            %% 未知协议版本（未来 19.8 混合版本）→ 计数丢弃，不崩溃
            Sync = whereis(imboy_cache_sync),
            Sync ! {cache_sync, #{protocol => 3, operation => set}},
            pong = gen_server:call(imboy_cache_sync, ping_ok, 5000),
            ?assert(is_process_alive(Sync))
        after
            ok
        end
    end).

%% ===================================================================
%% origin_seq 缺口检测（W4-05）
%% ===================================================================

v2_seq_gap_is_detected_test_() ->
    ?TEST_WITH_APP(fun() ->
        ready = wait_ready(100),
        setup_replicate_env(),
        try
            Origin = 'gap_source@x',
            %% 先建立 seq 基线 5
            Key = {cache_v2_test_rep, gap1},
            Boot = remote_boot(),
            ok = send_v2(v2_set(Key, <<"v">>, 10, Origin, 5, Boot, future_ms(120))),
            [{_, 5}] = ets:lookup(imboy_cache_meta, {seq, Origin}),
            %% 跳号（5 → 8）：缺口被记录
            ok = send_v2(v2_set(Key, <<"v2">>, 20, Origin, 8, Boot, future_ms(120))),
            [{_, 8}] = ets:lookup(imboy_cache_meta, {seq, Origin})
        after
            teardown_replicate_env(),
            imboy_cache:put_delete({cache_v2_test_rep, gap1})
        end
    end).

%% ===================================================================
%% invalidate 事件（远端失效 → fenced gen 推进）
%% ===================================================================

v2_invalidate_event_advances_remote_generation_test_() ->
    ?TEST_WITH_APP(fun() ->
        Old = application:get_env(imboy, cache_fenced_namespaces, undefined),
        application:set_env(imboy, cache_fenced_namespaces, [cache_v2_test]),
        try
            Key = {cache_v2_test, remote_inv1},
            G0 = imboy_cache_sync:generation(Key),
            ok = send_v2(#{
                protocol => 2,
                operation => invalidate,
                key => Key,
                revision => {0, node()},
                origin_seq => 0,
                origin_node => 'remote@x',
                origin_boot_id => remote_boot()
            }),
            %% 远端失效推进本地栅栏代次（19.3 跨节点栅栏）
            ?assert(imboy_cache_sync:generation(Key) > G0)
        after
            case Old of
                undefined -> application:unset_env(imboy, cache_fenced_namespaces);
                _ -> application:set_env(imboy, cache_fenced_namespaces, Old)
            end
        end
    end).

%% ===================================================================
%% legacy 兼容（19.8：混合期能力交集，行为保持）
%% ===================================================================

legacy_messages_still_work_test_() ->
    ?TEST_WITH_APP(fun() ->
        try
            Key = {cache_v2_legacy_probe, legacy_set_1},
            %% legacy {set,...}（含本节点回流语义）正常写
            Sync = whereis(imboy_cache_sync),
            Sync ! {cache_sync, {set, Key, <<"legacy_v">>, 60, []}},
            pong = gen_server:call(imboy_cache_sync, ping_ok, 5000),
            ?assertEqual({ok, <<"legacy_v">>}, imboy_cache:get(Key)),
            %% legacy {flush, Key} 正常清
            Sync ! {cache_sync, {flush, Key}},
            pong = gen_server:call(imboy_cache_sync, ping_ok, 5000),
            ?assertEqual(undefined, imboy_cache:get(Key))
        after
            ok
        end
    end).

legacy_flush_of_fenced_key_advances_generation_test_() ->
    ?TEST_WITH_APP(fun() ->
        Old = application:get_env(imboy, cache_fenced_namespaces, undefined),
        application:set_env(imboy, cache_fenced_namespaces, [cache_v2_test]),
        try
            Key = {cache_v2_test, legacy_fenced1},
            G0 = imboy_cache_sync:generation(Key),
            Sync = whereis(imboy_cache_sync),
            Sync ! {cache_sync, {flush, Key}},
            pong = gen_server:call(imboy_cache_sync, ping_ok, 5000),
            %% 保守栅栏：legacy flush 到达也推进 fenced 代次（混合版本宁可多失效）
            ?assert(imboy_cache_sync:generation(Key) > G0)
        after
            case Old of
                undefined -> application:unset_env(imboy, cache_fenced_namespaces);
                _ -> application:set_env(imboy, cache_fenced_namespaces, Old)
            end
        end
    end).

%% ===================================================================
%% 恢复状态机（W4-01/5.3：单节点空缓存直接 READY）
%% ===================================================================

single_node_startup_reaches_ready_test_() ->
    ?TEST_WITH_APP(fun() ->
        %% app 常驻 sync 已完成 discover：单节点（无其他 scope 成员）→ ready
        ready = wait_ready(50),
        ?assertEqual(ready, imboy_cache_sync:status()),
        %% boot_id 非空（incarnation，5.3）
        {_, Boot} = imboy_cache_sync:node_boot(),
        ?assert(is_binary(Boot) andalso byte_size(Boot) > 0)
    end).

%% ===================================================================
%% 有界清理与过载哨兵（集成后回归保护，cd68af2d）
%% ===================================================================

bounded_sweep_removes_expired_meta_and_rindex_test_() ->
    ?TEST_WITH_APP(fun() ->
        %% 回归保护：旧实现的 ets:match('$1') 整行绑定与 [K, V] 列表解构
        %% 失配，sweep 恒空转；且 rev 淘汰时 rindex 行残留无界增长。
        NowMs = erlang:system_time(millisecond),
        Rev = {1, node()},
        Rows = [
            %% 过期墓碑（已跨完整写窗）→ rev + rindex 一并淘汰
            {{rev, f_sweep_tomb}, {Rev, 1, NowMs - 721000, deleted}},
            {{rindex, f_sweep_tomb}, NowMs - 721000},
            %% 过期 live（值已随 depcache TTL 消失）→ 淘汰
            {{rev, f_sweep_exp}, {Rev, 1, NowMs - 1000, live}},
            {{rindex, f_sweep_exp}, NowMs - 1000},
            %% 未过期 live → 保留
            {{rev, f_sweep_live}, {Rev, 1, NowMs + 60000, live}},
            {{rindex, f_sweep_live}, NowMs}
        ],
        ets:insert(imboy_cache_meta, Rows),
        imboy_cache_sync ! reconcile_tick,
        pong = gen_server:call(imboy_cache_sync, ping_ok, 5000),
        ?assertNot(ets:member(imboy_cache_meta, {rev, f_sweep_tomb})),
        ?assertNot(ets:member(imboy_cache_meta, {rindex, f_sweep_tomb})),
        ?assertNot(ets:member(imboy_cache_meta, {rev, f_sweep_exp})),
        ?assertNot(ets:member(imboy_cache_meta, {rindex, f_sweep_exp})),
        ?assert(ets:member(imboy_cache_meta, {rev, f_sweep_live})),
        ?assert(ets:member(imboy_cache_meta, {rindex, f_sweep_live})),
        %% 清理注入行，不污染后续套件
        ets:delete(imboy_cache_meta, {rev, f_sweep_live}),
        ets:delete(imboy_cache_meta, {rindex, f_sweep_live}),
        ok
    end).

queue_depth_alive_returns_real_length_test_() ->
    ?TEST_WITH_APP(fun() ->
        %% sync 存活时 queue_depth 是真实队列长度（非负）；overloaded 为假
        D = imboy_cache_sync:queue_depth(),
        ?assert(is_integer(D) andalso D >= 0),
        ?assertNot(imboy_cache_sync:overloaded())
    end).

%% ===================================================================
%% Helpers
%% ===================================================================

entries_for(Key) ->
    [
        E
     || {{Tag, K}, _} = E <- imboy_cache_sync:meta_snapshot(),
        K =:= Key,
        lists:member(Tag, [rev, rindex])
    ].

wait_ready(0) ->
    timeout;
wait_ready(N) ->
    case imboy_cache_sync:status() of
        ready ->
            ready;
        _ ->
            timer:sleep(100),
            wait_ready(N - 1)
    end.
