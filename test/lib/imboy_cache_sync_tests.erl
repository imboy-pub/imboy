%%% @doc imboy_cache_sync V2 协议测试（W3-01..W3-10 + legacy 兼容）
%%%
%%% 每个用例在独立 VM 使用真实缓存组件，不启动完整 App。
%%% 定向注入 V2 事件 / legacy 消息，验证版本比较、幂等、乱序收敛、墓碑、
%%% TTL 剩余、自环过滤、白名单 fail-closed 与恢复状态机。
%%% 隔离测试 namespace（cache_v2_test_rep）不进入生产默认配置。
-module(imboy_cache_sync_tests).
-include_lib("eunit/include/eunit.hrl").
-define(TEST_WITH_CACHE_SYNC(TestFun),
    {timeout, 35, ?_test(pure_mock_fixture:run(fun() -> with_sync_components(TestFun) end))}
).
-include("cache.hrl").
-include("chat.hrl").
-include("imboy_cache_sync_state.hrl").

%% V2 事件直发 sync 进程（绕过 syn，等价远端消息送达；自环过滤用 origin≠node()）
send_v2(Event) ->
    %% 本套件测试已确认来源的应用；未知来源准入由专属回归覆盖。
    %% 用真实报告处理逻辑建立合成身份，不将此 fixture 当成员握手验收。
    confirm_fixture_source(Event),
    Sync = whereis(imboy_cache_sync),
    Sync ! {cache_sync, Event},
    %% 串行化：ping sync 确保消息已处理
    pong = gen_server:call(imboy_cache_sync, ping_ok, 5000),
    ok.

confirm_fixture_source(#{origin_node := Peer, origin_boot_id := Boot} = E) ->
    case
        imboy_cache_sync_proto:validate(E) =:= ok andalso
            maps:get(config_hash, E) =:= imboy_cache_sync_proto:config_hash() andalso
            maps:get(epoch, E) =:= imboy_cache_sync_proto:epoch() andalso
            Peer =/= node() andalso imboy_cache_sync_proto:check_incarnation(Peer, Boot) =/= ok
    of
        true ->
            sys:replace_state(imboy_cache_sync, fun(S) ->
                imboy_cache_sync_recover:handle_version_report(
                    Peer,
                    0,
                    maps:get(epoch, E),
                    maps:get(config_hash, E),
                    maps:get(registry_hash, E),
                    Boot,
                    false,
                    self(),
                    S
                )
            end);
        false ->
            ok
    end;
confirm_fixture_source(_) ->
    ok.

v2_set(Key, Value, Counter, OriginNode, Seq, BootId, ExpiresAt) ->
    base_v2(set, Key, Value, {Counter, OriginNode}, Seq, OriginNode, BootId, ExpiresAt).

%% C01 冻结协议字段（epoch/config_hash/registry_hash）默认取本节点合法值
base_v2(Op, Key, Value, Rev, Seq, OriginNode, BootId, ExpiresAt) ->
    #{
        protocol => 2,
        operation => Op,
        namespace => imboy_cache_policy:namespace_of(Key),
        key => Key,
        value => Value,
        expires_at_ms => ExpiresAt,
        revision => Rev,
        origin_seq => Seq,
        origin_node => OriginNode,
        origin_boot_id => BootId,
        epoch => 0,
        config_hash => imboy_cache_policy:config_hash(),
        registry_hash => imboy_cache_policy:registry_hash()
    }.

v2_delete(Key, Counter, OriginNode, Seq, BootId) ->
    Ev = base_v2(delete, Key, undefined, {Counter, OriginNode}, Seq, OriginNode, BootId, 0),
    Ev#{expires_at_ms := erlang:system_time(millisecond) + 720000}.

v2_invalidate(Key, OriginNode, BootId) ->
    base_v2(invalidate, Key, undefined, {0, OriginNode}, 0, OriginNode, BootId, 0).

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
    ?TEST_WITH_CACHE_SYNC(fun() ->
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
    ?TEST_WITH_CACHE_SYNC(fun() ->
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
                    1,
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
    ?TEST_WITH_CACHE_SYNC(fun() ->
        with_recovery_fixture(fun() ->
            ready = wait_ready(100),
            setup_replicate_env(),
            try
                Key = {cache_v2_test_rep, order1},
                Boot = remote_boot(),
                %% 乱序送达：v3 → v1 → v2，最终收敛到 v3（W3-09）
                ok = send_v2(v2_set(Key, <<"v3">>, 30, 'remote@x', 3, Boot, future_ms(120))),
                ok = send_v2(v2_set(Key, <<"v1">>, 10, 'remote@x', 1, Boot, future_ms(120))),
                ok = send_v2(v2_set(Key, <<"v2">>, 20, 'remote@x', 2, Boot, future_ms(120))),
                ?assertEqual({30, 'remote@x'}, imboy_cache_sync:current_rev(Key)),
                ?assertEqual({ok, <<"v3">>}, depcache:get(Key, imboy_cache)),
                ?assertNotEqual(ready, imboy_cache_sync:status()),
                ?assertEqual(undefined, imboy_cache:get(Key))
            after
                teardown_replicate_env(),
                imboy_cache:put_delete({cache_v2_test_rep, order1})
            end
        end)
    end).

v2_delete_tombstone_blocks_resurrection_test_() ->
    ?TEST_WITH_CACHE_SYNC(fun() ->
        ready = wait_ready(100),
        setup_replicate_env(),
        try
            Key = {cache_v2_test_rep, tomb1},
            Boot = remote_boot(),
            ok = send_v2(v2_set(Key, <<"v1">>, 10, 'remote@x', 1, Boot, future_ms(120))),
            %% DELETE（更高版本）→ 墓碑
            ok = send_v2(v2_delete(Key, 20, 'remote@x', 2, Boot)),
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
    ?TEST_WITH_CACHE_SYNC(fun() ->
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
    ?TEST_WITH_CACHE_SYNC(fun() ->
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
            [{{rev, Key}, {_Rev, _C, StoredExp, live, _}}] =
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
    ?TEST_WITH_CACHE_SYNC(fun() ->
        ready = wait_ready(100),
        setup_replicate_env(),
        try
            Key = {cache_v2_test_rep, selfloop1},
            %% 本地 publish：同步等 ACK 后值与 revision 就位
            ?assertEqual(ok, imboy_cache:put(Key, <<"local_set">>, 120, replicate)),
            [{{rev, Key}, {Rev0, _C0, _E0, live, _}}] =
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
            [{{rev, Key}, {Rev1, _C1, _E1, live, _}}] =
                ets:lookup(imboy_cache_meta, {rev, Key}),
            ?assertEqual(Rev0, Rev1)
        after
            teardown_replicate_env(),
            imboy_cache:put_delete({cache_v2_test_rep, selfloop1})
        end
    end).

v2_unauthorized_key_is_rejected_receiver_side_test_() ->
    ?TEST_WITH_CACHE_SYNC(fun() ->
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
    ?TEST_WITH_CACHE_SYNC(fun() ->
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
    ?TEST_WITH_CACHE_SYNC(fun() ->
        with_recovery_fixture(fun() ->
            ready = wait_ready(100),
            setup_replicate_env(),
            try
                Origin = 'gap_source@x',
                %% 先建立 seq 基线 1
                Key = {cache_v2_test_rep, gap1},
                Boot = remote_boot(),
                ok = send_v2(v2_set(Key, <<"v">>, 10, Origin, 1, Boot, future_ms(120))),
                [{_, 1}] = ets:lookup(imboy_cache_meta, {seq, {Origin, Boot}}),
                %% 跳号（1 → 4）：记录缺口并隐藏待校验副本
                ok = send_v2(v2_set(Key, <<"v2">>, 20, Origin, 4, Boot, future_ms(120))),
                [{_, 4}] = ets:lookup(imboy_cache_meta, {seq, {Origin, Boot}}),
                ?assertNotEqual(ready, imboy_cache_sync:status()),
                ?assertEqual(undefined, imboy_cache:get(Key))
            after
                teardown_replicate_env(),
                imboy_cache:put_delete({cache_v2_test_rep, gap1})
            end
        end)
    end).

%% 合成来源无法提供恢复 authority；断言完成后恢复本 fixture 的初始状态。
with_recovery_fixture(Fun) ->
    Original = sys:get_state(imboy_cache_sync),
    try
        Fun()
    after
        Current = sys:get_state(imboy_cache_sync),
        case Current#state.recovery_timer of
            Ref when is_reference(Ref) -> erlang:cancel_timer(Ref);
            _ -> ok
        end,
        ok = imboy_cache_sync_apply:flush_replicas(),
        sys:replace_state(imboy_cache_sync, fun(_) -> Original end),
        ets:insert(imboy_cache_meta, {status, Original#state.status})
    end.

%% ===================================================================
%% invalidate 事件（远端失效 → fenced gen 推进）
%% ===================================================================

v2_invalidate_event_advances_remote_generation_test_() ->
    ?TEST_WITH_CACHE_SYNC(fun() ->
        Old = application:get_env(imboy, cache_fenced_namespaces, undefined),
        application:set_env(imboy, cache_fenced_namespaces, [cache_v2_test]),
        try
            Key = {cache_v2_test, remote_inv1},
            G0 = imboy_cache_sync:generation(Key),
            ok = send_v2(v2_invalidate(Key, 'remote@x', remote_boot())),
            %% 远端失效推进本地栅栏代次（19.3 跨节点栅栏）
            ?assert(imboy_cache_sync:generation(Key) > G0)
        after
            case Old of
                undefined -> application:unset_env(imboy, cache_fenced_namespaces);
                _ -> application:set_env(imboy, cache_fenced_namespaces, Old)
            end
        end
    end).

%% 19.8：V2 失效发给全部成员，legacy 兼容帧只发旧节点且不回流本节点。
v2_invalidate_targets_legacy_peers_test_() ->
    ?TEST_WITH_CACHE_SYNC(fun() ->
        ready = wait_ready(100),
        Old = application:get_env(imboy, cache_sync_protocol, undefined),
        application:set_env(imboy, cache_sync_protocol, v2),
        meck:new(syn, [passthrough]),
        Parent = self(),
        LegacyPeer = spawn(fun() -> invalidation_test_peer(Parent) end),
        V2Peer = spawn(fun() -> invalidation_test_peer(Parent) end),
        Members = [
            {whereis(imboy_cache_sync), #{cache_protocol => 2}},
            {LegacyPeer, #{}},
            {V2Peer, #{cache_protocol => 2}}
        ],
        try
            meck:expect(syn, publish, 3, fun(_Scope, _Group, _Msg) -> {ok, 1} end),
            meck:expect(syn, members, 2, fun(_Scope, _Group) -> Members end),
            Key = {cache_v2_test, dual_pub1},
            ok = imboy_cache_sync:invalidate(Key),
            Msgs = [
                M
             || {_P, {syn, publish, [_S, _G, {cache_sync, M}]}, _R} <- meck:history(syn)
            ],
            V2Ev = [
                M
             || M <- Msgs,
                is_map(M),
                maps:get(operation, M, none) =:= invalidate,
                maps:get(key, M, none) =:= Key
            ],
            ?assertEqual(1, length(V2Ev)),
            ?assertEqual([], [M || M <- Msgs, is_tuple(M), element(1, M) =:= flush]),
            ?assertEqual({cache_sync, {flush, Key}}, receive_invalidation_peer(LegacyPeer)),
            ?assertEqual(none, receive_invalidation_peer(V2Peer)),
            %% legacy 配置仍向所有远端成员发送 flush 和来源序列收据。
            meck:reset(syn),
            meck:expect(syn, publish, 3, fun(_Scope, _Group, _Msg) -> {ok, 1} end),
            meck:expect(syn, members, 2, fun(_Scope, _Group) -> Members end),
            application:set_env(imboy, cache_sync_protocol, legacy),
            ok = imboy_cache_sync:invalidate(Key),
            Msgs2 = [
                M
             || {_P, {syn, publish, [_S, _G, {cache_sync, M}]}, _R} <- meck:history(syn)
            ],
            case {Msgs2, [M || M <- Msgs2, is_map(M)]} of
                {[{legacy_invalidation_seq, Source, Boot, Seq, Hash, Registry, Key}], []} ->
                    ?assertEqual(node(), Source),
                    ?assertEqual(16, byte_size(Boot)),
                    ?assert(Seq > 0),
                    ?assertEqual(imboy_cache_sync_proto:config_hash(), Hash),
                    ?assertEqual(imboy_cache_sync_proto:registry_hash(), Registry),
                    ?assertEqual({cache_sync, {flush, Key}}, receive_invalidation_peer(LegacyPeer)),
                    ?assertEqual({cache_sync, {flush, Key}}, receive_invalidation_peer(V2Peer)),
                    ok;
                _ ->
                    erlang:error(
                        {legacy_publish_missed, imboy_cache_policy:protocol(), Msgs2}
                    )
            end
        after
            LegacyPeer ! stop,
            V2Peer ! stop,
            meck:unload(syn),
            case Old of
                undefined -> application:unset_env(imboy, cache_sync_protocol);
                _ -> application:set_env(imboy, cache_sync_protocol, Old)
            end
        end
    end).

invalidation_test_peer(Parent) ->
    receive
        stop ->
            ok;
        Message ->
            Parent ! {invalidation_peer_message, self(), Message},
            invalidation_test_peer(Parent)
    end.

receive_invalidation_peer(Pid) ->
    receive
        {invalidation_peer_message, Pid, Message} -> Message
    after 1000 ->
        none
    end.

%% ===================================================================
%% legacy 兼容（19.8：混合期能力交集，行为保持）
%% ===================================================================

legacy_messages_still_work_test_() ->
    ?TEST_WITH_CACHE_SYNC(fun() ->
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
    ?TEST_WITH_CACHE_SYNC(fun() ->
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
    ?TEST_WITH_CACHE_SYNC(fun() ->
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
    ?TEST_WITH_CACHE_SYNC(fun() ->
        %% 回归保护：旧实现的 ets:match('$1') 整行绑定与 [K, V] 列表解构
        %% 失配，sweep 恒空转；且 rev 淘汰时 rindex 行残留无界增长。
        NowMs = erlang:system_time(millisecond),
        Rev = {1, node()},
        Rows = [
            %% 过期墓碑（已跨完整写窗）→ rev + rindex 一并淘汰
            {{rev, f_sweep_tomb}, {Rev, 1, NowMs - 721000, deleted, 0}},
            {{rindex, f_sweep_tomb}, NowMs - 721000},
            %% 过期 live 已跨保护窗 → 淘汰；最近过期版本仍须保留
            {{rev, f_sweep_exp}, {Rev, 1, NowMs - 721000, live, 0}},
            {{rindex, f_sweep_exp}, NowMs - 721000},
            {{rev, f_sweep_guard}, {Rev, 1, NowMs - 1000, live, 0}},
            {{rindex, f_sweep_guard}, NowMs - 1000},
            %% 未过期 live → 保留
            {{rev, f_sweep_live}, {Rev, 1, NowMs + 60000, live, 0}},
            {{rindex, f_sweep_live}, NowMs}
        ],
        ets:insert(imboy_cache_meta, Rows),
        imboy_cache_sync ! reconcile_tick,
        pong = gen_server:call(imboy_cache_sync, ping_ok, 5000),
        ?assertNot(ets:member(imboy_cache_meta, {rev, f_sweep_tomb})),
        ?assertNot(ets:member(imboy_cache_meta, {rindex, f_sweep_tomb})),
        ?assertNot(ets:member(imboy_cache_meta, {rev, f_sweep_exp})),
        ?assertNot(ets:member(imboy_cache_meta, {rindex, f_sweep_exp})),
        ?assert(ets:member(imboy_cache_meta, {rev, f_sweep_guard})),
        ?assert(ets:member(imboy_cache_meta, {rindex, f_sweep_guard})),
        ?assert(ets:member(imboy_cache_meta, {rev, f_sweep_live})),
        ?assert(ets:member(imboy_cache_meta, {rindex, f_sweep_live})),
        %% 清理注入行，不污染后续套件
        ets:delete(imboy_cache_meta, {rev, f_sweep_live}),
        ets:delete(imboy_cache_meta, {rindex, f_sweep_live}),
        ets:delete(imboy_cache_meta, {rev, f_sweep_guard}),
        ets:delete(imboy_cache_meta, {rindex, f_sweep_guard}),
        ok
    end).

queue_depth_alive_returns_real_length_test_() ->
    ?TEST_WITH_CACHE_SYNC(fun() ->
        %% sync 存活时 queue_depth 是真实队列长度（非负）；overloaded 为假
        D = imboy_cache_sync:queue_depth(),
        ?assert(is_integer(D) andalso D >= 0),
        ?assertNot(imboy_cache_sync:overloaded())
    end).

%% ===================================================================
%% C02 双端一致性：epoch / hash / incarnation（收口 V1）
%% ===================================================================

c02_hash_mismatch_event_is_rejected_and_marks_divergent_test_() ->
    ?TEST_WITH_CACHE_SYNC(fun() ->
        ready = wait_ready(50),
        setup_replicate_env(),
        try
            Key = {cache_v2_test_rep, c02_hash1},
            Origin = 'hash_peer@x',
            Boot = remote_boot(),
            %% 首次观察注册化身
            Good = v2_set(Key, <<"ok">>, 10, Origin, 1, Boot, future_ms(120)),
            ok = send_v2(Good),
            ?assertEqual({ok, <<"ok">>}, imboy_cache:get(Key)),
            %% config_hash 不一致：事件整体拒绝，值不被修改，peer 标记 divergent
            Bad = Good#{
                value => <<"sneak">>,
                origin_seq => 2,
                config_hash => crypto:strong_rand_bytes(16)
            },
            ok = send_v2(Bad),
            ?assertEqual({ok, <<"ok">>}, imboy_cache:get(Key)),
            ?assert(imboy_cache_sync_proto:hash_divergent(Origin)),
            %% registry_hash 不一致同样拒绝
            Bad2 = Good#{
                value => <<"sneak2">>,
                origin_seq => 3,
                registry_hash => crypto:strong_rand_bytes(16)
            },
            ok = send_v2(Bad2),
            ?assertEqual({ok, <<"ok">>}, imboy_cache:get(Key))
        after
            teardown_replicate_env(),
            imboy_cache:put_delete({cache_v2_test_rep, c02_hash1})
        end
    end).

c02_missing_protocol_field_is_rejected_test_() ->
    ?TEST_WITH_CACHE_SYNC(fun() ->
        ready = wait_ready(50),
        setup_replicate_env(),
        try
            Key = {cache_v2_test_rep, c02_missing1},
            %% 缺 epoch/config_hash/registry_hash 任一字段 → 结构化拒绝
            NoEpoch = maps:remove(
                epoch,
                v2_set(
                    Key,
                    <<"v">>,
                    10,
                    'remote@x',
                    1,
                    remote_boot(),
                    future_ms(120)
                )
            ),
            NoCfg = maps:remove(
                config_hash,
                v2_set(
                    Key,
                    <<"v">>,
                    10,
                    'remote@x',
                    1,
                    remote_boot(),
                    future_ms(120)
                )
            ),
            NoReg = maps:remove(
                registry_hash,
                v2_set(
                    Key,
                    <<"v">>,
                    10,
                    'remote@x',
                    1,
                    remote_boot(),
                    future_ms(120)
                )
            ),
            [ok = send_v2(E) || E <- [NoEpoch, NoCfg, NoReg]],
            ?assertEqual(undefined, imboy_cache:get(Key))
        after
            teardown_replicate_env()
        end
    end).

c02_stale_epoch_set_is_rejected_invalidate_passes_test_() ->
    ?TEST_WITH_CACHE_SYNC(fun() ->
        ready = wait_ready(50),
        setup_replicate_env(),
        OldF = application:get_env(imboy, cache_fenced_namespaces, undefined),
        application:set_env(imboy, cache_fenced_namespaces, [cache_v2_test]),
        try
            %% 本节点 epoch 推进到 1
            1 = imboy_cache_sync_proto:bump_epoch(),
            Key = {cache_v2_test_rep, c02_epoch1},
            SetEv = v2_set(Key, <<"v">>, 10, 'remote@x', 1, remote_boot(), future_ms(120)),
            %% 远端 epoch=0 < 本节点 1：set 拒绝
            ok = send_v2(SetEv),
            ?assertEqual(undefined, imboy_cache:get(Key)),
            %% invalidate 不参与 epoch 比较（C01：宁多失效）→ fenced gen 推进
            FKey = {cache_v2_test, c02_epoch_inv},
            G0 = imboy_cache_sync:generation(FKey),
            Inv = v2_invalidate(FKey, 'remote@x', remote_boot()),
            ok = send_v2(Inv),
            ?assert(imboy_cache_sync:generation(FKey) > G0),
            %% 恢复 epoch=0 不污染后续测试
            true = ets:insert(imboy_cache_meta, {sync_epoch, 0})
        after
            teardown_replicate_env(),
            case OldF of
                undefined -> application:unset_env(imboy, cache_fenced_namespaces);
                _ -> application:set_env(imboy, cache_fenced_namespaces, OldF)
            end
        end
    end).

%% C01 第 2 节（收口修订 r1）：化身层为检测/注册/补偿，不再拒绝——
%% 新化身首现接受并注册（防复活由 revision/epoch 承担）；
%% 宽限集合内旧化身放行；全新化身事件照常应用。
c02_incarnation_registration_and_grace_test_() ->
    ?TEST_WITH_CACHE_SYNC(fun() ->
        ready = wait_ready(50),
        setup_replicate_env(),
        try
            Origin = 'inc_peer@x',
            Key = {cache_v2_test_rep, c02_inc1},
            NewBoot = remote_boot(),
            OldBoot = remote_boot(),
            true = ets:insert(
                imboy_cache_meta,
                {{peer_incarnation, Origin}, NewBoot}
            ),
            %% fixture 先用报告确认 OldBoot，再验证已确认新化身的应用与补偿。
            Ev = v2_set(Key, <<"new_incarnation">>, 10, Origin, 1, OldBoot, future_ms(120)),
            ok = send_v2(Ev),
            ?assertEqual({ok, <<"new_incarnation">>}, imboy_cache:get(Key)),
            [{_, OldBoot}] = ets:lookup(imboy_cache_meta, {peer_incarnation, Origin}),
            %% 旧化身 NewBoot 进入宽限集合（W 内）
            true = ets:member(
                imboy_cache_meta,
                {grace_incarnation, {Origin, NewBoot}}
            ),
            %% 宽限内旧化身重放：接受（防复活由版本比较承担——低版本拒绝）
            Ev2 = v2_set(Key, <<"stale_replay">>, 2, Origin, 2, NewBoot, future_ms(120)),
            ok = send_v2(Ev2),
            %% 版本 2 < 已应用版本 10 → stale_revision 拒绝，值不变
            ?assertEqual({ok, <<"new_incarnation">>}, imboy_cache:get(Key)),
            %% 已知化身（当前 OldBoot）事件正常应用
            Ev3 = v2_set(Key, <<"fresh">>, 20, Origin, 2, OldBoot, future_ms(120)),
            ok = send_v2(Ev3),
            ?assertEqual({ok, <<"fresh">>}, imboy_cache:get(Key)),
            ets:delete(imboy_cache_meta, {peer_incarnation, Origin}),
            ets:delete(imboy_cache_meta, {grace_incarnation, {Origin, NewBoot}})
        after
            teardown_replicate_env(),
            imboy_cache:put_delete({cache_v2_test_rep, c02_inc1})
        end
    end).

c02_incarnation_change_triggers_fenced_compensation_test_() ->
    ?TEST_WITH_CACHE_SYNC(fun() ->
        ready = wait_ready(50),
        setup_replicate_env(),
        OldF = application:get_env(imboy, cache_fenced_namespaces, undefined),
        application:set_env(imboy, cache_fenced_namespaces, [cache_v2_test_rep]),
        try
            Origin = 'reconnect@x',
            Key = {cache_v2_test_rep, c02_reconnect},
            %% 建立 fenced generation 基线
            G0 = imboy_cache_sync:generation(Key),
            %% 化身变化（peer 重启）：新化身事件触发全量 fenced 补偿（C05）
            Ev = v2_set(Key, <<"v">>, 10, Origin, 1, remote_boot(), future_ms(120)),
            ok = send_v2(Ev),
            %% 化身变化再次发事件
            Ev2 = v2_set(Key, <<"v2">>, 20, Origin, 1, remote_boot(), future_ms(120)),
            ok = send_v2(Ev2),
            %% fenced generation 被推进（有界清理，不依赖 TTL）
            ?assert(imboy_cache_sync:generation(Key) > G0),
            imboy_cache:put_delete({cache_v2_test_rep, c02_reconnect})
        after
            teardown_replicate_env(),
            case OldF of
                undefined -> application:unset_env(imboy, cache_fenced_namespaces);
                _ -> application:set_env(imboy, cache_fenced_namespaces, OldF)
            end
        end
    end).

%% ===================================================================
%% C03 资源预算：Nmax / Tmax / budget_digest
%% ===================================================================

%% 清理 META 表 rindex/rev 残留（预算测试对计数敏感，前序套件留下的
%% 复制键/墓碑行会污染 Nmax/Tmax 判定）
cleanup_meta_rows() ->
    _ = [
        ets:delete(imboy_cache_meta, K)
     || {K, _} <- ets:tab2list(imboy_cache_meta),
        is_tuple(K) andalso tuple_size(K) =:= 2 andalso
            (element(1, K) =:= rindex orelse element(1, K) =:= rev)
    ],
    true = ets:insert(imboy_cache_meta, {rindex_count, 0}),
    ok = imboy_cache_sync_apply:recalibrate_rindex_count(),
    ok.

c03_nmax_full_rejects_new_replicate_test_() ->
    ?TEST_WITH_CACHE_SYNC(fun() ->
        ready = wait_ready(50),
        setup_replicate_env(),
        OldCfg = application:get_env(imboy, cache_replication, #{}),
        ok = cleanup_meta_rows(),
        try
            %% n_max=1：首个新键成功，第二个不同键被拒 index_full
            application:set_env(
                imboy,
                cache_replication,
                maps:merge(OldCfg, #{whitelist => [cache_v2_test_rep], n_max => 1})
            ),
            K1 = {cache_v2_test_rep, c03_nmax1},
            K2 = {cache_v2_test_rep, c03_nmax2},
            ?assertEqual(ok, imboy_cache:put(K1, <<"v">>, 120, replicate)),
            ?assertEqual({error, index_full}, imboy_cache:put(K2, <<"v">>, 120, replicate)),
            %% 既有键更新不受影响（非新键）
            ?assertEqual(ok, imboy_cache:put(K1, <<"v2">>, 120, replicate))
        after
            application:set_env(imboy, cache_replication, OldCfg),
            teardown_replicate_env(),
            imboy_cache:put_delete({cache_v2_test_rep, c03_nmax1})
        end
    end).

c03_tmax_full_triggers_controlled_cold_recovery_test_() ->
    ?TEST_WITH_CACHE_SYNC(fun() ->
        ready = wait_ready(50),
        setup_replicate_env(),
        OldCfg = application:get_env(imboy, cache_replication, #{}),
        ok = cleanup_meta_rows(),
        OriginalState = sys:get_state(imboy_cache_sync),
        try
            %% t_max=1：两次不同键 delete → 墓碑满 → 受控冷恢复（epoch 推进 +
            %% 受管副本清空），删除请求 fail-closed 拒绝
            application:set_env(
                imboy,
                cache_replication,
                maps:merge(OldCfg, #{whitelist => [cache_v2_test_rep], t_max => 1})
            ),
            K1 = {cache_v2_test_rep, c03_tmax1},
            K2 = {cache_v2_test_rep, c03_tmax2},
            ?assertEqual(ok, imboy_cache:put(K1, <<"v">>, 120, replicate)),
            ?assertEqual(ok, imboy_cache:put(K2, <<"v">>, 120, replicate)),
            ?assertEqual(ok, imboy_cache:put_delete(K1)),
            %% 第二个墓碑使预算满：冷恢复 + 拒绝（fail-closed，不部分应用）
            ?assertEqual({error, tombstone_budget_exceeded}, imboy_cache:put_delete(K2)),
            %% epoch 已推进、受管副本（rindex）已清空
            ?assert(imboy_cache_sync_proto:epoch() > 0),
            ?assertNotEqual(ready, imboy_cache_sync:status()),
            ?assertEqual([], [
                K
             || [K] <- ets:match(
                    imboy_cache_meta,
                    {{rindex, '$1'}, '_'}
                )
            ])
        after
            application:set_env(imboy, cache_replication, OldCfg),
            teardown_replicate_env(),
            %% 只恢复测试 fixture；产品冷恢复状态已在断言前真实执行。
            CurrentState = sys:get_state(imboy_cache_sync),
            Ref = CurrentState#state.recovery_timer,
            case is_reference(Ref) of
                true -> erlang:cancel_timer(Ref);
                false -> ok
            end,
            sys:replace_state(imboy_cache_sync, fun(_) -> OriginalState end),
            true = ets:insert(imboy_cache_meta, [{sync_epoch, 0}, {status, ready}])
        end
    end).

c03_budget_digest_is_stable_and_config_sensitive_test_() ->
    ?TEST_WITH_CACHE_SYNC(fun() ->
        D1 = imboy_cache_policy:budget_digest(),
        ?assert(is_binary(D1) andalso byte_size(D1) =:= 16),
        %% 同配置摘要稳定
        ?assertEqual(D1, imboy_cache_policy:budget_digest()),
        %% 预算值变化 → 摘要变化
        Old = application:get_env(imboy, cache_replication, #{}),
        application:set_env(imboy, cache_replication, maps:merge(Old, #{n_max => 49999})),
        D2 = imboy_cache_policy:budget_digest(),
        application:set_env(imboy, cache_replication, Old),
        ?assertNotEqual(D1, D2)
    end).

%% ===================================================================
%% C04 快照回合：round/页/digest/闭合判定（snapshot 模块单元级）
%% ===================================================================

c04_round_requires_done_and_pages_test_() ->
    {timeout, 40, fun c04_round_requires_done_and_pages_checks/0}.

c04_round_requires_done_and_pages_checks() ->
    with_snapshot_authorization(fun() ->
        %% round token 由 new_round 自生成（快照回合身份），测试从状态取出使用
        Deadline = erlang:system_time(millisecond) + 60000,
        St0 = imboy_cache_sync_snapshot:new_round(0, Deadline),
        Round = maps:get(round, St0),
        Peer = 'snap_peer@x',
        St1 = snapshot_authority(St0, Peer),
        %% 仅空 done 不闭合（C04 核心：三条件）
        Entries = [{{rep_ns, k1}, <<"v1">>, future_ms(60), {1, Peer}}],
        Digest = imboy_cache_sync_snapshot:page_digest(Round, 1, Entries),
        {ok, St2} = imboy_cache_sync_snapshot:handle_page(
            Peer, Round, 1, Entries, Digest, true, St1
        ),
        ?assertNot(imboy_cache_sync_snapshot:round_complete(St2)),
        %% done 到齐但 expected_more 悬挂（页未收完）→ 不闭合
        {abort, incomplete_snapshot, St3} = imboy_cache_sync_snapshot:handle_done(
            Peer, Round, 42, 2, St2
        ),
        ?assertNot(imboy_cache_sync_snapshot:round_complete(St3)),
        %% 未知 round 的 done 不推进
        {ok, St4} = imboy_cache_sync_snapshot:handle_done(
            Peer, crypto:strong_rand_bytes(12), 99, 2, St3
        ),
        ?assertNot(imboy_cache_sync_snapshot:round_complete(St4)),
        ?assertEqual(
            0,
            maps:get(
                watermark,
                maps:get(Peer, maps:get(peers, St4))
            )
        ),
        ?assertEqual(1, imboy_cache_sync_snapshot:applied_count(St4)),
        %% 正常时序（协议保证）：页全收齐 → done → 闭合；页收齐但无 done 不闭合
        StA0 = snapshot_authority(
            imboy_cache_sync_snapshot:new_round(
                0, erlang:system_time(millisecond) + 60000
            ),
            Peer
        ),
        RoundA = maps:get(round, StA0),
        PageA1 = [{{rep_ns, a1}, <<"a1">>, future_ms(60), {1, Peer}}],
        {ok, StA1} = imboy_cache_sync_snapshot:handle_page(
            Peer,
            RoundA,
            1,
            PageA1,
            imboy_cache_sync_snapshot:page_digest(RoundA, 1, PageA1),
            true,
            StA0
        ),
        ?assertNot(imboy_cache_sync_snapshot:round_complete(StA1)),
        PageA2 = [{{rep_ns, a2}, <<"a2">>, future_ms(60), {2, Peer}}],
        {ok, StA2} = imboy_cache_sync_snapshot:handle_page(
            Peer,
            RoundA,
            2,
            PageA2,
            imboy_cache_sync_snapshot:page_digest(RoundA, 2, PageA2),
            false,
            StA1
        ),
        ?assertNot(imboy_cache_sync_snapshot:round_complete(StA2)),
        {ok, StA2Cut} = accept_test_source_cut(Peer, StA2),
        {ok, StA3} = imboy_cache_sync_snapshot:handle_done(Peer, RoundA, 42, 2, StA2Cut),
        ?assert(imboy_cache_sync_snapshot:round_complete(StA3)),
        ?assertEqual(2, imboy_cache_sync_snapshot:applied_count(StA3)),
        %% 已完整闭合后的迟到页不再应用。
        {ok, StA3} = imboy_cache_sync_snapshot:handle_page(
            Peer, RoundA, 3, PageA2, 0, false, StA3
        )
    end).

%% 显式 fixture 生命周期预算（peer boot 10s / call 20s / cleanup）；产品 deadline 不变。
%%（与 c04_round_requires_done_and_pages 同款修复；预生成 token 与
%% new_round 自生成的身份不一致，round-match 恒失败——全量 r3 实证）
c04_page_gap_and_digest_mismatch_abort_test_() ->
    {timeout, 40, fun c04_page_gap_and_digest_mismatch_abort_checks/0}.

c04_page_gap_and_digest_mismatch_abort_checks() ->
    with_snapshot_authorization(fun() ->
        St0 = imboy_cache_sync_snapshot:new_round(
            0,
            erlang:system_time(millisecond) + 60000
        ),
        Round = maps:get(round, St0),
        Peer = 'snap_peer2@x',
        St1 = snapshot_authority(St0, Peer),
        %% 页号跳号（1 → 3）→ abort page_gap
        Entries = [{{rep_ns, k}, <<"v">>, future_ms(60), {1, Peer}}],
        Digest = imboy_cache_sync_snapshot:page_digest(Round, 3, Entries),
        {abort, page_gap, _} = imboy_cache_sync_snapshot:handle_page(
            Peer, Round, 3, Entries, Digest, false, St1
        ),
        %% digest 篡改 → abort page_digest_mismatch
        St1b = snapshot_authority(
            imboy_cache_sync_snapshot:new_round(
                0,
                erlang:system_time(millisecond) + 60000
            ),
            Peer
        ),
        <<First, Rest/binary>> = Digest,
        DigestBad = <<(First bxor 1), Rest/binary>>,
        {abort, page_digest_mismatch, _} = imboy_cache_sync_snapshot:handle_page(
            Peer, maps:get(round, St1b), 1, Entries, DigestBad, false, St1b
        )
    end).

%% 显式 fixture 生命周期预算 + round token 从 new_round 状态取（第三个同款修复，
%% 全量 r4 实证：预生成 token 与 new_round 身份不一致 → abort 分支永不命中）
c04_buffer_overflow_abort_test_() ->
    {timeout, 40, fun c04_buffer_overflow_abort_checks/0}.

c04_buffer_overflow_abort_checks() ->
    with_snapshot_authorization(fun() ->
        Old = application:get_env(imboy, cache_replication, #{}),
        application:set_env(imboy, cache_replication, maps:merge(Old, #{b_max => 2})),
        try
            St0 = imboy_cache_sync_snapshot:new_round(
                0,
                erlang:system_time(millisecond) + 60000
            ),
            Peer = 'snap_peer3@x',
            Round = maps:get(round, St0),
            St1 = snapshot_authority(St0, Peer),
            %% 三条目页超过 Bmax=2 → abort buffer_overflow
            Entries = [
                {{rep_ns, a}, <<"v">>, future_ms(60), {1, Peer}},
                {{rep_ns, b}, <<"v">>, future_ms(60), {2, Peer}},
                {{rep_ns, c}, <<"v">>, future_ms(60), {3, Peer}}
            ],
            Digest = imboy_cache_sync_snapshot:page_digest(Round, 1, Entries),
            {abort, buffer_overflow, _} = imboy_cache_sync_snapshot:handle_page(
                Peer, Round, 1, Entries, Digest, false, St1
            )
        after
            application:set_env(imboy, cache_replication, Old)
        end
    end).

%% ===================================================================
%% C05 缺口补偿：fenced namespace 的 generation 推进
%% ===================================================================

c05_seq_gap_compensates_fenced_generation_test_() ->
    ?TEST_WITH_CACHE_SYNC(fun() ->
        with_recovery_fixture(fun() ->
            ready = wait_ready(50),
            OldF = application:get_env(imboy, cache_fenced_namespaces, undefined),
            %% fenced 域必须同时是授权 replicate 域（cache_v2_test 不在 registry，
            %% 事件 not_authorized 被拒、seq-gap 永不触发——全量 r3/单跑实证；
            %% 与 dcheck C9d 同款教训）
            application:set_env(imboy, cache_fenced_namespaces, [cache_v2_test_rep]),
            setup_replicate_env(),
            try
                Origin = 'gap_peer@x',
                Key = {cache_v2_test_rep, c05_gap_key},
                G0 = imboy_cache_sync:generation(Key),
                %% seq 1 → 3 跳号：缺口检测 + fenced namespace 补偿（gen 推进）
                Boot = remote_boot(),
                ok = send_v2(v2_set(Key, <<"v1">>, 1, Origin, 1, Boot, future_ms(120))),
                G1 = imboy_cache_sync:generation(Key),
                ok = send_v2(v2_set(Key, <<"v3">>, 3, Origin, 3, Boot, future_ms(120))),
                ?assert(imboy_cache_sync:generation(Key) > G1),
                ?assert(G1 >= G0)
            after
                teardown_replicate_env(),
                case OldF of
                    undefined -> application:unset_env(imboy, cache_fenced_namespaces);
                    _ -> application:set_env(imboy, cache_fenced_namespaces, OldF)
                end
            end
        end)
    end).

%% ===================================================================
%% Helpers
%% ===================================================================

with_sync_components(Test) ->
    {ok, _} = application:ensure_all_started(crypto),
    {ok, _} = application:ensure_all_started(syn),
    ok = syn:add_node_to_scopes([imboy_cache]),
    Saved = [
        {K, application:get_env(imboy, K)}
     || K <- [
            cluster_nodes,
            dsync_enabled,
            cache_fenced_namespaces,
            cache_sync_protocol,
            cache_replication,
            cache_registry_overrides
        ]
    ],
    Components = cache_fixture_helper:setup(Saved, fun() ->
        application:set_env(imboy, cluster_nodes, []),
        application:set_env(imboy, dsync_enabled, false),
        D = cache_fixture_helper:acquire(imboy_cache, fun() ->
            depcache:start_link(imboy_cache, #{memory_max => 64})
        end),
        M = cache_fixture_helper:acquire(elib_metric, fun elib_metric:start_link/0),
        S = cache_fixture_helper:acquire(imboy_cache_sync, fun imboy_cache_sync:start_link/0),
        ready = wait_ready(50),
        {D, M, S}
    end),
    try
        Test()
    after
        {D0, M0, S0} = Components,
        try
            cache_fixture_helper:cleanup([
                fun() -> cache_fixture_helper:release(S0) end,
                fun() -> cache_fixture_helper:release(M0) end,
                fun() -> cache_fixture_helper:release(D0) end
            ])
        after
            cache_fixture_helper:restore_env(Saved)
        end
    end.

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

snapshot_authority(State, Peer) ->
    requested_peer(
        imboy_cache_sync_snapshot:expect_peer(State, Peer), Peer
    ).

accept_test_source_cut(Peer, Round) ->
    Boot = <<7:128>>,
    Epoch = imboy_cache_sync_proto:epoch(),
    ets:insert(imboy_cache_meta, [
        {{peer_incarnation, Peer}, Boot}, {{peer_epoch, Peer}, Epoch}
    ]),
    Peers = maps:get(peers, Round),
    PeerState = maps:get(Peer, Peers),
    [SourcePid | _] = imboy_cache_sync:syn_scope_members(),
    Round1 = Round#{
        cohort_epoch => Epoch,
        retained_obligations := [],
        peers := Peers#{
            Peer := PeerState#{
                boot => Boot,
                source_epoch => Epoch,
                source_pid => SourcePid,
                origin_target => {Peer, Boot, Epoch, 0}
            }
        }
    },
    Frame =
        {cache_snapshot_cut, Peer, maps:get(round, Round1), Boot, Epoch, 0,
            imboy_cache_sync_proto:config_hash(), imboy_cache_sync_proto:registry_hash(), {ok, []},
            {0, crypto:hash(sha256, term_to_binary([]))}},
    imboy_cache_sync_recover:accept_source_cut(Frame, Round1).

with_snapshot_authorization(Test) ->
    pure_mock_fixture:run(fun() -> snapshot_authorization_checks(Test) end).

snapshot_authorization_checks(Test) ->
    Names = [cache_replication, cache_registry_overrides],
    Previous = [{N, application:get_env(imboy, N)} || N <- Names],
    Cfg = application:get_env(imboy, cache_replication, #{}),
    Overrides = application:get_env(imboy, cache_registry_overrides, #{}),
    OwnMeta = ets:info(imboy_cache_meta) =:= undefined,
    case OwnMeta of
        true -> ets:new(imboy_cache_meta, [named_table, public, set]);
        false -> ok
    end,
    OwnCache = whereis(imboy_cache) =:= undefined,
    try
        meck:new(syn, [passthrough]),
        meck:expect(syn, members, 2, fun(_Scope, _Group) -> [{self(), #{cache_protocol => 2}}] end),
        case OwnCache of
            true -> {ok, _} = depcache:start_link(imboy_cache, #{});
            false -> ok
        end,
        application:set_env(imboy, cache_registry_overrides, Overrides#{
            rep_ns => {replicate, false}
        }),
        application:set_env(imboy, cache_replication, Cfg#{whitelist => [rep_ns]}),
        Test()
    after
        lists:foreach(
            fun
                ({N, undefined}) -> application:unset_env(imboy, N);
                ({N, {ok, Value}}) -> application:set_env(imboy, N, Value)
            end,
            Previous
        ),
        case OwnCache of
            true -> gen_server:stop(imboy_cache);
            false -> ok
        end,
        meck:unload(syn),
        case OwnMeta of
            true -> ets:delete(imboy_cache_meta);
            false -> ok
        end
    end.

requested_peer(Round, Peer) ->
    Authorized = imboy_cache_sync_snapshot:authorize_peer(Round, Peer),
    imboy_cache_sync_snapshot:mark_requested(Authorized, Peer).
