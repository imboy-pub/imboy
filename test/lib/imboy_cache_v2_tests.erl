%%% @doc V2.2.1 缓存 API 与 19.3 generation 栅栏测试（W2/W1-04）
%%%
%%% 受控时序（不用 sleep 猜交错）：
%%% - stale loader：loader 挂起→invalidate→旧 loader 回写旧物理键→新请求不得读旧值；
%%% - 等待者：并发 memo 期间失效，depcache 原生等待者被唤醒后经栅栏校验丢弃旧值；
%%% - 连续失效：两轮栅栏失败后返回权威结果且不缓存（计划 7.2）；
%%% - 版本化依赖标签级联失效。
%%% 测试 namespace（cache_v2_test / cache_v2_test_rep）为隔离测试专用，
%%% 不进入生产默认配置（19.5：不为测试加生产白名单）。
-module(imboy_cache_v2_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("cache.hrl").

-define(FENCED_SETUP(NS), fun() ->
    Old = application:get_env(imboy, cache_fenced_namespaces, undefined),
    application:set_env(imboy, cache_fenced_namespaces, NS),
    Old
end).
-define(FENCED_RESTORE(Old), fun(Old) ->
    case Old of
        undefined -> application:unset_env(imboy, cache_fenced_namespaces);
        _ -> application:set_env(imboy, cache_fenced_namespaces, Old)
    end
end).

%% ===================================================================
%% put/4 与 invalidate/1（计划 4.2/2.2）
%% ===================================================================

put_local_writes_without_broadcast_test_() ->
    ?TEST_WITH_APP(fun() ->
        application:set_env(imboy, dsync_enabled, true),
        try
            Key = {cache_v2_test, put_local_1},
            ?assertEqual(ok, imboy_cache:put(Key, <<"v">>, 60, local)),
            ?assertEqual({ok, <<"v">>}, imboy_cache:get(Key)),
            ?assertEqual(undefined, imboy_cache:get({cache_v2_test, put_local_missing}))
        after
            application:unset_env(imboy, dsync_enabled),
            imboy_cache:delete({cache_v2_test, put_local_1})
        end
    end).

put_replicate_denied_without_authorization_test_() ->
    ?TEST_WITH_APP(fun() ->
        %% 生产默认（白名单空）：任何 Key 的 replicate 请求明确失败，不伪装成功
        Key = {cache_v2_test_rep, denied_1},
        ?assertEqual(
            {error, replicate_not_authorized},
            imboy_cache:put(Key, <<"v">>, 60, replicate)
        ),
        %% 未写入任何值
        ?assertEqual(undefined, imboy_cache:get(Key))
    end).

put_replicate_authorized_in_isolated_test_namespace_test_() ->
    ?TEST_WITH_APP(fun() ->
        %% 启动恢复屏障：replicate 写在 READY 前被拒（5.3），先等就绪
        ready = wait_sync_ready(50),
        %% 隔离测试 namespace：override 登记 + 白名单（仅测试配置）
        application:set_env(
            imboy,
            cache_registry_overrides,
            #{cache_v2_test_rep => {replicate, false}}
        ),
        application:set_env(imboy, cache_replication, #{whitelist => [cache_v2_test_rep]}),
        try
            Key = {cache_v2_test_rep, ok_1},
            ?assertEqual(ok, imboy_cache:put(Key, <<"replicated">>, 120, replicate)),
            %% 本地可见 + rindex 登记（get/1 走版本校验路径）
            ?assertEqual({ok, <<"replicated">>}, imboy_cache:get(Key)),
            %% 版本化删除（4.3 replicate DELETE）：墓碑生效
            ?assertEqual(ok, imboy_cache:put_delete(Key)),
            ?assertEqual(undefined, imboy_cache:get(Key)),
            %% 墓碑后旧 SET 不能复活值（AC-15）：直接写 depcache 逻辑键也不可见
            _ = depcache:set(Key, <<"zombie">>, 60, [], ?DEPCACHE_SERVER),
            ?assertEqual(undefined, imboy_cache:get(Key))
        after
            application:unset_env(imboy, cache_registry_overrides),
            application:unset_env(imboy, cache_replication),
            imboy_cache:put_delete({cache_v2_test_rep, ok_1})
        end
    end).

invalidate_flushes_local_and_advances_generation_test_() ->
    ?TEST_WITH_APP(fun() ->
        {ok, Old} = setup_fenced([cache_v2_test]),
        try
            Key = {cache_v2_test, inv1},
            ?assertEqual(0, imboy_cache_sync:generation(Key)),
            _ = imboy_cache:put(Key, <<"v">>, 60, local),
            ?assertEqual({ok, <<"v">>}, imboy_cache:get(Key)),
            ?assertEqual(ok, imboy_cache:invalidate(Key)),
            %% 逻辑键已清、generation 已推进（19.3：先推进再失效）
            ?assertEqual(undefined, imboy_cache:get(Key)),
            ?assert(imboy_cache_sync:generation(Key) >= 1)
        after
            restore_fenced(Old)
        end
    end).

%% ===================================================================
%% 19.3 受控时序栅栏
%% ===================================================================

%% 场景 1：loader 挂起→invalidate→旧 loader 回写旧物理键。
%% 旧物理键允许存在（TTL 有界），但 memo 入口不得返回旧值（重试取新值）。
fenced_stale_loader_must_not_return_old_value_test_() ->
    ?TEST_WITH_APP(fun() ->
        {ok, Old} = setup_fenced([cache_v2_test]),
        try
            Key = {cache_v2_test, stale_loader_1},
            Controller = self(),
            Counter = counters:new(1, []),
            LoaderRelease = make_ref(),
            Loader = fun() ->
                N = counters:get(Counter, 1) + 1,
                counters:put(Counter, 1, N),
                case N of
                    1 ->
                        %% 第一轮 loader：挂起等待主进程放行（受控时序，无 sleep）
                        Controller ! {loader_started, self()},
                        receive
                            LoaderRelease -> old_value
                        end;
                    _ ->
                        new_value
                end
            end,
            Worker = spawn(fun() ->
                R = imboy_cache:memo(Loader, Key, 60, []),
                Controller ! {memo_result, R}
            end),
            receive
                {loader_started, Worker} -> ok
            end,
            %% loader 挂起期间数据变更（事务 COMMIT 后失效，7.5）
            ?assertEqual(ok, imboy_cache:invalidate(Key)),
            %% 放行旧 loader：旧值写入旧物理键 {cache_v2, Key, 0}
            Worker ! LoaderRelease,
            R1 =
                receive
                    {memo_result, V1} -> V1
                after 5000 -> erlang:error(memo_timeout)
                end,
            %% 栅栏丢弃旧结果，重试取新值
            ?assertEqual(new_value, R1),
            %% 旧物理键存在旧值（19.3 允许，TTL 有界）——但只能从物理键直接读到
            ?assertEqual(
                {ok, old_value},
                depcache:get({cache_v2, Key, 0}, ?DEPCACHE_SERVER)
            )
        after
            restore_fenced(Old)
        end
    end).

%% 场景 2：并发等待者。慢 loader 执行中另一进程 memo 同键（depcache 原生
%% get_wait 挂起等待）。期间失效。等待者被唤醒拿到旧值后必须被栅栏拦截。
fenced_waiter_woken_with_stale_value_is_rejected_test_() ->
    ?TEST_WITH_APP(fun() ->
        {ok, Old} = setup_fenced([cache_v2_test]),
        try
            Key = {cache_v2_test, waiter_1},
            Controller = self(),
            Counter = counters:new(1, []),
            LoaderRelease = make_ref(),
            Loader = fun() ->
                N = counters:get(Counter, 1) + 1,
                counters:put(Counter, 1, N),
                case N of
                    1 ->
                        Controller ! {loader_started, self()},
                        receive
                            LoaderRelease -> old_value
                        end;
                    _ ->
                        new_value
                end
            end,
            Worker1 = spawn(fun() ->
                R = imboy_cache:memo(Loader, Key, 60, []),
                Controller ! {w1_result, R}
            end),
            receive
                {loader_started, Worker1} -> ok
            end,
            %% Worker2 在 loader 挂起时并发 memo：depcache waiter 挂起等待
            _Worker2 = spawn(fun() ->
                R = imboy_cache:memo(Loader, Key, 60, []),
                Controller ! {w2_result, R}
            end),
            timer:sleep(50),
            ?assertEqual(ok, imboy_cache:invalidate(Key)),
            Worker1 ! LoaderRelease,
            R1 =
                receive
                    {w1_result, V1} -> V1
                after 5000 -> erlang:error(w1_timeout)
                end,
            R2 =
                receive
                    {w2_result, V2} -> V2
                after 5000 -> erlang:error(w2_timeout)
                end,
            %% 双方都不得返回旧值（等待者栅栏同样生效）
            ?assertEqual(new_value, R1),
            ?assertEqual(new_value, R2)
        after
            restore_fenced(Old)
        end
    end).

%% 场景 3：连续失效（两轮栅栏都失败）→ 返回权威结果且不缓存（7.2/19.3）
fenced_continuous_invalidation_returns_authoritative_uncached_test_() ->
    ?TEST_WITH_APP(fun() ->
        {ok, Old} = setup_fenced([cache_v2_test]),
        try
            Key = {cache_v2_test, continuous_1},
            Counter = counters:new(1, []),
            %% loader 每次执行都触发新的失效（模拟持续数据变更）
            Loader = fun() ->
                N = counters:get(Counter, 1) + 1,
                counters:put(Counter, 1, N),
                _ = imboy_cache:invalidate(Key),
                {authoritative, N}
            end,
            R = imboy_cache:memo(Loader, Key, 60, []),
            %% 返回权威结果（不返回陈旧成功，不无限递归）
            ?assertMatch({authoritative, _}, R),
            %% 有界：loader 恰好执行 3 次（初试+1 重试+最终权威直取）
            ?assertEqual(3, counters:get(Counter, 1)),
            %% 当前代物理键未缓存任何结果（F 结果未写入）
            G = imboy_cache_sync:generation(Key),
            ?assertEqual(
                undefined,
                depcache:get({cache_v2, Key, G}, ?DEPCACHE_SERVER)
            )
        after
            restore_fenced(Old)
        end
    end).

%% 场景 4：版本化依赖标签。memo 依赖标签 T；T 失效（代次推进+旧物理标签
%% flush）后 depcache 原生级联清除依赖条目，下一次 memo 重新执行 loader。
fenced_versioned_dep_tag_cascades_test_() ->
    ?TEST_WITH_APP(fun() ->
        {ok, Old} = setup_fenced([cache_v2_test]),
        try
            Key = {cache_v2_test, dep_tag_1},
            Tag = {tag, dep_tag_1},
            Counter = counters:new(1, []),
            Loader = fun() ->
                counters:add(Counter, 1, 1),
                <<"loaded">>
            end,
            %% 首次填充：物理键 {cache_v2,Key,0} 依赖 {cache_v2_dep,Tag,0}
            ?assertEqual(<<"loaded">>, imboy_cache:memo(Loader, Key, 60, [Tag])),
            ?assertEqual(1, counters:get(Counter, 1)),
            %% 热命中不重执行
            ?assertEqual(<<"loaded">>, imboy_cache:memo(Loader, Key, 60, [Tag])),
            ?assertEqual(1, counters:get(Counter, 1)),
            ?assertMatch(
                {ok, _},
                depcache:get({cache_v2, Key, 0}, ?DEPCACHE_SERVER)
            ),
            %% 依赖标签失效：代次推进 + flush 旧物理标签 → 级联清除条目
            G = imboy_cache_sync:next_dep_generation(Tag),
            ?assertEqual(1, G),
            ?assertEqual(
                undefined,
                depcache:get({cache_v2, Key, 0}, ?DEPCACHE_SERVER)
            ),
            %% 下次 memo 重新执行 loader（依赖新物理标签）
            ?assertEqual(<<"loaded">>, imboy_cache:memo(Loader, Key, 60, [Tag])),
            ?assertEqual(2, counters:get(Counter, 1))
        after
            restore_fenced(Old)
        end
    end).

%% 场景 5：非 fenced Key 的 memo 完全走旧路径（零行为变化，AC-04）
unfenced_memo_uses_legacy_path_test_() ->
    ?TEST_WITH_APP(fun() ->
        Key = {cache_v2_unfenced_probe, legacy_1},
        ?assertEqual(<<"legacy">>, imboy_cache:memo(fun() -> <<"legacy">> end, Key, 60)),
        %% 物理键未被使用（旧路径写逻辑键）
        ?assertEqual(undefined, depcache:get({cache_v2, Key, 0}, ?DEPCACHE_SERVER)),
        ?assertEqual({ok, <<"legacy">>}, imboy_cache:get(Key))
    end).

%% ===================================================================
%% replicate 状态保护（9.1/5.5）
%% ===================================================================

not_ready_publish_is_rejected_test_() ->
    ?TEST_WITH_APP(fun() ->
        ready = wait_sync_ready(50),
        application:set_env(
            imboy,
            cache_registry_overrides,
            #{cache_v2_test_rep => {replicate, false}}
        ),
        application:set_env(imboy, cache_replication, #{whitelist => [cache_v2_test_rep]}),
        try
            %% 注入 degraded 状态：replicate 写必须明确失败（不伪装成功）
            true = ets:insert(imboy_cache_meta, {status, degraded}),
            Key = {cache_v2_test_rep, not_ready_1},
            ?assertMatch(
                {error, {not_ready, degraded}},
                imboy_cache:put(Key, <<"v">>, 60, replicate)
            ),
            %% 恢复 ready 后可写
            true = ets:insert(imboy_cache_meta, {status, ready}),
            ?assertEqual(ok, imboy_cache:put(Key, <<"v">>, 60, replicate)),
            ?assertEqual({ok, <<"v">>}, imboy_cache:get(Key))
        after
            true = ets:insert(imboy_cache_meta, {status, ready}),
            application:unset_env(imboy, cache_registry_overrides),
            application:unset_env(imboy, cache_replication),
            imboy_cache:put_delete({cache_v2_test_rep, not_ready_1})
        end
    end).

degraded_replicate_reads_return_unverified_undefined_test_() ->
    ?TEST_WITH_APP(fun() ->
        ready = wait_sync_ready(50),
        application:set_env(
            imboy,
            cache_registry_overrides,
            #{cache_v2_test_rep => {replicate, false}}
        ),
        application:set_env(imboy, cache_replication, #{whitelist => [cache_v2_test_rep]}),
        try
            Key = {cache_v2_test_rep, degraded_read_1},
            ?assertEqual(ok, imboy_cache:put(Key, <<"v">>, 60, replicate)),
            %% DEGRADED：未校验旧值不得返回（9.1），读为 miss → 业务回权威
            true = ets:insert(imboy_cache_meta, {status, degraded}),
            ?assertEqual(undefined, imboy_cache:get(Key)),
            true = ets:insert(imboy_cache_meta, {status, ready}),
            ?assertEqual({ok, <<"v">>}, imboy_cache:get(Key))
        after
            true = ets:insert(imboy_cache_meta, {status, ready}),
            application:unset_env(imboy, cache_registry_overrides),
            application:unset_env(imboy, cache_replication),
            imboy_cache:put_delete({cache_v2_test_rep, degraded_read_1})
        end
    end).

%% ===================================================================
%% Helpers
%% ===================================================================

wait_sync_ready(0) ->
    erlang:error(sync_not_ready);
wait_sync_ready(N) ->
    case imboy_cache_sync:status() of
        ready ->
            ready;
        _ ->
            timer:sleep(100),
            wait_sync_ready(N - 1)
    end.

setup_fenced(NS) ->
    Old = application:get_env(imboy, cache_fenced_namespaces, undefined),
    application:set_env(imboy, cache_fenced_namespaces, NS),
    {ok, Old}.

restore_fenced(undefined) ->
    application:unset_env(imboy, cache_fenced_namespaces);
restore_fenced(Old) ->
    application:set_env(imboy, cache_fenced_namespaces, Old).
