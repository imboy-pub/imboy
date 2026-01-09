# CETS 分布式缓存实施计划

> **版本**: 0.8.0
> **创建日期**: 2026-01-07
> **目标**: 使用 CETS 实现多节点缓存同步
> **源码**: https://github.com/esl/cets

---

## 📋 项目概述

### 背景
- 当前版本 (0.7.3)：使用 `imboy_cache_sync` + `syn` 进行缓存同步
- 存在问题：消息丢失、重复操作、竞争条件、缺少确认机制
- 解决方案：使用 **CETS (Clustered ETS)** 替换现有同步机制

### 技术选型

| 方案 | 优点 | 缺点 | 选择 |
|------|------|------|------|
| CETS | Ericsson 出品、与 syn 集成、自动复制、生产验证 | 需要额外依赖 | ✅ **采用** |
| Mnesia | 内置、事务支持 | 性能低、磁盘 I/O | ❌ |
| 自研方案 | 完全控制 | 维护成本高 | ❌ |

### CETS 核心特性

```erlang
% 官方仓库: https://github.com/esl/cets
% 作者: Erlang Solutions
% 许可证: Apache License 2.0

% 特性:
% ✅ ETS 表跨节点自动复制
% ✅ insert/delete 操作自动同步
% ✅ 动态节点管理
% ✅ 可插拔发现后端（支持 syn）
% ✅ 性能接近原生 ETS
```

---

## 🎯 实施阶段

### 第一阶段：准备与测试（Week 1-2）
**目标**：熟悉 CETS，完成 POC

### 第二阶段：集成与开发（Week 3-4）
**目标**：集成到现有系统，实现平滑迁移

### 第三阶段：测试与优化（Week 5-6）
**目标**：全面测试，性能优化

### 第四阶段：部署与监控（Week 7-8）
**目标**：灰度发布，建立监控

---

## ✅ 详细任务清单

### 第一阶段：准备与测试（Week 1-2）

#### 任务 1.1：添加依赖
**优先级**: 🔴 高
**负责人**: -
**预估时间**: 0.5 天

```erlang
%% 文件: rebar.config
{deps, [
    {cets, "0.3.0"}
]}.
```

**验收标准**:
- [ ] `rebar3 compile` 成功
- [ ] `rebar3 upgrade` 成功
- [ ] 依赖版本锁定

---

#### 任务 1.2：学习 CETS API
**优先级**: 🟡 中
**负责人**: -
**预估时间**: 1 天

**学习资源**:
- [ ] 阅读 CETS README: https://github.com/esl/cets
- [ ] 查看 API 文档: https://hexdocs.pm/cets
- [ ] 阅读源码: `cets.erl`, `cets_join.erl`, `cets_discovery.erl`
- [ ] 运行示例代码

**关键 API**:
```erlang
% 启动表管理器
cets:start(Tab, Opts) -> {ok, Server}

% 写入操作
cets:insert(Server, Record) -> ok
cets:insert_many(Server, [Record]) -> ok

% 读取操作（直接使用 ETS）
ets:lookup(Tab, Key) -> [Object]

% 删除操作
cets:delete(Server, Key) -> ok
cets:delete_many(Server, [Key]) -> ok
```

---

#### 任务 1.3：创建 POC 测试
**优先级**: 🔴 高
**负责人**: -
**预估时间**: 1 天

```erlang
%% 文件: test/lib/cets_poc_tests.erl
-module(cets_poc_tests).
-include_lib("eunit/include/eunit.hrl").

%% 基础测试
cets_basic_test() ->
    % 启动两个节点
    % 在节点 A 上创建 CETS 表
    % 在节点 B 上加入
    % 测试数据同步
    ok.

%% 性能测试
cets_performance_test() ->
    % 对比 ETS vs CETS 性能
    % 测试 insert/delete 吞吐量
    ok.

%% 节点故障测试
cets_node_failure_test() ->
    % 测试节点宕机时的行为
    % 测试节点恢复时的数据同步
    ok.
```

**验收标准**:
- [ ] 基本 CRUD 测试通过
- [ ] 多节点同步测试通过
- [ ] 节点故障恢复测试通过
- [ ] 性能测试数据收集

---

### 第二阶段：集成与开发（Week 3-4）

#### 任务 2.1：创建 CETS 缓存模块
**优先级**: 🔴 高
**负责人**: -
**预估时间**: 2 天

```erlang
%% 文件: src/lib/imboy_cache_cets.erl
-module(imboy_cache_cets).
-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1]).
-export([set/4, get/1, flush/1, flush/0]).
-export([get_table/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TABLE, imboy_cache_cets).

-record(state, {
    server,
    table
}).

%% ===================================================================
%% API
%% ===================================================================

%% @doc 启动 CETS 缓存服务
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link([]).

%% @doc 启动 CETS 缓存服务（带选项）
-spec start_link(proplists:proplist()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).

%% @doc 设置缓存
-spec set(term(), term(), non_neg_integer(), list()) -> ok.
set(Key, Data, MaxAge, _Depend) ->
    gen_server:call(?SERVER, {set, Key, Data, MaxAge}).

%% @doc 获取缓存
-spec get(term()) -> {ok, term()} | undefined.
get(Key) ->
    gen_server:call(?SERVER, {get, Key}).

%% @doc 删除指定缓存
-spec flush(term()) -> ok.
flush(Key) ->
    gen_server:call(?SERVER, {flush, Key}).

%% @doc 清空所有缓存
-spec flush() -> ok.
flush() ->
    gen_server:call(?SERVER, flush_all).

%% @doc 获取 CETS 表（用于直接访问）
-spec get_table() -> ets:tid().
get_table() ->
    gen_server:call(?SERVER, get_table).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init(Opts) ->
    % 合并默认配置
    DefaultOpts = [
        {discovery, cets_discovery_syn},
        {join_on_sync, true},
        {name, ?TABLE}
    ],
    FinalOpts = lists:ukeymerge(1, lists:sort(Opts), lists:sort(DefaultOpts)),

    % 启动 CETS 表管理器
    case cets:start(?TABLE, FinalOpts) of
        {ok, Server} ->
            ?LOG(info, "CETS cache started successfully", []),
            {ok, #state{server = Server, table = ?TABLE}};
        {error, Reason} ->
            ?LOG(error, "Failed to start CETS: ~p", [Reason]),
            {stop, Reason}
    end.

handle_call({set, Key, Data, MaxAge}, _From, State) ->
    % 计算过期时间戳
    ExpireTS = erlang:system_time(millisecond) + MaxAge * 1000,
    Record = {Key, Data, ExpireTS},
    cets:insert(State#state.server, Record),
    {reply, ok, State};

handle_call({get, Key}, _From, State) ->
    Result = case ets:lookup(State#state.table, Key) of
        [{Key, Data, ExpireTS}] ->
            Now = erlang:system_time(millisecond),
            if
                Now < ExpireTS ->
                    {ok, Data};
                true ->
                    % 已过期，删除
                    cets:delete(State#state.server, Key),
                    undefined
            end;
        [] ->
            undefined
    end,
    {reply, Result, State};

handle_call({flush, Key}, _From, State) ->
    cets:delete(State#state.server, Key),
    {reply, ok, State};

handle_call(flush_all, _From, State) ->
    % 清空表
    ets:delete_all_objects(State#state.table),
    {reply, ok, State};

handle_call(get_table, _From, State) ->
    {reply, State#state.table, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
```

**验收标准**:
- [ ] 模块编译通过
- [ ] gen_server 行为正常
- [ ] 与 CETS 集成成功

---

#### 任务 2.2：修改 imboy_cache 添加后端选择
**优先级**: 🔴 高
**负责人**: -
**预估时间**: 1 天

```erlang
%% 文件: src/lib/imboy_cache.erl

%% 添加后端定义
-define(CACHE_BACKEND_CETS, cets).
-define(CACHE_BACKEND_DEPCACHE, depcache).

%% 修改 start_link 函数
start_link(Args) ->
    Backend = application:get_env(imboy, cache_backend, ?CACHE_BACKEND_DEPCACHE),
    case Backend of
        ?CACHE_BACKEND_CETS ->
            % 启动 CETS 后端
          imboy_cache_cets:start_link(Args);
        ?CACHE_BACKEND_DEPCACHE ->
            % 原有 depcache 后端
            start_link_depcache(Args)
    end.

%% 原有逻辑封装为独立函数
start_link_depcache(Args) ->
    MemoryMax =
        case lists:keyfind(depcache_memory_max, 1, Args) of
            {_, V} -> V;
            false -> undefined
        end,
    depcache:start_link(?DEPCACHE_SERVER,
                        #{memory_max => MemoryMax,
                          callback => {?MODULE, record_depcache_event, [Args]}}),
    {ok, self()}.

%% 修改 set 函数
set(Key, Data, MaxAge, Depend, Server) ->
    Backend = application:get_env(imboy, cache_backend, ?CACHE_BACKEND_DEPCACHE),
    case Backend of
        ?CACHE_BACKEND_CETS ->
            imboy_cache_cets:set(Key, Data, MaxAge, Depend);
        ?CACHE_BACKEND_DEPCACHE ->
            % 原有逻辑
            depcache:set(Key, Data, MaxAge, Depend, Server),
            case should_broadcast(Key) of
                true ->
                    broadcast({set, Key, Data, MaxAge, Depend});
                false ->
                    ok
            end,
            ok
    end.

%% 修改 get 函数
get(Key) ->
    Backend = application:get_env(imboy, cache_backend, ?CACHE_BACKEND_DEPCACHE),
    case Backend of
        ?CACHE_BACKEND_CETS ->
            imboy_cache_cets:get(Key);
        ?CACHE_BACKEND_DEPCACHE ->
            depcache:get(Key, ?DEPCACHE_SERVER)
    end.

%% 修改 flush 函数
flush(Key) ->
    Backend = application:get_env(imboy, cache_backend, ?CACHE_BACKEND_DEPCACHE),
    case Backend of
        ?CACHE_BACKEND_CETS ->
            imboy_cache_cets:flush(Key);
        ?CACHE_BACKEND_DEPCACHE ->
            depcache:flush(Key, ?DEPCACHE_SERVER),
            case application:get_env(imboy, dsync_enabled, false) of
                true ->
                    broadcast({flush, Key});
                false ->
                    ok
            end,
            ok
    end.
```

**验收标准**:
- [ ] 编译通过
- [ ] 两种后端可以切换
- [ ] 不影响现有功能

---

#### 任务 2.3：更新应用启动配置
**优先级**: 🔴 高
**负责人**: -
**预估时间**: 0.5 天

```erlang
%% 文件: src/imboy_app.erl

%% 在监督树中添加 imboy_cache_cets
init([]) ->
    CacheBackend = application:get_env(imboy, cache_backend, depcache),
    CacheChildSpec = case CacheBackend of
        cets ->
            #{id => imboy_cache_cets,
              start => {imboy_cache_cets, start_link, []},
              restart => permanent,
              shutdown => 5000,
              type => worker,
              modules => [imboy_cache_cets]};
        depcache ->
            % 原有配置
            #{
                id => imboy_cache,
                start => {imboy_cache, start_link, [[]]},
                restart => permanent,
                shutdown => 5000,
                type => worker,
                modules => [imboy_cache]
            }
    end,

    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },

    ChildSpecs = [
        CacheChildSpec,
        % ... 其他子进程
    ],

    {ok, {SupFlags, ChildSpecs}}.
```

**验收标准**:
- [ ] 应用正常启动
- [ ] CETS 服务正确启动
- [ ] syn 注册成功

---

#### 任务 2.4：添加配置选项
**优先级**: 🟡 中
**负责人**: -
**预估时间**: 0.5 天

```erlang
%% 文件: config/sys.config

{imboy, [
    % 缓存后端选择: depcache | cets
    {cache_backend, depcache},

    % CETS 配置
    {cets, [
        {discovery, cets_discovery_syn},
        {join_on_sync, true},
        {sync_interval, 5000}
    ]},

    % 原有配置保持不变
    {dsync_enabled, false},
    {depcache_memory_max, 1024}
]}.
```

**验收标准**:
- [ ] 配置文件格式正确
- [ ] 可以切换后端
- [ ] 默认使用 depcache（向后兼容）

---

### 第三阶段：测试与优化（Week 5-6）

#### 任务 3.1：单元测试
**优先级**: 🔴 高
**负责人**: -
**预估时间**: 2 天

```erlang
%% 文件: test/lib/imboy_cache_cets_tests.erl
-module(imboy_cache_cets_tests).
-include_lib("eunit/include/eunit.hrl").

%% 基础 CRUD 测试
crud_test() ->
    {ok, Pid} = imboy_cache_cets:start_link(),
    unlink(Pid),

    % set
    ok = imboy_cache_cets:set(key1, value1, 3600, []),

    % get
    {ok, value1} = imboy_cache_cets:get(key1),

    % flush
    ok = imboy_cache_cets:flush(key1),
    undefined = imboy_cache_cets:get(key1),

    gen_server:stop(Pid),
    ok.

%% TTL 过期测试
ttl_expiration_test() ->
    {ok, Pid} = imboy_cache_cets:start_link(),
    unlink(Pid),

    % 设置 1 秒 TTL
    ok = imboy_cache_cets:set(key2, value2, 1, []),

    % 立即获取
    {ok, value2} = imboy_cache_cets:get(key2),

    % 等待 2 秒
    timer:sleep(2000),

    % 已过期
    undefined = imboy_cache_cets:get(key2),

    gen_server:stop(Pid),
    ok.

%% 批量操作测试
batch_operations_test() ->
    {ok, Pid} = imboy_cache_cets:start_link(),
    unlink(Pid),

    % 批量设置
    lists:foreach(fun(N) ->
        ok = imboy_cache_cets:set(N, value, 3600, [])
    end, lists:seq(1, 1000)),

    % 批量获取
    Results = [imboy_cache_cets:get(N) || N <- lists:seq(1, 1000)],
    ?assertEqual(1000, length([R || R <- Results, R =/= undefined])),

    gen_server:stop(Pid),
    ok.
```

**验收标准**:
- [ ] 所有单元测试通过
- [ ] 代码覆盖率 > 80%
- [ ] 边缘情况测试完整

---

#### 任务 3.2：集成测试
**优先级**: 🔴 高
**负责人**: -
**预估时间**: 2 天

```erlang
%% 文件: test/integration/distributed_cache_tests.erl
-module(distributed_cache_tests).
-include_lib("eunit/include/eunit.hrl").

%% 多节点同步测试
multi_node_sync_test() ->
    % 启动 3 个节点
    % 在节点 A 上写入数据
    % 验证节点 B 和 C 同步成功
    ok.

%% 节点故障恢复测试
node_failure_recovery_test() ->
    % 在 3 节点集群中
    % 停止节点 B
    % 验证节点 A 和 C 继续工作
    % 重启节点 B
    % 验证数据自动同步
    ok.

%% 网络分区测试
network_partition_test() ->
    % 模拟网络分区
    % 验证分区恢复后的数据一致性
    ok.
```

**验收标准**:
- [ ] 多节点测试通过
- [ ] 故障恢复测试通过
- [ ] 分区恢复测试通过

---

#### 任务 3.3：性能测试
**优先级**: 🟡 中
**负责人**: -
**预估时间**: 2 天

```erlang
%% 文件: test/performance/cache_benchmark_tests.erl
-module(cache_benchmark_tests).

%% 吞吐量测试
throughput_test() ->
    % 对比 depcache vs cets
    % 测试 10 万次写操作
    % 测试 100 万次读操作
    ok.

%% 延迟测试
latency_test() ->
    % 测试 P50, P95, P99 延迟
    % 测试不同数据大小的影响
    ok.

%% 内存占用测试
memory_usage_test() ->
    % 测试不同数据量的内存占用
    % 对比 depcache vs cets
    ok.
```

**性能目标**:
- [ ] 读吞吐量: > 100k ops/sec
- [ ] 写吞吐量: > 50k ops/sec
- [ ] P99 延迟: < 10ms
- [ ] 内存占用: < depcache 的 120%

---

#### 任务 3.4：压力测试
**优先级**: 🟡 中
**负责人**: -
**预估时间**: 1 天

```erlang
%% 文件: test/stress/cache_stress_tests.erl
-module(cache_stress_tests).

%% 高并发写入测试
high_concurrency_write_test() ->
    % 100 个进程同时写入
    % 验证数据一致性
    ok.

%% 大数据量测试
large_dataset_test() ->
    % 写入 1000 万条记录
    % 验证性能和稳定性
    ok.

%% 频繁启停节点测试
frequent_node_restart_test() ->
    % 频繁启动/停止节点
    % 验证系统稳定性
    ok.
```

**验收标准**:
- [ ] 无数据丢失
- [ ] 无内存泄漏
- [ ] 无进程泄漏

---

### 第四阶段：部署与监控（Week 7-8）

#### 任务 4.1：添加监控指标
**优先级**: 🟡 中
**负责人**: -
**预估时间**: 1 天

```erlang
%% 文件: src/lib/imboy_cache_cets.erl

%% 添加监控导出函数
-export([get_metrics/0]).

%% @doc 获取缓存指标
-spec get_metrics() -> map().
get_metrics() ->
    Table = get_table(),
    Info = ets:info(Table),

    #{
        table_size => proplists:get_value(size, Info, 0),
        memory => proplists:get_value(memory, Info, 0),
        node_count => length([node() | nodes()]),
        backend => cets
    }.

%% 集成到 telemetry
-record_metrics(Tab) ->
    telemetry:execute(
        [imboy, cache, size],
        #{count => ets:info(Tab, size)},
        #{backend => cets}
    ),
    telemetry:execute(
        [imboy, cache, memory],
        #{bytes => ets:info(Tab, memory)},
        #{backend => cets}
    ).
```

**验收标准**:
- [ ] 指标收集正常
- [ ] 接入监控系统
- [ ] 告警规则配置

---

#### 任务 4.2：灰度发布方案
**优先级**: 🔴 高
**负责人**: -
**预估时间**: 1 天

**发布步骤**:

1. **准备阶段**
   - [ ] 完整备份
   - [ ] 回滚方案准备
   - [ ] 监控就绪

2. **阶段 1：单个节点（10% 流量）**
   - [ ] 选择一个低优先级节点
   - [ ] 配置 `cache_backend = cets`
   - [ ] 观察 24 小时

3. **阶段 2：多个节点（30% 流量）**
   - [ ] 扩展到 3 个节点
   - [ ] 验证数据同步
   - [ ] 观察 48 小时

4. **阶段 3：全部节点（100% 流量）**
   - [ ] 所有节点切换到 CETS
   - [ ] 关闭旧的 `imboy_cache_sync`
   - [ ] 观察 72 小时

5. **完成**
   - [ ] 移除旧代码
   - [ ] 更新文档

---

#### 任务 4.3：文档更新
**优先级**: 🟡 中
**负责人**: -
**预估时间**: 1 天

**需要更新的文档**:

1. **架构文档**
   - [ ] `doc/architecture/database-access.md`
   - [ ] `CLAUDE.md`

2. **API 文档**
   - [ ] 缓存接口说明
   - [ ] 配置选项说明

3. **运维文档**
   - [ ] 部署指南
   - [ ] 故障排查指南
   - [ ] 性能调优指南

4. **变更日志**
   - [ ] `CHANGELOG.md`
   - [ ] 版本说明

---

#### 任务 4.4：代码清理
**优先级**: 🟢 低
**负责人**: -
**预估时间**: 1 天

**清理任务**:
- [ ] 移除 `imboy_cache_sync.erl`（确认不再使用）
- [ ] 移除广播相关代码
- [ ] 更新导出函数
- [ ] 代码审查
- [ ] 格式化代码

---

## 📊 测试矩阵

| 测试类型 | 测试数量 | 覆盖率目标 | 状态 |
|---------|---------|-----------|------|
| 单元测试 | 20+ | > 80% | ⬜ 待开始 |
| 集成测试 | 10+ | > 70% | ⬜ 待开始 |
| 性能测试 | 5+ | - | ⬜ 待开始 |
| 压力测试 | 3+ | - | ⬜ 待开始 |

---

## 🚨 风险与缓解

| 风险 | 影响 | 概率 | 缓解措施 |
|------|------|------|---------|
| 数据不一致 | 🔴 高 | 🟡 中 | 充分测试、灰度发布 |
| 性能下降 | 🟡 中 | 🟢 低 | 性能基准测试 |
| 节点故障 | 🟡 中 | 🟡 中 | 故障恢复测试 |
| 配置错误 | 🟡 中 | 🟡 中 | 配置验证工具 |

---

## 📈 性能基准

### 目标指标

| 指标 | 当前 (depcache) | 目标 (CETS) | 提升 |
|------|----------------|------------|------|
| 读吞吐量 | 100k ops/sec | > 100k ops/sec | 持平 |
| 写吞吐量 | 50k ops/sec | > 50k ops/sec | 持平 |
| P99 延迟 | 5ms | < 10ms | 可接受 |
| 内存占用 | 基准 | < 120% | 可接受 |

---

## 📝 检查清单

### 代码完成前

- [ ] 所有单元测试通过
- [ ] 所有集成测试通过
- [ ] 代码审查完成
- [ ] 文档更新完成
- [ ] 性能测试达标

### 部署前

- [ ] 备份完成
- [ ] 回滚方案准备
- [ ] 监控就绪
- [ ] 告警配置完成
- [ ] 运维培训完成

### 部署后

- [ ] 灰度发布成功
- [ ] 性能指标正常
- [ ] 错误率正常
- [ ] 用户反馈正常
- [ ] 文档归档

---

## 🔗 相关资源

### 官方资源

- **CETS 源码**: https://github.com/esl/cets
- **Hex 文档**: https://hexdocs.pm/cets
- **Hex 包**: https://hex.pm/packages/cets
- **Erlang Solutions**: https://www.erlang-solutions.com/

### 相关文档

- `doc/architecture/database-access.md` - 数据库访问规范
- `CLAUDE.md` - 项目文档
- `src/lib/imboy_cache.erl` - 当前缓存实现
- `src/lib/imboy_cache_sync.erl` - 当前同步实现

---

## 📅 时间线

```
Week 1-2:  准备与测试
           ├─ 添加依赖
           ├─ 学习 API
           └─ POC 测试

Week 3-4:  集成与开发
           ├─ 创建 CETS 模块
           ├─ 修改 imboy_cache
           ├─ 更新启动配置
           └─ 添加配置选项

Week 5-6:  测试与优化
           ├─ 单元测试
           ├─ 集成测试
           ├─ 性能测试
           └─ 压力测试

Week 7-8:  部署与监控
           ├─ 添加监控
           ├─ 灰度发布
           ├─ 文档更新
           └─ 代码清理
```

---

## ✍️ 注意事项

### 向后兼容

1. **保留 depcache 后端**：至少保留一个版本，确保可以快速回滚
2. **配置开关**：使用 `cache_backend` 配置项，方便切换
3. **API 不变**：对外接口 `imboy_cache:set/get/flush` 保持不变

### 数据迁移

1. **无需迁移**：CETS 和 depcache 可以共存，无需数据迁移
2. **逐步切换**：通过配置开关逐步切换到 CETS
3. **双写验证**：可以先同时写两个后端，验证一致性

### 运维建议

1. **监控先行**：先建立完善的监控，再开始切换
2. **小步快跑**：分阶段灰度，每个阶段充分观察
3. **快速回滚**：出现问题立即回滚到 depcache

---

**最后更新**: 2026-01-07
**文档版本**: 1.0
**审核人**: -
**批准人**: -
