# IMBoy Distributed Cache V2.2.1 — 分布式缓存架构与实施验收计划

> **版本**：V2.2.1
> **日期**：2026-10-10
> **状态**：`W0_BLOCKED` / `W1_W5_NOT_AUTHORIZED`
> **目标**：`LOCAL_CANDIDATE_PASS`
> **项目**：IMBoy Erlang/OTP Backend
> **仓库**：`/Users/leeyi/project/imboy.pub/imboy`
> **本次本地代码基线**：`578d83acd53cf9cccb373b6b004a4b2083eecf49`（工作区非 clean，必须另记文件摘要）
> **结束时 HEAD 观察**：`5aff3863efc01ca8f89adadda4a625cb0a0157d1`（其他会话提交 TSID 测试；本轮全部缓存取证源码 SHA 未漂移，压测仍绑定起始基线）
> **设计质量目标**：9.5+/10
> **执行方式**：GPT-5.6 设计复核，GLM-5.3 分阶段实施
> **约束**：纯 Erlang/OTP，不引入 Redis、Mnesia 或其他缓存中间件

## 0. Executive Decision — 最终架构决策

### 0.0 V2.2.1 增量合同与优先级

本版不重建架构，保留以下 V2.2 章节与 W0～W5 顺序；新增第 19 节对冲突条款优先。
本轮仅修订计划和执行 W0；W1～W5、生产数据、部署、push 均不在授权内。
W0 的性能指标属于真实依赖、合成负载的本地证据，不是生产资格或 UAT。
旧路径 `evidence/cache-v2.2/w0/` 仅为历史示例；本轮版本化交付路径改为
`docs/architecture/cache-v2.2.1/w0/`，原始执行证据遵守仓库规则保存在仓外。

### 0.1 核心目标

将 IMBoy 当前基于 `depcache + syn` 的缓存模块优化为一套：

**简洁、高效、高并发、支持分布式 SET 同步、中等数据量、长期容易维护的 Erlang 缓存系统。**

必须满足：

| 维度 | 目标 |
|---|---|
| 实现语言 | 纯 Erlang |
| 本地缓存 | 高并发 ETS 读写 |
| 分布式通信 | 复用 syn |
| SET 同步 | 支持跨节点完整 Key/Value 同步 |
| 失效同步 | 支持跨节点 Key/依赖失效 |
| TTL | 自动过期，避免同步延长生命周期 |
| 缓存防击穿 | 保留 depcache memo/get_wait |
| 内存管理 | 有界容量、过期清理、过载保护 |
| 节点恢复 | 重启、重连及网络分区恢复 |
| 一致性 | 普通缓存最终一致，安全关键状态独立保证 |
| 运维 | 监控、灰度、回滚、故障诊断 |
| 测试 | 单节点、三节点、故障注入、并发压测 |
| 依赖 | 不新增缓存基础设施 |

### 0.2 冻结架构：A 优先，B 按需

**方案 A：depcache + syn + 轻量同步元数据（默认）**

```text
                       IMBoy Business
                             |
                             v
                       imboy_cache
                             |
              +--------------+--------------+
              |                             |
              v                             v
           depcache                  imboy_cache_sync
              |                             |
              v                             v
          Local ETS                        syn
              |                             |
              |                 +-----------+-----------+
              |                 |                       |
              v                 v                       v
           Node A             Node B                  Node C
```

职责：

- `depcache`：本地 KV、TTL、memo、依赖失效、内存管理。
- `imboy_cache`：统一业务 API、策略路由、兼容旧接口。
- `imboy_cache_sync`：分布式 SET、失效消息、版本与节点恢复。
- 必要的轻量 ETS 元数据：版本、墓碑、复制 Key 索引、失效栅栏。
- `syn`：跨节点消息传递与成员发现。
- OTP Supervisor：进程监督和故障恢复。

**方案 A 中允许使用辅助 ETS 保存同步元数据，但不开发第二套完整的缓存引擎。**

**方案 B：depcache + 专用 ETS + syn（条件启用）**

只有 W0 性能测试证明 `depcache` 的写入串行化构成实际瓶颈，且方案 A 的合理优化无法解决时，才考虑将指定高频 SET Key 迁移到独立并发 ETS。

不允许仅因为 ETS 理论上更快就实施方案 B。

### 0.3 V2.2 相对 V2.1 的主要升级

| 编号 | V2.1 不足 | V2.2 处理 |
|---|---|---|
| U01 | Key 策略仅为候选 | 增加完整 Key Registry 与冻结门禁 |
| U02 | confirmed 适用条件不明确 | 增加业务必要性判定，不默认实现 |
| U03 | depcache 提前淘汰影响未定义 | 明确 soft-state 与不可依赖缓存持久性的边界 |
| U04 | Lamport 版本重启不完整 | 增加节点 incarnation、恢复屏障和版本上界 |
| U05 | 事务提交后失效仍有竞态 | 增加并发回填保护 |
| U06 | 墓碑保留期没有计算方法 | 增加公式、窗口限制和冷恢复规则 |
| U07 | 性能 SLO 只有相对指标 | 增加绝对延迟候选值及冻结条件 |
| U08 | 分布式复制可能复制敏感值 | 增加安全白名单与值大小限制 |
| U09 | 混合版本升级边界不清晰 | 增加能力握手和切换门禁 |
| U10 | Agent 执行易扩张任务范围 | 增加逐波次修改边界、自动恢复和停止条件 |

### 0.4 不做的事情

明确排除：

- Redis、Mnesia、Khepri、Raft；
- 分布式事务；
- 通用 CRDT 框架；
- 完整分布式数据库；
- 跨节点强一致 KV；
- 分布式锁服务；
- 自研完整替代 depcache 的缓存框架；
- 复杂 LRU/LFU 淘汰算法；
- 默认引入多层缓存；
- 无性能证据的 ETS 分片；
- 不相关业务重构。

**最终架构要以满足需求为目标，而不是以功能数量为目标。**

## 1. 当前代码基线与问题清单

### 1.1 已核实的事实

依据本地 `578d83acd53cf9cccb373b6b004a4b2083eecf49` 及本轮源码取证：

| 模块 | 已确认事实 | 对 V2.2 的影响 |
|---|---|---|
| `imboy_cache.erl` | 本地 depcache .app vsn 为 git；依赖 HEAD fa72b28ff2d4945ae0a19f4ee42ab576c4714f28，另绑定源码/beam 摘要；dsync_enabled=true 时 SET 向非 local_cache Key 广播 | 不用未经核实的 2.0.0 标签替代实装依赖；引入明确策略 |
| `imboy_cache_sync.erl` | 通过 `syn:publish` 传播 SET/flush | 可以复用，但需要补充版本、恢复和观测 |
| `imboy_sup.erl` | `dsync_enabled` 控制同步进程启动 | 必须兼容现有配置 |
| `account_ds.erl` | 使用 `get_wait` 读取 ID 列表后再修改 | 必须核查并修复原子分配风险 |
| `qr_login_handler.erl` | QR 状态保存在缓存；登录 Token 读取后删除 | 缓存复制不能代替一次性消费 |
| `auth_session_ds.erl` | 存在事务内执行缓存失效的调用 | 必须调整事务边界 |
| `group_member_ds.erl` | 存在事务中更新群成员缓存的调用 | 必须避免事务回滚后缓存错误 |
| `friend_ds.erl` | 使用 depcache 依赖失效，部分字段缓存 Key 不包含 Field | 必须保留依赖语义并修正 Key 隔离 |
| `user_denylist_logic.erl` | 存在 10 天 TTL 的拉黑判定缓存 | 不得让丢失失效事件造成不可控授权风险 |
| `ack_retry_cache.erl` | ACK 标志/timer ref 已有独立本地 ETS，非 depcache | 保留既有专用业务能力，不把它当本轮启用方案 B 的证据 |
| `login_attempt_ds.erl` | 登录失败计数在自有 ETS，login_security_logic 却读 depcache Key；IP 计数另走缓存读改写 | 单独注册 store 例外，不能假设普通缓存复制会修复限流 |

代码参考：

- [imboy_cache.erl](https://github.com/imboy-pub/imboy/blob/main/src/lib/imboy_cache.erl)
- [imboy_cache_sync.erl](https://github.com/imboy-pub/imboy/blob/main/src/lib/imboy_cache_sync.erl)
- [account_ds.erl](https://github.com/imboy-pub/imboy/blob/main/src/ds/account_ds.erl)
- [qr_login_handler.erl](https://github.com/imboy-pub/imboy/blob/main/src/api/qr_login_handler.erl)

### 1.2 depcache 技术定位

`depcache` 已经实现：

- ETS 并发读取；
- 缓存 TTL；
- `memo` 及同 Key 并发加载协调；
- 依赖标签失效；
- 内存限制和过期清理；
- 可选的进程内缓存。

它并非单纯的 `gen_server` 字典。正常的 `get` 可以直接经过 ETS，而写入和失效经由缓存服务进程处理。[Hex](https://hex.pm/packages/depcache/2.0.0/files?utm_source=chatgpt.com)

因此，V2.2 明确规定：

**不重新实现 depcache 已有且满足需求的能力。**

### 1.3 本地取证要求

执行前必须重新读取：

```text
src/lib/imboy_cache.erl
src/lib/imboy_cache_sync.erl
src/lib/imboy_syn.erl
src/lib/imboy_cluster.erl
src/imboy_sup.erl
include/cache.hrl
include/chat.hrl
config/sys.config*
include/deps.mk
test/lib/imboy_cache_tests.erl
test/lib/imboy_cache_sync_tests.erl
```

同时扫描全部实际调用点，包括：

```text
imboy_cache:
depcache:
syn:
dsync_enabled
get_wait
in_process
flush_process_dict
```

检查已有测试和依赖版本，明确：

`CONFIRMED / CHANGED / NOT_VERIFIED`

不得使用旧文档的描述替代当前代码。

---

## 2. 缓存一致性与生命周期合同

### 2.1 正式保留三类缓存策略

| 策略 | 本地存储 | 跨节点行为 | 数据丢失后 | 一致性 |
|---|---|---|---|---|
| `local` | depcache | 无 | 本地重建 | 节点本地 |
| `invalidate` | depcache | 广播失效通知 | 查询权威数据源 | 有界最终一致 |
| `replicate` | depcache，必要时专用 ETS | 广播完整 SET/DELETE | 从存活副本恢复；否则未命中 | 最终一致 |

这里采用一个重要设计修正：

**invalidate 是数据变更后的失效动作，不是普通缓存填充的同步方式。**

例如：

```erlang
%% 数据库查询缓存
imboy_cache:memo(
    fun() -> user_repo:find_by_id(Uid) end,
    {user_profile, Uid},
    300
).

%% 数据库事务成功提交后
imboy_cache:invalidate({user_profile, Uid}).
```

正常查询缓存填充不需要广播到整个集群。

### 2.2 replicate 的精确定义

使用：

```erlang
imboy_cache:put(
    {shared_state, Key},
    Value,
    60,
    replicate
).
```

其语义为：

1. 校验 Key、Value、TTL 和复制权限。
2. 分配更新版本。
3. 写入当前节点缓存。
4. 向当前可达的集群成员发布 SET 事件。
5. 其他节点收到后进行版本校验。
6. 符合版本规则才应用。
7. 通过后续校对机制补偿遗漏。

**replicate 默认是内存软状态复制。**

`ok` 只表示本地写入和本轮发布操作达到约定条件，不代表全部节点收到并持久保存。

若本节点已无法进入可写状态、同步组件不可用或消息超过限制，必须返回明确错误，不能伪装成已复制成功。

### 2.3 depcache 自动淘汰合同

depcache 在达到内存限制时可能提前清理缓存，因此：

`TTL = 最大可缓存时间，不等于数据保证存活时间。`

例如：

```text
Node A                   Node B

SET K = V
TTL = 60s

         replicate
------------------------>
                         SET K = V

                         内存达到限制
                         K 被提前淘汰

GET K = V                GET K = undefined
```

这是允许的 soft-state 行为。

必须满足：

- 缓存提前淘汰不能改变权威业务数据。
- GET 未命中必须可以安全处理。
- 没有存活副本时允许返回未命中。
- 不允许把业务唯一状态仅保存在可能被淘汰的缓存中。
- 不允许把缓存存在性直接当作权限、登录、支付成功的依据。

如果业务需要 TTL 内的数据可靠存活，应使用 PostgreSQL 等已有权威存储，而不是增加一套复杂缓存持久化机制。

### 2.4 confirmed 的启用标准

V2.2 不默认实现 `confirmed`。

W0 必须建立 `confirmed-requirements` 清单，并逐个回答：

| 判断项 | 问题 |
|---|---|
| C01 | 写入成功返回前，业务是否必须确认其他节点已经应用？ |
| C02 | 该状态是否允许节点重启、缓存淘汰后丢失？ |
| C03 | 是否存在多个节点并发更新同一状态？ |
| C04 | 是否要求全局唯一、一次性领取、严格原子性？ |
| C05 | 使用已有 PostgreSQL 权威存储是否更简单？ |

只有在远端应用确认确有必要、允许 soft-state 丢失，并且不需要跨节点原子事务时，才考虑增加：

```erlang
imboy_cache:put_confirmed(Key, Value, TTL, Options).
```

该接口必须等待目标节点实际应用后的 ACK。

`syn:publish` 返回接收者数量不能作为 ACK。syn 的发布实现使用向成员 PID 发送消息的方式，返回数量不代表应用成功。

对于 QR 登录凭据、ID 分配等严格原子业务，**不得采用 confirmed 代替权威原子操作**。

W0 未发现真实需求时：

`CONFIRMED = NOT_REQUIRED`

不实施相关代码。

---

## 3. Key Registry — 必须冻结的缓存策略矩阵

### 3.1 策略真源

增加一份静态的 Key 策略定义。

优先放在现有 `imboy_cache` 中；只有策略数量或复杂度确实需要时才独立为 `imboy_cache_policy.erl`。

每种 Key 必须明确：

| 字段 | 含义 |
|---|---|
| `namespace` | Key 命名空间 |
| `key_pattern` | 实际 Key 结构 |
| `owner` | 所属业务模块 |
| `authority` | 权威数据源 |
| `policy` | local / invalidate / replicate |
| `ttl_sec` | TTL |
| `max_value_bytes` | 最大值 |
| `evictable` | 是否允许提前淘汰 |
| `sensitive` | 是否包含敏感信息 |
| `mutation_hook` | 数据修改后由哪里失效 |
| `recovery` | 节点恢复方式 |
| `failure_policy` | 缓存异常时业务如何处理 |

### 3.2 候选 Key 策略

下表仅用于指导 W0，不视为最终冻结结果。

| Key 类型 | 建议策略 | 权威来源 | 关键注意事项 |
|---|---|---|---|
| `{local_cache, _}` | local | 本地业务状态 | 禁止同步 |
| `{is_friend2, ...}` | invalidate | PostgreSQL | 修复 Field 维度 |
| `{is_friend_fields2, ...}` | invalidate | PostgreSQL | 保留依赖标签 |
| `{friend_rel_dep, ...}` | invalidate | PostgreSQL | 级联失效 |
| `{in_denylist, ...}` | 失效 + 权威校验 | PostgreSQL | 安全敏感 |
| `{auth_session_epoch, ...}` | 失效 + 权威校验 | PostgreSQL | Token 吊销 |
| `{adm_session_epoch, ...}` | 失效 + 权威校验 | PostgreSQL | 管理员会话 |
| `{user_device_active, ...}` | 失效 + 权威校验 | PostgreSQL | 设备撤销 |
| `{group_e2ee_mode, ...}` | 失效 + 权威校验 | PostgreSQL | E2EE 模式切换 |
| 群成员缓存 | invalidate | PostgreSQL | 入群/退群竞态 |
| 业务配置缓存 | invalidate | PostgreSQL | 配置变更传播 |
| `{cluster_node_info, ...}` | local 或 replicate | 集群运行时 | 只复制非敏感信息 |
| `{qr_login, ...}` | 待 G-QR 通过后确定 | 待核实 | 一次性消费 |
| `{qr_login_qr, ...}` | 待 G-QR 通过后确定 | 待核实 | QR Token 映射 |
| APNs JWT | local | 本地可重新生成 | 禁止默认全节点复制 |

### 3.3 未分类 Key

迁移期：

- 旧调用保持 legacy 行为。
- 记录尚未分类的 Key 使用情况。
- 不改变已有返回类型。
- 不直接删除旧同步能力。

V2 启用后：

- 未分类 Key 不允许进入 replicate。
- 不允许依赖 Key 名称字符串猜测业务策略。
- 未分类的业务写入应按冻结合同走显式 legacy 兼容路径或返回配置错误。
- 生产关键 Key 必须完成分类才能切换。

W0 必须输出所有静态和动态 Key 的分类证据。

---

## 4. API 兼容合同

### 4.1 继续保留现有接口

```erlang
imboy_cache:get/1
imboy_cache:get/2
imboy_cache:get_wait/1
imboy_cache:get_subkey/2

imboy_cache:set/2
imboy_cache:set/3
imboy_cache:set/4

imboy_cache:memo/1
imboy_cache:memo/2
imboy_cache:memo/3
imboy_cache:memo/4

imboy_cache:flush/0
imboy_cache:flush/1
imboy_cache:delete/1
imboy_cache:size/0
```

现有进程内缓存辅助接口也必须保留兼容，不能直接删除。

### 4.2 新增最少量接口

```erlang
%% 只写本地
imboy_cache:put(Key, Value, TTL, local).

%% 跨节点复制完整值
imboy_cache:put(Key, Value, TTL, replicate).

%% 数据变更后通知其他节点失效
imboy_cache:invalidate(Key).
```

如果 W0 确认确有业务需要：

```erlang
%% 可选能力
imboy_cache:put_confirmed(Key, Value, TTL, Options).
```

不要修改现有 `set/4` 参数含义，因为第四个参数当前是 depcache 的依赖列表。

### 4.3 DELETE 和 flush

需要明确两种语义：

**depcache invalidate**

清除目标 Key，并保留依赖标签级联失效的行为。

**replicate DELETE**

生成一个带版本的删除事件，跨节点传播，并维护必要的短期墓碑。

现有 `delete/1` 是 `flush/1` 的别名。迁移时必须按 Key 策略选择对应实现，并保证旧调用的结果兼容。

### 4.4 进程字典缓存

W0 检查 `in_process/1` 的实际调用点。

如果业务显式开启 depcache 进程内缓存，必须证明跨节点失效不会让长生命周期进程无限使用旧值。

不能通过清理某个进程的进程字典，就假设其他 Erlang 进程的进程字典也已失效。

V2.2 默认不允许安全关键 Key 使用无法跨进程失效的进程字典缓存。

## 5. 分布式 SET 同步协议 V2

### 5.1 协议结构

建议统一内部事件格式：

```erlang
#{
    protocol => 2,
    operation => set,
    namespace => Namespace,
    key => Key,
    value => Value,
    expires_at_ms => ExpiresAt,
    revision => {LogicalCounter, NodeId},
    origin_seq => OriginSeq,
    origin_node => NodeId,
    origin_boot_id => BootId
}.
```

这是设计合同，并非已实现代码。

三种事件：

```erlang
set
delete
invalidate
```

区别：

| 事件 | 是否携带 Value | 是否维护墓碑 | 操作对象 |
|---|---|---|---|
| SET | 是 | 否 | replicate Key |
| DELETE | 否 | 是 | replicate Key |
| INVALIDATE | 否 | 不需要 SET 墓碑，但需要失效栅栏 | 数据库派生缓存 |

所有同步事件必须经过格式、大小和策略校验。

接收端不得直接相信消息携带的任意 Key、TTL 或操作类型。

### 5.2 Lamport 逻辑版本

V2.2 默认使用 Lamport Logical Clock。

```erlang
Revision = {Counter, NodeId}.
```

比较规则：

1. Counter 较大的版本较新。
2. Counter 相同时，根据 NodeId 做确定性比较。
3. 同版本重复 SET 不重复修改状态。
4. 低版本不能覆盖高版本。
5. DELETE 与 SET 使用统一版本序列。
6. 版本必须在真正应用写入之前完成检查。

**必须注意：`ets:lookup` 后再执行 `ets:insert` 并不是一个原子比较更新。**

Erlang ETS 可以保证单对象更新的原子性，但不能因为单次插入是原子的，就认为多步“检查版本→修改记录”也是原子的。[Erlang.org](https://www.erlang.org/docs/28/apps/stdlib/ets.html?utm_source=chatgpt.com)

**方案 A 的实现约束：**

复制事件统一交给节点本地 `imboy_cache_sync` 进程串行判定和应用。

它可以维护版本、墓碑与同步元数据；最终值仍交由 depcache 保存。

这样优先避免另行实现复杂 ETS CAS 状态机。

如果 W0 证明同步进程写入能力不足，再通过方案 B 评估专用 ETS 的原子条件更新。

### 5.3 节点重启与 incarnation

V2.1 的 `{Counter, NodeId}` 不足以独立解决节点重启问题。

因此 V2.2 增加：

```erlang
Origin = {NodeId, BootId}.
```

其中：

- `NodeId`：稳定且唯一的 Erlang 节点标识。
- `BootId`：每次缓存同步进程重新初始化时生成的随机运行实例标识。
- `Counter`：当前运行实例使用的逻辑版本计数器。

BootId 可以采用 `crypto:strong_rand_bytes(16)` 生成。

**BootId 仅用于识别运行实例，不参与版本的新旧排序。**

节点重新启动时：

```text
STARTING
    |
    v
DISCOVER_PEERS
    |
    v
VERSION_RECOVERY
    |
    v
STATE_RECONCILIATION
    |
    v
READY
```

要求：

1. 同步进程重新启动即获得新的 BootId。
2. 在接受新的复制写入前，必须完成必要的版本上界恢复。
3. 从当前可信节点汇总版本上界。
4. 本地 Counter 必须高于已观察到的最大逻辑计数。
5. 旧 BootId 的延迟消息不能直接覆盖当前状态。
6. 无法识别的旧运行实例消息必须拒绝或进入恢复流程。
7. 配置为多节点的集群，如果重启节点无法完成必要校对，不允许假装已经具备安全的 replicate 写入能力。

单节点部署允许从空缓存状态启动。

全部节点同时重启时，因为系统没有持久化缓存状态，必须将原有复制值视为已经丢失，而不是宣称已经恢复。

### 5.4 消息顺序

Lamport Revision 解决的是不同 SET/DELETE 版本的比较问题。

V2.2 另外保留：

```erlang
origin_seq => OriginSeq
```

OriginSeq 是来源运行实例的连续事件序列号。

作用：

- 检测同一来源的同步事件缺口；
- 判断是否需要发起补偿；
- 防止仅凭收到较新事件就错误认定之前全部事件已送达。

OriginSeq 和 Lamport Counter 不能混为一谈。

收到来自其他节点的较大 Lamport Counter 会影响本地逻辑时钟，但不意味着本节点已经发送了相同数量的事件。

### 5.5 depcache + 版本元数据

方案 A 允许创建一张轻量 ETS 元数据表。

逻辑数据结构：

```erlang
{Key, Namespace, Revision, ExpiresAt, State}
```

其中：

```erlang
State = live | deleted.
```

该 ETS 只承担：

- 版本记录；
- DELETE 墓碑；
- 复制 Key 索引；
- 恢复状态；
- 缓存失效栅栏。

**不在这张表中重复存放完整 Value。**

完整缓存值仍保存在 depcache 中。

同步进程必须控制版本元数据与 depcache 写入的执行顺序。由于它们不是跨两张表的原子事务，一旦进程在中途崩溃，必须将相应状态标记为需要校对，不能在元数据不完整时继续宣称复制数据可信。

本地读取复制 Key 时还必须检查当前同步状态和过期边界，避免 depcache 保留的旧值绕过版本保护。

### 5.6 复制值提前被淘汰

假设：

```text
Metadata:
K -> Revision=100, State=live

depcache:
K -> undefined
```

这属于允许的软状态缓存缺失。

处理规则：

1. GET 返回未命中。
2. 不能自行降低本地版本。
3. 可以从其他节点请求当前版本的数据。
4. 如果远端仍有同版本 Value，可重新填充。
5. 不能接收低版本 Value 来替代已经丢失的高版本值。
6. 如果没有存活副本，则保持未命中。

版本相同的恢复事件也必须确认状态仍为 live，不能覆盖 DELETE 墓碑。

这可以避免为了存储同步元数据而再造一个完整的缓存系统。

---

## 6. TTL、墓碑和恢复窗口

### 6.1 TTL 默认值

第一版候选参数：

| 参数 | 建议默认 |
|---|---:|
| replicate 最大 TTL | 600 秒 |
| 单项最大复制值 | 32 KiB |
| 最大允许节点恢复窗口 | 300 秒 |
| 最大单轮快照时间 | 60 秒 |
| 最大传播延迟预算 | 30 秒 |
| 时钟偏差预算 | 5 秒 |
| 墓碑最小保留时间 | 由公式计算 |
| 墓碑初始建议保留时间 | 720 秒 |

这些是 V2.2 的候选配置，不是已经实测的最佳参数。

### 6.2 TTL 计算方式

SET 事件携带原始截止时间：

```erlang
ExpiresAt = OriginWallTimeMs + TTLms.
```

接收节点根据自己的时钟计算剩余生命周期。

计算时必须考虑时钟偏差预算，并保守取整，不能重新应用完整 TTL。

例如：

```text
Node A:

10:00:00 SET K
TTL = 60s

Node B:

10:00:20 收到事件
剩余 TTL 约 40s

Node C:

10:01:05 收到事件
原值已经过期
不能继续缓存
```

保守提前过期可以接受。

超过配置的时钟偏差时，不能对需要严格过期保证的复制状态继续使用不可信的剩余 TTL。

### 6.3 墓碑保留公式

定义：

- `T`：最大复制 TTL；
- `R`：允许的节点断线恢复窗口；
- `S`：最大快照恢复时间；
- `D`：最大传播延迟预算；
- `C`：节点间时钟偏差预算。

候选下界：

```text
TombstoneTTL >= max(
    T + C,
    R + S + D + 2*C
)
```

根据默认值：

```text
T = 600
R = 300
S = 60
D = 30
C = 5

TombstoneTTL >= max(
    605,
    400
)

TombstoneTTL >= 605 秒
```

考虑实现余量，候选采用：

**720 秒。**

这只是满足有限窗口模型的初始参数，不是对任意时间延迟、无限网络分区或无限消息排队的保证。

此外必须验证：

- 原 SET 截止时间在重传时保持不变；
- 过期消息不会重新计算 TTL；
- 来源运行实例校验有效；
- 超过恢复窗口的节点必须冷恢复；
- 旧实例残留消息不能绕过恢复门禁。

### 6.4 墓碑过期

墓碑过期后，不能无条件接纳旧消息。

必须同时满足：

1. 旧 Value 的有效期已经结束；或者
2. 已通过版本和运行实例屏障排除历史消息。

如果无法证明旧版本不会复活数据，则应进入恢复模式，而不是删除必要的保护元数据。

对于长期离线节点：

```text
offline > RecoveryWindow
             |
             v
        COLD_RECOVERY
             |
       丢弃旧复制状态
             |
       校对当前可信节点
             |
          READY
```

禁止直接将该节点离线前的缓存重新传播到集群。

---

## 7. INVALIDATE 与缓存回填竞态

这是 V2.2 最重要的正确性增强之一。

### 7.1 必须覆盖的竞态

```text
Node A                              Node B

查询数据库旧值
                                    更新数据库
                                    COMMIT
                                    invalidate(K)
接收到失效事件

旧数据库查询返回
memo 把旧值重新写入缓存
```

如果只在事务提交后广播失效，仍然不能完全防止上述竞态。

### 7.2 建议采用轻量失效栅栏

对 invalidate 类缓存维护本地 Generation。

缓存读取或 memo 开始前读取版本：

```erlang
G0 = generation(Key).
```

缓存加载完成后再次检查：

```erlang
G1 = generation(Key).
```

如果：

```erlang
G0 =/= G1
```

说明查询过程中发生过失效。

必须丢弃本轮可能陈旧的缓存结果、确保旧填充值被清除，再进行有界重试。

具体实现必须验证 depcache `memo/get_wait` 的并发等待和依赖失效语义，不能简单在外面包两次判断就宣称已经消除全部竞态。

同时要求：

- Generation 更新与本地失效操作采用受控顺序；
- 每次缓存填充都遵守同一栅栏协议；
- 不能存在绕过统一缓存入口的写入；
- 无法协调时返回未命中或权威查询结果；
- 不能无限重试；
- 出错时不得将旧结果保留为新的有效缓存。

Generation 可以按 Key 或命名空间管理，优先复用同步元数据表。

### 7.3 节点重连后的失效补偿

不能简单把 `depcache:flush()` 当作通用恢复手段，因为它可能同时清掉与本次故障无关的节点本地状态。

V2.2 采用以下规则：

1. invalidate 类 Key 在统一入口登记必要的命名空间索引。
2. 同步协议维护每个来源的事件序号或失效摘要。
3. 发现事件缺口、运行实例变化或网络重连时，将受影响命名空间标记为 dirty。
4. dirty 状态下不能继续直接使用该命名空间的旧缓存命中。
5. 分批清除该命名空间缓存，或通过已验证的版本机制使旧缓存不可见。
6. 清理完成并校对必要状态后恢复 ready。

对于 depcache 内部无法枚举的匿名 memo Key，W0 必须明确其分类和恢复策略，不得擅自依赖 depcache 私有 ETS 表结构。

### 7.4 安全关键缓存

以下状态不允许单纯依赖上述 Generation 机制保证安全：

- Token 吊销；
- 管理员权限撤销；
- 设备删除；
- E2EE 授权模式切换；
- 其他跨节点权限收回操作。

因为广播可能丢失，且失效栅栏不能阻止失效事件尚未送达时的旧缓存命中。

这些场景必须继续保留权威检查、业务版本约束或 fail-closed 路径。

### 7.5 PostgreSQL 事务约束

所有数据库派生缓存的修改动作原则上遵守：

```text
BEGIN
  修改数据库
COMMIT
  invalidate(Key)
```

如果事务回滚：

```text
ROLLBACK
  不广播未提交的数据状态
```

但提交后广播仍可能失败。

普通缓存可以使用有界 TTL、失效重试及定期校对。

严格安全业务不得把广播成功视为操作安全完成的唯一条件。

---

## 8. 业务正确性前置门禁

本阶段不扩展缓存功能，而是处理会影响缓存重构正确性的现存问题。

### G-01：账号 ID 分配

**范围：`account_ds.erl`**

当前代码通过：

```erlang
imboy_cache:get_wait(Key)
```

取得 ID 列表，再：

```erlang
imboy_cache:set(Key, Rest, TTL)
```

更新剩余列表。

缓存命中后，这个读取和更新过程不能保证整体原子性。

**V2.2 决策：**

优先使用现有 PostgreSQL Sequence 实现原子 ID 分配。

若保留批量预分配，必须通过专用分配进程或等价机制保证每个 ID 只领取一次。

禁止使用分布式缓存 SET 实现唯一 ID 分配。

验收：

- 单节点多进程并发无重复 ID。
- 多节点并发无重复 ID。
- 进程异常不会重复分配已经领取的 ID。
- PostgreSQL 不可用时不能虚假返回成功。
- 保持原有 ID 类型及业务接口兼容。

### G-02：QR 登录一次性消费

**范围：`qr_login_handler.erl` 及相关 SSE、DS、Logic。**

必须完整检查：

```text
create
  ↓
scan
  ↓
confirm
  ↓
status / SSE
  ↓
login token
  ↓
consume
```

重点排查：

- status 同时请求；
- status 与 SSE 同时领取；
- 重复 confirm；
- cancel 与 confirm 并发；
- 不同节点同时处理；
- 缓存提前淘汰；
- 网络分区及节点切换。

目前公开代码中，status 路径存在读取 Token 后删除缓存的操作，而且 confirm 还会向 SSE 订阅者发送 Login Token。

**这两个交付通道必须整体审查。**

如果安全修复要求改变客户端可观察协议，必须先形成独立变更合同，不能由缓存重构任务擅自修改 Flutter 客户端。

**原则：**

- 登录凭据全局一次性消费由权威原子机制保证。
- 缓存同步只加速临时状态传播。
- 不使用 replicate/confirmed 模拟原子领取。
- 如果需要新的数据库 Schema，先进入授权门禁，禁止自动增加迁移。

未通过 G-02，不允许将 QR 安全关键状态切换到新缓存同步模式。

### G-03：数据库事务与缓存失效

重点检查：

```text
auth_session_ds
adm_session_ds
group_member_ds
group_ds
friend_ds
user_denylist_logic
user_device_logic
```

必须解决：

1. 事务提交前广播失效；
2. 事务回滚后缓存仍被修改；
3. 旧查询在 invalidate 之后重新填充缓存；
4. 多节点同时变更导致旧值覆盖；
5. 缓存异常导致业务错误返回成功。

对于数据库派生缓存，建议优先采用：

**事务成功提交 → 失效 Key → 业务返回。**

对于严格安全状态，则需要独立的权威校验策略。

### G-04：敏感数据保护

replicate 只允许复制明确列入白名单的数据。

默认禁止复制：

- 私钥、签名密钥、JWT 密钥；
- 长期有效的认证凭据；
- 可直接用于越权登录的 Token；
- 支付密钥；
- 其他业务明确禁止复制的敏感字段。

如果确实需要传播敏感临时状态，必须先证明现有 Erlang 分布式通信的信任边界、安全配置及业务必要性。

日志和指标不得输出完整 Value、Token 或密钥。

### G-05：集群一致配置

集群启用 V2 前必须检查：

- Erlang/OTP 版本；
- syn scope；
- 节点 ID；
- 缓存协议版本；
- Key 策略版本；
- TTL 与对象大小限制；
- 分布式同步开关；
- 节点连接与健康状态。

多节点部署中，不允许部分节点启用 V2、部分节点仍仅支持 legacy，却继续假装同步正常。

---

## 9. 节点恢复与反熵同步

### 9.1 恢复状态机

```text
          STARTING
              |
              v
       DISCOVER_PEERS
              |
              v
       CHECK_PROTOCOL
              |
              v
       RECOVER_VERSION
              |
              v
        SYNC_SNAPSHOT
              |
              v
         APPLY_DELTA
              |
              v
        VERIFY_STATE
              |
              v
            READY
```

任何阶段失败：

```text
RETRY_WITH_LIMIT
        |
        v
    DEGRADED
        |
        v
 RECONCILE / BLOCKED
```

DEGRADED 状态下：

- 本地普通缓存可按业务合同继续工作。
- 未恢复的 replicate Key 不得返回未经校验的旧值。
- 新 replicate 写入不得虚假返回成功。
- 安全关键业务继续执行其独立权威校验。

### 9.2 快照机制

方案 A 不读取 depcache 私有 ETS 表结构。

通过 V2.2 的同步元数据索引枚举需要恢复的 replicate Key。

每个 Key：

1. 检查当前版本。
2. 检查 depcache 是否仍有有效值。
3. 检查是否过期。
4. 若有效，加入快照。
5. 按批次发送。
6. 接收节点按版本合并。

默认批次建议不超过 256 条，并受到总字节大小限制。

### 9.3 同步窗口中的新写入

节点恢复过程中，其他节点可能继续 SET/DELETE。

不能只复制一次静态快照就认为同步完成。

V2.2 需要：

- 快照起始版本水位；
- 快照期间的增量事件；
- 有界增量缓冲；
- 版本校验；
- 恢复完成水位；
- 溢出时重新校对。

如果无法证明快照及增量之间没有遗漏，节点不能进入 READY。

### 9.4 定期状态校对

`syn:publish` 不是可靠持久消息队列。

除了节点重连触发恢复，还需要轻量定期校对。

建议每 60 秒进行一次候选级别的状态摘要交换，实际周期由 W0 性能结果冻结。

重点：

- 每个来源的最新事件序号；
- 来源 BootId；
- replicate 版本摘要；
- invalidate 命名空间失效水位；
- 节点恢复状态。

发现事件缺口后进行有界补偿。

不要默认周期性全量复制所有 Key。

### 9.5 跨区域边界

IMBoy 存在多区域部署规划，因此 V2.2 必须区分：

**同机房 Erlang 集群：**

作为 V1 的正式性能和恢复验收范围。

**跨区域集群：**

独立评估网络延迟、断线频率、数据敏感性及同步成本。

不能将同机房三节点 P99 指标直接用于跨区域。

不要求 V2.2 实现全球强一致缓存。

---

# 10. 分波次实施计划

本版继续采用 **W0～W5 六个阶段**。

每个阶段只实现自己范围内的任务，前一个阶段 Gate 通过才进入下一个阶段。

## W0 — Baseline & Architecture Gate

**目标：先取证，再决定，不预先扩张架构。**

### Tasks

| Task | 工作内容 |
|---|---|
| W0-01 | 记录本地 HEAD、工作区变更和依赖版本 |
| W0-02 | 全仓缓存调用点扫描 |
| W0-03 | 完整 Key Registry |
| W0-04 | 敏感数据和权限类缓存分类 |
| W0-05 | confirmed 真实业务需求审查 |
| W0-06 | 当前 depcache 性能基线 |
| W0-07 | 当前 syn 同步性能基线 |
| W0-08 | 三节点故障与恢复现状调查 |
| W0-09 | 绝对性能 SLO 冻结 |
| W0-10 | 方案 A/B 决策冻结 |

### 交付物

```text
evidence/cache-v2.2/w0/
    baseline.json
    key-registry.md
    cache-callsite-census.md
    confirmed-requirements.md
    performance-baseline.json
    architecture-decision.md
```

### Gate W0

必须同时满足：

- 生产缓存调用点全部完成分类或记录明确例外。
- 所有 replicate 候选 Key 已明确权威来源。
- 所有安全关键 Key 已有失败处理合同。
- depcache GET/SET 瓶颈有实测证据。
- 绝对 SLO 与负载模型已经冻结。
- 方案 A/B 选择有数据支持。

默认：

`ARCHITECTURE = A`

不满足方案 B 触发条件，不允许实施方案 B。

---

## W1 — Correctness & Security

**目标：优先修复当前会影响分布式缓存正确性的业务问题。**

### Tasks

| Task | 工作内容 |
|---|---|
| W1-01 | 修复 ID 原子分配问题 |
| W1-02 | 修复好友字段缓存 Key 冲突 |
| W1-03 | 修复事务内缓存变更问题 |
| W1-04 | 增加缓存回填竞态测试 |
| W1-05 | 修复重复失效广播及不必要的 local 广播 |
| W1-06 | 审查 QR status/SSE 一次性消费 |
| W1-07 | 验证设备、认证和权限缓存的安全行为 |

### 测试要求

- ID 并发唯一性；
- 事务 COMMIT/ROLLBACK；
- 缓存填充与失效交错；
- 好友字段隔离；
- QR 并发状态机；
- 设备吊销；
- depcache 原有依赖失效。

### Gate W1

**关键正确性问题必须有通过的测试证据。**

不能通过修改测试期望、隐藏异常、缩小并发规模等方式获得 PASS。

如果 QR 修复必须改变客户端协议或 Schema，应进入 BLOCKED 并提交独立方案，不得擅自扩大范围。

---

## W2 — Cache API & Policy

**目标：建立清晰的统一缓存入口。**

### Tasks

| Task | 工作内容 |
|---|---|
| W2-01 | 保持旧 API 兼容 |
| W2-02 | 实现 `put/4` |
| W2-03 | 实现 `invalidate/1` |
| W2-04 | 冻结 Key Registry |
| W2-05 | 实现统一策略校验 |
| W2-06 | 实现 TTL/Value 大小验证 |
| W2-07 | 实现最小同步元数据表 |
| W2-08 | 增加缓存状态与错误指标 |

### 模块约束

优先修改：

```text
src/lib/imboy_cache.erl
src/lib/imboy_cache_sync.erl
```

如果确有必要：

```text
src/lib/imboy_cache_policy.erl
```

不允许为了划分职责而继续拆出大量微型 Erlang 模块。

### Gate W2

- 旧 API 无回归。
- 本地缓存正常。
- local Key 不广播。
- 未授权 Key 不能 replicate。
- `memo` 依赖失效正确。
- 缓存返回类型符合合同。
- 新协议默认未启用。

---

## W3 — Distributed SET / DELETE

**目标：完成真实的跨节点复制能力。**

### Tasks

| Task | 工作内容 |
|---|---|
| W3-01 | 同步协议 V2 |
| W3-02 | 完整 Key/Value SET 广播 |
| W3-03 | Lamport Revision |
| W3-04 | NodeId + BootId |
| W3-05 | 版本校验和写入串行化 |
| W3-06 | DELETE 墓碑 |
| W3-07 | TTL 剩余生命周期传播 |
| W3-08 | 重复消息幂等 |
| W3-09 | 乱序和并发冲突收敛 |
| W3-10 | 自己发出的广播不重复修改本地状态 |

### 测试场景

```text
A SET K=1
B GET K=1
C GET K=1

A SET K=2
B GET K=2
C GET K=2

A DELETE K
B GET K=undefined
C GET K=undefined
```

以上 GET 判断必须等待协议约定的收敛条件，而不是假设普通异步 SET 立即全节点可见。

另外必须执行：

```text
旧 SET → 新 SET
新 SET → 旧 SET

SET → DELETE
DELETE → 延迟旧 SET

重复 SET
重复 DELETE

A/B 同时 SET
A/B 同时 DELETE/SET
```

### Gate W3

- 三节点真实同步通过。
- 版本比较符合合同。
- 无旧值覆盖新值。
- 无 DELETE 后旧值复活。
- TTL 不被重置。
- 所有复制失败可诊断。

禁止只使用 meck 证明跨节点功能完成。

---

## W4 — Recovery, Capacity & Performance

**目标：完成故障恢复、资源控制和性能优化。**

### Tasks

| Task | 工作内容 |
|---|---|
| W4-01 | 节点启动恢复屏障 |
| W4-02 | Version 上界恢复 |
| W4-03 | Replicate 快照同步 |
| W4-04 | 快照期间 Delta 处理 |
| W4-05 | Invalidate 事件缺口检测 |
| W4-06 | 受影响命名空间的有界清理 |
| W4-07 | 墓碑过期与冷恢复 |
| W4-08 | 缓存提前淘汰回归测试 |
| W4-09 | 消息队列过载保护 |
| W4-10 | 最终缓存性能压测 |
| W4-11 | 同机房三节点故障注入 |

### 性能原则

同步进程允许短时间积压，但不允许长期失控。

必须有：

- 同步发布前的容量检查；
- 单 Value 大小限制；
- 单批次数量和字节数限制；
- 同步队列高水位监控；
- 超载时拒绝新 replicate 写入或降级；
- 持续积压告警。

需要注意：syn 的异步进程消息发送不自带端到端背压，因此不能仅通过设置应用队列长度，就声称 Erlang 接收进程邮箱具有严格上限。

W4 必须实测消息邮箱、堆内存和调度器在过载场景下的行为。

### Gate W4

- 节点重启恢复通过。
- 网络分区恢复通过。
- 失效事件丢失补偿通过。
- 墓碑生命周期验证通过。
- 未恢复节点不能提供未经校验的复制状态。
- 无无限重试或无界资源积压。
- 性能达到 W0 冻结目标。

### 方案 B 触发门禁

如果 W4 证明方案 A 存在无法接受的写入瓶颈：

进入：

`B_REQUIRED_FOR_REVIEW`

而不是直接实施方案 B。

必须另行产出针对具体热点 Key 的最小 ETS 方案，说明预计收益、额外状态、原子写入机制和新增维护成本。

---

## W5 — Integration & Final Acceptance

**目标：完成真实业务迁移、最终回归和交付。**

### Tasks

| Task | 工作内容 |
|---|---|
| W5-01 | 按 Key Registry 迁移现有业务调用 |
| W5-02 | 保留必要的完整 SET 复制场景 |
| W5-03 | 验证好友、群、配置等 invalidate 场景 |
| W5-04 | 验证 QR、认证、设备及权限合同 |
| W5-05 | 混合版本滚动升级演练 |
| W5-06 | 回滚演练 |
| W5-07 | 性能结果与基线比较 |
| W5-08 | 清理冗余代码和配置 |
| W5-09 | 文档与真实代码对齐 |
| W5-10 | 最终一次全量 EUnit |

### 真实业务验证

至少验证一个经过 W0 安全分类的真实业务 Key 使用完整 SET 复制。

如果现有业务没有可安全迁移的 Key，必须在报告中说明，不得为了满足数量要求把敏感状态强行改成 replicate。

此外，必须保留一组三节点的完整 SET 集成测试，保证该能力真实可用。

### Gate W5

全部必需 AC 通过，并具备可验证回滚方案：

`LOCAL_CANDIDATE_PASS`

否则：

`BLOCKED`

不得将只通过单节点单元测试的实现标记为生产候选。

---

# 11. 验收标准 AC-01～AC-30

## 11.1 基础架构与兼容

| AC | 验收内容 |
|---|---|
| AC-01 | 本地代码、依赖、配置基线已冻结 |
| AC-02 | 全部缓存 Key 完成分类或记录例外 |
| AC-03 | 方案 A/B 有真实性能证据 |
| AC-04 | 现有 imboy_cache API 兼容 |
| AC-05 | depcache memo、TTL、依赖失效零回归 |
| AC-06 | 不引入 Redis/Mnesia 或新缓存中间件 |

## 11.2 缓存策略

| AC | 验收内容 |
|---|---|
| AC-07 | local 不跨节点广播 |
| AC-08 | invalidate 正确使目标及依赖缓存失效 |
| AC-09 | 普通数据库缓存填充不引发无意义广播 |
| AC-10 | replicate 完整同步 Key/Value |
| AC-11 | 未分类或禁止复制的 Key 无法使用 replicate |
| AC-12 | depcache 提前淘汰不导致错误业务成功 |

## 11.3 分布式一致性

| AC | 验收内容 |
|---|---|
| AC-13 | 并发 SET 按版本规则收敛 |
| AC-14 | 重复和乱序事件幂等 |
| AC-15 | DELETE 墓碑阻止旧 SET 复活 |
| AC-16 | TTL 不因同步延长 |
| AC-17 | 节点重启后的版本恢复正确 |
| AC-18 | 超过恢复窗口的节点正确冷恢复 |
| AC-19 | 快照及增量同步无未经处理的遗漏 |
| AC-20 | 丢失 invalidate 时能检测并执行安全补偿 |

## 11.4 业务正确性

| AC | 验收内容 |
|---|---|
| AC-21 | 账号 ID 并发唯一 |
| AC-22 | QR 一次性领取及 SSE/status 并发安全 |
| AC-23 | 数据库事务边界正确 |
| AC-24 | 缓存并发回填竞态得到控制 |
| AC-25 | 权限、吊销等安全状态不依赖不可靠缓存放行 |

## 11.5 性能、运维及交付

| AC | 验收内容 |
|---|---|
| AC-26 | 单节点性能达到冻结 SLO |
| AC-27 | 三节点同步性能及资源预算达标 |
| AC-28 | 监控、过载保护和故障诊断有效 |
| AC-29 | 滚动升级及回滚通过 |
| AC-30 | 定向测试、最终全量 EUnit、证据和文档完整 |

### 验收纪律

- AC-13～25 属于正确性硬门禁。
- 正确性失败不能被更好的性能分数抵消。
- AC-22 未解决时，不得宣称 QR 迁移生产就绪。
- 所有 AC 必须对应实际测试、日志或代码证据。
- 禁止将计划、注释、Mock 结果当作真实集群运行证明。

---

# 12. 性能目标与负载模型

## 12.1 基准环境

候选环境：

```text
CPU:       8 Core
RAM:       16 GiB
Runtime:   Erlang/OTP 项目实际版本
Cluster:   1 Node / 3 Nodes
Cache:     depcache
Sync:      syn
```

测试必须记录真实 CPU、内存、OTP、调度器数量以及节点部署方式。

IMBoy 有百万连接规模目标，因此缓存预算不能只按照空载服务器测量。

需要分别测试：

- 独立缓存能力；
- 与实际 IMBoy 业务负载混合运行时的表现。

### 12.2 Key 数量

| 测试等级 | Key 数量 | 说明 |
|---|---:|---|
| S | 10,000 | 基础正确性和性能 |
| M | 50,000 | 主要压力测试 |
| L | 100,000 | 较大规模测试 |
| Stress | 500,000 | 专用环境可选测试 |

Value 大小：

```text
128 B
1 KiB
8 KiB
32 KiB（边界测试）
```

不能默认把 50 万个 8 KiB 对象放进内存。

50 万 × 8 KiB 的原始 Value 数据已经接近 4 GiB，还不包含 ETS 元数据、Erlang term 和内存分配开销。

当前示例配置的 depcache 内存预算是 100 MB，因此测试必须依据实际内存限制选择 Key 数量和 Value 组合。

### 12.3 并发负载

| 场景 | GET | SET |
|---|---:|---:|
| Read Heavy | 95% | 5% |
| Balanced | 70% | 30% |
| Write Heavy | 30% | 70% |

另外测试：

- 不同 Key 高并发；
- 相同 Key 高并发；
- 热点 Key；
- 大批量失效；
- 重复广播；
- 节点恢复期间继续写入。

### 12.4 绝对性能 SLO

V2.2 初始候选：

| 指标 | 候选目标 |
|---|---:|
| 本地 GET P99 | ≤ 2 ms |
| 本地 SET P99 | ≤ 5 ms |
| 同机房三节点 SET 收敛 P99 | ≤ 1 s |
| 相对 GET P99 退化 | ≤ 10% |
| 同等负载下持续吞吐 | 不低于冻结基线的 95% |

这些不是现有系统实测结果。

**W0 必须在明确的并发数、请求速率、Key 数量、Value 大小及硬件条件下冻结最终 SLO。**

吞吐量不能只测系统能够完成多少请求，还必须同时检查尾延迟、错误率、GC 压力和内存增长。

### 12.5 资源限制

V2.2 必须监控：

```text
cache_get_total
cache_hit_total
cache_miss_total
cache_set_total
cache_eviction_total

cache_sync_publish_total
cache_sync_receive_total
cache_sync_reject_total
cache_sync_conflict_total
cache_sync_reconcile_total
cache_sync_reconcile_failed_total

cache_sync_queue_depth
cache_sync_mailbox_length
cache_sync_payload_bytes
cache_sync_latency_ms
cache_sync_recovery_ms
```

指标名称可以对齐现有 `elib_metric` 命名规范。

禁止将完整 Key、UserId、Token 等高基数或敏感值直接写入指标标签。

---

# 13. 配置、滚动升级与回滚

## 13.1 配置

保留现有：

```erlang
{dsync_enabled, true}.
```

增加最少配置：

```erlang
{cache_sync_protocol, legacy}.

{cache_replication, #{
    max_ttl_sec => 600,
    max_value_bytes => 32768,
    batch_size => 256,
    recovery_window_sec => 300
}}.
```

其中 `cache_sync_protocol` 初始必须为 `legacy`。

同步消息大小、队列预算、墓碑生命周期等参数应当从统一配置或经过验证的默认值派生，不允许不同模块各自维护一套互相矛盾的配置。

### 13.2 两阶段升级

**阶段一：部署兼容代码**

```text
Node A: legacy + V2 decoder
Node B: legacy + V2 decoder
Node C: legacy + V2 decoder
```

此时仍使用 legacy 协议。

必须确认所有节点具备 V2 能力。

**阶段二：统一切换**

```text
Node A: V2
Node B: V2
Node C: V2
```

切换过程中：

- 不允许静默丢弃未知协议。
- 不允许某个节点单独启用不兼容的复制策略。
- 协议切换失败必须能诊断。
- 未完成同步恢复的节点不能被判定为 READY。

### 13.3 回滚

回滚顺序：

1. 停止接受新的 V2 replicate 写入。
2. 处理当前正在进行的同步操作。
3. 确认需要清理或失效的 V2 缓存状态。
4. 恢复 legacy 策略。
5. 验证原有业务行为。
6. 记录回滚证据。

不允许简单修改配置后，继续让旧节点使用无法理解的 V2 状态。

回滚不能改变 PostgreSQL 权威数据。

---

# 14. 无人值守执行合同

这是 V2.2 的正式执行机制。

## 14.1 执行状态机

```text
PENDING
   |
   v
DISCOVER
   |
   v
IMPLEMENT
   |
   v
TARGETED_TEST
   |
   v
VERIFY
   |
   +-------- PASS --------+
   |                      |
 FAILED                    |
   |                      |
   v                      |
DIAGNOSE                   |
   |                      |
   v                      |
REPAIR                     |
   |                      |
   +------> TARGETED_TEST |
                          |
                          v
                     NEXT_WAVE
```

### 14.2 自动修复限制

| 场景 | 处理 |
|---|---|
| 编译失败 | 分析根因，最小修复 |
| 定向测试失败 | 自动修复最多两轮 |
| 相同失败重复出现 | 停止盲目重试 |
| 测试环境暂态故障 | 有界重试并记录 |
| 网络节点启动失败 | 检查配置、端口、依赖后重试 |
| 出现架构合同冲突 | BLOCKED |
| 需要生产 Schema 修改 | BLOCKED |
| 需要改变客户端协议 | BLOCKED |
| 测试被修改以降低要求 | FAIL |
| 工作区出现无法解释的外部修改 | BLOCKED |

不得无限循环。

### 14.3 会话恢复

执行状态保存：

```text
.Codex/runs/cache-v2.2/state.json
```

建议结构：

```json
{
  "plan_version": "2.2",
  "base_sha": "<actual-local-head>",
  "architecture": "A",
  "current_wave": "W0",
  "current_task": "W0-01",
  "status": "PENDING",
  "retry_count": 0,
  "completed_ac": [],
  "blocked_ac": [],
  "evidence_dir": "evidence/cache-v2.2/",
  "last_error": null
}
```

要求：

- 原子写入状态文件。
- 执行前检查本地 SHA。
- 恢复时核对实际修改文件和测试证据。
- 不能仅因为 JSON 写着 PASS 就跳过验证。
- 状态文件及临时日志不得无意进入 Git 提交。
- 不得覆盖其他 Agent 的工作。

### 14.4 并行执行规则

允许并行：

- 只读代码扫描；
- 独立测试分析；
- 文档证据整理；
- 不修改共享文件的性能取证。

必须串行：

- `imboy_cache.erl` 修改；
- `imboy_cache_sync.erl` 修改；
- 同步协议定义；
- Key Registry 修改；
- W0 架构选型决策；
- 最终 Git 状态核对。

避免两个 Agent 同时修改相同模块，造成无意义的冲突和返工。

### 14.5 测试执行纪律

仓库当前支持通过以下形式定向运行 EUnit：

```bash
make eunit-local t=imboy_cache_tests
make eunit-local t=imboy_cache_sync_tests
```

执行前须根据本地 Makefile 复核实际命令。

每个波次：

```text
修改
  ↓
编译
  ↓
定向 EUnit
  ↓
必要的集成测试
  ↓
证据记录
  ↓
进入下一波次
```

只有 W5 最终验收运行全量 EUnit。

三节点测试应采用独立 Erlang 节点，真实连接 syn，而不是单机 meck 模拟。

不得将既有失败、环境失败和本次引入的失败混为一谈。

---

# 15. 最终文件与交付物

## 15.1 生产代码

方案 A 预计主要涉及：

```text
src/lib/imboy_cache.erl
src/lib/imboy_cache_sync.erl

src/imboy_sup.erl
include/cache.hrl
config/sys.config.example
```

根据真实取证结果，可能调整：

```text
src/ds/account_ds.erl
src/ds/friend_ds.erl
src/ds/auth_session_ds.erl
src/ds/group_ds.erl
src/ds/group_member_ds.erl

src/api/qr_login_handler.erl
```

这些是候选修改范围。

不允许根据清单机械修改所有文件。

确实需要新模块时，先证明不能在现有模块中简单实现。

## 15.2 测试代码

优先扩展：

```text
test/lib/imboy_cache_tests.erl
test/lib/imboy_cache_sync_tests.erl
```

必要时增加一个真正的三节点 Common Test 套件，例如：

```text
test/cache/imboy_cache_cluster_SUITE.erl
```

测试套件必须覆盖网络分区、重连、旧事件、并发 SET/DELETE 以及 TTL。

## 15.3 长期保留文档

```text
docs/architecture/imboy-distributed-cache-v2.md

docs/guides/operations/cache-cluster.md

docs/plans/2026-10-10-distributed-cache-v2.2.md
```

过程证据：

```text
evidence/cache-v2.2/
    w0/
    w1/
    w2/
    w3/
    w4/
    w5/
    final-report.md
```

避免生成大量内容重复的文档。

---

# 16. 最终验收与执行报告

最终要求输出：

| 项目 | 最终状态 |
|---|---|
| Architecture | A_PASS / B_PASS / BLOCKED |
| API Compatibility | PASS / FAIL |
| depcache Regression | PASS / FAIL |
| Local Cache | PASS / FAIL |
| Invalidation | PASS / FAIL |
| SET Replication | PASS / FAIL |
| TTL & Tombstone | PASS / FAIL |
| Node Recovery | PASS / FAIL |
| Security | PASS / FAIL |
| Performance | PASS / FAIL |
| Cluster Tests | PASS / FAIL |
| Full EUnit | PASS / FAIL |
| Rollback | VERIFIED / NOT_VERIFIED |
| Overall | LOCAL_CANDIDATE_PASS / BLOCKED |

`LOCAL_CANDIDATE_PASS` 必须有实际代码和测试证据支撑。

该状态只代表本地候选可用，不代表已经部署生产环境。

---

# 17. GLM-5.3 实施提示词

计划由 GPT-5.6 结合最新本地代码确认后，可使用以下提示词驱动执行：

> 你是 IMBoy Erlang/OTP 高级架构实施工程师。
>
> 请严格执行 `docs/plans/2026-10-10-distributed-cache-v2.2.md`。
>
> **目标**：在不依赖 Redis、Mnesia 或新缓存中间件的前提下，优化 IMBoy 缓存系统，优先保留 depcache，复用 syn，完成 local、invalidate、replicate 三种缓存行为，实现跨节点完整 SET 同步、版本控制、TTL、墓碑及故障恢复。
>
> **执行要求**：
>
> 1. 首先读取最新本地代码和任务计划，执行 W0，完成代码取证、Key 分类与真实性能基线。
> 2. 优先执行方案 A。除非满足计划规定的性能门禁，否则禁止自行引入第二套 ETS 缓存引擎。
> 3. 按 W0→W5 依赖顺序实施，每个波次必须通过 Gate，才能继续执行。
> 4. 优先复用 depcache 已有能力，不重新开发 TTL、memo、防击穿、依赖失效等功能。
> 5. 分布式复制必须处理并发版本、重复事件、乱序、DELETE、TTL、节点重启和网络恢复。
> 6. 严格遵守安全前置门禁，不得把普通缓存同步当作 QR 一次性消费或授权强一致机制。
> 7. 每轮修改后只运行必要的定向测试，全部完成后统一运行一次全量 EUnit。
> 8. 持续维护 state.json，支持会话中断恢复。
> 9. 单个问题最多自动修复两轮，无法解决时记录证据并进入 BLOCKED，不允许无限重试。
> 10. 只修改相关代码、测试、配置及必要文档。保护现有工作区，不执行破坏性 Git 操作；不提交、不推送、不部署、不修改生产数据。
>
> 严格遵循 `FACT → GAP → DECISION → CONTRACT → TASK → TEST → EVIDENCE`。
>
> 最终交付完整验收矩阵、性能对比、三节点测试证据、Git diff 摘要及回滚说明。禁止未经验证宣称任务完成。

---

## 18. V2.2 最终架构结论

**推荐最终形态仍然是两个主要业务模块：**

```text
imboy_cache.erl
imboy_cache_sync.erl
```

加上项目现有的：

```text
depcache
syn
OTP Supervisor
```

仅用最少量的元数据解决同步正确性问题。

V2.2 不追求提供 Redis 的全部功能，也不尝试实现 Mnesia、Raft 或强一致分布式数据库。

它需要解决的是 IMBoy 的真实问题：

**本地 GET 快、SET 足够快、部分 Key 能全节点同步、数据能正确失效、节点故障后可恢复，而且代码长期容易维护。**

### 计划质量目标

| 版本 | 质量评估 |
|---|---|
| V2.0 | 7.8/10 |
| V2.1 | 约 9.0/10 |
| **V2.2** | **设计目标 9.5+/10，待 W0 与测试验证** |

**后续实施必须绑定 V2.2.1 的文件 SHA256 和本地候选，旧提示词中的 V2.2 不是执行授权。**

下一步直接进入 **W0 本地源码取证、Key Registry 冻结和 depcache 性能基线测试**。W0 的结果将决定最终只增强 `depcache + syn`，还是有充分理由增加专用 ETS。

# 19. V2.2.1 九项补全与执行边界（覆盖冲突旧条款）

## 19.1 可执行 W0 框架与证据等级

执行入口（仅创建独立 BEAM/peer，合成缓存，不启动 IMBoy/PG/Garage）：

```bash
cd /Users/leeyi/project/imboy.pub/imboy
python3 scripts/run_cache_w0.py --operations 10000
```

退出 2 表示 W0_BLOCKED，不是压测必然失败；检查 baseline 的 compile_exit/run_exit、
performance 的三节点 status/errors，再判断具体阻塞。不得将任一 exit 忽略。
运行目录必须全新；保存 HEAD、dirty 状态、源码摘要、依赖 .app、负载、环境、原始日志及摘要。
框架不加载业务 supervisor、不读取业务配置、不使用现役 cookie，不连接现役节点。
peer 使用本次随机 cookie，通过 loopback 建立三个真实 OTP 节点；不能启动时记录原因而不是填零。
1/8/32 并发、1024 Key、256-byte 值、TTL=3600；GET、SET、90/10 混合、memo 热命中；
syn 逐条等待所有测试接收者 depcache:set 完成的 ACK，测量的是应用完成延迟而非 publish 返回延迟。
冷 memo 的 32 请求/一次 loader 自检和失效后旧回填复现分开记录，不伪称已有 V2 栅栏通过。
记录吞吐、P50/P95/P99 微秒、错误数/率、BEAM 内存、ETS words、采样队列及执行时间。
depcache:size 是 ETS words，不是 bytes；乘 wordsize 后仍不包括完整 refc binary 占用。
队列为各测试节点接收进程+depcache 队列总和的采样值，不是精确峰值或 syn 全内部队列。
BEAM 采样峰值只属于主节点，另记录各节点结束时内存；不能冒充集群内存峰值。
逐请求计时分辨率为微秒，P50=0 表示小于计时分辨率，不代表零成本；
吞吐含 worker 启动/样本收集，BEAM 总内存含 load generator/采样器，不是缓存纯内存。
冷/热、持续饱和、热点、突发、TTL、依赖失效、容量压力和恢复负载必须分别标识；
未运行项目以 NOT_EXECUTED 留在 W0 coverage，不得由热命中数据推断。
至少两次独立进程运行对照，报告每次值而非只取最佳值；目标硬件/生产负载未知时
只能建立本机 baseline，不能最终冻结生产 SLO，也不能强行 W0_PASS。

## 19.2 QR 原子迁移阻塞隔离

当前真实代码：qr_login_handler 的会话、QR 映射通过 set/get/delete；
status 的 get→delete 不是原子消费；confirmed 状态还通过 syn/SSE 推送 token。
G-QR 为独立安全 gate，要求同一 PG authority、原子 claim/consume、token 可见性、
过期、重试幂等、status 与 SSE 的同一授权/消费语义、双读竞争 oracle。
未完成前，QR 两类 Key 保留现有路径且禁止进入 V2 replicate；不得声称已安全或更改当前语义。
QR 阻塞不妨碍独立普通缓存测量与设计；全局 ENABLE_V2/安全完成 gate 仍被 QR 阻塞。
W1 的普通修复和 QR 子任务可分别取证，但不得靠 confirmed ACK 替代权威原子消费。
同理 account_list 热命中读改写、IP 限流读改写、bind_mail/CSRF 消费均非跨节点原子计数。

## 19.3 memo/get_wait 的完整并发栅栏

depcache:memo/5 内部 get_wait→loader→set，绕过 imboy_cache:set；
仅对 wrapper set 做 generation 检查不能保护 memo，也不能唤醒等待者或消除旧结果返回。
采用版本化物理 Key `{cache_v2,LogicalKey,Generation}`，保留 depcache 原生 memo/get_wait 等待合并。
generation 与依赖 generation 由单一同步 owner 串行更新；loader 前读取快照，
返回前再验证；旧 generation 结果不得返回调用者或成为当前可读值。
旧 loader 允许写入旧物理 Key（TTL 有界）；禁止通过并发 flush 当前 Key 误杀新 loader。
依赖使用版本化标签；逻辑失效先推进 generation，再使新请求选择新物理 Key。
generation 读取+验证不等于原子 claim，安全最终决策仍要权威校验。
最多一次重试；再次碰到失效则明确 stale_generation/error，不得返回陈旧成功或无限递归。
原始 get_wait/1 无 token 且返回 undefined 的 API 不能携带 ownership；W0 仅发现 account_ds
业务调用，该 ID 业务先原子化，不把旧 get_wait 暴露给 V2 回填生产路径。
若后续发现更多手工 get_wait，逐项迁移带 generation 的内部 contract 或保留明确 legacy，
未迁移 Key 不能切换。不得重写 depcache 的 waiter、异常传播、premature_exit 和超时能力。
测试必须控制 loader 开始/失效/新 loader/旧结束顺序，覆盖 direct Key、依赖标签、异常、
超时、等待者、进程死亡、连续失效和 get_wait 手工填充；断言缓存与调用者都不收到旧值。

## 19.4 完整 Key Registry 与零静默缺漏

本轮机器清单：calls.json（全部非 vendored .erl）、source-evidence.json（原始源码+摘要）、
key-registry.json（静态命名空间、结构、owner、authority、TTL、敏感性、恢复与失败策略）。
必须包含动态 Csrf、tag_subtitle_2/3、config5 的派生 Key、adm_user_logic:find 的外部传入 Key、
friend_rel_dep、仅删除无填充标签、匿名 memo/MFA、direct depcache、整库 flush 和注释伪调用。
词法扫描只提供清单，不能冒充全数据流覆盖；动态 apply、跨行模块/函数表达式须人工补充
并作为明确例外记录。TTL 宏要解析具体值；从业务真实调用反推，不按计划旧候选猜测。
每个生产调用要关联 namespace 或显式 legacy/exception ID；依赖标签不是 replicate 值。
mutation_hook 必须追到权威写入和 COMMIT 后路径；未闭合钩子标记 BLOCKED，不预先冻结。
自有 ETS/persistent_term/ACK/进程字典句柄以 other-stores.json 逐模块显式排除迁移；
它们不是通用 depcache Key，不得漏扫后误声称“全部在 depcache”。
Registry 的边界策略可冻结为禁止复制；值大小、数量上界与恢复资源预算仍未验证时
必须保留未冻结状态，不把完整字段形状当成完整实施验收。

## 19.5 安全复制白名单

默认 deny；本轮生产复制白名单为空。cluster_node_info 仅保留候选资格，
先逐字段投影再定 whitelist，禁止复制整个 NodeInfo map。
未来白名单必须精确匹配 namespace、arity、Key 类型/长度、值 schema、字段集合、
TTL 上限、字节上限、数量上限与 authority；发送/接收两端独立校验，未知字段拒绝。
JWT/OAuth、QR/session token、CSRF、mail、IP、设备标识、权限资料和潜在配置 secret
不得全值复制；invalidate 可发送最小身份标签，日志/指标不能原样输出 Key/Value/凭据。
同步元数据只保留版本和允许的索引；不能以快照/重放/调试旁路绕过白名单。
安全缓存 miss/分区/过期应查权威或拒绝；不能静默 fail-open。

## 19.6 有界墓碑与恢复模型

禁止无限墓碑、无限增量 journal、无限恢复缓冲和无限失联成员集合。
冻结符号：Nmax 索引项数、Tmax 墓碑数/bytes、Jmax journal 数/bytes、Bmax 恢复缓冲、
Pmax 快照页、Rmax 恢复并发、Dmax 恢复期限；全部由 W0 负载与 W2 正确性结果绑定。
墓碑可保留 `W = max_partition_window + max_recovery_time + skew_margin`，
只在配置 epoch 和白名单 incarnation 内比较版本；超过窗口禁止继续增量。
达到容量/保留窗口时不能随意删 tombstone 后接纳旧 peer；推进同步 epoch，
要求该 peer 丢弃其受管复制缓存、作新 authority 快照并切换 READY。
恢复中的 peer 不参与读服务与 confirmed 集合；旧 epoch/incarnation 重放一律拒绝。
快照有 epoch、watermark、分页 token、页摘要；增量只能在 bounded journal 窗口内追赶。
窗口溢出、重试超限、无可用 authority、deadline 到期：中止恢复，进入 DEGRADED，
普通可重建缓存回权威；安全缓存拒绝；不得从已过期孤岛恢复未知全值。
删除风暴 > Tmax、分区 > W、快照与新写交错、清理后旧 SET 到达、源节点崩溃必须实测。

## 19.7 参数冻结时机与 A/B 决策

W0 开始前冻结 benchmark profile 与字段单位；完成后绑定原始数据摘要。
W0 结束时只有业务峰值/目标硬件已知，才冻结绝对吞吐、P99、错误率和 headroom。
本机微基准不反向编造业务 SLO；未知项明确 W0_BLOCKED。
Registry 先冻结禁止复制/安全行为，QR authority 独立；W1 不得调整 TTL 掩盖正确性失败。
Tmax/Jmax/Bmax 等恢复预算在 W2 故障负载 gate 前冻结，不能在报告失败后放宽。
协议/schema/config hash 在三节点混合版本测试前冻结；每次调整重新跑受影响证据。
方案 B 必须同 profile、多次实验、目标真实高频 SET Key、A 合理调优失败、
显著 SLO 缺口与小范围 ETS 对照共同证明；纯理论、synthetic 极限和单次数据均不能启用 B。
当前保留 A；若缺少最终 SLO，仅表示“B 不获准”，不能伪称 A 已满足生产负载。

## 19.8 混合版本可观测性

每节点报告 protocol_version、supported range、config/registry hash、incarnation、
sync epoch、READY/RECOVERING/DEGRADED、last accepted/applied version、peer lag、
队列、rejected protocol/schema/oversize/expired、legacy received/sent、unknown Key 计数。
syn 成员可达不等于应用已完成；publish count 不等于 ACK 或集群成员同版本。
legacy 当前未知消息静默丢弃；未来 receiver 必须计数告警，不能靠新 envelope 向旧节点发消息。
混合期只允许能力交集（legacy 失效），replicate/confirmed 禁止；旧节点的 legacy SET
对已切 V2 namespace 不能覆盖 V2 值，需明确隔离或停止该 namespace 切换。
回滚按配置 epoch 清理受管缓存并重新从权威加载，不回放旧 snapshot。
滚动升级、hash 不一致、旧节点发送 SET、未知消息、无法协商等都有验收 case。

## 19.9 单节点/三节点独立验收与文档一致性

SINGLE_NODE_PASS：API/TTL/依赖失效、冷 memo/get_wait、容量、竞争 oracle；
THREE_NODE_PASS：三真实节点、远端应用 oracle、分区重连、epoch、旧值拒绝、混合版本、
删除风暴、重启恢复、快照/journal/queue 上界。两者是独立 ledger，不能相互代替。
三台 loopback peer 可证明本地 distributed 协议路径，不代表三机部署网络/容量资格。
本轮框架只测当前依赖，故障恢复、V2 栅栏、新协议测试 NOT_EXECUTED，留给后续波次。
文档校验必须比较计划/Registry/实现配置/测试负载/报告的版本、SHA、Key 集、
TTL 单位、白名单、默认 A、confirmed=false、QR 阻塞、波次范围与 exit/result。
只有机器可比字段一致和未决项显式列出才 PASS；原计划已有候选不能冒充实际默认值。
W0 输出 status、全部性能数据、Registry、confirmed 决策、A/B 证据和 W1 readiness。
任何必需 gate 缺失均 W0_BLOCKED；W1 默认 NOT_STARTED，须另轮明确授权。

### 本轮 W0 状态

源码/实际依赖扫描与本机单节点、三真实 OTP 节点基线已执行；
冷 memo 32 请求仅运行一次 loader，flush 后旧 loader 回填竞态已复现。
决策：保留方案 A，confirmed=false，生产复制白名单为空；不启用方案 B。
W0_BLOCKED 原因：实际业务峰值/目标硬件与绝对 SLO 尚不能证据化冻结；
Registry 的权威变更钩子闭环、大小/资源预算仍有待核验项。
QR 原子迁移是后续独立安全 gate，不拿它阻塞无关本机基线。
报告与完整矩阵在 `imboy/docs/architecture/cache-v2.2.1/w0/`，
原始执行证据目录由 decision.json 的 runs 精确引用，不强行入仓。
另保存本计划仓内快照与摘要；W1 NOT_STARTED / BLOCKED_ON_W0，不自动继续。
