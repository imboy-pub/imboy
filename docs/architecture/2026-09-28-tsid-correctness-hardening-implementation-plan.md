# IMBoy TSID 正确性加固实施与验收计划

> 状态：`PLAN_READY / NOT_IMPLEMENTED / RELEASE=NO_GO`
>
> 计划编制基线：`c1f92e66d3b037b6a9c4173a7d5ed0e30b1cd6b8`
>
> 编制日期：2026-09-28
>
> 目标仓库：`/Users/leeyi/project/imboy.pub/imboy`
>
> 执行者：GLM-5.3（单一实现协调者；可断点续跑）
>
> 本文件只授权本地、可逆的实现、测试、证据和本地提交；不授权 push、PR、发布、部署、
> 生产写、真实凭据、第三方通知或任何外向操作。

## 1. 现状问题

### 1.1 已核实源码事实

以下为本计划编制时从当前源码确认的事实，不是实施后的完成声明：

| 编号 | 当前事实 | 正确性影响 |
|---|---|---|
| F-01 | `elib_tsid` 为每个 generator name 创建独立 atomics | 同节点、同毫秒、不同业务名可生成相同数值，跨表日志、事件、附件和审计引用不具备数值全局唯一性 |
| F-02 | atomics 仅驻留内存，重启从 0 开始 | 若重启后系统时钟落后于历史生成时刻，可能重放历史 `(timestamp,node,sequence)` |
| F-03 | `generate_n/2` 是 `[generate(Name) || ...]` | N 个 ID 至少 N 次成功 CAS，未做 batch reservation |
| F-04 | sequence 达 2047 后直接推进逻辑毫秒 | 持续超过 2048 ID/ms 时逻辑时间可无限领先真实时间，ID timestamp 语义失真 |
| F-05 | `NowRel` 在 CAS 重试前只读取一次 | 激烈竞争时 CAS 失败者持续使用陈旧墙钟，放大无谓逻辑领先 |
| F-06 | cursor 编码为 `(RelTs bsl 11) bor Seq` | 最大值为 `2^53-1`，放入 signed 64-bit atomics 本身安全；不能据此推出完整 ID 或输入均合法 |
| F-07 | 最大合法完整 ID 恰为 `2^63-1` | 42/10/11 布局覆盖 signed PostgreSQL `BIGINT` 正数上界，任何再多 1 ms 都必须拒绝 |
| F-08 | 42-bit 最大相对毫秒为 `4398046511103` | 最后合法时刻为 `2164-05-15 07:35:11.103 UTC`；下一毫秒永久不可生成 |
| F-09 | `from_binary/1` 接受任意正 Erlang integer | 可接收大于 `2^63-1` 的输入；`parse/timestamp/node_id` 随后会掩码截断，伪装成另一个 TSID |
| F-10 | `from_base62/1` 空串返回 0，非法字符可能 `function_clause`，超界不拒绝 | trust boundary 行为不统一且可接受非 TSID 值 |
| F-11 | `calendar:system_time_to_universal_time/2` 返回秒精度 datetime | `parse/1.timestamp` 保留毫秒；`created_at` 天然截断毫秒。最大日期在 calendar 范围内，但二者精度语义必须写清 |
| F-12 | `init/1` 可重写 CombinedNode，而旧 cursor 仍存在 | 同一 VM 重配节点的行为未冻结；生产应拒绝非幂等重初始化，测试必须使用显式 reset/seam |
| F-13 | `register/1` 的 names 是 read-modify-write persistent_term | 并发注册不同名称存在丢更新窗口；注册不是 ID 热路径，可由本地控制进程串行化 |
| F-14 | CAS 成功顺序给出单节点 cursor 的线性化顺序 | 并发调用的返回/消息到达顺序不保证按 ID 排序；只能保证区间不重叠、每个 batch 内严格升序 |
| F-15 | `generate_n` 当前没有生产源码调用 | 可以先以测试和兼容 API 锁定新语义，但仍不得删除或改动现有 arity/健康路径返回类型 |
| F-16 | Helm 后端 `replicaCount=1`，默认 RollingUpdate 可能短时存在新旧 Pod | 若两 Pod 复用同一 CombinedNode，即使副本目标为 1 也可能并发生成；必须 Recreate 或证明每 Pod NodeId 稳定且唯一 |
| F-17 | Docker/Helm 已挂载 `/opt/imboy/priv_runtime` | 可作为 durable guard 根目录；挂载存在不等于已证明 `fsync/rename/lock` 语义可靠 |
| F-18 | `/healthz` 同时被 liveness/readiness 使用且目前以 PostgreSQL 为主 | TSID guard 不可用时必须停止接流，但不能用错误的 liveness 策略制造无休止重启 |
| F-19 | ADR-0004 状态为 Proposed，拟从 10-bit node 段切 origin 位 | 本计划默认不改变 42/10/11 布局；必须先裁决 ADR 冲突，禁止顺手同时落地 origin 切位 |

基线文件 SHA-256：

```text
e69f4467fc2f680cd2fb6d441433ae459aa8984b65268219faf6e063d340050b  src/lib/elib_tsid.erl
e906f2d4735c2d3a49baf6c44d2af82fa575f3810e84a761bbe994519c2f7b8e  test/lib/elib_tsid_tests.erl
32ab8d5574e2b3454acdb63969c505eeef0991fc68c3d43c2985cc391fe038d8  test/lib/elib_tsid_registration_guard_tests.erl
bbb8d791aabe20a7a6d5247c8d88abacd697f1ead79cdfd2e6117ae0a67b6b69  src/imboy_app.erl
0e3f8907f0571e480ec1473c657e03cdd5b1553df6f4c864b7a73010aec27058  docs/adr/0004-tsid-origin-namespace.md
```

这些指纹只用于发现计划编制后漂移。GLM-5.3 不得用它们覆盖当前文件，也不得假设执行时
HEAD 仍等于上述值。

### 1.2 问题优先级

| 优先级 | 问题 | 原因 |
|---|---|---|
| P0 | 跨重启重复、同 Node 双实例、42-bit 越界 | 会直接破坏主键/引用唯一性或生成负 BIGINT |
| P0 | durable guard 写入顺序与损坏恢复 | 错一次即可在崩溃恢复后重放 ID |
| P1 | 全局 generator、batch reservation、有界逻辑时间 | 降低跨域复杂度并解决吞吐与时间语义 |
| P1 | 输入边界与 Base62 | 防止非法大整数被掩码解析为合法-looking TSID |
| P1 | cutover 高水位与回滚 | 防止新全局空间撞到历史命名空间已有值 |
| P2 | 监控、文档、性能回归 | 保证无人值守运行可观测且可长期维护 |

## 2. 技术约束

### 2.1 不可破坏约束

1. ID 保持正 signed 63-bit，数据库列继续使用 PostgreSQL `BIGINT`，不做 schema 类型迁移。
2. 位布局保持 `[sign=0][timestamp:42][CombinedNode:10][sequence:11]`。
3. EPOCH 保持 `2025-01-01T00:00:00.000Z`。
4. 保留 `persistent_term` 发布不可变运行时句柄，保留 atomics/CAS 热路径。
5. `generate/0,1`、`generate_n/1,2`、`register/1`、`registered/0` 的 arity 和健康路径返回形状不变。
6. 每 ID 不访问数据库、不访问磁盘、不调用中心协调服务；跨节点唯一性继续依赖 CombinedNode 唯一。
7. 业务类型由表/领域表达；generator name 仅作为兼容和治理标签，不再分配独立数值空间。
8. 存量 ID 不重写；只承诺完成 cutover 后生成值在所盘点历史值及新值范围内全局唯一。
9. 正常运行不得依赖 wall clock 单调。等待 deadline、重试和超时必须用 monotonic time。
10. 任何无法证明不会重复的状态均 fail-closed，不得随机 reset、删除 guard 或自动换 NodeId 继续。

### 2.2 调用兼容边界

| API | 健康路径兼容要求 | 新增失败语义 |
|---|---|---|
| `generate/0,1` | 返回 `pos_integer()` | guard/锁/时钟/容量不安全时抛出稳定、可监控的 typed error，不返回猜测值 |
| `generate_n/1,2` | 返回严格升序的 integer list | 普通 batch 一次 CAS；超大 batch 采用有界 chunk reservation 保留 API，不退化为每 ID CAS |
| `register/1` | 继续幂等，`registered/0` 可见 label | 并发注册必须无丢更新；不创建独立 cursor |
| `parse/1` | 合法 ID map 现有字段保留 | 非 integer、`=<0`、`>2^63-1` 必须稳定拒绝 |
| `from_binary/1` | 合法十进制仍为 `{ok, Id}` | 空、符号、空白、非十进制、0、超界统一为 `error` |
| `to_base62/1` / `from_base62/1` | 合法 TSID round-trip 不变 | 非 TSID、空、非法字符、超长/溢出统一拒绝；具体 exception 形状先由 TSID-01 call-site inventory 冻结 |

非法输入从“偶然接受/偶然崩溃”改为稳定拒绝不视为健康调用破坏。若 inventory 发现生产调用依赖
旧非法行为，必须将 TSID-01 标记 `BLOCKED_CONTRACT_DECISION`，不得自行改变外部契约。

### 2.3 执行与 Git 约束

1. 共享主工作树有无关 WIP。执行前必须创建独立 worktree 和任务分支，禁止在共享 `main` 写代码。
2. 禁止 `reset --hard`、`clean`、`stash`、恢复或覆盖用户修改；禁止 blanket stage。
3. 每个功能卡验证通过后单独本地提交，只 stage 该卡 owned paths。
4. Git author/committer 使用命令级环境变量：`leeyi <leeyisoft@qq.com>`；不得改全局 Git 配置。
5. 不授权 push、PR、部署、生产迁移、生产 DB 扫描、真实凭据或第三方通知。
6. 证据写到仓库外：`${TMPDIR:-/tmp}/imboy-tsid-hardening/<RUN_ID>/`，不得污染仓库。
7. 同一根因最多自动修复重试 2 次（首次 + 2 次重试）；仍失败即精确 `BLOCKED_*`。

### 2.4 必须验证而不能靠推断的外部语义

| V-ID | 待验证结论 | 权威来源/实测 | 不通过行为 |
|---|---|---|---|
| EXT-01 | OTP 目标版本 atomics signed 范围、`compare_exchange/4` 返回和内存语义 | OTP 官方 atomics 文档 + 目标 OTP 28/29 小程序 | `BLOCKED_OTP_SEMANTICS` |
| EXT-02 | `system_time/1` 可回拨，monotonic time 仅适合测间隔 | OTP 官方 Time Correction / `erlang` 文档 | 禁止把 monotonic time 编进 ID；设计不成立则 STOP |
| EXT-03 | `file:sync/1`、`file:open(...directory...)`、rename 的目标平台行为 | OTP 官方 `file` 文档 + Linux 容器实测 | `BLOCKED_DURABILITY_PRIMITIVE` |
| EXT-04 | 目标 PVC/宿主卷的原子 rename、文件 fsync、目录 fsync、断电近似恢复 | 真实相同 storageClass/文件系统 crash harness | `RELEASE=NO_GO` |
| EXT-05 | lifetime lock 在两个独立 BEAM/容器间互斥且 owner crash 后释放 | 目标 Linux 镜像和 PVC 双进程测试 | `RELEASE=NO_GO`；不得用 NFS 不可靠的 `O_EXCL` 冒充锁 |
| EXT-06 | runtime 镜像是否包含可靠 `flock/lockf`，或需最小依赖 | Dockerfile + `command -v` + crash test | 无可靠 primitive 则 `BLOCKED_LOCK_PRIMITIVE` |
| EXT-07 | Boundary Flake durable clock guard 只作设计参考，不直接复制 128-bit 实现 | Boundary Flake 源码/README | 发现许可或语义不符时只保留独立实现 |
| EXT-08 | calendar 对最大 timestamp 的转换范围和毫秒截断 | OTP 官方 calendar 文档 + EUnit | 不得把 `created_at` 宣称为毫秒精度 |

参考入口：

- OTP atomics：<https://www.erlang.org/doc/apps/erts/atomics.html>
- OTP file：<https://www.erlang.org/doc/apps/kernel/file.html>
- OTP time correction：<https://www.erlang.org/doc/apps/erts/time_correction.html>
- Boundary Flake：<https://github.com/boundary/flake>

## 3. 候选方案

### 3.1 跨重启时钟保护

| 方案 | 优点 | 缺点 | 结论 |
|---|---|---|---|
| A. 仅启动时读上次 timestamp | 简单 | 每次生成后不持久化，崩溃仍可丢失最近进度 | 淘汰 |
| B. 每个 ID 同步落盘 | 最直观 | 完全破坏吞吐和无中心热路径 | 淘汰 |
| C. 周期异步保存已用 timestamp | I/O 少 | 保存前 crash 会重放，不构成严格保证 | 淘汰 |
| D. durable future fence | 先持久化未来上界，再只在 fence 内分配；摊销 I/O | 需严格写序、锁和恢复协议 | 推荐 |

核心不变量：任何 timestamp `T` 被分配前，磁盘上必须已有有效记录满足
`safe_before > T`。重启后从已持久化 `safe_before` 或更大 timestamp 开始，绝不再生成
`timestamp < safe_before`。

### 3.2 generator 模型

| 方案 | 跨表唯一 | 兼容性 | 复杂度 | 结论 |
|---|---:|---:|---:|---|
| 每业务独立 cursor | 否 | 当前行为 | 跨域引用高 | 淘汰 |
| 每业务分配 sequence 子区间 | 是（配置正确时） | label 仍影响编码 | 容量碎片、动态注册困难 | 淘汰 |
| 所有 label 映射同一全局 cursor | 是（单 CombinedNode） | API 可保持 | 最小 | 推荐 |

全局唯一仍是“在 CombinedNode 唯一且 cutover 正确”的约束下成立；它不是对错误 NodeId 配置的
魔法修复。Event/Audit/Message/Attachment/Agent Runtime 等可直接用 ID 关联日志，不再携带
“表名才能消歧”的隐含前提。

### 3.3 sequence 溢出

| 方案 | timestamp 语义 | 高负载可用性 | 回拨行为 | 结论 |
|---|---|---|---|---|
| 等待下一真实毫秒 | 最严格 | 上限 2048/ms，可能长阻塞 | 大回拨可无限等 | 不单独采用 |
| 永久逻辑推进 | 会无限漂移 | 不阻塞 | 隐藏时钟错误 | 淘汰 |
| 有界混合 | 最多领先 `max_logical_lead_ms` | 小突发吸收，持续过载显式失败 | 小回拨吸收，大回拨 bounded wait 后失败 | 推荐 |

推荐默认值只能由 TSID-00 基准与业务峰值确定，不能在实现时拍脑袋写死。候选起点为：

```text
max_logical_lead_ms = 5
capacity_wait_timeout_ms = 100
fence_window_ms = 1000
fence_renew_margin_ms = 100
startup_clock_wait_timeout_ms = 5000
```

这些是 benchmark 输入，不是本计划预先批准的生产值。最终值必须写入证据和 ADR。

### 3.4 batch reservation

| 方案 | CAS 次数 | 有界时钟 | API 兼容 | 结论 |
|---|---:|---:|---:|---|
| 循环 `generate` | N | 可做 | 是 | 淘汰 |
| 任意 N 一次 CAS、无限借未来 | 1 | 否 | 是 | 淘汰 |
| 可容纳 batch 一次 CAS；超大 N 分有界 chunk | 通常 1，最坏 `ceil(N/chunk)` | 是 | 是 | 推荐 |

普通 batch 必须是一次 CAS。对大于当前 lead 窗口容量的 N，为保持现有返回 list 的契约，按
最大安全连续区间分 chunk；每个 chunk 一次 CAS，禁止退化为逐 ID CAS。单次调用返回仍严格
升序，但并发调用可在 chunk 之间插入自己的区间，因此不承诺超大 batch 全部连续。

## 4. 推荐方案

### 4.1 目标架构

```text
generate(Name) / generate_n(Name, N)
              |
              +-- validate registered label
              |
              +-- persistent_term runtime handle (immutable)
                         |
                         +-- global cursor atomics -- CAS reservation hot path
                         +-- safe_before atomics --- durable horizon read
                         +-- status atomics -------- ready/fenced/stopping
                         +-- immutable node/layout/limits
                                      |
                          slow path only when fence nears
                                      |
                             elib_tsid_guard process
                                      |
                    lifetime lock + two-slot durable store
```

1. `persistent_term` 只发布已完全初始化、不可变的 runtime handle；禁止半初始化可见。
2. 所有 generator label 共享一个 cursor。label registry 由本地控制进程串行更新，生成热路径不
   向该进程请求 ID。
3. cursor 把 timestamp/sequence 看作线性 slot：`slot = ts * 2048 + seq`。
4. batch 通过一次 CAS 从 `old_slot` 跳到 `last_slot`，本地纯计算展开 ID。
5. durable guard 先落盘未来 fence，再把 `safe_before` 发布到 atomics；顺序不可反转。
6. 容量耗尽或 wall clock 回拨时，只允许在 lead 上限内前进；超过上限用 monotonic deadline
   等待，超时 typed failure。
7. 锁丢失、双槽均损坏、持久化失败、timestamp 越界时停止签发 ID 并把 readiness 置失败。

健康端点固定为：新增 `/livez` 只表示 BEAM/Cowboy 仍可服务；新增 `/readyz` 聚合 PostgreSQL 与
TSID guard；保留 `/healthz` 作为 `/readyz` 的兼容别名。Helm liveness 改用 `/livez`、readiness
改用 `/readyz`，避免 TSID/数据库暂时不可用触发错误的容器重启风暴。

### 4.2 明确不做

- 不替换为其他 TSID/Snowflake 库。
- 不改 42/10/11 位布局，不在本阶段落地 ADR-0004 origin 位。
- 不重写历史 ID，不创建跨表总 ID 表，不让每 ID 访问 PostgreSQL/Redis/etcd。
- 不承诺跨不同 CombinedNode 的调用完成顺序或因果顺序。
- 不将 TSID 用作不可猜测 token；它继续暴露大致时间和节点信息。
- 不以 mock、源码存在或单次 HTTP 200 代替真实文件系统/crash oracle。

### 4.3 STOP/GO 总原则

```text
GO:
  当前 candidate SHA 已冻结；全部 P0 Gate 有真实 oracle；durable store 和 lifetime lock
  在目标文件系统通过 crash matrix；cutover 高水位已在停写窗口重算；回滚不会启动旧生成器写入。

STOP:
  无法唯一分配 CombinedNode；guard 状态无法可信恢复；双槽均无效；锁不可证明互斥；
  now < epoch；timestamp > max；持久化/目录 sync 失败；candidate SHA 漂移；
  共享工作树被误修改；需生产凭据/生产写/对外操作但未获单独授权。
```

## 5. 数据结构/算法

### 5.1 常量与边界

```text
EPOCH_MS        = 1735689600000
TIMESTAMP_BITS  = 42
NODE_BITS       = 10
SEQUENCE_BITS   = 11
MAX_REL_TS      = 4398046511103
MAX_NODE        = 1023
MAX_SEQ         = 2047
MAX_ID          = 9223372036854775807
MAX_CURSOR      = 9007199254740991  # 2^53 - 1，signed atomics 安全
LAST_UTC        = 2164-05-15T07:35:11.103Z
```

每个公开解析入口先验证 `1 =< Id =< MAX_ID`，再移位。生成入口在任何移位前验证
`0 =< RelTs =< MAX_REL_TS`，避免先构造出 sign bit 为 1 的整数。

### 5.2 runtime handle

建议最小结构；字段名可按仓库规范调整，但不变量不可改变：

```erlang
#{cursor_ref => atomics_ref(),
  guard_ref => atomics_ref(),       %% safe_before + status/counters
  combined_node => 0..1023,
  dc_bits => 0..10,
  layout_hash => binary(),
  max_logical_lead_ms => non_neg_integer(),
  capacity_wait_timeout_ms => pos_integer(),
  max_batch_chunk => pos_integer(),
  guard_pid => pid()}.
```

`persistent_term` key 应从每 name 一份 state 收敛为单一 runtime key。name registry 可以是同一
handle 内的不可变集合快照；更新由 guard/control process 串行发布。旧的 `PT_STATE(Name)` 不应再
保存独立 atomics。

### 5.3 durable record

每个 CombinedNode 使用独立目录和双槽文件：

```text
/opt/imboy/priv_runtime/tsid/
  node-0129/
    manifest
    clock.a
    clock.b
    owner.lock
```

建议 canonical binary record（禁止 `binary_to_term` 接受任意不可信 term）：

| 字段 | 要求 |
|---|---|
| magic | 固定 8 bytes，例如 `IMBTSID1` |
| format_version | unsigned fixed width，当前 1 |
| layout_hash | 绑定 EPOCH、42/10/11、dc_bits 和编码版本 |
| combined_node | 0..1023，必须等于启动配置 |
| generation | 每次成功持久化递增，用于双槽择新 |
| safe_before | 0..`MAX_REL_TS + 1`；生成只允许 `ts < safe_before` |
| written_at_ms | 诊断字段，不参与唯一性判断 |
| payload_length | 严格长度校验 |
| crc32c/crc32 | 覆盖前述字段；算法必须固定并有 golden vector |

若 OTP/std library 只有 CRC32，则复用它，不为 CRC32C 单独加依赖。CRC 只检测损坏，不提供抗篡改；
目录权限必须为专用用户可写，拒绝 symlink，文件权限 0600、目录 0700。

写入协议：

```text
1. 持有 node lifetime lock。
2. 选择 generation 较旧/无效的槽为目标。
3. 在同一目录创建唯一 temp（exclusive），写完整 record。
4. file:sync(temp) 成功；close 的错误也必须检查。
5. rename(temp, target_slot) 成功。
6. 对父目录执行可证明有效的 directory sync。
7. 重读 target_slot，校验 exact bytes/CRC/generation/safe_before。
8. 最后 atomics 发布新的 safe_before。
```

任何一步失败：不发布新内存 horizon；清理仅限本次 temp；已存在有效槽不得删除；进入 retry/fenced
状态。不得先更新 cursor 再补持久化。

### 5.4 启动恢复

```text
acquire lifetime lock
  -> validate manifest/layout/node
  -> read clock.a + clock.b independently
  -> 0 valid:
       both absent + explicit bootstrap contract -> run fresh/existing bootstrap
       otherwise -> STOP CORRUPT_OR_MISSING
  -> 1 valid: select it, record DEGRADED, repair other slot before READY
  -> 2 valid: select highest generation; equal generation but unequal payload -> STOP SPLIT_BRAIN
  -> recovered_floor = persisted.safe_before
  -> if recovered_floor > MAX_REL_TS -> STOP EXHAUSTED
  -> if recovered_floor - now > allowed startup lead:
       monotonic bounded wait; timeout -> STOP CLOCK_BEHIND
  -> persist a new future fence > first allocatable timestamp
  -> initialize cursor to (start_ts << 11) - 1
  -> publish complete runtime handle
  -> READY
```

首次空库必须显式 `bootstrap=fresh` 且用 DB oracle 证明目标 TSID 集合为空。已有系统必须在停写窗口
执行 `bootstrap=existing` 高水位扫描。文件全部丢失时也必须走 existing scan，禁止自动当 fresh。

### 5.5 单个与批量 reservation

定义 `old` 为线性 cursor，`now_slot = NowRel bsl 11`：

```text
first = max(old + 1, now_slot)
last  = first + count - 1
first_ts = first bsr 11
last_ts  = last  bsr 11
```

预留伪算法：

```text
reserve(count, monotonic_deadline):
  require runtime status == READY
  now = wall_clock_ms() - EPOCH_MS
  require 0 <= now <= MAX_REL_TS
  old = atomics:get(cursor)
  candidate = [max(old + 1, now << 11), ..., + count - 1]
  require last_ts <= MAX_REL_TS
  if last_ts - now > max_logical_lead_ms:
      wait without busy-spin until wall clock catches up or deadline expires
      refresh wall clock and retry; on timeout fail typed
  if last_ts >= safe_before:
      synchronously ask guard to extend fence; coalesce concurrent renewals
      retry from fresh cursor/wall clock
  CAS(old, last)
    ok       -> materialize candidate slots to IDs
    conflict -> refresh wall clock and retry
```

materialize：`Ts = Slot bsr 11`，`Seq = Slot band 2047`，
`Id = (Ts bsl 21) bor (CombinedNode bsl 11) bor Seq`。同一 reservation 中 Slot 严格递增，故 ID
严格递增，即使跨毫秒也成立。

超大 N 分 chunk 时，每个新 chunk 必须从最新 cursor 重新 reserve，最终 list 严格升序。禁止先返回
部分 list 再报错；要么返回完整 list，要么抛出 typed failure，调用者不会观察半批结果。

### 5.6 时钟与 fence 参数关系

必须维持：

```text
0 <= max_logical_lead_ms < fence_window_ms
0 < fence_renew_margin_ms < fence_window_ms - max_logical_lead_ms
max_batch_chunk <= (max_logical_lead_ms + 1) * 2048
```

guard extension 的新 `safe_before` 至少大于 `max(now, current_cursor_ts) + fence_window_ms`，同时不能
超过 `MAX_REL_TS + 1`。接近 2164 边界时允许缩短窗口，但最后合法 timestamp 用完后永久停止。

## 6. 并发与故障场景

| 场景 | 自动行为 | GO 条件 | STOP/告警 |
|---|---|---|---|
| 多进程同时 generate | CAS 竞争，失败者刷新 wall clock 重试 | ID 全唯一；非重叠调用单调 | 重试超预算 -> overload typed failure |
| generate 与 generate_n 并发 | 各 reservation 在 CAS 处线性化 | 区间不重叠；batch 内严格升序 | 不承诺跨进程返回顺序 |
| named generators 并发 | label 校验后共用 cursor | 跨 name 零交集 | 任何独立 cursor 残留 -> STOP |
| sequence 用尽 | 允许最多 lead 上限，随后等待真实时间 | deadline 内追平 | 超时 `logical_clock_ahead/capacity_exhausted`，不继续借未来 |
| 小幅 wall clock 回拨 | 沿用 cursor，lead 未超限可继续 | 回拨被 fence/lead 吸收 | 记录 metric |
| 大幅 wall clock 回拨 | monotonic bounded wait | 时钟追平后自动恢复 | timeout 后 readiness=false，停止发 ID |
| fence 临界 | 一个控制进程扩展，其他调用等待/合并 | disk commit 后才发布 horizon | sync/rename/readback 失败即 fenced |
| 写 temp 前/中 crash | 旧双槽仍有效 | 重启选旧 generation 并跳过旧 fence | temp 可回收，不得覆盖旧槽 |
| rename 后、dir sync 前 crash | 按两个槽实际有效性择新 | 至少一个有效；不重放 | 目标 FS 无法证明 -> NO_GO |
| 单槽损坏 | 读另一有效槽并修复 | 修复且再次重启通过 | readiness 在修复前不得 true |
| 双槽损坏/丢失 | 不自动 reset | existing bootstrap + 停写扫描可恢复 | 默认 STOP，保留坏文件取证 |
| 同 Node 第二实例 | lifetime lock 获取失败 | 第二实例拒绝 READY/生成 | 不能改 NodeId 偷跑 |
| lock owner crash | OS 自动释放锁，新实例按 durable fence 恢复 | 双进程 crash test 通过 | stale lock 需人工删则 NO_GO |
| 磁盘满/只读/权限变化 | 当前 fence 内可按明确策略短暂服务；到 renew margin 前 fenced | 存储恢复、成功扩 fence 后 READY | 不得越过 persisted safe_before |
| persistent_term re-init | 相同配置幂等返回；不同配置拒绝 | 无 cursor 重置 | typed `already_initialized` |
| 2164 边界 | 只生成至 MAX_ID | 最后 ID 正且可解析 | 下一 slot 永久 exhausted |
| readiness 查询 | 报 guard、lock、clock、fence 状态，不泄露路径敏感信息 | 不可生成时 readiness 非 200 | liveness 仍只表示 VM 活着，避免重启风暴 |

运行时持久化失败策略必须机器可判定：若当前已持久化 fence 尚有安全余量，可以继续发放
`ts < safe_before` 并持续重试；到 `fence_renew_margin_ms` 或 retry deadline 即转 `FENCED`。恢复后
只有成功提交更高 fence 才能回 READY。任何路径都不能发放 `ts >= safe_before`。

## 7. 测试矩阵

### 7.1 功能与边界矩阵

| T-ID | 测试 | Oracle | 层级 |
|---|---|---|---|
| T-001 | 42/10/11 encode/decode golden vectors | 每字段 exact，相同布局 | EUnit |
| T-002 | `MAX_ID` 解析 | timestamp=`LAST_UTC ms`、node=1023、seq=2047、正数 | EUnit |
| T-003 | 下一 timestamp/slot | typed exhausted，绝不返回 ID | EUnit |
| T-004 | before epoch clock | 启动/生成 fail-closed | deterministic clock |
| T-005 | decimal 边界 corpus | 仅 canonical `1..MAX_ID` 接受 | EUnit/fuzz |
| T-006 | Base62 corpus | valid round-trip；空/0/非法/溢出稳定拒绝 | EUnit/fuzz |
| T-007 | calendar 最大日期 | timestamp 保留 `.103` ms；`created_at` 明示秒精度 | EUnit |
| T-008 | dc_bits 0..10 全边界 | CombinedNode exact，无负移位/越界 | EUnit |
| T-009 | 重复同配置 init | cursor 不回退 | EUnit |
| T-010 | 不同配置 re-init | typed reject，旧 runtime 不变 | EUnit |

### 7.2 batch、并发与时钟矩阵

| T-ID | 测试 | Oracle |
|---|---|---|
| T-101 | N=1/2/2048/2049/10000 | 每个可容纳 chunk 仅一次成功 CAS；list 严格升序且唯一 |
| T-102 | batch 从 seq=2047 跨毫秒 | timestamp/seq 展开 exact，无洞或重复 |
| T-103 | 32/64 workers 混合 generate/batch | 至少 1,000,000 IDs，`count == usort count` |
| T-104 | user/group/attachment 混合 | 所有新 ID 全局零交集 |
| T-105 | CAS 人工冲突 | 失败者刷新 wall clock；无递归栈/饥饿失控 |
| T-106 | 小回拨、刚好 lead 边界、超过边界 | continue / wait / typed timeout 三态 exact |
| T-107 | 持续超 2048/ms | lead 永不超过配置；不无限逻辑推进 |
| T-108 | 超大 N chunk | 完整 list 严格升序；CAS 次数为 chunk 数，不是 N |
| T-109 | 并发完成顺序 | 测试只断言 reservation 线性化，不错误断言消息到达顺序 |

### 7.3 durable/crash 矩阵

| T-ID | 故障点 | 恢复 Oracle |
|---|---|---|
| T-201 | 首次 bootstrap 前 crash | 无 READY、无 ID；重跑可恢复 |
| T-202 | temp create/write 中 kill -9 | 旧槽恢复，首个新 ts `>= old safe_before` |
| T-203 | file sync 后 kill -9 | 同上，无重复 |
| T-204 | rename 后、directory sync 前 kill -9 | 选任何有效最高 generation 均不生成旧 fence 以下 ID |
| T-205 | directory sync 后、内存 publish 前 kill -9 | 新槽恢复，允许跳号，禁止重复 |
| T-206 | 单槽 truncate/bit flip/bad CRC | 使用另一槽，自动修复后两槽有效 |
| T-207 | 双槽损坏 | STOP，坏文件保留，不能 fresh reset |
| T-208 | equal generation divergent payload | STOP split-brain |
| T-209 | node/layout hash 不匹配 | STOP config mismatch |
| T-210 | ENOSPC/EACCES/EROFS | 不越 fence；margin 前转 FENCED；恢复后可续租 |
| T-211 | 两个独立 BEAM 同 Node | 恰一个持锁并 READY，另一个确定失败 |
| T-212 | 持锁 BEAM kill -9 | 锁自动释放；新实例从 fence 恢复 |
| T-213 | 系统 clock 向后/向前注入 | bounded wait 或 horizon/exhaustion exact |
| T-214 | 连续 100 次随机 crash/restart | 全历史 ID 零重复，最小新 timestamp 不低于上次 durable fence |

### 7.4 cutover、部署与性能矩阵

| T-ID | 测试 | Oracle |
|---|---|---|
| T-301 | 历史 catalog inventory | 每个 TSID 生产列有 owner、查询和计数；无 UNKNOWN |
| T-302 | 停写前后高水位 | 停写后两次扫描值稳定；bootstrap floor 大于历史最大 timestamp |
| T-303 | 旧 named 与新 global 数据集 | cutover 后 ID 不与历史任一已盘点 ID 相等 |
| T-304 | 回滚演练 | 新代码写过 ID 后旧生成器不得重新接写；只允许 forward-fix 或恢复前快照 |
| T-305 | Helm render | same-node 模式为 Recreate；或每 replica 唯一稳定 NodeId 有机械证明 |
| T-306 | readiness/liveness | guard fenced 时 readiness fail、liveness pass；恢复后 readiness 自动 pass |
| T-307 | 单 ID 基准 | 5 轮 median 吞吐不低于基线 90%，p99 不高于基线 125%（预热且无续租） |
| T-308 | batch 基准 | N=1k/10k，5 轮 median 至少为旧循环实现 2x，且 CAS 计数符合 reservation |
| T-309 | 并发 soak | 30 分钟，无重复、无越界、RSS 无持续线性增长、lead/fence invariant 始终成立 |

阈值若因环境噪声失败，只允许重跑同一冻结 candidate 2 次并报告全部原始样本；禁止删除最差样本或
降低阈值。若业务基线表明 2x 不现实，必须以数据提交 `BLOCKED_ACCEPTANCE_DECISION`，由用户改 Gate。

## 8. 实施步骤

### 8.1 GLM-5.3 启动合同

将下列文字作为 GLM-5.3 的唯一启动指令；计划正文是权威范围：

```text
读取 docs/architecture/2026-09-28-tsid-correctness-hardening-implementation-plan.md，
先校验同名 .sha256，再按 TSID-00 到 TSID-11 顺序执行。只在独立 worktree 工作，
不得修改共享 main 工作树，不得 reset/clean/stash/覆盖任何用户 WIP。每卡先制造 RED，
再最小实现到 GREEN，保存仓库外证据，经卡内 Verify 和 Acceptance 后才做该卡本地提交。
不 push、不建 PR、不部署、不访问生产、不使用真实凭据、不通知第三方。遇 STOP 条件立即
fail-closed，写 RESULT.json 和 PROGRESS.md，以精确 BLOCKED_* 结束，不得放宽 Gate。
```

启动时必须重采样并记录：

```bash
git rev-parse --show-toplevel
git rev-parse HEAD
git status --short --untracked-files=all
git worktree list --porcelain
git diff -- src/lib/elib_tsid.erl test/lib/elib_tsid_tests.erl \
  test/lib/elib_tsid_registration_guard_tests.erl src/imboy_app.erl \
  docs/adr/0004-tsid-origin-namespace.md
shasum -a 256 src/lib/elib_tsid.erl test/lib/elib_tsid_tests.erl \
  test/lib/elib_tsid_registration_guard_tests.erl src/imboy_app.erl \
  docs/adr/0004-tsid-origin-namespace.md
find priv/migrations -type f -name '*.up.sql' -print | sort | tail -1
```

若目标文件相对本计划基线已漂移，不自动覆盖：生成 `drift-report.md`。能基于当前 HEAD 重放本计划且
不吞改动则更新 `BASE_SHA_EXECUTED` 继续；语义冲突则 `BLOCKED_SHA_DRIFT`。

证据初始化：

```bash
RUN_ID="$(date -u +%Y%m%dT%H%M%SZ)-$(git rev-parse --short=12 HEAD)"
EVIDENCE_ROOT="${TMPDIR:-/tmp}/imboy-tsid-hardening/${RUN_ID}"
mkdir -p "${EVIDENCE_ROOT}"/{BASELINE,CARDS,TESTS,BENCH,CRASH,FINAL}
```

每条命令追加 `commands.jsonl`，至少含：`card_id,cwd,command,started_at,ended_at,exit_code,
candidate_sha,stdout_log,stdout_sha256`。禁止把凭据和完整环境变量写入证据。

状态机：

```text
PENDING -> STARTED -> RED -> GREEN -> EVIDENCE_SUBMITTED
        -> VERIFIED_PASS | VERIFIED_FAIL | BLOCKED_<REASON>
```

恢复时只读取 `PROGRESS.md` 和各 `CARDS/<ID>/RESULT.json`；重新核验 candidate SHA 和最近一项
oracle 后从首个非 `VERIFIED_PASS` 卡继续。不得仅凭聊天记录跳卡。

### 8.2 波次与依赖

```text
W0: TSID-00
W1: TSID-01 -> TSID-02
W2: TSID-03
W3: TSID-04
W4: TSID-05 -> TSID-06
W5: TSID-07
W6: TSID-08
W7: TSID-09 -> TSID-10
W8: TSID-11 -> TSID-G0..G7
```

这是单执行者顺序计划，不授权多代理并发写同一 worktree。若 GLM-5.3 自带 reviewer，只能做只读
review，不能与实现者并发修改 owned paths。

### 8.3 任务卡

#### TSID-00 基线冻结、调用盘点与架构裁决

- Owner：GLM-5.3。
- Depends：无。
- Owned paths：本卡只读；证据写 `BASELINE/`；需要更新计划时 STOP 请求用户，不直接改本计划。
- 实施：重采样 HEAD/WIP/worktrees/目标指纹；盘点所有 generate/register/parse/Base62 调用；列出所有
  TSID 数据库列、部署 NodeId 来源、启动顺序；测旧实现功能/并发/吞吐基线；裁决 ADR-0004 为
  `DEFERRED_BY_TSID_HARDENING` 或另写 superseding ADR 草案，默认不切 origin 位。
- RED：证明跨 name 可构造相同 ID、restart+回拨可重放、N batch 发生 N 次成功 CAS或等价调用。
- Verify：`make compile`；`make eunit t=elib_tsid_tests`；独立最小 reproducer；调用点 inventory count。
- Acceptance：`AC-00A` 当前事实可复现；`AC-00B` 所有调用/字段无 UNKNOWN；`AC-00C` 参数候选有基准数据；
  `AC-00D` ADR 冲突有单一结论。
- Evidence：`baseline.json`、`call-sites.tsv`、`tsid-columns.tsv`、`benchmark-baseline.json`、
  `adr-0004-decision.md`、命令日志。
- Retry/STOP：同命令最多 2 次；发现目标文件未归属本计划或 migration/catalog 无法完整盘点，
  `BLOCKED_INVENTORY`。
- Commit：无源码提交；若 ADR 结论获计划内默认授权，只在 TSID-11 文档提交落地。

#### TSID-01 最小测试 seam 与契约冻结

- Owner：GLM-5.3。
- Depends：TSID-00 PASS。
- Owned paths：`src/lib/elib_tsid.erl`、`test/lib/elib_tsid_tests.erl`，必要时新增一个纯模型测试模块；
  不得先实现 durable store。
- 实施：抽出纯 `reserve_candidate`/encode/decode 边界；提供私有可注入 wall clock 和 monotonic deadline；
  冻结 typed errors。优先纯函数和 stdlib，不新增仅一个实现的 factory/behavior。
- RED：固定时钟下覆盖回拨、overflow、MAX 边界、CAS 冲突模型，先失败。
- Verify：`make eunit t=elib_tsid_tests`；`make format-check`；`git diff --check`。
- Acceptance：`AC-01A` 测试不依赖真实 sleep 改系统时钟；`AC-01B` 公开 arity/健康返回不变；
  `AC-01C` 错误 taxonomy 固定。
- Evidence：RED/GREEN 日志、API diff、error-taxonomy.md。
- Retry/STOP：测试 seam 泄漏为生产公开 API 或需全局 mutable test config 时 `BLOCKED_TESTABILITY_DESIGN`。
- Commit：`test(tsid): freeze clock and reservation semantics`。

#### TSID-02 63-bit、时间、calendar 与输入校验

- Owner：GLM-5.3。
- Depends：TSID-01 PASS。
- Owned paths：`src/lib/elib_tsid.erl`、`test/lib/elib_tsid_tests.erl`；仅在 call-site 必须适配时修改对应
  最小调用文件并记录。
- 实施：统一 `1..MAX_ID` 校验；MAX_REL_TS fail-closed；decimal/Base62 canonical 校验与溢出前检查；
  记录 `created_at` 秒精度；验证 dc_bits/Node 边界和 signed cursor。
- RED：T-001..T-010 先红，包含 fuzz corpus 和 2164 下一毫秒。
- Verify：目标 EUnit、所有受影响调用方测试、`make dialyze-check`（若 baseline 本来失败则差分零新增）。
- Acceptance：`AC-02A` 非法输入不再掩码成合法 ID；`AC-02B` MAX_ID exact；`AC-02C` BIGINT 正数兼容；
  `AC-02D` calendar 精度无虚假声明。
- Evidence：boundary-vectors.json、fuzz seed/corpus、Dialyzer 差分。
- Retry/STOP：发现外部 caller 依赖非法输入，`BLOCKED_CONTRACT_DECISION`。
- Commit：`fix(tsid): enforce signed bigint and timestamp boundaries`。

#### TSID-03 单全局 cursor 与注册竞态修复

- Owner：GLM-5.3。
- Depends：TSID-02 PASS。
- Owned paths：`src/lib/elib_tsid.erl`、`src/imboy_app.erl`、必要的 TSID control child、
  `test/lib/elib_tsid_registration_guard_tests.erl`、相关 TSID tests。
- 实施：所有 labels 映射同一 StateRef；注册串行且无丢更新；相同配置 init 幂等，不同配置拒绝；
  `registered/0` 兼容；启动时完整 runtime 一次发布。
- RED：不同 name 固定同毫秒生成交集测试必须从“允许”改为“零交集”；并发 register 无丢 label。
- Verify：两个 TSID EUnit suite；全量 generate call-site guard；重复/异配 init tests。
- Acceptance：`AC-03A` 跨 name 1M IDs 零重复；`AC-03B` 只有一个 cursor ref；`AC-03C` API arity 不变；
  `AC-03D` Node 配置不可热切换。
- Evidence：global-cursor.json、registration-race.log、persistent-term inventory。
- Retry/STOP：存在无法迁移的独立 generator 语义 caller，`BLOCKED_GENERATOR_CONTRACT`。
- Commit：`refactor(tsid): share one global id cursor across labels`。

#### TSID-04 batch reservation 与有界逻辑时间

- Owner：GLM-5.3。
- Depends：TSID-03 PASS。
- Owned paths：`src/lib/elib_tsid.erl`、TSID tests/benchmark harness。
- 实施：线性 slot 一次 CAS reserve；跨毫秒展开；超大 N 有界 chunk；CAS 失败刷新 wall clock；
  hybrid lead/wait/deadline；等待不 busy-spin、不使用 wall clock 测超时。
- RED：T-101..T-109；通过 instrumentation 证明旧实现 N 次、候选普通 batch 1 次。
- Verify：目标 EUnit、1M mixed concurrency、可重复 fake clock tests、scheduler responsiveness probe。
- Acceptance：`AC-04A` 普通 batch 一次成功 CAS；`AC-04B` 严格有序且全局不重；`AC-04C` lead 有界；
  `AC-04D` 超时稳定失败且无无限等待。
- Evidence：cas-counts.json、concurrency-summary.json、clock-matrix.json。
- Retry/STOP：为追求一次 CAS 而突破 lead 上限时立即 `VERIFIED_FAIL`。
- Commit：`feat(tsid): reserve ordered batches with bounded logical time`。

#### TSID-05 双槽 durable future fence store

- Owner：GLM-5.3。
- Depends：TSID-04 PASS、EXT-03 初步 PASS。
- Owned paths：新增最少的 `src/lib/elib_tsid_store.erl` 及 tests；不接生产启动链。
- 实施：canonical record、layout hash、CRC、双槽选择、temp+sync+rename+dir sync+readback、权限/symlink
  检查、单槽恢复；用 fault injection 实现每个 crash point。
- RED：T-202..T-210 的 store 层测试先红。
- Verify：EUnit temp-dir suite；独立进程 kill -9 harness；record golden vectors。
- Acceptance：`AC-05A` 任一写断点至少保留一份有效旧/新槽；`AC-05B` 双坏不 reset；
  `AC-05C` generation 择新确定；`AC-05D` publish-before-durable 路径机械不存在。
- Evidence：record-format.json、crash-matrix.json、slot hexdumps/sha256（不得含敏感数据）。
- Retry/STOP：dir sync/rename 语义在本机无法验证先标 `PARTIAL_LOCAL`，目标 FS 仍是最终 NO_GO Gate。
- Commit：`feat(tsid): add crash-safe durable clock fence store`。

#### TSID-06 lifetime lock、guard 状态机与健康状态

- Owner：GLM-5.3。
- Depends：TSID-05 PASS、EXT-05/06 方案已证明。
- Owned paths：TSID guard/lock 最小模块、`src/imboy_sup.erl`、`src/imboy_app.erl`、
  `src/api/healthz_handler.erl`、`src/imboy_router.erl`、对应 tests；Dockerfile 只在锁 primitive 确需时修改。
- 实施：锁覆盖 guard 全生命周期；恢复时先锁后读；future fence 续租 coalescing；READY/FENCED/STOPPING；
  TSID child 必须早于任何可生成 ID 的 worker；增加 `/livez`、`/readyz`，保留 `/healthz` 为 readiness
  兼容别名。
- RED：双 BEAM 同 Node、owner kill、存储失败、锁丢失、guard 恢复 tests。
- Verify：T-201..T-214；监督树启动顺序测试；health endpoint contract tests。
- Acceptance：`AC-06A` 同 Node 恰一实例 READY；`AC-06B` crash 自动释放锁；`AC-06C` 永不越 fence；
  `AC-06D` fenced 不接流但不触发错误重启风暴。
- Evidence：lock-dual-process.json、guard-transitions.jsonl、health-contract.json、crash matrix。
- Retry/STOP：只能用 stale exclusive lock file 或 NFS 不可靠 O_EXCL 时 `BLOCKED_LOCK_PRIMITIVE`。
- Commit：`feat(tsid): supervise durable guard and fail-closed readiness`。

#### TSID-07 配置、持久卷与部署安全

- Owner：GLM-5.3。
- Depends：TSID-06 PASS。
- Owned paths：`config/*` 中相关配置、`.env.example`、`Dockerfile`、`deploy/docker-compose.community.yml`、
  `deploy/helm/{values*.yaml,templates/deployment-backend.yaml}`、preflight 及其 tests。
- 实施：显式 state dir/node/lead/fence 参数；校验目录位于 persistent mount；Docker/Helm preflight；
  same-node 默认 rollout 改 Recreate，或实现有机械证明的 StatefulSet ordinal NodeId（两者只能选一，默认 Recreate）。
- RED：Helm render 证明旧 RollingUpdate 可重叠；缺卷/只读/重复 Node 配置失败。
- Verify：compose config、helm template、shell tests、容器内 lock/fsync smoke；不得实际部署。
- Acceptance：`AC-07A` 默认不会相同 Node 并发 rollout；`AC-07B` state path 持久化；
  `AC-07C` 配置非法启动失败；`AC-07D` secret/PII 未进入配置或证据。
- Evidence：rendered manifests（脱敏）、preflight.log、image-primitives.json。
- Retry/STOP：选择动态唯一 NodeId 却无法保证重启稳定映射，`BLOCKED_NODE_IDENTITY`。
- Commit：`ops(tsid): persist node clock state and prevent overlapping writers`。

#### TSID-08 cutover 高水位、bootstrap 与回滚合同

- Owner：GLM-5.3。
- Depends：TSID-07 PASS、TSID-00 字段 inventory 完整。
- Owned paths：只读 scanner/离线 bootstrap 工具、scratch DB tests、runbook；不得连接生产。
- 实施：从 catalog/inventory 构造所有 TSID 列查询；验证合法范围；计算历史最大 timestamp；
  停写后二次扫描稳定；生成 node/layout-bound 初始 guard；fresh/existing 明确；定义旧版回滚禁写条件。
- RED：scratch DB 含跨表重复、未来 ID、MAX_ID、非法 bigint；scanner 必须给稳定分类。
- Verify：临时 PostgreSQL clone/scratch DB；同数据两次 scan digest 一致；bootstrap 后首批 ID 全大于历史 timestamp。
- Acceptance：`AC-08A` 历史表无漏项；`AC-08B` cutover floor 不低于任一历史 TSID timestamp+1；
  `AC-08C` 全程停写合同明确；`AC-08D` rollback 不允许旧 generator 在新写后接写。
- Evidence：scanner-manifest.json、scratch-high-water.json、cutover-runbook.md、rollback-decision-table.md。
- Retry/STOP：任何 UNKNOWN 列、扫描期间高水位变化、未来值超启动容忍度均 `BLOCKED_CUTOVER`。
- Commit：`feat(tsid): add offline high-water bootstrap and cutover checks`。

#### TSID-09 完整正确性、并发、crash 与 fuzz 验证

- Owner：GLM-5.3。
- Depends：TSID-08 PASS。
- Owned paths：TSID 专项 tests/harness；只允许为已复现 defect 最小修正生产代码并回到对应卡重验。
- 实施：执行 T-001..T-306；随机模型测试固定 seed；100 crash cycles；1M concurrency；解析 fuzz；
  目标文件系统测试若当前环境不可用，明确记录 external pending，不能假冒 PASS。
- RED：至少注入一次能被测试捕获的故意 invariant 破坏，再撤销该 mutation，证明 oracle 非真空。
- Verify：专项 EUnit、registration guard、crash harness、Helm/Compose checks。
- Acceptance：`AC-09A` 测试矩阵本地可执行项全绿；`AC-09B` mutation 被捕获；`AC-09C` 无 skip 当 PASS；
  `AC-09D` 每个外部项明确 PASS/BLOCKED。
- Evidence：test-report.json、fuzz.json、crash-matrix.json、mutation-proof.log。
- Retry/STOP：发现唯一性反例立即 `VERIFIED_FAIL`，不得靠重跑消失。
- Commit：`test(tsid): cover restart concurrency corruption and overflow`。

#### TSID-10 基准、soak 与参数定标

- Owner：GLM-5.3。
- Depends：TSID-09 PASS。
- Owned paths：benchmark harness、配置默认值及说明；不做无数据的微优化。
- 实施：同机器/OTP/CPU governor 下旧基线与 candidate 各 5 轮；单 ID、batch、混合并发、fence renew；
  30 分钟 soak；依据峰值和 crash 恢复窗口确定 lead/fence 参数。
- RED：先确认基准能区分旧循环 batch 和一次 reservation。
- Verify：T-307..T-309；原始样本、median/p95/p99、CAS/fence sync counters。
- Acceptance：`AC-10A` 单 ID 无显著退化；`AC-10B` batch >=2x；`AC-10C` 热路径无磁盘 I/O；
  `AC-10D` 参数满足 5.6 不变量且有数据来源。
- Evidence：`benchmark.json`、raw CSV、soak.json、parameter-decision.md、环境指纹。
- Retry/STOP：同 candidate 最多重跑 2 次；不达阈值 `BLOCKED_PERFORMANCE_GATE`。
- Commit：`perf(tsid): validate batch reservation and tune bounded clock limits`（仅有文件变化时）。

#### TSID-11 文档、独立 review 与 candidate 冻结

- Owner：GLM-5.3；只读 reviewer 与实现者身份须在 evidence 中区分。
- Depends：TSID-10 PASS。
- Owned paths：`docs/adr/0004-tsid-origin-namespace.md` 或新的 superseding ADR、TSID reference、运维 runbook、
  本计划状态附录；禁止更改功能来掩盖 review finding。
- 实施：同步全局唯一语义、timestamp 精度、errors、bootstrap/cutover/rollback、监控；独立 correctness review；
  冻结 `CANDIDATE_SHA`，生成 changed-files 和提交清单。
- RED：文档扫描必须先能发现“命名生成器可能重复”“绝不阻塞/无限借未来”等旧声明。
- Verify：`rg` 消除矛盾声明；完整 Gate；review findings 全部 disposition。
- Acceptance：`AC-11A` 代码/ADR/runbook 同义；`AC-11B` ADR-0004 不再与实现并存冲突；
  `AC-11C` candidate SHA 后无修改；`AC-11D` 所有 commit 只含 owned paths。
- Evidence：review.md、findings.json、changed-files.txt、commit-list.txt、candidate-manifest.json。
- Retry/STOP：P0/P1 finding 未关闭即 `BLOCKED_REVIEW`。
- Commit：`docs(tsid): define durable global generator operations`。

### 8.4 每卡 RESULT.json 最小 schema

```json
{
  "card_id": "TSID-04",
  "status": "VERIFIED_PASS",
  "base_sha": "<sha>",
  "candidate_sha": "<sha>",
  "started_at": "<RFC3339>",
  "ended_at": "<RFC3339>",
  "acceptance": {"AC-04A": "PASS"},
  "commands_file": "commands.jsonl",
  "evidence": [{"path": "cas-counts.json", "sha256": "<sha256>"}],
  "retries": 0,
  "findings": [],
  "stop_reason": null
}
```

每个 JSON 必须通过 `jq -e .`；所有 evidence path 必须位于 `EVIDENCE_ROOT`；最终 manifest 记录每个文件
SHA-256，避免结果与证据错配。

## 9. 验收 Gate

### 9.1 Gate 定义

| Gate | 必须满足 | 失败状态 |
|---|---|---|
| TSID-G0 范围与基线 | 独立 worktree；BASE/CANDIDATE SHA 清楚；无共享 WIP 被吸收；inventory 完整 | `BLOCKED_SCOPE_OR_DRIFT` |
| TSID-G1 编码兼容 | 42/10/11、EPOCH、BIGINT、合法 parse/Base62 golden vectors 全过 | `FAIL_ENCODING_COMPAT` |
| TSID-G2 唯一与顺序 | 跨 name/批量/并发/跨毫秒零重复；batch 内严格升序；语义声明不夸大 | `FAIL_UNIQUENESS` |
| TSID-G3 跨重启安全 | future fence 写序、双槽恢复、100 crash cycle、双坏 STOP 全过 | `FAIL_DURABLE_GUARD` |
| TSID-G4 时钟与容量 | lead 有界；回拨/过载 bounded wait；2164 永久 STOP；无 busy-spin | `FAIL_CLOCK_POLICY` |
| TSID-G5 单写者与部署 | lifetime lock 真互斥；owner crash 自动释放；rollout 无同 Node overlap | `BLOCKED_SINGLE_WRITER` |
| TSID-G6 质量与性能 | 专项/全量测试、静态 Gate、benchmark/soak 达阈值，零新增严重 finding | `BLOCKED_QUALITY_OR_PERF` |
| TSID-G7 cutover 可操作 | scratch 高水位、停写、bootstrap、readiness、forward-fix/回滚演练全过 | `BLOCKED_CUTOVER` |

G0-G7 必须全部 PASS 才能得到 `LOCAL_CANDIDATE_PASS`。本计划不含真实目标 PVC/生产 cutover 授权，
因此即使本地全绿也保持 `RELEASE=NO_GO`；外部存储 Gate 未执行时最终最多为
`LOCAL_CANDIDATE_PASS / EXTERNAL_VALIDATION_PENDING`，不得写成 release ready。

### 9.2 最终验证命令

按当前仓库已存在入口执行；某命令基线失败时必须记录 baseline/candidate 差分，不能静默跳过：

```bash
git diff --check
make compile
make eunit t=elib_tsid_tests
make eunit t=elib_tsid_registration_guard_tests
make eunit
make format-check
make contract-check
make security-gate
make dialyze-check
```

另执行本计划新增的 crash、dual-process lock、bootstrap、Helm render、benchmark 和 soak harness。全量
`make eunit` 只在冻结 candidate 上执行一次最终 Gate；各任务卡优先跑其 owned tests，避免反复用全仓
测试制造噪声。

### 9.3 机械验收规则

1. `acceptance-matrix.md` 列出 `AC-00A..AC-11D`，每项只能为 `PASS/FAIL/BLOCKED/NOT_APPLICABLE`；
   P0/P1 不允许 NOT_APPLICABLE。
2. `changed-files.txt` 必须等于 `git diff --name-only <BASE_SHA_EXECUTED>..<CANDIDATE_SHA>` 排序结果。
3. 每个本地 commit 的 author/committer 均为 `leeyi <leeyisoft@qq.com>`，且只含对应任务卡 owned paths。
4. `candidate-manifest.json` 中 SHA 必须等于所有最终命令实际执行的 SHA；测试后代码变化则全部 Gate 作废重跑。
5. `commands.jsonl` 每行可解析，exit code、日志 SHA 和 candidate SHA 齐全。
6. skipped、mock-only、源码存在、HTTP 200、旧报告均不能计作真实 crash/storage/cutover PASS。
7. 任一唯一性反例、越 durable fence、同 Node 双 READY、越 MAX_ID 直接 `FAIL`，不可降级为 warning。

可用机械检查示例：

```bash
jq -e . "$EVIDENCE_ROOT"/CARDS/*/RESULT.json "$EVIDENCE_ROOT"/FINAL/RESULT.json
awk -F'|' '/AC-[0-9]/{gsub(/ /,"",$0); print $0}' \
  "${EVIDENCE_ROOT}/FINAL/acceptance-matrix.md"
git diff --name-only "$BASE_SHA_EXECUTED..$CANDIDATE_SHA" | sort > /tmp/tsid.changed.actual
diff -u /tmp/tsid.changed.actual "${EVIDENCE_ROOT}/FINAL/changed-files.txt"
git status --short
```

### 9.4 最终 RESULT.json 与终态

最终证据目录至少包含：

```text
PROGRESS.md
commands.jsonl
BASELINE/baseline.json
CARDS/TSID-00..TSID-11/RESULT.json
TESTS/test-report.json
BENCH/benchmark.json
CRASH/crash-matrix.json
FINAL/acceptance-matrix.md
FINAL/changed-files.txt
FINAL/candidate-manifest.json
FINAL/RESULT.json
```

允许的最终状态：

| 状态 | 定义 |
|---|---|
| `LOCAL_CANDIDATE_PASS` | G0-G7 本地可执行项全过，candidate 冻结；仍 `RELEASE=NO_GO` |
| `PARTIAL` | 本地实现/测试通过，但目标 PVC、真实升级或其他外部 Gate 未执行 |
| `BLOCKED_<REASON>` | 缺少权限、primitive、环境或架构决策，已安全停止且证据完整 |
| `FAIL_<REASON>` | 发现可复现 correctness/security/compatibility 反例 |

禁止使用含糊的“基本完成”“看起来可用”。最终回复必须报告：执行基线、candidate SHA、本地提交列表、
PASS/FAIL/BLOCKED Gate、未执行外部验证、`RELEASE=NO_GO`，以及从 evidence manifest 可复核的路径。

### 9.5 发布前另行授权的外部 Gate

以下不在本计划当前执行授权内，只定义未来 GO 条件：

1. 在与生产相同 storageClass/PVC 上执行 EXT-04/05 crash 和双实例锁测试。
2. 获取真实峰值但脱敏的容量指标，复核 `max_logical_lead_ms` 和 fence window。
3. 在生产克隆/脱敏快照执行 catalog inventory 和高水位扫描，不写生产。
4. 经人工批准停写窗口后，再在真实环境重算高水位并 bootstrap。
5. 按 Recreate 或唯一稳定 NodeId 策略发布，验证 readiness、锁、guard、指标和日志。
6. 一旦新 generator 写入，不允许旧版本恢复写流；失败采用 forward-fix，或恢复到 cutover 前完整快照。

任一项没有独立授权与真实证据，发布结论保持 `RELEASE=NO_GO`。
