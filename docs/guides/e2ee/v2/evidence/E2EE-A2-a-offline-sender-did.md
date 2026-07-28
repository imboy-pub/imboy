# A2-a：离线路径 `sender_did` 持久化

- **Slice**：`22-claude-code-execution-state.md` §1.1 自动推进队列第 1 项（A2-a 后端 `sender_did` 持久化）
- **会话**：`20260728-1730-claude-code`
- **仓库**：`imboy`（单仓；本 Slice 不改客户端）
- **状态**：`PASS`（后端持久化闭环）；A2-b 客户端 decrypt-on-read 接线仍为下一件
- **上游依据**：`evidence/E2EE-012-024-025-029-reacceptance.md` §6.1 / §6.1.3

---

## 1. 做了什么

PFv3 接收侧 `_validateContextBinding` 第 6 项（ADR 15 §3.3）拿**信封顶层**的
`sender_did` 与受认证的 `protected_header.sender_did` 硬比对。

实时投递路径已闭合（`message_ds:stamp_sender_device/2` + `with_sender_device/2`）。
**离线路径没有**：发送者设备标识从未被持久化，因此离线期间收到的 C2C v3 消息
重连拉取后必判 `context_mismatch_sender_did` 而永久不可读。

本 Slice 把该标识贯通「WS 认证态 → staging → worker → `msg_c2c` → 离线读取 →
出站信封顶层」六个接缝：

| # | 接缝 | 改动 |
|---|---|---|
| 1 | 生产入口取值 | `msg_c2c_logic:stage_and_send_c2c/11` 从 `Data` 顶层取 `sender_did`（websocket_handler 已盖章，客户端不可伪造），传入 4 处 `stage` 调用 |
| 2 | staging 落库 | 新增 `msg_store_ds:stage/11` → `msg_store_repo:stage/11` |
| 3 | staging schema | 迁移 `00000048` 的 `ALTER TABLE IF EXISTS ... ADD COLUMN IF NOT EXISTS`（存量部署）**并** `msg_store_repo:ensure_table_exists/0` 的 DDL（全新安装） |
| 4 | worker 搬运 | `msg_store_repo:claim_pending/2` 的 SELECT 加列；`msg_store_worker:do_write(c2c, _)` 读出并传下去 |
| 5 | 正式表落库 | `msg_c2c_ds:write_msg/9` → `msg_c2c_repo:write_msg_with_sender/9`；`msg_c2c` 加列（迁移 48） |
| 6 | 离线读取与出站 | `msg_c2c_ds:read_msg_filter/3` 列集加 `sender_did`；`message_ds:offline_envelope/2` 并入信封顶层 |

### 1.1 两处设计取舍（按「选安全那个」裁决，未询问）

**取舍一：专用列，而不是塞进现成的 `e2ee` JSONB。**
`e2ee` 列已经端到端携带、零 schema 改动，看起来更省事。**否决**：E2EE-060
（后端 PFv3 不透明透传契约，状态 `PASS`）要求服务端对 `e2ee` 逐字节保真透传，
服务端注入自有字段会直接违约。这是既有契约裁决的，不是口味问题。

**取舍二：空值一律写 NULL，绝不写空串。**
`msg_store_repo:put_sender_did/2` 与 `msg_c2c_repo:null_if_empty/1` 在
`<<>>`/`null` 时**不写该列**；`message_ds:offline_envelope/2` 在读到
`null`/`<<>>`/缺键时**不往信封里放该键**。理由与 `with_sender_device/2` 的既有
注释一致：补空串会让接收侧把「服务端没提供」误判成「设备 ID 是空串」，
两者失败语义不同。这是 fail-closed 方向的选择。

**取舍三：`stage/10` 与 `write_msg/8` 保留原调用形状，不改写成「新 arity + 默认值」。**
第一版实现让旧 arity 委托给新 arity，结果 `msg_store_ds_tests`（5 例）与
`msg_c2c_ds_tests`（1 例）**真实回归**：这些测试按 arity 挂 meck 期望，
换 arity 后期望不命中，静默穿透到真实实现。已改为两条 arity 各自直调对应的
repo arity、共用结果归一化函数（`handle_stage_result/3`）。**这是实证发现的，
不是预判**——记录在此以免后续会话重犯。

---

## 2. RED 记录

先写测试、后改生产代码。新增
`test/ds/e2ee_offline_sender_did_tests.erl`（12 例）。

### 2.1 第一次 RED（改生产代码之前）

```
=======================================================
  Failed: 8.  Skipped: 0.  Passed: 4.
```

其中 2 例失败是 **harness 缺陷**（`elib_tsid` 生成器未注册），不是被测缺口。
按「对照组红 = harness 缺陷，立刻停下重估」的纪律停下修 harness
（`meck:expect(elib_tsid, generate, ...)`），未继续钻。

### 2.2 修完 harness 后的 RED（真实缺口）

```
=======================================================
  Failed: 6.  Skipped: 0.  Passed: 6.
```

**6 个红 = 6 个真实断点：**

| 用例 | 失败形态 |
|---|---|
| `read_msg_for_device_selects_sender_did_test` | 行为失败：列集里没有 `sender_did`（`?assert` false） |
| `read_msg_selects_sender_did_test` | 同上 |
| `offline_envelope_carries_sender_did_test` | 行为失败：信封里没有该键 |
| `claim_pending_selects_sender_did_test` | 行为失败：SELECT 语句里没有该列 |
| `ensure_table_ddl_has_sender_did_test` | 行为失败：CREATE TABLE DDL 里没有该列 |
| `stage_persists_sender_did_test` | `undef`（`stage/11` 尚不存在） |

**认识论诚实标注**：前 5 例是**行为失败**（函数存在、被生产调用、行为不对）。
第 6 例是 `undef`——持久化这一腿需要新增 API 面，改前不存在可失败的行为。
它的真实证据是改后的**真 PostgreSQL 集成用例**（见 §4.2），不是这条 RED。

### 2.3 对照组（harness 有效性）

改前就绿、改后仍绿的 4 例，证明 harness 本身没坏、断言不是恒成立：

- `offline_envelope_preserves_payload_and_e2ee_test` —— **正向可用性**：
  `payload` / `e2ee` / `id` / `type` / `from` / `to` 逐字段原样透传。
  一个「丢弃所有字段」的实现在这条上必红。
- `offline_envelope_null_sender_did_not_fabricated_test`
- `offline_envelope_absent_sender_did_not_fabricated_test`
- `offline_envelope_empty_sender_did_not_fabricated_test`

另有 `stage_legacy_arity_omits_sender_did_test` /
`stage_group_arity_omits_sender_did_test` 守护向后兼容（旧 arity 不得多写空值列）。

### 2.4 「只验拒收」反模式的规避

本 Slice 没有任何「篡改能否拒收」类断言，全部是**正向可用性**：
断言的是「正确的设备标识能一路带到信封顶层」「payload/e2ee 不被破坏」。
一个拒绝所有消息、或丢弃所有字段的实现在这些断言上**拿零分**。

---

## 3. 生产调用方核实（避免又掉在旁路上）

E2EE-012/023/024/025/029 的教训是「验收对象不是生产实际调用的入口」。
本 Slice 逐个核实：

| 被测函数 | 生产调用链 |
|---|---|
| `msg_c2c_ds:read_msg_for_device/4` | `messaging_logic:offline/6`（HTTP 拉取）与 `message_ds:check_and_notify_offline_msgs/2`（WS 上线推送）**两条**生产路径 |
| `message_ds:offline_envelope/2` | `sent_offline_msg/4` ← `handle_offline_msgs/5` ← `check_and_notify_offline_msgs/2` ← `websocket_handler` |
| `msg_store_repo:stage/11` | `msg_store_ds:stage/11` ← `msg_c2c_logic:stage_and_send_c2c/11`（4 处）← `msg_c2c_logic:c2c/3` |
| `msg_store_repo:claim_pending/2` | `msg_store_worker:claim_and_process_batch/0`（gen_statem 常驻） |
| `msg_store_repo:ensure_table_exists/0` | `msg_store_ds` 监督树启动时 |

`message_ds:offline_envelope/2` 是本次从 `sent_offline_msg/4` **抽出**的纯函数
（step 0，零行为变更，抽出后既有 292 例仍全绿）。抽出的唯一目的是可测：
离线信封的字段集是 PFv3 接收侧的硬依赖，必须能在不起 syn、不发真实 WS 帧的
前提下被断言。它**不是**为了测试而新造的旁路——生产走的就是它。

---

## 4. 验收命令与结果

### 4.1 门禁套件（无 DB，确定性）

```
$ make e2ee-verify
  All 304 tests passed.
=== E2EE verify ALL PASSED ===
```

基线 292 → 304（+12 全部为本 Slice 新增）。
新模块 `e2ee_offline_sender_did_tests` **已加进 Makefile 的 Modules 清单**，
受门禁保护。

### 4.2 真 PostgreSQL 端到端（**已实证**，非阅读结论）

```
$ IMBOYENV=local make eunit t=e2ee_message_pipeline_integration_tests \
    EUNIT_ERL_OPTS="-config config/sys.local -pa ebin -pa test"
  All 4 tests passed.
```

新增用例 `test_sender_did_survives_pipeline_to_offline_envelope/0` 闭合整条链：

1. `msg_store_ds:stage/11` 写 staging；
2. `msg_store_worker` 异步搬进 `msg_c2c`，断言 `msg_c2c.sender_did` == 写入值；
3. `msg_c2c_ds:read_msg_for_device/4` 读回，断言行里带该值；
4. `message_ds:offline_envelope/2` 组装，断言**信封顶层**带该值；
5. 正向可用性：信封的 `e2ee` 与写入时逐字段一致。

### 4.3 迁移落地实证（直连本地 PostgreSQL 核实，非推断）

```
$ psql ... -tAc "select * from schema_migrations order by version desc limit 3;"
48|f|2026-07-28 17:30:53.290477+08

$ psql ... -tAc "select table_name, column_name, data_type, character_maximum_length
                 from information_schema.columns where column_name='sender_did';"
msg_c2c|sender_did|character varying|128
msg_store_staging|sender_did|character varying|128
_hyper_1_1_chunk|sender_did|character varying|128      (× 7 个 TimescaleDB chunk)
compress_hyper_50_4_chunk|sender_did|USER-DEFINED      (× 6 个压缩 chunk)
```

`msg_c2c` 是 TimescaleDB hypertable：`ADD COLUMN` 已正确传播到全部普通 chunk
与压缩 chunk，无报错、无数据重写。

### 4.4 受影响模块的回归

| 套件 | 结果 |
|---|---|
| `msg_store_ds_tests` | 13/13 ✅（先红 5 例，见 §1.1 取舍三，已修） |
| `msg_c2c_ds_tests` | 21/21 ✅（先红 1 例，同上，已修） |
| `msg_store_repo_tests` | 32/32 ✅ |
| `message_ds_tests` | 10/10 ✅ |
| `git diff --check` | 通过 |
| `erlfmt --check`（全部改动文件） | 通过 |

### 4.5 预存基线失败（**非本次引入**，未修）

| 套件 | 失败 | 判定依据 |
|---|---|---|
| `msg_c2c_repo_tests` | 2 例 `msg_c2c_repo:read_msg/3` **undef** | 该函数早已因 SQL 注入风险从公共 API 移除（`msg_c2c_repo.erl` 源码注释明载），测试未同步。本次 diff 未触及 `read_msg`（`git diff` 核实） |
| `msg_reply_integration_tests` | 1 例 `test_reply_nonexistent_msg` | 断言 `c2c/3` 返回 `ok`，实际返回 `{reply, #{action => msg_not_found}}`——这是生产**设计行为**（引用目标不存在时回 S2C 错误）。测试与实现的口径分歧，与 `sender_did` 无关 |

两者均**未删除、未 skip、未放宽**，原样留红并在此备案。

---

## 5. 残留风险（未闭合）

1. **A2-b 客户端 decrypt-on-read 接线仍未做**（队列第 2 项）。
   后端现在提供了 `sender_did`，但客户端
   `message_model_mapper.dart::toTypeMessage()` 仍不认识 v3
   （见 reacceptance §6.1.2）。**在 A2-b 完成前，离线 v3 消息依然不可读**——
   本 Slice 是必要条件，不是充分条件。

2. **引用回复路径存在预存重复写点，本次刻意未动。**
   `msg_c2c_logic:stage_and_send_c2c/11` 里 `stage/11`（`created_at=CreatedAtRfc`,
   `server_ts=NowTs`）与 `msg_c2c_ds:write_msg_with_reply/11`
   （`created_at=NowTs`, `server_ts=CreatedAtRfc`）**两个参数互换**，
   `ON CONFLICT (msg_id, created_at)` 因此不会命中 → 同一条引用回复消息
   可能落两行，其中 `write_msg_with_reply` 那行没有 `sender_did`。
   **取舍**：给 12-arity 函数的 3 个子句扩参并不能修掉重复行本身，只会扩大
   爆炸半径；而 staging 那条腿已经为**每条** C2C 消息（含引用回复）产出了
   带 `sender_did` 的行。留作独立 Slice。
   **认识论状态：文件级阅读结论，未实证**（未构造引用回复消息实测是否真落两行）。

3. **`sender_dtype` 未持久化**（按 §6.1.3 的既定裁决）。context binding 不用它，
   属展示元数据。若离线消息的设备类型展示出现缺失，再加列或从 `user_device` 反查。

4. **`msg_archive_repo:archive/1` 未同步。** 永久归档表不带 `sender_did`；
   若 `msg_archive_enabled=true` 的部署将来用归档表回放 v3 历史消息，
   会遇到同一缺口。**认识论状态：文件级阅读结论，未实证。**

5. **C2G 未覆盖。** 当前 C2G 走 Megolm v2（非 PFv3），不受本缺陷影响；
   C2G 若上 PFv3，`msg_c2g_logic` 需同步接 `stage/11`。

6. **真机双端始终未验证。** 本 Slice 的全部结论只在单测 + 本地 PostgreSQL 层成立，
   与 session_ref / message_id / message_type / ADR 26 counter 语义 /
   E2EE-030 PFS 同属一条真机腿。

7. **旧行不可回填。** 迁移 48 之前落库的消息 `sender_did` 为 NULL，
   信封不带该键，接收侧仍按「服务端未提供」处理。这是设计选择，不是缺陷——
   但意味着**升级前积压的离线 v3 消息永久不可读**。

---

## 6. 认识论状态汇总

| 结论 | 状态 |
|---|---|
| 六个接缝全部贯通、信封顶层带出 `sender_did` | **已实证**（真 PostgreSQL 集成用例 §4.2） |
| 迁移在 TimescaleDB hypertable 上安全落地 | **已实证**（直连 PG 查 `information_schema` §4.3） |
| 空值不伪造（NULL / `<<>>` / 缺键三种输入） | **已实证**（单测 §2.3） |
| `payload` / `e2ee` 不被破坏（E2EE-060 透传） | **已实证**（单测 + 集成 §4.2） |
| 旧 arity 调用方零行为变更 | **已实证**（回归套件 §4.4，且是被回归**打脸后**才修对的） |
| 引用回复路径落两行 | **文件级阅读结论，未实证** |
| `msg_archive` 同款缺口 | **文件级阅读结论，未实证** |
| 离线 v3 消息「因此变得可读」 | **不成立**——还缺 A2-b 客户端接线（残留风险 1） |

---

## 7. 未做

- 不 push、不部署、不访问生产、不通知第三方。
- 未改任何 ADR、未改协议规范、未动 E2EE-012/023/024/025/029 的状态标记。
- 未删除、未 skip 任何测试；未放宽任何安全策略；未开 fallback 变绿。
