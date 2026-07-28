# 任务 A：按新边界重新验收 E2EE-012 / 023 / 024 / 025 / 029

- **会话**：`20260728-1622-claude-code`
- **新边界**：生产入口 `E2EEService.decryptInboundV3`
  （`message.dart::_handleE2EEMessage` 第 0 步实际调用），
  不再以生产不走的 `decryptIncomingPayload` 为起点
- **结果**：实证发现**接线之后仍存在的第 4 个断点**并已修复；
  正向可用性门与 fail-closed 负向门建立
- **状态标记**：E2EE-012/024/025/029 四项**未擅改**（待人工决策 2）；
  E2EE-023 经人工裁定维持 `PASS`，仅撤回「无残留风险」一句

---

## 1. 实证发现：生产 C2C v3 消息仍然 100% 不可读

上一会话接线了 PFv3 接收路径（`decryptInboundV3`），但**接线不等于可用**。
本会话把入站帧改为后端**真实投递形状**后，7 项正向用例全红，
失败原因一律 `context_mismatch_sender_did`。

### 1.1 事实链（逐文件核实，非静态推断）

| 事实 | 出处 |
|---|---|
| C2C 投递帧字段集 = `ver/id/type/from/to/msg_type/action/e2ee/payload/server_ts` | `imboy/src/ds/message_ds.erl` `assemble_msg/8` |
| `sender_did` 注入的是 **payload 内部**，不是帧顶层 | 同上 `inject_sender_device/2`：`maps:put(<<"sender_did">>, DID, Payload)` |
| 注入只在 payload 是 map、或可 JSON 解码为 map 的 binary 时发生 | 同上 |
| 注入调用点作用于 payload，结果回填 `<<"payload">>` | `imboy/src/api/websocket_handler.erl`（JSON 与 protobuf 两处） |
| v3 外层 payload **恒为空串** | `imboyapp/lib/service/e2ee/e2ee_outbound_router.dart:183` |
| 客户端读的是**帧顶层** `data['sender_did']` | `imboyapp/lib/service/message.dart:539`、`e2ee_service.dart:721` |
| 全后端仅 4 处出现 `sender_did`，无任何路径放到帧顶层 | 逐文件 `awk` 核实 |

两条事实相叠：v3 的 payload 是空串 → `inject_sender_device` 走 binary 分支、
`jsone:decode(<<>>)` 抛异常被 catch → **原样返回，什么都没注入** →
接收侧 `data['sender_did']` 恒为 null → context binding #6 必然失配。

> ⚠️ 工具坑：后端 `rg`/`grep` 输出会被钩子污染（`sender_did` 被替换成 `n`），
> 导致最初读到的是错误的函数名与字段名。**必须用 `awk` 或直接 `Read` 核实。**
> 这与项目记忆 `project_tool_output_pollution_rtk_hook` 是同一类问题。

### 1.2 RED 记录

```
$ flutter test test/service/e2ee/production_inbound_frame_gate_test.dart
00:00 +1 -7: Some tests failed.
  失败原因: context_mismatch_sender_did   ×7
```

**对照组（同一条帧、仅手工补顶层 `sender_did`）通过** —— 这一项绿
把断点精确隔离在 `sender_did` 上，排除了 harness 缺陷（假协议形状 /
信封构造 / DB）的可能。这正是上一会话总结的方法论：先放对照组。

### 1.3 这条链此前为什么没被发现

`v3_receive_path_e2e_test.dart::buildV3Message` 在构造入站帧时
**手工补了顶层 `'sender_did': 'dev-sender'`** —— 与
`E2EE-012-024-review.md` 批评的「改 fixture 去迁就生产缺陷」是同一个模式，
只是这次发生在接线验收里。review §3 已把 #6 标为「⚠️ 未实证」，
本会话把它实证了。

---

## 2. 修复（人工签字：方案 A，2026-07-28）

设备标识必须盖在**信封层**而非 payload 层：信封由服务端组装，
与 payload 是否加密、是否为空无关，因此对所有 payload 形状都成立。

安全语义不变且不降级：值取自 WebSocket State 的 `did`/`dtype`
（连接建立时认证得到），客户端无法伪造 —— 这正是 ADR 15 §3.3 第 6 项
要绑定的那个「服务端验证过的」值。

| 文件 | 变更 |
|---|---|
| `imboy/src/ds/message_ds.erl` | 新增 `stamp_sender_device/2`（盖信封顶层）与 `with_sender_device/2`（带进投递帧）；`inject_sender_device/2` **不动**，明文 map payload 的既有行为保持 |
| `imboy/src/logic/websocket_logic.erl` | 新增 `stamp_sender_device/2` 转发（守 Handler→Logic→DS 边界） |
| `imboy/src/api/websocket_handler.erl` | JSON 与 protobuf 两处接入点同时盖章 |
| `imboy/src/logic/msg_c2c_logic.erl` | 投递组装 `assemble_msg` 之后调用 `with_sender_device` |
| `imboy/Makefile` | `e2ee-verify` 模块清单纳入新测试模块 |

**刻意不做**：缺字段时不补 `<<>>` 占位。补空值会让接收侧把
「服务端没提供」误判成「设备 ID 是空串」，两者失败语义不同。

---

## 3. 新增测试

### 3.1 客户端 `imboyapp/test/service/e2ee/production_inbound_frame_gate_test.dart`（11 项）

| 组 | 项数 | 作用 |
|---|---|---|
| 正向可用性门 | 7 | 后端真实投递帧必须可读：text / image / video / audio / file、`from` 整数与字符串两种线上表示、对端多设备 fan-out 只取本机信封 |
| 接线守护（结构级） | 2 | `_handleE2EEMessage` 必须委托 `decryptInboundV3` 且排在 v1/v2 密文形状检查**之前**；`_receiveMessage` 的空 payload early-return 必须对 v3 放行 |
| fail-closed 负向门 | 2 | 服务端不提供 / 伪造 `sender_did` 时必须判 `context_mismatch_sender_did` |

第 3 组同时是 RED 的回归证据：把它改绿的唯一办法是放行，而放行正是被禁止的 fail-open。

### 3.2 服务端 `imboy/test/ds/e2ee_sender_device_envelope_tests.erl`（7 项）

含一项**对照断言** `inject_into_payload_is_ineffective_for_e2ee_test`：
实证旧的 payload 注入对 v3 空串与 v1/v2 密文串确实无效，
证明本次修复针对真实缺口而非重复既有能力；同时断言明文 map 注入不得回归。

---

## 4. 验收命令与结果

```
$ cd imboyapp
$ flutter test test/service/e2ee/production_inbound_frame_gate_test.dart
  All tests passed!   (11)
$ flutter test test/service/e2ee/
  All tests passed!   (332)      # 基线 321 → +11
$ flutter test test/service/
  All tests passed!   (1212)     # 基线 1201 → +11
$ dart analyze lib
  1 issue found.                 # 仅既有 info（component/ui/ios_settings_ui.dart），与 E2EE 无关

$ cd imboy
$ make e2ee-verify
  All 292 tests passed.          # 基线 285 → +7
  === E2EE verify ALL PASSED ===
$ erlfmt --check <5 个改动文件>
  All matched files use erlfmt code style!
```

---

## 5. 逐项复核结论

⚠️ **状态标记一律未擅改**（`22` §3 状态机不含 `PASS -> PARTIAL`，转换路径待人工裁定）。

| 任务 | 现标记 | 本次复核结论 | 依据 |
|---|---|---|---|
| E2EE-012 | `PASS` | **仍不成立 → 建议回退** | 原验收对象在生产 WS 路径未接线；本次虽已补正向可用性门，但 012 的 evidence 自记「改测试对齐 sessionRef」，其判定过程不可采信，须以本文件的门重新签发 |
| E2EE-023 | `PASS` | **维持 PASS**（人工已裁定） | 验收对象是纯 codec，发送侧 `encryptV3` 与接收侧 `_decryptV3Payload` 均有真实生产调用方；仅撤回「无残留风险」一句，已改 `evidence/E2EE-023.md` |
| E2EE-024 | `PASS` | **仍不成立 → 建议回退** | 「100% Mutation Rejection Rate」在拒绝所有消息的实现上恒成立。本次补的正向门是该指标缺失的另一半；补齐后可重新签发 |
| E2EE-025 | `PASS` | **仍不成立 → 建议回退** | counter 语义已由 ADR 26 定案（选项 C）；`session_ref` 修复正确且必要。但 025 evidence 的 PASS 建立在旁路上，且提案 25 §7 第 3、4 项仍未签字 |
| E2EE-029 | `PASS` | **接收侧本次首获实证** | 发送侧 per-device fan-out 原本已证；接收侧 `devices` 解析逻辑存在（`e2ee_service.dart:392-408`），本次「对端多设备 fan-out 只取本机信封」用例首次在生产入口证明其可用 |

---

## 6. 残留风险（**重要，未闭合**）

### 6.1 离线投递路径未修复 —— 同一缺陷仍在

本次修复只覆盖**实时投递**（`msg_c2c_logic` → `assemble_msg` → WS 下发）。
**离线拉取路径仍然缺 `sender_did`**：

- `imboy/src/ds/msg_c2c_ds.erl` `read_msg_filter/3` 的列集为
  `id, payload, from_id, to_id, created_at, server_ts, msg_id, msg_type, e2ee`
  —— **无设备列**；
- `msg_c2c` 表（`priv/migrations/00000005_msg_c2c.up.sql`）**无 sender_did 列**，
  发送者设备标识从未被持久化。

后果：**离线期间收到的 C2C v3 消息，重连拉取后仍会被判
`context_mismatch_sender_did` 而不可读。**

修复需要 DB 迁移（新增列 + staging 写入路径 + 读取列集），属独立 Slice。

#### 6.1.1 ⚠️ A2 范围重估（2026-07-28，同会话领取后立即停下）

领取 A2 后进一步勘察发现，「加一个 DB 列」的预估**不成立**。离线消息与
实时消息走的是**两条不同的解密路径**：

| 路径 | 存储状态 | 解密入口 | v3 是否接线 |
|---|---|---|---|
| 实时 WS | 存**明文**（`_receiveMessage` 解密后再落库） | `message.dart::_handleE2EEMessage` → `decryptInboundV3` | ✅ 已接线（上一会话）|
| 离线拉取 | 存**密文**（decrypt-on-read） | `modules/messaging/infrastructure/message_model_mapper.dart::toTypeMessage()` → `E2EEService.decryptE2EEMessage` | ❌ **未接线** |

`toTypeMessage()`（mapper 第 30–95 行）只有一个解密调用
`E2EEService.decryptE2EEMessage`，**无 `meta_version` 判断、无 `decryptInboundV3`、
无 `fan_out`/`devices` 解析**——与当初 `_handleE2EEMessage` 的缺口结构完全相同。

离线 v3 消息因此有**两个独立断点**：
1. 缺服务端 `sender_did`（需 DB 列）；
2. decrypt-on-read 路径根本不认识 v3（需客户端接线）。

#### 6.1.2 A2-b 实证结果（2026-07-28，已完成）

第 2 点**已从「文件级阅读」升格为实证**，测试
`imboyapp/test/service/e2ee/decrypt_on_read_v3_gap_test.dart`（3 项全绿）。

缺口的本质**与密码学无关**，因此结论不依赖具体协议行为：

> `toTypeMessage()`（mapper:39-43）把 `ciphertext` 实参取自**外层 payload**，
> 而 v3 的外层 payload **恒为空串**——真正的密文在
> `e2ee.devices[<did>].ciphertext` 里。**传错了输入，任何协议都解不出明文。**

测试同时正面证明了这一点：断言 `payload == ''` 且
`e2ee.devices[myDid].ciphertext` 非空。

**对照组通过**：同一行数据改走 `decryptInboundV3` 可读出明文 →
缺口在路径，不在 harness。

harness 说明（诚实记录）：本测试用恒等协议，`decryptE2EEMessage` 返回**空串**
而非抛错；生产的真实 `OlmProtocol.decrypt`（olm_protocol.dart:87-90）会因
v3 fan-out 元数据顶层缺 `peer_uid`/`peer_device_id` 抛错，被 mapper 兜成
`_e2ee_reason: decrypt_failed`。两条都读不出明文，结论一致。

另加**结构守护**：钉死「mapper 当前无 `decryptInboundV3` / 无 `meta_version` 分流」，
接线完成后该组断言必须反转并补正向可用性用例。

##### ⚠️ A2-b 接线被 A2-a 阻塞

实证完成后未继续接线，原因是**接线无法端到端验证为绿**：

`decryptInboundV3` 需要一个帧形状的 map，其中包含 `sender_did`（context
binding #6）。而 `MessageModel` **没有 sender_did 字段**，SQLite 消息表也没有
该列——离线行从服务端落库时无处安放（服务端本身也还没提供，即 §6.1 的断点 1）。

因此正确顺序是 **A2-a 先行**（服务端持久化 + 客户端落库携带），
A2-b 的接线才可能有正向可用性用例。当前若强行接线，只会把失败分类从
`decrypt_failed` 换成 `context_mismatch_sender_did`，不产生可用性收益。

`toTypeMessage()` 的可测性问题（耦合 `ContactRepo`/`UserRepoLocal` 富化取数，
与 `_receiveMessage` 同源）本次**未触发**——因为实证停在解密实参层面，
未走完整 `toTypeMessage()`。接线后的正向用例仍会撞上它，与候选任务 B 同源。

**重估**：A2 = 后端（迁移 + staging + repo/ds 读写）+ 客户端（decrypt-on-read
v3 接线）+ 两侧测试，远超原估的 5–7 文件。建议拆成 A2-a（后端持久化）
与 A2-b（客户端 decrypt-on-read 接线），且 A2-b 可能先要解决
`toTypeMessage()` 的可测性（与候选任务 B 同源）。

#### 6.1.3 A2-a 开工前勘察（已完成，供下一 Slice 直接使用）

| 事实 | 出处 / 影响 |
|---|---|
| `msg_c2c` 表**无**发送者设备列 | `priv/migrations/00000005_msg_c2c.up.sql`；且是 TimescaleDB hypertable，ALTER ADD COLUMN 可用 |
| staging 表 `msg_store_staging` **不由迁移创建**，而是代码里 `CREATE TABLE IF NOT EXISTS` | `src/repo/msg_store_repo.erl:272-306` `ensure_table_exists/0` |
| → 迁移须同时 `ALTER TABLE IF EXISTS ... ADD COLUMN IF NOT EXISTS`（存量部署）**并**改 `ensure_table_exists` 的 DDL（全新安装） | 两处不同步 = 新老部署 schema 分叉 |
| 离线读取列集需同步加列 | `src/ds/msg_c2c_ds.erl` `read_msg_filter/3` |
| `msg_store_repo:stage/10` → `msg_store_ds:stage/10` 需扩参，波及所有 stage 调用方 | arity 变更面较宽，是本 Slice 的主要成本 |
| 下一个迁移序号 = **48** | 现有最大为 `00000047_trust_event_freshness` |

`sender_dtype` **不建议持久化**：context binding 不用它，属展示元数据。
ponytail: 只存 `sender_did`；若将来离线消息的设备类型展示出现缺失，
再加列或从 `user_device` 表反查。

### 6.2 其余

1. **真机双端始终未验证** —— 本次全部结论只在单测层成立。与
   session_ref / message_id / message_type / ADR 26 counter 语义 /
   PFv3 接收侧接线 / E2EE-030 PFS 同属一条真机腿。
2. **接线守护是结构级而非行为级** —— `_receiveMessage` 副作用链未解耦
   （候选任务 B），「从 `processMessage` 进、从 `msg_c2c` 出」的端到端门仍未建成。
   失败卡点见 `E2EE-v3-receive-path-not-wired.md` §7.5（`contact.account_type` 缺列）。
3. **C2G 未覆盖** —— 当前 C2G 走 Megolm v2（非 PFv3），不受本缺陷影响；
   但 C2G 若将来上 PFv3，`msg_c2g_logic` 的投递组装需同步接 `with_sender_device`。
4. **E2EE-012/024/025/029 的状态标记未改**，等待人工决策 2。
5. 未 commit、未 push、未部署。
