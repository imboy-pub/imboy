# ADR 05 — Metadata Versioning

> **状态**：Architecture Freeze（容器结构与字段冻结清单不可单方面变更；演进流程可在向后兼容约束下扩展）
> **关联**：01-overview、02-protocol（ProtocolSuite 三元组）、03-device-identity、04-capability-negotiation、08-threat-model（T7/T9）
> **范围**：E2EE 元数据**容器**（envelope）的版本化、字段分层、演进与守护。与 ADR 02 的关系是「信封 vs 信封内容」——02 定义信封里装什么协议套件，本 ADR 定义信封本身怎么演进。

---

## 1. 决策（Decision）

E2EE 元数据采用**两层结构 + 单调 `meta_version`**：

```
e2ee = {
  // —— envelope（跨协议通用、版本化、稳定）——
  meta_version: 2,                  // 容器版本号，本 ADR 引入，单调递增
  e2ee: true,                       // 顶层「是否加密」布尔（冻结，服务端唯一消费点）
  e2ee_ver: 2,                      // legacy 协议族号（冻结，见 §5）
  e2ee_suite: "MEGOLM.V1",          // legacy 套件字符串（ADR 02 fromMetadata 读取）
  protocol: "megolm", version: 1,   // v2 三元组（ADR 02 引入，与 e2ee_suite 双写）
  created_at: 1721300000000,        // 客户端创建时间戳（ms，T9 rollback 辅助检测）
  // —— payload（协议私有、可变）——
  session_id: "base64...",          // Megolm 私有
  gid: "01J...",                    // Megolm 私有（群域；缺省=scope='c2c'）
  message_index: 42,                // Megolm 私有（天然 counter，见 §7）
  // Olm 私有：peer_uid / peer_device_id / message_type
  // RSA 私有：nonce / keys[{did,kid,wrap_alg,ek}]
}
```

核心决策四条：

1. **envelope / payload 分层**：跨协议通用的字段（版本号、套件标识、时间戳）放 envelope；协议私有字段（session_id、message_type、nonce、keys）放 payload。新增协议不污染 envelope，新增 envelope 字段不破坏协议。
2. **引入 `meta_version`**：作为容器的单调版本号，独立于 `e2ee_ver`。`meta_version` 管「信封长什么样」，`e2ee_ver` 管「里面装哪族协议」——两者解耦，`e2ee_ver` 永久冻结在 1/2，`meta_version` 可一路涨到 3/4/…而不动存量。
3. **双写过渡**：新客户端在过渡期同时写 v1（`e2ee_ver`+`e2ee_suite`）与 v2（`meta_version`+`protocol`/`version`）字段；单写期退出由可量化指标驱动（§4.3）。
4. **未知字段忽略（forward compat）**：解析 envelope 时，任何未识别字段一律保留原值透传给协议层，**绝不抛错、绝不裁剪**。这是未来加字段不破坏老客户端的硬保证。

---

## 2. 问题陈述（Problem Statement）

当前 `e2ee_ver` 只有 `1`（RSA 旧套件）和 `2`（Megolm/Olm 新套件）两个值，且与 `e2ee_suite` 字符串**语义冗余耦合**：

| `e2ee_ver` | `e2ee_suite` | 含义 |
|---|---|---|
| 1 | `"RSA-OAEP-256+AES-256-GCM"` | RSA 单聊 |
| 2 | `"MEGOLM.V1"` | Megolm 群聊/单聊 |
| 2 | `"OLM.V1"` | Olm 单聊 PFS |

四个具体问题：

1. **`e2ee_ver` 复用为「套件族号」**：Olm 与 Megolm 共用 ver=2，加 MLS 时无处可放，版本号失去单调语义。
2. **双重事实源**：`e2ee_ver` 与 `e2ee_suite` 必须一致，但服务端不校验（§8），冲突时行为未定义。
3. **加字段即破坏存量**：未来要加 message counter（T7）、capability snapshot、forwarding token、`message_index`（T9 乱序检测），老客户端的 `decryptE2EEMessage` 见到陌生字段虽不崩，但新字段语义对老端不可见，安全特性形同虚设。
4. **无容器版本号**：无法区分「v1 信封」与「v2 信封」，迁移期无法做兼容性 gating。

ADR 02 已把「套件字符串」升级为 `ProtocolSuite(protocol, version, cipher)` 三元组，解决了**信封内容**的标识；本 ADR 回答信封**本身**怎么演进、字段怎么分层、新老共存多久。

---

## 3. 元数据容器分层

### 3.1 envelope 层（跨协议通用、版本化、稳定）

| 字段 | 类型 | 含义 | 冻结 |
|---|---|---|---|
| `meta_version` | int | 容器版本号；v1 省略=隐式 1，v2=2 | 是 |
| `e2ee` | bool | 顶层「是否加密」；服务端 `encrypted_message_body` 唯一消费点 | 是 |
| `e2ee_ver` | int (1\|2) | legacy 协议族号；冻结后不再扩值，新协议走 `protocol`/`version` | 是 |
| `e2ee_suite` | string | legacy 套件字符串；ADR 02 `fromMetadata` 读取，过渡期与三元组双写 | 是 |
| `protocol` | string | v2 三元组之一；`"olm"`/`"megolm"`/`"rsa"` | 是 |
| `version` | int | v2 三元组之二；协议自己的版本 | 是 |
| `cipher` | string | v2 三元组之三（ADR 02 §3.1）；可选写入 | 否 |
| `created_at` | int (ms) | 消息级创建时间戳；T9 rollback 检测、乱序诊断辅助 | 否 |

**为什么 `created_at` 放 envelope**：T9 rollback 检测需跨协议统一比较「这条消息是不是太旧」，若放 payload 则各协议各写各的时间戳无法横向比对。注意与业务层 `server_ts` 区分：`created_at` 是发送方客户端时间（密码学诊断），`server_ts` 是服务端落库时间（业务排序）。

### 3.2 payload 层（协议私有、可变）

各协议自己的字段，envelope 不解释、Registry 层不路由（ADR 02 §7.3 已列）。本 ADR 只声明归属。

| 协议 | payload 字段 | 来源 |
|---|---|---|
| Olm | `peer_uid`, `peer_device_id`, `message_type`(0=prekey,1=normal) | `e2ee_service.dart:313-320` |
| Megolm | `session_id`, `gid`(可缺省→scope='c2c'), `message_index` | `e2ee_service.dart:332-344` |
| RSA legacy | `nonce`, `keys[{did,kid,wrap_alg,ek}]` | `e2ee_service.dart:275-278` |

**为什么 payload 不进 envelope**：这些字段只有对应协议的 `E2eeSessionProtocol.decrypt` 会读（ADR 02 §2.1），放 envelope 会迫使 Registry 理解协议私有语义。payload 由协议插件自己写自己读，envelope 完全不感知。

---

## 4. `meta_version` 演进规则

### 4.1 版本定义

| `meta_version` | 信封形态 | 字段组合 |
|---|---|---|
| 1（隐式，省略） | 现有生产格式 | `e2ee_ver` + `e2ee_suite` 字符串（+ payload） |
| 2（本 ADR 引入） | 新格式 | `meta_version:2` + `protocol`/`version`/`cipher` 三元组 + `created_at`（+ payload） |

**为什么 v1 隐式**：已落库的数亿条消息没有 `meta_version` 字段，强制回填不可行。解析器规定「缺省=1」，零数据迁移即可识别存量。

### 4.2 双写期（Dual-Write Phase）

新客户端发送消息时，envelope **同时写** v1 与 v2 字段：

```dart
metadata = {
  // v1 字段（兼容老客户端）
  'e2ee':      true,
  'e2ee_ver':  2,
  'e2ee_suite': suite.wire,        // 'OLM.V1' / 'MEGOLM.V1' / 'RSA-OAEP-256+AES-256-GCM'
  // v2 字段（新客户端优先读）
  'meta_version': 2,
  'protocol':    suite.protocol,
  'version':     suite.version,
  'cipher':      suite.cipher,
  'created_at':  DateTime.now().millisecondsSinceEpoch,
  // payload（协议私有）
  ...protocolPayload,
};
```

解析侧（ADR 02 `ProtocolSuite.fromMetadata`）优先读 v2 三元组，缺省时回退 v1 字符串——已在 ADR 02 §3.2 落定，本 ADR 不重复。

### 4.3 双写期判定与退出条件（量化，非空话）

**进入双写期**：v2 客户端发布之日起（路线图 B.2 阶段，见 01-overview §3）。

**退出双写期（停止写 v1 字段）需同时满足**：

| 指标 | 阈值 | 数据来源 |
|---|---|---|
| 最近 30 天活跃设备中「能理解 v2 三元组」占比 | ≥ 95% | 客户端心跳上报的 `app_version` + capability snapshot |
| 最近 30 天入站解密走 v2 路径（读 `protocol`/`version`）占比 | ≥ 99% | 客户端解密路径埋点 |
| 距离最后「仅写 v1」客户端版本发布时间 | ≥ 90 天 | 发版记录 |

**时间线建议**（基于 B.2→B.5 灰度节奏）：

```
B.2  v2 客户端发布            ── 进入双写期（v1+v2 同写）
B.5  灰度完成（预计 +8 周）   ── 指标 1 达 95%
+12 周（冷却期）              ── 指标 2/3 达标，发布「停写 v1」小版本
+12 周之后                   ── 单写期：新消息只写 v2；v1 解析能力永久保留（§5 冻结）
```

**为什么 95%/99%/90 天**：95% 覆盖绝大多数活跃用户；99% 解密成功率保证残留 1% 老端不会因停写而大面积解密失败；90 天冷却期确保用户至少经历一个发版周期升级。

### 4.4 单写期之后

新消息 envelope 只含 `meta_version`/`protocol`/`version`/`cipher`/`created_at` + payload；`e2ee_ver`/`e2ee_suite` 不再写入新消息，但**解析能力永久保留**（§5）。升级 `meta_version` 到 3 时，重复本节流程（双写 v2 与 v3 字段，直到 v2 退役）。

---

## 5. 字段冻结清单（Frozen Field Registry）

下列字段一旦发布到生产，**不得改名、不得改语义、不得改类型**。变更必须新建 `NN-supersedes-05.md` 走 01-overview §5 流程，并保留旧字段解析能力。

| 冻结字段 | 冻结语义 | 冻结原因 |
|---|---|---|
| `e2ee` | bool，非空 map 即视为加密（服务端 `imboy_policy.erl:197` 唯一消费点） | 改语义直接破坏 required 模式判定 |
| `e2ee_ver` | int，取值 1（RSA 族）/ 2（Olm+Megolm 族）；不再扩值 | 生产数据已固化 1/2；扩值破坏老客户端 |
| `e2ee_suite` | string，取值 `"OLM.V1"` / `"MEGOLM.V1"` / `"RSA-OAEP-256+AES-256-GCM"` | ADR 02 `fromMetadata` 字节级匹配；改字符串=数据迁移 |
| `protocol` | string，小写协议名（`olm`/`megolm`/`rsa`/`mls`） | ADR 02 Registry 注册表 key |
| `version` | int，协议自己的版本号 | ADR 02 三元组之一 |
| `session_id` | base64 string，Megolm/Olm 会话 ID | 协议层路由 key |
| `peer_uid` / `peer_device_id` | string，Olm 对端标识 | `e2ee_service.dart:313-314` 直接消费 |
| `message_type` | int (0\|1)，Olm prekey/normal | `e2ee_service.dart:317` 直接消费 |
| `nonce` | base64，RSA GCM IV | `e2ee_service.dart:364-365` 比对 |
| `keys` | `[{did,kid,wrap_alg,ek}]` 数组，RSA per-device wrapped key | `e2ee_service.dart:372-377` 索引 |

**为什么单独维护冻结清单**：这些字段是跨版本/跨客户端的「公共契约面」，任何一个改名都会让某个版本组合的客户端解密失败。冻结清单让 PR review 一眼可见「这个字段动不得」。

---

## 6. 新增字段流程（Checklist）

未来加任何新字段（capability snapshot、forwarding token、padding policy…）必须走完下列 checklist，缺一项 PR 不予合并：

| 步骤 | 检查项 | 通过标准 |
|---|---|---|
| 1 | 归层判定：envelope（跨协议通用）还是 payload（协议私有）？ | 协议私有→进 payload，不升 `meta_version`；跨协议→进 envelope |
| 2 | 向后兼容：老客户端解析含新字段的 metadata，能否仍正常解密？ | 必须能（§1 决策 4「未知字段忽略」）；CI 加 `legacy_parse_unknown_field` 测试 |
| 3 | 默认值：新字段缺省时的行为是否确定？ | 必须有明确默认（如 `forwarding_token` 缺省=不可转发） |
| 4 | 是否升 `meta_version`？ | 仅当新字段改变 envelope 路由/版本语义时才升；纯附加信息字段不升 |
| 5 | 冻结评估：该字段是否应加入 §5 清单？ | 一旦写入生产消息即默认冻结，改名走 supersedes 流程 |
| 6 | 双写评估：若升 `meta_version`，是否需双写期？ | 是→按 §4.3 制定时间线；否→直接写入 |
| 7 | 服务端契约：是否要求服务端「看懂」该字段？ | 若是，违反 §8 不透明透传契约，必须先发新 ADR |

**为什么强制 checklist**：E2EE 字段一旦写入生产消息就永久存在（消息不可改写），事后发现设计错误只能再发新字段并存。checklist 把成本前置到 PR 阶段。

---

## 7. Message Counter / Replay Protection（T7/T9）

威胁 08-T7（Malicious Client 重放/乱序/复制）与 T9（Rollback）要求消息可排序、可去重。本 ADR 分工：

### 7.1 counter 放哪层：payload，不是 envelope

**决策**：counter 字段（`message_index`、ratchet index）放 **payload 层**，由各协议插件自己维护；envelope 不引入跨协议统一 counter。

**为什么不放 envelope**：Olm 的 ratchet index 与 Megolm 的 `message_index` 是协议**密码学结构原生**的（ratchet 每步推进、Megolm 每次 encrypt 自增），复用零额外成本；envelope 再造统一 counter 等于三份事实源。且跨协议 counter 语义不可比（Olm 是 per-session、Megolm 是 per-sender-in-session、RSA 无 counter）。

### 7.2 协议层天然 counter

| 协议 | 天然 counter | 来源 | 防御 |
|---|---|---|---|
| Olm | Double Ratchet chain index（密文隐含） | 协议内置 | T7 重放：相同 ratchet state 下重复密文解密失败 |
| Megolm | `message_index`（每次 outbound encrypt 自增，写入 payload） | `message_index` 字段 | T7 乱序：客户端记录每 session 的 `highest_seen_index`，更小则拒收 |
| RSA legacy | 无 counter | — | 依赖 §7.3 应用层 msg_id 去重（RSA 已冻结 decrypt-only） |

**为什么 Megolm 显式写 `message_index` 而 Olm 不写**：Megolm 的 index 明文可见（receiver 用它定位 session key），天然在 payload；Olm 的 ratchet index 密文隐含（解密成功即证明顺序正确），无需在 metadata 暴露。

### 7.3 应用层 msg_id 去重（已实现，分工边界）

服务端 `msg_c2c_repo:write_msg` 对 `msg_id` 走 `ON CONFLICT (msg_id) DO NOTHING`（已落地），防御「同一密文复制投递」。三层分工：

| 防御层 | 防御的攻击 | 实现 |
|---|---|---|
| 应用层 `msg_id` 去重 | 服务端复制投递同一消息 | `ON CONFLICT DO NOTHING`（已实现） |
| 协议层 counter（payload） | 攻击者重放/乱序**同一 session 内**的密文 | Olm ratchet / Megolm message_index |
| envelope `created_at`（§3.1） | T9 rollback：服务端返回过老的 session 元数据 | 客户端诊断辅助，非硬拒收 |

**为什么三层各管一段**：`msg_id` 只能防「字节级相同的消息」，防不住「攻击者用旧 session key 加密的新密文」；协议层 counter 防得住后者但跨设备/跨 session 失效；`created_at` 提供最后的人类可见诊断信号。无单一机制覆盖所有路径。

---

## 8. 服务端契约（零语义消费强化）

服务端对 e2ee 字段的契约**不变**（ADR 02 §6 已声明的「不透明 map 透传」），本 ADR 强化**字段保留策略**：

| 服务端职责 | 具体行为 | 守护 |
|---|---|---|
| 全链路不解密/不解析 | `msg_c2c_logic` → `msg_store_ds:stage` → `msg_store_worker:do_write` 不读 e2ee 内部字段 | ADR 02 §6 grep 守护 |
| jsonb 列原样存 | `msg_c2c_repo:write_msg` 把 e2ee 作为 jsonb 列入库，无 schema 校验 | `msg_c2c_repo.erl:116/150/195/449` |
| **不裁剪未知字段**（本 ADR 新增） | 服务端**禁止**任何「只保留已知字段、丢弃其他」的 jsonb 清洗逻辑 | §9 `server_preserves_unknown_e2ee_field` |
| 唯一语义消费点 | `imboy_policy:encrypted_message_body/3`（`imboy_policy.erl:197-201`）：`is_map(E2EE) andalso map_size(E2EE) > 0 andalso Payload =/= <<>>` 即视为加密 | 行号守护 |

**为什么强调「不裁剪未知字段」**：未来客户端加新字段（capability snapshot 等），若服务端有「字段白名单」逻辑，会把新字段在落库时悄悄丢掉——这是一种**向前兼容性攻击**（服务端无意中破坏未来版本）。jsonb 列必须 byte-for-byte 保留客户端写入的所有字段。

**唯一允许的「语义消费」**：`imboy_policy.erl:184-200`，且只看 `e2ee` 是不是非空 map，不看内部任何字段。任何让服务端「读懂」具体字段（如读 `protocol` 做路由）的提案，违反本 ADR 与 ADR 02，必须先发新 ADR。

---

## 9. 守护测试要求

下列测试必须存在，CI 强制运行。缺失任一项视为本 ADR 未落地。

### 9.1 legacy 解析（ADR 01 §6 验收标准 1）与双写兼容

| 测试 ID | 用例 | 验证点 |
|---|---|---|
| T-05-01 | 3 种 legacy metadata（`e2ee_ver=1` RSA / `e2ee_ver=2` Megolm / `e2ee_ver=2` Olm）经新解析器解密，结果与 v1 `decryptE2EEMessage` 一致 | §4.1 v1 隐式识别 |
| T-05-02 | legacy metadata 缺省 `meta_version` 字段，解析器识别为 v1，不抛错 | §4.1 缺省=1 |
| T-05-03 | 新客户端发送的 metadata 同时含 `e2ee_suite`（v1）与 `protocol`/`version`（v2），字段值一致 | §4.2 双写 |
| T-05-04 | 仅懂 v1 的老客户端解析双写 metadata，走 v1 路径解密成功 | §4.2 向后兼容 |
| T-05-05 | 仅懂 v2 的新客户端解析双写 metadata，优先走 v2 路径 | ADR 02 §3.2 优先级 |

### 9.2 未知字段忽略（§1 决策 4，§6 步骤 2）

| 测试 ID | 用例 | 验证点 |
|---|---|---|
| T-05-06 | metadata 含虚构字段 `future_field: {...}`，解析器不抛错、不裁剪，原值透传给协议插件 | §1 决策 4 |
| T-05-07 | Erlang EUnit：客户端写入含未知字段的 e2ee map，`msg_c2c_repo:write_msg` 落库读回，未知字段原样存在 | §8 不裁剪契约 |

### 9.3 counter 单调（§7，T7/T9）与时间戳（§3.1）

| 测试 ID | 用例 | 验证点 |
|---|---|---|
| T-05-08 | Megolm 入站消息 `message_index` 小于本端 `highest_seen_index` 时，解密拒收并告警 | §7.2 乱序检测 |
| T-05-09 | Olm 同一 ratchet state 下重放旧密文，解密失败（协议内置） | §7.2 Olm counter |
| T-05-10 | 同一 `msg_id` 重复入库，第二次 `ON CONFLICT DO NOTHING` 无效写入 | §7.3 应用层去重 |
| T-05-11 | 入站消息 `created_at` 与本端时钟偏差超阈值（如 7 天），客户端记诊断日志（不硬拒收） | §7.3 created_at 定位 |

---

## 10. 与其他 ADR 的关系

| ADR | 本 ADR 的依赖点 | 对方对本 ADR 的约束 |
|---|---|---|
| **01-overview** | §5 冻结清单是 01 §5「不可单方面变更冻结项」中「e2ee 元数据的 `e2ee_ver` 字段语义」的具体化 | 01 §5 变更流程适用于本 ADR 冻结项 |
| **02-protocol** | `ProtocolSuite(protocol, version, cipher)` 三元组 + `fromMetadata` 是本 ADR envelope v2 字段的来源 | 02 §3.2 legacy 字符串解析矩阵不得删除（本 ADR §4.1 v1 隐式识别依赖） |
| **03-device-identity** | payload 中 Olm 的 `peer_uid`/`peer_device_id` 引用 03 的 device 模型 | device identity 版本单调（03）与本 ADR `created_at` 共同防御 T9 |
| **04-capability-negotiation** | 协商选定的 `ProtocolSuite` 决定 envelope v2 三元组的值 | 04 协商结果不可降级约束不影响本 ADR 容器结构 |
| **08-threat-model** | T7（重放/乱序/复制）→ §7 counter 分工；T9（rollback）→ §3.1 `created_at` + §4 `meta_version` 单调 | 08 §4 矩阵「消息重放/乱序/复制」「Megolm session rotate 单调」由本 ADR §7、§9 落地 |

**冲突仲裁**：本 ADR 冻结后，§5 冻结清单任何字段变更、§4 `meta_version` 演进规则的变更，必须走 01 §5 的「supersedes」流程并人工 review 签字。`meta_version` 数值空间、envelope/payload 分层边界、未知字段忽略策略三项为**不可单方面变更**冻结项。

---

## 11. 决策摘要（一页速览）

| 决策点 | 选择 | 一句话理由 |
|---|---|---|
| 容器结构 | envelope（通用）+ payload（协议私有）两层 | 加协议不污染 envelope，加 envelope 字段不破坏协议 |
| 容器版本号 | 引入 `meta_version`，与 `e2ee_ver` 解耦 | `e2ee_ver` 已被生产固化，新轴干净可涨 |
| v1 兼容 | `meta_version` 缺省=1，零数据迁移 | 存量消息不可改写 |
| 过渡策略 | v1+v2 双写，95%/99%/90 天达标后停写 v1 | 灰度期旧端可解密，退出条件可量化 |
| 字段冻结 | §5 清单一旦发布不可改名/改语义 | 跨版本公共契约面，PR review 可执行 |
| counter 分层 | payload 层（协议原生），envelope 不引入 | Olm ratchet / Megolm message_index 已是天然 counter |
| 去重分工 | 应用层 `msg_id` ON CONFLICT + 协议层 counter + envelope `created_at` | 无单一机制覆盖所有攻击路径 |
| 服务端契约 | 不透明透传 + 不裁剪未知字段 | 防向前兼容性攻击（未来字段被服务端悄悄丢掉） |
| 新字段流程 | §6 七步 checklist | 字段一旦写入生产即永久存在，成本必须前置 |
