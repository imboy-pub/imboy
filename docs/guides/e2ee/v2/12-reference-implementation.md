# ADR 12 — Reference Implementation

> **状态**：Architecture Freeze
> **用途**：明确哪个平台是 E2EE v2 规范实现（normative），哪些是衍生实现（derivative）。避免「Flutter 这么做、Web 那么做、谁是对的」争议。

---

## 1. 决策

| 平台 | 角色 | 含义 |
|---|---|---|
| **Flutter（imboyapp）** | **Normative Reference**（规范实现） | 协议行为、字段格式、错误处理以 Flutter 实现为准；其他平台与之行为不一致时，**Flutter 是对的** |
| **Web SDK（sdk-e2ee-webcrypto）** | Derivative（衍生实现） | 必须与 Flutter 行为一致；分歧以 Flutter 为准 |
| **JS SDK（imboy-sdk-js）** | API Contract Only（仅契约） | 仅定义 HTTP/Wire 契约，不含加密实现 |
| **后端 Erlang（imboy）** | Protocol-Agnostic（协议无关） | 不参与协议语义，零密码学，无需与客户端「对齐」 |

---

## 2. 为什么选 Flutter 作为规范实现

1. **完整性**：Flutter 端覆盖 Olm + Megolm + RSA legacy 三套件，是协议覆盖最全的客户端；
2. **密码学库权威性**：使用 vodozemac 0.5.0（Matrix 基金会维护），与 Web 端的 vodozemac-js 同源，确保语义一致；
3. **首发平台**：移动端是 IM 的主战场，最先落地、最先暴露协议边界情况；
4. **测试覆盖**：Flutter 端有最完整的单元 + 集成测试（`imboyapp/test/`）。

---

## 3. 规范化的具体内容

以下以 Flutter 实现为「对的」，其他平台必须对齐：

### 3.1 字段格式

| 字段 | 规范值（Flutter 定义） | Web 必须一致 |
|---|---|---|
| `e2ee_suite` legacy 字符串 | `'OLM.V1'` / `'MEGOLM.V1'` / `'RSA-OAEP-256+AES-256-GCM'` | ✅ |
| `e2ee_ver` | `1`（RSA）/ `2`（Megolm/Olm） | ✅ |
| Olm 元数据字段名 | `peer_uid` / `peer_device_id` / `message_type` | ✅ |
| Megolm 元数据字段名 | `session_id` / `gid` / `scope` | ✅ |
| ciphertext 编码 | `base64(nonce).base64(ciphertext)`（RSA）/ base64（Megolm/Olm） | ✅ |
| keys 数组结构 | `[{did, kid, wrap_alg, ek}]` | ✅ |

### 3.2 协议行为

| 行为 | 规范（Flutter 定义） | Web 必须一致 |
|---|---|---|
| OTK 低水位补传阈值 | `_otkLowWaterMark = 5`，目标 `_otkTargetCount = 50` | ✅ |
| pickle 加密密钥派生 | 设备级 secret（FlutterSecureStorage） | ⚠️ Web 用 StorageProvider，语义等价即可 |
| Megolm rotate 触发条件 | 成员/设备集合变化 | ✅ |
| Olm session 建立时机 | 首条消息 claim prekey + createOutboundSession | ✅ |
| 解密失败兜底 | `_decryptFailedPayload` 标记 `[加密消息]` | ✅ |

### 3.3 错误处理

| 场景 | 规范行为（Flutter 定义） |
|---|---|
| OTK 耗尽 | 降级 fallback key，再耗尽则拒发 |
| 解密失败（密钥不匹配/数据损坏） | 不抛异常崩溃，返回 `_e2ee_failed` 标记 payload |
| 对端密钥变更（TOFU） | S2C 通知触发 UI 告警，不自动信任新密钥 |
| 协议套件不识别 | `ProtocolSuite.parse` 返回 unknown → 尝试 legacy RSA → 失败则 `[加密消息]` |

---

## 4. Web SDK 的衍生约束

Web SDK（B.4 阶段实现）必须：
1. 复用 Flutter 的字段格式（§3.1），通过**互操作测试**守护（`envelope.roundtrip.test.ts`）；
2. 复用 Flutter 的协议行为（§3.2），关键阈值与时机一致；
3. 复用 Flutter 的错误处理（§3.3），用户感知一致；
4. 使用 **vodozemac-js**（与 Flutter 的 vodozemac 同源），确保密码学语义一致；
5. 互操作测试 fixture 与 Flutter 共享同一组输入向量。

---

## 5. 分歧解决流程

当 Flutter 与 Web 实现出现行为分歧时：

1. **确认是否为规范内容**（§3）：若是，Web 必须对齐 Flutter；
2. **若非规范内容**（如 UI 交互、性能优化）：平台可自由实现；
3. **若 Flutter 行为本身有 bug**：修复 Flutter + 同步 Web，并在本 ADR 追加「勘误记录」；
4. **若规范本身需演进**：走 01 §5 superseding ADR 流程，同步更新本文件 §3。

---

## 6. 后端的角色（协议无关）

后端 Erlang **不是**规范实现，也**无需与客户端对齐协议语义**：
- 后端对 e2ee map 不透明透传（ADR 02 §6）；
- 后端不参与协议协商（ADR 04 §7）；
- 后端不持有任何协议特定的状态（除 OTK/identity/prekey 的存储，这些是数据存储而非协议语义）。

后端的「规范」是**零密码学契约**（ADR 07 §6）：不调用 E2EE payload 的 decrypt、不解析字段语义、不裁剪未知字段。

---

## 7. 测试策略

| 测试类型 | 守护内容 | 所在位置 |
|---|---|---|
| **互操作 fixture** | 共享输入向量，Flutter 加密 → Web 解密（反之亦然） | `imboyapp/test/fixtures/e2ee_vectors.json` + `sdk-e2ee-webcrypto/test/fixtures/` |
| **规范守护测试** | Flutter 实现的 §3 字段/行为/错误处理 | `imboyapp/test/service/e2ee_spec_*_test.dart` |
| **Web 对齐测试** | Web 实现与规范一致 | `sdk-e2ee-webcrypto/test/spec_conformance_test.ts` |

---

## 8. 与其他 ADR 的关系

- **02-protocol**：ProtocolSuite 与接口签名以 Flutter 实现为规范；
- **05-metadata**：双写期字段格式以 Flutter 为规范；
- **07-storage**：Web 的 StorageProvider 是 Flutter FlutterSecureStorage 的语义等价物（非字段级一致）；
- **11-backward-compatibility**：兼容矩阵以 Flutter 各版本行为为基准。

---

## 9. 未来演进

当第二个平台（如原生 iOS/Android 重写、或 Rust 核心）出现时：
1. 若新平台覆盖更全的协议套件或更权威的密码学库，可申请成为 normative reference；
2. 走 01 §5 superseding ADR 流程，明确切换条件与过渡期；
3. 切换后旧 reference 降级为 derivative。

**本轮不切换**：Flutter 在可见未来保持 normative 地位。
