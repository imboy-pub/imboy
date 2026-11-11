# chat_history_bundle 消息合同（RUN chat-multi-20261111 · 冻结）

> 定位：多选消息「合并转发」快照类型 `chat_history_bundle` 的**权威合同**（三端对照基准）。
> 信封与通道规范见 [ws-protocol-contract.md](./ws-protocol-contract.md) 与
> [websocket-api-2.md](./websocket-api-2.md)；本文只定义 bundle 内容合同。

最后更新：2026-11-11（W1-D 冻结 v1；W2-H 补充 §3 注入跳过与 §3.1 尺寸口径说明）

---

## 1. 类型登记

```text
msg_type      = chat_history_bundle   （JSON-only 通道类型）
bundle_version = 1
```

- JSON-only 类型：已在 `imboy_codec:is_json_channel_type/1` 登记（与
  `talk_record_card` / `agent_task` 同列）。JSON 连接 msg_type 原样透传；
  protobuf 连接自动回退 JSON 帧（`pb_lossless` → `CONTENT_TYPE_UNSPECIFIED`
  → JSON fallback），**无 proto 变更**。
- 传输形态：标准 v2.0 信封（`type` = `C2C` / `C2G`），`msg_type` 与 `payload`
  在顶层；走既有 staging → QoS 投递 → offline envelope 链路，
  **无新数据库表 / 无迁移 / 无 HTTP API**。
- 使用转发时快照：详情页不回查原会话。每个**目标会话单独构建** bundle；
  附件必须重新上传，或按新 message ID、目标会话和转发者重新封装。

## 2. payload 形态（v1）

```json
{
  "bundle_version": 1,
  "entries": [
    { "msg_type": "text",  "payload": { "text": "..." } },
    { "msg_type": "image", "payload": { "url": "...", "w": 1, "h": 1 } }
  ]
}
```

| 字段 | 类型 | 约束 |
|------|------|------|
| `bundle_version` | integer | 必须为 `1`；其他值/缺失拒绝 |
| `entries` | array | 必须为 list，`1..50` 条（0 条与 51 条均拒绝） |
| `entries[].msg_type` | binary | 非空字符串（原消息内容类型快照；服务端不深解） |
| `entries[].payload` | any | 必须存在（原消息 payload 快照；服务端不深解业务类型） |

服务端是**透传管道**：只做形态 / 条目数 / 尺寸守护，不解析 `entries[].payload`
的业务语义，不做内容审查（禁止字段清单由客户端 sanitizer 在构建侧强制，见 §4）。

## 3. 边界与限制

| 限制 | 值 | 执行点 |
|------|----|--------|
| payload 序列化上限 | `256 KiB`（256×1024 字节，`byte_size(jsone:encode(payload))`，≤ 含等号） | `chat_history_bundle_ds:validate_payload/1`，落库前拒绝 |
| entries 条数 | `1..50` | 同上 |
| bundle_version | `= 1` | 同上 |
| 错误码（稳定原子） | `invalid_bundle_version` / `bundle_entries_out_of_range` / `bundle_payload_too_large` / `invalid_bundle_shape` | WS 校验错误帧 `payload.reason` 为同名 binary |

校验挂点：`message_ds:validate_content_message/2` →（仅 `msg_type =
chat_history_bundle` 且**明文**）→ `chat_history_bundle_ds:validate_payload/1`，
发生在 decode → convert → **validate** → inject → route → store 链条的
validate 步骤，即 staging 落库与广播之前。

W2-H 补充：bundle 的 payload 在 inject 步骤**跳过** sender 设备注入
（`websocket_handler:inject_payload_sender_device/3`）——注入会把 §4 禁止字段
`sender_did`/`sender_dtype` 种进 payload 顶层，且发生在 256KiB 校验之后，
恰边界 bundle 落库/出网会实际超限；发送设备信息由信封顶层
`stamp_sender_device/2` 提供（ADR15 context binding 比对信封层，绑定不丢失）。
其他 msg_type 的 payload 注入行为不变。

### 3.1 尺寸口径补充（W2-H 审查 LOW）

- **E2EE 包裹的 bundle 受 E2EE 信封上限约束**：§3 的 256KiB 是明文 payload
  口径；走 E2EE 时 bundle 位于密文内部，外层受 `e2ee_envelope_max_bytes`
  （默认 **1 MiB**，`message_ds:validate_e2ee_envelope/1`）约束。密文膨胀
  （nonce/认证头/base64）与 v3 **per-device fan-out**（`e2ee.devices` 按接收
  设备逐份加密）会放大体积——名义 256KiB 以下的 bundle 也可能触发
  `e2ee_envelope_too_large`；客户端构建大 bundle 时应按目标设备数预留
  膨胀余量。
- **字节口径权威性**：以服务端 `jsone:encode/1`（`native_utf8`）后的
  `byte_size` 为准；客户端序列化（浮点最短表示、`\uXXXX` 转义规范化等）
  可能与该口径存在数字节残差，恰边界判定两端可漂移。建议客户端预留
  安全边（如 **64KiB**，即按 ≤192KiB 自查），避免客户端自判合法、服务端
  按 256KiB 拒绝的边界抖动。

## 4. 禁止字段清单（客户端构建合同）

payload 及任意嵌套层**禁止**包含以下字段（泄露原会话上下文 / 服务端内部标识 /
解密材料；由客户端 Bundle Sanitizer 在构建时剔除，服务端不做内容扫描）：

```text
原消息 id            （original message id / msg_id / message_id 快照）
peer_id
conversation_uk3
sender_did
sender_dtype
原 E2EE 外层信封     （e2ee / devices / protected_header 等原信封嵌套）
原 content key / descriptor
burn_read_at
服务端内部主键       （数据库 TSID 行 id 等）
原会话成员列表       （member list / receivers 等）
```

服务端对应责任（W2-H）：入站管道对 bundle payload **不做** sender 设备注入，
明文 bundle 落库/出网形态即客户端构建形态（见 §3 W2-H 补充）。

## 5. E2EE 语义

- 服务端**不解密**：bundle 走 E2EE 时，bundle payload 位于密文内部，外层
  `payload` 不透明（v3 恒为空串）。
- 服务端只复用现有**外层信封形态校验**（`message_ds:validate_e2ee_envelope/1`：
  尺寸上界 + v3 必填字段形态），通过后原样透传，不重建、不裁剪。
- 明文 bundle 才做 §3 的 DS 层合同校验；E2EE bundle 的合同自查由客户端在
  加密前完成（同 §4 责任方）。

## 6. 通道与离线行为

- protobuf 连接：bundle 帧自动回退 JSON（text 帧 / v2 帧 JSON payload），
  与 `talk_record_card` 同机制、同四象限保障。
- offline envelope（`message_ds:offline_envelope/2`）：`msg_type` /
  `payload` / `e2ee` 原样透传，无 bundle 专属分支。

## 7. 服务端实现索引（W1-D）

| 组件 | 位置 |
|------|------|
| JSON-only 登记 | `src/lib/imboy_codec.erl` `is_json_channel_type/1` |
| 合同校验 DS | `src/ds/chat_history_bundle_ds.erl` `validate_payload/1` |
| 入站挂点 | `src/ds/message_ds.erl` `validate_content_message/2`（落库前） |
| 传输四象限测试 | `test/lib/imboy_codec_tests.erl` |
| 合同 / E2EE / 离线测试 | `test/ds/chat_history_bundle_tests.erl` |
