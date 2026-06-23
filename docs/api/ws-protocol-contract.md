# WebSocket 协议契约（三端镜像基准）

> 定位：三端（Erlang 后端 / Flutter / JS SDK）协议常量的**单一对照基准**，防止漂移。
> 本文是速查契约表，不重复 API 用法（见 [websocket-api-2.md](./websocket-api-2.md)）
> 与帧格式设计（见 `../../../.claude/plans/imboy-frame-protocol.md`）。
>
> 最后更新：2026-06-23

---

## 1. V2 二进制帧格式

```
[Magic:2 = 0x4942 "IB"] [Ver:1 = 2] [Flags:1] [Type:1] [PayloadLen:4 BE] [Payload:N]
```

头部固定 **9 字节**，PayloadLen 为 big-endian uint32，最大载荷 **16 MiB**。

## 2. 帧类型枚举（三端必须一致）

| 区间 | 名称 | 值 |
|------|------|----|
| 控制帧 | heartbeatPing / heartbeatPong / ack / nack / close / error | 0x01 / 0x02 / 0x03 / 0x04 / 0x05 / 0x06 |
| 握手 | handshakeHello / handshakeAuth / handshakeOk | 0x10 / 0x11 / 0x12 |
| 业务 | msgC2C / msgC2G / msgC2S / msgS2C / msgSync / msgTyping / msgRead / msgRecall | 0x20 / 0x21 / 0x22 / 0x23 / 0x24 / 0x25 / 0x26 / 0x27 |
| RPC | rpcReq / rpcRsp | 0x80 / 0x81 |

## 3. Flags 位定义

| 位 | 名称 | 含义 |
|----|------|------|
| bit7 | CMP | 载荷 zstd 压缩 |
| bit6 | ENC | 载荷 E2EE（预留） |
| bit5 | ACK | 需要 ACK |
| bit4-3 | DIR | ACK 帧方向：c2c=0 / c2g=1 / s2c=2 / c2s=3 |
| bit2-0 | PRI | 优先级 0-7 |

## 4. 字节序列固化（跨端测试向量）

三端编码必须产出完全相同的字节，守护测试已锁定：

```
heartbeatPing seq=7:
  49 42 02 27 01 00 00 00 02 00 07

ack msgId=0x1234567890ABCDEF (默认方向 C2C):
  49 42 02 00 03 00 00 00 08 12 34 56 78 90 AB CD EF
```

## 5. 重试策略（两套语义，勿混淆）

### 5.1 服务端投递重试 — `elib_retry_config.erl`（按消息类型）

服务端「投递消息给接收端 → 等接收端 CLIENT_ACK → 超时重投」的节奏：

| 类型 | 间隔 (ms) |
|------|-----------|
| c2c | `[0, 3000]` |
| c2g | `[0]` |
| c2s | `[0, 5000, 7000, 11000]` |
| s2c | `[0, 1500, 1500, 3000, 5000, 7000]` |
| pull | `[8000, 10000, 20000]` |
| notice | `[0, 5000, 10000]` |

### 5.2 客户端侧重试 — `retry_policy.dart`（Flutter）/ SDK

客户端「发出后等服务端确认」的重试，**独立于服务端投递重试**，不镜像：

| 语义 | 常量 | 间隔 (ms) | 使用方 |
|------|------|-----------|--------|
| 发消息等服务端确认 | `messageSendRetryIntervals` | `[3000, 5000, 10000, 20000]` | Flutter `MessageRetry`、SDK `sendWithAck`（`ACK_RETRY_INTERVALS_MS`） |
| 发 ACK 等服务端 confirm | `ackConfirmRetryIntervals` | `[3000, 5000, 10000, 15000]` | Flutter `AckManager` |

> 历史教训：`ack_manager.dart` 曾误注「服务端统一 2s/5s/7s/11s」、SDK 曾误注「3s/5s/10s 最多 3 次」，
> 实际服务端按类型不同、客户端按语义不同。本次（2026-06-23）已统一到上表。

## 6. WS action 路由（`message_router_logic.erl`）

| 类型 | 支持的 action |
|------|---------------|
| C2C | `message_revoke` / `message_revoke_ack` / `message_edit` / `message_edit_ack` / `message_read` / `message_read_ack` |
| C2G | `message_revoke` / `message_revoke_ack` / `message_edit` / `message_edit_ack` |
| S2C | `pull_offline_msg` / `c2c_revoke` / `apply_friend` / `user_muted` / `channel_*` / ...（完整清单见 Flutter `message_s2c.dart` `switchS2C`） |

## 7. 三端实现指针

| 端 | 帧编解码 | 重试策略 | 测试 |
|----|---------|---------|------|
| 后端 Erlang | `src/lib/imboy_frame.erl` + `include/imboy_frame.hrl` | `src/lib/elib_retry_config.erl` | `test/lib/elib_retry_config_tests.erl`、imboy_frame 相关 |
| Flutter | `lib/service/protocol/imboy_frame.dart` | `lib/service/retry_policy.dart` | `test/service/imboy_frame_test.dart`、`retry_policy_test.dart` |
| JS SDK | `src/protocol/imboy-frame.ts` | `src/websocket.ts` (`ACK_RETRY_INTERVALS_MS`) | `test/imboy-frame.test.ts`、`test/ack_retry.test.ts` |

## 8. 变更协议

修改任一协议常量前，必须同时更新三端实现 + 三端守护测试，并更新本表的「最后更新」日期。
