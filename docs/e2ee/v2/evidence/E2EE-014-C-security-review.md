# E2EE-014-C 安全复核（#3）+ HIGH 修复

> 日期：2026-07-21 | security-reviewer 复核 codec + 客户端模块 + 后端 trust 路径

## 复核范围
- `imboyapp/lib/service/e2ee/trust_event_canonical.dart`
- `imboyapp/lib/service/e2ee/trust_event_client.dart`
- `imboy/src/logic/e2ee_trust_logic.erl`

## 发现与处置

| 级别 | 发现 | 处置 |
|---|---|---|
| CRITICAL C1 | 后端 `normalize/2` 不复算 event_id 字符集守卫（只长度+非空）——非 Dart 客户端可提交异常 event_id | 归入统一根因（见下）。⚠️ 复核「critical」框定偏高：event_id 两侧是数字字段，单独不能构造签名碰撞（复核自述） |
| HIGH H1 | **canonical 非单射**：`target_device_id`/`target_ed25519` 相邻自由文本字段无 `\n` 约束，一个 Ed25519 签名可对两组不同字段拆分同时有效→信任伪造原语 | **已修** |
| MEDIUM M1 | 客户端 `fromBroadcast` 校验 method 枚举但漏 to_state 枚举 | **已修**（加 `kTrustStates` 白名单） |
| LOW L1 | `buildTrustRecordRequest` 未本地校验 target_device_id/ed25519 非空（依赖后端兜底） | 记录，未改（后端权威拒收） |
| LOW L2 | `isFreshTrustEvent` 未强制接入构建流程 | 记录（可靠性/UX，非安全） |

## 根因与修复（C1+H1 统一）

**根因**：canonical `key=value\n` 唯一分隔符是 `\n`，无转义/无长度前缀。任一字符串字段值内含
`\n` 即破坏 `key=value` 行结构，使编码**非单射**。`=` 无害（后端从不反解析 canonical，键固定有序）。
故单射充要条件 = **所有字符串值不含 `\n`/`\r`**（无需 hex 全字符集，且全字符集会破坏现有 evt-1 fixture）。

**修复（双端，权威在后端）**：
- 后端 `e2ee_trust_logic:normalize/2` 加 `no_ctrl_chars/1` 守卫 `[ActorDeviceId, TargetDeviceId,
  TargetEd25519, FromState, ToState, EventId]`，含 `\n`/`\r` → `bad_request`。commit imboy `c2bab1e9`，eunit +1（`newline_in_field_rejected_test`），**22/22 绿**。
- 客户端 `trust_event_canonical.dart` `canonicalBytes()` 加 `_rejectNewline` 守卫 target_device_id/
  target_ed25519/from_state/to_state（event_id 已 hex 正则排除换行），fail-closed `ArgumentError`。
- M1：`trust_event_client.dart` `fromBroadcast` 加 `to_state ∈ kTrustStates` 校验。
- commit imboyapp `ecdc1805`，client 单测 +3（两 newline + 一 to_state 枚举），**26/26 绿**。

## 契约一致性（复核确认无漂移）
method 白名单 / §3.2 转换白名单（5 组）/ freshness 常量与判定 / canonical 11 字段序 / 请求体 13 字段键名
两端逐一匹配；actor_uid 正确不入请求体（服务端 current_uid 权威注入，客户端伪造只会自我失效）。

## 结论
CRITICAL/HIGH = 0（H1 已修，C1 同根因已修）。2 LOW 记录（owner=后续接线时接入 L2 预检、L1 本地非空）。
