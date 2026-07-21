# E2EE-014 残留 C — 客户端 trust-event canonical codec 子件

> 状态：PARTIAL（codec + 签名封装 DONE；wire 往返 + 真机验证 **未做**，见「未覆盖」）
> 日期：2026-07-21 | 执行：Claude Code | 授权：leeyi 选「先做 -C 纯 codec 子件」

## 范围（ADR 16 §3.3.1 scoped waiver 已解锁子集）

只实现「A 信任 B」trust-event 的**客户端 canonical 编码器 + Ed25519 签名封装**，
使客户端能产出与后端 `e2ee_trust_logic:canonical_payload/1` **逐字节一致**的签名负载。

**未实现 / 仍 BLOCKED（§5 cross-signing 仍 Proposed）**：
account Master Key / device-signing key 授权、manifest `account_signature`、
transparency inclusion proof、safety-number 校验 UI、实际 POST `/e2ee/trust/record`
与 `e2ee_trust_changed` 广播接收。

## 交付物

| 文件 | 说明 |
|---|---|
| `imboyapp/lib/service/e2ee/trust_event_canonical.dart` | `TrustEventCanonicalFields`（11 字段不可变）+ `canonicalBytes()`（纯编码，零 crypto 依赖）+ `sign(Ed25519Signer)`（注入式签名器） |
| `imboyapp/test/service/e2ee/trust_event_canonical_test.dart` | 6 单测，含 backend golden 逐字节比对 |
| `imboyapp/lib/service/e2ee/trust_event_client.dart` | 客户端逻辑补全（app `2556d744`）：`buildTrustRecordRequest`（13 字段请求体，不含 actor_uid）+ `isFreshTrustEvent`（freshness 预检，镜像后端 `fresh/2`）+ `isValidTrustTransition`（§3.2 白名单镜像）+ `TrustChangedEvent.fromBroadcast`（广播解析/校验，外部数据 fail-closed 抛 FormatException） |
| `imboyapp/test/service/e2ee/trust_event_client_test.dart` | 17 单测（转换白名单/freshness 六边界/请求体/广播解析五拒绝） |

后端契约锚点（逐一对齐 `imboy/src/logic/e2ee_trust_logic.erl`）：method 白名单 `?VALID_METHODS`、
转换白名单 `valid_transition/2`（5 条）、freshness `FRESH_PAST_MS=300000/FRESH_FUTURE_MS=120000/MAX_TTL_MS=300000`、
广播 7 字段 `broadcast_trust_changed/1`、请求体字段 `normalize/2`。任一漂移会致后端拒收，改动须同步两端。

## Golden 向量来源（非手算，取自真实后端）

以确定性输入运行后端权威编码器：

```
erl -pa ebin -eval 'e2ee_trust_logic:canonical_payload(#{
  <<"actor_device_generation">> => 1, <<"actor_uid">> => 100,
  <<"event_id">> => <<"3b1e0c4a-5f2d-4a1b-9c3e-7d8f0a1b2c3d">>,
  <<"expires_at">> => 1700000060000, <<"from_state">> => <<"unverified">>,
  <<"issued_at">> => 1700000000000, <<"target_device_id">> => <<"phone-b">>,
  <<"target_ed25519">> => <<"ZWQtYg==">>, <<"target_identity_version">> => 1,
  <<"target_uid">> => 200, <<"to_state">> => <<"verified">>})'
```

- `SHA256 = e8fb84b37ffc4e69beebea5665dcbc4997f146482fc4bb1bde0884b86940815b`
- 客户端 `canonicalBytes()` 产出与之 base64 + SHA256 双重比对一致。

## 验收

`flutter test test/service/e2ee/trust_event_canonical_test.dart` → **6/6 绿**；
`dart analyze` → No issues found。

1. `matches backend golden vector byte-for-byte`（base64 + SHA256 双重锚定）
2. `renders exactly 11 lines, no trailing newline`
3. `fields are emitted in ASCII dictionary order`
4. `rejects event_id with newline (canonical injection guard)`
5. `rejects event_id longer than 64 chars`
6. `signer receives exact canonical bytes; result base64-encoded`

## 关键决策 / 坑

- **§3.3.1 `event_id` 正则 `[0-9a-f-]{1,64}` 是 hex-only**。后端 eunit fixture 用
  `evt-1`（含 `v`/`t`）实为违规占位——后端 `canonical_payload/1` 不校验格式仅拼接，
  故不报错。客户端按 spec 加了 fail-closed 校验（非法 event_id 抛 `ArgumentError`），
  golden 遂改用合规 UUID 形式 `3b1e0c4a-...`。
- **签名器注入**（`Ed25519Signer` 回调，沿用 E2eeSecretInventory 注入模式）：单测以
  fake 签名器断言其收到的正是 golden 字节，**无需 vodozemac FFI**，无幻影。生产接线
  = 调用点传 `vod.Account.sign`，属 wire slice。
- **canonical 注入面**：唯一客户端生成的自由字段是 `event_id`，其 hex-only 正则天然
  排除 `\n`/`=`，堵死破坏 `key=value\n` 结构的注入。其余字段来自设备身份/状态机受控词汇。

## 未覆盖（需真机 + 运行后端，勿 mock 协议边界）

- Dart 产签 → 后端 `record_trust_event/2` 逐字节验签通过（wire 双端契约实证）。
- 端到端：客户端 POST `/e2ee/trust/record` → 收 `e2ee_trust_changed` 广播。
- freshness/幂等/单调/撤销在真实网络下的行为（后端 eunit 已覆盖逻辑侧）。

以上属 E2EE-014-C 的 wire 部分，须配真机 + 运行本地后端逐字节验证后再补证据。

## #1 后端 wire 半程集成测（BLOCKED，待 dev 库迁移对账决策）

设想：本地起后端 + imboy_v1，Erlang 侧生成 Ed25519 → `canonical_payload/1` → 签名 →
真实 POST `/e2ee/trust/record` → 断言验签通过 + `trust_audit` 落库 + 广播意图。此可证
**服务端 wire 半程**（HTTP→verify→audit→broadcast），无需真机。

阻塞（2026-07-21 探测）：imboy 节点未运行；`imboy_v1.schema_migrations = {37, 44, 47}`，
38-43/45/46 空缺。`IMBOYENV=local make run` 启动会跑迁移 runner 补这些号，与已登记的
44/47 乱序 → erlang_migrate strict 乱序检测报错，后端起不来。需先做 dev 库迁移对账（三选一）：
(a) 按序补 38-46；(b) 登记 38-46 为已应用（假对账，可能掩盖真实 schema 缺口，不推荐）；
(c) 本次测试禁用启动迁移 runner。均触 dev DB、属用户决策，未擅自执行。
