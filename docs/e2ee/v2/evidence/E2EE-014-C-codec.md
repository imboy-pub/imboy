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
