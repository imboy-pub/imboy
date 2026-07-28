# E2EE-062：客户端拒绝上传未签名的 fallback key

- **Slice**：`E2EE-062-client-fallback-signature.md` §5.2 记载的
  「没有任何机制阻止『新增一个不签名的调用点』」
- **会话**：`20260729-2100-claude-code`
- **仓库**：`imboyapp`
- **状态**：该残留**已闭合**。E2EE-062 整体仍为 `PARTIAL`

---

## 1. 做了什么

客户端为 fallback key 签名那一刀，明确留下一条残留：

> **只覆盖注册路径** …… 本刀没有加任何机制阻止「新增一个不签名的调用点」。

此后又加了周期轮换，于是现在有**两个** `reportFallbackKey` 调用点
（`registerDevice` / `maybeRotateFallbackKey`），**都会签名**。
但 `signature` 参数当时带着 `= ''` 默认值——**漏签是静默的**：
新调用点忘了传，请求体就少一个字段，服务端为兼容旧客户端**照单全收**
（两阶段推进的第一阶段），整道验签被绕过而没有任何信号。

| 接缝 | 改动 |
|---|---|
| `lib/store/api/olm_api.dart` | `buildFallbackBody` 对空签名 `throw ArgumentError`（fail-closed）；`reportFallbackKey` 的 `signature` **去掉默认值**改为 `required` |

无新依赖、无协议变更、无 schema 变更。两个既有调用点均已传参，故编译零改动。

### 1.1 这是针对**本会话已两次踩到的缺陷类别**的硬化

- `_refillOneTimeKeys(account, seed: true)` —— 参数默认/传值与调用点现实不符，
  导致每次登录全量重建 OTK 池（`E2EE-062-seed-only-for-new-account.md`）；
- `reportFallbackKey(signature: '')` —— 同一类：一个安全相关参数带着
  「静默降级」语义的默认值。

**共同教训：安全相关参数不该有默认值。** 强制调用方显式决定，
把「漏传」从运行时的静默降级变成**编译期不可表达**。

### 1.2 取舍：守卫放在 `buildFallbackBody` 而不是网络调用处

`buildFallbackBody` 是 `reportFallbackKey` 真正用来构造请求体的函数、且是纯函数，
**可直接验收**；放在网络调用处则测试必须先跨过 HTTP 层（本仓无 Dio mock 基建）。
两者都在实际发出请求之前，安全效果相同。

---

## 2. RED 记录 —— 重写既有断言（未删用例）

本刀**没有新增用例**，而是**重写**了 `fallback_key_signature_test.dart` 里
一条既有断言：

| | 内容 |
|---|---|
| 原断言 | 「正向可用性：签名为空时不得写入该键（旧语义零破坏）」 |
| 新断言 | 「签名为空必须拒绝构造请求体（fail-closed）」 |

**废止理由（已写入该用例上方注释）**：写下原断言时客户端**还不会签名**，
"允许空签名"是为旧语义兼容留的口子。此后两刀分别接上了注册路径与轮换路径的签名，
本端两个调用点现在都必然带签名，于是"空签名"只可能来自新增调用点漏签。
**出处**：`E2EE-062-client-fallback-signature.md` §5.2。

**用例未删除**，只改断言方向——符合「若某断言的语义已被决策废止，允许重写，
但须写明废止理由与出处，不得删用例」。

```
00:00 +4 -1: Some tests failed.
```

**1 红为行为失败**（空签名未被拒绝），**4 绿**含：
- `签名非空时必须进入请求体`（正向可用性，改前改后都绿）；
- 三条 canonical golden vector / 字段序 / 无尾随换行用例（对照组性质）。

### 2.1 正向可用性的另一半由既有测试承担

「合法签名的上传路径未被这道守卫误伤」由既有
`fallback_rotation_wiring_test.dart` 承担——它断言轮换产出的上报
`signatures.single` **非空**且流程走通。本刀跑全量时该文件全绿。

---

## 3. 生产调用方核实

```
lib/service/olm_session_service.dart:325  registerDevice → olm.reportFallbackKey(..., signature:)
lib/service/olm_session_service.dart:423  maybeRotateFallbackKey → olm.reportFallbackKey(..., signature:)
  → OlmApi.buildFallbackBody(...)         ← 守卫在此
  → POST /api/v1/e2ee/olm/fallback_key
```

`grep -rn reportFallbackKey lib/` 全仓只有这两处（已核实），两处都传签名。

---

## 4. 验收命令与结果

```
$ flutter test test/service/e2ee/fallback_key_signature_test.dart
  All 5 tests passed.

$ flutter test test/service/e2ee/
  All tests passed!   (394 passed)

$ flutter test test/service/
  All tests passed!   (1274 passed)

$ dart analyze lib
  1 issue found.   （component/ui/ios_settings_ui.dart:104 既有 info，与 E2EE 无关）
```

> 用例总数与上一次 imboyapp 轮次持平（394 / 1274）——本刀**重写**既有断言而非新增用例。
> `dart analyze lib` 通过即证明两个调用点都已显式传参（去掉默认值后漏传会是编译错误）。

imboy 侧未改动，`make e2ee-verify` 本刀不适用。

---

## 5. 残留风险

1. **服务端仍接受未签名上传** —— 两阶段推进的第一阶段未变：本刀只保证**本端**
   不会发出未签名的 fallback key，服务端为兼容旧版本客户端仍会接受。
   第二阶段（服务端改必填）需等 `olm_fallback_unsigned_total` 降到零。
2. **`signature` 仍可传空字符串字面量** —— `required` 只强制"必须传"，
   不强制"非空"；非空由运行时 `ArgumentError` 兜。二者叠加已足够：
   要绕过必须显式写 `signature: ''` 并撞上异常。
3. **同类默认值风险未做全仓排查** —— 本刀只处理了这一个参数。
   `_refillOneTimeKeys` 的 `seed` 仍有 `= false` 默认（但其语义是保守方向，
   且已由 `publish_seed_only_for_new_account_test.dart` 钉住）。
   **认识论状态：其余安全相关参数是否也带危险默认值，未逐个排查。**
4. E2EE-062 其余残留不变（告警规则、留存期 ≈2 周期、端到端未实证、
   单租户/全局限流、60/min 未压测、真机等）。

---

## 6. 认识论状态汇总

| 结论 | 状态 |
|---|---|
| 空签名被拒绝（fail-closed） | **已实证** |
| 合法签名照常进入请求体 | **已实证**（既有用例，改前改后都绿） |
| 轮换路径产出的签名非空且流程走通 | **已实证**（既有 wiring 测试） |
| 全仓只有两个 `reportFallbackKey` 调用点、均已传签名 | **已实证**（grep + `dart analyze` 通过） |
| 漏传参数现在是编译错误 | **已实证**（去掉默认值后 analyze 仍通过 ⇒ 两处都显式传了） |
| 服务端已拒绝未签名上传 | **否** —— 仍在第一阶段（§5.1） |
| 其余安全相关参数无危险默认值 | **未排查** |

---

## 7. 未做

- 不 push、不部署、不访问生产、不通知第三方。
- 未改 ADR / 协议规范；未动 E2EE-012/023/024/025/029 状态标记。
- **未删除任何测试用例**（重写断言并写明废止理由与出处）。
- 未新增依赖与迁移；未改服务端行为（第二阶段仍未启动）。
