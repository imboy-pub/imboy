# E2EE-062 残留 2：客户端为 fallback key 签名

- **Slice**：E2EE-062 残留 2（上一刀「服务端验签第一阶段」的直接后继）
- **会话**：`20260729-1300-claude-code`
- **仓库**：`imboyapp`（含 imboy 侧 golden vector 钉桩）
- **状态**：客户端签名已接线；**签名仍非必填**，本项仍未关闭（§5.1）。
  E2EE-062 整体仍为 `PARTIAL`

---

## 1. 做了什么

上一刀让服务端**能验**签，但**客户端不发送签名**——与第四刀 `request_id` 完全同形：
服务端做的工作在生产流量上等于零，且因此**无法把签名改为必填**
（改了所有设备都发布不了 fallback key）。本刀补上客户端这一半。

| 接缝 | 改动 |
|---|---|
| `lib/service/e2ee/fallback_key_signature.dart`（新） | 纯函数 `fallbackKeyCanonical` |
| `lib/store/api/olm_api.dart` | `reportFallbackKey` 新增可选 `signature`；抽出 `buildFallbackBody` 作为可验收接缝 |
| `lib/service/olm_session_service.dart` | `registerDevice` 用 `account.sign(canonical)` 签名后上报 |
| imboy `test/logic/e2ee_fallback_signature_tests.erl` | +1 例：把服务端 canonical 钉到 golden vector |

无新依赖、无协议破坏（`signature` 可选）、无 schema 变更。

### 1.1 ⚠️ 跨语言一致性是本刀的核心风险，不是附带事项

客户端 canonical 必须与服务端 `fallback_canonical/4` **逐字节一致**。
不一致的后果**不是「少一层防护」**，而是：

> 验签必然失败 → 该设备**发布不了 fallback key** → 每次 OTK 耗尽都变成
> `no_prekey_available` → **新会话直接建不起来**。

即：一个编码细节的偏差会变成**生产可用性事故**。

处置：**两侧各自把同一条 golden vector 钉死**——

| 侧 | 断言 |
|---|---|
| imboy | `canonical_golden_vector`：用「按 golden 字面量签名 → 服务端必须接受」间接钉死服务端 canonical（构造字节不同则验签必失败） |
| imboyapp | 直接断言 `fallbackKeyCanonical(...)` 等于同一字面量 |

两侧同时断言**长度 = 82 字节**。这是本项目在没有联调环境时能做的最强一致性检查。

### 1.2 长度断言不是冗余 —— 本轮它抓到了我两次算错

写 golden vector 时我手算长度先后写成 96、81，**两次都错**，实算是 82。
若不把长度列入向量、只比内容，一旦编码规则理解错（多/少尾随换行、字段序不对），
得到的只是无信息量的「不相等」；长度先对不上，能立刻指出错在哪一类。

### 1.3 canonical 方案复用既有编码

`key=value\n`、ASCII 字典序、末字段无尾随换行——与
`trust_event_canonical.dart` / `e2ee_trust_logic:canonical_payload/1` 同一方案，
也与 KT profile §3 一致。**项目不发明第三套编码。**
字段序 `device_id < key_base64 < key_id < user_id` 已是字典序。

---

## 2. RED 记录

### 2.1 服务端侧

在既有 `e2ee_fallback_signature_tests.erl` 上加 `canonical_golden_vector`。
第一次失败于我手算的长度（96），第二次仍错（81）——
**签名断言本身一次就通过**，说明服务端 canonical 与 golden 字面量一致。
改用实算值 82 后 8/8 全绿。

### 2.2 客户端侧

新增 `test/service/e2ee/fallback_key_signature_test.dart`（5 例）。
先落**载体**（canonical 返回空串、`buildFallbackBody` 忽略 signature），
使 RED 是行为失败：

```
00:00 +1 -4: Some tests failed.
```

**4 红均为行为失败**：golden vector 不符、末字段尾随换行判定失败、
字段序判定失败、`body['signature']` 为 null。

**1 绿 = 正向可用性**：`签名为空时不得写入该键`（旧语义零破坏）。
载体阶段空绿，实现后仍须绿——它红就说明我把旧客户端的兼容性砍掉了。

---

## 3. 生产调用方核实

```
lib/service/olm_session_service.dart  registerDevice
  → account.generateFallbackKey()
  → fallbackKeyCanonical(...)                    ← 本刀
  → account.sign(canonical).toBase64()
  → OlmApi().reportFallbackKey(..., signature:)
  → buildFallbackBody(...)                       ← 请求体真正的构造函数
  → POST /api/v1/e2ee/olm/fallback_key
```

`grep -n reportFallbackKey lib/` 全仓只有 `registerDevice` 一处调用点。
`buildFallbackBody` 是 `reportFallbackKey` **真正用来构造请求体**的函数，
不是平行实现、也不是读源码的结构断言。

⚠️ 未做 HTTP 层端到端测试（本仓无 Dio mock 基建，引入属新依赖方向）。
**「签名真的随请求发到服务端并通过验证」为分段实证、端到端未实证**（§6）。

---

## 4. 验收命令与结果

```
$ flutter test test/service/e2ee/fallback_key_signature_test.dart
  All 5 tests passed.

$ flutter test test/service/e2ee/
  All tests passed!   (374 passed；上一刀 369)

$ flutter test test/service/
  All tests passed!   (1254 passed；上一刀 1249)

$ dart analyze lib
  1 issue found.   （component/ui/ios_settings_ui.dart:104 既有 info，与 E2EE 无关）

$ make e2ee-verify          # imboy 侧（本刀加了一条后端断言）
  All 351 tests passed.     # 上一刀 350，本刀 +1

$ erlfmt --check test/logic/e2ee_fallback_signature_tests.erl
  All matched files use erlfmt code style!

$ git diff --check
  （通过）
```

---

## 5. 残留风险（E2EE-062 仍未完成）

### 5.1 ⚠️ 本项仍未关闭：签名仍非必填

服务端两阶段的第二阶段（改为**必填**）**未做**。启动它现在具备了前置条件
（客户端会签名了），但需要观察 `olm_fallback_unsigned_total` 降到零、
即旧版本客户端基本退场之后才能安全切换。**在那之前，被盗 token 的攻击者
仍可「干脆不带签名」绕过整道校验。**

### 5.2 只覆盖注册路径

`registerDevice` 是全仓唯一的 `reportFallbackKey` 调用点（已 grep 核实），
但 fallback key 的**轮换**路径若日后新增，必须同样带签名。
本刀没有加任何机制阻止「新增一个不签名的调用点」。

### 5.3 端到端未实证

客户端产出签名、服务端能验签，两侧各自实证；**拼接只有 golden vector 这一层
静态保证**，未在真实网络上跑通一次。

### 5.4 其余残留（不变）

1. `report_identity` 的 signature 只校验非空、未验证（**已实证**）；
2. 告警规则未做；`/metrics` 输出未实证；
3. 被拦下的重发行仍被扫描器每轮捡起（不写库、不出网）；滞留后 UX 无具体提示；
4. 幂等/补传链路端到端未实证；单租户/全局限流未做；租约无独立 TTL；
   60/min 未压测；进程重启后重投仍消费新 OTK；客户端无 batch_claim 调用方；
5. 真机双端未验证。

---

## 6. 认识论状态汇总

| 结论 | 状态 |
|---|---|
| 客户端 canonical 与服务端逐字节一致 | **已实证**（双侧 golden vector + 长度 82） |
| canonical 末字段无尾随换行、字段序为 ASCII 字典序 | **已实证** |
| 签名进入 fallback 上报请求体 | **已实证**（`buildFallbackBody` 是生产构造函数） |
| 签名为空时不写入该键（旧语义零破坏） | **已实证** |
| `registerDevice` 是全仓唯一调用点 | **已实证**（全仓 grep） |
| 服务端 canonical 与 golden 一致 | **已实证**（按 golden 签名 → 服务端接受） |
| 「签名随请求发到服务端并通过验证」端到端 | **分段已实证，端到端未实证** |
| 「fallback prekey 已受身份验证保护」 | **不成立** —— 签名仍非必填（§5.1） |

---

## 7. 未做

- 不 push、不部署、不访问生产、不通知第三方。
- 未改 ADR / 协议规范（`signature` 为可选字段）。
- 未动 E2EE-012/023/024/025/029 状态标记。
- 未删除、未 skip 任何测试；未新增依赖与迁移。
- **未把服务端签名改为必填**（§5.1）。
