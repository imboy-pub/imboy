# E2EE-062：`seed` 只应对新建账号成立（自查发现的回归）

- **Slice**：修正 `E2EE-062-client-refill-wiring.md` 那一刀引入的回归
- **会话**：`20260729-1400-claude-code`
- **仓库**：`imboyapp`
- **状态**：回归已修并实证。**E2EE-062 整体仍为 `PARTIAL`**

---

## 1. ⚠️ 本刀修的是我自己上上轮引入的缺陷

`E2EE-062-client-refill-wiring.md` 那一刀把发布路径改成：

```dart
// 生成并上报首批 one-time keys（首次注册：池必然为空，直接铺满）
await _refillOneTimeKeys(account, seed: true);
```

**那条注释对这个调用点是错的。** 核实结果：

| 事实 | 出处 | 状态 |
|---|---|---|
| `publishIdentityAndPrekeys` 由 `passport_notifier.dart:1092` 与 `olm_protocol.dart:35` 调用 | 全仓 grep | **已实证** |
| 前者是**登录**路径，不是"首次注册"专用 | 调用点上下文 | **已实证** |
| 函数体内**没有任何幂等/跳过守卫** | 逐行 | **已实证** |

于是 `seed: true` 恒成立 → **低水位判断被完全绕过** → 每次登录都全量重发 50 条 OTK。
而 `report_one_time_keys` 是**全量替换式**（先删后插，见 imboy
`olm_identity_logic:report_one_time_keys/4` 注释）——

> **每次登录都会把一个健康的 OTK 池推倒重建一次。**

这正是那一刀自己在 §1.1 里指认为有害的那种 churn。它修好了「每次入站建会话都重置池」，
却在同一次改动里把「每次登录都重置池」写了进去。

**教训：`seed` 这个参数名描述的是"意图"（首次注册），而代码里能断定的只有"事实"
（账号是否新建）。名字与可判定事实不一致时，调用点迟早会传错。**

---

## 2. 做了什么

`seed` 的依据从「调用方自称是注册流程」改成「**账号是本次加载中新建的**」——
这是代码真能断定的事实：

| 接缝 | 改动 |
|---|---|
| `lib/service/olm_session_service.dart` | 新增 `_accountCreatedThisLoad`，由 `_loadOrCreateAccount` 置位；`publishIdentityAndPrekeys` 传 `seed: _accountCreatedThisLoad` |
| 同上 | `publishIdentityAndPrekeys` / `_refillOneTimeKeys` 新增 `@visibleForTesting OlmApi? api` 注入口，使接线**可验证** |
| 同上 | 新增 `debugResetAccountCache()`（**刻意不并入 `resetForTest`**，避免改变既有测试依赖的缓存行为） |

新建账号 → 服务端确实一把 OTK 都没有 → 无条件铺满（且**不查询**，避免一次查询失败
就让新设备永远没有 OTK）。从 pickle 载入 → 走低水位判断。

无新依赖、无协议变更、无 schema 变更。

### 2.1 取舍：为什么用「账号新鲜度」而不是「查一次再决定」

「先查真实余量，查不到才 seed」听起来更精确，但把**未知**当作**空池**处理
就是在未知状态上执行全量替换——正是上上轮明确否掉的方向。
账号新鲜度是本地可确定的事实，不依赖网络，且判错的方向是安全的：
载入的账号若真的没有 OTK，低水位判断（count=0）照样会补满。

---

## 3. RED 记录

新增 `test/service/e2ee/publish_seed_only_for_new_account_test.dart`（3 例）。
用**真 vodozemac 账号**（`vod.init` + `debugMarkVodReady`）+ 假 `OlmApi` 子类记录调用，
断言的是**有没有发出上报**，不是内部返回值。

生产代码在同一轮内修好，因此 RED 用**空验证**取得：把 seed 决策临时还原成恒 `true`。

```
00:00 +2 -1: Some tests failed.
Failing tests:
  … 从 pickle 载入 + 池健康 → 不得重发 OTK
```

**唯独核心用例变红**，对照组与正向可用性仍绿 —— 说明该用例确实在测这个决策，
而不是被别的条件顺带盖住。验证后已恢复。

### 3.1 对照组与正向可用性

- **对照组** `新建账号必须铺满 OTK 池`：它红就说明发布流程根本没跑起来，
  此时"不重发"的绿毫无意义。空验证前后**都绿**。
  该用例还断言 `countCalls == 0`——新建账号不该依赖查询。
- **正向可用性** `载入账号但池见底 → 仍必须补传`：
  一个「载入账号一律不补」的实现在"不 churn"指标上**恒得满分**，被这条否掉。

---

## 4. 验收命令与结果

```
$ flutter test test/service/e2ee/publish_seed_only_for_new_account_test.dart
  All 3 tests passed.

$ flutter test test/service/e2ee/
  All tests passed!   (377 passed；上一刀 374)

$ flutter test test/service/
  All tests passed!   (1257 passed；上一刀 1254)

$ dart analyze lib
  1 issue found.   （component/ui/ios_settings_ui.dart:104 既有 info，与 E2EE 无关）
```

imboy 侧未改动，`make e2ee-verify` 本刀不适用。

---

## 5. 残留风险（E2EE-062 仍未完成）

1. **fallback key 的轮换周期未评估** —— 本轮顺带核实：`generateFallbackKey()`
   全仓只在 `publishIdentityAndPrekeys` 出现一次，即**只在登录时轮换**，
   没有基于时间的轮换，也没有 `forgetFallbackKey`。
   长期不登出的设备，其 fallback key 会长期不变；而 OTK 耗尽时所有新会话都走它。
   Olm/Matrix 的惯例是按周期轮换并遗忘旧 key。
   **认识论状态：调用点分布已实证；「当前轮换频率是否足够」未评估。**
2. `report_identity` 每次登录仍无条件上报（幂等 upsert，无害但也没必要）；
3. 服务端 fallback 签名仍非必填；客户端 fallback 只覆盖注册/登录路径；
4. 告警规则未做；`/metrics` 输出未实证；
5. 被拦下的重发行仍被扫描器每轮捡起（不写库、不出网）；滞留后 UX 无具体提示；
6. 幂等/补传链路端到端未实证；单租户/全局限流未做；租约无独立 TTL；
   60/min 未压测；进程重启后重投仍消费新 OTK；客户端无 batch_claim 调用方；
7. 真机双端未验证。

---

## 6. 认识论状态汇总

| 结论 | 状态 |
|---|---|
| `publishIdentityAndPrekeys` 每次登录都会跑、无幂等守卫 | **已实证**（全仓 grep + 逐行） |
| 原 `seed: true` 使低水位判断被完全绕过 | **已实证**（空验证：还原后核心用例即红） |
| 修后：新建账号铺满且不查询 | **已实证** |
| 修后：载入账号 + 池健康 → 不重发 | **已实证** |
| 修后：载入账号 + 池见底 → 仍补传 | **已实证** |
| `generateFallbackKey` 只在登录路径出现一次 | **已实证**（全仓 grep） |
| 「当前 fallback 轮换频率足够」 | **未评估** |

---

## 7. 未做

- 不 push、不部署、不访问生产、不通知第三方。
- 未改 ADR / 协议规范；未动 E2EE-012/023/024/025/029 状态标记。
- 未删除、未 skip 任何测试；未新增依赖与迁移。
- **未动 fallback key 的轮换策略**（§5.1，需先评估）。
