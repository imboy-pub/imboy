# E2EE-062：fallback key 周期轮换（实施）

- **Slice**：上一刀评估结论的实施
- **会话**：`20260729-1600-claude-code`
- **仓库**：`imboyapp`
- **状态**：周期轮换已落地并实证。**`forgetFallbackKey()` 仍未调用**（§5.1）。
  E2EE-062 整体仍为 `PARTIAL`

---

## 1. 做了什么

上一刀评估确认：`generateFallbackKey()` 只在登录时调用 →
**长期不登出的会话，其 fallback key 永不被替换**；而 OTK 耗尽（整个 E2EE-062
系列处理的正是这件事）会把所有新会话逼到那把 key 上。

本刀补上周期轮换。

| 接缝 | 改动 |
|---|---|
| `lib/service/e2ee/fallback_rotation_policy.dart`（新） | 纯函数 `shouldRotateFallbackKey` + `kFallbackRotationInterval`（7 天） |
| `lib/service/olm_session_service.dart` | `maybeRotateFallbackKey`（生成 → 签名 → 上报 → 记录时刻 → 持久化）；`_recordFallbackRotation` / `_readLastFallbackRotation`；登录路径上报后也记录时刻 |
| 同上 | 接线到**入站建会话后**的维护链（与 OTK 补传同处） |

无新依赖、无协议变更、无 schema 变更。

### 1.1 取舍一：触发点挂在入站建会话之后

**"长期不登录"正是缺口成因，把轮换绑在登录上等于没修。** 候选与取舍：

| 候选 | 结论 |
|---|---|
| **入站建会话后**（采用） | 活跃用户天然触发，**零新基建** |
| 应用启动时 | 长期不重启的设备同样漏掉 |
| 定时器 / 后台任务 | 需要新的调度基建与生命周期管理，面更大 |

代价诚实记录：**完全不收消息的设备不会轮换**。但那样的设备也不会有人向它建新会话，
暴露面随之为零——这个残留是自洽的。

### 1.2 取舍二：时间戳异常一律判「该轮换」

- `null`（升级上来的老账号 / 新装）→ 轮换。"拿不准就不换"会让老账号继续无限期
  沿用同一把 key，正是本项要消除的情形；
- 未来时间戳（时钟回拨 / 数据损坏）→ 轮换。判成"刚轮换过"会让该设备**永远**
  不再轮换；
- 恰好等于周期 → 轮换（到点即换，宁早勿晚）。

多换一次的代价只是一次上报，且旧 key 被 vodozemac 保留、在途消息不受影响
（上一刀已实证）。**三种异常都倒向"换"，是本项唯一安全的方向。**

### 1.3 取舍三：**不**调用 `forgetFallbackKey()`

轮换后旧私钥被保留，正是"在途 pre-key 消息仍可解密"的机制（上一刀已实证）。
过早遗忘会**丢消息**，而 `forgetFallbackKey()` 的确切语义（丢的是"当前"还是
"上一把"）尚未特征化——Dart 侧文档写的是 "Forget the **current** fallback key"，
与 vodozemac Rust 侧的常见描述不一致，**不能凭文档下手**。
留作独立一刀，见 §5.1。

不调用它的后果是旧私钥留在 pickle 里——**这与本刀之前的状态相同，不是新增风险**。

---

## 2. RED 记录

### 2.1 纯策略

`test/service/e2ee/fallback_rotation_policy_test.dart`（5 例）。
先落载体（`shouldRotateFallbackKey` 恒 `false`，即"从不轮换"＝今天在这条路径上的
行为），取得 RED `+1 -4`，**4 红均为行为失败**。
**对照组**「刚轮换过 → 不轮换」在载体阶段即绿、实现后仍绿。

### 2.2 接线

`test/service/e2ee/fallback_rotation_wiring_test.dart`（4 例）。
用**真 vodozemac 账号** + 假 `OlmApi` 记录上报，打的是生产入口
`maybeRotateFallbackKey`。生产代码同轮写就，故 RED 用**空验证**取得
（把判据临时还原成恒 `false`）。

**⚠️ 第一次空验证暴露了 harness 的一个真实弱点：3 例全红，包括业务对照组。**
原因是业务对照组「刚轮换过 → 不得再报」**依赖"第一次轮换成功"这个前置条件**，
判据被摘除时它因**前置不成立**而红，而非因被测性质失效——
这削弱了「对照组红 = harness 缺陷」这个信号本身。

处置：补一条**与策略无关**的 harness 对照组（secure storage mock 通道可读写）。
再次空验证结果：

```
+1 -3     # harness 对照组绿，三条业务用例全红
```

**harness 对照组在两态都绿**，信号恢复有效。恢复后 `git diff --stat` 无漂移。

### 2.3 正向可用性

- `从未记录过轮换时刻 → 必须轮换并上报`：一个「永不轮换」的实现在"不重复上报"
  指标上恒满分，被这条否掉；
- 该用例同时断言**上报带签名非空**——否则就绕过了服务端验签那一刀；
- `时刻过期 → keyId 与上一次不同`：若 keyId 不变，只是把同一把 key 重发了一遍，
  **暴露窗口并没有缩短**。

---

## 3. 生产调用方核实

```
olm_session_service.dart  decrypt（pre-key 分支）
  → unawaited(_refillOneTimeKeys(account)
      .then((_) => maybeRotateFallbackKey())      ← 本刀
      .then((_) => _persistAccount(...)))
```

`maybeRotateFallbackKey` 是 `@visibleForTesting` 但**由生产代码调用**，
测试打的就是这个入口，不是旁路。

---

## 4. 验收命令与结果

```
$ flutter test test/service/e2ee/fallback_rotation_policy_test.dart
  All 5 tests passed.

$ flutter test test/service/e2ee/fallback_rotation_wiring_test.dart
  All 4 tests passed.

$ flutter test test/service/e2ee/
  All tests passed!   (390 passed；上一刀 381)

$ flutter test test/service/
  All tests passed!   (1270 passed；上一刀 1261)

$ dart analyze lib
  1 issue found.   （component/ui/ios_settings_ui.dart:104 既有 info，与 E2EE 无关）
```

imboy 侧未改动，`make e2ee-verify` 本刀不适用。

---

## 5. 残留风险（E2EE-062 仍未完成）

1. **`forgetFallbackKey()` 仍未调用** —— 旧私钥留在 pickle。
   需先特征化其确切语义（Dart 文档说 "forget the **current**"，与 Rust 侧常见描述
   不一致），**误调会丢在途消息**。独立一刀。
2. **完全不收消息的设备不会轮换** —— 触发点取舍的自洽代价（§1.1）。
3. **7 天周期未经论证** —— 取自 Olm/Matrix 生态惯例，**未针对本项目的会话时长分布
   做过评估**。**认识论状态：惯例值，未验证。**
4. 服务端 fallback 签名仍非必填；`report_identity` 的 signature 未验证；
5. 告警规则未做；`/metrics` 输出未实证；
6. 被拦下的重发行仍被扫描器每轮捡起（不写库、不出网）；滞留后 UX 无具体提示；
7. 幂等/补传链路端到端未实证；单租户/全局限流未做；租约无独立 TTL；
   60/min 未压测；进程重启后重投仍消费新 OTK；客户端无 batch_claim 调用方；
8. 真机双端未验证。

---

## 6. 认识论状态汇总

| 结论 | 状态 |
|---|---|
| 从未记录时刻 → 轮换并上报（带签名） | **已实证** |
| 刚轮换过 → 不再上报 | **已实证** |
| 时刻过期 → 再次轮换且 keyId 改变 | **已实证** |
| 时间戳异常（null/未来/恰好到期）→ 轮换 | **已实证**（纯策略） |
| 轮换由入站建会话后的生产链路触发 | **已实证**（生产调用方 + 空验证） |
| harness 本身可用（与策略无关） | **已实证**（两态都绿） |
| 7 天周期对本项目是否合适 | **未验证**（生态惯例值） |
| 完全不收消息的设备的暴露面为零 | **推理**（无人向其建新会话），未测 |
| 旧私钥已从 pickle 清除 | **否** —— 未调 `forgetFallbackKey()`（§5.1） |

---

## 7. 未做

- **未调用 `forgetFallbackKey()`**（§1.3 / §5.1）。
- 不 push、不部署、不访问生产、不通知第三方。
- 未改 ADR / 协议规范；未动 E2EE-012/023/024/025/029 状态标记。
- 未删除、未 skip 任何测试；未新增依赖与迁移。
