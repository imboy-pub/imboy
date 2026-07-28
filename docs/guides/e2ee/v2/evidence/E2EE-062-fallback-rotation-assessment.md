# E2EE-062：fallback key 轮换评估（含 vodozemac 特征测试）

- **Slice**：上一刀残留 1「fallback key 轮换周期未评估」
- **会话**：`20260729-1500-claude-code`
- **仓库**：`imboyapp`
- **状态**：评估完成，**结论：是缺口，需实施周期轮换**。实施未做（§4）。
  E2EE-062 整体仍为 `PARTIAL`
- **本刀不改任何生产代码**（`git diff --stat lib/` 无输出）

---

## 1. 做了什么

上一刀顺带实证：`generateFallbackKey()` 全仓**只在 `publishIdentityAndPrekeys`
出现一次**（即只在登录时轮换），且全仓**没有** `forgetFallbackKey()` 调用。
要判断这是不是前向保密缺口，必须先确定 vodozemac 的**实际**语义——
读文档只能形成假设，本项目已多次被实证推翻。

新增 `test/service/e2ee/fallback_key_rotation_characterization_test.dart`（4 例），
用**真 vodozemac 账号**把库行为钉死。

### 1.1 实证结果（4/4 全绿）

| # | 问题 | 结果 |
|---|---|---|
| 1 | **对照组**：新账号生成前是否为空、生成后是否取得到 | 生成前**空**、生成后 **1 把**。它红就说明测试根本没在测轮换 |
| 2 | `markKeysAsPublished()` 之后 `fallbackKey` 还剩什么 | **变空** —— 文档里 "current **unpublished** fallback key" 是字面意思 |
| 3 | 再次 `generateFallbackKey()` 是否换出新 keyid | **是**，keyid 改变 —— 确实是轮换，不是幂等空操作 |
| 4 | 轮换后，用**旧** fallback key 建的会话还能否解密 | **能** —— 旧私钥被保留 |

事实 2 解释了生产代码 `if (fbKey.isNotEmpty)` 的分支条件为何成立：
必须紧接在 `generateFallbackKey()` 之后取值。

事实 4 有两层含义：
- **轮换是安全的**：在途的 pre-key 消息不会因轮换而丢失；
- **不调 `forgetFallbackKey()` 就意味着旧私钥一直留在 pickle 里**。

---

## 2. 评估结论：**是缺口**

### 2.1 缺口本身

vodozemac 保留 **current + previous** 两把 fallback 私钥。我们只在**登录**时轮换。
因此：

> **对于长期不登出的会话，当前 fallback key 永远不会被替换。**

这与 E2EE-062 前面几刀是**互相咬合**的：整个系列都在处理「OTK 会被耗尽」，
而耗尽的结果就是**所有新会话都改用 fallback key**。于是攻击者可以

1. 先把目标的 OTK 池抽干（前几刀让这件事变慢、可观测，但**没有变成不可能**）；
2. 迫使此后所有新会话都走那把**可能已存在数月**的 fallback key；
3. 一旦拿到该私钥，**回溯性地**解开这期间通过它建立的每一条会话。

单把 key 的生命周期越长，其泄漏的爆炸半径越大——这正是 Olm/Matrix 生态按周期
（通常约一周）轮换并在宽限期后遗忘旧 key 的原因。

### 2.2 严重程度的诚实界定

- **不是**「立刻可利用」的漏洞：利用前提是拿到 fallback 私钥，
  而它存放在与 identity 私钥同级的加密 pickle 里；
- **是**一个放大器：它把「单点私钥泄漏」放大成「一段时间内全部 fallback 会话泄漏」，
  且该时间段**无上界**。

**认识论状态：vodozemac 语义已实证（§1.1）；调用点分布已实证（全仓 grep）；
「长期在线设备实际会持续多久不登录」为部署侧事实，本项目未测。**

### 2.3 为什么现在才发现

前几刀的注意力都在「耗尽路径能不能被拖慢 / 被看见 / 不被降级」，
默认了「回退到 fallback 是安全的兜底」。本刀检查的是**兜底本身的时效性**——
一个在防御链上游做完功课后才会暴露出来的问题。

---

## 3. 触发点的选择（实施前必须先定）

轮换的难点不是"怎么换"，而是**"什么时候触发"**：现在唯一的触发是登录，
而"长期不登录"恰恰是问题本身。候选：

| 候选 | 评价 |
|---|---|
| **挂到 `_refillOneTimeKeys`** | 它在**每次入站建会话**后都会跑（`olm_session_service` 的 pre-key 分支），活跃用户天然会触发，**无需引入调度器**。倾向此项 |
| 应用启动时 | 长期不重启的设备同样漏掉；且启动路径已经很拥挤 |
| 定时器 / 后台任务 | 需要新的调度基建与生命周期管理，是更大的面 |

无论选哪个，都需要**持久化上次轮换时间**（account pickle 之外的一个小状态），
并定义宽限期后才调用 `forgetFallbackKey()`——**过早遗忘会丢在途消息**（事实 4）。

---

## 4. 未实施，及其理由

本刀**只做评估**，不实施。理由：
实施需同时定下「触发点 + 轮换周期 + 遗忘宽限期 + 时间戳持久化」四项，
且触发点的选择会改变 `_refillOneTimeKeys` 的职责边界。
按「一轮一件、宁可多跑几轮也不留半成品跨越压缩边界」，拆成下一刀更稳。

**在实施前，本缺口仍然存在。**

---

## 5. 验收命令与结果

```
$ flutter test test/service/e2ee/fallback_key_rotation_characterization_test.dart
  All 4 tests passed.

$ flutter test test/service/e2ee/
  All tests passed!   (381 passed；上一刀 377，本刀 +4)

$ flutter test test/service/
  All tests passed!   (1261 passed；上一刀 1257)

$ dart analyze lib
  1 issue found.   （component/ui/ios_settings_ui.dart:104 既有 info，与 E2EE 无关）

$ git diff --stat lib/
  （无输出：本刀不改生产代码）
```

imboy 侧未改动，`make e2ee-verify` 本刀不适用。

---

## 6. 残留风险（E2EE-062 仍未完成）

1. **⚠️ fallback key 轮换未实施** —— 本刀确认了缺口，未修。见 §3/§4；
2. **`forgetFallbackKey()` 从未被调用** —— 旧私钥长期留在 pickle 里。
   与残留 1 同一刀处理；
3. 服务端 fallback 签名仍非必填；客户端 fallback 只覆盖注册/登录路径；
4. `report_identity` 每次登录仍无条件上报；其 signature 只校验非空、未验证；
5. 告警规则未做；`/metrics` 输出未实证；
6. 被拦下的重发行仍被扫描器每轮捡起（不写库、不出网）；滞留后 UX 无具体提示；
7. 幂等/补传链路端到端未实证；单租户/全局限流未做；租约无独立 TTL；
   60/min 未压测；进程重启后重投仍消费新 OTK；客户端无 batch_claim 调用方；
8. 真机双端未验证。

---

## 7. 认识论状态汇总

| 结论 | 状态 |
|---|---|
| 新账号生成前无 fallback key、生成后有一把 | **已实证**（对照组） |
| `markKeysAsPublished()` 后 `fallbackKey` 变空 | **已实证** |
| `generateFallbackKey()` 换出新 keyid（真轮换） | **已实证** |
| 轮换后旧 fallback key 仍可解密（旧私钥保留） | **已实证** |
| `generateFallbackKey` 全仓只在登录路径出现一次 | **已实证**（全仓 grep） |
| 全仓无 `forgetFallbackKey()` 调用 | **已实证**（全仓 grep） |
| 「长期不登出 → fallback key 永不轮换」 | **已实证**（由上两条直接推出） |
| 「这构成前向保密缺口」 | **推理**（基于上述实证 + Olm/Matrix 轮换惯例） |
| 「长期在线设备实际多久不登录」 | **未测**（部署侧事实） |
| 缺口已修复 | **否** —— 见 §4 |

---

## 8. 未做

- **未实施轮换**（§4）；未调用 `forgetFallbackKey()`。
- 不 push、不部署、不访问生产、不通知第三方。
- 未改 ADR / 协议规范；未动 E2EE-012/023/024/025/029 状态标记。
- 未删除、未 skip 任何测试；未新增依赖与迁移。
- **未改动任何生产代码**（本刀是纯评估）。
