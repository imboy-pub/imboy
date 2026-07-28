# E2EE-062：耗尽计数确实出现在 Prometheus 导出里

- **Slice**：耗尽埋点那一刀的残留「未实证：计数最终出现在 `/metrics` 输出」
- **会话**：`20260729-1800-claude-code`
- **仓库**：`imboy`
- **状态**：该残留**已闭合**。E2EE-062 整体仍为 `PARTIAL`
- **本刀不改任何生产代码**（只新增测试 + 门禁清单一行）

---

## 0. 先处理上一刀点名的「下一件」：`report_identity` 验签

上一刀把它列为下一件，但同时标注「**价值需先界定**」。本轮先做了这个界定：

| 事实 | 出处 | 状态 |
|---|---|---|
| **对端客户端已经在验这个签名** | `olm_session_service.dart:658` `_verifyIdentitySignature`，在 `_establishOutboundSession` 内 | **已实证** |
| 服务端再验只能证明 (ed25519, curve25519, sig) 三元组**内部一致** | 签名由客户端用自己的 ed25519 对自己的 curve25519 生成 | 推理 |
| 被盗 token 的攻击者可自造一致三元组 | 三个值都由其控制 | 推理 |

**结论：低价值，降优先级。** 它既拦不住实际威胁（身份替换），又与对端已有的检查重复；
真正的防护在客户端 TOFU 与 KT（E2EE-065，被签字阻塞）。
定位至多是「边界快速失败 / 数据卫生」。故本轮改做价值更高的一项。

---

## 1. 做了什么

耗尽埋点那一刀把「计数最终出现在 `/metrics` 输出」明确标为
**「文件级阅读结论，未实证」**——埋点被调用已实证，但
`elib_metric` → `metrics_handler` 这一段是照文件阅读认定的。

**一个只增不导出的计数器等于没有计数器**：运维那边永远是零，而上一刀正是为了
让运维不再对耗尽攻击盲。本刀补上这段实证。

新增 `test/api/e2ee_otk_metric_exposition_tests.erl`（3 例，**已入 e2ee-verify 门禁**）。

⚠️ **本文件不 mock `elib_metric`**——它正是被测链路的一环。用真的 gen_server
（`ensure_metric_server/0` 在未启动时 `start_link`）。只 mock DS 层制造"耗尽"。

---

## 2. RED 记录 —— 空验证

生产代码（埋点）在上一刀已写好，直接跑必然全绿，**改前改后都绿的测试没有价值**。
故 RED 用空验证取得：把 `olm_identity_logic` 里两处
`elib_metric:increment(olm_otk_exhausted_total)` 临时注释掉。

```
  Failed: 1.  Skipped: 0.  Passed: 2.
```

**唯独第三条（生产耗尽路径的计数确实出现在导出里）变红**，两条对照组仍绿。
恢复后 `git diff --stat src/logic/olm_identity_logic.erl` 无输出。

### 2.1 两条对照组各自挡什么

| 对照组 | 挡住的失效 |
|---|---|
| `导出器把拿到的计数原样导出` | 导出器本身坏了 / 我调用方式不对。它红则后两条的绿毫无意义 |
| `未被计数的指标不得出现在导出里` | **一个"把所有已知名字都打印一遍"的导出器，在"指标出现了"这个断言上恒得满分**。这条把它否掉 |

### 2.2 断言从「文本里有没有」收紧为「计数是否递增」

初版第三条断言的是导出文本里**是否出现**该指标名。问题：同一个 VM 里若别的路径
早已把该计数器加过，"出现了"会**恒真**，测不出任何东西。

改为读 `get_all_metrics()` 里的**计数值**，断言 `Before + 1`，
再附加断言导出文本里含该指标名。这样既与执行顺序无关，又覆盖了导出这一段。

---

## 3. 生产调用方核实

```
olm_handler:do_claim_key1 → olm_identity_logic:claim_keys/3
  → claim_with_identity/4（OTK 耗尽分支）
  → elib_metric:increment(olm_otk_exhausted_total)     ← 被测链路起点
  → elib_metric:get_all_metrics()                       ← 真 gen_server
  → metrics_handler:format_prometheus/1                 ← 真导出器
```

测试走的是 `olm_identity_logic:claim_keys/3`（`olm_handler` 的下探点），
不是内部私有函数。

⚠️ **未覆盖 HTTP 层**：`metrics_handler:init/2` 的内网 IP 门与 cowboy 响应未纳入。
那一段与"计数有没有被导出"正交。**认识论状态：`increment → get_all_metrics →
format_prometheus` 已实证；`init/2` 的 IP 门与响应组装未实证。**

---

## 4. 验收命令与结果

```
$ IMBOYENV=local make eunit t=e2ee_otk_metric_exposition_tests
  All 3 tests passed.

$ make e2ee-verify
  All 354 tests passed.        # 上一轮基线 351，本刀 +3
=== E2EE verify ALL PASSED ===

$ erlfmt --check test/api/e2ee_otk_metric_exposition_tests.erl
  All matched files use erlfmt code style!

$ git diff --check
  （通过）
```

imboyapp 侧未改动。

---

## 5. 残留风险（E2EE-062 仍未完成）

1. **告警规则仍未做** —— 指标现在确实导出了，但没有任何 Prometheus rule /
   阈值 / 通知渠道。运维仍需**主动去看** `/metrics`。
   属部署侧配置（`imboy/deploy/`）且需基线数据。**这是"运维不再盲"的最后一段。**
2. **HTTP 层未覆盖**（§3）。
3. `report_identity` 验签 —— 已界定为低价值，降优先级（§0）。
4. 服务端 fallback 签名仍非必填（等客户端普及后才能改必填）。
5. fallback key 留存期 ≈2 个轮换周期；7 天周期未针对本项目论证；
   完全不收消息的设备不会轮换。
6. 被拦下的重发行仍被扫描器每轮捡起（不写库、不出网）；滞留后 UX 无具体提示。
7. 幂等/补传链路端到端未实证；单租户/全局限流未做；租约无独立 TTL；
   60/min 未压测；进程重启后重投仍消费新 OTK；客户端无 batch_claim 调用方。
8. 真机双端未验证。

---

## 6. 认识论状态汇总

| 结论 | 状态 |
|---|---|
| 导出器把拿到的计数原样导出 | **已实证**（对照组） |
| 导出器不会无中生有 | **已实证**（对照组） |
| 生产耗尽路径使计数**恰好 +1** | **已实证** |
| 该计数出现在 Prometheus 导出文本里 | **已实证** |
| 测试非空转（摘掉埋点即红） | **已实证**（空验证） |
| `metrics_handler:init/2` 的 IP 门与响应组装 | **未实证**（与本结论正交） |
| 对端客户端已验 identity 签名 | **已实证** |
| 「服务端再验 identity 签名价值低」 | **推理**（基于上一条 + 攻击者可自造一致三元组） |
| 「运维不再对耗尽攻击盲」 | **仍不成立** —— 缺告警规则（§5.1） |

---

## 7. 未做

- 不 push、不部署、不访问生产、不通知第三方。
- 未改 ADR / 协议规范；未动 E2EE-012/023/024/025/029 状态标记。
- 未删除、未 skip 任何测试；未新增依赖与迁移。
- **未改动任何生产代码**（只加测试与门禁清单一行）。
- 未做告警规则（§5.1）；未做 `report_identity` 验签（§0，已降优先级）。
