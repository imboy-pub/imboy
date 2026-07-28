# E2EE-062：限流 scope 的配置守护

- **Slice**：把「删掉一行配置就静默关掉整道 OTK 限流」这个已实证的失效模式钉死
- **会话**：`20260729-2000-claude-code`
- **仓库**：`imboy`
- **状态**：守护已落地并实证。E2EE-062 整体仍为 `PARTIAL`
- **本刀不改任何生产代码**（只新增测试 + 门禁清单一行）

---

## 0. 先否掉一个看似更该做的选项：告警规则

上一刀把「告警规则」列为「运维不再盲」的最后一段。本轮先评估了它，**决定不做**：

| 障碍 | 事实 |
|---|---|
| 阈值需要基线数据 | `deploy/prometheus/rules/imboy-alerts.yml` 已有 12+ 组告警，但**零条 E2EE/Olm 相关**；耗尽速率多高算异常，本项目没有任何观测数据 |
| PromQL 无法验证 | `promtool` 在本机不可用（`which promtool` 无输出），仓库与 CI 里也没有 `check rules` 步骤 |

写一条**猜出来的阈值** + **验不了语法**的规则，是「看起来像进展」的伪工作。
**记为需人工/部署侧介入，不自行编造。**

---

## 1. 缺口：一行配置即可静默关掉整道限流

第二刀实证过（`E2EE-062-per-target-throttle.md` §4.1）：
`throttle:check/2` 遇到**未注册的 scope 返回原子 `rate_not_set`，不崩**。
生产代码 `olm_handler:scope_limited/2` 因此显式识别它、打 ERROR 日志，
但**仍然放行**——这是刻意的取舍（scope 缺失是配置错误而非攻击，
拒掉全部 claim 会让 E2EE 建会话不可用）。

净效果：

> **从 `config/sys.config` 里删掉一行，整道 OTK 限流就消失了**，
> 唯一的信号是一条运行时 ERROR 日志——**没有任何测试会红**。

E2EE-062 前八刀的抗耗尽工作（幂等租约、目标级限流、batch 幂等、耗尽指标）
全部建立在 `olm_claim` 与 `olm_claim_target` 这两个 scope 之上。

新增 `test/api/e2ee_throttle_scope_config_tests.erl`（4 例，**已入 e2ee-verify 门禁**）。

---

## 2. RED 记录 —— 空验证就是「执行那个要防的操作」

生产配置本来就对，直接跑必然全绿。RED 用空验证取得：
**从 `config/sys.config` 里临时删掉 `{olm_claim_target, 60, per_minute}` 那一行**
——也就是本守护要防的那个操作本身。

```
  Failed: 2.  Skipped: 0.  Passed: 2.
```

存在性与单位两条**同时**变红；对照组与执行性用例仍绿。
恢复后 `git diff --stat config/sys.config` 无输出，复跑 4/4。

### 2.1 四条用例各自的职责

| 用例 | 挡住的失效 |
|---|---|
| **对照组** 未注册 scope 返回 `rate_not_set` | **本守护的全部理由就是这个返回值。** 它一旦不成立，"删一行就静默关掉限流"的风险模型需要重估——那时该重写的是守护本身，不是配置 |
| 两个 scope 存在于**随发布的** `sys.config` | 有人删掉/改名 |
| 两个 scope 的周期是 `per_minute` | 见 §2.2 |
| **正向可用性 / 单位正确性**：声明的数字确实被强制执行 | 见 §2.2 |

### 2.2 为什么「存在」还不够

一个 scope **在**、但配额根本不是声明的那个（例如把 `per_minute` 误写成
`per_second`），**既不会触发 `rate_not_set`、也不会有任何日志**——
前两条用例全绿，而实际防护完全变形。

故第四条用一个独立的探针 scope（3/min）验证**「第 3 次仍放行、第 4 次才拒」**。
它同时承担正向可用性职责：配额内的调用必须**全部放行**，
否则那是个「一律拒绝」的实现，在"有限流"这个指标上恒得满分。

### 2.3 读的是随发布的那份配置

测试用 `file:consult("config/sys.config")` 直接读**随发布走的那份**，
而不是 `application:get_env`（测试环境加载的是 gitignored 的
`sys.local.config`，守护它没有意义）。

---

## 3. 生产调用方核实

```
olm_handler:do_claim_key / do_batch_claim / target_rate_limited
  → scope_limited(olm_claim | olm_claim_target, Key)
  → throttle:check/2        ← 依赖 sys.config 的 rates 声明
```

被守护的两个 scope 名与生产代码里的字面量一致；
`scope_limited/2` 的行为（含 `rate_not_set` 分支）已由
`e2ee_claimant_scope_drift_tests` 与 `e2ee_otk_target_throttle_tests` 覆盖。
本刀补的是**配置侧**这一环。

---

## 4. 验收命令与结果

```
$ IMBOYENV=local make eunit t=e2ee_throttle_scope_config_tests
  All 4 tests passed.

$ make e2ee-verify
  All 361 tests passed.        # 上一轮基线 357，本刀 +4
=== E2EE verify ALL PASSED ===

$ erlfmt --check test/api/e2ee_throttle_scope_config_tests.erl
  All matched files use erlfmt code style!

$ git diff --check
  （通过）
```

imboyapp 侧未改动。

---

## 5. 残留风险

1. **告警规则仍未做** —— 需基线数据定阈值 + `promtool`/CI 验证 PromQL（§0）。
   **这仍是"运维不再盲"的最后一段，且现在明确记为需人工/部署侧介入。**
2. **本守护只覆盖两个 OTK scope** —— `sys.config` 里还有
   `e2ee_report_key` / `e2ee_backup` / `e2ee_transfer` 等 E2EE 相关 scope，
   同样会因删行而静默失效。本刀**刻意只钉 E2EE-062 直接依赖的两个**，
   避免把守护写成「把整份配置抄一遍」那种一改就红的噪音测试。
   **认识论状态：其余 scope 的同类风险为推理（同一机制），未逐个实证。**
3. **常量双写** —— 期望值（30/60 per_minute）同时存在于 `sys.config` 与测试宏。
   这是刻意的：守护的价值正来自「两处必须同时改」这个摩擦。
4. E2EE-062 其余残留不变（fallback 签名非必填、留存期 ≈2 周期、端到端未实证、
   单租户/全局限流、60/min 未压测、真机等）。

---

## 6. 认识论状态汇总

| 结论 | 状态 |
|---|---|
| 未注册 scope 返回 `rate_not_set` | **已实证**（对照组，本轮复验） |
| `olm_claim` / `olm_claim_target` 存在于随发布配置且值符合预期 | **已实证** |
| 两者周期均为 `per_minute` | **已实证** |
| 声明的数字确实被强制执行（第 N 放行、N+1 拒） | **已实证**（独立探针 scope） |
| 守护非空转（删掉一行即红） | **已实证**（空验证＝执行那个要防的操作） |
| 其余 E2EE scope 存在同类风险 | **推理**（同一机制），未逐个实证 |
| 「60/min 是正确的阈值」 | **仍未验证** —— 本刀验的是"声明与执行一致"，不是"数字选得对" |
| 「运维不再对耗尽攻击盲」 | **仍不成立** —— 缺告警规则 |

---

## 7. 未做

- 不 push、不部署、不访问生产、不通知第三方。
- 未改 ADR / 协议规范；未动 E2EE-012/023/024/025/029 状态标记。
- 未删除、未 skip 任何测试；未新增依赖与迁移。
- **未改动任何生产代码与配置**（只加测试与门禁清单一行）。
- **未写告警规则**（§0，需基线数据与 promtool 验证）。
