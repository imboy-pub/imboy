# E2EE-062 第七刀：per-claimant 限流的配置漂移可见性

- **Slice**：`22-...state.md` §1.1 队列第 3 项的第七刀（残留 ⑥ 中已实证存在的一项）
  - 第一~六刀见 `E2EE-062-otk-claim-idempotent-lease.md` /
    `E2EE-062-per-target-throttle.md` / `E2EE-062-batch-claim-idempotency.md` /
    `E2EE-062-client-request-id.md` / `E2EE-062-prekey-count-endpoint.md` /
    `E2EE-062-client-refill-wiring.md`
- **会话**：`20260729-0100-claude-code`
- **仓库**：`imboy`
- **状态**：本刀完成；**E2EE-062 整体仍为 `PARTIAL`**（残留见 §5）

---

## 1. 做了什么

第二刀实证发现：`throttle:check/2` 遇到**未注册的 scope** 返回原子 `rate_not_set`
（不崩），于是朴素写法

```erlang
case throttle:check(Scope, Key) of
    {limit_exceeded, _, _} -> 拒;
    _ -> 放行
end
```

会把它当成「未超限」**静默放行**——`sys.config` 少写一条 scope，整道限流就无声
消失且没有任何信号。当时**只修了目标层**（`target_rate_limited/1`），并把
per-claimant 那道门明确记为「未动，列入残留」
（`E2EE-062-per-target-throttle.md` §4.1 与 §5 第 9 项）。

`olm_claim` 是 claim / batch_claim 两条路径上的**第一道**门。它无声失效意味着
单账号高频 claim 完全不受限，目标层的 60/min 成为唯一防线。

| 接缝 | 改动 |
|---|---|
| `src/api/olm_handler.erl` | 新增 `scope_limited/2` 作为**所有** OTK 限流门的唯一判定点；`do_claim_key/2`、`do_batch_claim/2`、`target_rate_limited/1` 全部改走它 |

无迁移、无新依赖、无协议变更、无新配置项。

### 1.1 取舍一：收敛成一个判定函数，而不是把修复复制到第二处

两道门此前各写一份 `case`。目标层修过、领取方层没修——**正是这种复制粘贴让同一个
缺陷在一处修好、在另一处继续存在**。若本刀只是把 `rate_not_set` 分支再抄一遍，
下一次加第三道门（playbook 还要求单租户/全局两层）时会重演同一件事。
故收敛为 `scope_limited/2`，新增门只需调用它。

日志标签随之从 `{olm_claim_target_scope_missing, olm_claim_target}` 改为
`{olm_throttle_scope_missing, Scope}`。已 grep 确认全仓无任何测试或代码断言旧标签
（唯一出现处就是被替换的那一行）。

### 1.2 取舍二：仍然**不**改成 fail-closed

scope 缺失是**配置错误**而非攻击。拒掉全部 claim 会让整个 E2EE 建会话不可用，
代价远大于「限流暂时失效」。因此该情形仍放行，只是必须留下 ERROR 信号。
守护用例同时钉死这两面（§2.1）。

### 1.3 取舍三：正常路径不得打这条日志

若对每次 `check` 都打日志，配置漂移的信号会被正常流量淹没，等于没有信号。
对照组 `claim_key_healthy_scope_is_silent_test_` 钉死这一点。

---

## 2. RED 记录

新增 `test/api/e2ee_claimant_scope_drift_tests.erl`（5 例）。
通过 meck `elib_log:internal_log/4`（`?ERROR_LOG` 宏的展开目标，见
`include/log.hrl:19`）把「是否留下信号」变成**可观测的行为**，而不是读源码断言。

```
=======================================================
  Failed: 2.  Skipped: 0.  Passed: 3.
```

**2 红均为行为失败**：

| 用例 | 失败形态 |
|---|---|
| `claim_key_missing_claimant_scope_is_visible_test_` | `has_claimant_scope_signal(Logs)` 为 false —— scope 缺失被静默吞掉 |
| `batch_claim_missing_claimant_scope_is_visible_test_` | 同上（batch 路径走同一道门） |

**3 绿全部是对照组**，改前改后都必须绿：

- `claim_key_healthy_scope_is_silent_test_` —— scope 正常时不打这条日志；
- `claim_key_limit_exceeded_still_429_test_` / `batch_claim_..._still_429_test_`
  —— 超限仍 429，本刀不得削弱既有行为。

对照组全绿 → harness 本身没坏，2 红是真缺口。

### 2.1 「只验拒收」反模式的规避

一个「scope 缺失就拒掉全部 claim」的实现在**可见性**指标上也能满分（顺带把日志
打了），但它会让 E2EE 建会话在一次配置漂移下全面不可用。
两条缺失-scope 用例因此同时断言 **`{responded, success}`** ——
请求必须照常放行。这是本刀的正向可用性用例。

信号判定只认 `{_Tag, olm_claim}`：目标层的 `olm_claim_target` 是第二刀已有的信号，
不能拿来充数。

---

## 3. 生产调用方核实

| 被测入口 | 生产路由 |
|---|---|
| `olm_handler:init/2` `action => claim_key` | `POST /api/v1/e2ee/olm/claim` |
| `olm_handler:init/2` `action => batch_claim` | `POST /api/v1/e2ee/devices/batch_claim` |

`scope_limited/2` 有三个生产调用方（`do_claim_key/2`、`do_batch_claim/2`、
`target_rate_limited/1`），全部在上述两条路由的必经路径上。测试打的是真实
`init/2` 入口，不是内部私有函数。

---

## 4. 验收命令与结果

```
$ make e2ee-verify
  All 333 tests passed.
=== E2EE verify ALL PASSED ===
```

上一刀 328 → **333**（本刀 +5）。新模块 `e2ee_claimant_scope_drift_tests`
**已加进 Makefile Modules 清单**。

既有的 `olm_handler_claim_throttle_tests`（per-claimant 层守护）与
`e2ee_otk_target_throttle_tests`（含目标层的 `rate_not_set` 守护）
在门禁内**全绿**——重构未改变任一既有行为。

`erlfmt --check` 改动文件通过；`git diff --check` 通过。

本刀不涉及数据库，无真 PG 集成测试。

---

## 5. 残留风险（E2EE-062 仍未完成）

1. **耗尽告警 / 运维指标缺失** —— 有了配置漂移的 ERROR 日志，但仍无「某用户池
   见底」的指标或告警。运维侧对耗尽攻击本身仍然盲。
2. **端到端未实证** —— 幂等链路与补传链路的服务端半边（真 PG）与客户端半边
   （单测）各自实证，两半拼接只有文件级论证。
3. **「耗尽/限流绝不触发 RSA/Megolm/明文」无守护用例** —— 残留 ⑥ 中**尚未做**
   的一项，且是安全性最高的一项。**认识论状态：未实证。**
4. **单租户 / 全局两层限流未做**（有意识缺口，网关承担更合适）。
   新增门时只需调用 `scope_limited/2`，不会重演本刀修的这个缺陷。
5. **租约无独立 TTL**；**fallback prekey 未在服务端验签**（未实证）。
6. **60/min 阈值未经压测校准**（推理值）。
7. `config/sys.local.config` 是 gitignored；本地 scope 缺失现在至少有 ERROR 日志。
8. 进程重启后重投仍消费新 OTK（第四刀的有意识取舍）；
   客户端无 batch_claim 调用方（全仓 grep 零命中）。
9. 真机双端未验证。

---

## 6. 认识论状态汇总

| 结论 | 状态 |
|---|---|
| `olm_claim` scope 缺失会留下 ERROR 信号 | **已实证**（meck `elib_log:internal_log/4`，真实 `init/2` 入口） |
| 该情形仍照常放行（正向可用性） | **已实证** |
| scope 正常时不打该日志 | **已实证**（对照组） |
| 超限仍 429（既有行为未削弱） | **已实证**（对照组 + 既有两个套件全绿） |
| 三处门收敛到唯一判定点 | **已实证**（三个调用方 + 全套件绿） |
| 全仓无代码/测试断言旧日志标签 | **已实证**（grep 唯一命中即被替换那行） |
| 「OTK 抗耗尽」整体达成 | **不成立** —— 见 §5.1/§5.2/§5.3 |

---

## 7. 未做

- 不 push、不部署、不访问生产、不通知第三方。
- 未改 ADR / 协议规范；未动 E2EE-012/023/024/025/029 状态标记。
- 未删除、未 skip 任何测试；未放宽任何安全策略；未新增迁移、依赖与配置项。
