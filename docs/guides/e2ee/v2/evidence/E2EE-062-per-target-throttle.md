# E2EE-062 续：OTK claim 目标级限流

- **Slice**：`22-...state.md` §1.1 队列第 3 项的第二刀（第一刀见 `E2EE-062-otk-claim-idempotent-lease.md`）
- **会话**：`20260728-2020-claude-code`
- **仓库**：`imboy`
- **状态**：本刀完成；**E2EE-062 整体仍为 `PARTIAL`**（残留见 §5）
- **上位验收**：`21-...playbook.md` E2EE-025「不同攻击 request 触发目标级限流」

---

## 1. 做了什么

第一刀的幂等租约只挡住「**同一个** request_id 重放」。它挡不住两条路：

1. 攻击者每次换一个新 `request_id` —— 每次都是「新请求」，租约不命中；
2. **N 个账号协同** —— 每个账号都在自己的 per-claimant 配额（30/min）之内。

两者都能把**同一个目标**的 OTK 池抽干，把该用户所有新会话逼到复用同一条
fallback prekey（前向保密显著下降）。根因是限流的键选错了：
`throttle:check(olm_claim, CurrentUid)` 按**领取方**限流，与被抽干的**目标**无关，
换个号就绕过。

本刀补上以**目标 uid** 为键的第二层：

| 接缝 | 改动 |
|---|---|
| `src/api/olm_handler.erl` | `do_claim_key1/2` 与 `do_batch_claim1/2` 均加 `target_rate_limited/1` 门；新增该私有函数 |
| `config/sys.config` | 新增 scope `{olm_claim_target, 60, per_minute}` |
| `config/sys.local.config` | 新增 scope `{olm_claim_target, 120, per_minute}`（本地放宽，便于集成测试） |

### 1.1 三处设计取舍（按「选安全那个」裁决，未询问）

**取舍一：限流在参数校验之后才计入配额。**
`TargetLimited` 虽然在 `case` 之前求值，但只有走到 `true when TargetLimited`
分支（即 target_uid 已合法）才会返回 429；非法 target_uid 走 `false` 分支返 400。
`target_rate_limited/1` 对非 `pos_integer` 直接返回 `false`，**不调 throttle**。
理由：否则攻击者用畸形请求就能凭空消耗某个目标的预算，把合法用户挡在门外——
限流本身变成拒绝服务工具。

**取舍二：60/min（生产）。**
OTK 池典型 100 条。60/min 把满池的抽空时间抬到分钟级，给客户端低水位补传留窗口；
同时远高于任何真实的「新会话建立」速率（正常用户一天也到不了 60 个新对端设备）。
没有取更严的值，是因为**耗尽本身不拒服务**——还有 signed fallback 兜底，
限流的职责是「拖慢到可补充」，不是「杜绝」。取更严只会伤可用性。

**取舍三：只做单目标一层，不做单租户 / 全局。**
playbook 要求四层。单机部署下「单租户」与「全局」等价，而全局面更应由网关承担
（应用层做全局限流会把正常高峰误杀）。已在 `target_rate_limited/1` 上写
`ponytail:` 注释标明升级触发条件（真有多租户部署、或压测暴露出全局面）。
**这是有意识的缺口，不是遗漏**，同时列进 §5 残留。

---

## 2. RED 记录

新增 `test/api/e2ee_otk_target_throttle_tests.erl`（5 例）。

```
=======================================================
  Failed: 4.  Skipped: 0.  Passed: 1.
```

**4 红 = 4 个真实缺口**（全部是行为失败，不是编译错误）：

| 用例 | 失败形态 |
|---|---|
| `claim_key_checks_target_scope_test_` | `lists:member({olm_claim_target, 200}, Scopes)` 为 false —— 从未按目标 uid 限流 |
| `claim_key_target_limited_returns_429_test_` | 目标层超限却仍到达 logic（`must_not_reach_logic_when_target_limited`）——**耗尽向量逐字复现** |
| `batch_claim_checks_target_scope_test_` | 同上（batch 路径） |
| `batch_claim_target_limited_returns_429_test_` | 同上 |

**1 绿 = 对照组**：`claim_key_claimant_limit_still_enforced_test_` —— per-claimant
那道门今天就生效、改完必须仍生效。它在改前改后都绿 → harness 本身没坏。

### 2.1 「只验拒收」反模式的规避

两条 scope 断言用例同时断言 **`{responded, success}`**：两层都未超限时请求必须
**照常到达 logic 并成功**。这是正向可用性用例——一个「一律返回 429」的实现在
限流指标上恒得满分，被这条否掉。

关键用例 `claim_key_target_limited_returns_429_test_` 的 mock 里
**per-claimant 层显式返回 `ok`**（即领取方自己没超限），只有目标层超限。
这正是「N 个协同账号各自都在配额内」的建模——只有目标层能拦住它们。

---

## 3. 生产调用方核实

| 被测入口 | 生产路由 |
|---|---|
| `olm_handler:init/2` `action => claim_key` | `POST /api/v1/e2ee/olm/claim` |
| `olm_handler:init/2` `action => batch_claim` | `POST /api/v1/e2ee/olm/batch_claim` |

测试打的是 handler 的真实 `init/2` 入口（与既有 `olm_handler_claim_throttle_tests`
同一范式），不是内部私有函数。

---

## 4. 验收命令与结果

```
$ make e2ee-verify
  All 315 tests passed.
=== E2EE verify ALL PASSED ===
```

上一轮 309 → **315**（本刀 +6）。新模块 `e2ee_otk_target_throttle_tests`
**已加进 Makefile Modules 清单**。

### 4.1 意外实证发现：未注册 scope 会让限流**无声消失**

写完实现后直接跑了一次隔离实验（不是静态推断）：

```
$ erl ... -eval 'application:set_env(throttle, rates, [{olm_claim, 30, per_minute}]),
                 application:ensure_all_started(throttle),
                 io:format("~p~n", [catch throttle:check(olm_claim_target, 200)])'
unknown_scope_result=rate_not_set
```

`throttle:check/2` 遇到**未注册的 scope 返回原子 `rate_not_set`，不崩**。
朴素写法 `case ... of {limit_exceeded,_,_} -> true; _ -> false end` 会把它
当成「未超限」**静默放行** —— 也就是说 `sys.config` 少写一条 scope，
整道限流就无声消失，且没有任何信号。**既有的 `olm_claim` 那道门也有同一问题。**

处置：显式识别该原子并打 `?ERROR_LOG`，让配置漂移可见。
**不**改成 fail-closed —— scope 缺失是配置错误而非攻击，拒掉全部 claim 会让整个
E2EE 建会话不可用，代价远大于「限流暂时失效」。
守护用例：`claim_key_missing_scope_degrades_visibly_test_`。

⚠️ 已知未处理：`olm_claim`（per-claimant）那道门仍是朴素写法，同样会静默失效。
本刀未动它以免扩大爆炸半径，列入 §5 残留。

```
$ IMBOYENV=local make eunit t=e2ee_otk_claim_idempotency_integration_tests \
    EUNIT_ERL_OPTS="-config config/sys.local -pa ebin -pa test"
  Application imboy started on node nonode@nohost
  All 5 tests passed.
```

这条同时是**新 throttle scope 的落地验证**：应用带着
`{olm_claim_target, 120, per_minute}` 正常启动、throttle 未因未知 scope 崩溃、
第一刀的幂等租约用例仍全绿。**已实证。**

`git diff --check` 通过；`erlfmt --check` 改动文件通过。
既有 `olm_handler_claim_throttle_tests`（per-claimant 层守护）在 e2ee-verify 内全绿。

---

## 5. 残留风险（E2EE-062 仍未完成）

1. **单租户 / 全局两层限流未做**（取舍三，有意识缺口）。
2. **`batch_claim` 未接幂等租约** —— `batch_claim_keys/3` 逐设备调 `claim_keys/3`
   （无 request_id），多设备 fan-out 的重试仍逐次消费。目标层限流能拖慢它，
   但幂等性缺口依旧。**认识论状态：文件级阅读结论，未实证。**
3. **租约无独立 TTL**，边界是审计保留期，过期后同 request_id 会重新消费。
4. **fallback prekey 未在服务端验签**（playbook 要求「身份验证通过」）。
   **认识论状态：文件级阅读结论，未实证。**
5. **「耗尽/限流绝不触发 RSA/Megolm/明文」无针对性守护用例。**
6. **低水位补充与耗尽告警缺失** —— 池见底时没有主动通知设备补传，也无运维指标。
   这是取舍二「限流只拖慢、靠补传恢复」的**前提条件**，目前**该前提尚不成立**：
   补传只发生在客户端自发上报时。这是本刀最重要的配套缺口。
7. **客户端未发送 `request_id`**，第一刀的幂等路径生产流量仍走不到。
8. **限流阈值未经压测校准**，60/min 是推理值不是实测值。
   **认识论状态：文件级推理，未实证。**
9. **`olm_claim`（per-claimant）门仍是朴素写法**，未注册 scope 时同样静默失效
   （见 §4.1）。本刀只修了目标层，未动它以免扩大爆炸半径。**已实证该行为存在。**
10. **`config/sys.local.config` 是 gitignored 的**，本次对它的修改不入库；
    其他开发者的本地配置若无该 scope，限流会静默失效（现在至少有 ERROR 日志）。
11. 真机双端未验证。

---

## 6. 认识论状态汇总

| 结论 | 状态 |
|---|---|
| claim / batch_claim 均按目标 uid 限流 | **已实证**（handler `init/2` 真实入口） |
| 目标层超限 → 429 且不到达 logic | **已实证** |
| 两层都未超限 → 照常成功（正向可用性） | **已实证** |
| per-claimant 层未被削弱 | **已实证**（对照组 + 既有套件全绿） |
| 新 throttle scope 可正常启动 | **已实证**（带 sys.local.config 启动 + 集成测试全绿） |
| 60/min 是合适阈值 | **文件级推理，未实证**（未压测） |
| 未注册 scope 返回 `rate_not_set` 而非崩溃 | **已实证**（隔离 erl 实验，见 §4.1） |
| 配置漂移现在可见（ERROR 日志） | **已实证**（守护用例 + 生产代码显式分支） |
| 「OTK 抗耗尽」整体达成 | **不成立** —— 见 §5 第 2/6/7 项 |

---

## 7. 未做

- 不 push、不部署、不访问生产、不通知第三方。
- 未改 ADR / 协议规范；未动 E2EE-012/023/024/025/029 状态标记。
- 未删除、未 skip 任何测试；未放宽任何安全策略。
