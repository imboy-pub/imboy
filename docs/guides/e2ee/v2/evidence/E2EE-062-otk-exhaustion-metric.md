# E2EE-062 残留 3 第一刀：OTK 耗尽的可观测性

- **Slice**：E2EE-062 残留 3「耗尽告警 / 运维指标缺失」的**第一刀（指标）**
- **会话**：`20260729-1100-claude-code`
- **仓库**：`imboy`
- **状态**：指标已落地；**告警规则未做**（§5.1）。E2EE-062 整体仍为 `PARTIAL`

---

## 1. 做了什么

前八刀把幂等租约、目标级限流、batch 幂等、客户端补传、重发闸门都做了，
但残留里始终有一条：

> **耗尽告警 / 运维指标缺失 —— 补传是客户端自愈，运维侧对耗尽攻击仍然盲。**

服务端**其实知道每一次耗尽**：`claim_with_identity` 在
`olm_identity_ds:claim_one_time_key` 返回 `{error, exhausted}` 时回退到
fallback prekey ——**那一刻就是前向保密降级的瞬间**（该对端此后的新会话都复用
同一条 fallback prekey）。但这个事实此前**没有被记录到任何地方**：
没有计数、没有日志、没有指标。运维只能等用户报障。

项目已有 `elib_metric` 计数器 facility 并在生产使用
（`message_ds` 的 `msg_sent_total`、`msg_ack_logic` 的 `msg_delivered_total`），
经 `/metrics` 以 Prometheus 格式导出（`metrics_handler`）。本刀把耗尽事件接上去。

| 接缝 | 改动 |
|---|---|
| `src/logic/olm_identity_logic.erl` | `claim_with_identity/4` 与 `/5` 的耗尽分支各 +1 行埋点；两处 `no_prekey_available` 分支各 +1 行 |
| `Makefile` | e2ee-verify Modules 清单 +1 |

两个计数器：

| 指标 | 含义 |
|---|---|
| `olm_otk_exhausted_total` | OTK 池空，回退到 fallback prekey（**前向保密降级**） |
| `olm_prekey_unavailable_total` | **连 fallback 都没有**，claim 直接失败 |

分开计数是刻意的：两者严重程度不同，合并会让告警无法分级。

无迁移、无新依赖、无协议变更、无新增函数、**未改任何函数 arity**。

### 1.1 取舍：指标**不携带 uid**

Prometheus 指标会被抓取并长期留存。把 uid 放进标签有两个问题：

1. **基数无上限** —— 每个用户一条时间序列，会拖垮存储；
2. ⚠️ **「谁的池快空了」正是耗尽攻击要的择时信号。**
   第五刀的 `prekey_count` 端点**为此刻意不接受 uid 入参**
   （`E2EE-062-prekey-count-endpoint.md` §1.1：「余量本身不是秘密，
   但谁的池快空了是」）。指标端把同一信息漏出去，
   **等于从后门把那道设计推翻**。

因此只计聚合量。运维能知道「耗尽正在以多高的速率发生」，足够告警；
定位具体目标属事件响应，走别的途径。
守护用例：`metric_carries_no_uid_test_`（断言指标调用参数里不出现任一 uid）。

### 1.2 为什么两个函数子句各插一次而不是合并

`claim_with_identity/4`（旧语义）与 `/5`（幂等路径）是两个独立子句。
把它们合并成「旧的委托新的」会让**按 arity 挂 meck 期望的既有测试静默穿透**
到真实实现——本项目已两次踩此坑（A2-a、E2EE-062 第一刀）。
故各插一行，接受这点重复。

---

## 2. RED 记录

新增 `test/logic/e2ee_otk_exhaustion_metric_tests.erl`（5 例）。

### 2.1 ⚠️ 初版是**假绿**：eunit instantiator 必须是 1 元 fun

第一次运行报 **`All 5 tests passed`** —— 但生产代码里一行埋点都还没加，
不可能通过。

根因：`{setup, Setup, Cleanup, Body}` 的 `Body` 必须是**1 元** instantiator
（接收 setup 的返回值）。我写成了 0 元 `fun() -> [...] end`，
eunit 于是把它当成**一个普通测试**直接执行——它只是返回了一个列表，
**内部断言一次都没运行**。整个文件"全绿"却什么都没验。

改成 `fun(_) -> Body() end` 后取得真 RED：

```
=======================================================
  Failed: 3.  Skipped: 0.  Passed: 2.
```

**这条比一次普通的 RED 更值得记**：对照组只能挡住「harness 坏了」，
挡不住「harness 根本没跑」。**「全绿」本身也需要被怀疑**——
一个刚写完、尚未实现功能就全绿的测试文件，第一反应应当是它没在执行。

### 2.2 3 红均为行为失败

| 用例 | 失败形态 |
|---|---|
| `exhausted_claim_emits_metric_test_` | `lists:member(olm_otk_exhausted_total, [])` 为 false |
| `exhausted_claim_with_request_id_emits_metric_test_` | 同上（幂等路径是另一个子句） |
| `no_prekey_emits_unavailable_metric_test_` | `olm_prekey_unavailable_total` 缺失 |

### 2.3 对照组与正向可用性

- **对照组** `healthy_claim_emits_no_exhaustion_metric_test_`：
  OTK 正常可领时**不得**计任何耗尽指标。改前改后**都绿**。
  它红就说明埋点打在了正常路径上——指标会恒为噪音，告警起不到作用。
- **正向可用性**（写在耗尽用例内部）：耗尽时仍必须**成功返回 fallback key**。
  一个「耗尽就报错」的实现在「能观测到耗尽」这个指标上**也满分**，被这条否掉。
- **安全** `metric_carries_no_uid_test_`：⚠️ 该用例在 RED 阶段是**空绿**
  （没有任何指标调用，自然不含 uid）。它只有在实现之后才有意义——
  这一点已在下方认识论表中标明。

---

## 3. 生产调用方核实

```
olm_handler:do_claim_key1 / do_batch_claim1
  → olm_identity_logic:claim_keys/3,/4
  → claim_with_identity/4,/5          ← 埋点在此
  → olm_identity_ds:claim_one_time_key
```

两条 claim 路由（`POST /api/v1/e2ee/olm/claim`、
`POST /api/v1/e2ee/devices/batch_claim`）都必经这两个子句。
测试打的是 `olm_identity_logic:claim_keys/3,/4` 这一层，其上是真实 handler。

指标导出侧 `elib_metric` → `metrics_handler:fetch_metrics/0` → `/metrics`
是既有生产链路，本刀未改。
**认识论状态：埋点被调用已实证；「计数最终出现在 `/metrics` 输出里」为文件级
阅读结论，未实证**（未起服务抓取 `/metrics`）。

---

## 4. 验收命令与结果

```
$ IMBOYENV=local make eunit t=e2ee_otk_exhaustion_metric_tests
  All 5 tests passed.

$ make e2ee-verify
  All 343 tests passed.        # 上一轮基线 338，本刀 +5
=== E2EE verify ALL PASSED ===

$ erlfmt --check src/logic/olm_identity_logic.erl test/logic/e2ee_otk_exhaustion_metric_tests.erl
  All matched files use erlfmt code style!

$ git diff --check
  （通过）
```

新模块是**后端单测**，按规定**已加进 Makefile 的 e2ee-verify Modules 清单**。
既有 `olm_identity_logic_tests`、`e2ee_otk_claim_idempotency_tests`
在门禁内全绿 → 埋点未改变任何既有行为。

---

## 5. 残留风险（E2EE-062 仍未完成）

### 5.1 本刀只做了「可观测」，没做「告警」

指标已产出，但**没有任何告警规则**（Prometheus rule / 阈值 / 通知渠道）。
运维仍需主动去看 `/metrics` 才知道在发生耗尽。
告警规则属部署侧配置（`imboy/deploy/`），且「多高的速率算异常」需要基线数据，
**本刀未做**。

### 5.2 未实证「指标出现在 `/metrics` 输出」

埋点被调用已实证；从 `elib_metric` 到 `/metrics` 文本输出这一段是既有链路，
本刀按文件阅读认定其可用，**未起服务抓取验证**。

### 5.3 其余残留（不变）

1. 被拦下的重发行仍会被扫描器每轮重新捡起（**不写库、不出网**，仅 find+判定+日志）；
2. 滞留后 UX 无具体提示；
3. 幂等 / 补传链路端到端未实证；
4. 单租户/全局两层限流未做；租约无独立 TTL；**fallback prekey 未在服务端验签**；
   60/min 未压测校准；进程重启后重投仍消费新 OTK；客户端无 batch_claim 调用方；
5. 真机双端未验证。

---

## 6. 认识论状态汇总

| 结论 | 状态 |
|---|---|
| OTK 耗尽回退 fallback 时会计 `olm_otk_exhausted_total` | **已实证** |
| 幂等路径（`claim_keys/4`）同样计数 | **已实证**（独立子句，单独用例） |
| 连 fallback 缺失时另计 `olm_prekey_unavailable_total` | **已实证** |
| 正常路径不计耗尽指标（对照组） | **已实证** |
| 耗尽时仍成功返回 fallback（正向可用性） | **已实证** |
| 指标不携带 uid | **已实证**（但该用例在 RED 阶段是空绿，见 §2.3） |
| 既有行为未被改变 | **已实证**（门禁 343 全绿） |
| 计数最终出现在 `/metrics` 输出 | **文件级阅读结论，未实证** |
| 「运维侧不再对耗尽攻击盲」 | **部分成立** —— 有指标、无告警（§5.1） |

---

## 7. 未做

- 不 push、不部署、不访问生产、不通知第三方。
- 未改 ADR / 协议规范；未动 E2EE-012/023/024/025/029 状态标记。
- 未删除、未 skip 任何测试；未放宽任何安全策略；未新增依赖与迁移。
- **未改任何函数 arity**（见 §1.2）。
- 未做告警规则（§5.1）。
