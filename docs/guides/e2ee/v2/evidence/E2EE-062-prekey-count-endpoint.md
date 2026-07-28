# E2EE-062 第五刀：OTK 余量查询端点

- **Slice**：`22-...state.md` §1.1 队列第 3 项的第五刀（残留 ③ 的**第一小刀**）
  - 第一刀 `E2EE-062-otk-claim-idempotent-lease.md`（单设备幂等租约）
  - 第二刀 `E2EE-062-per-target-throttle.md`（目标级限流）
  - 第三刀 `E2EE-062-batch-claim-idempotency.md`（batch 幂等）
  - 第四刀 `E2EE-062-client-request-id.md`（客户端发 `request_id`）
- **会话**：`20260728-2300-claude-code`
- **仓库**：`imboy`
- **状态**：本刀完成；**E2EE-062 整体仍为 `PARTIAL`**（残留见 §5）

---

## 1. 做了什么

第二刀的取舍二写着「限流只拖慢、靠补传恢复」。这条取舍有一个**前提**：
客户端能知道自己的 OTK 池见底并及时补传。**该前提今天不成立。**

- 客户端 `OlmApi.countPrekeys`（imboyapp `lib/store/api/olm_api.dart`）是
  **恒返回 0 的桩实现**，注释自承「准确的服务端计数需后端补 count 端点」；
- 服务端 `olm_identity_repo` / `olm_identity_ds` 的 `count_one_time_keys/2`
  **早已存在**（真 PG 集成测试一直在用），但**没有 logic 与 handler 承载点，
  也没有路由**——能力有，出口没有。

本刀把出口打通。

| 接缝 | 改动 |
|---|---|
| `src/logic/olm_identity_logic.erl` | 新增 `count_one_time_keys/2`（入参守卫 + 错误不降级为 0） |
| `src/api/olm_handler.erl` | 新增 `prekey_count` action + `do_prekey_count/2` |
| `src/imboy_router.erl` | 新增 `GET /api/v1/e2ee/olm/prekey_count` |

无迁移、无新依赖、无协议变更（纯新增只读端点）。

### 1.1 关键取舍一：查询对象**只取自 token，不接受任何入参**

自然的设计是 `?uid=&device_id=`。**没有采用。**

余量本身不是秘密，但「**谁的池快空了**」是。带参端点等于对外开放
「探测任意用户任意设备还剩多少 prekey」——攻击者据此可以精确判断耗尽攻击
何时奏效，正好补上第二刀目标级限流想拿掉的那个能力。
因此 uid 与 device_id 全部取自 token（`auth_ds:current_uid/1` /
`auth_ds:current_did/1`），请求参数**不参与查询**。
守护用例：`prekey_count_ignores_request_params_test_`（请求里塞入别人的
`uid` / `device_id`，断言 logic 仍以 token 的那一对被调用）。

### 1.2 关键取舍二：legacy token → fail-closed 403

token 未绑定 DID（`current_did` 返回 `<<>>`）时返回 `device_binding_required`
403，与其余 crypto 端点同一语义（E2EE-013）。
后果只是「该 token 拿不到补传信号」，不是功能不可用；而放行则需要凭入参猜设备，
正好回到 §1.1 拒绝的那条路。

### 1.3 关键取舍三：查询失败**不得降级为 0**

`count = 0` 是「该补传了」的有效信号。把 DB 故障也报成 0 会触发无谓的全量补传，
更糟的是让**真正的池见底**与**数据库故障**无法区分。
logic 层遇错打 `?ERROR_LOG` 并返回 `{error, <<"internal_error">>}`，
handler 转 500。守护用例：`prekey_count_error_is_not_zero_test_`
（单测）+ `count_reflects_consumption`（真 PG，断言非法入参得 `{error,_}` 而非 `{ok,0}`）。

---

## 2. RED 记录

新增 `test/api/e2ee_otk_count_tests.erl`（7 例）。

### 2.1 第一次 RED 暴露了一条**假绿**，已修正

```
第一次：Failed: 5.  Passed: 2.
```

2 绿中有一条是**假绿**：`prekey_count_respects_e2ee_gate_test_` 当时只断言
`?assertMatch({responded, error, _, _}, Result)` —— 端点**根本不存在**时返回的
`{responded, error, <<"not_found">>, 404}` 同样满足该模式。
按「对照组红 = harness 缺陷，立刻停下重估」的同一精神，此处是「绿得没有意义」，
一样要停下修。改为断言具体的 capability gate 码：

```erlang
?assertMatch({responded, error, _, ?ERR_FEATURE_DISABLED}, Result)
```

```
收紧后：Failed: 6.  Passed: 1.
```

**6 红均为行为失败**（全部实得 `{responded, error, <<"not_found">>, 404}`
——路由与 action 都不存在）：

| 用例 | 期望 |
|---|---|
| `prekey_count_returns_own_count_test_` | `#{<<"count">> => 17}` |
| `prekey_count_zero_is_success_test_` | `#{<<"count">> => 0}`（0 是信号不是错误） |
| `prekey_count_ignores_request_params_test_` | logic 被以 token 的 uid/did 调用 |
| `prekey_count_requires_device_binding_test_` | 403 `device_binding_required` 且不到达 logic |
| `prekey_count_respects_e2ee_gate_test_` | `?ERR_FEATURE_DISABLED`（5190） |
| `prekey_count_error_is_not_zero_test_` | 下层错误 → error，不得变成 count=0 |

**唯一的 1 绿是对照组**：`unknown_action_still_404_test_`（未知 action 仍走 404
分支）——改前改后都绿 → handler 的 action 分发本身没坏，6 红是真缺口。

### 2.2 「只验拒收」反模式的规避

三条正向可用性用例：合法请求返回 `count=17`；**`count=0` 必须是成功响应**
（一个「一律报错」或「见底即 500」的实现会被这条否掉，而 0 恰恰是补传要等的信号）；
非法入参不影响合法路径的返回值。

---

## 3. 生产调用方核实

| 被测入口 | 生产路由 |
|---|---|
| `olm_handler:init/2` `action => prekey_count` | `GET /api/v1/e2ee/olm/prekey_count` |

路由已注册在 `src/imboy_router.erl:160`，与 `claim`（159 行）同一段认证路由内。
链路：`do_prekey_count/2 → olm_identity_logic:count_one_time_keys/2 →
olm_identity_ds → olm_identity_repo`。

⚠️ **该端点目前没有客户端调用方**。imboyapp 的 `OlmApi.countPrekeys` 仍是恒 0
桩实现，本刀只打通了服务端出口，**补传闭环尚未闭合**——见 §5.1。
这是范围事实，已如实记录，不是「端点建在生产不走的旁路上」：
路由真实注册、handler 是真实入口，缺的是下一刀的客户端接线。

---

## 4. 验收命令与结果

```
$ make e2ee-verify
  All 328 tests passed.
=== E2EE verify ALL PASSED ===
```

上一刀 321 → **328**（本刀 +7）。新模块 `e2ee_otk_count_tests`
**已加进 Makefile Modules 清单**。

```
$ IMBOYENV=local make eunit t=e2ee_otk_claim_idempotency_integration_tests \
    EUNIT_ERL_OPTS="-config config/sys.local -pa ebin -pa test"
  All 7 tests passed.
```

上一刀 6 → **7**：新增 `count_reflects_consumption`，在真 PG 上证明

- 余量随消费**真的**递减（5 → 4）；
- **幂等重放不改变余量**（重放后仍 4）——否则补传信号会被重放次数污染；
- 未注册设备返回 `{ok, 0}`（0 是合法答案）；
- 非法入参（空 device_id / uid=0）返回 `{error, _}` 而**非** `{ok, 0}`（§1.3）。

该集成模块**不在** `e2ee-verify` 硬门禁内（无 DB 时会 skip）；手动命令即上方那条。

`erlfmt --check` 改动文件通过；`git diff --check` 通过。

---

## 5. 残留风险（E2EE-062 仍未完成）

1. **补传闭环未闭合（本刀的直接后继）** —— 服务端出口已开，但客户端
   `OlmApi.countPrekeys` 仍是恒 0 桩实现，`_refillOneTimeKeys` 拿不到真实余量。
   **下一刀就是它。** **已实证**（读该函数体 + 全仓无调用方）。
2. **耗尽告警缺失** —— 有了 count 端点后，服务端侧仍无「某用户池见底」的运维
   指标/告警。本刀只做查询，未做可观测性。
3. **端到端未实证**（第四刀残留）—— 幂等链路服务端半边真 PG 实证、客户端半边
   单测实证，两半拼接只有文件级论证。
4. 进程重启后重投仍消费新 OTK（第四刀的有意识取舍）。
5. 客户端**无 batch_claim 调用方**（全仓 grep 零命中），第三刀的 batch 幂等
   暂无生产流量。
6. 租约无独立 TTL；fallback prekey 未在服务端验签；
   「耗尽/限流绝不触发 RSA/Megolm/明文」无守护用例；
   单租户/全局两层限流未做；`olm_claim` 门仍朴素写法；60/min 未压测校准。
7. 真机双端未验证。

---

## 6. 认识论状态汇总

| 结论 | 状态 |
|---|---|
| 端点返回本用户本设备余量 | **已实证**（真实 `init/2` 入口） |
| 请求入参不影响查询对象 | **已实证** |
| legacy token → 403 且不到达 logic | **已实证** |
| e2ee 关闭 → 5190（不是 404） | **已实证**（收紧后） |
| 查询失败不降级为 0 | **已实证**（单测 + 真 PG） |
| 余量随消费递减、幂等重放不改变余量 | **已实证**（真 PG） |
| 路由已注册且在认证段内 | **已实证**（`imboy_router.erl:160`，与 claim 同段） |
| 该端点有客户端调用方 | **不成立** —— 见 §3 / §5.1 |
| 「低水位补传前提成立」 | **不成立** —— 见 §5.1 |

---

## 7. 未做

- 不 push、不部署、不访问生产、不通知第三方。
- 未改 ADR / 协议规范；未动 E2EE-012/023/024/025/029 状态标记。
- 未删除、未 skip 任何测试；未放宽任何安全策略；未新增迁移与依赖。
