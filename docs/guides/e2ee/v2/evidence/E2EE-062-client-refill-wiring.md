# E2EE-062 第六刀：客户端接真实 OTK 余量（补传闭环闭合）

- **Slice**：`22-...state.md` §1.1 队列第 3 项的第六刀（第五刀残留 ① 的直接后继）
  - 第一刀 `E2EE-062-otk-claim-idempotent-lease.md`（单设备幂等租约）
  - 第二刀 `E2EE-062-per-target-throttle.md`（目标级限流）
  - 第三刀 `E2EE-062-batch-claim-idempotency.md`（batch 幂等）
  - 第四刀 `E2EE-062-client-request-id.md`（客户端发 `request_id`）
  - 第五刀 `E2EE-062-prekey-count-endpoint.md`（后端 count 端点）
- **会话**：`20260729-0000-claude-code`
- **仓库**：`imboyapp`
- **状态**：本刀完成；**E2EE-062 整体仍为 `PARTIAL`**（残留见 §5）

---

## 1. 做了什么

第五刀开出了 `GET /api/v1/e2ee/olm/prekey_count`，但客户端
`OlmApi.countPrekeys` 仍是恒返回 0 的桩实现。本刀接上。

### 1.1 缺口比「补传信号缺失」更严重

`_refillOneTimeKeys` 的判据是 `remaining < _otkLowWaterMark`。
`remaining` 恒为 0 意味着**每次调用都判定为低水位、每次都全量重发**。
而 `report_one_time_keys` 是**全量替换式**（先删后插，见 imboy
`olm_identity_logic:report_one_time_keys/4` 的文档注释：「全量替换式上报
one-time keys（先删后插）」）。

`_refillOneTimeKeys` 有两个调用点，其中一个是**每次入站建会话**
（`olm_session_service.dart` 的 pre-key 分支，`unawaited(_refillOneTimeKeys(...))`）。
也就是说：**每收到一条 pre-key 消息，就把自己整个未被领取的 OTK 池推倒重来一次。**
这不是「少了个优化」，是一条持续的破坏性动作。

**认识论状态：`countPrekeys` 恒 0 与 `report_one_time_keys` 全量替换语义均为已实证
（读源码 + 后端函数注释）；「每次入站建会话都会重置池」为文件级推理，未在真实
网络上观测。**

| 接缝 | 改动 |
|---|---|
| `lib/service/e2ee/otk_refill_policy.dart`（新） | 纯函数 `otkRefillCount`，把「该不该补、补多少」从副作用里摘出来 |
| `lib/store/api/olm_api.dart` | `countPrekeys()` 改为真实请求、返回 `int?`；新增 `parseCountPayload` |
| `lib/config/const.dart` | `olmPrekeyCount` 路由常量 |
| `lib/service/olm_session_service.dart` | `_refillOneTimeKeys` 改用策略函数；注册路径改走 `seed: true` |

无新依赖、无协议变更、无 schema 变更。

### 1.2 取舍一：查询失败 → **不补**（不在未知状态上做破坏性动作）

`countPrekeys` 返回 `int?`，`null` = 未知。两种可选的降级：

| 降级方式 | 后果 |
|---|---|
| 未知 → 当 0 | 触发全量替换。**在未知状态上执行破坏性动作**，且会冲掉其它对端正待领取的 key |
| 未知 → 不补（**采用**） | 池可能饿一会儿 → 退到 fallback prekey（既定降级路径）；下次查询成功即恢复 |

后端在第五刀已经拒绝把错误降级为 0（`E2EE-062-prekey-count-endpoint.md` §1.3），
客户端在此对齐——两端对「0 与未知」的区分是同一个决定的两半。

### 1.3 取舍二：首次注册走 `seed`，不依赖查询

若注册路径也依赖查询，一次查询失败就会让**新设备永远没有 OTK**，所有对端只能
退到同一条 fallback prekey。注册时池必然为空，无需查询——
`_refillOneTimeKeys(account, seed: true)` 直接铺满。
这条是 §1.2 fail-closed 的必要配套：不加它，安全方向的选择会变成可用性事故。

### 1.4 取舍三：`countPrekeys` 去掉 `deviceId` 入参

服务端只认 token 里的设备（第五刀 §1.1：带参端点等于「探测谁的池快空了」的接口）。
保留一个**看起来能选设备、实际被忽略**的参数是主动误导，故删除。

---

## 2. RED 记录

新增 `test/service/e2ee/otk_refill_policy_test.dart`（10 例）。
先落**保留今天语义**的载体（`otkRefillCount` 实现为 `targetCount - (remaining ?? 0)`，
即「未知当 0」；`parseCountPayload` 恒返回 `null`），使 RED 是行为失败。

> 中途一次 `flutter test` 因删除 `deviceId` 入参而**编译失败**（调用点未同步）。
> 按铁律「RED 必须是行为失败，不是编译错误」，先把调用点接到载体策略函数上，
> 再重跑取 RED。

```
00:00 +6 -4: Some tests failed.
```

**4 红均为行为失败**：

| 用例 | 失败形态 |
|---|---|
| `fail-closed：余量未知 → 不补` | 得 `50` —— 未知状态下仍会全量替换 |
| `余量充足 → 不补` | `remaining=5`（恰在水位线）得 `45` |
| `未知与 0 必须区分对待` | 两者都得 `50` |
| `prekey_count 响应解析 正常载荷 → 取出 count` | 得 `null` |

**对照组**：`余量为 0 时必须补满` —— 这条**今天就成立**（恒 0 桩实现下必然补满），
改后仍须成立。改前改后都绿 → harness 本身没坏，4 红是真缺口。

### 2.1 「只验拒收」反模式的规避

一个「一律不补」的实现在 fail-closed 指标上恒得满分。三条正向可用性用例否掉它：

- `余量为 0（真的空了）→ 必须补满`（对照组同时承担此职责）；
- `余量低于水位 → 补到目标值`（4 → 46，1 → 49）；
- `seed：首次注册不依赖查询，直接铺满`。

`未知与 0 必须区分对待` 是一条**关系断言**：把两者混为一谈的实现必在这两组里
错其一，无论它倒向哪边。

---

## 3. 生产调用方核实

```
lib/service/olm_session_service.dart:267  _refillOneTimeKeys(account, seed: true)   ← 设备注册
lib/service/olm_session_service.dart:656  unawaited(_refillOneTimeKeys(account))    ← 入站建会话后
lib/service/olm_session_service.dart:~288 otkRefillCount(...)                       ← 唯一决策点
lib/store/api/olm_api.dart                countPrekeys() → API.olmPrekeyCount
```

`otkRefillCount` 是 `_refillOneTimeKeys` **真正用来决策**的函数，不是平行实现；
`parseCountPayload` 是 `countPrekeys` 真正用来解析响应的函数。
两者都不是读源码的结构断言。

⚠️ 未做 HTTP 层的端到端测试（本仓无 Dio mock 基建，引入属新依赖方向）。
`countPrekeys` 里「`resp.ok` 为假 → 返回 null」这一行是**文件级阅读结论，未实证**。

---

## 4. 验收命令与结果

```
$ flutter test test/service/e2ee/otk_refill_policy_test.dart
  All 10 tests passed.

$ flutter test test/service/e2ee/
  All tests passed!   (355 passed；上一刀 345，本刀 +10)

$ flutter test test/service/
  All tests passed!   (1235 passed；上一刀 1225)

$ dart analyze lib
  1 issue found.   （component/ui/ios_settings_ui.dart:104 既有 info，与 E2EE 无关）
```

---

## 5. 残留风险（E2EE-062 仍未完成）

服务端 + 客户端的**主链路至此闭合**（幂等租约 → 目标级限流 → batch 幂等 →
客户端发 `request_id` → count 端点 → 客户端低水位补传）。剩余项：

1. **耗尽告警 / 运维指标缺失** —— 服务端仍无「某用户池见底」的指标或告警。
   补传是客户端自愈，运维侧对耗尽攻击仍然盲。
2. **端到端未实证** —— 幂等链路与补传链路的服务端半边（真 PG）与客户端半边
   （单测）各自实证，**两半拼接只有文件级论证**；`countPrekeys` 的 HTTP
   失败分支未实证（§3）。
3. **「每次入站建会话重置 OTK 池」的旧行为未在真实网络上观测**（§1.1），
   本刀按源码语义修复，未做故障复现。
4. 进程重启后重投仍消费新 OTK（第四刀的有意识取舍）。
5. 客户端无 batch_claim 调用方（全仓 grep 零命中），第三刀的 batch 幂等暂无生产流量。
6. 租约无独立 TTL；fallback prekey 未在服务端验签；
   「耗尽/限流绝不触发 RSA/Megolm/明文」无守护用例；
   单租户/全局两层限流未做；`olm_claim` 门仍朴素写法；60/min 未压测校准。
7. 真机双端未验证。

---

## 6. 认识论状态汇总

| 结论 | 状态 |
|---|---|
| 余量充足 → 不补 | **已实证** |
| 余量低于水位 → 补到目标值 | **已实证** |
| 余量未知 → 不补（fail-closed） | **已实证** |
| 余量 0（真空池）→ 补满（正向可用性） | **已实证** |
| seed 路径不依赖查询 | **已实证** |
| 响应解析把失败表达为 `null` 而非 0 | **已实证** |
| 端点常量指向第五刀注册的路由 | **已实证** |
| `countPrekeys` 在 HTTP 失败时返回 `null` | **文件级阅读结论，未实证**（无 Dio mock 基建） |
| 旧行为「每次入站建会话重置池」 | **文件级推理，未实证** |
| 「低水位补传前提成立」 | **成立（代码层面）**，端到端未实证 |
| 「OTK 抗耗尽」整体达成 | **不成立** —— 见 §5.1/§5.2 |

---

## 7. 未做

- 不 push、不部署、不访问生产、不通知第三方。
- 未改 ADR / 协议规范；未动 E2EE-012/023/024/025/029 状态标记。
- 未删除、未 skip 任何测试；未放宽任何安全策略；未新增依赖。
