# E2EE-062 第三刀：batch_claim 幂等租约

- **Slice**：`22-...state.md` §1.1 队列第 3 项的第三刀
  （第一刀 `E2EE-062-otk-claim-idempotent-lease.md`，第二刀 `E2EE-062-per-target-throttle.md`）
- **会话**：`20260728-2100-claude-code`
- **仓库**：`imboy`
- **状态**：本刀完成；**E2EE-062 整体仍为 `PARTIAL`**（残留见 §5）
- **上位验收**：`21-...playbook.md` E2EE-025「同 request id 重放 100 次只消费一次」

---

## 1. 做了什么

第一刀只给**单设备** claim 装上幂等租约。多设备 fan-out 走的是另一条路：
`batch_claim_keys/3` 逐设备调 `claim_keys/3` —— **没有 request_id**。

这不是边角路径，而是多设备场景下的主路径。客户端给一个 N 设备的对端建会话时，
一次网络超时后的重试会**再消费 N 条 OTK**：单设备重试消费 1 条，batch 重试消费
N 条，抽干速度是前者的 N 倍。第二刀的目标级限流只能拖慢它，幂等缺口依旧。

| 接缝 | 改动 |
|---|---|
| `src/logic/olm_identity_logic.erl` | 新增 `batch_claim_keys/4`（逐设备走 `claim_keys/4`）；抽出 `fan_out/2` 与 `normalize_device_ids/1` 供 /3 与 /4 共用 |
| `src/api/olm_handler.erl` | `do_batch_claim1/2` 读 body `request_id`，复用既有 `normalize_request_id/1`，按空/非空分派 /3 或 /4 |

无迁移、无新依赖、无协议变更（`request_id` 是可选字段，缺省即旧语义）。

### 1.1 关键设计取舍：**不按设备派生 request_id**

一个直觉方案是给每个设备派生 `<<ReqId/binary, ":", DeviceId/binary>>`。**没有采用**，
理由是安全那侧：

1. 迁移 49 的部分唯一索引键已经是
   `(claimed_by, user_id, device_id, claim_request_id)` —— **device_id 本就在键里**，
   同一 request_id 在不同设备上天然不互相命中，派生不解决任何问题；
2. 派生会把长度推过 `claim_request_id varchar(64)`。`normalize_request_id/1` 放行
   的上限正是 64，拼上 `":" ++ DeviceId` 必然溢出 → DB 层报错 →
   把一个**可选的幂等优化**变成一条**新的失败路径**。

这条判断是本刀的核心，因此**没有停在文件级阅读**，在真 PG 上钉死（见 §4.1）。

### 1.2 保留旧 arity 的原调用形状

`batch_claim_keys/3` 仍然逐字调 `claim_keys/3`；handler 在 `request_id` 为空时仍然
逐字调 `batch_claim_keys/3`。既有测试按 arity 挂 meck 期望，改成「旧的委托新的」
会让它们静默穿透到真实实现——本项目已两次踩此坑（A2-a、E2EE-062 第一刀）。
守护用例：`handler_without_request_id_keeps_arity3_test_`。

---

## 2. RED 记录

新增 `test/logic/e2ee_batch_claim_idempotency_tests.erl`（6 例）。
先只落**载体**（`batch_claim_keys/4` 原样委托 /3，不含新语义），使 RED 是行为失败
而非 `undef` 编译错误。

```
=======================================================
  Failed: 2.  Skipped: 0.  Passed: 4.
```

**2 红 = 2 个真实缺口，均为行为失败**：

| 用例 | 失败形态 |
|---|---|
| `batch_replay_consumes_once_per_device` | `length(lists:usort(Results))` 期望 1、**实得 4** —— 10 次重放拿到 4 批不同的 key，即每次都在消费新 OTK |
| `handler_passes_request_id_test_` | `error:must_not_drop_request_id` —— handler 把 body 的 `request_id` 丢掉，走了 `batch_claim_keys/3` |

**对照组**：`legacy_batch_consumes_each_time`（无 request_id 的 batch 保持逐次消费）
在改前改后**都绿** → harness 本身没坏，2 红是真缺口不是脚手架故障。

### 2.1 「只验拒收」反模式的规避

两条正向可用性用例：

- `distinct_batch_requests_consume_each` —— 不同 request_id 的两批必须**各自消费**
  （`[a1,b1]` 后是 `[a2,b2]`）。一个「永远返回第一批」的实现在幂等指标上恒得满分，
  被这条否掉。
- `devices_do_not_collide_under_one_request` —— 同一 request_id 下 A/B 两设备必须
  拿到**各自的** key。串键 = 用别人设备的 prekey 建会话，是比重复消费更严重的错误。

---

## 3. 生产调用方核实

| 被测入口 | 生产路由 |
|---|---|
| `olm_handler:init/2` `action => batch_claim` | `POST /api/v1/e2ee/devices/batch_claim` |

链路：`olm_handler:do_batch_claim1/2 → olm_identity_logic:batch_claim_keys/4 →
claim_keys/4 → olm_identity_ds:claim_one_time_key/4 → olm_identity_repo:claim_one_time_key/4`。
两条 handler 用例打的是真实 `init/2` 入口，不是内部私有函数。

---

## 4. 验收命令与结果

```
$ make e2ee-verify
  All 321 tests passed.
=== E2EE verify ALL PASSED ===
```

上一刀 315 → **321**（本刀 +6）。新模块 `e2ee_batch_claim_idempotency_tests`
**已加进 Makefile Modules 清单**。

### 4.1 真 PostgreSQL：核心设计判断已实证

```
$ IMBOYENV=local make eunit t=e2ee_otk_claim_idempotency_integration_tests \
    EUNIT_ERL_OPTS="-config config/sys.local -pa ebin -pa test"
  Application imboy started on node nonode@nohost
  All 6 tests passed.
```

上一刀 5 → **6**：新增 `batch_same_request_across_devices`，在真 PG 上证明

- 同一 `request_id` 的两个设备**各拿各的 key**（部分唯一索引里的 `device_id` 确实消歧）；
- 整批重放时**每设备仍只消费一条**，且恒返回同一条 key（池 5→4，重放后仍 4）。

这条把 §1.1 的取舍从「文件级阅读结论」升级为 **已实证**。
该集成模块**不在** `e2ee-verify` 硬门禁内（无 DB 时会 skip，放进门禁只会得到假绿），
手动运行命令即上方那条。

`erlfmt --check` 改动文件通过；`git diff --check` 通过。

---

## 5. 残留风险（E2EE-062 仍未完成）

按当前优先级重排：

1. **客户端两条路径都未发送 `request_id`** —— 服务端 claim / batch_claim 现在都已就绪，
   但**生产流量一条也走不到幂等路径**。这是幂等租约兑现价值的唯一剩余前提。
   **认识论状态：文件级阅读结论，未实证。**
2. **低水位补传与耗尽告警缺失** —— 池见底时没有主动通知设备补传，也无运维指标。
   这是第二刀取舍二「限流只拖慢、靠补传恢复」的**前提条件**，目前**该前提尚不成立**。
3. **租约无独立 TTL**，边界是审计保留期，过期后同 request_id 会重新消费。
4. **fallback prekey 未在服务端验签**（playbook 要求「身份验证通过」）。
   **认识论状态：文件级阅读结论，未实证。**
5. **「耗尽/限流绝不触发 RSA/Megolm/明文」无针对性守护用例。**
6. **单租户 / 全局两层限流未做**（第二刀取舍三，有意识缺口，网关承担更合适）。
7. **`olm_claim`（per-claimant）门仍是朴素写法**，未注册 scope 时静默失效
   （见 `E2EE-062-per-target-throttle.md` §4.1）。**已实证该行为存在。**
8. **限流阈值未经压测校准**，60/min 是推理值。
9. **`config/sys.local.config` 是 gitignored 的**，本地 scope 缺失时限流静默失效
   （现在至少有 ERROR 日志）。
10. **batch 内部仍是逐设备串行 claim**（N≤20 上限内可接受，`ponytail:` 注释已标明
    升级路径=单条 CTE 批量 claim）。
11. 真机双端未验证。

---

## 6. 认识论状态汇总

| 结论 | 状态 |
|---|---|
| batch 同 request_id 重放每设备只消费一条 | **已实证**（状态化 ets mock + 真 PG） |
| 同 request_id 跨设备不串键 | **已实证**（真 PG，`batch_same_request_across_devices`） |
| 不同 request_id 各自消费（正向可用性） | **已实证** |
| handler 透传 body `request_id` | **已实证**（真实 `init/2` 入口） |
| 缺省 request_id 时保留 `/3` 原调用形状 | **已实证**（守护用例 + 对照组） |
| 不派生 per-device key 是正确取舍 | **已实证**（索引含 device_id，真 PG 验证） |
| 「OTK 抗耗尽」整体达成 | **不成立** —— 见 §5 第 1/2 项 |

---

## 7. 未做

- 不 push、不部署、不访问生产、不通知第三方。
- 未改 ADR / 协议规范；未动 E2EE-012/023/024/025/029 状态标记。
- 未删除、未 skip 任何测试；未放宽任何安全策略。
- 未新增迁移、未新增依赖。
