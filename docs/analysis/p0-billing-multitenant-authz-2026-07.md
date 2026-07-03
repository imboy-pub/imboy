# P0-B · billing 多租户权限模型 — 设计与落地方案

> 版本 2026-07-02 | 方法：两路只读 agent 独立盘点（证据链互印）+ 主会话核实。全程未改代码。
> 执行分工：Fable 出方案，glm-5.2 盲执行。**含一处必须人工拍板项（归属模型语义），见文末 D。**

---

## 事实基础（已确证，全量）

**12 个 `/v1/billing/*` 端点零 current_uid 校验**（核实：`grep -c current_uid src/api/billing_handler.erl` → **0**；12/12 函数签名 `xxx(Req0, _State)` 丢弃 State）：

| action | 路由 | 方法 | 资源定位符（全部客户端自报） | 类别 |
|---|---|---|---|---|
| plan_create | POST /v1/billing/plan | POST | body code/name/price | **管理动作** |
| plan_update | POST /v1/billing/plan/update | POST | body `id` | **管理动作** |
| plan_list | GET /v1/billing/plan/list | GET | — | 公开读（套餐目录） |
| subscribe | POST /v1/billing/subscribe | POST | body `tenant_id`（默认0） + plan_id | 租户动作 |
| renew | POST /v1/billing/renew | POST | body `subscription_id` | 租户动作 |
| cancel | POST /v1/billing/cancel | POST | body `subscription_id` | 租户动作 |
| subscription | GET /v1/billing/subscription | GET | qs `tenant_id` | 租户读 |
| report_usage | POST /v1/billing/usage | POST | body `subscription_id` | 租户动作 |
| check_quota | GET /v1/billing/quota | GET | qs `subscription_id` | 租户读 |
| invoice_generate | POST /v1/billing/invoice/generate | POST | body `subscription_id` | 租户动作 |
| invoice_pay | POST /v1/billing/invoice/pay | POST | body `invoice_no`（全局唯一号） | 租户动作 |
| invoice_list | GET /v1/billing/invoice/list | GET | qs `subscription_id` | 租户读 |

**数据层无归属锚点**（核实：`grep -rn "owner\|uid" src/repo/billing_*_repo.erl` → 0；`grep "CREATE TABLE.*tenant" priv/migrations/` → 0）：
- `priv/migrations/00000011_billing.up.sql`：4 张表 `billing_plan` / `billing_subscription` / `billing_usage` / `billing_invoice`，**无一张有 uid/owner 列**。
- `billing_subscription.tenant_id bigint DEFAULT 0`（`:45`）是**裸整数逻辑字段，无 FK、无 tenant 实体表**，代码注释自认"imboy 当前无多租户表"（`:41`）。
- **全库不存在 tenant↔uid 归属映射**。→ 这是核心障碍：即使 handler 补上 current_uid，也没有底层数据可供归属比对。

**已有正确参照（可直接复用）**：
- 用户侧：`wallet_handler.erl` 全部 action `CurrentUid = auth_ds:current_uid(State)`（12 处），资源天然按 uid 归属。`auth_ds:current_uid/1 = maps:get(current_uid, State, 0)`，由 auth middleware 注入。
- 管理侧：`/adm/finance/billing/*`（`adm_finance_handler.erl`）已走 `adm_acl:ensure_permission(State, finance:read/write, Req0)` RBAC 门，且**套餐 CRUD 在 adm 侧已有一份**（同一 billing_logic 函数）。role_acl 已登记 `finance:read/write` 且留了 `finance:billing:*` 细化 TODO。
- license（`imboy_license`）是**部署级单实例**模型（domains/max_users），与 billing tenant（库内多租户雏形）**零代码交集，不可混同**。

---

## 架构决策：归属校验落在哪一层？

**判定：Logic 层做归属校验，DS 层提供归属查询原语。** 理由：
- Handler 层只负责提取 `current_uid`（从 State）并下传（边界职责，参照 wallet_handler）。
- **归属校验是业务规则**（"这个 uid 能不能操作这个 subscription"），属 Logic 层职责；DS/Repo 只提供"subscription 归谁"的数据查询，不含判断。符合 4 层单向依赖。
- 不在 DS 层做校验——DS 无 State/uid 上下文，且会让归属逻辑散落难测。

---

## 落地方案（分两阶段：管理端可立即做，租户端需拍板）

### 阶段 1 — 管理端动作迁移（低风险，无需拍板，[MODEL] glm 可独立执行）

`plan_create` / `plan_update` 是**管理动作**（改套餐定价 = 权限提升），必须走 admin RBAC，不该暴露在 `/v1`：

- **BILL-01**：`src/imboy_router.erl` 把 `/v1/billing/plan`（create）、`/v1/billing/plan/update` 两条路由**删除**（adm 侧 `/adm/finance/billing/plan` + `/plan/update` 已存在且有 `finance:write` 门，功能不丢）。核实 adm 侧存在：`grep -n "billing_plan_create\|billing_plan_update" src/imboy_router.erl` → L760-766 段。
- `plan_list`（GET /v1/billing/plan/list）：产品确认套餐目录对所有登录用户可见则**保留**（仍需 JWT），否则一并收到 adm。
- **验收 gate**：`make compile` 绿；`curl` 非 admin JWT 打 `/v1/billing/plan` → 404（路由已删）；adm cookie 打 `/adm/finance/billing/plan` 仍可创建。新增 `test/logic/billing_logic_tests.erl` 断言路由层行为（或 adm handler 层 eunit）。
- **glm 陷阱**：删路由后确认前端 admin 调的是 `/adm/finance/billing/*`（imboyadmin 侧），若有前端调 `/v1/billing/plan` 需同步改（grep imboyadmin `billing/plan`）；erlfmt/DCO。

### 阶段 2 — 租户端归属校验（需先拍板归属模型，见 D）

租户端 9 个 action 的归属校验骨架（**待 D 拍板后确定 `tenant_of_uid` 语义**）：

- **BILL-02（归属查询原语，DS/Repo 层）**：新增 `billing_subscription_ds:owner_uid(SubId)` → 查 subscription 的归属 uid；`billing_invoice_ds:owner_uid(InvoiceNo)` → 经 subscription 反查。**前提**：`billing_subscription` 表需加 `owner_uid bigint` 列（迁移 `00000019`，见 D 决策）。
- **BILL-03（Logic 层统一 gate）**：新增 `billing_logic:assert_owner(CurrentUid, SubId)` → `case billing_subscription_ds:owner_uid(SubId) of CurrentUid -> ok; _ -> {error, no_permission} end`。所有租户端 logic 函数首行调用。
- **BILL-04（Handler 层注入）**：`billing_handler.erl` 9 个租户 action 函数体首行 `CurrentUid = auth_ds:current_uid(State)`（去掉 `_State`），下传 logic。参照 wallet_handler。
- **BILL-05（logic 签名扩展）**：`subscribe/renew/cancel/current_subscription/report_usage/check_quota/generate_invoice/pay_invoice/list_invoices` 各加 `CurrentUid` 首形参，内部先 `assert_owner`。
  - `subscribe` 特殊：创建订阅时 `owner_uid = CurrentUid`（写入而非校验），`tenant_id` 由 D 决策决定是否 = CurrentUid 或独立。
  - `invoice_pay` 特殊：`invoice_no` 全局唯一，须经 subscription 反查 owner，拒绝代付他人账单。
- **边界**：不动 billing 业务计算（定价/周期换算/配额逻辑）；不动 payment_gateway 支付原语；迁移序号 `ls priv/migrations | tail -1` 核实（当前 `00000018`，下一个 `00000019`），erlang_migrate strict 已开，序号必须递增。
- **验收 gate（每端点一个 EUnit）**：新增 `test/logic/billing_logic_authz_tests.erl`（meck billing_subscription_ds:owner_uid）：
  - `test_renew_rejects_non_owner`：owner_uid=1001，CurrentUid=2002 调 renew → `{error, no_permission}`。
  - `test_cancel_rejects_non_owner`：同上 cancel。
  - `test_subscription_read_rejects_non_owner`：非 owner 读 → no_permission。
  - `test_report_usage_rejects_non_owner`：非 owner 报用量 → no_permission。
  - `test_check_quota_rejects_non_owner`：同上。
  - `test_invoice_generate_rejects_non_owner`：同上。
  - `test_invoice_pay_rejects_non_owner`：CurrentUid 非 invoice_no 对应 subscription 的 owner → no_permission（防代付）。
  - `test_invoice_list_rejects_non_owner`：非 owner 列账单 → no_permission。
  - `test_subscribe_sets_owner`：subscribe 后 owner_uid == CurrentUid。
  - `make compile && make eunit` 绿。
- **glm 陷阱**：迁移 `00000019` 加 `owner_uid` 列须带 down 脚本；存量数据 `owner_uid` 回填策略需 D 决策（历史 subscription 无 owner）；erlfmt 会重排、DCO `-s`、`git restore --staged .` 后精确 add；PostToolUse auto-stage 注意。
- **分工**：**需 Fable 出最终签名映射**（待 D 拍板 tenant 语义后，本文档 BILL-05 骨架即定稿）；实现 [MODEL] glm。
- **回滚条件**：单函数迁移，任一 eunit 失败回滚该函数；迁移 `00000019` 若回填失败，先只对新 subscription 强制 owner、历史订阅豁免（加注释标记技术债）。

---

## D. 必须人工拍板（阻塞阶段 2）

**BLK-BILL · tenant↔uid 归属模型语义** —— 这是 SEC-01 租户端修复的唯一前置。当前 `tenant_id` 是客户端自报裸整数、无实体表。三个选项（见对话中的拍板问题）：

- **选项 1（推荐，最省）**：单租户简化 —— `owner_uid = 当前 uid`，`tenant_id` 直接取 `current_uid`（一个用户 = 一个租户）。迁移只加 `owner_uid` 列，归属校验即 `owner_uid == current_uid`。适合当前"无组织概念"现状，YAGNI。
- **选项 2（多租户完整）**：建 `billing_tenant` 实体表 + `billing_tenant_member(tenant_id, uid, role)` 多对多映射。`owner_uid` 换成"uid 是否 tenant 成员且有权限"。适合未来 ToB 组织付费，但工作量大、当前无组织模型支撑。
- **选项 3（混合）**：先做选项 1 的 `owner_uid` 归属校验止血，`billing_tenant` 实体表留待有真实组织需求时再建（选项 2 的表结构可后加，不冲突）。

**存量数据处理**（依赖上面选择）：历史 `billing_subscription` 的 `owner_uid` 如何回填？若选项 1 且历史订阅 tenant_id 恰好曾存过 uid，可直接迁移映射；否则历史订阅标记为"无主"仅 admin 可操作。
