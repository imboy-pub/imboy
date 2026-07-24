# DDD 充血模型落地现状评估 / DDD Rich Model Implementation Status

> 跨 `imboy`（Erlang 后端）+ `imboyapp`（Flutter 客户端）/ Spans `imboy` (Erlang backend) + `imboyapp` (Flutter client)
> **版本 / Version**: 1.0.0 | **最后更新 / Last updated**: 2026-06-03
> **结论速览 / TL;DR**: 工程侧结构性落地全面完成（Phase 0–4，27 tasks complete），属**经评审取舍的务实充血模型**，**非教科书纯净六边形**；真机运行时回归仍是未签收的门。

---

## 简体中文（权威版本）

### 1. 总体判断

满血版 DDD 的**代码架构已全面落地，并通过编译 / 单元测试 / dialyze 三关**，但需明确两点限定：

1. 它是一套**经评审 / 用户拍板取舍的务实充血模型**，存在几处刻意的妥协（见 §4），不是无妥协的纯净版。
2. **工程侧落地 ≠ 运行时验证**：真机全流程回归（P3）尚未执行（见 §5）。

### 2. 后端已落地（`imboy/src/domain/`，9 文件）

| 类别 | 产物 | 不变量 / 职责 |
|------|------|--------------|
| 充血聚合根 | `user_agg` | 资料校验（性别 / 邮箱 / 允许搜索 / 字段白名单） |
| | `friend_agg` | 关系状态机 none/pending/friends/blocked + 6 转换 + 6 事件 |
| | `group_agg` | 角色 / 成员上限 / 转让·解散 |
| | `conversation_agg` | 未读数（≥0）/ 已读游标（单调不减）/ 置顶（幂等） |
| 领域策略 | `message_policy` | C2C send 决策（好友 / 黑名单 guard） |
| 值对象 | `conv_key_vo` / `group_role_vo` / `message_id_vo` | 不可变标识与键 |
| 领域事件 | `imboy_domain_event` | 事件总线，聚合产出事件 |

Logic 退化外壳：`user_logic` / `group_member_logic` 校验委托聚合，外壳仅保留 I/O 与线格式映射。

### 3. 前端已落地（`imboyapp/lib/modules/`，限界上下文划分）

- **充血实体**：`identity/domain/user`、`group_collab/domain/group`、`messaging/domain/{message,conversation,message_status}`、`social_graph/domain/friendship`（与后端状态机**对称**）。
- **值对象**：`user_id` / `group_id` / `group_role` / `message_id` / `conversation_id`。
- **Repository 端口**：`messaging/infrastructure/message_repository`、`group_collab/infrastructure/group_repository`、`social_graph/infrastructure/contact_repository`（`abstract interface`，SQLite 实现 `implements`）。
- **文档**：8 个 bounded context 均有双语 CLAUDE.md。

### 4. 刻意的务实妥协（非纯净 DDD，勿误判为技术债）

| # | 妥协 | 决策来源 | 理由 |
|---|------|---------|------|
| 1 | Repository 端口置于 `infrastructure/` 而非纯 `domain/`，务实引用 `sqflite Transaction` | T4.4a，**用户拍板**（务实 port vs 纯净 port） | 纯净需事务边界上移 application + 重设计全部调用方，成本高收益低 |
| 2 | `message_models` 保持 re-export barrel | T4.2c | KISS/YAGNI，领域不变量已由 `message.dart` + policy 承载 |
| 3 | 部分 `*_rules`（`image_url_extract` / `message_status_icon`）归 presentation 未迁 | T4.2 | 深依赖 flutter_chat_core UI 子类 / `IconData`，属表现层关注点 |
| 4 | `friend_logic` 未完全退化为纯外壳 | T3.4，**产品评审** | state-gating 是行为变更；经评审后接线 add/confirm/reject，而非无脑外壳化 |

### 5. 未关闭的门（运行时验证）

- **P3 真机全流程回归**（需真机，loop/CI 不可执行）：
  - 好友状态机：申请 → 重复申请拒 `already_requested` → 确认（pending 0→1）→ 再确认拒 `no_pending_request` → 拒绝删 pending → 可再申请 → 拉黑后申请拒 `blocked`。
  - 已读回执：好友正常 / 非好友 `not_a_friend` / 拉黑 `in_denylist`。
  - Chat 渲染：收发 / 撤回（canRevoke）/ 离线消息富化渲染（toTypeMessage）。
- **smoke 仅 happy-path 在 dev 环境绿**：依赖补测试好友数据（`1000000051`/`1000000056`），非全场景覆盖。

### 6. 结论

> DDD 充血模型在 imboy + imboyapp 的**代码架构层面已全面落地并通过静态门**，是一套**架构清晰、经取舍的务实充血模型**。最后一道"完成"签收门是 **P3 真机运行时回归**，需人工在真机执行。

---

## English (Mirror)

### 1. Overall Verdict

The full DDD rich-model architecture **is fully implemented and passes compile / unit-test / dialyze gates**, with two qualifications:

1. It is a **pragmatic rich model shaped by review / user decisions**, with several deliberate trade-offs (see §4) — not a no-compromise textbook hexagonal design.
2. **Engineering landing ≠ runtime verification**: real-device end-to-end regression (P3) has not been executed (see §5).

### 2. Backend — Landed (`imboy/src/domain/`, 9 files)

| Category | Artifact | Invariant / Responsibility |
|----------|----------|----------------------------|
| Rich aggregates | `user_agg` | Profile validation (gender / email / searchable / field allowlist) |
| | `friend_agg` | Relationship FSM none/pending/friends/blocked + 6 transitions + 6 events |
| | `group_agg` | Role / member cap / transfer·disband |
| | `conversation_agg` | Unread (≥0) / read cursor (monotonic) / pin (idempotent) |
| Domain policy | `message_policy` | C2C send decision (friend / denylist guard) |
| Value objects | `conv_key_vo` / `group_role_vo` / `message_id_vo` | Immutable identifiers and keys |
| Domain events | `imboy_domain_event` | Event bus emitted by aggregates |

Degenerate Logic shells: `user_logic` / `group_member_logic` delegate validation to aggregates; shells keep only I/O and wire-format mapping.

### 3. Frontend — Landed (`imboyapp/lib/modules/`, bounded contexts)

- **Rich entities**: `identity/domain/user`, `group_collab/domain/group`, `messaging/domain/{message,conversation,message_status}`, `social_graph/domain/friendship` (**symmetric** with backend FSMs).
- **Value objects**: `user_id` / `group_id` / `group_role` / `message_id` / `conversation_id`.
- **Repository ports**: `message_repository` / `group_repository` / `contact_repository` (`abstract interface`, SQLite `implements`).
- **Docs**: all 8 bounded contexts have bilingual CLAUDE.md.

### 4. Deliberate Pragmatic Trade-offs (NOT tech debt)

| # | Trade-off | Decision source | Rationale |
|---|-----------|-----------------|-----------|
| 1 | Repository ports in `infrastructure/` (not pure `domain/`), referencing `sqflite Transaction` | T4.4a, **user call** | Pure ports require lifting tx boundary to application + reworking all callers; high cost, low value |
| 2 | `message_models` kept as re-export barrel | T4.2c | KISS/YAGNI; invariants already carried by `message.dart` + policy |
| 3 | Some `*_rules` (`image_url_extract` / `message_status_icon`) stay in presentation | T4.2 | Deep dependency on flutter_chat_core UI subclasses / `IconData` |
| 4 | `friend_logic` not fully reduced to a pure shell | T3.4, **product review** | state-gating is a behavior change; wired add/confirm/reject after review |

### 5. Open Gates (Runtime Verification)

- **P3 real-device E2E regression** (requires a real device; not loop/CI executable):
  - Friend FSM: request → duplicate `already_requested` → confirm (pending 0→1) → re-confirm `no_pending_request` → reject removes pending → re-request → block then `blocked`.
  - Read receipts: friend OK / non-friend `not_a_friend` / blocked `in_denylist`.
  - Chat rendering: send/receive / revoke (canRevoke) / offline message enrichment (toTypeMessage).
- **smoke green only for happy-path in dev**: depends on seeded friend rows (`1000000051`/`1000000056`); not full-scenario coverage.

### 6. Conclusion

> The DDD rich model **is fully landed at the code/architecture level across imboy + imboyapp and passes static gates**. It is an **architecturally clean, deliberately scoped pragmatic rich model**. The final sign-off gate is **P3 real-device runtime regression**, to be run manually on a device.
