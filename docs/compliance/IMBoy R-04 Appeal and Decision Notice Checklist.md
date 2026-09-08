# IMBoy R-04 Appeal and Decision Notice Checklist

> 任务：R-04 — Appeal and Decision Notice（海外合规实施计划，`LEGAL` scope gate）
> 状态：**后端最小申诉链 DONE（R-04.1 前端两页待续）** | 执行日期：2026-09-08
> 范围口径：计划 Implementation 要求 "Configure availability by policy profile, not country if statements"——本批把申诉链做成 **profile/运行时可配置**（`appeal` 内建 feature 键，默认开放），是否对用户开放由政策/法务结论决定（配置关闭即不开放）；工程落地不预设 LEGAL 结论，与 `LEGAL` scope gate 不冲突（Gap Matrix 亦记 "operational baseline recommended"）。

---

## Implementation（计划要求 → 实际交付）

### 1. reasoned decision notice（有理由的处置通知）

- [x] 处置理由在 moderation_action.reason 已有（R-02）；本批保证**可达性**：用户经 `GET /api/v1/appeal/my` 自助查询处置动作（类型/理由/状态/是否已撤销）与申诉进度、终审理由。
- [x] 主动推送通知依赖既有通知设施（R-02 遗留打磨项），列为对接点不在本批硬造（见"已知边界"）。

### 2. one appeal（一次申诉）

- [x] 迁移 00000094：`moderation_appeal` 表，`UNIQUE (action_id, appellant_uid)` 库级约束 + logic 前置查询双保险；重复申诉返回明确错误。

### 3. independent reviewer permission（独立复审）

- [x] 复审者不得为原处置执行者（`reviewer_id ≠ action.actor_id`，logic 层硬约束+测试）；Admin 权限复用举报处置族 `reports:read`/`reports:handle`（申诉是处置链一部分；独立性由回避约束本质保证）。

### 4. final decision / reversal（终审与翻案）

- [x] 状态机：`pending → accepted | rejected`（CHECK 约束 + `mark_reviewed` 仅更新 pending 行）；
- [x] accepted 联动 `moderation_action_logic:reverse`（翻案=撤销原处置，reverse_reason 前缀"申诉翻案:"）；撤销失败不回滚终审判定（truthful 落库+幂等可重试，与 R-02 撤下容错同口径）。

### 5. Availability by policy profile

- [x] `appeal` 加入 `imboy_feature` Builtin 键（bot_webhook 先例）；门= `imboy_feature:enabled(appeal)`，handler 与 logic 双层校验；profile/runtime 配置（admin feature 配置端点）即可开关，无需发版。

### 6. Reporter identity privacy（举报人隐私）

- [x] moderation_appeal 表无 reporter 字段；用户出参（user_view）刻意排除 case_id/reviewer_uid/reporter——契约测试断言键集。

## 交付物

| 文件 | 内容 |
|---|---|
| `priv/migrations/00000094_moderation_appeal.{up,down}.sql` | 申诉表（TSID bigint、状态 CHECK、唯一约束、双索引） |
| `src/repo/moderation_appeal_repo.erl` | insert/find/查重/用户列表/admin 分页/终审落库 |
| `src/logic/moderation_appeal_logic.erl` | eligible/窗口/状态机/独立复审/翻案联动/出参脱敏 |
| `src/api/appeal_handler.erl` | `POST /api/v1/appeal/create`、`GET /api/v1/appeal/my` |
| `src/adm/adm_appeal_handler.erl` | `GET /api/adm/appeal/list`、`POST /api/adm/appeal/review` |
| `src/imboy_router.erl` | 用户段+adm 段四处挂载 |
| `src/lib/imboy_feature.erl` / `src/imboy_app.erl` | `appeal` Builtin 键 / `moderation_appeal` TSID 注册 |

## Tests（计划六条 → 10/10 通过）

| 计划测试项 | 用例 |
|---|---|
| eligible/ineligible | `submit_happy_path` / `submit_rejects_non_target_user` / `submit_rejects_non_executed_action` / `submit_rejects_duplicate_appeal` |
| deadline | `submit_rejects_expired_action`（40 天前 action vs 默认 30 天窗口；`appeal_window_days` 可配，0=不设限） |
| reviewer conflict | `review_rejects_original_actor` |
| reversal | `review_accept_reverses_action`（联动断言）/ `review_reject_keeps_action`（零撤销断言） |
| notification | `my_list_view_reaches_user_and_hides_reporter`（终审结果用户可达+隐私键集） |
| privacy of reporter identity | 同上（case_id/reporter_uid/reviewer_id 键集断言） |
| 可用性门 | `submit_blocked_when_appeal_feature_disabled` |

回归：`imboy_feature_tests` 10/10（**同步更新两个过期断言**：硬编码全集缺 L-01 的 bot_webhook，补 appeal/bot_webhook）、`feature_composition_compat_tests` 6/6（manifest ⊆ 全集兼容新键）。

## 已知边界与 R-04.1（前端两页）

1. **Flutter 状态页 / Admin 复核页**未做（计划 Files 的后两项）——API 已就绪，页面列 R-04.1 小批；
2. 主动推送通知对接（R-02 通知打磨项）——本链先保证"自助查询可达"；
3. 是否对用户开放申诉（LEGAL 结论）：配置 `appeal` feature 即可，无需代码变更；
4. 申诉窗口默认 30 天，`appeal_window_days`（DB 配置）可调，0=不设限。

## 提交

- imboy：`feat(R-04): 处置申诉链——一次申诉/独立复审/翻案联动/profile 门控`（只 commit 不 push）
