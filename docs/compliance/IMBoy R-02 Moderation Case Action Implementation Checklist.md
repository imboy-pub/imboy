# IMBoy R-02 Moderation Case & Action Executor — Implementation Checklist

> 供实施会话直接开工的地基勘察与任务拆解。2026-09-05 由 D-04 收官会话产出。
> 上游：R-01（`2cad32f5` 迁移 00000087 / `e037803d` report_logic+handlers / `5f57cb48` 1700 行测试）。

## 地基现状（已勘察）

- `report_ticket` 表：迁移 00000004 创建；00000087 已扩展 `target_sub_type` /
  `target_scope_id` / `target_author_id` / `evidence jsonb`（R-01）。
- R-01 已有：举报创建契约（report/create.yaml）、Admin 详情端点
  （adm-report/detail.yaml）、`report_logic.erl` 状态机（+420 行）、
  msg_id 契约 / IDOR / E2EE 同意门 / 限流复用校验链、4 套件 1700+ 行测试。
- 可复用处置 primitives：
  - 群禁言/踢人：`group_member_logic.erl` / `group_member_handler.erl`
  - 用户账号状态：`user.status`（1 启用 / 2 申请注销中 / -1 已注销——D-01 契约，
    R-02 的 restriction **不要**私造新状态值，用独立 restriction 字段/表）
  - 内容删除：消息撤回/删除链路（msg_store）
- 认证/权限基座：`auth_ds` + admin 角色门（A-02 之前 admin/moderator 区分有限，
  R-02 权限矩阵需自带最小角色检查，勿等 A-02）。

## 关键设计决策（开工前定）

1. **case 与 report_ticket 的关系**：推荐 `report_ticket.status` 已含
   confirmed 态（R-01），case 直接**复用 report_ticket 行**（confirmed 即
   case），新建 `moderation_action` 表记录动作（不动 report_ticket 结构）。
2. **迁移号**：00000088（`moderation_action` + 索引 actor/target/status）。
   TSID：`moderation_action` 记得 `imboy_app:tsid_generator_names()` 同步。
3. **动作集合**（MVP，全部走既有 primitives）：`warning`（站内通知）、
   `content_removal`（消息/动态删除）、`group_mute`（scoped，带 start/end）、
   `group_kick`、`account_restrict`（temporary/permanent，独立表字段+到期
   sweep）。**不做**：shadowban、全站封 IP。
4. **Fail-closed**：无权限/目标已删/重复执行 → 显式失败记录，case 状态保持
   truthful（action 表记 `failed` 而非静默）。
5. **撤销/reversal**：每个动作可逆记 `reversed_at/reversed_by/reason`；
   到期动作由 sweep（复用 D-03 sweep 模式：SKIP LOCKED + job 表）。
6. **审计**：actor/reason/policy/scope/start/end/result 全部落库（清单 S 系
   验收口径）。
7. **通知目标用户**：S2C 系统消息（复用 logged_another_device 通知通道）；
   举报人身份永不暴露（R-04 的隐私红线提前守住）。

## 测试矩阵（对齐 plan 验收）

- 权限矩阵：admin / moderator / 普通用户 × 每动作（未授权 → fail-closed 403）
- 每动作 happy path + 到期（mute/restrict 过期自动失效）
- duplicate/retry：同 case 重复执行同一动作 → 幂等或显式拒绝（不双发通知）
- reversal：撤销后目标状态恢复 + 审计留痕
- 目标通知：动作送达目标用户；举报人零泄漏
- action 失败：case 状态不被误推进（truthful）
- EUnit：`test/logic/moderation_action_logic_tests.erl` +
  `test/api/adm_moderation_handler_tests.erl`（对齐 R-01 测试风格，
  elib_pg query/2,3 恒 `{ok,[map]}`、vendored epgsql 零行 RETURNING 双形态、
  中文二进制带 /utf8、erlang.mk 警告即错误——unused var 会让模块静默不进 ebin）

## Admin UI（imboyadmin）

- 队列页：confirmed 举报列表 → 详情（R-01 已有 detail 端点）→ 动作面板
- 动作面板：warning/mute/kick/restrict/reject(no-action) + reason 必填
- 动作历史时间线（per target 聚合）

## 工序建议（两段）

1. 后端：迁移 00000088 + `moderation_action_repo/logic/handler`（adm 路由）
   + EUnit 权限矩阵/动作/到期/撤销 → 单笔或两笔提交（erlfmt -w 先行，
   提交信息勿含反引号，用 -F 文件）。
2. Admin UI + 通知 + sweep 到期 → 第二笔。

## 环境注意（本会话实测）

- 本地验收后端用 **9801**（已热更为 main）；9804/9800 是旧构建勿用。
  判别法：`code:which(moderation_action_repo)`。热更/新表见
  「d04-e2e-macos-green-backend-hotpatch-2026-09-05」记忆（TSID register、
  config_ds:set 加密、cowboy:set_env 路由热替换）。
- 真机 reverse：设备 tcp:9804 → 宿主 9801。
