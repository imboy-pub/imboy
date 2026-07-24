# ADR: DDD 迁移终点线 / DDD Migration Endpoint

**日期**: 2026-06-03 | **状态**: 已接受

## 决策

> **2026-07-22 修订**：原死线"Q3 2026（2026-09-30）之前"改为**触发式迁移，不设日历死线**（适配依赖/状态驱动的推进方式；见下）。

所有 `src/logic/` 中含状态校验的函数应迁入 `src/domain/` 聚合根。采用**触发式迁移**：某 logic 函数**下次因状态校验相关改动被触碰时**就地迁入 domain；新增含状态不变量的代码直接写 domain。未被触碰的模块保持现状，不强制批量迁移、不设截止日。

## 规则

| 层 | 新代码写在哪 | 说明 |
|----|------------|------|
| 含状态不变量（FSM、校验、guard） | `src/domain/*_agg.erl` | 聚合根承载业务规则 |
| 无状态编排 / I/O 映射 | `src/logic/*_logic.erl` | 外壳：调聚合根 + 处理 I/O |
| 数据访问 | `src/ds/` + `src/repo/` | 不变 |

## 当前状态（2026-06-03）

- `src/domain/`：9 个聚合根（user/friend/group/conversation + policy + VO + event）
- `src/logic/`：71 个模块，其中已退化为外壳：`user_logic`、`group_member_logic`
- 未迁移高优先级模块：`msg_c2c_logic`（763行）、`friend_logic`（部分保留，已评审）

## 不迁移的例外（已评审）

- `friend_logic` 中的 add/confirm/reject：state-gating 行为变更，经产品评审保留在 logic 层
- `message_*_rules`（image_url_extract / message_status_icon）：深依赖 Flutter UI 类型，属表现层

## 终点判定（状态驱动，非日期）

当满足以下**条件**时更新本 ADR 状态为"已完成"或"已搁置"，并在 CLAUDE.md 注明 logic 层最终形态：高优先级模块（`msg_c2c_logic` 等）已迁移或经评审保留为例外，且 logic 层不再新增含状态校验的函数。**由状态达成触发，不由日历触发。**
