# ADR: DDD 迁移终点线 / DDD Migration Endpoint

**日期**: 2026-06-03 | **状态**: 已接受

## 决策

**Q3 2026（2026-09-30）之前**，所有 `src/logic/` 中含状态校验的函数迁入 `src/domain/` 聚合根。到期未迁移的模块，`logic/` 层即为最终形态，不再强制迁移。

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

## 截止后行动

2026-09-30 后，更新本 ADR 状态为"已完成"或"已搁置"，并在 CLAUDE.md 中注明 logic 层最终形态。
