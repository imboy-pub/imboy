# 创建 Markdown 系统设计文档

## 输出文件

创建：

`/Users/leeyi/project/imboy.pub/imboy/docs/plans/2026-08-09-IMBoy-appointment-activity-system-design.md`

不创建 DOCX，不执行 DOCX 渲染。

## 文档结构

沿用 System Design 模板的语义结构：

- 背景与目标
- 当前系统能力基线
- 产品边界与非目标
- 预约系统架构
- 频道与预约服务的绑定关系
- 群活动与 `group_collab` / `group_schedule` 的关系
- 核心用户流程
- 领域对象与状态
- API 和数据模型 TODO
- 权限、隐私、支付与运营约束
- 架构取舍
- 用户验证与 Go / No-Go 条件
- Claude Code 执行 TODO
- 未决问题和外部阻塞项

## 内容原则

- 预约是独立能力，频道只是入口和内容承载。
- 群活动先复用群日程，不创建新顶层模块。
- 首版采用固定服务时段，不做复杂员工排班。
- AA 先记录预算和结算，不托管真实资金。
- 没有真实试点前，预约研发保持 `BLOCKED`。
- 明确区分代码事实、产品假设、用户反馈和待验证事项。

当前仍处于 Plan Mode，因此本轮只修订为 Markdown 文档方案，尚未创建文件。
