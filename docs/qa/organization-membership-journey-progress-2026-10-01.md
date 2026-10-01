# 企业加入、退出与重新加入真实数据库旅程 — 2026-10-01

English summary: A real synthetic PostgreSQL journey now verifies default workspace, General group and Announcements subscriptions, atomic rollback, self-departure, historical invitation replay and new-invitation rejoin. Existing product behavior passed; this change adds regression coverage only.

对应完整六项合同 ORG-01 / ORG-02 的局部证明。源码基础 `32d086d5bc10bb2697c9630ccc9f33f22381ef8c`；计划、最终检查源码和日志 SHA256 见 [证据清单](./evidence/organization-membership-journey-2026-10-01/sha256.json)。不将整个验收或产品状态改成完成。

## 实际调用与断言

使用既有一次性 PG 与完整真实迁移，合成组织 Owner / target；生产 workspace_ds:create_template 创建显式默认工作空间、全员群及公告频道。调用 production accept_targeted + membership_hook，数据库四层 active 计数各为 1；工作空间详情和企业目录授权通过。接受重放不新增群历史世代。

在公告订阅末端注入真实 CHECK 失败：接受返回 500，邀请保留 pending，组织/工作空间/群/频道目标关系均为 0，群历史世代没有残留。移除测试约束后正常接受，证明可以重试。

Owner 自助退出返回 409；普通成员通过 organization_member_logic:leave 退出，四层 active 计数均为 0，工作空间详情拒绝 403，目录应用层返回 insufficient_scope，当前群历史世代关闭。目标在原先另一个企业的 active 成员关系保持。旧邀请重放只返回历史 already_accepted，不恢复四层关系；新邀请正常重新加入，群历史累计 2 代、仅最新 1 代开放，公告频道归属仍是原默认工作空间。

检查没有启动领域事件总线（测试中显式断言），没有通知订阅者；不调用邀请创建推送，邀请仅 SQL 合成。数据均在本轮独立 marker DB，最终由测试清理；不影响共享数据库或真实用户。

## 命令与结果

`IMBOY_DEPS_ROOT=/Users/leeyi/project/imboy.pub/imboy bash scripts/test/customer_service_internal_http_gate.sh`：退出 0，当前全部产品源码编译、真实迁移、42 项 Internal 操作与扩展工作空间旅程通过；8 个顶层 EUnit 检查包含本轮新增旅程，不等于 8 个端点。最终证据 `/tmp/imboy-seat-http.nkefno`。预期 CHECK 失败产生的 ERROR 日志是故障注入断言的一部分，不能据此误判门禁失败。

两次初期失败均为新增夹具写错：目录 API 参数顺序以及应用层错误码断言；已按实际合同纠正并保留日志，不将夹具错误声称为产品缺陷。最终 git diff / erlfmt / shell 语法检查通过。手工审阅测试调用、真实 SQL oracle 与现有副作用边界，未运行独立审查代理。

## 边界

本轮没有修改产品源码或新增依赖；复用现有真实检查入口。证明的是数据库及应用层旅程，未覆盖真实设备/UI、全部文件与 OA 撤权、消息历史区间内容、对象存储或投产。六项目标继续推进，未推送、部署、生产迁移或对外通知。
