# 工作区移除：频道撤权本地验证

日期：2026-10-01。状态：LOCAL_FOCUSED_PASS；企业退出整体尚未完成。

## 已实现

- 原工作区移除事务下沉至现有 workspace_ds，保留项目、任务冲突、群资格与历史世代关闭行为。
- 同事务撤销该工作区内目标用户的频道订阅和管理员资格；订阅人数只随真实状态变化扣减。
- 包含归档频道，排除其他工作区及个人频道；保留频道、消息及企业资料。
- 活跃频道创建者先交接，不能移除后依赖 creator_uid 重新取得角色 3。
- 提交成功后清理群实时成员、发布原有成员事件，并清理受影响频道的订阅及详情缓存。
- 回滚不执行提交后的清理。新增 affected_channels 清单，原返回字段保留。

## 证据

源基线：1169e01ddf542b11a4060653173affd08da349df。

| 检查 | 结果 | 范围 |
|---|---|---|
| 新回归在基线运行 | 3 FAIL、1 PASS | 复现旧工作区移除遗漏频道撤权和冲突 |
| workspace_logic_tests 与 workspace_departure_tests | 40 PASS、0 skipped | 既有工作区逻辑及频道清理失败、父关系失败、提交后缓存清理 |
| workspace_departure_pg_tests:run/1 | 3 PASS、0 skipped | 真实 PostgreSQL 18、实际 Repo SQL；作用域、归档频道、人数、幂等、回滚、创建者保护 |
| 变更源编译及 diff 检查 | PASS | 本轮三个业务模块；不是全仓编译 |

日志：/tmp/gz-workspace-departure-before.log、/tmp/gz-workspace-departure-test.log、/tmp/gz-workspace-departure-pg.log。
缓存依赖来自主仓既有 deps；业务模块从本工作树源码编译至独立临时目录，不复用主仓业务 beam。

## 重现数据库检查

编译 test/repo/workspace_departure_pg_tests.erl 及 workspace_member_repo、elib_pg 等所需模块，
使用现有 epgsql 依赖。创建全新的临时 PostgreSQL 集群：initdb 指定测试用户 departure_test，
只启用该临时目录的 Unix socket，关闭 TCP 监听。该套件仅创建连接私有的 TEMP 表。

把其 socket 文件绝对路径传入 workspace_departure_pg_tests:run/1；ok 才视为通过。
缺少连接会直接失败，不跳过；测试结束关闭连接、停止临时集群。不使用业务配置或业务数据库。

## 尚未覆盖

组织 remove/offboard 还未调用这个共用撤权事务，员工主动退出接口与 App 操作尚未接入。
组织 Owner、工作区 Owner、群主、经办关系、资料访问、OA 会话退出及并发竞态需要在完整退出链路验收。
本轮没有独立代理审计、全仓测试、真实设备测试或生产验证；未 push、部署或执行生产迁移。
