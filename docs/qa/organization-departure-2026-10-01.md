# 企业退出事务验证（2026-10-01）

状态：本轮受影响路径 `PASS`，整体目标仍未完成。本轮基于后端 `85dd86082bcd3e6447ac1f58236acc0e0a429384`；证据对应同提交中的源文件和测试。

## 实际改变

- POST `/api/v1/organizations/:organization_id/members/:user_id/offboard`：目标是登录用户本人时调用 `leave/2`，否则保留 Owner/Admin 治理授权。身份来自 Human JWT，不接受请求正文覆盖。
- 普通成员、管理员、暂停成员可退出；归档企业也允许本人退出。企业 Owner 必须先移交；工作区负责人及有效群/频道负责人、项目/任务交接依赖阻拦离场。
- 本企业工作区按 ID 顺序锁定，包括归档工作区。下属群资格、开放历史世代、频道订阅/管理员资格、工作区资格与企业资格在一个真实事务内撤销；其他企业及个人频道不受影响。
- 只在提交后更新现有实时成员/投递缓存。依赖守卫拒绝时全部回滚，无成功事件。群、频道及历史实体不删除。
- 工作区与群成员撤销的 updated_at 改用 CURRENT_TIMESTAMP。真实 PostgreSQL 驱动不再尝试把 RFC3339 binary 编成错误参数类型。

## 验证与覆盖

1. 改动的 4 个源模块、6 个测试模块均通过 erlc 编译；依赖显式使用当前主仓 epgsql/cowboy。
2. `organization_departure_tests`、`organization_member_logic_tests`、`organization_member_handler_tests`、`workspace_logic_tests`、`workspace_departure_tests`：80 PASS，0 skipped，exit 0。日志 `/tmp/gz-organization-departure-test.log`。
3. PostgreSQL 18 独立新建数据库，仅 Unix socket，不监听 TCP，不读取业务数据库。实际 Logic/DS/Repo、elib_pg/epgsql 事务；仅连接池、环境和提交后服务使用替身。7 PASS，exit 0：本人退出、治理移除、暂停/归档退出、真实经办关系守卫拒绝、第二工作区负责人冲突、第二工作区频道负责人冲突。
4. 守卫函数直接读取 `priv/migrations/00000114_enterprise_business_identity.up.sql`，没有写一个更宽松的替代触发器。失败比较企业/工作区/群/频道/历史世代/序列快照；成功核实其他企业、个人频道关系保留。
5. 原频道 PostgreSQL 范围/幂等、回滚、创建者交接套件：3 PASS。数据库合计 10 PASS，0 skipped。日志 `/tmp/gz-organization-departure-pg.log`。测试数据库每次重建、结束后停止。

显式数据库入口（先编译相应模块，socket 必须指向新建空库；会建测试表，不得指向已有数据库）：

```erlang
organization_departure_pg_tests:run(SocketPath).
workspace_departure_pg_tests:run(SocketPath).
```

## 未闭合范围

这不是全迁移、并发撤销或真机证明。平台 Admin 的独立成员移除路径仍需同步接入同事务撤销；App 本人退出入口、OA 会话退出、资产读取/下载失权、存量资料归属、负责人可操作交接、客服投产与全部 Internal API 仍待完成。没有发布、部署、生产迁移或外向通知。
