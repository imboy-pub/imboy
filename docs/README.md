# IMBoy 文档

第一次接触项目，请先按 [根 README](../README.md) 启动后端，再根据任务查阅下面的专题文档。

## 开发

- [后端架构](./architecture/overview.md)：系统分层和核心组件
- [模块地图](./architecture/module_map.md)：功能对应的代码位置
- [数据库访问](./architecture/database-access.md)：Repo 与 SQL 规范
- [API 格式](./standards/api-format.md)：请求、响应和分页约定
- [错误码](./standards/error-codes.md)：错误码定义与使用
- [REST API 目录](./analysis/rest-api-v1-catalog.md)：`/api/v1` 接口速查
- [WebSocket 协议](./analysis/ws-protocol-contract.md)：消息信封与事件约定

## 部署与运维

- [生产部署](../deploy/README.md)
- [备份与恢复](./operations/deployment/BACKUP-RESTORE.md)
- [版本升级](./operations/upgrade-runbook.md)
- [Garage 附件存储](./operations/garage-deployment.md)
- [监控](./operations/deployment/MONITORING.md)

## 安全

- [运行安全](./operations/security.md)
- [安全加固](./security/security-hardening.md)
- [漏洞报告](../SECURITY.md)

## 文档维护

- 优先更新现有文档，不为一次性任务新建长期文档。
- 主索引只收录稳定、仍可执行的内容。
- 计划、评审和审计材料保留在各自目录，不作为新手入口。
- 接口或部署方式变化时，同步更新根 README 和对应专题文档。
