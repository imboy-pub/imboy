# 人类附件确认、消息绑定事务审计

普通群/频道附件确认只对首次持久元数据追加 file.confirmed；复用现有工作区事务锁，确认前后检查实际上传者与不可变 scope/ref。重放保持原引用更新，不重复审计。

首次群消息附件绑定 UPDATE RETURNING 实际附件 ID 后追加 file.message_bound，与消息、anchor、序号、请求账本及接收者快照同事务；审计故障全部回滚，重放不增加事实。Internal API 保持原审计入口。

真实 PostgreSQL 原子矩阵 12 项通过（含两连接同工作区确认、不同工作区同路径受控竞态）；全迁移真实数据库绑定矩阵及相关领域检查共 154 项通过，独立代码复审 APPROVE。原子存储 HEAD/PUT 为 mock；本证据不替代该新候选的 macOS/Garage 全链路验收。
