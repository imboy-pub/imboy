# 企业对象删除队列处置清单补齐

源码基线：`0532538fbaed51f65a98564a930f124d8e87e116`。

`enterprise_asset_delete_queue` 保存已提交的对象删除意图。它没有个人用户归属字段，不能因个人账号注销而删除，否则会丢失需要重试的对象定位信息。清单登记为 retain，保留到存储端确认删除或对象不存在，随后由 `eb_pg_asset_delete_queue:finish/3` 删除记录。失败继续保留，工作空间外键 ON DELETE RESTRICT 防止未完成清理时移除归属。

使用现有隔离 PostgreSQL 门的数据库生命周期、原生迁移和 EUnit runner，重新编译全部产品与测试源码后验证；未使用日常数据库。运行目录 `/tmp/imboy-seat-http.fmz2zI`，启动和时间编解码探针均通过。

- `data_disposition_tests`：12 项通过，包含当前数据库全部 public 表覆盖校验。
- `cs_widget_env_tests`：通过，覆盖密钥缺失拒绝、配置合并和验签。
- 三套件组合：42 项通过、1 项失败、0 项跳过，进程退出 1。失败为 `cs_route_contract_tests` 对 `cs_http` 动作名 `presign` 的字符串误判，仍需修复并重跑。

这证明清单缺口已关闭，不证明完整回归、对象存储实际清理或生产投产完成。旧基线 `21e50654` 的全量运行已停止并标记被新提交替代，不能计为 PASS；新的全量门仍待执行。
