# 企业应用治理写入与审计事务闭环

状态：本地专项 PASS；整体六项目标仍为 PARTIAL。

历史红证据显示应用状态已提交、审计却为 0 行。治理写面现在复用同一数据库连接：先锁定目标组织下的 Application，保留版本检查、校验与租户范围，再执行业务变更和审计；审计插入返回空集或 SQL 错误均明确回滚。凭证明文只在成功提交后的返回值出现，审计只含允许的元数据与平台管理员归因。

Grant 同时修改 scopes/workspaces 仍沿用两次 CAS、成功后 version+2 的现有契约，但两次修改处于同一事务；第二次失败连同第一次完整回滚。读取目标凭证和 Grant 也在事务内执行，不再用独立查询或空壳元数据掩盖错误。删去分离事务和吞错的重复逻辑，未引入依赖或迁移。

真实一次性 PostgreSQL，完整现有迁移，fresh compile 当前产品源码：

- `0sVYyF` exit 0：应用状态、应用 scopes、凭证签发/轮换/撤销、Grant 签发/权限/工作空间/组合修改/撤销共 10 类写入，每类分别使用原生触发器 RETURN NULL 和 RAISE EXCEPTION。20 次均返回审计错误，Application/Credential/Grant 及子表完整行投影保持不变、审计为 0；移除故障后同一请求重试成功，恰有 1 条审计。
- 组合修改使用其他组织的 Workspace 时，权限与版本也完整回滚；有效组合随后成功。
- 两个同版本并发状态请求恰有一个成功、一个 version_conflict、1 条审计。
- 跨组织应用和其他组织凭证被拒绝，状态与审计不变。
- 成功审计 actor_user_id 为 null、记录合成平台管理员账号、无 secret/digest 字段。公开 credential_prefix 仍允许进入审计。

现有 Internal HTTP 门禁 `8mifpG` exit 0：26 项通过，身份契约检查通过。该门禁不替代 Admin HTTP 全流程。

复现：

```sh
IMBOY_DEPS_ROOT=/path/to/independent/deps IMBOY_ADMIN_GOVERNANCE_PG_CHECK=1 bash scripts/test/customer_service_internal_http_gate.sh
```

源码 SHA、运行状态、实际断言结果及日志归档于 `evidence/enterprise-governance-audit-atomic-2026-10-01`；原始失败源码与运行结果继续保留在历史红证据目录。未归档凭证、配置或崩溃转储。

本专项验证实际池化治理逻辑与数据库，不代表 Admin 页面/HTTP 全流程、真实 OA、App 真机或全量投产资格。没有生产操作。
