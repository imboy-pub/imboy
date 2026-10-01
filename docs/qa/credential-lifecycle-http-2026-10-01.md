# 凭证与授权生命周期真实 HTTP 回归

状态：本地专项 PASS。六项目标仍为 PARTIAL；本记录不代表全部 Internal 端点或 Admin HTTP/页面已通过。

复用现有 enterprise_app_credential_lifecycle_http_pg_tests，不另造夹具。它现在接入持续 native governance 门禁，共用一次性 marker 数据库、真实 Cowboy listener 与完整中间件认证链。独立运行该 EUnit 套件的 teardown 也明确释放 marker 数据库。产品逻辑没有新增改动。

真实独立 PostgreSQL / 完整迁移 / fresh compile 当前产品源码：`ncUZUH` exit 0，6 项 HTTP 生命周期检查全部通过；此前的审计故障、重复/并发轮换、组合修改回滚等 native 检查同轮通过。

安全响应归档的实际状态顺序：

| 场景 | HTTP 状态 |
|---|---|
| 已签发凭证与有效 Grant | 200 |
| 轮换后的旧凭证 | 401 |
| 轮换后的新凭证 | 200 |
| 撤销新凭证后 | 401 |
| 撤销 Grant 前 / 后 | 200 / 403 |
| 无 Grant / 应用停用 / Grant 过期 | 403 / 403 / 403 |
| 审计故障前 / 撤销失败后 / 成功重试撤销后 | 200 / 200 / 401 |

成功响应逐项核对真实 organization_id/application_id/credential_id 和精确 granted_scopes。所有响应递归拒绝 secret/digest 等敏感键，并确认完整请求凭证未回显。数据库 schema 检查收紧为 secret 类列只有 secret_digest；Admin 读取仍不得返回 secret/digest。归档仅保存安全身份投影与错误码，不含请求 Authorization、原始凭证或 digest。

原生审计触发器 RETURN NULL 时，实际治理 revoke 返回 audit_failed；HTTP 原凭证仍可用，证明事务回滚不只是在 SQL 表面检查。移除故障后同请求重试成功，HTTP 下一请求立即 401。治理动作仍经真实池化逻辑，尚未替代 Admin Cookie HTTP 的完整旅程。

复现：

```sh
IMBOY_DEPS_ROOT=/path/to/independent/deps IMBOY_ADMIN_GOVERNANCE_PG_CHECK=1 bash scripts/test/customer_service_internal_http_gate.sh
```

证据及源码 SHA：`evidence/credential-lifecycle-http-2026-10-01`。本专项实际调用 INT-01 自检端点来验证生命周期影响；当前 route manifest 的 42 条 API 仍需逐项业务验收，不把 6 项测试当作 42 条端点覆盖。未做生产操作、真实 OA 或 App 真机验证。
