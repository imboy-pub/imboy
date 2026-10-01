# 企业应用治理审计失败被忽略

状态：历史 FAIL 证据；本地修复与回归见 [事务闭环](enterprise-governance-audit-atomic-2026-10-01.md)。六项目标仍为 PARTIAL。

enterprise_admin_governance_logic 的写面先调用池化独立事务提交业务，再用 append_audit 单独开启事务。append_audit 对任何审计失败仅记录日志并返回 ok。应用状态、能力 scopes、凭证签发/轮换/撤销、Grant 签发/变更/撤销均复用此模式；Grant 同时改 scopes/workspaces 还分为两个独立 CAS，第二步失败可能留下第一步变更。不是仅撤销一个入口的局部问题。

真实一次性 PostgreSQL /tmp/imboy-seat-http.vjPWCr exit 1：创建全新合成企业应用，BEFORE INSERT 审计触发器对 application_status_changed 返回 NULL。调用实际池化治理 set_status，操作返回成功，应用从 active/version=1 变为 disabled/version=2，审计 0 行。严格要求失败时状态不变的断言失败。没有用 meck 替换审计结果。

复现：

```sh
IMBOY_DEPS_ROOT=/path/to/independent/deps IMBOY_ADMIN_GOVERNANCE_PG_CHECK=1 bash scripts/test/customer_service_internal_http_gate.sh
```

新门禁复用独立容器、marker 库和完整迁移，fresh compile 当前产品；只验证实际治理 logic 与仓储，不宣称 Admin HTTP/页面或真实 OA 已验证。归档在 evidence/enterprise-governance-audit-red-2026-10-01，保存原红检查源码、实际状态响应、日志与 SHA；不保存配置、JWT、凭证或崩溃转储。

修复必须让每种写面与审计使用同一连接、同一事务，审计失败明确回滚且返回错误；双字段 Grant patch 不可部分提交。保留 CAS、租户边界、平台管理员归因和 secret 只返回一次的约束，不通过删除审计检查或忽略错误解决。原始红检查源码已归档；持续门禁保留严格回滚断言，没有生产变更。
