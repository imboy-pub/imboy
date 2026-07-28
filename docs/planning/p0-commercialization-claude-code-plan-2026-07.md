# IMBoy 商业化 P0：Claude Code 可重复执行计划

> 版本：2026-07-28  
> 范围：排除 E2EE 实现本身；只处理“安全可卖、可交付、可续费”的商业化底座。  
> 状态唯一真源：[`docs/roadmap/tasks.md`](../roadmap/tasks.md) 中 `tag: commercialization` 的任务块。  
> 执行方式：每轮只执行一个 `status: ready` 且依赖全部 `done` 的任务；验收通过后才改为 `done`。

## 目标

把 IMBoy 从“有支付/License/SSO/部署骨架”推进到首个付费客户可验收的 P0 状态：

1. 计费对象不可越权。
2. License 能稳定控制规模、续费和版次状态。
3. 客户可以替换品牌并构建客户端。
4. 备份、恢复、告警和升级有自动化证据。
5. 至少有可自动测试的 OIDC 企业登录闭环。
6. 合规导出、审计、API 文档和发布检查不再断裂。

## 固化决策：为了无人干预执行

以下决策是本计划的默认实现，不在循环中询问用户：

| 项目 | 固化默认 | 后续扩展 |
|---|---|---|
| Billing 归属 | 单租户简化：`owner_uid = current_uid`；客户端传入的 `tenant_id` 不作为授权依据 | 有真实组织客户后再增加 `billing_tenant` 与成员表 |
| P0 身份协议 | OIDC Authorization Code + PKCE；使用本地 fake IdP 做自动测试 | SAML、LDAP、SCIM 作为 P1 客户触发项 |
| 支付验收 | 只用 `payment_mock_gateway` 验证业务幂等和回调安全；禁止自动切 live | 真实商户凭证到位后单独执行外部任务 |
| 白标验收 | 使用固定 fixture 构建，不涉及 Apple/Google 签名和上架 | 真机签名与商店发布另列人工/外部任务 |
| 失败策略 | 保持当前任务 `in_progress`，记录 blocker；不跳过失败，不把未知改成 done | 连续两轮无进展才停止本轮并报告 |

## 状态机与循环协议

状态值只能使用：`blocked`、`ready`、`in_progress`、`done`、`wont_fix`。

每轮 Claude Code 执行以下流程：

启动商业化专用循环时使用 `focus=commercialization`；状态读取仍来自
`docs/roadmap/tasks.md`，本文件只提供详细动作和验收定义。

```text
1. cd /Users/leeyi/project/imboy.pub/imboy
2. git rev-parse --show-toplevel
3. 读取 docs/roadmap/tasks.md 与本文件
4. 检查当前 worktree；不 stash、不 reset、不覆盖其他会话改动
5. 将 deps 全部 done 且仍为 blocked 的商业化任务刷新为 ready
6. 选择第一个 tag=commercialization 且 status=ready 的任务
7. 将它改为 in_progress，执行 action
8. 执行 verify 中的全部命令；任何一项失败都保持 in_progress 并记录 blocker
9. 全部通过后写入 evidence，改为 done
10. 只提交本任务涉及的文件；禁止 push
11. 重新计算 C0 闸门状态，然后结束本轮
```

若没有可执行任务：

- 有 `blocked` 且 blocker 属于 `external`、`decision`、`device`：输出阻塞项并结束，不等待。
- 有 `blocked` 但依赖状态可以刷新：先刷新状态，再执行下一轮。
- 没有任何可推进项：输出 `P0-COMMERCIALIZATION: WAITING`。

## P0 任务分解

### C0-BILL-01：Billing 归属与越权修复

- 状态：`blocked`
- 依赖：`W0-SEC-01`
- 标签：`commercialization,security`
- 动作：为 `billing_subscription` 增加 `owner_uid` 迁移和 down 脚本；9 个租户动作统一提取 `auth_ds:current_uid(State)`；Logic 层增加 `assert_owner/2`；`invoice_pay` 通过 invoice→subscription 反查归属；新订阅写入当前用户；历史无主订阅只允许管理端处理。
- 自动验收：`make compile`、`make eunit`；新增 8 类跨用户拒绝测试；billing handler 调用必须包含当前用户参数；billing schema 必须存在 `owner_uid`。
- 失败处理：迁移或任一授权测试失败，保持 `in_progress`；不得放宽测试以获得绿灯。

### C0-LICENSE-01：License 规模与续费闭环

- 状态：`ready`
- 依赖：`none`
- 标签：`commercialization`
- 动作：将 `max_nodes` 接入集群加入硬 gate；补签名、域名、过期、宽限、续费、用户数和节点数边界测试；补专业版/企业版 fixture 和脱敏状态 API；不引入远端 License SaaS。
- 自动验收：`make compile`、`make eunit`；断言 `max_nodes=1` 拒绝第二节点、`max_nodes=0` 不限；License API 不泄露原文、私钥和签名材料。

### C0-BRAND-01：白标构建配置

- 状态：`ready`
- 依赖：`none`
- 标签：`commercialization`
- 动作：Flutter 建立单一 `BrandConfig`，覆盖应用名、Logo、启动页、主题主色和客服/隐私文案；缺失或非法配置回退默认值；Admin 复用可验证品牌字段；补默认与白标 fixture；不修改 `ios/*`、`macos/*` 或禁改插件区。
- 自动验收：`flutter analyze`、`flutter test`、默认/白标 fixture 构建配置校验、`bun test`、`bun run build`。

### C0-OPS-01：备份、恢复和健康告警闭环

- 状态：`ready`
- 依赖：`none`
- 标签：`commercialization,operations`
- 动作：为 PostgreSQL 和 Garage 备份提供受版本控制的调度入口；备份成功推送 `imboy_backup_last_success_timestamp`；增加 TLS 证书到期和支付结果指标/告警；增加临时数据库 restore smoke。
- 自动验收：`bash -n scripts/backup_pg.sh scripts/backup_garage.sh deploy/preflight.sh`；`docker compose -f deploy/docker-compose.prod.yml config`；`helm lint deploy/helm -f deploy/helm/values.prod.yaml`；`promtool check rules deploy/prometheus/rules/imboy-alerts.yml`；mock 备份成功/失败退出码和 Pushgateway payload；restore smoke 不触碰生产数据。

### C0-IAM-01：OIDC 企业登录自动化加固

- 状态：`ready`
- 依赖：`none`
- 标签：`commercialization,security`
- 动作：固定 issuer、audience、expiry、nonce 和 PKCE 校验；解决或显式阻断多节点一次性状态；未实现的 SAML/LDAP provider 不得假报已启用；保留本地 fake IdP。
- 自动验收：`make compile`；OIDC EUnit 覆盖重放、claims 错误和 OTC 并发消费；fake IdP 完成 authorize→callback→exchange→JWT 全链路；多节点模式要么共享状态成功，要么 preflight 明确失败。

### C0-GOV-01：审计、数据导出与权限拒绝

- 状态：`blocked`
- 依赖：`C0-IAM-01`
- 标签：`commercialization,security,compliance`
- 动作：实现受限范围 `user/export_data`；审计登录、管理员权限变更、License 变更、计费和导出；RBAC 不可用时拒绝敏感写操作；未实现 Legal Hold 时显式标记不支持。
- 自动验收：`make compile && make eunit`；导出 schema 和敏感字段断言；模拟 `/rbac/me` 404 时敏感写操作被拒；关键动作产生不可变审计事件。

### C0-CONTRACT-01：商业 API 合同与发布验收门

- 状态：`blocked`
- 依赖：`C0-BILL-01,C0-GOV-01`
- 标签：`commercialization,contract`
- 动作：补 finance、billing、License、SSO、export_data OpenAPI；以 handler 实际 payload 生成 schema；加入版本、迁移、License、备份、恢复、升级和支持矩阵检查；增加三仓最小回归。
- 自动验收：`redocly lint api/openapi.yaml`；`make compile && make eunit`；`cd ../imboyadmin && bun test && bun run build`；`cd ../imboyapp && flutter analyze && flutter test`；`git diff --check`。

## P0 闸门：GATE-C0

- 状态：`blocked`
- 依赖：`C0-BILL-01,C0-LICENSE-01,C0-BRAND-01,C0-OPS-01,C0-IAM-01,C0-GOV-01,C0-CONTRACT-01`
- 自动验收：所有依赖 `done`；三仓检查全绿；本地 mock 商业冒烟通过：注册→License quota→OIDC→订阅→mock 支付→审计→导出→备份；`git diff --check`；工作区不存在新增密钥、联系方式或生产数据。
- 通过输出：`P0-COMMERCIALIZATION: READY_FOR_EXTERNAL_CHECKS`。

## 不自动执行的任务

以下任务不进入 `GATE-C0`，Claude Code 不得等待、猜测或调用外部系统：

| 任务 | 状态 | 解锁条件 |
|---|---|---|
| 真实支付宝/微信/Stripe | `blocked` / `external` | 用户提供商户凭证并明确测试环境与金额 |
| Apple/Google 签名与上架 | `blocked` / `external` | 用户人工确认发布窗口 |
| SAML/LDAP/SCIM 完整适配 | P1 | 形成目标客户 IdP 需求 |
| 合规/法律宣传语 | `blocked` / `decision` | 用户或法务确认 |
| E2EE 真机验收 | 现有计划处理 | 继续执行 E2EE 计划 |

## 失败、恢复和幂等规则

- 所有迁移必须有 down 脚本；重复运行前检测 schema、索引和配置是否已存在。
- 所有 fixture 使用固定 ID、临时目录和 mock 外部服务；测试结束必须清理。
- 禁止 `git reset --hard`、`git checkout --`、盲目 `stash pop` 和 push。
- 发现非本任务 dirty 文件时只记录，不修改、不暂存、不提交。
- 提交前运行 `git restore --staged .`，再精确添加本任务文件。
- `evidence` 至少包含提交 SHA、验收命令、关键输出摘要和文件/行号。
