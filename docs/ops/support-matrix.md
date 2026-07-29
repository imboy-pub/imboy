# IMBoy 支持矩阵 / Support Matrix

> 私有化交付的依赖版本与支持范围。发布前由 `scripts/check_release_consistency.sh`
> 校验本文件存在且声明了关键依赖。
>
> 产品版本以仓库根 `VERSION` 为准（当前 `1.0.0-alpha.16`，与 `relx.config` 同步）。

## 服务端运行时

| 组件 | 支持版本 | 说明 |
|---|---|---|
| Erlang/OTP | 28+ | OTP 28 把裸 `catch` 视为编译错误，源码统一 `try...catch` |
| PostgreSQL | 18+ | 扩展需求：`pg_jieba`、`postgis`、`timescaledb`、`pgcrypto` |
| Docker / Docker Compose | 24+ / Compose v2 | 演示与评估环境（`deploy/docker-compose.demo.yml`） |
| Kubernetes (可选) | 1.27+ | Helm chart 见 `deploy/helm/` |
| 对象存储 | Garage S3 兼容 | 附件直传（presigned PUT/GET） |

## 客户端

| 端 | 支持版本 | 说明 |
|---|---|---|
| Flutter | 3.8+ | iOS / Android 客户端 |
| iOS | 13.0+ | — |
| Android | API 21+ (Android 5.0) | — |
| Web 管理后台 | Chrome / Edge / Firefox / Safari 最近两个大版本 | React 19 + Vite 构建产物 |

## 授权与计费

| 能力 | 状态 | 说明 |
|---|---|---|
| License 规模/配额 gate | 支持 | `imboy_license`，RSA-SHA256 验签；状态查询只暴露 7 个脱敏白名单字段 |
| 社区版 | 支持 | 无 license 文件时的默认档，用户数上限可由 `community_max_users` 覆盖 |
| 专业版试用 | 支持 | 无 license 时自动签发的试用期（默认 30 天） |
| 到期宽限 | 支持 | 过期后 7 天内仍按授权运行，仅告警 |
| SaaS 订阅/账单 | 支持 | `/api/v1/billing/*` + `/api/adm/finance/billing/*` |
| 真实支付渠道（支付宝/微信/Stripe） | **未开通** | 需商户凭证，属外部阻塞项，本地仅 mock 验证 |

## 身份与合规

| 能力 | 状态 | 说明 |
|---|---|---|
| 账号密码 / 短信 / 扫码登录 | 支持 | — |
| OIDC (OAuth2) 单点登录 | 支持 | PKCE S256、issuer/audience/nonce 校验；多节点需粘性会话或共享 state |
| SAML | **未实现** | 配置可预存但不可启用（fail-closed） |
| LDAP | **未实现** | 同上 |
| SCIM 用户同步 | **未实现** | P1 |
| 个人数据导出（GDPR 第 20 条） | 支持（受限范围） | 同步导出账户/好友/群组/设置，敏感字段剥离并写审计 |
| Legal Hold（诉讼保全） | **未实现** | 导出响应显式声明 `supported=false` |

## 备份与恢复

| 项 | 命令 | 说明 |
|---|---|---|
| PostgreSQL 备份 | `scripts/backup_pg.sh` | 定时任务见 `deploy/cron/imboy-ops.cron` |
| PostgreSQL 恢复 | `scripts/restore_pg.sh` | timescaledb 需 pre/post_restore 包裹，禁止并行恢复 |
| 恢复演练 | `scripts/restore_smoke.sh` | 校验备份可用性 |
| 对象存储备份 | `scripts/backup_garage.sh` | — |

## 升级

- 迁移由 `erlang_migrate` 驱动，序号为时间戳式递增；每个 `*.up.sql` 必须配套 `*.down.sql`
  （由 `scripts/check_release_consistency.sh` 校验）。
- 升级顺序：备份 → 迁移 → 滚动重启（蓝绿见 `scripts/deploy.sh`）。
- 破坏性 API 变更通过 `/v2` 路径前缀发布，`1.0.x` 期间路径签名向后兼容。
