<p align="center">
  <img src="./docs/brand/logo/imboy_logo_2474E5_0512.png" alt="IMBoy" width="140" />
</p>

<h1 align="center">IMBoy</h1>

<p align="center">
  <strong>高性能、可自托管、端到端加密的开源即时通讯 SKU</strong>
</p>

<p align="center">
  <strong>简体中文</strong> | <a href="README.en.md">English</a>
</p>

<p align="center">
  <img src="https://img.shields.io/badge/version-1.0.0--rc.1-2474E5" alt="version" />
  <img src="https://img.shields.io/badge/license-MulanPSL--2.0-blue" alt="license" />
  <img src="https://img.shields.io/badge/Erlang%2FOTP-28%2B-A90533" alt="erlang" />
  <img src="https://img.shields.io/badge/PostgreSQL-18%2B-336791" alt="postgres" />
  <img src="https://img.shields.io/badge/Flutter-3.41%2B-02569B" alt="flutter" />
  <img src="https://img.shields.io/badge/React-19.2-61DAFB" alt="react" />
</p>

---

IMBoy 是一款以「**单机百万并发**」为目标设计的开源即时通讯系统，覆盖单聊、群聊、频道、朋友圈、端到端加密等 10 条完整功能线。后端基于 Erlang/OTP + Cowboy + PostgreSQL，客户端覆盖 iOS/Android/macOS/Windows/Linux（Flutter），并提供 React 管理后台，面向需要自托管 IM 的企业、SaaS 团队与独立开发者。

本仓库是一个**多项目工作区**（非单一 Git 仓库），三端（backend / app / admin）可各自独立发布，版本号在 workspace 层统一为 `VERSION` 文件（当前 `1.0.0-rc.1`）。

## 为什么选择 IMBoy

- **端到端加密（默认关闭，可选启用）**：RSA-OAEP-256 + AES-256-GCM 信封套件，服务端不解密 `ciphertext`，含社交恢复与设备迁移能力。⚠️ 当前客户端默认走"传输加密 + 服务端存储加密"路径（消息可被服务端读取）；真端到端为可选项，启用需完成密钥恢复体系（路线图中）。无前向保密（PFS），未经第三方安全审计。
- **严格顺序保障**：消息永久存储采用 `conv_seq` 游标（per-conversation 单调递增），跨数据中心、跨节点依然可严格重放，不依赖 `TSID` 做顺序依据。
- **高并发底座**：Erlang/OTP 天然分布式。「单机百万并发」为架构设计目标，待发布可复现的第三方压测报告佐证；当前推荐单节点部署（多节点水平扩展见路线图，跨节点 ACK 去重状态迁移开发中）。
- **完整三端交付**：Flutter 客户端（含 12 个 E2EE 设置页）+ React 管理后台（27 单元 + 4 E2E 测试）+ Erlang 后端（382 文件、90k+ 行、0 功能性 TODO）。
- **一键自托管**：`deploy/docker-compose.prod.yml` 一键起 PG18+后端+管理后台+nginx 反代（`nginx:1.27-alpine`）+ certbot（Let's Encrypt 自动签发/续期），首次部署执行一次 `bash nginx/init-letsencrypt.sh` 签发证书，通过 `/setup` 向导创建超级管理员，无需 `erl shell`。
- **MulanPSL-2.0 开源**：商业友好的中国本土开源协议，三端统一授权。

## 10 条功能线

| # | 功能线 | 代码完整度 | 要点 |
|---|---|---|---|
| 1 | 单聊 (C2C) | 100% | WAL 零丢失 / 撤回 / 编辑 / 已读 / 阅后即焚 / 引用回复 |
| 2 | 群聊 (C2G) | 100% | 禁言 / @提醒 / 管理员 @all / 批量投递 / 已读统计 |
| 3 | 会话管理 | 100% | 合并 C2C+C2G / 软删除 / 置顶 / 分页 |
| 4 | 消息提醒 | 100% | FCM + APNs（后端）+ 客户端本地通知 |
| 5 | WebSocket / ACK | 100% | 4 步重试 2s→5s→7s→11s / syn 跨节点广播 / 心跳 120s |
| 6 | 端到端加密 (E2EE) | 能力已实现·**客户端默认关闭** | RSA-OAEP-256 + AES-256-GCM / 社交恢复 / 设备迁移；默认明文+服务端存储加密，真 E2E 为可选项；无 PFS、未审计 |
| 7 | Tag 标签系统 | 100% | 好友分组 / 收藏命名空间 / 级联删除 |
| 8 | 收藏系统 | 100% | 文本 / 图片 / 语音 / 视频 / 文件 / 位置 / 联系人 |
| 9 | 频道系统 | 100% | 订阅 / 发布 / 管理员 / 邀请 / 付费 / 统计 |
| 10 | 朋友圈 | 100% | ACL 隐私 / 评论 / 点赞 / 举报 / 管理员审核 |

详见 [`IMBOY_FEATURE_PROGRESS.md`](./IMBOY_FEATURE_PROGRESS.md)。

## 系统架构

```
                   ┌─────────────────────────────────────┐
                   │            Clients                  │
                   │  Flutter App   │   React Admin      │
                   │  (iOS/Android/ │   (imboy-admin-    │
                   │   Desktop)     │    frontend)       │
                   └────────┬───────┴─────────┬──────────┘
                            │ WSS + HTTPS     │ HTTPS
                            ▼                 ▼
                   ┌─────────────────────────────────────┐
                   │    nginx (TLS via certbot/ACME)     │
                   └────────────────┬────────────────────┘
                                    ▼
                   ┌─────────────────────────────────────┐
                   │        IMBoy Backend (Erlang)       │
                   │  ┌──────────────────────────────┐   │
                   │  │ Handler → Logic → DS → Repo  │   │
                   │  └──────────────────────────────┘   │
                   │  Cowboy 2.10 │ syn │ depcache      │
                   └────────┬───────────────────┬────────┘
                            ▼                   ▼
                   ┌────────────────┐  ┌────────────────┐
                   │ PostgreSQL 18+ │  │   FastDFS      │
                   │ + pg_jieba     │  │   (附件存储)   │
                   │ + postgis      │  └────────────────┘
                   │ + timescaledb  │
                   │ + pgcrypto     │
                   └────────────────┘
```

详细分层、模块索引与 Mermaid 架构图见 [`imboy/CLAUDE.md`](./imboy/CLAUDE.md)。

## 子项目

| 目录 | 说明 | 技术栈 |
|---|---|---|
| [`imboy/`](./imboy) | Erlang/Cowboy 即时通讯后端 | Erlang/OTP 28+ · Cowboy 2.10 · PostgreSQL 18+ |
| [`imboyapp/`](./imboyapp) | Flutter 跨平台客户端 | Flutter 3.41+ · Riverpod · Dart 3 |
| [`imboy-admin-frontend/`](./imboy-admin-frontend) | React 管理后台 | React 19.2 · TypeScript · Vite · Bun |
| [`elib/`](./elib) | 共享 Erlang 基础库 | Erlang/OTP |
| [`go-fastdfs/`](./go-fastdfs) | 附件存储组件 | Go |
| [`deploy/`](./deploy) | 一键生产部署包 | Docker Compose · nginx · certbot |

## 快速开始（生产部署）

**前置**：一台 Linux 服务器（Ubuntu 22.04 / Debian 12 / Alma 9 推荐）、Docker 24+、两个解析到本机的域名、开放 80/443 端口。

```bash
# 1. 克隆工作区
git clone https://github.com/imboy-pub/imboy.git
cd imboy/deploy

# 2. 准备环境变量
cp .env.example .env
$EDITOR .env          # 修改 API_DOMAIN / ADMIN_DOMAIN / 密钥三件套

# 3. 前置检查
bash ../script/preflight.sh --docker

# 4. 启动
docker network create imboy-network 2>/dev/null || true
docker compose -f docker-compose.prod.yml up -d

# 5. 初始化
#    浏览器打开 https://${ADMIN_DOMAIN}
#    自动跳转 /setup 向导 → 填手机号/邮箱 + 强密码 + 昵称
#    完成后跳回 /login 登录
```

**无需 `erl shell`** — 首启向导（P0-5）已实现，配置 flag + `adm_user` 表存在性双重防线，仅允许执行一次。

完整部署指南见 [`deploy/README.md`](./deploy/README.md)。

## 本地开发

### 后端

```bash
cd imboy
make compile
make eunit
IMBOYENV=local make run
```

### Flutter 客户端

```bash
cd imboyapp
flutter pub get
flutter run --dart-define=APP_ENV=local_home
```

10 条功能线自动化入口：

```bash
cd imboyapp
bash test_automation/scripts/run_yaml_mapped_suite.sh --dry-run
```

### 管理后台

```bash
cd imboy-admin-frontend
bun install
bun run dev         # 开发服
bun test            # 单元测试
bun run test:e2e    # Playwright E2E（含 P0-5 首启向导 6 条）
```

## 文档与资源

| 主题 | 链接 |
|---|---|
| 后端架构（DDD 4 层） | [`imboy/CLAUDE.md`](./imboy/CLAUDE.md) |
| 管理后台规范 | [`imboy-admin-frontend/CLAUDE.md`](./imboy-admin-frontend/CLAUDE.md) |
| WebSocket API v2 | [`imboy/doc/api/websocket-api-2.md`](./imboy/doc/api/websocket-api-2.md) |
| 部署指南 | [`deploy/README.md`](./deploy/README.md) |
| 功能完成度审计 | [`IMBOY_FEATURE_PROGRESS.md`](./IMBOY_FEATURE_PROGRESS.md) |
| 变更日志 | [`CHANGELOG.md`](./CHANGELOG.md) |
| 可观测性 / Sentry | [`imboy/doc/operations/observability.md`](./imboy/doc/operations/observability.md) |
| 备份与恢复 | [`imboy/doc/operations/deployment/BACKUP-RESTORE.md`](./imboy/doc/operations/deployment/BACKUP-RESTORE.md) |
| 隐私政策 | [`PRIVACY_POLICY_TEMPLATE.md`](./PRIVACY_POLICY_TEMPLATE.md) |
| 安全披露 | [`SECURITY.md`](./SECURITY.md) |
| 贡献指南 | [`CONTRIBUTING.md`](./CONTRIBUTING.md) |
| 行为准则 | [`CODE_OF_CONDUCT.md`](./CODE_OF_CONDUCT.md) |
| 路线图 | [`ROADMAP.md`](./ROADMAP.md) |
| 获取帮助 | [`SUPPORT.md`](./SUPPORT.md) |

## 项目路线图

- **1.0.0-rc.1** — 当前候选版，首个标准 SKU。P0-5 首启向导已落地，三端功能完整。
- **1.0.0** — 补全 Sentry DSN 生产注入、Grafana dashboards、升级 runbook、brand/ 资源、seed_demo 脚本。
- **1.1.x** — OpenAPI/AsyncAPI schema 冻结、docs-site VitePress 站、iOS 正式上架。
- **企业版** — 审计日志增强、多租户隔离、高级 RBAC、SSO。

完整状态见 [`.claude/plan/standard-sku-gap-2026-04-10.md`](./.claude/plan/standard-sku-gap-2026-04-10.md)（内部跟踪文档）。

## 贡献

欢迎任何形式的贡献 — bug 报告、功能请求、文档修正、代码提交。请先阅读：

- [贡献指南](./CONTRIBUTING.md)
- [行为准则](./CODE_OF_CONDUCT.md)
- [安全披露流程](./SECURITY.md)（**请勿**在公开 issue 中报告安全漏洞）

提交 PR 前请确保相关项目的本地检查通过：

```bash
cd imboy && make eunit && make dialyze          # 后端
cd imboyapp && flutter test                     # Flutter
cd imboy-admin-frontend && bun test             # 管理后台
```

## 工作区约束

- 根目录不是一个统一的 Git 仓库；`imboy`、`imboyapp`、`imboy-admin-frontend` 各自维护 Git 状态。
- 各子项目的 `CLAUDE.md` 是 AI 上下文与项目约定的权威来源。
- 本 README 只记录稳定、可执行、可追踪的信息，不存放临时笔记、账号密码、API key 或 prompt 残留。

## 许可证

[MulanPSL-2.0](./LICENSE) © IMBoy Contributors

三端（`imboy/` `imboyapp/` `imboy-admin-frontend/`）均采用同一许可证，各自目录下保留完整副本。

