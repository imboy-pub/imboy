# IMBoy 路线图 / Roadmap

> 本文档记录 IMBoy 已完成的里程碑与近期 / 中期 / 长期规划。
> This document records completed milestones and near / mid / long-term plans for IMBoy.
>
> **状态基线注记（2026-09-19）**：已完成清单对齐当前版本（`1.0.0-alpha.77`）；计划区完成状态已按代码/CI/文档证据逐项核对（勾选项附验证依据），**目标日期未重排**——GA「2026 Q2」与中期「Q3–Q4」已过期，新日期待产品决策后更新。
>
> **版本策略 / Versioning strategy**：遵循 [Semantic Versioning](https://semver.org/)。
> `1.0.0` 为首个生产就绪 GA 版本；`1.x` 聚焦稳定性与可运维性；`2.x` 引入架构扩展。

---

## 已完成 / Completed

### 1.0.0-alpha.77（当前 · 公测中）

**三端核心功能全部交付 / All three-component core features delivered**

| 功能线 / Feature | 状态 / Status |
|---|---|
| C2C 单聊（WAL 零丢失 / 撤回 / 编辑 / 已读 / 阅后即焚 / 引用回复）| ✅ |
| C2G 群聊（禁言 / @提醒 / 批量投递 / 已读统计）| ✅ |
| WebSocket ACK（4 步重试 / 跨节点 syn 广播 / 心跳）| ✅ |
| 端到端加密 E2EE（RSA-OAEP-256 + AES-256-GCM / 设备迁移）| ✅ |
| Tag 标签系统 / 收藏系统 / 频道系统 / 朋友圈 | ✅ |
| 群组发现（FTS 搜索 / 分类浏览 / 精选 / 热门）| ✅ |
| 频道发现（FTS 搜索 / 分类 / 热门统计 / 精选）| ✅ |
| Agent 公开发现（助手广场 / 搜索 / 分类）| ✅ |
| Bot 基础设施（注册 / Webhook 推送 / api_token 认证 / 防骚扰 / 管理后台）| ✅ |
| Flutter 客户端（iOS / Android / macOS）| ✅ |
| React 管理后台 | ✅ |
| Docker Compose 一键生产部署 + nginx 反代 + certbot 自动 TLS | ✅ |
| 首启初始化向导（消除默认密码风险）| ✅ |
| Prometheus + Grafana 可观测性包（33 条 SLO 告警规则，14 组）| ✅ |
| CI/CD 三端自动化（ci + release + codeql + trivy）| ✅ |
| 部署后 sanity_check.sh（8 项验证）| ✅ |

---

## 近期计划 / Near-term (1.0.0 GA)

**目标发布日期 / Target release date**：2026 Q2

### 必须完成 / Must-have

- [ ] **iOS App Store 上架**：TestFlight 内测 → App Review 正式审核（商店状态需人工确认，未核对）
- [ ] **Google Play 内测轨**：Internal Test → Production track（`imboyapp/scripts/check_play_release.sh` 已备，上架状态需人工确认）
- [ ] **Sentry DSN 生产注入文档化**：`SENTRY_DSN` 环境变量接入指南 + 前后端 source map 上传（deploy/README 下一步清单亦标 pending）
- [ ] **API schema 冻结**：`api/openapi.yaml` (REST) + `api/asyncapi.yaml` (WebSocket) 标注 `stable`

### 应该完成 / Should-have

- [x] **Demo 数据脚本**：`scripts/seed_demo.sh`（2026-09-19 核对：脚本已交付；灌库语义未逐项验证）
- [x] **升级 runbook**：`docs/guides/operations/upgrade-runbook.md`，alpha → 1.0.0 滚动更新 + 回滚 + PITR
- [x] **DCO sign-off CI 强制**：`.github/workflows/backend-ci.yml` 的 `dco-check`（dco-org/dco-action）对所有 PR 生效
- [x] **README.en.md 英文镜像**：仓库根 `README.en.md` 与中文版同步维护

---

## 中期计划 / Mid-term (1.x)

**目标周期 / Timeline**：2026 Q3–Q4

### 可运维性 / Operability

- [x] **Helm chart**：`deploy/helm/`（实验性：副本数固定 1、后端 HPA 默认关、不在交付支持范围——见 deploy/helm/README.md）
- [x] **Loki 日志聚合**：社区版 `docker-compose.community.yml` 监控 profile 已含 `imboy_loki`(3.3.2) + promtail，Grafana 统一日志 + 指标
- [x] **多节点部署文档**：`docs/guides/operations/clustering.md`
- [x] **PG 只读副本**：`docs/guides/operations/postgres-read-replica.md`

### 功能增强 / Feature enhancements

- [ ] **消息翻译**：接入第三方翻译 API，聊天界面长按"翻译"（`src/logic/` 无对应模块，未开始）
- [x] **消息搜索**：基于 `pg_jieba` 全文索引（`fts_logic` + `fts_user/fts_group` 表，迁移 68），客户端跨会话搜索已上线（E2EE 密文按设计排除）
- [ ] **语音消息转文字**：Whisper API 集成（后端流式 + 客户端展示）（无对应模块，未开始）
- [ ] **Windows / Linux 客户端**：Flutter Desktop 正式打包 + 分发（iOS/Android/macOS 已交付，Win/Linux 打包状态 UNKNOWN）
- [ ] **Bot OAuth Grant**：Bot 代表用户操作的授权流程（数据表 `bot_oauth_grant` 已建·迁移 92，但无 API 面——流程未实现，维持 YAGNI）
- [ ] **Bot 市场 / Inline 模式**：`@botname query` 实时卡片返回（未开始）

### 开发体验 / Developer experience

- [x] **本地开发一键环境**：`scripts/dev_setup.sh`
- [x] **API Sandbox**：`docs/api-sandbox/`（Swagger UI 本地交互文档）
- ~~**SDK**：JavaScript / Python 客户端 SDK~~（2026-09-09 决策：删除 imboy-sdk-js，短期内不做 JS SDK；重启需重新立项）

---

## 长期愿景 / Long-term (2.x+)

**目标周期 / Timeline**：2027+

- **联邦协议支持**：探索与 Matrix / XMPP 互通（读取联邦消息，不承诺写入）
- **OpenTelemetry 全链路追踪**：替换现有 Prometheus metrics + Sentry，统一 OTLP
- **AI 助理集成**：内置 LLM 对话能力（本地部署 / 云端 API，用户数据不离境）
- **多租户 SaaS 模式**：基于 PostgreSQL Row-Level Security 的租户隔离
- **性能白皮书**：公开发布百万并发压测方法论与数据

---

## 不在计划中 / Not Planned

以下需求目前不在路线图内，但可以在 [Discussions](../../discussions) 中讨论：
The following are currently out of scope but can be discussed in [Discussions](../../discussions):

- 浏览器端（Web App）PWA / Browser PWA
- 第三方登录（微信 / Google OAuth）/ Third-party OAuth login
- 付费托管云版本 / Paid managed cloud version

---

> 路线图内容随项目进展调整，欢迎在 [GitHub Discussions](../../discussions) 提交功能建议。
> This roadmap evolves with the project. Feature suggestions are welcome in [GitHub Discussions](../../discussions).
