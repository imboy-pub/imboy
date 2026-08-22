# IMBoy 路线图 / Roadmap

> 本文档记录 IMBoy 已完成的里程碑与近期 / 中期 / 长期规划。
> This document records completed milestones and near / mid / long-term plans for IMBoy.
>
> **版本策略 / Versioning strategy**：遵循 [Semantic Versioning](https://semver.org/)。
> `1.0.0` 为首个生产就绪 GA 版本；`1.x` 聚焦稳定性与可运维性；`2.x` 引入架构扩展。

---

## 已完成 / Completed

### 1.0.0-rc.1（当前）

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
| Prometheus + Grafana 可观测性包（13 条 SLO 告警规则）| ✅ |
| CI/CD 三端自动化（ci + release + codeql + trivy）| ✅ |
| 部署后 sanity_check.sh（8 项验证）| ✅ |

---

## 近期计划 / Near-term (1.0.0 GA)

**目标发布日期 / Target release date**：2026 Q2

### 必须完成 / Must-have

- [ ] **iOS App Store 上架**：TestFlight 内测 → App Review 正式审核
- [ ] **Google Play 内测轨**：Internal Test → Production track
- [ ] **Sentry DSN 生产注入文档化**：`SENTRY_DSN` 环境变量接入指南 + 前后端 source map 上传
- [ ] **API schema 冻结**：`imboy/doc/api/openapi.yaml` (REST) + `asyncapi.yaml` (WebSocket) 标注 `stable`

### 应该完成 / Should-have

- [ ] **Demo 数据脚本**：`script/seed_demo.sh`，幂等灌库（5 用户 / 2 群）
- [ ] **升级 runbook**：`doc/operations/upgrade-runbook.md`，rc.1 → 1.0.0 滚动更新 + 回滚 + PITR
- [ ] **DCO sign-off CI 强制**：所有 PR 要求 `Signed-off-by` 行
- [ ] **README.en.md 英文镜像**：与 README.md 保持同步

---

## 中期计划 / Mid-term (1.x)

**目标周期 / Timeline**：2026 Q3–Q4

### 可运维性 / Operability

- [ ] **Helm chart**：`deploy/helm/` 支持 Kubernetes 单集群部署
- [ ] **Loki 日志聚合**：`docker-compose.prod.yml` 新增 `imboy_loki` 服务，Grafana 统一日志 + 指标
- [ ] **多节点部署文档**：Erlang 集群节点发现（`epmd` / DNS SRV）配置指南
- [ ] **PG 只读副本**：读写分离配置，减轻主库压力

### 功能增强 / Feature enhancements

- [ ] **消息翻译**：接入第三方翻译 API，聊天界面长按"翻译"
- [ ] **消息搜索**：基于 `pg_jieba` 全文索引，客户端跨会话关键词搜索
- [ ] **语音消息转文字**：Whisper API 集成（后端流式 + 客户端展示）
- [ ] **Windows / Linux 客户端**：Flutter Desktop 正式打包 + 分发
- [ ] **Bot OAuth Grant**：Bot 代表用户操作的授权流程（待真实场景，YAGNI）
- [ ] **Bot 市场 / Inline 模式**：`@botname query` 实时卡片返回

### 开发体验 / Developer experience

- [ ] **本地开发一键环境**：`script/dev_setup.sh` 自动配置 PG + 后端 + 前端热重载
- [ ] **API Sandbox**：基于 OpenAPI 3.1 的在线交互文档（Swagger UI / Redoc）
- [ ] **SDK**：JavaScript / Python 客户端 SDK（WebSocket + REST 封装）

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
