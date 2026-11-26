# 部署说明

本目录保留生产部署与运维配置。**本地开发不要从本目录选择多个 Compose 文件**；统一使用仓库根目录的 \`docker-compose.yml\`。

## 本地开发

\`\`\`bash
cp .env.example .env
./scripts/dev_setup.sh
\`\`\`

完整说明见 [本地开发指南](../docs/local-development.md)。它启动 PostgreSQL 18.7、Garage、OTP 29 API、React Admin、Widget/客服坐席、LiveKit 和 Nginx；不启动 Uptrace、Prometheus 等监控服务。

## 生产部署入口

生产环境与本地开发不同：需要准备域名、TLS、持久化目录、备份、密钥和外部网络策略，不能承诺 5 分钟完成。

- 社区版生产 Compose：\`deploy/docker-compose.community.yml\`
- 客服 Widget 生产扩展：\`deploy/docker-compose.prod-cs.yml\`
- LiveKit TURN：\`deploy/docker-compose.livekit-turn.yml\`
- 监控组件是可选运维扩展，不是 IMBoy 基本功能的依赖。
- 安装脚本入口：\`./deploy/install.sh --help\`

## 社区版与企业版

本地开发的 Community / Enterprise 服务定义共用根目录 Compose，通过 \`IMBOY_EDITION\` 和 \`IMBOY_PRODUCT_PROFILE\` 选择运行配置。企业版仍需要对应商业代码、授权和许可证；切换 profile 不会自动获得商业功能。

生产环境企业版镜像和授权材料按商业交付渠道提供，不应把本地 profile 当作企业版生产安装包。

## 客服 Widget 与坐席工作台

Widget 不是独立业务后端，也不需要单独部署数据库或 Redis：

1. 在 \`imboy-admin-frontend\` 执行 \`bun install --frozen-lockfile\`。
2. 执行 \`bun run build:widget\`，生成 Widget、Loader 和 Seat Console 静态资源。
3. 执行 \`bun run verify:widget\` 检查构建产物。
4. 使用前端仓库的 \`Dockerfile.widget\` 构建生产静态资源镜像。
5. Nginx 将 Widget / Seat Console API、会话、消息和 SSE 事件转发给 IMBoy API；静态文件由 Widget Nginx 提供。SSE 路由必须关闭代理缓冲。

本地 Compose 已自动完成前四步中的构建与校验，并提供静态 Widget 服务。生产路由仍应按正式域名和 TLS 配置部署。

## Customer Input

| 配置项 | 用途 |
|---|---|
| \`IMBOY_PG_PASSWORD\` | PostgreSQL 主库密码 |
| \`IMBOY_PG_PRO_PASSWORD\` | 独立生产数据库密码 |
| \`IMBOY_GARAGE_ACCESS_KEY\` / \`IMBOY_GARAGE_SECRET_KEY\` | Garage S3 访问凭据 |
| \`GARAGE_RPC_SECRET\` | Garage 节点 RPC 密钥 |
| \`LIVEKIT_API_KEY\` / \`LIVEKIT_API_SECRET\` | LiveKit API 凭据 |
| \`API_DOMAIN\` / \`ADMIN_DOMAIN\` / \`RTC_DOMAIN\` | 生产 API、管理后台和音视频域名 |
| \`CS_WIDGET_DOMAIN\` | 客服 Widget 域名 |
| \`IMBOY_EDITION\` / \`IMBOY_PRODUCT_PROFILE\` | Community / Enterprise 运行配置 |

生产密钥必须使用独立随机值，不要复用仓库中的本地开发默认值，也不要把真实密钥提交到 Git。
