# 环境变量与配置参考 / Environment Variables & Configuration Reference

> **Last Updated:** 2026-09-30
> **真源 / Source of Truth:**
> - `deploy/.env.example`（部署期变量）
> - `config/sys.config.example` 及 `config/sys.*.config` 结构（运行时配置键）
> - `src/lib/imboy_env.erl`（`IMBOY_*` → sys.config 键的覆盖映射）
> - `src/imboy_app.erl: validate_runtime_config/0` + `src/lib/imboy_secret_policy.erl`（生产 fail-fast 必填清单）
>
> 部署流程文档不在本文范围，见 `deploy/README.md`。

## 配置加载链

1. **构建期**：`IMBOYENV` 决定加载 `relx$(IMBOYENV).config` + `config/sys.$(IMBOYENV).config`（未设置回退 `relx.config` + `config/sys.config`，后者不存在时回退 `sys.config.example`）。选中的 sys 配置被复制为 `config/sys.runtime.config` 后启动。
2. **启动期**：`imboy_env:override_from_env/0` 读取进程环境里的 `IMBOY_*` 变量，覆盖 sys.config 同名键。**优先级：`IMBOY_*` 环境变量 > sys 配置文件。**
3. **环境判定**：`imboy_env:current/0` 读取 `IMBOYENV`（缺省回退 `application env imboy.env`，最终 fail-safe 为 `prod`）。`imboy_app:is_strict_env/1`：**仅显式 `dev` / `local` / `test` 宽松，空或未知值一律按生产严格校验。**

<!-- AUTO-GENERATED:fail-fast-required BEGIN（提取自 imboy_app:validate_runtime_config/0 与 imboy_secret_policy；仅 IMBOYENV ∉ {dev,local,test} 时校验） -->

## 生产 fail-fast 必填（缺任一项节点拒绝启动）

| 变量 | 要求 | 校验来源 |
|------|------|----------|
| `IMBOY_JWT_KEY` | 必填，≥32 字节，与下列两者互异 | `imboy_secret_policy:validate_strict/0` |
| `IMBOY_POSTGRE_AES_KEY` | 必填，≥32 字节，三者互异 | 同上 |
| `IMBOY_ADM_COOKIE_SECRET` | 必填，三者互异 | 同上 |
| `IMBOY_SOLIDIFIED_KEY` | 必填，32 字节（AES-256-CBC + HMAC-SHA512） | `ensure_required_secret` |
| `IMBOY_SOLIDIFIED_KEY_IV` | 必填，16 字节 | `ensure_required_secret` |
| `IMBOY_PASSWORD_SALT` | 必填；**投产后不可更改**（改了存量旧格式密码全部失效） | `ensure_required_secret` |
| `IMBOY_LOGIN_RSA_PUB_KEY_FILE` | 必填，容器内 PEM 路径，文件必须存在 | `ensure_required_file` |
| `IMBOY_LOGIN_RSA_PRIV_KEY_FILE` | 必填，容器内 PEM 路径，文件必须存在 | `ensure_required_file` |
| `IMBOY_API_AUTH_SWITCH` | 必须为 `on`（生产显式开启 API 认证） | `ensure_api_auth_switch_on/0` |
| `IMBOY_PG_PASSWORD` | 必填且不得为默认/弱值（`123456`/`password`/`abc54321`/空 均拒绝；`pg_conf` 与 `super_account` 同步校验） | `ensure_pg_password_not_default/0` 等 |
| `IMBOY_PAYMENT_MODE` | 外部支付网关启用时必须为 `live`；strict 环境下 `sandbox` 直接启动失败（sandbox 跳过回调验签） | `ensure_payment_gateway_config/0` |
| `IMBOY_PLUGIN_TRUSTED_PUBLIC_KEY_FILES` | 商务版（`IMBOY_PRODUCT_PROFILE=enterprise`）必填：≥1 个可信 Ed25519 公钥文件 | `ensure_plugin_signature_config/0` |

<!-- AUTO-GENERATED:fail-fast-required END -->

<!-- AUTO-GENERATED:conditional-required BEGIN（提取自 imboy_app ensure_jpush_if_push_enabled / ensure_sms_if_enabled / ensure_payment_gateway_config 与 deploy/.env.example） -->

## 条件必填（由功能开关驱动）

| 前提条件 | 必填变量 |
|----------|----------|
| 推送启用（`{push, enabled, true}`） | `IMBOY_JPUSH_APP_KEY`、`IMBOY_JPUSH_MASTER_SECRET` |
| 短信启用（`IMBOY_SMS_SWITCH=on`）且平台 `yjsms` | `IMBOY_YJSMS_ACCOUNT`、`IMBOY_YJSMS_SECRET`、`IMBOY_YJSMS_URL` |
| 短信启用且平台 `jsms` | `IMBOY_JPUSH_APP_KEY`、`IMBOY_JPUSH_MASTER_SECRET`、`IMBOY_JSMS_TEMP_ID`、`IMBOY_JSMS_SIGN_ID` |
| 外部支付网关启用（`IMBOY_PAYMENT_GATEWAY_ENABLED=true`，`IMBOY_PAYMENT_MODE=live`） | 微信：`IMBOY_WECHAT_MCH_ID` / `IMBOY_WECHAT_APP_ID` / `IMBOY_WECHAT_API_V3_KEY` / `IMBOY_WECHAT_CERT_SERIAL` / `IMBOY_WECHAT_PRIVATE_KEY` / `IMBOY_WECHAT_PLATFORM_PUBLIC_KEY`（`IMBOY_WECHAT_NOTIFY_URL` 留空由 install.sh 派生）；支付宝：`IMBOY_ALIPAY_APP_ID` / `IMBOY_ALIPAY_PRIVATE_KEY` / `IMBOY_ALIPAY_PUBLIC_KEY` / `IMBOY_ALIPAY_PID`（证书模式另需 `IMBOY_ALIPAY_APP_CERT_SN` / `IMBOY_ALIPAY_ROOT_CERT_SN`，可选 `IMBOY_ALIPAY_AES_KEY`，`IMBOY_ALIPAY_NOTIFY_URL` 可派生）；Stripe：`IMBOY_STRIPE_SECRET_KEY` / `IMBOY_STRIPE_WEBHOOK_SECRET`。**至少一个网关凭据完整。** |
| Garage 附件存储（文件上传必需） | `IMBOY_GARAGE_ENDPOINT`、`IMBOY_GARAGE_BUCKET`、`IMBOY_GARAGE_ACCESS_KEY`、`IMBOY_GARAGE_SECRET_KEY`；社区版 compose 内置 garage 时 endpoint 默认 `http://garage:3900` |

## deploy/.env.example 变量全表

### 必填（部署前必须修改）

| 变量 | 默认值 | 说明 |
|------|--------|------|
| `API_DOMAIN` | `api.example.com` | 后端 API + WebSocket 域名 |
| `ADMIN_DOMAIN` | `admin.example.com` | 管理后台域名 |
| `CS_WIDGET_DOMAIN` | `cs.example.com` | 客服 Widget 域名（fail-closed 三域之一，须与前两者两两不同） |
| `IMBOY_BASE_URL` | `https://cs.example.com` | 附件 presign 公网基址，必须恰为 `https://<CS_WIDGET_DOMAIN>`（preflight 精确比对） |
| `RTC_DOMAIN` | `rtc.example.com` | LiveKit RTC 信令域 |
| `TURN_DOMAIN` | `turn.example.com` | LiveKit embedded TURN 域（TLS 固定 5349） |
| `CERTBOT_EMAIL` | `ops@example.com` | Let's Encrypt 通知邮箱 |
| `POSTGRES_PASSWORD` | `CHANGE_ME_...` | PG 密码（不得为弱默认值，见上表 fail-fast） |
| `IMBOY_PASSWORD_SALT` | `CHANGE_ME_...` | 密码哈希盐（见上表） |
| `IMBOY_GARAGE_ACCESS_KEY` | `GK_CHANGE_ME_...` | Garage access key |
| `IMBOY_GARAGE_SECRET_KEY` | `CHANGE_ME_...` | Garage secret key |
| `GARAGE_RPC_SECRET` | `CHANGE_ME_...` | Garage 集群 RPC 密钥（**产生数据后不可更换**） |
| `LIVEKIT_API_KEY` | `CHANGE_ME_...` | LiveKit API key |
| `LIVEKIT_API_SECRET` | `CHANGE_ME_...` | LiveKit API secret（≥32 字符） |
| `GRAFANA_ADMIN_PASSWORD` | `CHANGE_ME_...` | Grafana 管理口令 |

### 安装器自动生成（勿手工复用旧值）

| 变量 | 说明 |
|------|------|
| `JWT_KEY` | 核心 JWT 密钥（32 字节） |
| `POSTGRE_AES_KEY` | PG 列加密 AES 密钥 |
| `ADM_COOKIE_SECRET` | 管理后台 Cookie 签名密钥 |
| `IMBOY_SOLIDIFIED_KEY` / `IMBOY_SOLIDIFIED_KEY_IV` | 客户端 init 握手密钥（32B / 16B） |
| `RSA 登录密钥对` | install.sh 生成至 `data/backend_priv/keys/login_rsa_{pub,priv}.pem` |

### 业务开关与版次

| 变量 | 默认值 | 说明 |
|------|--------|------|
| `IMBOY_VERSION` | `1.0.0-alpha.71` | 镜像版本。真源是仓根 `VERSION` 文件；此默认值抄自 `deploy/.env.example`，示例可能滞后（仓根现值更高），部署时以 `VERSION` 为准 |
| `IMBOY_EDITION` | `community` | `community` \| `professional` \| `enterprise` |
| `IMBOY_PRODUCT_PROFILE` | `community` | 销售版产品策略档位 |
| `IMBOY_PRODUCT_EXPERIENCE` | `chat` | 产品体验开关：`chat` \| `workspace`（改后需受控重启） |
| `IMBOY_E2EE_MODE` | `required` | E2EE 模式 |
| `IMBOY_FEATURE_E2EE` / `IMBOY_FEATURE_CHANNEL` / `IMBOY_FEATURE_CHANNEL_ORDER` | `true` | 内置功能开关 |
| `IMBOY_SALES_RELEASE` | `true` | 销售发布检查清单开关 |
| `IMBOY_PAYMENT_GATEWAY_ENABLED` | `true` | 外部支付网关总开关（false 时站内钱包不受影响） |
| `IMBOY_PAYMENT_MODE` | `live` | `live` \| `sandbox`（仅本地联调；strict 环境 sandbox 拒启） |
| `IMBOY_PLUGIN_LIFECYCLE_ENABLED` | `false` | 动态插件写操作总开关（子系统 FROZEN，默认关闭） |
| `IMBOY_API_AUTH_SWITCH` | `on` | API 认证开关（生产必须 on） |
| `IMBOY_TRUSTED_PROXY_IPS` | 空（默认 `127.0.0.1,::1`） | 受信反代白名单，逗号分隔；前面还有 LB/ingress 时必配 |

### 可选 / 按需

| 变量 | 默认值 | 说明 |
|------|--------|------|
| `TZ` | `Asia/Shanghai` | 时区 |
| `DATA_DIR` | `./data` | 数据持久化根目录 |
| `POSTGRES_USER` / `POSTGRES_DB` | `imboy_user` / `imboy_pro` | PG 账号与库名 |
| `PG_PORT` / `PG_BIND_ADDR` | `5432` / `127.0.0.1` | PG 宿主机暴露（默认仅本机） |
| `PG_MEM_LIMIT` / `PG_IMAGE` | `4096M` / GHCR 随版本 | PG 容器资源与镜像覆盖 |
| `BACKEND_PORT` / `BACKEND_BIND_ADDR` | `9800` / `127.0.0.1` | 后端端口与绑定 |
| `BACKEND_MEM_LIMIT` / `BACKEND_IMAGE` | `8192M` / GHCR 随版本 | 后端容器资源与镜像覆盖 |
| `ADMIN_IMAGE` / `IMBOY_WIDGET_IMAGE` | GHCR 随版本 | admin / widget 镜像覆盖 |
| `IMBOY_GARAGE_PUBLIC_ENDPOINT` | 未设置（回落 endpoint） | presign 公网基址（独立 s3 域名或外部存储时设置） |
| `IMBOY_JVERIFICATION_RSA_PRIV_KEY_FILE` | 复用登录 RSA 私钥 | 极光认证私钥 |
| `LIVEKIT_RECORDING_BUCKET` | 未设置 | 录制输出桶（egress 录制当前不可用） |
| `IMBOY_LIVEKIT_WS_URL` | `wss://<RTC_DOMAIN>` | 客户端信令地址覆盖 |
| `LIVEKIT_TURN_ENABLED` | `false` | embedded TURN 开关（前置条件见 .env.example 注释） |
| `LIVEKIT_TURN_CERT_DIR` | 安装器自动展开 | TURN TLS 证书目录（宿主机 certbot 形态才需设置） |
| `IMBOY_SMTP_RELAY` / `PORT` / `SSL` / `USERNAME` / `PASSWORD` / `FROM` | 空 / `465` / `true` | SMTP 邮件（填任意一项即要求整组完整；FROM 空时回退 USERNAME） |
| `IMBOY_SMS_SWITCH` / `IMBOY_SMS_PLATFORM` | `off` / `yjsms` | 短信开关与平台（`yjsms` \| `jsms`；aliyun 无实现会被 preflight 拒绝） |
| `SENTRY_DSN` | 空 | Sentry 监控 |
| `GRAFANA_PORT` / `GRAFANA_BIND_ADDR` | `3000` / `127.0.0.1` | Grafana 端口与绑定 |
| `UPTRACE_ENABLED` | `false` | Uptrace overlay 总开关 |
| `UPTRACE_DOMAIN` / `UPTRACE_ADMIN_EMAIL` | - | Uptrace 域名与管理员邮箱（ENABLED=true 时必填） |
| `UPTRACE_IMAGE` 等 12 项 `UPTRACE_*` | 模板值 | Uptrace overlay 镜像与凭据（安装器生成密钥） |
| `IMBOY_LICENSE_FILE` | 空 | License 文件路径（professional/enterprise 必配） |
| `PG_EXPORTER_USER` / `PG_EXPORTER_PASSWORD` | 回落 POSTGRES_* | postgres_exporter 只读账号 |
| `IMBOY_TSID_STATE_DIR` | `/opt/imboy/tsid_state` | TSID durable fence 状态目录 |
| `IMBOY_TSID_DC_ID` / `IMBOY_TSID_NODE_ID` / `IMBOY_TSID_DC_BITS` | `1` / `1` / `3` | 数据中心/节点标识（同 DC 内 node_id 唯一，重复=ID 碰撞） |
| `IMBOY_TSID_STORE_BOOTSTRAP` | `existing` | store bootstrap 合同（`fresh` 仅首次部署，割接后必须改回） |
| `IMBOY_TSID_BOOTSTRAP_MODE` | `auto_scan` | 首启自举模式（`auto_scan` \| `manual_floor`） |
| `IMBOY_TSID_BOOTSTRAP_FLOOR_UNIX_MS` | 空 | manual_floor 模式必填（unix 毫秒） |
| `IMBOY_TSID_BOOTSTRAP_LEGACY_ACK` | 注释态（不注入） | 旧 writer 接管确认，唯一合法值 `I-CONFIRM-OLD-WRITER-STOPPED`；割接后必须移除 |
| `IMBOY_TSID_MAX_LOGICAL_LEAD_MS` 等 5 项时序参数 | `512`/`100`/`1000`/`100`/`5000` | TSID 时序参数（默认已定标，一般无需调整） |

<!-- AUTO-GENERATED:conditional-required END -->

<!-- AUTO-GENERATED:runtime-override BEGIN（提取自 src/lib/imboy_env.erl 头注释与 Makefile LLM 注入段） -->

## 运行时 IMBOY_* 覆盖键（imboy_env:override_from_env/0）

以下环境变量在启动时覆盖 sys.config 同名键（完整映射见 `src/lib/imboy_env.erl` 头注释）：

| 分组 | 变量 → sys.config 键 |
|------|----------------------|
| 核心密钥 | `IMBOY_JWT_KEY`→`jwt_key`；`IMBOY_POSTGRE_AES_KEY`→`postgre_aes_key`；`IMBOY_POSTGRE_AES_KEY_OLD`→`postgre_aes_key_old`（Bot 轮换只读窗）；`IMBOY_ADM_COOKIE_SECRET`→`adm_cookie_secret` |
| URL | `IMBOY_BASE_URL`→`base_url`；`IMBOY_WS_URL`→`ws_url` |
| 数据库 | `IMBOY_PG_HOST` / `IMBOY_PG_PORT` / `IMBOY_PG_PASSWORD` → `pg_conf`；`IMBOY_PG_DATABASE`（别名 `IMBOY_PG_DB`）/ `IMBOY_PG_USERNAME`（别名 `IMBOY_PG_USER`） |
| SMTP | `IMBOY_SMTP_RELAY` / `PORT` / `SSL` / `USERNAME` / `PASSWORD` / `FROM` → `smtp_option` |
| 推送/短信 | `IMBOY_JPUSH_APP_KEY` / `IMBOY_JPUSH_MASTER_SECRET`；`IMBOY_YJSMS_ACCOUNT` / `SECRET` / `URL`；`IMBOY_SMS_SWITCH` / `IMBOY_SMS_PLATFORM`；`IMBOY_JSMS_TEMP_ID` / `IMBOY_JSMS_SIGN_ID` |
| Garage | `IMBOY_GARAGE_ENDPOINT` / `IMBOY_GARAGE_PUBLIC_ENDPOINT`（未设置时回落 endpoint）/ `IMBOY_GARAGE_BUCKET` / `IMBOY_GARAGE_ACCESS_KEY` / `IMBOY_GARAGE_SECRET_KEY` |
| 支付 | `IMBOY_WECHAT_MCH_ID` / `APP_ID` / `API_V3_KEY` / `CERT_SERIAL` / `PRIVATE_KEY` / `PLATFORM_PUBLIC_KEY`；`IMBOY_ALIPAY_APP_ID` / `PRIVATE_KEY` / `PUBLIC_KEY`；`IMBOY_STRIPE_SECRET_KEY` / `WEBHOOK_SECRET`；`IMBOY_PAYMENT_MODE` |
| 功能开关 | `IMBOY_AUTO_MIGRATE`→`auto_migrate`；`IMBOY_API_AUTH_SWITCH`；`IMBOY_PRODUCT_PROFILE`；`IMBOY_PRODUCT_EXPERIENCE`；`IMBOY_E2EE_MODE`；`IMBOY_FEATURE_E2EE` / `IMBOY_FEATURE_CHANNEL` / `IMBOY_FEATURE_CHANNEL_ORDER` |
| 插件 | `IMBOY_PLUGIN_ROOT`（SEC-02 受控插件根）；`IMBOY_PLUGIN_TRUSTED_PUBLIC_KEY_FILES`（逗号分隔公钥文件） |
| 遥测 | `IMBOY_UPTRACE_DSN`（含 token 属敏感，只走 env；未设置则遥测关闭）；`IMBOY_UPTRACE_OTLP_ENDPOINT`（默认 `http://127.0.0.1:14318`） |
| CS Widget / CORS | `IMBOY_CS_WIDGET_*`、`IMBOY_CORS_*_ORIGINS` 各键由 `imboy_env_overrides` 装载（映射见该模块头注释） |

## 本地开发（IMBOYENV=local）

| 变量 | 说明 |
|------|------|
| `BIGMODEL_API_KEY` / `ARK_API_KEY` / `BAILIAN_API_KEY` | LLM 密钥；`IMBOYENV=local make run` 会从 `.env.local`（优先）或 `.env` 读取并导出到节点进程环境（Makefile LLM 注入段）；可选 `BIGMODEL_MODEL` / `ARK_MODEL` / `BAILIAN_MODEL` 覆盖默认模型 |
| `IMBOY_PG_HOST` / `IMBOY_PG_PORT` 等 | 本地 PG 指向（sys.local.config 的 `pg_conf` 已含本地默认，env 优先） |
| `IMBOYENV` | `local` / `dev` / `test` / `pro`；仅 `dev` / `local` / `test` 跳过生产 fail-fast 校验 |

<!-- AUTO-GENERATED:runtime-override END -->

## 相关文档

- Make 命令：[make-commands.md](./make-commands.md)
- 配置工程笔记（现状评审）：[engineering/configuration-notes.md](./engineering/configuration-notes.md)
- 部署流程与运维变量（ops.env.example）：`deploy/README.md`（不在本文范围）
