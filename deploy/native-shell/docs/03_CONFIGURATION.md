# 配置、环境变量与目录合同

## 1. 三类配置分开管理

| 类型 | 文件 | 谁填写 / 生成 | 能否进入通用交付包 |
| --- | --- | --- | --- |
| 现场非秘密参数 | 包目录的 `site.env` | 环境负责人从 `templates/site.env.example` 填写 | 模板可以；真实客户拓扑应按现场信息分级处理 |
| 固定发行信息 | `metadata/release.env` 与 `CHECKSUMS.sha256` | 发布方组装工具生成 | 可以；不得由客户手改以跳过兼容检查 |
| 运行秘密与应用配置 | `/etc/imboy/` 下生成文件 | 安装器在现场生成；外部服务秘密由现场注入 | 不可以；属于安装身份与恢复材料 |

`site.env` 是受限制的 `KEY=value` 数据文件，不是任意 Shell 程序。按模板填写，不加 `export`、命令替换、Shell 函数、表达式或额外未支持的键。安装器解析数据，不应执行客户文件中的代码。也不要手工 `source site.env` 作为排障捷径。

## 2. 现场参数表

| 参数 | 含义 | 约束 / 选择依据 |
| --- | --- | --- |
| `SITE_NAME` | 本次部署标识 | 使用模板允许的简单标识；不含个人秘密 |
| `API_DOMAIN` | 后端 HTTPS/WSS 域名 | 真实 FQDN，证书覆盖；不附协议、端口、路径 |
| `S3_DOMAIN` | S3 公网签名入口 | 独立 FQDN；生成 `https://s3.example.com` 这类 URL |
| `FILES_DOMAIN` | 公开桶读取域名 | 与 S3 签名入口分开；明确公开内容范围 |
| `TLS_DIR` | 三域名 PEM 来源目录 | 绝对路径；其下为 `api/`、`s3/`、`files/` |
| `GARAGE_CAPACITY` | Garage 布局声明的可用存储容量 | 不大于实际可用于附件的空间；预留数据库、日志和备份余量 |
| `CORS_ORIGINS` | 允许的 Web 来源 | 逗号分隔的 `https://真实域名`，当前仅支持默认 443、无显式端口/路径/通配符；无 Web 范围可留空 |

首次支持面固定端口与路径。已有端口占用、共享盘布局或外部数据库不能靠添加未声明变量解决，应作为不同部署拓扑由发布方提供相应候选。

## 3. 后端实际读取的变量

以下为固定源码确认的映射，供发布方和运维定位问题。用户通常只填 `site.env`；运行文件由安装器生成。完整示例见 `templates/backend.env.example`，不要把示例中的秘密占位符直接用于运行。

| 运行变量 / 配置项 | 本包目标 | 说明 |
| --- | --- | --- |
| `IMBOYENV` | `pro` | 启用生产环境启动校验 |
| `HTTP_PORT` | `9800` | 实际 HTTP 监听端口变量；`IMBOY_HTTP_PORT` 不是当前监听器读到的变量 |
| `sys.config` 的 `listen_ip` | `{127,0,0,1}` | 当前没有对应环境变量覆盖；由配置生成器写入 |
| `IMBOY_BASE_URL` | `https://API_DOMAIN` | 本包后端公开基址；客服特殊 URL 合同另行核对 |
| `IMBOY_WS_URL` | `wss://API_DOMAIN/api/v1/ws` | 固定源码真实 WebSocket 路由 |
| `IMBOY_PG_HOST` / `PORT` | `127.0.0.1` / `5432` | 本机专用数据库 |
| `IMBOY_PG_USER` / `DB` / `PASSWORD` | 本包专用业务身份与现场密码 | 源码还接受 USERNAME / DATABASE 别名，交付统一一种写法 |
| `IMBOY_AUTO_MIGRATE` | 按安装/升级流程设置 | 迁移失败或 dirty 状态导致应用启动失败，不应忽略 |
| `IMBOY_PRODUCT_PROFILE` | `phase1_delivery` | 必须与发行物编译的清单一致 |
| `IMBOY_PRODUCT_EXPERIENCE` | 见生成配置 | 不能单凭切换体验值承诺完整工作台/组织业务交付 |
| `IMBOY_GARAGE_ENDPOINT` | `http://127.0.0.1:3900` | 服务端访问对象存储的内部地址 |
| `IMBOY_GARAGE_PUBLIC_ENDPOINT` | `https://S3_DOMAIN` | 客户端预签名地址；不加 `/s3` |
| `IMBOY_GARAGE_BUCKET` | 私有桶 | 桶名由生成配置固定 |
| `IMBOY_GARAGE_PUBLIC_BUCKET` | 独立公开桶 | 只有明确公开内容使用 |
| `IMBOY_GARAGE_ACCESS_KEY` / `SECRET_KEY` | 现场生成 | 不提供给客户端或写入验收报告 |
| `garage.region` | `garage` | 当前在 `sys.config` 中设置，不能假设存在任意 IMBOY_* 覆盖 |
| `garage.public_base_url` | `https://FILES_DOMAIN` | 当前在 `sys.config` 中设置 |
| `IMBOY_ATTACHMENT_REGISTRY_FROM_GARAGE` | `true` | 按 Garage 参数生成存储注册表；缺失有效注册表会触发严格启动检查 |
| 登录 RSA 文件变量 | `/etc/imboy/keys/` 下文件 | 权限必须允许服务账号读取，禁止在 release 中夹带旧私钥 |

数据加密、JWT、密码盐、管理员 Cookie、TSID 等参数也属于运行合同。秘密一经使用不得通过“重新初始化”更换，否则可能导致历史密文、会话或签名失效。需要轮换时应使用独立的、经验证的轮换流程。

直接证据：[环境覆盖](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/src/lib/imboy_env.erl)、[额外环境覆盖](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/src/lib/imboy_env_overrides.erl)、[监听与启动迁移](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/src/imboy_app.erl)、[真实路由](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/src/imboy_router.erl)。

## 4. 安装后的固定目录

| 路径 | 内容 | 备份 / 修改规则 |
| --- | --- | --- |
| `/opt/imboy/releases/<RELEASE_ID>/backend` | 每个后端发行物 | 不在线改代码；保留旧发行物与哈希 |
| `/opt/imboy/current` | 当前发行物指针 | 由安装/升级入口维护，不能靠手改链接做数据库回滚 |
| `/opt/imboy/runtime/postgresql` | PG18 二进制和扩展 | 应与恢复时的数据库版本一致 |
| `/opt/imboy/runtime/garage` | Garage 二进制 | 保留版本记录；不能随意替换大版本 |
| `/etc/imboy/site.env` | 已接受的现场参数 | 修改需走受控流程，确保生成配置同步 |
| `/etc/imboy/backend.env`、`sys.config`、`vm.args` | 应用运行配置 | 含敏感值，限制读取；备份中一并保护 |
| `/etc/imboy/secrets.env`、`keys/` | 安装身份与秘密 | 必须备份，禁止公共分发 |
| `/etc/imboy/tls/` | 服务使用的 TLS 材料 | 由证书负责人管理轮换、权限、到期验证 |
| `/var/lib/imboy/postgres` | 数据库集群 | 不手工清空；不能把在线普通文件复制当作一致快照 |
| `/var/lib/imboy/garage/` | Garage 元数据与对象数据 | 必须作为同一组恢复；不能只备份其中一边 |
| `/var/lib/imboy/backend` | 后端本地持久状态 | 与数据库/密钥联合备份；不可当缓存随便删除 |
| `/var/backups/imboy` | 本机备份归档 | 非异机备份；制定离机复制、加密、保留及恢复抽查 |
| `/var/lib/imboy-install` | 安装/恢复身份、状态与互斥锁 | 不通过删除状态绕过续跑或并发保护 |

路径、权限和所属账号以安装器最终生成结果为准。`root` 能读取服务器秘密属于运维信任边界；不要通过给所有用户开放目录权限来解决启动问题。

## 5. 配置变更原则

先记录候选版本、现有配置摘要、变化原因与受影响业务，再在演练环境验证。URL、域名、证书、CORS、桶、数据库连接、加密材料不是可以随意互换的字符串。修改后重新进行对应验证，至少检查生成配置语法、服务 readiness 和真实附件操作。

`site.env` 修改不意味着已运行的服务自动接收新值；实际生效需重新生成对应配置并受控重启/reload。当前安装器面向首次部署与同一状态续跑，不将任意配置变更视为已支持的升级方式。

渲染器冻结了已核对上游配置的摘要。安装/升级时，release 的无秘密配置模板、元数据 `CONFIG_SHA256` 与渲染器允许的摘要必须一致。上游配置变化时，由发布方适配渲染器并重新验收；不能只换版本号或手改哈希让门禁通过。

## 6. 企业 IdP 与其他外部集成

如合同要求企业免登/SSO，至少记录 IdP 类型、issuer/discovery 地址、客户端标识、精确回调 URL、租户/组织映射、授权 scopes、退出策略、测试账户和停用用户行为。client secret、证书私钥与用户 Token 通过独立安全渠道提供。

本包没有替客户自动创建 IdP 应用，也没有声称已完成特定 IdP 接入。SMTP/短信/推送同理：缺少供应商配置时应将相关登录/通知验收标为 `BLOCKED`，不能用健康检查通过代替。实际启用这些集成前核对对应代码与配置合同，不在 `site.env` 中随意增加变量。
