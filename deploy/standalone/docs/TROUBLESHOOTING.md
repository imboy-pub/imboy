# 社区版安装故障排查 / Troubleshooting

先定位失败阶段，再做对应检查。保留原 `.env`、RSA、数据库、Garage 和证书目录。以下命令针对本包标准社区路径；如果已启用 TURN，所有 Compose 操作还要追加 `-f docker-compose.livekit-turn.yml`，保持与当前运行栈一致。

## 1. 先收集最少必要信息

从包根目录检查材料；从 `deploy/` 查看运行状态：

```bash
cd /opt/imboy-community
bash scripts/check_delivery.sh
cd deploy
docker version
docker compose version
docker compose --env-file .env -f docker-compose.community.yml config -q
docker compose --env-file .env -f docker-compose.community.yml ps -a
docker compose --env-file .env -f docker-compose.community.yml logs --tail=200 imboy_backend
```

记录命令、退出码、时间、安装阶段、第一条关键错误、相关服务，以及是否已经产生数据。日志保存在客户受控目录；外发前去除密码、token、cookie、真实账号和带签名的下载 URL。不要提供完整 `.env`、展开后的 `compose config` 或未过滤的容器环境变量。

## 2. 启动前失败

| 症状 | 核对原因 | 处理及复验 |
|---|---|---|
| `缺少 ../scripts/sanity_check.sh` | 只复制了 `deploy/` 或解压层级错误 | 完整解压 ZIP，保留 `deploy/` 与 `scripts/` 同级；运行包完整性检查 |
| 校验和 / 文件权限 / 必需文件检查失败 | 不完整下载、跨平台复制丢权限、静态文件被修改 | 从原 ZIP 在 Linux 文件系统重新解压；保留已有 `.env` / 数据，不重算清单掩盖差异 |
| 并发锁被占用 | 另一个安装进程仍在运行 | 确认操作者和进程状态；等当前操作结束 |
| 存在数据但配置 / 密钥不完整，入口拒绝继续 | 误删 `.env`、换目录、遗失 RSA 或凭据 | 从对应备份恢复；不复制新模板，不随机更换存储凭据 |
| 缺 `jq`、`flock`、OpenSSL 等 | 宿主工具未准备 | 按 INSTALL 第 5 节补齐，重做离线检查 |
| Docker daemon 不可用 | Docker 未启动或账号无访问权限 | 执行 `docker info`，由管理员启动服务 / 授权 |
| Compose 不认识 `configs.content` | Compose 版本低于 2.23.1 | 升级 Compose v2 插件，再执行 `config -q` |
| 五域、邮箱、origin 未填或冲突 | 仍是占位值，域名重复，origin 有路径 | 填写纯域名与精确 `https://<API_DOMAIN>`；五域两两不同 |
| 首次手工 preflight 报内部密钥 / RSA 缺失 | 尚未经过安装器初始化 | 完成客户字段准备，再在获准服务器执行包根安装入口；preflight 不生成密钥 |
| Compose 密钥不独立或服务不一致 | 继承环境覆盖 `.env`，复制了旧字段 | 核对配置来源与字段，不输出密钥；账号密码须与数据库实际状态一致 |
| 绝对 DATA_DIR 或自定义 BACKEND_PORT 被拒绝 | 超出本包固定路径 / 端口范围 | 保留 `DATA_DIR=./data` 与 9800；已有实例换路径须另做迁移方案 |
| Uptrace / enterprise-direct 配置被拒绝 | 超出本包标准安装路径 | 保持默认关闭；如需启用，另做配置与现场验收后扩展交付 |

## 3. 镜像失败

| 症状 | 处理 |
|---|---|
| `manifest unknown` / 标签不存在 | 核对发布清单、`IMBOY_VERSION` 和 image override；不自行换 `latest` |
| `unauthorized` / `denied` | 确认客户有对应 GHCR 包读取权限，使用获授权账号交互 `docker login ghcr.io` |
| 网络超时或证书错误 | 核对出站网络、DNS、系统 CA；不关闭 TLS 校验 |
| 不支持当前平台 | 本包要求 Linux x86_64；LiveKit 使用 amd64 digest，不能直接宣称支持 ARM |
| edge 镜像文件检查失败 | backend 必须包含下载 edge 入口、ERTS 和 worker；由交付方修正镜像，不能删掉检查 |
| 实际镜像内容与预期不明 | 记录运行镜像 RepoDigest / image ID 并对照发布身份；源码 SHA 不能替代镜像证据 |

安装器先拉齐缺失镜像再启动服务，仍应根据实际日志确认失败阶段。镜像探针容器可能为诊断而保留，不代表业务服务已成功部署。

## 4. 服务启动后失败

| 症状 | 优先检查 | 恢复原则 |
|---|---|---|
| backend 反复重启 / `/healthz` 失败 | backend 日志、PG、迁移状态、必需配置、RSA | 修复明确错误；端口已监听不等于健康 |
| RSA 文件不存在 / permission denied | `data/backend_priv/keys/`、权限、backend 实际挂载与运行用户 | 使用本包对应挂载，从原备份恢复已有私钥，不重建已有登录密钥 |
| PG 密码认证失败 | `.env` 与已有数据库账号实际密码 | 改 `.env` 不会自动修改 PG 账号；由 DBA 按密码轮换流程处理 |
| 清理 worker 报账号 / 权限错误 | LOGIN、`v5_worker` 成员资格、TCP 密码认证 | 按 DB_CLEANUP_ACCOUNT.md 开通 / 核对；生成密码不等于已创建数据库账号 |
| 迁移表有记录但仍异常 | 单行 head、dirty、history 与镜像文件集合 | 按现场命令逐项比对，不以 count 大于 0 记为全部成功 |
| Nginx 启动 / `nginx -t` 失败 | 证书、模板、必需 env、上游服务名 | 使用包内白名单模板，不把宿主机 stream 模板放进 HTTP `conf.d` |
| API 正常但 Widget 失败 | `imboy_widget`、`/v1/loader.js`、CS 域反代 | 原 sanity 没覆盖 Widget 完整业务，须单独验收 |
| API 正常但下载 / 头像失败 | edge、Garage 桶和授权、registry、实际请求路径 | 区分私有文件附件和公开资源，重用现有 Garage key |
| LiveKit 运行但无法通话 | RTC 域 TLS / WSS、客户端返回地址、媒体端口和 NAT | 本包补了 RTC 反代，仍须真实两端媒体验证 |
| 监控无数据 / 无告警通知 | profile、采集目标、接收器、出口网络 | 监控运行不等于采集 / 通知成功，逐项记录 |

## 5. TLS 签发与续期

首次签发依赖五域 DNS 与 HTTP-01 条件。证书失败后可能已有部分正式证书和部分临时自签证书，服务也可能已经启动。

```bash
cd /opt/imboy-community/deploy
docker compose --env-file .env -f docker-compose.community.yml logs --tail=200 imboy_nginx
docker compose --env-file .env -f docker-compose.community.yml logs --tail=200 imboy_certbot
```

| 症状 | 处理 |
|---|---|
| ACME 验证失败 | 核对全部五域 A / AAAA、80、防火墙、代理和 challenge 路径；未修复前不要连续申请 |
| 浏览器收到自签证书 | 检查首签结果、`.imboy-selfsigned` 标记与证书文件；修复原因后沿原目录重试 |
| archive / renewal 存在但 live 不完整 | 恢复 certbot 完整目录和链接，不只复制证书文件、不删除 archive |
| sanity 通过但外网证书不受信任 | 原脚本内部探测关闭证书校验；按现场命令验证正式信任链和主机名 |
| RTC 已签证但 WSS 不通 | 检查实际 RTC vhost、后端下发地址与 LiveKit 7880；签证与反代分别验证 |
| TURN 容器读不到证书 | 核对 `LIVEKIT_TURN_CERT_DIR`、同域 archive 挂载、容器内证书路径与 `../../archive` 链接目标 |
| TURN 运行后重跑 preflight 报端口占用 | 用 `ss -lntup` 确认是否为当前实例自身，不据此删除服务或数据 |
| certbot 在运行但新证书未加载 | 分别验证续期、Nginx reload 与 TURN 证书加载；容器存在不是续期证据 |

正式实例不传 `--skip-tls`，外部探测不加 `curl -k`。TURN 默认使用 TCP 5349，TURN 域没有 443 vhost 不应被误报为本包 HTTPS 配置失败。

## 6. 基础设施正常但业务不通

- **登录失败**：核对正式开户流程、客户端版本、服务器地址、密码盐和 RSA。缺短信 / SMTP 条件时记录 `BLOCKED`，不启用开发验证码。
- **WS 握手成功但消息未达**：记录两端账号代号、设备和时间，实际检查发送、接收、回执及断线恢复；101 不是消息投递证据。
- **附件上传后打不开**：核对 private / public 桶、现有 key 权限、URL origin、edge 和 S3 反代；证据不外泄签名 URL。
- **升级后旧数据异常**：先比对密码盐、RSA、Garage 凭据、数据路径和迁移，按升级恢复手册处理；不单独降镜像或执行通用删除命令。

修复后复验受影响步骤及其直接业务路径。缺服务器、DNS、镜像权限、DBA、客户端或账号时记录 `BLOCKED`，未执行项保留 `NOT_RUN`。分别描述静态配置检查和实际执行结果。

依据：包内 `deploy/README.md`、`deploy/install.sh`、`deploy/preflight.sh`、`scripts/sanity_check.sh`，以及 `SOURCE_COMMIT` 对应的配置。仓库参考入口为 [部署说明](https://github.com/imboy-pub/imboy/blob/main/deploy/README.md)。
