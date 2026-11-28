# 本地开发 / Local development

本地开发与评估统一使用仓库根目录 `docker-compose.yml`，通过 profiles 组合服务。
社区版与企业版共用编排；生产安装、证书和升级仍按 [deploy/README.md](../../../../deploy/README.md)。

Local development uses one Compose file at the repository root. Community and
enterprise modes share this stack. Production uses the separate deployment guide.

## 首次启动

需要 Docker 24+、Compose v2.23.1+、Bash 与 OpenSSL，并启动 Docker daemon。
容器化运行无需宿主机 Erlang / Bun；首次源码构建和镜像下载需要网络，耗时随机器与网络变化。
“约 5 分钟”是镜像准备就绪后的快速启动目标，不是首次编译耗时保证。

以下命令均在仓库根目录执行：

```bash
test -f .env || cp .env.example .env
chmod 600 .env
${EDITOR:-vi} .env
bash scripts/dev_setup.sh
```

填写 `IMBOY_PG_PASSWORD` 与 `IMBOY_PG_PRO_PASSWORD`；Compose 解析时两者都必需，
填写 pro 密码不会启动 pro 数据库。若未先复制模板，脚本会创建 `.env` 并退出，填写后重跑。
脚本只补齐缺失的 Garage / edge / LiveKit 内部密钥，保留已有非空值。已有数据库必须沿用实际密码；
修改 `.env` 不会修改 PostgreSQL 数据卷里已创建账号的密码。根目录 `.env` 供本地使用，
与生产 `deploy/.env` 分开维护。

默认启动 PostgreSQL、Garage、API、Admin 和 Nginx，含附件下载边缘服务。
不默认启动 LiveKit 或 Widget；本地栈不包含 Prometheus、Uptrace 等监控组件。
默认构建当前后端检出与编排中固定提交的 Admin / Widget 源码；可通过
`IMBOY_FRONTEND_CONTEXT=../imboy-admin-frontend` 使用本地前端检出。该目录须包含可用的
`.git` 元数据，Widget 构建会写入并校验真实提交号。已有镜像时在 `.env` 设置
`BACKEND_IMAGE` / `ADMIN_IMAGE` / `WIDGET_IMAGE`，然后运行
`bash scripts/dev_setup.sh --no-build`（使用 Widget 时加 `--with-widget`）。镜像 tag 必须
实际存在，不能仅从 `VERSION` 推断镜像已发布。

| 入口 | 默认地址 | 用途 |
|---|---|---|
| API | `http://api.localhost:8080/api/v1/init` | 后端初始化接口 |
| Admin | `http://admin.localhost:8080` | 管理后台 |
| 公开文件 | `http://files.localhost:8080/<object-key>` | Garage 公开桶的匿名文件入口 |
| Widget（可选） | `http://cs.localhost:8080/v1/loader.js` | 客服静态入口与同源 API |
| RTC（可选） | `ws://rtc.localhost:8080` | LiveKit 信令入口 |

新数据库首次访问 [Admin 初始化向导](http://admin.localhost:8080/setup)，自行创建第一个
超级管理员。已有实例使用原账号登录；向导会拒绝重复初始化，不会重置已有账号。

浏览器通常将 `*.localhost` 解析到本机；若本机解析失败，为上表主机名添加
`127.0.0.1` hosts 记录。入口端口变更后，按 `.env.example` 同步公开 URL。
网关只接受配置中的域名及 HTTP 入口端口；未配置的 Host 返回 `421`。

## 按需启动

```bash
bash scripts/dev_setup.sh --with-livekit
bash scripts/dev_setup.sh --with-widget
bash scripts/dev_setup.sh --with-livekit --with-widget
bash scripts/dev_setup.sh --help
```

部署能力由编译清单、运行时功能策略和启用的服务决定，不按商业版次区分。
旧参数 `--with-garage` 仍接受，Garage 已默认启用，无需另加。

| Profile | 用途 |
|---|---|
| `dev` | PostgreSQL、Garage 与 Nginx |
| `api` | `imboy_backend` 与 `imboy_edge` |
| `admin` | 管理后台 |
| `livekit` | 单节点音视频服务 |
| `widget` | 客服 Widget 静态服务 |
| `pro` | 保留原有独立 PostgreSQL 服务及其数据挂载 |

直接用 Compose 时要启用依赖所需 profiles。例如默认栈使用
`docker compose --profile dev --profile gateway --profile api --profile admin`。
单独启用 `pro` 不会替默认 API 切换数据库。`bash scripts/dev_setup.sh --profile pro`
会在默认服务之外启动 pro PG，并在缺失时创建其原有外部网络 `imboy-network`。
直接 Compose 启动 pro 时须先准备该网络；两库可以同时运行，容器名、端口和数据挂载独立。

### LiveKit 与 Widget

LiveKit 默认 `LIVEKIT_NODE_IP=127.0.0.1`，媒体绑定本机回环，仅供同一电脑联调。
媒体端口为 TCP `7881` 与 UDP `50000–50200`；信令经上表 RTC 入口。手机联调时，
`localhost` 指向手机自身，须将 `LIVEKIT_NODE_IP` 设置为电脑可达的 LAN 地址、调整
`IMBOY_BIND_ADDR`，并为 API/Admin/Widget/RTC/文件入口配置手机可解析的独立域名与对应 URL。
Docker Desktop、宿主防火墙及 LAN/NAT 也须允许媒体端口。信令通不能证明媒体通；生产
HTTPS/WSS、TURN 与真实双端验收见 [LiveKit 运行手册](./livekit-single-service-runbook.md)。

Widget 还需要在后台配置 installation 与允许嵌入的来源。loader 能访问只证明静态面
已就绪；访客会话、SSE 与坐席收发按 [Widget 运行手册](./customer-service-widget.md) 验证。

## 宿主机源码调试

宿主机准备 Erlang/OTP 28+ 与 GNU Make；开发工具镜像固定的 OTP 版本见
[`docker/backend_dev_Dockerfile`](../../../../docker/backend_dev_Dockerfile)。准备基础设施后，停止可能仍在运行的容器后端，避免占用 `9800`：

```bash
bash scripts/dev_setup.sh --infra-only
docker compose --profile dev --profile api stop imboy_backend imboy_edge
${EDITOR:-vi} config/sys.local.config
IMBOYENV=local make rel
IMBOYENV=local bash scripts/start_node.sh imboy_local
```

`--infra-only` 启动 PG、Garage 与 Nginx，并将 API 上游改为
`host.docker.internal:9800`。缺少本地配置时从 `config/sys.config.example` 创建
`config/sys.local.config`；已有文件保留。核对主库与迁移连接、Garage 和公开 URL。
默认 PG 为 `127.0.0.1:4323`，Garage 为 `http://127.0.0.1:3900`。

本地 Compose 显式设置 `IMBOY_CLUSTER_NODES=[]`，让单节点缓存恢复就绪。
宿主单节点调试时，在 `.env` 中设置 `IMBOY_CLUSTER_NODES='[]'`（旧文件需补充）。
新变量使用 JSON 节点数组，显式值优先于旧的 `CLUSTER_NODES`；未设置或空字符串
保留原应用集群配置与旧变量回退行为，`[]` 才会清除模板中的示例对等节点。

`start_node.sh` 需要已构建的 release 与节点名，启动前依次加载 `.env` / `.env.local`，
后者同名变量优先；非空 `IMBOY_*` 环境变量覆盖应用配置。两个文件须为可信的 Bash
赋值语法，密码等含 `$` 或空格的字面值使用单引号，例如 `IMBOY_PG_PASSWORD='your value'`。
直接 `make run` 不会加载 `.env` 中全部数据库和存储配置，不能省略相应配置核对。

基础设施模式不启动 `imboy_edge`。需要验证受控附件下载、头像边缘路由等完整链路时使用
默认容器栈，或自行运行边缘服务并设置正确的 `EDGE_UPSTREAM`。修改代码后重新
`IMBOYENV=local make rel` 再启动；数据库迁移随应用现有启动机制执行。
请求 `http://127.0.0.1:9800/api/v1/init` 并检查业务响应与日志。定向测试可用
`make eunit-local t=<模块>`；测试涉及真实数据库，应使用测试专用库。

## 版次与可选文件

先运行默认社区栈。已取得有效许可证、验证公钥或业务密钥环时，使用
`docker compose --profile dev --profile api cp <本机文件> imboy_backend:/opt/imboy/priv_runtime/<文件名>`
把所需文件放入持久化 `backend_runtime` 卷；不要只在 `.env` 填宿主机路径。
使用容器内 `10001:10001` 所有者，企业密钥环必须为 `0600`。例如复制后执行：

```bash
docker compose --profile dev --profile api exec --user 0 imboy_backend \
  sh -c 'chown 10001:10001 /opt/imboy/priv_runtime/enterprise_keyring.json && chmod 600 /opt/imboy/priv_runtime/enterprise_keyring.json'
```

如启用了企业托管加密，请按 `.env.example` 设置 `IMBOY_EB_ENTERPRISE_KEYRING_FILE`，再重跑启动命令。
密钥环格式以 [eb_keyring_file.erl](../../../../src/features/enterprise_business/infrastructure/eb_keyring_file.erl)
为准；缺失、权限或内容无效会拒绝启动，运行标记本身不代表企业能力验收完成。

## 查看状态与停止

```bash
docker compose --profile dev --profile gateway --profile api --profile admin ps
docker compose --profile dev --profile gateway --profile api --profile admin logs --tail=100 imboy_backend
# 暂停服务，保留容器与持久化数据；按需加 livekit/widget profile。
docker compose --profile dev --profile gateway --profile api --profile admin stop
```

开发库继续使用命名卷 `imboy_pg18_data`。 `pro` 和生产 profiles 共用 `prod_imboy_pg18`，数据默认放在 `deploy/data/pg18`。接回旧库前，在 `.env` 显式设置 `IMBOY_PROD_PG_DATA_DIR=/data/docker/pro_imboy_pg18`、`PG_PORT=5181`，并核对 `PG_IMAGE` 和数据库账号。已有旧容器必须停止后才能让新容器挂载同一目录。

The `pro` and production profiles share `prod_imboy_pg18` and default to `deploy/data/pg18`. To reuse the old database, set `IMBOY_PROD_PG_DATA_DIR=/data/docker/pro_imboy_pg18` and `PG_PORT=5181` explicitly, verify the image and credentials, and stop the old container before mounting the same directory.

开发服务使用隔离网络
`imboy-local`，已有开发 PG 容器可能重建，但继续挂载原卷。不要为了重新启动而改卷名、
删除卷或使用 `docker compose down -v`。

新 Garage 数据使用 Compose 的 `garage_data` 卷；旧 `garage-local` 容器与
`/tmp/garage` 数据不会自动导入。若旧容器占用 `3900`，确认它就是旧本地开发实例后
执行 `docker stop garage-local`，保留容器和数据。需要旧附件时先备份，再显式迁移数据
与凭据并验收；不要删除旧目录或误把新空桶当作已迁移。

连接失败先检查 `ps` 和对应服务日志。PG 已健康但 API 认证失败时，核对实际数据库密码；
可选服务未开启时，相应入口不可用属于预期。不要把仅有容器 `running` 当作
API、登录、附件上传下载或真实通话验收通过。
