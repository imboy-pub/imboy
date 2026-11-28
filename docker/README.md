# Docker 说明

本目录统一存放 IMBoy 四个可投产运行镜像的配方、构建工具和 PostgreSQL 初始化脚本。
本地启动统一使用根目录 `docker-compose.yml` 与 `bash scripts/dev_setup.sh`，步骤见
[本地开发指南](../docs/guides/operations/deployment/local-development.md)。

## 文件说明

- `backend_Dockerfile`
  Backend 多阶段生产运行镜像，Erlang/OTP 编译、自包含 release、Debian 13 slim 运行；仓根 `Dockerfile` 是兼容软链接。
- `admin_Dockerfile` / `widget_Dockerfile`
  管理后台及客服 Widget 生产静态服务镜像；配方与批准的前端源码逐字节核对。
- `backend_dev_Dockerfile`
  Erlang/OTP 29 开发工具镜像；从当前检出挂载源码，不在构建时克隆远端分支。
- `pg18_Dockerfile`
  PostgreSQL 18 运行镜像，集成 PostGIS、pgvector、timescaledb、pg_jieba、pgRouting 等扩展。
- `pg-initdb-imboy.sh`
  PostgreSQL 首次初始化时执行，负责加载和升级核心扩展。
- `pg-update-imboy.sh`
  PostgreSQL 启动后用于补充扩展安装和升级。
- `pg_jieba_userdict.txt.big`
  `pg_jieba` 使用的自定义词典。

## 当前版本基线（imboy/pg18:3.7.0-1）

对齐上游 [postgis/docker-postgis 18-3.7](https://github.com/postgis/docker-postgis/tree/master/18-3.7)：

| 组件 | 版本 |
|---|---|
| PostgreSQL | 18.7（Trixie，pgdg `18.7-1.pgdg13+2`） |
| PostGIS | 3.7.0（pgdg `3.7.0+dfsg-1.pgdg13+2`） |
| TimescaleDB | 2.32.0 |
| pgRouting | 4.0.2 |
| pg_roaringbitmap | 1.3.0 |
| pgvector | 0.8.7（pgdg 包） |
| pg_jieba | master（上游 2022 年后无发布，v2.0.1 tag 早于 master） |

以上 PostgreSQL 版本来自已发布镜像的 amd64/arm64 配置；`3.7.0-1` 是 PostGIS 标签，不是 PostgreSQL 小版本。
已核对的多架构摘要为 `sha256:367c1affeee9afc2d46705d65bee5af8294cbda2159b2be96c0393fdec6c41bb`。
Dockerfile 仍从浮动的 `postgres:18-trixie` 构建；重建的镜像须重新核验版本和摘要，不能沿用本次证据。
原生源码构建使用独立的 [版本及校验和锁](../scripts/ci/native-eunit-pins.json)，不等同于镜像扩展组合。

PostGIS 3.7.0 破坏性变更：`postgis_tiger_geocoder`（NEWS #6052）与
`address_standardizer`（NEWS #6053）被移出核心发行版，pgdg 不再打包，
镜像与初始化脚本已同步移除（业务代码无引用）。旧数据卷升级到本镜像后，
已存在的同名扩展对象不受影响，但无法在新建库中安装这两个扩展。

## 常用构建命令

### 1. 构建 Erlang 开发镜像

```bash
# Align the non-root development user with the checkout owner.
docker build -f docker/backend_dev_Dockerfile \
  --build-arg DEV_UID="$(id -u)" --build-arg DEV_GID="$(id -g)" \
  -t imboy/imboy-api:dev .
```

### 2. 构建生产 Backend 镜像

```bash
docker build -f docker/backend_Dockerfile -t imboy/backend:$(cat VERSION) .
```

### 3. 构建 PostgreSQL 18 镜像

```bash
docker build -f docker/pg18_Dockerfile -t imboy/pg18:dev .
```

如需覆盖版本参数，可在构建时显式传入 `--build-arg`，例如：

```bash
docker build \
  -f docker/pg18_Dockerfile \
  --build-arg PG_MAJOR=18 \
  --build-arg POSTGIS_VERSION='3.7.0+dfsg-1.pgdg13+2' \
  -t imboy/pg18:dev .
```

## 发布镜像到 Docker Hub（imboy org）

pg18 镜像采用双架构发布：`linux/amd64`（prod 服务器 x86_64）+
`linux/arm64`（Apple Silicon 开发机）。**单一架构的镜像对另一半平台不可用**
（Apple Silicon 上直接 `docker build` 只产出 arm64，勿直接 push）。

**执行目录：必须在 imboy 仓根**（Dockerfile 内 `COPY ./docker/...` 相对构建
上下文解析，上下文=仓根；`-f docker/pg18_Dockerfile` 指定配方）。

```bash
cd /path/to/imboy            # 仓根，不是 docker/ 子目录

# 1. 登录（账号须有 imboy org 的 push 权限；仓不存在时首次 push 自动建仓）
docker login

# 2. 双架构一次构建+推送（本机 arm64 命中缓存，amd64 走 Rosetta 模拟编译，
#    首次约 1-2 小时；建议先用单架构 --load 预热再推，见下方"预热"）
docker buildx build \
  --platform linux/amd64,linux/arm64 \
  -f docker/pg18_Dockerfile \
  -t imboy/pg18:3.7.0-1 \
  --push .

# 3. 校验 Hub 上的 manifest（审计留档：记录 digest 与平台清单）
docker buildx imagetools inspect imboy/pg18:3.7.0-1
```

多架构构建耗时过长时，可先预热 amd64 再推（构建缓存共享）：

```bash
# 预热（免登录）：amd64 模拟编译全部走一遍
docker buildx build --platform linux/amd64 \
  -f docker/pg18_Dockerfile -t imboy/pg18:amd64-warmup --load .
# 登录后正式推：双平台均命中缓存，分钟级完成
```

**docker driver 限制的实测替代配方（2026-11-19 首发实证）**：Docker Desktop
默认 `desktop-linux` builder（docker driver）不支持 `--platform a,b` 多平台
（开启 containerd image store 可解，但需重启 Docker）。替代：单平台分推 +
manifest 合并：

```bash
CTX='--build-context postgres:18-trixie=docker-image://docker.1ms.run/library/postgres:18-trixie'
# 1) arm64 推送（记录 digest_A）
docker buildx build --platform linux/arm64 $CTX \
  -f docker/pg18_Dockerfile -t imboy/pg18:3.7.0-1 --push .
docker buildx imagetools inspect imboy/pg18:3.7.0-1   # 记下 Digest
# 2) amd64 推送（同 tag 覆盖，记录 digest_B）
docker buildx build --platform linux/amd64 $CTX \
  -f docker/pg18_Dockerfile -t imboy/pg18:3.7.0-1 --push .
docker buildx imagetools inspect imboy/pg18:3.7.0-1   # 记下 Digest
# 3) 合并为 manifest list（tag 指向 list，双平台齐备）
docker buildx imagetools create -t imboy/pg18:3.7.0-1 \
  imboy/pg18@sha256:<digest_A> imboy/pg18@sha256:<digest_B>
```

`$CTX` 重定向基础镜像解析到可用 mirror：Docker Desktop 的 registry-mirrors
配置（如 docker.zhai.cm）失效时 BuildKit 元数据解析会整体超时，`--build-context`
可绕开。匿名拉取验证（模拟 prod）：`curl auth.docker.io/token...` 换 token 后
GET `registry-1.docker.io/v2/imboy/pg18/manifests/3.7.0-1` 应返回
manifest list 且含双平台。

发布纪律：

- tag 沿用 `<PostGIS 版本>-<序号>` 惯例（如 `3.7.0-1`），与 compose 引用一致
- **禁止推 `latest` 等浮动 tag**（镜像审计红线：不可复现、无法钉 digest）
- prod 部署侧可用 digest 钉扎：`imboy/pg18:3.7.0-1@sha256:<digest>`
- 与 release CI（tag 触发构建 `ghcr.io/imboy-pub/imboy-pg18`，仅 amd64）
  是既有独立通道；历史 PG18 专用标签继续保留，四镜像组合分发见下一节


## 四镜像统一构建与 Docker Hub 分发 / Four-image builds and Docker Hub distribution

生产 Backend 唯一配方为 `docker/backend_Dockerfile`，构建上下文仍为仓根。
仓根 `Dockerfile` 仅为兼容旧命令的软链接；新脚本、CI 与 Compose 显式使用新路径。
原 `imboy_Dockerfile_dev` 改名为 `backend_dev_Dockerfile`，是独立的交互开发工具，
不是生产运行镜像。

### 交付定位 / Delivery positioning

Docker 优先服务快速体验、低门槛交付，以及有容器运维能力、部署规模和可用性要求适合单机编排的团队。四个镜像仍按可投产标准设计，不提供另一套降低安全或数据可靠性要求的演示镜像。选择 Docker 不等于必须选择原生部署，也不代表容器模式已经通过所有生产验收。

对于长期运行、需要更精细资源控制和运维的生产环境，优先推荐云 ECS 上的原生部署（Erlang/OTP 自包含 release、PostgreSQL、Nginx 等），使用现有 `scripts/imboy-deploy.sh` 部署与升级路径和[原生 systemd 交付指南](../docs/guides/operations/deployment/native-shell/README.md)。两种部署形态共享产品版本和业务契约；本工具不改变原生部署路径。

Docker 正式交付须完成对应平台的四镜像候选验收、按 digest 部署、持久化数据和密钥、配置与 HTTPS 检查、备份恢复、升级/回滚演练和基本监控。 Backend API 以非 root 身份运行，丢弃全部 Linux capabilities 并启用 `no-new-privileges`；保留既有 app `priv` 可写合同，私钥卷只读、日志卷独立可写，`/tmp`、`/home/imboy`、`/opt/imboy/var` 使用受限 tmpfs。无业务写入需求的 edge 与 proof 服务采用只读根文件系统。未通过的验收不得宣称已具备生产验证证据；单机容器部署也不等于高可用部署。

Docker images prioritize fast evaluation and low-friction delivery, including production use by teams whose operational capacity and availability requirements fit the supported deployment topology. They are not demo-only artifacts. Native ECS deployment remains the recommended option for long-running production with more granular resource and operations control. Both modes retain the same product and business contracts. Production readiness requires evidence for the actual candidate and target platform, including persistent state, security, backup/restore, and upgrade/rollback; single-host deployment is not an HA guarantee.

### 构建配方 / Build recipes

Admin 和 Widget 的两个配方镜像于本目录
（`admin_Dockerfile`、`widget_Dockerfile`），由已批准的前端提交
`imboy-admin-frontend` 的 `Dockerfile`、`Dockerfile.widget` 提供真源。
脚本逐字节核对配方，发生漂移时拒绝构建；升级前端配方时同步这两份镜像。

| 组件 | GHCR | Docker Hub |
|---|---|---|
| Backend | ghcr.io/imboy-pub/imboy-backend | imboy/backend |
| PG18 | ghcr.io/imboy-pub/imboy-pg18 | imboy/pg18 |
| Admin | ghcr.io/imboy-pub/imboy-admin | imboy/admin |
| Widget | ghcr.io/imboy-pub/imboy-widget | imboy/widget |

Backend 运行时和 PG18 采用 Debian 13（trixie）；Admin/Widget 保留前端仓的
Bun/Nginx Alpine 配方。不要用 `latest` 替代明确版本或已核验的摘要。

需要 Docker Buildx、Git、Python 3、Bun 1.3.14；双架构需要支持多平台的
builder（如 docker-container driver）及相应架构执行能力。
在干净的后端检出中设置与 Release CI 相同、经批准的完整 `ADMIN_REPO_REF`，
禁止使用 main、latest 或短 SHA。脚本使用提交快照，不把本机未跟踪/忽略文件打包。

```bash
# 本地候选构建，不向注册表写入；Apple Silicon 可选 linux/arm64
export ADMIN_REPO_REF='<approved-full-40-character-sha>'
bash docker/build_images.sh build --platform linux/arm64

# 一次构建、两个 registry 候选标签；不发布正式版本
docker login ghcr.io
docker login docker.io
bash docker/build_images.sh build --platform linux/amd64,linux/arm64 --push
```

`--push` 验证两个 registry 上的摘要均等于 Buildx 输出的摘要。
候选标签包含版本、后端提交与构建标识；输出的 digest 不是验收通过收据。
默认 `build` 仅本地加载单平台镜像，默认平台为 linux/amd64。
构建失败可能留下不完整候选，不得作为正式版本发布。

正式版本需先通过完整四镜像候选验收，并按现行发布授权将同一组摘要晋升到 GHCR
正式版本标签。随后使用如下命令（参数从已验收候选清单/收据取得，按 backend、
pg18、admin、widget 顺序）：

```bash
# 默认只检查四个 GHCR 来源和四个 Docker Hub 目标，不写入
bash docker/build_images.sh publish "$(cat VERSION)" "$BACKEND_REF" "$PG_REF" "$ADMIN_REF" "$WIDGET_REF"

# 需要 skopeo（macOS: brew install skopeo）；登录后显式执行
skopeo login ghcr.io
skopeo login docker.io
bash docker/build_images.sh publish --apply "$(cat VERSION)" "$BACKEND_REF" "$PG_REF" "$ADMIN_REF" "$WIDGET_REF"
```

`publish` 要求 GHCR 正式版本标签已与传入摘要一致；不会自行判定业务验收通过，
不得绕过完整验收门。使用 skopeo --all --preserve-digests 跨仓复制所有平台及
manifest，不重新编译。目标已存在且摘要不同则拒绝写入。所有发布写者需串行；
跨 registry 不能原子提交，失败后仅用同一组摘要重试，四个目标全部核验后才宣布发布完成。
现有 `imboy/pg18:3.7.0-1` 标签与其独立发布流程保持不变；
四镜像组合使用 IMBoy 的 VERSION 标签，不覆盖数据库历史标签。

离线回归：`python3 docker/test_build_images.py`。不需要真实 registry 或 Docker。

## 使用开发工具镜像

```bash
# 当前源码由 bind mount 提供；首次使用需拉取并编译依赖。
docker run --rm -it -v "$PWD:/workspace/imboy" imboy/imboy-api:dev
```

开发镜像默认以非 root 用户运行（UID/GID 10001）；挂载本机源码并编译时，按上面的构建命令匹配宿主用户 UID/GID，避免生成 root 所有的文件。UID/GID 不允许为 0；不要使用 root 或全局放宽目录权限解决挂载问题。

The development image runs as a non-root user by default (UID/GID 10001). Match the host checkout owner's numeric IDs at build time for writable bind mounts; neither ID may be zero.

在容器内构建源码前核对挂载目录与数据库地址。容器中的 `127.0.0.1` 指向容器自身，
不能直接照搬宿主机 PostgreSQL 配置。日常容器化启动和宿主机源码调试按
[本地开发指南](../docs/guides/operations/deployment/local-development.md) 选择入口。

## 维护约束

- 不在本文档中记录环境专属 IP、域名、面板地址、账号密码或本地绝对路径。
- 生产环境部署参数应放在运维仓、部署平台或环境专属清单中维护。
- 如果镜像依赖、扩展清单或初始化脚本发生变化，应同步更新本文档与对应 Dockerfile / shell 脚本。
