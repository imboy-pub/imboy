# Docker 说明

本目录存放 IMBoy 后端与 PostgreSQL 相关的 Docker 构建文件和初始化脚本。

## 文件说明

- `imboy_Dockerfile_dev`
  Erlang/OTP 开发镜像，用于拉起后端开发环境或构建带源码的调试容器。
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
docker build -f docker/imboy_Dockerfile_dev -t imboy/imboy-api:dev .
```

### 2. 构建 PostgreSQL 18 镜像

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
  是两条独立通道；Hub 为多架构人工发布通道

## 常用运行示例

### 1. 创建本地网络

```bash
docker network create imboy-network
```

### 2. 启动 PostgreSQL 容器

```bash
docker run -d \
  --name imboy_postgis \
  --network imboy-network \
  -e POSTGRES_USER=imboy_user \
  -e POSTGRES_PASSWORD=change-me \
  -e POSTGRES_DB=imboy_v1 \
  -p 4321:5432 \
  imboy/pg18:dev
```

### 3. 进入容器排查

```bash
docker exec -it imboy_postgis bash
docker exec -it imboy_api bash
```

## 维护约束

- 不在本文档中记录环境专属 IP、域名、面板地址、账号密码或本地绝对路径。
- 生产环境部署参数应放在运维仓、部署平台或环境专属清单中维护。
- 如果镜像依赖、扩展清单或初始化脚本发生变化，应同步更新本文档与对应 Dockerfile / shell 脚本。
