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
  --build-arg POSTGIS_VERSION='3.6.1+dfsg-1.pgdg13+1' \
  -t imboy/pg18:dev .
```

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
