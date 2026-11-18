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
| PostgreSQL | 18（trixie） |
| PostGIS | 3.7.0（pgdg `3.7.0+dfsg-1.pgdg13+2`） |
| TimescaleDB | 2.32.0 |
| pgRouting | 4.0.2 |
| pg_roaringbitmap | 1.3.0 |
| pgvector | 0.8.7（pgdg 包） |
| pg_jieba | master（上游 2022 年后无发布，v2.0.1 tag 早于 master） |

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
