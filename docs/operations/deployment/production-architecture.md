# 生产环境架构与部署文档

> 服务器 IP：106.53.76.53 | SSH 端口：32
> 最后更新：2026-06-19

---

## 架构总览

```
外网用户
   │
   ▼ 80/443
┌─────────────────────────────────────────────────────┐
│                    nginx (宝塔)                      │
│                                                     │
│  pro.imboy.pub        prodadm.imboy.pub             │
│  s3.imboy.pub         www.imboy.pub                 │
│  turn.imboy.pub                                     │
└────────┬──────────────────┬────────────────────────-┘
         │                  │
         ▼ :9800            ▼ 静态文件
  beam.smp (Erlang)    /www/wwwroot/
  prc1@127.0.0.1       prodadm.imboy.pub/   ← React SPA
  OTP 16.2             www.imboy.pub/public/ ← 落地页
         │
         ├──▶ prod_imboy_pg18 (Docker) :5182  ← 主数据库
         └──▶ garage (S3)              :3900  ← 对象存储
```

---

## 各域名路由说明

### pro.imboy.pub — 主 API + WebSocket

| location | 转发目标 | 说明 |
|----------|----------|------|
| `/ws/` | `http://pro_imboy_api/ws/` | WebSocket 长连接 |
| `/v1/*` | `http://pro_imboy_api` | REST API |
| `/adm/` | 302 → prodadm.imboy.pub | 管理后台重定向 |
| `/app/*` | `http://pro_imboy_api` | App 专用接口 |
| `/` | `http://pro_imboy_api` | 其余请求 |

upstream `pro_imboy_api` → `127.0.0.1:9800`

### prodadm.imboy.pub — 管理后台

| location | 处理方式 | 说明 |
|----------|----------|------|
| `/v1/*` `/adm-api/*` `/app_version/*` | 反代 `:9800` | API 请求 |
| `/adm/` | alias 静态目录 | React SPA 文件 |
| `/assets/` | 静态缓存 1 年 | 前端资源 |
| `/` | 301 → `/adm/` | 根路径跳转 |

静态文件目录：`/www/wwwroot/prodadm.imboy.pub/`

### s3.imboy.pub — 对象存储

全部请求反代到 garage S3 API `127.0.0.1:3900`

### www.imboy.pub — 官网落地页

静态文件目录：`/www/wwwroot/www.imboy.pub/public/`

### turn.imboy.pub — TURN 中继服务器

coturn 进程，监听 `10.1.20.14:3478`（WebRTC 穿透用）

---

## 各服务位置与端口

| 服务 | 进程/容器 | 监听地址 | 部署位置 |
|------|-----------|----------|----------|
| Erlang 后端 | `beam.smp` (prc1 节点) | `0.0.0.0:9800` | `/usr/local/imboy-1.0.0-rc.1-001/` |
| nginx | 系统服务（宝塔管理） | `0.0.0.0:80/443` | `/www/server/panel/vhost/nginx/` |
| PostgreSQL（生产） | Docker `prod_imboy_pg18` | `0.0.0.0:5182` | Docker volume |
| PostgreSQL（开发） | Docker `dev_imboy_pg18` | `0.0.0.0:5180` | Docker volume |
| Garage S3 | 裸进程 | `0.0.0.0:3900`（公网 API）`127.0.0.1:3901`（RPC）`127.0.0.1:3903`（Admin）| `/var/lib/garage/` |
| FastDFS（旧，待废弃） | Docker `imboy_fastdfs` | `0.0.0.0:8080` | Docker |
| coturn | 系统进程 | `10.1.20.14:3478` | 系统服务 |
| 宝塔面板 | BT-Panel | `*:9898` | 系统服务 |
| SSH | sshd | `0.0.0.0:32` | 系统服务 |
| epmd | Erlang 端口映射 | `0.0.0.0:4369` | 随 beam 启动 |

---

## Erlang 后端部署细节

### 发布目录结构

```
/usr/local/imboy-1.0.0-rc.1-001/   ← 当前运行版本
/www/wwwroot/imboy-api/             ← 工作目录（源码 + 配置）
  ├── config/
  │   └── sys.pro.config            ← 生产配置（含 DB 连接、CORS）
  ├── docker-compose-pg18-pro.yml   ← PG 容器启动配置
  └── docker-compose.yml
```

### 历史版本（已停用）

```
/usr/local/imboy-0.7.2-pro010/
/usr/local/imboy-0.7.3-pro001/
/usr/local/imboy-0.7.3-pro003/
/usr/local/imboy-0.7.3-pro0703/
```

### 关键配置（sys.pro.config）

- DB：`127.0.0.1:5182`，数据库 `imboy_pro`，用户 `imboy_user`
- CORS 白名单：`https://pro.imboy.pub`，`https://prodadm.imboy.pub`
- HTTP 端口：`9800`

---

## 数据库

| 容器名 | 端口 | 数据库 | 用途 | 状态 |
|--------|------|--------|------|------|
| `prod_imboy_pg18` | 5182 | imboy_pro | **当前生产库** | 运行中 |
| `dev_imboy_pg18` | 5180 | imboy_pro | 开发环境 | 运行中 |
| `pro_imboy_pg18` | 5181 | imboy_pro | 旧生产库 | 已停止 |

启动命令：
```bash
cd /www/wwwroot/imboy-api
docker-compose -f docker-compose-pg18-pro.yml up -d
```

---

## Garage S3 对象存储

配置文件：`/etc/garage.toml`（或 `/opt/garage/garage.toml`）

```toml
metadata_dir = "/var/lib/garage/meta"
data_dir     = "/var/lib/garage/data"
replication_factor = 1

[s3_api]
s3_region     = "garage"
api_bind_addr = "0.0.0.0:3900"

[admin]
api_bind_addr = "127.0.0.1:3903"
```

对外通过 `s3.imboy.pub` 域名（nginx 反代 `:3900`）访问。

---

## 部署流程

### 后端发布新版本

```bash
# 1. 本地构建发布包
cd ~/project/imboy.pub/imboy
IMBOYENV=pro make rel

# 2. 上传到服务器
scp -P 32 _rel/imboy-*.tar.gz root@106.53.76.53:/usr/local/

# 3. 服务器上解压并切换
ssh root@106.53.76.53 -p 32
cd /usr/local
tar xzf imboy-X.Y.Z.tar.gz
# 修改软链或直接启动新版本

# 4. 热重载配置（无需重启）
# 连接到 Erlang 节点
/usr/local/imboy-1.0.0-rc.1-001/bin/imboy remote_console
> config_ds:local_reload().
```

### 前端管理后台发布

```bash
# 本地构建
cd ~/project/imboy.pub/imboy-admin-frontend
bun run build   # 产出 dist/

# 上传替换
scp -P 32 -r dist/* root@106.53.76.53:/www/wwwroot/prodadm.imboy.pub/
```

### 数据库迁移

```bash
# 连接生产库
docker exec -it prod_imboy_pg18 psql -U imboy_user -d imboy_pro

# 或通过 Erlang 迁移工具
cd /www/wwwroot/imboy-api
make ctl ARGS="db migrate"
```

---

## 常用运维命令

```bash
# 查看 Erlang 节点状态
epmd -names

# 连接到生产节点 shell
/usr/local/imboy-1.0.0-rc.1-001/bin/imboy remote_console

# 热重载配置
# （在 remote_console 中）
config_ds:local_reload().

# 重启 nginx
nginx -s reload

# 查看实时日志
tail -f /www/wwwroot/imboy-api/log/console.log

# 查看 Docker 容器状态
docker ps

# 连接生产数据库
docker exec -it prod_imboy_pg18 psql -U imboy_user -d imboy_pro
```
