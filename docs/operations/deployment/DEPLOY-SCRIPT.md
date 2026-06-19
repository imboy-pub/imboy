# 自动化部署脚本使用手册

> 适用版本：imboy v1.0.0-rc.1+
> 脚本位置：`scripts/imboy-deploy.sh`
> 配置模板：`scripts/.env.deploy.example`

---

## 概述

`scripts/imboy-deploy.sh` 是统一的部署入口，支持两种模式：

| 模式 | 命令 | 说明 |
|------|------|------|
| **全量部署** | `bash scripts/imboy-deploy.sh all` | 编译→上传→重启→迁移→前端，一步到位 |
| **增量部署** | `bash scripts/imboy-deploy.sh <组件>` | 按需只部署某个组件 |

所有服务器地址、端口、Key 统一写在 `scripts/.env.deploy`，脚本读取后执行，**不在命令行传参**。

---

## 首次配置（只做一次）

### 1. 生成配置文件

```bash
cd imboy/scripts
cp .env.deploy.example .env.deploy
$EDITOR .env.deploy   # 填写真实值
```

### 2. 配置项说明

```bash
# ── 服务器 SSH ────────────────────────────────────────────
SERVER_HOST=your.server.ip   # 服务器 IP 或域名
SERVER_PORT=22               # SSH 端口
SERVER_USER=root             # SSH 用户

# ── Erlang 后端 ───────────────────────────────────────────
DEPLOY_VSN=1.0.0-rc.1                              # 版本号（与 VERSION 文件一致）
DEPLOY_PROJECT_DIR=/www/wwwroot/imboy-api          # 服务器上项目工作目录
DEPLOY_BRANCH=main                                 # 部署分支
DEPLOY_BLUE_PORT=9800                              # 蓝节点端口（当前生产）
DEPLOY_GREEN_PORT=9801                             # 绿节点端口（备用）
DEPLOY_COOKIE=imboycookie                          # Erlang 节点 cookie
NGINX_CONF=/path/to/nginx/pro.conf                 # nginx 配置文件路径
DEPLOY_STOP_OLD=true                               # 部署后是否停旧节点

# ── 管理后台 ─────────────────────────────────────────────
ADMIN_BUILD_DIR=../imboy-admin-frontend            # 本地 admin 仓库路径（相对 imboy/scripts/）
ADMIN_REMOTE_DIR=/www/wwwroot/prodadm.domain.com   # 服务器上静态文件目录

# ── 数据库 ────────────────────────────────────────────────
DB_CONTAINER=prod_imboy_pg18   # Docker 容器名
DB_NAME=imboy_pro              # 数据库名
DB_USER=imboy_user             # 数据库用户
DB_PORT=5182                   # 宿主机映射端口
```

> `.env.deploy` 已加入 `.gitignore`，不会提交到仓库。

### 3. 配置 SSH 免密登录

```bash
ssh-copy-id -p $SERVER_PORT $SERVER_USER@$SERVER_HOST
# 验证
ssh -p $SERVER_PORT $SERVER_USER@$SERVER_HOST "echo ok"
```

---

## 使用方式

### 全量部署

按顺序执行：api 蓝绿部署 → 数据库迁移 → admin 前端上传。

```bash
bash scripts/imboy-deploy.sh all
```

### 增量部署

```bash
# 只部署 Erlang 后端（蓝绿零停机）
bash scripts/imboy-deploy.sh api

# 只部署 React 管理后台（本地 bun build → rsync 上传）
bash scripts/imboy-deploy.sh admin

# 只执行数据库迁移
bash scripts/imboy-deploy.sh migrate

# 紧急回滚（将 Nginx 切回另一个节点端口）
bash scripts/imboy-deploy.sh rollback
```

---

## 蓝绿部署原理

```
当前状态:   [蓝 :9800] ← nginx upstream
                              ↓
部署新版本: [蓝 :9800]  [绿 :9801] ← 编译、启动
                              ↓
切换 nginx: [蓝 :9800]  [绿 :9801] ← nginx upstream
                              ↓
停旧节点:                [绿 :9801] ← nginx upstream（蓝已停）
```

- 每次部署自动识别当前活跃色，选对立色为目标
- nginx 切换前先 `nginx -t` 验证配置，失败自动回滚备份
- `DEPLOY_STOP_OLD=false` 可保留旧节点，手动确认稳定后再停

### 紧急回滚

新版本出现问题时：

```bash
# 方式 1：脚本回滚（切 nginx 指向旧节点）
bash scripts/imboy-deploy.sh rollback

# 方式 2：手动回滚
ssh -p $SERVER_PORT $SERVER_USER@$SERVER_HOST \
  "sed -i 's/9801/9800/' /path/to/nginx.conf && nginx -s reload"
```

> 回滚要求旧节点仍在运行，即 `DEPLOY_STOP_OLD=false` 或手动停的旧节点未被清理。

---

## 前端部署说明

`admin` 组件执行以下步骤：

1. 本机执行 `bun install --frozen-lockfile && bun run build`，生成 `dist/`
2. 用 `rsync -az --delete` 增量同步到服务器（比全量 scp 快，文件未变不传输）
3. 若本机无 rsync，回退为 scp 全量上传

前置要求：本机已安装 `bun`（`curl -fsSL https://bun.sh/install | bash`）。

---

## 脚本内部机制

| 机制 | 说明 |
|------|------|
| SSH ControlMaster | 整个部署只握手一次，所有命令复用同一 TCP 连接 |
| 远端编译 | `git pull` + `make rel` 在服务器上执行，避免本地环境差异 |
| 自动节点命名 | 节点名格式 `MMDDHHmm@127.0.0.1`，每次部署唯一 |
| 端口轮询 | 新节点就绪检测用 40s 轮询替代固定 sleep，慢服务器不误报 |
| 输入校验 | `SERVER_HOST`、`VSN`、`COOKIE` 等均有正则校验，防注入 |
| 错误中止 | `set -Eeuo pipefail`，任意步骤失败立即终止 |
| 退出清理 | `trap cleanup EXIT` 确保 SSH 连接正常关闭 |

---

## 常见问题

### SSH 连接失败

```
❌ SSH 连接失败，请检查 SERVER_HOST / SERVER_PORT / SERVER_USER
```

检查：
1. `.env.deploy` 中 `SERVER_HOST`、`SERVER_PORT`、`SERVER_USER` 是否正确
2. 是否配置了 SSH 免密：`ssh-copy-id -p $SERVER_PORT $SERVER_USER@$SERVER_HOST`

### nginx 切换失败，已回滚

```
upstream 替换失败已回滚
```

检查 `NGINX_CONF` 路径是否正确，以及配置文件中的端口格式是否为 `server 127.0.0.1:XXXX;`。

### 新节点 40s 未就绪

```
✗ 新节点 40s 内未就绪 (port=9801)
```

SSH 到服务器查看日志：

```bash
tail -100 /usr/local/imboy-*/log/console.log
tail -100 /usr/local/imboy-*/log/error.log
```

### bun 未安装

```
❌ 本机未安装 bun
```

```bash
curl -fsSL https://bun.sh/install | bash
```

### 回滚时旧节点未运行

```
✗ 旧节点 (port=9800) 未在运行，无法回滚
```

旧节点已被停止，需要手动启动旧版本目录下的节点：

```bash
# 找到旧版本目录
ls /usr/local/imboy-*
# 启动旧节点
/usr/local/imboy-OLD_VERSION/bin/imboy daemon
# 然后再执行脚本回滚
bash scripts/imboy-deploy.sh rollback
```

---

## 与现有脚本的关系

| 脚本 | 用途 |
|------|------|
| `scripts/imboy-deploy.sh` | **本文档**：统一入口，全量/增量部署 |
| `scripts/deploy.sh` | 原蓝绿部署脚本（命令行传参版，保留兼容） |
| `scripts/start_node.sh` | 手动启动单个节点 |
| `scripts/stop_node.sh` | 手动停止节点 |
| `scripts/backup_pg.sh` | 数据库备份 |
| `scripts/restore_pg.sh` | 数据库恢复 |

---

## 参考文档

- 从零搭建服务器：[DEPLOYMENT.md](./DEPLOYMENT.md)
- Day-1 快速上手：[DAY1-QUICKSTART.md](./DAY1-QUICKSTART.md)
- 备份与恢复：[BACKUP-RESTORE.md](./BACKUP-RESTORE.md)
- 监控：[MONITORING.md](./MONITORING.md)
- 生产架构图：[production-architecture.md](./production-architecture.md)
