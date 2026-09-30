# 自动化部署脚本使用手册

> 适用版本：imboy v1.0.0-rc.1+
> 脚本位置：`scripts/imboy-deploy.sh`
> 配置模板：`scripts/.env.deploy.example`

---

## 运维处置卡片（owner / 止损 / 回滚）· W3-A04

| 要素 | 值 |
|---|---|
| **Owner** | IMBoy Ops（待用户指名，指名后替换本行） |
| **Escalation** | ① IMBoy Ops 值班 → ② 待指名平台负责人（IM/电话占位）→ ③ 脚本本身缺陷提 issue 到 imboy 仓（附 `--verbose` 日志） |

### 止损线（量化，触发即回滚）

- **自动止损**（脚本内置，无需人工）：smoke 验证失败 / 新节点 40s 未就绪 /
  nginx 切换失败——脚本自动恢复 symlink/vhost（见"失败语义（不变量）"）。
- **人工止损**：nginx 切换成功后 **10 分钟观察窗**内，5xx > 1%（5 分钟窗口）
  或消息投递 p99 > 1s 持续 5 分钟，或 WS 连接较切换前掉 > 50%——立即执行
  下方回滚，不在新节点上排障。

### 回滚（紧急回滚 = nginx 切回旧色节点）

```bash
# 方式 1：脚本回滚（推荐，切 nginx 指向旧节点）
bash scripts/imboy-deploy.sh rollback

# 方式 2：手动回滚（脚本不可用时）
ssh -p $SERVER_PORT $SERVER_USER@$SERVER_HOST \
  "sed -i 's/9801/9800/' /path/to/nginx.conf && nginx -s reload"
```

前提：旧节点仍在运行（`DEPLOY_STOP_OLD=false`，或手动重启旧版本目录，见
"常见问题 → 回滚时旧节点未运行"）。回滚后确认指标回绿再收尾。

---

## 概述

`scripts/imboy-deploy.sh` 是统一的部署入口，支持两种模式：

| 模式 | 命令 | 说明 |
|------|------|------|
| **全量部署** | `bash scripts/imboy-deploy.sh all` | 编译→上传→重启→迁移→前端，一步到位 |
| **增量部署** | `bash scripts/imboy-deploy.sh <组件>` | 按需只部署某个组件 |

服务器地址、端口和 Key 写在默认的 `scripts/.env.deploy`，也可用 `--env-file PATH`
选择一套独立客户配置，敏感配置不在命令行逐项传递。

---

## 首次配置（只做一次）

### 1. 生成配置文件

```bash
cp scripts/.env.deploy.example scripts/.env.deploy
$EDITOR scripts/.env.deploy   # 填写真实值
```

### 2. 配置项说明

```bash
# ── 服务器 SSH ────────────────────────────────────────────
SERVER_HOST=your.server.ip   # 服务器 IP 或域名
SERVER_PORT=22               # SSH 端口
SERVER_USER=root             # SSH 用户

# ── Erlang 后端 ───────────────────────────────────────────
DEPLOY_VSN=1.0.0-rc.1                              # 版本号（与 VERSION 文件一致）
DEPLOY_RELX_CONFIG=relxpro.config                  # 第二份生产 relx 配置
DEPLOY_NODE_NAME=prod-release001                   # 节点名（不含 @host）
DEPLOY_PLUGIN_TRUSTED_PUBLIC_KEY_FILE=~/.config/imboy/customer/plugin-signing-public.raw
DEPLOY_PROJECT_DIR=/www/wwwroot/imboy-api          # 服务器上项目工作目录
DEPLOY_BRANCH=main                                 # 部署分支
DEPLOY_BLUE_PORT=9800                              # 蓝节点端口（当前生产）
DEPLOY_GREEN_PORT=9801                             # 绿节点端口（备用）
DEPLOY_COOKIE=<your-random-cookie>              # Erlang 节点 cookie（随机值，经 secrets 注入，严禁写死）
NGINX_CONF=/path/to/nginx/pro.conf                 # nginx 配置文件路径
API_DOMAIN=api.domain.com                          # API 对外域名
PRODADM_CONF=/path/to/nginx/admin.conf             # Admin API vhost 配置路径
DEPLOY_STOP_OLD=true                               # 部署后是否停旧节点

# ── 管理后台 ─────────────────────────────────────────────
ADMIN_BUILD_DIR=../imboy-admin-frontend            # 本地 admin 仓库路径（相对 imboy/scripts/）
ADMIN_REMOTE_DIR=/www/wwwroot/prodadm.domain.com   # 服务器上静态文件目录
ADMIN_DOMAIN=prodadm.domain.com                    # Admin 对外域名

# ── 数据库 ────────────────────────────────────────────────
DB_CONTAINER=prod_imboy_pg18   # Docker 容器名
DB_NAME=imboy_pro              # 数据库名
DB_USER=imboy_user             # 数据库用户
DB_PORT=5182                   # 宿主机映射端口
```

> `.env.deploy` 已加入 `.gitignore`，不会提交到仓库。

旧命令中的服务器地址和版本号分别迁移为 `SERVER_HOST`、`DEPLOY_VSN`；
节点名优先读取 `DEPLOY_NODE_NAME`；留空时由统一入口按时间自动生成。旧 `-v` 对应统一入口末尾的 `-v`。
`api/all --local` 会在 SSH 前把本地 `VERSION`、`relx.config` 和
`DEPLOY_RELX_CONFIG`（默认 `relxpro.config`）同步为 `DEPLOY_VSN`，再上传同一版本源码。
销售版还必须为每套客户配置独立的 `DEPLOY_PLUGIN_TRUSTED_PUBLIC_KEY_FILE`：
它是 32 字节 Ed25519 raw 公钥，脚本只把公钥安装进新 release，私钥不得进入仓库或服务器。

多客户部署建议把配置放在仓库外并限制读取权限：

```bash
mkdir -p ~/.config/imboy/deploy
cp scripts/.env.deploy.example ~/.config/imboy/deploy/customer-a.env
chmod 600 ~/.config/imboy/deploy/customer-a.env
```

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
# 远端 Git 模式：服务器拉取配置中的 DEPLOY_BRANCH
bash scripts/imboy-deploy.sh api -v

# 本地源码模式：rsync over SSH 上传当前工作树后执行同一套蓝绿流程
bash scripts/imboy-deploy.sh api -v -l --env-file ~/.config/imboy/deploy/customer-a.env

# 只部署 React 管理后台（本地 bun build → rsync 上传）
bash scripts/imboy-deploy.sh admin

# 只执行数据库迁移
bash scripts/imboy-deploy.sh migrate

# 紧急回滚（将 Nginx 切回另一个节点端口）
bash scripts/imboy-deploy.sh rollback
```

---

## 客服 Widget 部署（cs）

> 本节描述 `cs` 组件合同（CSD-CLI-01 冻结）；命令由 `scripts/imboy-deploy.sh`
> 统一入口提供。面向**蓝绿多节点生产**形态；标准 Compose 部署（`imboy_widget`
> 静态容器）见 [customer-service-widget.md](./customer-service-widget.md)。

### 命令

```bash
bash scripts/imboy-deploy.sh cs [--env-file ~/.config/imboy/deploy/customer-a.env]
```

`cs` 始终从 clean Git HEAD 执行 `bun run build:widget`，不会复用旧产物；Backend
默认由服务器拉取同一 Git HEAD。`-v` 只增加日志，`-l` 只把 Backend 改为本地 rsync。
固定顺序：

```text
PRECHECK → BUILD_AND_VERIFY_WIDGET → STAGE_WIDGET_RELEASE → VALIDATE_CS_VHOST_AND_TLS
  → DEPLOY_BACKEND_BLUE_GREEN → ATOMIC_ACTIVATE_WIDGET_AND_VHOST → REAL_SMOKE
  → FINALIZE_OR_ROLLBACK_WIDGET_GATEWAY
```

### 专属配置项（`.env.deploy`）

```bash
CS_WIDGET_DOMAIN=cs.example.com          # Widget 网关域名（与 API/Admin 两两不同）
CS_BUILD_DIR=../imboy-admin/dist-widget  # 本地 Widget 构建产物目录
CS_REMOTE_ROOT=/www/wwwroot/cs.domain.com # 服务器上不可变 release 根（须含 .imboy-cs-root marker）
CS_NGINX_CONF=/path/to/nginx/cs.conf     # CS vhost 配置路径
CS_CERT_FULLCHAIN=/path/to/fullchain.pem # CS 域证书链
CS_CERT_KEY=/path/to/privkey.pem         # CS 域证书私钥
CS_SMOKE_SHOP_ORIGIN=https://shop.example.com # smoke 用的宿主页 origin（须在 allowlist 内）
```

### 失败语义（不变量）

- Backend 先成功再激活新 Widget；新 Backend 必须向后兼容旧 Widget。
- Backend release、Admin deploy-meta 与 Widget manifest 都绑定并校验源码 Git HEAD。
- 升级失败：只恢复先前 Widget symlink/vhost；已成功且向后兼容的 Backend 不回滚。
- 首次安装失败：移除一切未成功激活的 vhost/symlink/临时文件，无半配置残留。
- smoke 失败：恢复先前 symlink/vhost；新 release 目录保留待人工排查。
- 远端写入前生成时间戳备份 + checksum + 恢复命令记录；未知/foreign 文件不覆盖不删除。
- 全部输入（domain/path/host/port/version）在 SSH 前 allowlist 校验；CS 远端
  realpath 必须位于批准根内且含 `.imboy-cs-root` marker。
- verbose 输出零 secret/token/证书私钥/完整敏感配置/联系方式。

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
| 节点命名 | `DEPLOY_NODE_NAME@127.0.0.1`；留空时使用 `MMDDHHmm@127.0.0.1` |
| 就绪检测 | 新节点就绪检测用 40s 轮询（每 2s 探 `GET /healthz`，需 HTTP 200 且自报 `version` 与目标版本一致才判就绪，见 `scripts/lib/blue_green_deploy.sh` 的 `wait_for_health/2`），替代固定 sleep，慢服务器不误报 |
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

### 新节点 40s 未就绪或版本不符

```
✗ 新节点 40s 内未就绪或版本不符 (port=9801, expect=<VSN>)
```

就绪判定要求 `/healthz` 返回 200 且自报 `version` 与目标版本一致；最常见原因是目标色端口上有上一次部署的残留进程。

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
| `scripts/lib/blue_green_deploy.sh` | 蓝绿部署内部实现，仅由统一入口调用，不接受人工直接运行 |
| `scripts/start_node.sh` | 手动启动单个节点 |
| `scripts/stop_node.sh` | 手动停止节点 |
| `scripts/backup_pg.sh` | 数据库备份 |
| `scripts/restore_pg.sh` | 数据库恢复 |

---

## 参考文档

- 从零搭建服务器：[deployment.md](./deployment.md)
- Day-1 快速上手：[day1-quickstart.md](./day1-quickstart.md)
- 备份与恢复：[backup-restore.md](./backup-restore.md)
- 监控：[monitoring.md](./monitoring.md)
- 生产架构图：[production-architecture.md](./production-architecture.md)
