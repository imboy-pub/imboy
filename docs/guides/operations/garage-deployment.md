# 如何安装 Garage S3 并接入 IMBoy

> **类型**：指南 · **读者**：第一次部署 IMBoy 的运维人员 · **适用版本**：Garage v2.4.1 · **最后验证**：2026-09-25
>
> **状态**：CURRENT。Garage 官方发布页在 2026-09-25 显示最新稳定版为
> `v2.4.1`（发布于 2026-09-08）。本指南固定版本，不使用会静默升级的 `latest`。

本指南用于单机 IMBoy 部署。最省事的方式是使用 IMBoy 社区版 Docker Compose；
已有裸机 Erlang 服务时，可在 Linux 上单独安装 Garage 二进制。

> Garage 单节点没有副本冗余。磁盘损坏会丢附件，生产环境必须备份
> `${DATA_DIR}/garage` 或使用 `scripts/backup_garage.sh`。多节点 Garage 不在本文范围内。

## 先选安装方式

| 你的情况 | 选择 | 入口 |
|---|---|---|
| 新装整套 IMBoy，Linux 已装 Docker | 方式 A，推荐 | `deploy/install.sh --edition community` |
| 只给现有 Linux IMBoy 增加 Garage | 方式 B | `scripts/garage-install.sh` |
| macOS 本地开发 | Docker 本地脚本 | `scripts/garage-local-setup.sh` |
| 已有 Garage，需要升级 | 先备份再升级 | [安全升级](#安全升级已有-garage) |

不要同时运行方式 A 和方式 B，否则两套 Garage 会争用 `3900`、`3901`、`3903`
端口。

## 安装前准备

### 1. 确认机器和磁盘

Linux 生产机建议至少准备：

- 64 位 x86_64 或 ARM64 Linux；
- 4 GB 内存；
- 一个不会随重启清空的磁盘目录；
- 磁盘可用空间大于预计附件量的 2 倍，以便备份和升级；
- 正确的系统时间，建议启用 NTP。

```bash
uname -s
uname -m
df -h
timedatectl status
```

`uname -s` 应为 `Linux`。`uname -m` 应为 `x86_64`、`aarch64` 或 `arm64`。

### 2. 确认端口用途

| 端口 | 用途 | 是否开放公网 |
|---|---|---|
| `3900` | S3 API | 不直接开放；生产经 Nginx `/s3/` 反代 |
| `3901` | Garage 节点 RPC | 单机不开放公网 |
| `3902` | Website API，仅服务 `scope=public` 对象 | 不直接开放；经独立文件域名反代 |
| `3903` | Admin/Metrics API | 不开放公网 |

```bash
ss -lnt | grep -E ':(3900|3901|3902|3903)\b' || true
```

没有输出表示端口未被占用。若有输出，先确认占用进程，不要直接结束未知服务。

## 方式 A：随 IMBoy 社区版一键安装

该方式会安装固定镜像 `dxflrs/garage:v2.4.1`，数据持久化到
`${DATA_DIR}/garage`。Garage 不直接暴露公网，客户端通过
`https://<API_DOMAIN>/s3/` 上传和下载。

### 1. 填写部署配置

```bash
cd /path/to/imboy/deploy
cp .env.example .env
chmod 600 .env
```

用文本编辑器打开 `.env`，至少填写域名、证书和安装器要求的项目。Garage 相关值：

```dotenv
IMBOY_GARAGE_ENDPOINT=http://garage:3900
IMBOY_GARAGE_BUCKET=imboy
IMBOY_GARAGE_ACCESS_KEY=GK_CHANGE_ME_GARAGE_ACCESS_KEY
IMBOY_GARAGE_SECRET_KEY=CHANGE_ME_GARAGE_SECRET_KEY
GARAGE_RPC_SECRET=CHANGE_ME_GARAGE_RPC_SECRET_64_HEX_CHARS_
```

正常使用 `deploy/install.sh` 时，安装器会幂等生成缺失的 Garage 随机密钥。
不要把 `.env` 提交到 Git，也不要把真实密钥发到聊天或工单。

### 2. 运行安装前检查

```bash
bash preflight.sh --edition community --docker
```

出现 `ERROR` 时先按提示修复。只有检查通过才继续。

### 3. 启动整套服务

```bash
bash install.sh --edition community
```

第一次运行若只生成 `.env` 模板后退出，这是正常行为：填完配置后再次执行同一命令。

### 4. 验证 Garage

```bash
docker compose -f docker-compose.community.yml ps garage
docker compose -f docker-compose.community.yml exec garage /garage --version
docker compose -f docker-compose.community.yml exec garage /garage status
curl -sS -o /dev/null -w '%{http_code}\n' http://127.0.0.1/healthz
```

验收标准：

- `garage` 容器状态为 `healthy`；
- 版本输出包含 `v2.4.1`；
- `garage status` 中节点位于 `HEALTHY NODES`；
- IMBoy `/healthz` 返回 `200`。

Garage 根路径未签名访问返回 `403` 是正常安全行为，不代表服务故障。

> **当前限制**：社区 Compose 的 `--default-bucket` 目前只自动创建私有桶 `imboy`，
> Nginx 也只代理 S3 API。它可用于私有附件，但 `scope=public` 的头像等公开资源尚未完成
> `imboy-public` + Website API 的一键初始化。该缺口已列入附件存储计划的 ST-04/AC-08；
> 未通过公开资源闭环前，不要把社区 Compose 标记为完整附件验收通过。

## 方式 B：Linux 裸机安装 Garage v2.4.1

### 1. 运行仓库脚本

```bash
cd /path/to/imboy
bash scripts/garage-install.sh
```

脚本会：

1. 下载官方固定版本 `v2.4.1` Linux 二进制；
2. 创建 `/etc/garage.toml`；
3. 创建 `garage` 系统用户和持久化目录；
4. 安装并启动 `garage.service`；
5. 初始化单节点布局、私有 bucket、公开 bucket 和访问密钥；
6. 仅对 `imboy-public` 启用 Garage Website 公开读取；
7. 打印需要写入 IMBoy 本地配置的示例。

脚本是幂等的，不会覆盖已有 `/etc/garage.toml`。若系统已安装其他版本，它也不会
擅自升级，必须先走[安全升级](#安全升级已有-garage)。

若已有配置缺少 `[s3_web]`，脚本会明确退出，避免自动拼接 TOML 破坏生产配置。请先
备份 `/etc/garage.toml`，按本文的公开文件入口配置 Website API，重启 Garage 并确认
`3902` 仅监听本机后，再重跑脚本完成 `imboy-public` 初始化。

需要补入 `/etc/garage.toml` 的最小配置是：

```toml
[s3_web]
bind_addr = "127.0.0.1:3902"
root_domain = ".garage.localhost"
index = "index.html"
```

编辑后执行 `sudo systemctl restart garage`。不要把 `3902` 绑定到公网 IP；外部访问必须
经过下文带 TLS 的 Nginx 文件域名。

> Garage v2.4.1 官方发布页只提供 Linux 二进制。macOS 请运行
> `bash scripts/garage-local-setup.sh`，不要使用来源不明的二进制。

### 2. 检查服务

```bash
garage --version
sudo systemctl status garage --no-pager
sudo garage -c /etc/garage.toml status
curl -sS -o /dev/null -w '%{http_code}\n' http://127.0.0.1:3900/
```

版本应包含 `v2.4.1`，服务应为 `active (running)`，节点应为 `HEALTHY`。
最后一条命令预期返回 `403`。

### 3. 配置 IMBoy

不要修改被 Git 跟踪的生产模板。把脚本最后打印的值写进
`config/sys.local.config`，或使用环境变量：

```bash
export IMBOY_GARAGE_ENDPOINT='http://127.0.0.1:3900'
export IMBOY_GARAGE_PUBLIC_ENDPOINT='https://api.example.com/s3'
export IMBOY_GARAGE_BUCKET='imboy'
export IMBOY_GARAGE_ACCESS_KEY='GK...'
export IMBOY_GARAGE_SECRET_KEY='...'
```

对应的 Erlang 配置结构为：

```erlang
{garage, #{
    endpoint => <<"http://127.0.0.1:3900">>,
    public_endpoint => <<"https://api.example.com/s3">>,
    region => <<"garage">>,
    bucket => <<"imboy">>,
    public_bucket => <<"imboy-public">>,
    public_base_url => <<"https://files.example.com">>,
    access_key => {env, <<"IMBOY_GARAGE_ACCESS_KEY">>},
    secret_key => {env, <<"IMBOY_GARAGE_SECRET_KEY">>}
}},
```

`endpoint` 是后端访问 Garage 的内网地址；`public_endpoint` 是客户端上传和私有下载时
使用的签名 S3 API 地址；`public_base_url` 是公开附件的匿名读取地址。三者不要写反，
`public_base_url` 后面也不要再拼 bucket 名。真实密钥只放环境变量或权限为 `0600`
的本地配置。

### 4. 配置反向代理

生产环境不要直接把 `3900`、`3902` 暴露公网。需要配置两条入口：

1. `https://api.example.com/s3/...` 反代到 `127.0.0.1:3900`，用于带签名的上传和私有下载；
2. `https://files.example.com/...` 反代到 `127.0.0.1:3902`，用于公开附件匿名读取，
   并把上游 `Host` 固定为 `imboy-public.garage.localhost`。

第一条可沿用仓库 Nginx 模板；第二条可参考生产 vhost 的 Website 分流：

- `deploy/nginx/templates/imboy.conf.template`
- `deploy/nginx/prod-vhosts/s3.imboy.pub.conf`

公开文件域名的最小 location 如下，放进已经配置好 TLS 的 `files.example.com` server：

```nginx
location / {
    proxy_pass http://127.0.0.1:3902;
    proxy_set_header Host imboy-public.garage.localhost;
    proxy_set_header X-Real-IP $remote_addr;
    proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
    proxy_set_header X-Forwarded-Proto $scheme;
}
```

不要把 Website API 代理到 S3 API 的 `3900`；匿名请求在 `3900` 返回 `403` 是正常行为。

修改后先检查语法，再平滑重载：

```bash
sudo nginx -t
sudo systemctl reload nginx
```

### 5. 验证真实上传闭环

先启动 IMBoy，再用测试账号 JWT 执行：

```bash
printf 'hello garage\n' > /tmp/imboy-garage-smoke.txt

curl -sS \
  -H 'Authorization: Bearer <TEST_JWT>' \
  'https://api.example.com/api/v1/attachment/presign?filename=imboy-garage-smoke.txt&mime_type=text/plain&scope=private'
```

从响应中取 `put_url` 和 `object_key`，继续：

```bash
curl -i -X PUT \
  -H 'Content-Type: text/plain' \
  --data-binary @/tmp/imboy-garage-smoke.txt \
  '<PUT_URL>'

curl -sS -X POST \
  -H 'Authorization: Bearer <TEST_JWT>' \
  -H 'Content-Type: application/json' \
  -d '{"object_key":"<OBJECT_KEY>","mime_type":"text/plain","size":13,"scope":"private"}' \
  'https://api.example.com/api/v1/attachment/confirm'

curl -sS \
  -H 'Authorization: Bearer <TEST_JWT>' \
  'https://api.example.com/api/v1/attachment/view_url?object_key=<URL_ENCODED_OBJECT_KEY>'
```

验收标准：PUT 返回 `2xx`，confirm 返回业务 `code=0`，view_url 返回短时 URL，
下载后内容与 `/tmp/imboy-garage-smoke.txt` 完全一致。

再把同一流程的 `scope` 改为 `public`：confirm 后返回的公开 URL 必须以
`https://files.example.com/` 开头，无 Authorization 访问返回 `200`，内容 SHA-256
与原文件一致。若公开流程失败，Garage 安装只能记为 `PARTIAL`，不能记为通过。

## macOS 本地开发

macOS 使用 Docker，数据写入 `/tmp/garage`，只适合开发测试：

```bash
cd /path/to/imboy
bash scripts/garage-local-setup.sh
docker exec garage-local /garage --version
docker exec garage-local /garage status
```

脚本会固定使用 `dxflrs/garage:v2.4.1`。`/tmp/garage` 可能被系统清理，不可当生产数据盘。

## 安全升级已有 Garage

不要直接覆盖正在运行的二进制。先记录版本并备份：

```bash
garage --version
sudo garage -c /etc/garage.toml status
sudo systemctl stop garage
sudo tar -C /var/lib -czf /var/backups/garage-before-v2.4.1.tgz garage
sudo cp /usr/local/bin/garage /usr/local/bin/garage.previous
```

阅读官方对应版本升级说明后，根据 CPU 下载固定版本，检查版本，再替换二进制：

```bash
case "$(uname -m)" in
  x86_64) GARAGE_PLATFORM=x86_64-unknown-linux-musl ;;
  aarch64|arm64) GARAGE_PLATFORM=aarch64-unknown-linux-musl ;;
  *) echo '不支持的 CPU 架构'; exit 1 ;;
esac

curl -fSL \
  "https://garagehq.deuxfleurs.fr/_releases/v2.4.1/${GARAGE_PLATFORM}/garage" \
  -o /tmp/garage-v2.4.1
chmod 755 /tmp/garage-v2.4.1
/tmp/garage-v2.4.1 --version
sudo install -m 755 /tmp/garage-v2.4.1 /usr/local/bin/garage
sudo systemctl start garage
garage --version
sudo garage -c /etc/garage.toml status
```

若启动或附件闭环失败，立即停止新进程，恢复旧二进制和备份数据，再调查原因：

```bash
sudo systemctl stop garage
sudo cp /usr/local/bin/garage.previous /usr/local/bin/garage
sudo systemctl start garage
```

不要在未验证备份可恢复前删除 `garage.previous` 或升级前备份。

## 备份与恢复

Docker 社区版至少备份 `${DATA_DIR}/garage`；裸机至少备份
`/etc/garage.toml`、`/var/lib/garage/meta` 和 `/var/lib/garage/data`。

项目提供 bucket 级备份入口：

```bash
bash scripts/backup_garage.sh --help
```

数据库中的附件元数据与 Garage 对象必须属于同一恢复点。只恢复 PostgreSQL 或只恢复
Garage 都可能产生“数据库有记录但文件不存在”或“有对象但无人引用”的孤儿状态。

## 常见问题

| 现象 | 原因 | 处理 |
|---|---|---|
| 根路径返回 `403` | 未签名访问被拒绝 | 正常；用 `garage status` 和真实上传闭环判断 |
| `SignatureDoesNotMatch` | 公网 Host、路径前缀、Region 或 Content-Type 与签名不一致 | 核对 `public_endpoint`、Nginx 是否保留 Host、上传 Content-Type |
| 手机拿到 `http://garage:3900` | 把容器内网地址发给客户端 | 设置 `IMBOY_GARAGE_PUBLIC_ENDPOINT=https://<API_DOMAIN>/s3` |
| `layout not configured` | 未应用单节点布局 | 重跑安装脚本，检查 `garage status` 与日志 |
| 重启后文件消失 | 数据目录位于 `/tmp` 或未挂载持久卷 | 恢复备份并改用持久目录 |
| 容器无法进入 shell | Garage 镜像基于 `scratch` | 使用 `docker exec <name> /garage ...`，不要执行 `/bin/sh` |
| Docker 拉取 `unauthorized` | 本机 registry 凭据或网络问题 | 先修复 Docker Hub 登录/网络；不要把本地构建当官方镜像 |

## 官方资料

- [Garage 下载页](https://garagehq.deuxfleurs.fr/download/)
- [Garage v2.4.1 发布构建列表](https://garagehq.deuxfleurs.fr/_releases.html)
- [Garage Quick Start](https://garagehq.deuxfleurs.fr/documentation/quick-start/)
- [Garage 公开 Website bucket](https://garagehq.deuxfleurs.fr/documentation/cookbook/exposing-websites/)
- [Garage 生产集群部署](https://garagehq.deuxfleurs.fr/documentation/cookbook/real-world/)
- [Garage systemd 指南](https://garagehq.deuxfleurs.fr/documentation/cookbook/systemd/)
