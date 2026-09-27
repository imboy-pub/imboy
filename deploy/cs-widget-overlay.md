# 商务版客服域 overlay 运维手册 / Business-edition CS overlay runbook

> 适用对象：使用商务交付基线 `docker-compose.prod.yml`（不随开源仓分发）的部署。
> 社区版（`docker-compose.community.yml`）**已内置**同等 CS 能力，不需要本 overlay。
>
> Audience: deployments on the business-edition baseline `docker-compose.prod.yml`
> (delivered via commercial channel, NOT in the open-source repo). The community
> edition already ships the same CS stack in-tree and must NOT stack this overlay
> on top (stacking is harmless but redundant — see §4).

---

## 0. overlay 是什么 / What the overlay is

`docker-compose.prod-cs.yml` 把社区版内置的客服域（Customer Service）增量以
`-f 基线 -f overlay` 形式补给商务版栈：

| 增量 | 内容 |
|---|---|
| 新增服务 `imboy_widget` | 客服托管 Widget 纯静态产物容器（imboyadmin 仓 `Dockerfile.widget` 构建：loader.js / assets / frame HTML 壳），零业务配置、零 DB、零队列、零 secret；只读根文件系统 + tmpfs `/tmp`；仅 compose 内网可达（只 expose 8080） |
| `imboy_nginx` 环境增量 | 注入 `CS_WIDGET_DOMAIN`，并把 `NGINX_ENVSUBST_FILTER` 扩为 `^(API_DOMAIN\|ADMIN_DOMAIN\|S3_UPSTREAM\|CS_WIDGET_DOMAIN)$`——envsubst 才会渲染 `nginx/templates/cs-widget.conf.template`（第三域 vhost） |
| `imboy_nginx` 依赖增量 | `depends_on` 追加 `imboy_widget`（compose 序列合并去重，基线依赖不受影响） |

所有命令默认在 `deploy/` 目录执行，`-f` 顺序固定为「基线在前、overlay 在后」。

---

## 1. 安装 / Install

前置条件：

1. 商务版基线栈已按 `deploy/README.md` 商务版路径正常运行（backend / admin / nginx / certbot）；
2. `deploy/.env` 中 `CS_WIDGET_DOMAIN` 已填写（与 `API_DOMAIN` / `ADMIN_DOMAIN` 两两不同，
   DNS A 记录指向本机；变量说明见 `deploy/.env.example`）；可选 `IMBOY_WIDGET_IMAGE`
   覆盖 widget 镜像（默认 `ghcr.io/imboy-pub/imboy-widget:${IMBOY_VERSION}`，与
   backend/admin 同一发布流水线）；
3. 数据库已应用全部客服域迁移——用门脚本自检（CP-ASSET-01）：
   ```bash
   make cs-migration-gate PGHOST=127.0.0.1 PGPORT=<PG_PORT> PGUSER=imboy_user \
     PGPASSWORD=*** PGDATABASE=<DB>
   ```
   非 zero 退出时**不要继续安装**（后端起量前客服迁移必须全部落库）。

安装步骤：

```bash
cd deploy

# 1) 预演合并结果（应 exit 0，且 services 列表包含 imboy_widget）
docker compose --env-file .env \
  -f docker-compose.prod.yml -f docker-compose.prod-cs.yml config >/dev/null && echo OK

# 2) 拉镜像并起 CS 增量（nginx 因 depends_on 追加了 widget 会一并重建）
docker compose --env-file .env \
  -f docker-compose.prod.yml -f docker-compose.prod-cs.yml pull imboy_widget
docker compose --env-file .env \
  -f docker-compose.prod.yml -f docker-compose.prod-cs.yml up -d

# 3) 为 CS 第三域签发 TLS 证书：把 CS_WIDGET_DOMAIN 加入 init-letsencrypt.sh
#    的域名清单后执行一次（后续续期由 certbot 容器自动覆盖）
bash nginx/init-letsencrypt.sh

# 4) 验收
docker compose --env-file .env \
  -f docker-compose.prod.yml -f docker-compose.prod-cs.yml ps imboy_widget
curl -fsS "https://${CS_WIDGET_DOMAIN}/health.txt"    # 应输出 OK imboy-cs-widget ...
```

## 2. 升级 / Upgrade

widget 镜像 tag 与 `IMBOY_VERSION` 强一致，升级 = 换 tag（禁 latest）：

```bash
# 1) .env 更新 IMBOY_VERSION（或显式设置 IMBOY_WIDGET_IMAGE 指定新 tag）
# 2) 拉新镜像并滚动重启 CS 服务
docker compose --env-file .env \
  -f docker-compose.prod.yml -f docker-compose.prod-cs.yml pull imboy_widget
docker compose --env-file .env \
  -f docker-compose.prod.yml -f docker-compose.prod-cs.yml up -d imboy_widget imboy_nginx

# 3) 健康验收（health.txt 的 source_head 应为新版本 commit）
curl -fsS "https://${CS_WIDGET_DOMAIN}/health.txt"
```

后端 / admin 若同批升级，按 `deploy/README.md` 的商务版升级路径一起滚动；
客服域数据库若有新增迁移，重复 §1 的第 3 步门自检。

## 3. 回滚 / Rollback

```bash
# 1) .env 把 IMBOY_VERSION 切回上一个发布 tag（或 IMBOY_WIDGET_IMAGE 指回旧 tag）
# 2) 重新拉起
docker compose --env-file .env \
  -f docker-compose.prod.yml -f docker-compose.prod-cs.yml pull imboy_widget
docker compose --env-file .env \
  -f docker-compose.prod.yml -f docker-compose.prod-cs.yml up -d imboy_widget imboy_nginx

# 3) 验收同 §2 第 3 步
```

注意：`/widget-assets/` 网关缓存策略为 no-cache 重验证（hosted-widget-contract
S6），回滚后已打开的宿主页刷新即取回旧资产，无需清理 CDN/浏览器缓存。

## 4. 卸载 / Uninstall

```bash
cd deploy

# 1) 停止并移除 CS 增量（去掉 overlay 后 up 会移除 overlay 独有的服务）
docker compose --env-file .env -f docker-compose.prod.yml up -d --remove-orphans

# 2) 确认第三域 vhost 已不再渲染（此时基线 filter 不含 CS_WIDGET_DOMAIN，
#    cs-widget.conf.template 不会被 envsubst 处理）
curl -fsS "https://${CS_WIDGET_DOMAIN}/" || echo "CS vhost 已下线（预期失败）"

# 3) （可选）删除 widget 镜像与 .env 中的 CS_WIDGET_DOMAIN / IMBOY_WIDGET_IMAGE
docker image rm "ghcr.io/imboy-pub/imboy-widget:${IMBOY_VERSION:-1.0.0-alpha.71}" || true
```

卸载不影响 backend / admin：客服域 API 与数据库表保留（静态面与网关面先下线，
数据面回滚属数据库迁移 down 链，不在本 overlay 职责内）。

---

## 5. 与社区版 CS 结构的机械等价对照 / Mechanical-equivalence table

对照方法：`docker compose config` 分别渲染「community 单文件」与
「prod 基线 + 本 overlay」，对 `imboy_widget` 服务做全字段 diff、对
`imboy_nginx` 的 CS 相关增量做逐 key 比对（CP-ASSET-03-A06 证据）：

| 项 | community（内置） | prod + 本 overlay | 等价 |
|---|---|---|---|
| `imboy_widget.image` | `${IMBOY_WIDGET_IMAGE:-ghcr.io/imboy-pub/imboy-widget:${IMBOY_VERSION:-1.0.0-alpha.71}}` | 同左（逐字节一致） | ✅ |
| `imboy_widget` 其余全部字段（container_name / hostname / restart / read_only / tmpfs / expose / healthcheck / deploy.resources / logging） | — | 渲染后 diff 为空集 | ✅ |
| `imboy_nginx.environment.CS_WIDGET_DOMAIN` | `${CS_WIDGET_DOMAIN}` | `${CS_WIDGET_DOMAIN}`（overlay 注入） | ✅ |
| `imboy_nginx.environment.NGINX_ENVSUBST_FILTER` | `^(API_DOMAIN\|ADMIN_DOMAIN\|S3_UPSTREAM\|CS_WIDGET_DOMAIN)$` | 同左（overlay 覆盖基线三变量 filter） | ✅ |
| `imboy_nginx.depends_on` | backend / admin / widget / livekit | backend / admin / livekit + widget（overlay 追加，键集合一致） | ✅ |
| 差异（非 CS 面，基线自带） | garage 为核心服务、监控栈内置 profile | prod 基线自有差异（监控栈、sales-policy 等），与本 overlay 无关 | — |

复验命令：

```bash
cd deploy
docker compose --env-file .env.example -f docker-compose.community.yml config > /tmp/cs_a.yaml
docker compose --env-file .env.example \
  -f docker-compose.prod.yml -f docker-compose.prod-cs.yml config > /tmp/cs_b.yaml
# 对比两文件中 services.imboy_widget 与 services.imboy_nginx 的 CS 相关字段
```

入仓实机复核记录（2026-09-27，docker compose v5.5.1；下述命令即上表证据的
可复现形态，`<WT>` 为本 overlay 所在仓根）：

```bash
# 1) 语法/插值预演（A05）
$ docker compose --env-file .env.example \
    -f docker-compose.prod.yml -f <WT>/deploy/docker-compose.prod-cs.yml config -q
$ echo $?                       # => 0

# 2) 渲染两栈为 JSON，逐字段比对（A06）
$ docker compose --env-file .env.example -f docker-compose.community.yml \
    config --format json > /tmp/cs_a.json
$ docker compose --env-file .env.example \
    -f docker-compose.prod.yml -f <WT>/deploy/docker-compose.prod-cs.yml \
    config --format json > /tmp/cs_b.json
$ diff <(jq -S '.services.imboy_widget' /tmp/cs_a.json) \
       <(jq -S '.services.imboy_widget' /tmp/cs_b.json) && echo WIDGET-IDENTICAL
WIDGET-IDENTICAL                # diff 空 = imboy_widget 渲染后逐字节等价
$ diff <(jq -S '.services.imboy_nginx.environment | {CS_WIDGET_DOMAIN,NGINX_ENVSUBST_FILTER}' /tmp/cs_a.json) \
       <(jq -S '.services.imboy_nginx.environment | {CS_WIDGET_DOMAIN,NGINX_ENVSUBST_FILTER}' /tmp/cs_b.json) \
    && echo NGINX-CS-KEYS-IDENTICAL
NGINX-CS-KEYS-IDENTICAL
$ diff <(jq -S '.services.imboy_nginx.depends_on | keys' /tmp/cs_a.json) \
       <(jq -S '.services.imboy_nginx.depends_on | keys' /tmp/cs_b.json) \
    && echo DEPENDS_ON_KEYS_IDENTICAL
DEPENDS_ON_KEYS_IDENTICAL       # 两侧同为 backend/admin/livekit/widget 四键
```
