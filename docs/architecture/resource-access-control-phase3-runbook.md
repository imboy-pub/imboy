# 资源访问控制 · 阶段三生产配置 Runbook / Phase 3 Production Runbook

> **状态 / Status**: ⚠️ 待人工执行 / Pending manual execution（命令就绪，**未执行任何生产操作**）
> **创建 / Created**: 2026-06-20
> **前置 / Prereq**: 阶段一（后端）已完成；阶段二（客户端）已完成并通过绿灯门（见 [resource-access-control-progress.md](./resource-access-control-progress.md)）
> **权威设计 / Design**: [resource-access-control.md](./resource-access-control.md) §4、§9、§10.3、§10.4

---

## ⚠️ 执行须知 / Read First

- 本 runbook 的所有命令均**面向生产**，涉及 Garage、nginx、数据库、域名下线——**不可逆 + 影响真实用户**，必须由你本人逐条审阅后执行。
- 建议在**低峰期**执行，并先在一台预发/测试节点演练。
- 每步均给出**验证**与**回滚**。出现任何异常先回滚再排查。
- 执行前确认备份：`bash imboy/scripts/backup_pg.sh`（数据库）、Garage 数据卷快照。

---

## 步骤总览 / Overview

| # | 步骤 | 影响 | 可回滚 |
|---|------|------|--------|
| 1 | Garage 开启 website 模式 + `imboy-public` 公开读 | 新增公开读端点 | ✅ |
| 2 | nginx 配置 `s3.imboy.pub` 公开路由（公开前缀→web 端点，其余→3900 私有） | 新增/改 vhost | ✅ |
| 3 | 后端下发 `public_base_url`（已在阶段一代码就绪，需重启生效） | 重启后端 | ✅ |
| 4 | 旧 `user.avatar`（含 `i.imboy.pub`/`a.imboy.pub` 完整 URL）批量置空 | 用户需重传头像 | ⚠️ 需备份 |
| 5 | 下线 `i.imboy.pub` / `a.imboy.pub`（go-fastdfs） | 旧域名停服 | ✅（保留 vhost 注释） |
| 6 | 全链路验证 | — | — |

---

## 步骤 1 · Garage website 模式 + imboy-public 公开读

> 目标：让 `imboy-public` 桶可经 Garage web 端点（website 模式）公开读，nginx 再代理到 `s3.imboy.pub`。

### 1.1 编辑 `/etc/garage.toml`，新增 `[s3_web]`（端口按实际，示例 3902）

```toml
[s3_web]
bind_addr = "127.0.0.1:3902"
root_domain = ".s3.imboy.pub"
index = "index.html"
```

> ⚠️ 仅新增 `[s3_web]` 块，**不要改动既有 `[s3_api]`（3900，私有 presign 用）**。

### 1.2 重启 Garage

```bash
# docker compose 部署（推荐）
cd imboy/deploy && docker compose -f docker-compose.prod.yml restart garage
# 或裸进程（确认无残留旁路进程占用端口，见 project_garage_rogue_native_process 教训）
sudo lsof -i :3900 -i :3902   # 执行前确认端口归属正确的 garage 进程
```

### 1.3 将 `imboy-public` 桶设为公开读（website）

```bash
# 进入 garage 容器或使用 garage CLI
garage bucket website --allow imboy-public
# 确认 imboy（私有桶）未开公开读
garage bucket info imboy | grep -i website   # 期望：未启用
garage bucket info imboy-public | grep -i website   # 期望：已启用
```

### 验证

```bash
# 上传一个测试对象到 imboy-public（用现有 presign 或 garage CLI），然后经 web 端点直读
curl -I http://127.0.0.1:3902/<test-object-key>   # 期望 200
```

### 回滚

```bash
garage bucket website --deny imboy-public
# 注释 /etc/garage.toml 的 [s3_web] 块并 restart garage
```

---

## 步骤 1.5 · ⚠️ garage.endpoint 必须指向真机可达的公网 S3 host（最易踩坑）

> **真机已暴露此问题**：presign 返回的 `put_url` host **就是后端 `garage.endpoint`**（`elib_s3_sign:presign_url` 用 Endpoint 拼 `Endpoint/Bucket/Key?签名`）。生产默认是 `http://127.0.0.1:3900`（内网），真机直传**无法连接**，表现为"上传地址不是 s3.imboy.pub / 上传失败"。
>
> **S3 v4 签名把 host 计入签名**，因此**不能靠 nginx 改写 Host** 来纠正——签名所用的 endpoint 必须**等于**真机实际连接的公网 host，否则 `SignatureDoesNotMatch`。

### 必须配置（二选一，与 nginx 路由匹配）

**方案 A（推荐）：S3 API 与公开读共用 `s3.imboy.pub`，靠 `X-Amz-` 查询参数区分**

```erlang
%% sys.config 或环境变量 IMBOY_GARAGE_ENDPOINT
endpoint => <<"https://s3.imboy.pub">>          %% 真机直传 PUT/GET 的公网 S3 API host
public_base_url => <<"https://s3.imboy.pub">>   %% 公开读基址（website）
```

- presigned 上传：`https://s3.imboy.pub/imboy-public/u<uid>/avatar/x.jpg?X-Amz-Algorithm=...`（path-style，带 bucket 段 + X-Amz 签名）
- 公开读：`https://s3.imboy.pub/u<uid>/avatar/x.jpg`（无 bucket 段、无 X-Amz）
- nginx 在 `s3.imboy.pub` 上**按 `X-Amz-` 查询参数存在与否分流**：有 → 3900（S3 API，签名自鉴权）；无 → 3902（website 公开读）。见步骤 2.2。

**方案 B：S3 API 用独立子域（如 `s3api.imboy.pub`）→ 3900，`s3.imboy.pub` 仅公开读 → 3902**

```erlang
endpoint => <<"https://s3api.imboy.pub">>
public_base_url => <<"https://s3.imboy.pub">>
```
- 需为 `s3api.imboy.pub` 配证书 + nginx → 3900。职责更清晰，但多一个子域。

### 验证

```bash
# 取一次真机 presign 的 put_url，确认 host 是公网 host（非 127.0.0.1）
imboy/scripts/... rpc elib_oss presign_put_for_key '<<"imboy-public/u1/avatar/test.jpg">>' ...
# 用该 put_url 裸 PUT 一个文件，期望 200（而非连接拒绝 / SignatureDoesNotMatch）
```

> ⚠️ 此前所有 presign 上传（含聊天图片）若在生产从未真机成功，根因很可能就是 endpoint 仍为内网默认。这是**所有 scope 上传**的共性前置，不止头像。

---

## 步骤 2 · nginx `s3.imboy.pub` 公开路由

> 目标：`s3.imboy.pub` 上，公开资源前缀走 Garage web 端点（公开读，可挂 CDN）；其余仍走 3900（私有，靠 presign 签名鉴权）。
>
> ⚠️ **关键约束**：object_key 第一段是 `u<Uid>/`，public 与受限资源**前缀形态相同**，无法用 URL 前缀区分公私。因此公开读由**物理分桶**保证：`imboy-public` 桶经 web 端点（3902）整体公开，`imboy` 桶经 3900 仅签名可读。nginx 用**不同 location / 不同 server** 分流到两个上游，而非靠路径前缀判断归属。

### 2.1 现状盘点（执行前）

```bash
ls -la /etc/nginx/conf.d/ /etc/nginx/sites-enabled/ 2>/dev/null
grep -rnE "i\.imboy\.pub|a\.imboy\.pub|s3\.imboy\.pub|3900|3902" /etc/nginx/ 2>/dev/null
```

### 2.2 `s3.imboy.pub` vhost（示例，按现有证书/路径调整）

```nginx
server {
    listen 443 ssl http2;
    server_name s3.imboy.pub;

    ssl_certificate     /etc/letsencrypt/live/s3.imboy.pub/fullchain.pem;
    ssl_certificate_key /etc/letsencrypt/live/s3.imboy.pub/privkey.pem;

    # 方案 A：同域按 X-Amz 查询参数分流（与步骤 1.5 方案 A 配套）
    #
    # 带 X-Amz-Algorithm 的是 presigned S3 API 请求（PUT 直传 / 私有 GET）→ 3900，
    # 由 presign 签名自鉴权；签名 host 必须 == garage.endpoint == s3.imboy.pub。
    location / {
        if ($arg_X-Amz-Algorithm != "") {
            proxy_pass http://127.0.0.1:3900;
            break;
        }
        # 无 X-Amz：公开读（imboy-public website 模式）→ 3902，可挂 CDN/长缓存
        proxy_pass http://127.0.0.1:3902;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        add_header Cache-Control "public, max-age=86400";
    }
}
```

> ⚠️ **关于 `if` + `proxy_pass`**：nginx 的 `if` 内 `proxy_pass` 有已知坑（"if is evil"）。生产更稳妥可用 `map $arg_X-Amz-Algorithm $s3_upstream { "" "127.0.0.1:3902"; default "127.0.0.1:3900"; }` 再 `proxy_pass http://$s3_upstream;`，并把 `proxy_set_header`/缓存头放到 server 级。上面示例仅示意分流意图，落地请用 `map` 写法。
>
> **方案 B（独立子域）**：若用 `s3api.imboy.pub`→3900 + `s3.imboy.pub`→3902，则两个 server 各自单一 `proxy_pass`，无需 X-Amz 判断，最简单稳妥。前提是 `garage.endpoint=https://s3api.imboy.pub`（步骤 1.5 方案 B）。

### 验证

```bash
nginx -t                      # 配置语法
systemctl reload nginx
curl -I https://s3.imboy.pub/<public-test-object-key>   # 期望 200，public 资源
```

### 回滚

```bash
# 还原修改前的 vhost 备份，reload
cp /etc/nginx/conf.d/s3.imboy.pub.conf.bak /etc/nginx/conf.d/s3.imboy.pub.conf
nginx -t && systemctl reload nginx
```

---

## 步骤 3 · 后端下发 public_base_url（重启生效）

> 阶段一/二代码已就绪：`index_handler:api_init` 已在 `/v1/init` 下发 `public_base_url`（取 `elib_oss:public_base_url/0`，默认 `https://s3.imboy.pub`）。客户端 `Env.publicBaseUrl` 优先用下发值，缺省回退 `https://s3.imboy.pub`。

### 3.1 确认 `sys.config` 的 garage 块

```erlang
%% imboy/config/sys.config（生产）
{garage, #{
    ...
    public_bucket   => <<"imboy-public">>,
    public_base_url => <<"https://s3.imboy.pub">>,
    ...
}}
```

> 若生产用 `IMBOY_*` 环境变量覆盖 garage 配置，注意现有 `override_garage/1` **未覆盖** `public_base_url`（仅覆盖 endpoint/bucket/access_key/secret_key）。如需用环境变量管理 `public_base_url`，需扩展 `imboy_env:override_garage/1`；否则以 `sys.config` 值为准。

### 3.2 重新编译并发布/重启后端

```bash
cd imboy
make compile                       # 已验证编译通过
# 生产发布按既有流程（imboy/scripts/deploy.sh 或滚动重启节点）
# 或热加载（remote_console 内）：config_ds:local_reload(), lm()
```

### 验证

```bash
# 取一个用户的 /v1/init 响应，确认 payload 含 public_base_url（注意响应是 AES 加密的 res 字段，
# 需在客户端或测试工具解密后查看），或直接 RPC：
imboy/scripts/... rpc elib_oss public_base_url   # 期望 <<"https://s3.imboy.pub">>
```

### 回滚

- 后端无破坏性变更；回滚即回退到不下发 `public_base_url` 的旧版本（客户端会用内置默认 `https://s3.imboy.pub`，仍可工作）。

---

## 步骤 4 · 旧 user.avatar 批量置空（引导重传）

> 目标：旧头像存的是 `i.imboy.pub`/`a.imboy.pub` 完整 URL（go-fastdfs），新链路无法公开直读。按设计 §10.4「旧图不迁移，用户重传」，将这类 avatar 置空 → 客户端渲染默认头像 → 引导用户重传（重传后即为 public object_key）。

### 4.1 ⚠️ 先备份

```bash
bash imboy/scripts/backup_pg.sh
# 或针对性导出：
psql "$IMBOY_PG_DSN" -c "\copy (SELECT id, avatar FROM \"user\" WHERE avatar LIKE 'http%') TO '/tmp/avatar_backup_20260620.csv' CSV HEADER"
```

### 4.2 影响评估（先查计数，不改数据）

```sql
-- 受影响行数（含 i.imboy.pub / a.imboy.pub / 任意 http 完整 URL）
SELECT count(*) FROM "user"
WHERE avatar LIKE 'http://%' OR avatar LIKE 'https://%';

-- 仅 go-fastdfs 旧域名（更保守）
SELECT count(*) FROM "user"
WHERE avatar LIKE '%i.imboy.pub%' OR avatar LIKE '%a.imboy.pub%';
```

### 4.3 批量置空（确认计数无误后执行）

> 客户端 `avatarImageProvider` 对空/`def_avatar.png` 渲染默认头像；对 object_key 走公开直读；对残留完整 URL 走旧授权兜底。置空后即触发默认头像。

```sql
-- 方案 A（保守）：仅清 go-fastdfs 旧域名头像
UPDATE "user"
SET avatar = ''
WHERE avatar LIKE '%i.imboy.pub%' OR avatar LIKE '%a.imboy.pub%';

-- 方案 B（彻底）：清所有完整 URL 头像（含其他历史域名）
-- UPDATE "user" SET avatar = ''
-- WHERE avatar LIKE 'http://%' OR avatar LIKE 'https://%';
```

> ⚠️ **不要**误清 object_key 形态（`u<digits>/...`）——那是新链路的有效公开头像。上面的 `LIKE 'http%'` 不会命中 object_key，安全。

### 验证

```sql
SELECT count(*) FROM "user" WHERE avatar LIKE '%i.imboy.pub%' OR avatar LIKE '%a.imboy.pub%';
-- 期望 0
```

客户端：受影响用户登录后头像显示为默认头像；重新设置头像后能被本人及他人正常看到（公开直读）。

### 回滚

```sql
-- 从备份 CSV 恢复
\copy avatar_restore (id, avatar) FROM '/tmp/avatar_backup_20260620.csv' CSV HEADER
UPDATE "user" u SET avatar = r.avatar FROM avatar_restore r WHERE u.id = r.id;
```

---

## 步骤 5 · 下线 i.imboy.pub / a.imboy.pub（go-fastdfs）

> 仅在确认新链路稳定、旧头像已置空、无残留依赖后执行。

### 5.1 确认无活跃流量

```bash
# 观察旧域名近 N 天访问日志，确认请求量降到可忽略
grep -hE "i\.imboy\.pub|a\.imboy\.pub" /var/log/nginx/access.log* | wc -l
```

### 5.2 停服（保留可回滚）

```bash
# 注释而非删除 vhost，便于回滚
mv /etc/nginx/conf.d/i.imboy.pub.conf /etc/nginx/conf.d/i.imboy.pub.conf.disabled
mv /etc/nginx/conf.d/a.imboy.pub.conf /etc/nginx/conf.d/a.imboy.pub.conf.disabled
nginx -t && systemctl reload nginx
# go-fastdfs 进程/容器停止（确认无其他用途后）
# docker compose ... stop go-fastdfs    # 若仍在编排中
```

### 验证

```bash
curl -I https://i.imboy.pub/   # 期望连接失败 / 404 / 410
```

### 回滚

```bash
mv /etc/nginx/conf.d/i.imboy.pub.conf.disabled /etc/nginx/conf.d/i.imboy.pub.conf
nginx -t && systemctl reload nginx
# 重启 go-fastdfs
```

---

## 步骤 6 · 全链路验证清单 / End-to-End Verification

执行完 1–5 后逐项核对：

- [ ] `curl -I https://s3.imboy.pub/<public-object-key>` → 200（公开直读，无需签名）
- [ ] 受限资源仍需 presign：直接 `curl` 私有桶对象（无签名）→ 403/拒绝
- [ ] 客户端 `/v1/init` 返回含 `public_base_url=https://s3.imboy.pub`
- [ ] 新用户上传头像 → A 与 B（不同账号）互相可见（公开直读，无 403）
- [ ] 旧头像用户登录 → 显示默认头像 → 重传后双方可见
- [ ] 单聊图片：仅会话双方可见（view_url 鉴权 600s）；非双方 403
- [ ] 群图片：仅群成员可见；非群成员 403
- [ ] 历史消息附件（旧完整 URL）仍可渲染（legacy view_url 路径未删）
- [ ] `i.imboy.pub`/`a.imboy.pub` 已停服
- [ ] Grafana/日志：无异常 4xx/5xx 激增

---

## 附：与阶段二客户端的对齐点 / Client Alignment

- 客户端头像直读基址来源：`/v1/init` 下发的 `public_base_url`（写入 `StorageService`），缺省回退编译期默认 `https://s3.imboy.pub`（`lib/config/env.dart` `Env.publicBaseUrl`）。
- 若生产 `public_base_url` ≠ `https://s3.imboy.pub`，**必须**确保步骤 3 的下发生效，否则客户端会用错误的默认值拼头像 URL。
- 安全建议：`public_base_url` 应为 `https://`（客户端 SSRF 门允许 http/https，但生产应强制 https，避免明文/中间人）。
