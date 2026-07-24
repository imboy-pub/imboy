# imboy 安全加固指南

> 版本：v2.x | 最后更新：2026-06-30
> 适用场景：生产环境首次部署、版本升级前、等保测评前
> 关联文档：`docs/guides/operations/security.md`、`docs/compliance/mlps2-checklist.md`、`config/sys.config.example`

本指南提供可执行的操作步骤，不是概念描述。按清单逐项执行，完成后打勾。

---

## 一、部署前安全检查清单（10 项）

在执行 `docker compose up` 或 `make rel` 之前，逐项确认：

```
[ ] 1. 所有 CHANGE_ME_* 占位符已替换（见第二节）
[ ] 2. config/sys.config 或 .env.deploy 已从 Git 排除（.gitignore 包含 sys.*.config 和 .env.deploy）
[ ] 3. 防火墙已配置，仅开放 80/443（见第三节）
[ ] 4. 数据库仅在内网监听，未绑定 0.0.0.0
[ ] 5. Garage S3 public bucket 不含敏感文件（仅 avatar/thumbnail）
[ ] 6. nginx 已配置 TLS 1.2+，禁用 SSLv3/TLS 1.0/1.1
[ ] 7. IMBOY_API_AUTH_SWITCH 确认为 on
[ ] 8. Prometheus /metrics 端点已被 nginx 限制为内网访问
[ ] 9. Erlang 节点 cookie 已设置为随机值（非默认 erlang）
[ ] 10. 管理后台默认 admin 账号密码已修改（非 admin/123456）
```

执行检查命令：

```bash
# 检查配置文件中是否残留占位符
grep -r "CHANGE_ME" /etc/imboy/ && echo "⚠️  发现未替换占位符" || echo "✅ 无占位符"

# 检查防火墙状态
ufw status numbered

# 检查 PG 监听地址（不应含 0.0.0.0）
ss -tlnp | grep 5432

# 检查 Garage 是否监听外网（仅 127.0.0.1:3900/3902 合法）
ss -tlnp | grep -E '3900|3902'

# 检查 metrics 端口是否对外暴露（不应在此出现）
curl -s -o /dev/null -w "%{http_code}" https://your-domain.com/metrics
```

---

## 二、必须修改的默认配置

### 2.1 后端密钥（`config/sys.config` 或环境变量）

以下字段**每个生产环境独立生成**，绝不复用或与开发环境共享：

| 配置项 | 生成命令 | 最低长度 | 说明 |
|--------|----------|----------|------|
| `IMBOY_JWT_KEY` | `openssl rand -base64 32` | 32 字节 | JWT HS256 签名密钥；修改后所有在线 token 立即失效 |
| `IMBOY_POSTGRE_AES_KEY` | `openssl rand -base64 32` | 32 字节 | 数据库字段级 AES-256 加密密钥；**修改后历史密文不可解**，仅新环境初始化时设置 |
| `IMBOY_SOLIDIFIED_KEY` | `openssl rand -base64 32` | 32 字节 | 客户端 init 接口响应加密密钥 |
| `IMBOY_SOLIDIFIED_KEY_IV` | `openssl rand -base64 16` | 16 字节 | 对应 AES IV |
| `IMBOY_PASSWORD_SALT` | `openssl rand -hex 16` | 32 hex chars | 历史 MD5 密码盐；**投产后不可修改**（修改导致所有用户无法登录） |
| `IMBOY_ADM_COOKIE_SECRET` | `openssl rand -hex 32` | 64 hex chars | 管理后台 Cookie 签名；修改后所有管理员需重新登录 |

**执行示例**：

```bash
# 一次性生成所有密钥并写入 .env.deploy（不要 echo 到终端）
{
  echo "IMBOY_JWT_KEY=$(openssl rand -base64 32)"
  echo "IMBOY_POSTGRE_AES_KEY=$(openssl rand -base64 32)"
  echo "IMBOY_SOLIDIFIED_KEY=$(openssl rand -base64 32)"
  echo "IMBOY_SOLIDIFIED_KEY_IV=$(openssl rand -base64 16)"
  echo "IMBOY_PASSWORD_SALT=$(openssl rand -hex 16)"
  echo "IMBOY_ADM_COOKIE_SECRET=$(openssl rand -hex 32)"
} >> /etc/imboy/.env.deploy

chmod 600 /etc/imboy/.env.deploy
chown root:root /etc/imboy/.env.deploy
```

### 2.2 RSA 登录密钥对

```bash
# 生成 2048-bit RSA 密钥对（放在加密目录）
mkdir -p /etc/imboy/keys && chmod 700 /etc/imboy/keys

openssl genrsa -out /etc/imboy/keys/login_rsa_priv.pem 2048
openssl rsa -in /etc/imboy/keys/login_rsa_priv.pem \
            -pubout -out /etc/imboy/keys/login_rsa_pub.pem

chmod 600 /etc/imboy/keys/login_rsa_priv.pem
chmod 644 /etc/imboy/keys/login_rsa_pub.pem
chown root:root /etc/imboy/keys/*
```

配置引用：

```
IMBOY_LOGIN_RSA_PRIV_KEY_FILE=/etc/imboy/keys/login_rsa_priv.pem
IMBOY_LOGIN_RSA_PUB_KEY_FILE=/etc/imboy/keys/login_rsa_pub.pem
```

### 2.3 数据库密码

```bash
# 创建专用应用账号（不使用 postgres 超级用户）
psql -U postgres <<EOF
CREATE USER imboy_app WITH PASSWORD '$(openssl rand -base64 20)';
GRANT CONNECT ON DATABASE imboy TO imboy_app;
GRANT USAGE ON SCHEMA public TO imboy_app;
GRANT SELECT, INSERT, UPDATE, DELETE ON ALL TABLES IN SCHEMA public TO imboy_app;
GRANT USAGE, SELECT ON ALL SEQUENCES IN SCHEMA public TO imboy_app;
ALTER DEFAULT PRIVILEGES IN SCHEMA public
  GRANT SELECT, INSERT, UPDATE, DELETE ON TABLES TO imboy_app;
EOF
```

### 2.4 管理后台默认管理员

首次部署后立即修改默认账号密码：

```bash
# 通过 Erlang remote console 重置密码
_rel/imboy/bin/imboy remote_console

# 在 Erlang shell 中执行
Pwd = <<"NewStr0ngP@ss!">>,
% 密码存储规则：前端 MD5 → 后端再次加盐存储
HashedPwd = crypto:hash(md5, <<"salt_from_env", Pwd/binary>>),
adm_user_repo:reset_password(1, HashedPwd).
```

### 2.5 Erlang 节点 Cookie

```bash
# 默认 cookie 为 imboycookie，生产必须修改
NEW_COOKIE=$(openssl rand -hex 16)
echo "IMBOY_CTL_COOKIE=${NEW_COOKIE}" >> /etc/imboy/.env.deploy

# 同时更新 config/sys.config 中的 {distributed_cookie, "..."} 项
# 多节点环境所有节点必须使用相同 cookie
```

---

## 三、网络隔离方案

### 3.1 端口规则（最小化暴露）

```bash
# 清空默认规则，从零开始
ufw --force reset
ufw default deny incoming
ufw default allow outgoing

# 必须开放
ufw allow 22/tcp comment 'SSH（建议改非标准端口）'
ufw allow 80/tcp comment 'HTTP（redirect to HTTPS）'
ufw allow 443/tcp comment 'HTTPS + WSS'

# 仅允许内网管理（根据实际 IP 段修改）
ufw allow from 10.0.0.0/8 to any port 5432 comment 'PostgreSQL 内网'
ufw allow from 127.0.0.1 to any port 9800 comment 'imboy app（nginx 转发）'
ufw allow from 127.0.0.1 to any port 3900 comment 'Garage API（内网）'
ufw allow from 127.0.0.1 to any port 3902 comment 'Garage Web（内网）'

# Erlang 集群端口（仅多节点部署需要）
ufw allow from 10.0.0.0/8 to any port 4369 comment 'EPMD'
ufw allow from 10.0.0.0/8 to any proto tcp port 9100:9200 comment 'Erlang dist'

# 监控内网访问（Prometheus 拉取）
ufw allow from 10.0.0.0/8 to any port 9100 comment 'node_exporter'

ufw --force enable
ufw status numbered
```

### 3.2 nginx 安全头

在 `deploy/nginx/nginx.conf` 的 `http` 或 `server` 块添加：

```nginx
# 禁止直接访问 metrics
location /metrics {
    allow 10.0.0.0/8;
    allow 127.0.0.1;
    deny all;
    proxy_pass http://imboy:9800;
}

# 安全响应头
add_header Strict-Transport-Security "max-age=31536000; includeSubDomains; preload" always;
add_header X-Content-Type-Options "nosniff" always;
add_header X-Frame-Options "DENY" always;
add_header Referrer-Policy "strict-origin-when-cross-origin" always;
add_header Permissions-Policy "camera=(), microphone=(), geolocation=()" always;

# 隐藏服务器版本
server_tokens off;

# TLS 配置
ssl_protocols TLSv1.2 TLSv1.3;
ssl_ciphers ECDHE-ECDSA-AES128-GCM-SHA256:ECDHE-RSA-AES128-GCM-SHA256:ECDHE-ECDSA-AES256-GCM-SHA384:ECDHE-RSA-AES256-GCM-SHA384:ECDHE-ECDSA-CHACHA20-POLY1305:ECDHE-RSA-CHACHA20-POLY1305;
ssl_prefer_server_ciphers off;
ssl_session_cache shared:SSL:10m;
ssl_session_timeout 1d;
ssl_session_tickets off;
```

---

## 四、数据加密配置

### 4.1 传输加密

| 链路 | 加密方式 | 配置位置 | 验证命令 |
|------|----------|----------|----------|
| 客户端 → nginx | TLS 1.2/1.3 | `deploy/nginx/nginx.conf` | `openssl s_client -connect your-domain.com:443 -tls1_1`（应失败） |
| nginx → imboy | 明文（内网） | 不需要加密 | `ss -tlnp \| grep 9800`（仅 127.0.0.1）|
| imboy → PostgreSQL | 内网（可选 SSL） | `IMBOY_PG_SSL=true` | `psql "sslmode=require host=..."` |
| imboy → Garage S3 | 内网（可选 HTTPS） | `GARAGE_ENDPOINT` 配置 | 请求头 `X-Forwarded-Proto` |
| Erlang 节点间 | Cookie 认证（明文，内网）| `distributed_cookie` | [三级] 需配置 SSL dist |

### 4.2 静态数据加密

**数据库字段级加密**（已实现）：

- 加密函数：`src/lib/elib_cipher.erl` → `encrypt/1`、`decrypt/1`
- 加密字段：`msg_c2c.payload`、`config.value`、`user.info`、`collect.info`
- 密钥来源：`IMBOY_POSTGRE_AES_KEY`（启动时注入，不存数据库）

**Garage S3 存储**：

- private bucket：仅通过 presign URL 访问，有效期 600 秒
- public bucket：仅存 avatar/thumbnail 等非敏感资源
- 服务端存储加密：Garage 支持 SSE（取决于底层存储），建议在 `garage.toml` 配置

---

## 五、日志审计配置

### 5.1 lager 日志级别与保留

```erlang
%% config/sys.config 生产推荐配置
{lager, [
    {handlers, [
        {lager_file_backend, [
            {file, "/var/log/imboy/error.log"},
            {level, error},
            {size, 104857600},  %% 100MB 轮转
            {date, "$D0"},      %% 每天轮转
            {count, 90}         %% 保留 90 天（等保要求 6 个月，配合 Loki）
        ]},
        {lager_file_backend, [
            {file, "/var/log/imboy/info.log"},
            {level, info},
            {size, 104857600},
            {date, "$D0"},
            {count, 30}
        ]}
    ]},
    {crash_log, "/var/log/imboy/crash.log"}
]}
```

### 5.2 Loki 日志保留配置

在 `deploy/loki/loki-config.yml` 中设置：

```yaml
limits_config:
  retention_period: 180d   # 6 个月，等保要求

compactor:
  working_directory: /loki/compactor
  shared_store: filesystem
  retention_enabled: true
  retention_delete_delay: 2h
  retention_delete_worker_count: 150
```

### 5.3 关键审计事件清单

确认以下事件被记录（Grafana → Explore → 搜索 action 字段）：

| 事件 | log 关键字 | 代码位置 |
|------|-----------|----------|
| 用户登录成功 | `action=login_success` | `passport_logic.erl` |
| 用户登录失败 | `action=login_failed` | `passport_logic.erl` |
| JWT token 刷新 | `action=token_refresh` | `token_logic.erl` |
| 账号注销申请 | `action=apply_logout` | `user_logic.erl` |
| 管理员后台登录 | `action=adm_login` | `adm_passport_handler.erl` |
| 管理员修改用户 | `action=adm_user_update` | `adm_user_handler.erl` |
| E2EE 密钥上传 | `action=e2ee_key_upload` | `e2ee_logic.erl` |
| 文件上传（presign）| `action=attachment_presign` | `attachment_handler.erl` |

---

## 六、定期安全任务

### 每周

```bash
#!/usr/bin/env bash
# /etc/cron.weekly/imboy-security-weekly

# 1. 检查 SSL 证书有效期（提前 30 天告警）
EXPIRY=$(openssl s_client -connect your-domain.com:443 -servername your-domain.com \
         2>/dev/null | openssl x509 -noout -enddate | cut -d= -f2)
EXPIRY_TS=$(date -d "$EXPIRY" +%s 2>/dev/null || date -j -f "%b %d %T %Y %Z" "$EXPIRY" +%s)
NOW_TS=$(date +%s)
DAYS_LEFT=$(( (EXPIRY_TS - NOW_TS) / 86400 ))
[ $DAYS_LEFT -lt 30 ] && echo "⚠️ SSL 证书还有 ${DAYS_LEFT} 天到期" | mail -s "imboy SSL 告警" ops@example.com

# 2. 检查未授权 SSH 登录（过去 7 天）
grep "Failed password" /var/log/auth.log | \
  awk '{print $11}' | sort | uniq -c | sort -rn | head -10

# 3. 检查 Docker 镜像更新（CVE 快速响应）
docker images --format "table {{.Repository}}\t{{.Tag}}\t{{.CreatedAt}}" | grep imboy
```

### 每月

```bash
#!/usr/bin/env bash
# /etc/cron.monthly/imboy-security-monthly

# 1. 轮转 JWT_KEY（需配合滚动部署，旧 token 10 分钟内自然过期）
# 注意：轮转前通知用户或选择低峰期
NEW_JWT_KEY=$(openssl rand -base64 32)
sed -i "s/^IMBOY_JWT_KEY=.*/IMBOY_JWT_KEY=${NEW_JWT_KEY}/" /etc/imboy/.env.deploy
# 重启服务热加载配置（或通过 config_ds:local_reload() ）

# 2. 审查后台账号
# 通过 imboy admin API 导出账号列表，确认无离职人员账号
curl -s -H "Cookie: ..." https://admin.your-domain.com/adm/admin/user/list | \
  jq '.data[] | select(.last_login < (now - 7776000)) | {id, username, last_login}'
  # 上述命令列出 90 天未登录的后台账号，逐一确认是否应停用

# 3. 检查 imboy 依赖 CVE
# Erlang 依赖
cd /opt/imboy && cat rebar.lock | grep -E 'hex|github' | awk '{print $1}' > /tmp/imboy_deps.txt
# 对照 hex.pm advisory 或使用 mix_audit 等工具

# 4. 检查 PG 慢查询（超过 1s）
psql -U imboy_app -d imboy -c "
  SELECT query, mean_exec_time, calls
  FROM pg_stat_statements
  WHERE mean_exec_time > 1000
  ORDER BY mean_exec_time DESC
  LIMIT 10;
"
```

### 每季度

```bash
# 1. 全量漏洞扫描（选其一）
# - 使用 Nessus / OpenVAS 扫描生产 IP
# - 使用 Trivy 扫描 Docker 镜像
trivy image imboy:latest --severity HIGH,CRITICAL

# 2. 渗透测试（建议由第三方执行）
# 重点测试：SQL 注入、认证绕过、WebSocket 消息伪造、IDOR

# 3. 备份恢复演练（参照 RESTORE-DRILL 记录格式）
# 文档：docs/guides/operations/deployment/RESTORE-DRILL-2026-06.md
# 步骤：恢复到测试环境 → 验证数据完整性 → 记录 RTO

# 4. 等保自查（参照 docs/compliance/mlps2-checklist.md）
# 更新清单状态，准备测评材料
```

---

## 七、应急响应流程

### 7.1 安全事件分级

| 级别 | 描述 | 示例 | 响应时限 |
|------|------|------|---------|
| P0 严重 | 数据泄露、服务中断、权限被攻破 | 数据库被脱库、后台被入侵 | 立即（15 分钟内开始处置） |
| P1 高危 | 高频异常登录、密钥可能泄露 | 大量登录失败告警、密钥出现在 Git 历史 | 1 小时内开始处置 |
| P2 中危 | 功能异常、局部安全缺陷 | 特定接口被绕过、XSS 发现 | 当天处置 |
| P3 低危 | 安全配置不合规、依赖 CVE | 证书临过期、依赖高危漏洞 | 1 周内处置 |

### 7.2 P0/P1 应急步骤

```bash
# ============================================================
# STEP 1：隔离（立即执行，不超过 5 分钟）
# ============================================================

# 方案 A：切断外网流量（保留内网排查能力）
ufw deny in on eth0

# 方案 B：完全停服（数据已泄露时）
docker compose -f /opt/imboy/docker-compose.prod.yml down

# ============================================================
# STEP 2：取证（隔离后 15 分钟内）
# ============================================================

# 保存当前连接（取证后再清理）
ss -tnp > /tmp/connections_$(date +%Y%m%d_%H%M%S).txt
who > /tmp/logins_$(date +%Y%m%d_%H%M%S).txt
ps aux > /tmp/processes_$(date +%Y%m%d_%H%M%S).txt

# 打包最近 24h 的 imboy 日志
tar czf /tmp/imboy_logs_$(date +%Y%m%d).tar.gz /var/log/imboy/

# 保存数据库最近操作（如 PG 审计已启用）
psql -U postgres -d imboy -c "
  SELECT pid, usename, application_name, client_addr, state, query, query_start
  FROM pg_stat_activity
  WHERE state != 'idle';" > /tmp/pg_activity_$(date +%Y%m%d_%H%M%S).txt

# ============================================================
# STEP 3：评估影响范围
# ============================================================
# 确认：哪些数据可能受影响？影响了多少用户？
# 查询最近异常 API 调用（Grafana / Loki）：
#   {job="imboy"} |= "error" | json | action != "" | line_format "{{.ts}} {{.uid}} {{.action}} {{.ip}}"

# ============================================================
# STEP 4：修复与恢复
# ============================================================

# 4.1 轮转所有密钥（即使只怀疑泄露）
bash /opt/imboy/scripts/rotate_secrets.sh  # 见下方脚本

# 4.2 强制下线所有 token（修改 jwt_key 后自动实现）

# 4.3 重置可疑账号
# via Erlang console:
#   adm_user_repo:force_logout(SuspectUid).

# 4.4 重启服务（使用新密钥）
docker compose -f /opt/imboy/docker-compose.prod.yml up -d

# 4.5 逐步恢复外网访问
ufw delete deny in on eth0

# ============================================================
# STEP 5：通报与复盘
# ============================================================
# 30 分钟内：通知客户安全联系人和 imboy 技术支持
# 24 小时内：提交事件报告（时间线、影响范围、处置措施）
# 72 小时内：完成根因分析，输出改进措施
# 如涉及个人信息泄露，根据《个人信息保护法》第 57 条，须在 72 小时内向网信部门报告
```

### 7.3 密钥轮转脚本

```bash
#!/usr/bin/env bash
# /opt/imboy/scripts/rotate_secrets.sh
# 用途：应急密钥轮转，执行前确认已备份数据库

set -euo pipefail

ENV_FILE=/etc/imboy/.env.deploy
BACKUP_FILE=/etc/imboy/.env.deploy.bak.$(date +%Y%m%d_%H%M%S)

echo "[$(date)] 开始密钥轮转..."
cp "$ENV_FILE" "$BACKUP_FILE"
echo "[$(date)] 已备份原密钥至 $BACKUP_FILE"

# 注意：postgre_aes_key 轮转需要数据迁移，此处仅轮转其他密钥
sed -i "s/^IMBOY_JWT_KEY=.*/IMBOY_JWT_KEY=$(openssl rand -base64 32)/" "$ENV_FILE"
sed -i "s/^IMBOY_SOLIDIFIED_KEY=.*/IMBOY_SOLIDIFIED_KEY=$(openssl rand -base64 32)/" "$ENV_FILE"
sed -i "s/^IMBOY_SOLIDIFIED_KEY_IV=.*/IMBOY_SOLIDIFIED_KEY_IV=$(openssl rand -base64 16)/" "$ENV_FILE"
sed -i "s/^IMBOY_ADM_COOKIE_SECRET=.*/IMBOY_ADM_COOKIE_SECRET=$(openssl rand -hex 32)/" "$ENV_FILE"

echo "[$(date)] 密钥轮转完成。⚠️  IMBOY_POSTGRE_AES_KEY 未轮转（需数据迁移，请单独处理）"
echo "[$(date)] 请重启 imboy 服务使新密钥生效"
```
