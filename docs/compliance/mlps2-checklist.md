# imboy v2.x 网络安全等级保护 2.0（MLPS 2.0）合规清单

> 版本：v2.x | 适用等级：**二级（基准）/ 三级（加固项标注）**
> 标准依据：GB/T 22239-2019《信息安全技术 网络安全等级保护基本要求》/ GA/T 1390
> 最后更新：2026-06-30
> 维护人：运维安全团队
> 关联文档：`docs/guides/operations/security.md`、`docs/guides/security/security-hardening.md`、`config/sys.config.example`

---

## 说明

本清单逐条对照 MLPS 2.0 七大控制域，标注 imboy 当前实现状态：

| 标记 | 含义 |
|------|------|
| ✅ 已实现 | 当前代码/配置已满足，注明实现位置 |
| ⚠️ 部分实现 | 有基础能力但存在缺口，注明差距 |
| ❌ 待实现 | 尚未覆盖，需额外工作 |
| N/A | 不适用于当前部署场景 |

三级专项加固项以 `[三级]` 标注，二级不做强制要求但建议实施。

---

## 控制域 1：安全通信网络

### 1.1 网络架构

| 控制点 | 要求 | 状态 | 说明 |
|--------|------|------|------|
| 1.1.1 业务网络与管理网络分离 | 生产接口与运维入口走不同端口/网段 | ✅ 已实现 | 业务端口 9800（`HTTP_PORT`），管理后台 `imboyadmin` 独立域名；后端管理接口前缀 `/adm/` 由 `adm_auth_middleware.erl` 独立鉴权 |
| 1.1.2 互联网边界部署边界防护设备 | 反向代理 / 防火墙在公网入口 | ✅ 已实现 | nginx 作反向代理（`deploy/nginx/`），ufw 控制入站端口 |
| 1.1.3 关键通信链路冗余 | 数据库/存储具备冗余路径 | ⚠️ 部分实现 | PostgreSQL 主从复制文档见 `docs/guides/operations/postgres-read-replica.md`；生产是否已部署取决于客户环境，需客户自行确认 |

### 1.2 通信传输

| 控制点 | 要求 | 状态 | 说明 |
|--------|------|------|------|
| 1.2.1 传输数据采用密码技术保护完整性 | HTTPS/TLS 加密所有公网流量 | ✅ 已实现 | nginx 强制 HTTPS；`IMBOY_API_AUTH_SWITCH=on` 时所有 API 请求须带签名（`src/api/auth_middleware.erl`） |
| 1.2.2 TLS 版本不低于 1.2 | 禁用 TLS 1.0/1.1 | ✅ 已实现 | nginx 配置 `ssl_protocols TLSv1.2 TLSv1.3`（`deploy/nginx/nginx.conf`） |
| 1.2.3 WebSocket 连接加密 | WSS 协议或由反向代理升级 | ✅ 已实现 | nginx 将 `wss://` 转发至内网 `ws://imboy:9800`；Cowboy WS handler 在 `src/api/websocket_handler.erl`；token 验证在握手阶段 |
| 1.2.4 传输数据采用密码技术保护保密性 | 关键字段额外加密 | ✅ 已实现 | 客户端 init 响应用 `solidified_key` AES 包装（`src/api/app_handler.erl`）；登录密码用 RSA-OAEP 传输（`src/lib/elib_cipher.erl`） |
| 1.2.5 [三级] 全流量双向认证 (mTLS) | 节点间通信使用证书双向验证 | ❌ 待实现 | 当前 Erlang 集群节点间通过 cookie 认证，未启用 TLS 分布连接；需配置 `erl -ssl_dist_opt` |

---

## 控制域 2：安全区域边界

### 2.1 边界防护

| 控制点 | 要求 | 状态 | 说明 |
|--------|------|------|------|
| 2.1.1 限制不必要的通信端口 | 最小化开放端口 | ✅ 已实现 | 对外仅开放 80/443；9800 仅监听 127.0.0.1（由 nginx 转发）；PG 端口 5432 仅内网可达 |
| 2.1.2 禁止未授权设备接入 | WebSocket 连接须 JWT 认证 | ✅ 已实现 | `websocket_handler.erl` 握手阶段校验 token，失败返回 401 关闭连接 |
| 2.1.3 边界处对网络流量做访问控制 | 防火墙规则管理 | ✅ 已实现 | `deploy/` 内 `preflight.sh` 包含 ufw 规则检查；生产防火墙配置见 `docs/guides/operations/deployment/day1-quickstart.md` |

### 2.2 入侵防范

| 控制点 | 要求 | 状态 | 说明 |
|--------|------|------|------|
| 2.2.1 拒绝服务攻击防护 | 接口层速率限制 | ✅ 已实现 | Refresh Token、WebSocket 握手、验证码等高频入口均有节流（`src/lib/elib_throttle.erl`）；nginx `limit_req_zone` 做外层限频 |
| 2.2.2 [三级] 入侵检测系统 | 部署 IDS/WAF | ❌ 待实现 | 当前无 WAF；建议在 nginx 前加阿里云 WAF 或 ModSecurity |
| 2.2.3 异常行为检测与告警 | 登录失败、异常 IP 告警 | ⚠️ 部分实现 | 登录失败记录到 ETS `login_attempt`（`src/logic/passport_logic.erl`）；未接入实时告警；[三级] 需对接 Prometheus alerting |

### 2.3 恶意代码防范

| 控制点 | 要求 | 状态 | 说明 |
|--------|------|------|------|
| 2.3.1 上传文件恶意内容检测 | 文件上传须病毒扫描或类型校验 | ⚠️ 部分实现 | Garage S3 presign 链路校验 MIME 类型和文件大小（`src/api/attachment_handler.erl`）；未集成 ClamAV 等病毒扫描引擎 |
| 2.3.2 [三级] 主机层防病毒 | 宿主机部署防病毒软件 | ❌ 待实现 | 取决于客户部署环境，私有化交付时须客户自行配置 |

---

## 控制域 3：安全计算环境

### 3.1 身份鉴别

| 控制点 | 要求 | 状态 | 说明 |
|--------|------|------|------|
| 3.1.1 用户身份唯一标识 | 每个账号对应唯一 TSID | ✅ 已实现 | 用户 ID 采用 TSID（64-bit，全局唯一）；`user_repo.erl` 保证用户名/手机号唯一索引 |
| 3.1.2 鉴别信息复杂度 | 密码强度要求 | ⚠️ 部分实现 | 前端有密码强度提示；后端 `passport_logic.erl` 校验最小长度（6位）；[三级] 建议增加复杂度策略（大小写+数字+特殊字符） |
| 3.1.3 登录失败处理（锁定/延迟）| 连续失败后锁定账号 | ⚠️ 部分实现 | ETS 记录失败次数，超限返回错误；未实现账号临时锁定（lockout）；需在 `passport_logic.erl` 增加锁定逻辑 |
| 3.1.4 双因素认证 [三级] | 高权限操作需 MFA | ❌ 待实现 | 当前仅密码+RSA；管理后台无 MFA；三级场景需加 TOTP/短信二次验证 |
| 3.1.5 管理后台独立认证 | 后台入口与业务接口鉴权独立 | ✅ 已实现 | `adm_auth_middleware.erl` 使用独立 Cookie（`IMBOY_ADM_COOKIE_SECRET`），与业务 JWT 完全分离 |
| 3.1.6 会话超时 | 非活跃会话自动失效 | ✅ 已实现 | Access Token 短期有效（配置 `jwt_expire`）；Refresh Token 有效期独立配置；WS 连接 token 过期后 8s 内刷新否则强制下线 |

### 3.2 访问控制

| 控制点 | 要求 | 状态 | 说明 |
|--------|------|------|------|
| 3.2.1 最小权限原则 | 用户仅能访问自身资源 | ✅ 已实现 | `auth_middleware.erl` 注入 `uid` 到请求上下文；资源 owner 校验在各 Logic 层（如 `user_logic.erl`、`group_logic.erl`） |
| 3.2.2 管理后台角色权限控制 | RBAC，按角色分配能力 | ✅ 已实现 | 后台权限矩阵通过 `settings:view`、`user:manage` 等权限点控制；`adm_auth_middleware.erl` 校验权限 |
| 3.2.3 群组访问控制 | 非成员不可读群消息 | ✅ 已实现 | `group_member_repo.erl` 成员校验；`msg_c2g_logic.erl` 发消息前验证成员身份 |
| 3.2.4 文件资源访问授权 | 附件须授权访问 | ✅ 已实现 | Garage S3 presign URL 带有效期（`src/lib/elib_garage.erl`），公开 scope 走独立 bucket；私有资源须 JWT 换 presign URL |
| 3.2.5 [三级] 强制访问控制 | 系统级 MAC | N/A | IM 平台场景以 DAC 为主；三级若需 SELinux 级别 MAC 需操作系统层面配置 |

### 3.3 安全审计

| 控制点 | 要求 | 状态 | 说明 |
|--------|------|------|------|
| 3.3.1 重要用户行为审计 | 登录/注销/敏感操作记录 | ✅ 已实现 | lager 记录所有 API 请求（`src/api/imboy_handler.erl`）；账号注销申请写入审计表（`user_repo.erl apply_logout`）；管理后台操作记录在 `adm_audit_log` |
| 3.3.2 审计记录内容完整 | 时间、用户、操作、结果 | ✅ 已实现 | 日志格式含时间戳、uid、IP、action、result_code |
| 3.3.3 审计记录不可被普通用户删除 | 日志保护 | ⚠️ 部分实现 | lager 日志写本地文件，需 OS 层面配置 append-only 或写入 Loki（`docs/guides/operations/deployment/monitoring.md`）；Loki 日志不可删是架构设计但未强制配置 |
| 3.3.4 审计记录保存不少于 6 个月 | 日志保留期 | ⚠️ 部分实现 | Loki 保留期取决于磁盘配置；生产部署须在 `deploy/loki/loki-config.yml` 中设置 `retention_period: 180d`（当前未明确设置） |
| 3.3.5 [三级] 集中审计管理平台 | 日志统一收集、告警 | ⚠️ 部分实现 | Prometheus + Grafana + Loki 栈已就绪（`docs/guides/operations/deployment/monitoring.md`）；告警规则需客户按场景补充 |

### 3.4 入侵防范（计算环境）

| 控制点 | 要求 | 状态 | 说明 |
|--------|------|------|------|
| 3.4.1 遵循最小安装原则 | 服务器只安装必需组件 | ✅ 已实现 | Docker 镜像基于精简 Alpine/OTP 基础镜像；`deploy/Dockerfile` 不含调试工具 |
| 3.4.2 关闭不必要的系统服务和端口 | 参照安全加固指南 | ✅ 已实现 | 见 `docs/guides/security/security-hardening.md` 端口最小化配置 |
| 3.4.3 SQL 注入防护 | 参数化查询 | ✅ 已实现 | 所有 SQL 经 `elib_pg` 模块参数化执行（`src/lib/elib_pg.erl`）；代码审查强制要求无字符串拼接 SQL |
| 3.4.4 代码安全加固（XSS/CSRF）| 管理后台防 XSS/CSRF | ✅ 已实现 | imboyadmin React 前端做输出转义；后台 Cookie 含 `HttpOnly`、`SameSite=Strict`（`adm_auth_middleware.erl`） |

### 3.5 数据完整性

| 控制点 | 要求 | 状态 | 说明 |
|--------|------|------|------|
| 3.5.1 传输数据完整性 | HTTPS 保证传输完整性 | ✅ 已实现 | TLS 提供完整性校验；消息投递有 ACK 确认机制（`msg_c2c_logic.erl`） |
| 3.5.2 存储数据完整性 | 数据库约束 + 事务 | ✅ 已实现 | PostgreSQL 约束、外键、事务（`elib_pg:with_tx/2`）；钱包操作强制单事务（`wallet_repo.erl reject_and_refund`） |
| 3.5.3 备份数据完整性校验 | 备份验证 | ⚠️ 部分实现 | `imboy/scripts/backup_pg.sh` 实现备份；恢复演练记录见 `docs/guides/operations/deployment/restore-drill-2026-06.md`；未实现自动完整性校验哈希 |

### 3.6 数据保密性

| 控制点 | 要求 | 状态 | 说明 |
|--------|------|------|------|
| 3.6.1 存储敏感数据加密 | 数据库字段级加密 | ✅ 已实现 | `postgre_aes_key` AES-256 加密 `payload`、`config`、`info` 等敏感字段（`src/lib/elib_cipher.erl`） |
| 3.6.2 个人信息保护 | PII 字段不明文存储 | ✅ 已实现 | 手机号等 PII 字段加密存储；用户密码存储 MD5（历史）+ 后续迁移 bcrypt |
| 3.6.3 端到端加密（可选模块）| E2EE 消息服务端不可解密 | ✅ 已实现 | E2EE 模式下服务端仅路由密文（RSA-OAEP-256 + AES-256-GCM）；`e2ee_logic.erl` 不持有私钥；启用方式见 `docs/archive/analysis/e2ee-web-activation.md` |
| 3.6.4 [三级] 静态全卷加密 | 磁盘层加密 | ❌ 待实现 | 依赖宿主机 LUKS/dm-crypt 配置；私有化交付客户需自行配置操作系统层面加密 |

---

## 控制域 4：安全管理中心

### 4.1 系统管理

| 控制点 | 要求 | 状态 | 说明 |
|--------|------|------|------|
| 4.1.1 集中配置管理 | 配置统一管理，不分散硬编码 | ✅ 已实现 | 三层配置架构：`sys.config` + 环境变量 `IMBOY_*` + 运行时 `config_ds:local_reload()`；见 `docs/architecture/config-architecture.md` |
| 4.1.2 运维账号独立 | 运维与业务账号分离 | ✅ 已实现 | 后台角色矩阵区分 admin / operator / viewer；Erlang 节点 remote_console 凭 `IMBOY_CTL_COOKIE` 访问，不使用业务账号 |
| 4.1.3 集中监控管理 | Prometheus + Grafana | ✅ 已实现 | 监控栈见 `docs/guides/operations/deployment/monitoring.md`；metrics 端点 `/metrics` 仅内网可达（nginx 配置拦截外部访问） |

### 4.2 审计管理

| 控制点 | 要求 | 状态 | 说明 |
|--------|------|------|------|
| 4.2.1 审计数据集中收集 | 日志集中存储 | ✅ 已实现 | lager → Loki 集中收集；Grafana Explore 查询历史日志 |
| 4.2.2 审计管理员与系统管理员分离 | 职责隔离 | ⚠️ 部分实现 | 后台有角色区分但审计角色未完全独立；建议增加 `audit:view` 专属权限点 |

---

## 控制域 5：安全管理制度

| 控制点 | 要求 | 状态 | 说明 |
|--------|------|------|------|
| 5.1 信息安全方针政策 | 明文安全策略文档 | ⚠️ 部分实现 | `docs/guides/operations/security.md` 覆盖技术安全基线；企业级安全方针文件（PDF/OA 系统）需客户自行建立 |
| 5.2 安全管理制度体系 | 覆盖开发、运维、应急 | ⚠️ 部分实现 | 技术层面有 `security-hardening.md`、`backup-restore.md`、`upgrade-runbook.md`；制度层面文件需结合买家组织实际编写 |
| 5.3 制度定期评审 | 每年至少评审一次 | ❌ 待实现 | 建议在 OA 系统或 Wiki 建立年度评审机制，本文档版本号随评审更新 |

---

## 控制域 6：安全管理机构

| 控制点 | 要求 | 状态 | 说明 |
|--------|------|------|------|
| 6.1 设置安全管理岗位 | 明确安全责任人 | ❌ 待实现 | 依赖客户组织架构；建议私有化交付合同中明确客户侧安全负责人 |
| 6.2 安全审批与检查机制 | 变更需安全审批 | ⚠️ 部分实现 | 代码变更通过 Git PR + 代码审查；生产变更审批流程需客户结合 ITSM 工具建立 |
| 6.3 供应链安全管理 | 第三方组件安全评估 | ⚠️ 部分实现 | `docs/guides/operations/dependencies.md` 列出核心依赖；未实现 SCA（软件成分分析）自动扫描；建议集成 Snyk 或 OWASP Dependency Check |

---

## 控制域 7：安全管理人员

| 控制点 | 要求 | 状态 | 说明 |
|--------|------|------|------|
| 7.1 人员安全培训 | 上岗前安全培训 | ❌ 待实现 | 依赖客户 HR/培训体系；建议提供 imboy 平台安全操作手册作为培训材料 |
| 7.2 外部人员安全管理 | 第三方人员访问控制 | ⚠️ 部分实现 | 技术上可通过后台角色限制第三方权限；流程层面需客户建立外部人员准入协议 |
| 7.3 离职人员权限撤销 | 离职立即停用账号 | ✅ 已实现 | 后台支持即时停用账号（`adm/admin/user_handler.erl`）；建议配合客户 HR 系统建立自动化撤权流程 |

---

## 等级差异汇总

### 二级要求（当前覆盖情况）

| 域 | 已实现 | 部分实现 | 待实现 | 合计 |
|----|--------|----------|--------|------|
| 安全通信网络 | 4 | 1 | 0 | 5 |
| 安全区域边界 | 3 | 1 | 1 | 5 |
| 安全计算环境 | 14 | 5 | 1 | 20 |
| 安全管理中心 | 4 | 1 | 0 | 5 |
| 安全管理制度 | 0 | 2 | 1 | 3 |
| 安全管理机构 | 0 | 2 | 1 | 3 |
| 安全管理人员 | 1 | 1 | 1 | 3 |
| **合计** | **26** | **13** | **5** | **44** |

### 三级额外要求（待评估项）

- `[三级]` mTLS 集群节点间加密
- `[三级]` WAF / IDS 部署
- `[三级]` 管理后台 MFA
- `[三级]` 磁盘全卷加密（宿主机层）
- `[三级]` 集中审计平台告警规则完善

---

## 证据归档清单

等保测评时需提供以下证据材料：

| 证据类型 | 文件/位置 |
|----------|-----------|
| 网络拓扑图 | `docs/guides/operations/deployment/production-architecture.md` |
| TLS 配置 | `deploy/nginx/nginx.conf` → `ssl_protocols` |
| 身份鉴别配置 | `src/api/auth_middleware.erl`、`src/adm/adm_auth_middleware.erl` |
| 访问控制矩阵 | `docs/business/edition-boundary.md` |
| 审计日志样本 | Loki / Grafana 截图（测评前 30 天） |
| 密钥管理配置 | `config/sys.config.example`（脱敏版） |
| 备份恢复记录 | `docs/guides/operations/deployment/restore-drill-2026-06.md` |
| 漏洞扫描报告 | 测评前由第三方出具 |
| 安全培训记录 | 客户提供 |
| 安全策略文件 | 客户提供 |
