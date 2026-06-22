# 安全策略 / Security Policy

> IMBoy 是一款 IM 产品，处理用户消息、设备密钥、端到端加密内容等敏感数据。
> 任何安全问题都应被认真对待。我们感谢负责任的安全研究者。

---

## 受支持的版本 / Supported Versions

| 版本 | 状态 | 支持截止 |
|------|------|---------|
| `1.0.0-rc.x` | ✅ 主动维护 | —— |
| `0.8.x` (app) / `0.7.x` (backend) | ⚠️ 仅修高危 | 1.0.0 GA 后 3 个月 |
| `< 0.7` | ❌ 不再支持 | —— |

首个标准 SKU 版本为 `1.0.0-rc.1`。早期 pre-SKU 版本仅在紧急情况下修复。

---

## 漏洞上报 / Reporting a Vulnerability

**请不要通过公开 Issue 报告安全漏洞。**

### 上报渠道（优先级从高到低）

1. **邮件（首选）**：`security@imboy.pub`
   - 可以用 PGP 加密（公钥见 [KEYS](#pgp-key)）
2. **GitHub Security Advisory**（私密）：
   - 仓库 → Security → Report a vulnerability
3. **临时沟通**：如以上都不可用，可在任意仓库开一个**空白 Issue**，标题写 `SECURITY CONTACT REQUEST`（不要附带任何细节），维护者会主动私下联系你。

### 请在报告中包含

- 影响的组件（backend / app / admin）及版本
- 复现步骤（最小可复现用例 PoC）
- 影响评估（数据泄露 / 权限提升 / 拒绝服务 / 其他）
- 您希望被致谢的名字（可选）

### 响应 SLA

| 严重级别 | 首次响应 | 初步评估 | 修复窗口 |
|---------|---------|---------|---------|
| Critical（RCE / 密钥泄露 / 消息伪造） | 24 小时 | 72 小时 | 7 天 |
| High（认证绕过 / 权限提升 / 隐私泄露） | 72 小时 | 7 天 | 30 天 |
| Medium（信息泄露 / DoS / CSRF） | 7 天 | 14 天 | 60 天 |
| Low（配置建议 / 弱化建议） | 14 天 | 30 天 | 尽力而为 |

我们会在修复发布后 30 天内发布安全公告（GitHub Security Advisory + CHANGELOG）。

---

## 安全边界声明 / Scope

### 在范围内（欢迎报告）

- `imboy` 后端（Erlang/Cowboy）认证、授权、会话、消息路由、E2EE 服务端实现
- `imboyapp` Flutter 客户端（Android / iOS）本地密钥存储、消息加解密、权限控制、本地 SQLite 未授权访问、资源 URL 签名伪造
- `imboy-admin-frontend` 管理后台（React）认证、XSS、CSRF、权限绕过
- 官方 docker-compose / 部署脚本中的默认配置弱点
- 文档（`doc/`）中的误导性安全建议

### 不在范围内（不会受理）

- 针对**自部署实例**配置错误导致的问题（如用户自己把 `JWT_KEY` 写成 `CHANGE_ME` 上线）
- 未使用官方 release 构建的第三方 fork
- 依赖库（Cowboy / epgsql / 等）的已知 CVE —— 请直接上报给上游
- 社工、物理攻击、针对特定用户的钓鱼
- 仅具有理论影响、无实际利用路径的问题
- 缺少"最佳实践"类安全头/配置的"最佳实践建议"（除非能证明实际危害）

---

## 已知的安全基线 / Security Baseline

在报告问题前，请先阅读 `imboy/doc/operations/security.md` 了解当前版本已声明的安全边界：

- 传输层强制 TLS（生产部署通过 nginx 反代 + certbot 自动签发/续期 Let's Encrypt）
- 密码使用 **HMAC-SHA512 + 随机盐**存储（`elib_password` 内置 dual-verify，兼容早期 MD5 格式并在登录时自然淘汰）
- JWT + 刷新 token 机制
- E2EE 采用 RSA-OAEP-256 + AES-256-GCM，服务端不持有私钥、不解密 `ciphertext`
- 管理后台使用独立 cookie secret + CSRF token

已记录在案的**已知限制**（不视为新漏洞，但欢迎参与讨论）：

1. 密码哈希算法为 HMAC-SHA512，非 memory-hard 函数；计划在 1.1 迁移到 Argon2id，老 hash 届时通过登录惰性 rehash 迁移
2. 消息存储层 `msg_store` 为明文落库（非 E2EE 对话），由运营方承担加密落盘责任
3. 首次管理员创建目前仍需要 `erl remote_console`（P0-5 已列入 1.0.0 计划）

---

## 致谢 / Hall of Fame

首批安全研究者致谢将在 1.0.0 GA 时发布到 `SECURITY-HALL-OF-FAME.md`。

---

## PGP Key

> 1.0.0-rc.1 临时占位。GA 前会替换为正式 key 并发布到 `keys.openpgp.org`。

```
(placeholder — will be published at v1.0.0)
```

---

**联系**：`security@imboy.pub`
**最近更新**：2026-04-10
