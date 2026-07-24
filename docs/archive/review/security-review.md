# IMBoy 三仓安全评审 / Security Review

> 评审类型：Fact-based 只读评审（OWASP + IM 威胁建模）
> 评审范围：`imboy`（Erlang/OTP 后端）、`imboyapp`（Flutter）、`imboyadmin`（React）、`imboy-sdk-js`、`erlang_pay`
> 评审日期：2026-07-22
> 方法：全部结论引用 `文件:行号`，未发现真实密钥泄露。发现真实弱点仅标注位置与类型。

---

## 0. 执行摘要 / Executive Summary

整体安全姿态：**中等偏上**——认证/密钥/支付/E2EE 的核心控制设计正确且有生产启动期强校验兜底；主要缺口集中在**计费 API 的对象级越权（IDOR/BOLA）**与若干**密码学/会话卫生**问题，无 SQL 注入、无提交到 git 的真实密钥、无服务端明文私钥托管。

关键正确性结论（复核通过项）：

- **JWT**：`token_ds` 用 jwerl HS256 签发/校验，强制 `exp`；空 `jwt_key` 在生产由启动期 `validate_runtime_config/0` fail-fast 兜底（`imboy/src/imboy_app.erl:311-321`）。
- **E2EE 密钥托管**：未发现服务端接触明文私钥的路径；服务端仅存储/路由公钥与 `ciphertext`（`grep` 明文私钥处理为空）。零信任线（DROP `private_key_encrypted`）成立。
- **支付红线**：`agent_payment_command.erl:46-57` 强制发起人 `FromUid == mandate.owner_uid`，付款人恒为 owner；`agent_payment_logic` 单事务原子结算。红线成立。
- **SQL 注入**：Repo 层全量 `$N` 参数化，表名走内部常量（`elib_pg_sql:public_tablename`），未发现拼接注入。
- **原子表 DoS**：未发现对外部输入 `binary_to_atom`（仅 `agent_task_observer.erl:244` 一条防御注释）。
- **密钥入库**：git 仅跟踪 `config/sys.config` 与 `*.example`（占位 `CHANGE_ME`）；`sys.local.config`/`*runtime*.config` 已被 `.gitignore` 排除，无真实密钥入库。

---

## 1. 越权端点清单 / Broken Access Control (BOLA/BFLA)

**判定口径**：端点已过 JWT 认证（`current_uid` 可得），但业务未用 `current_uid` 约束目标对象归属 → 任意登录用户可越权操作/读取他人资源。

| # | 端点（action） | 方法 | 证据 | 越权后果 | 级别 |
|---|---|---|---|---|---|
| B1 | `/v1/billing/subscribe` | POST | `billing_handler.erl:70-85`（`_State`，`tenant_id` 取自入参） | 为任意 tenant 开通订阅 | HIGH |
| B2 | `/v1/billing/renew` | POST | `billing_handler.erl:90-111`（`subscription_id` 取自入参，无归属校验） | 续费任意订阅 | HIGH |
| B3 | `/v1/billing/cancel` | POST | `billing_handler.erl:116-128` | 取消任意订阅（拒绝服务） | HIGH |
| B4 | `/v1/billing/subscription` | GET | `billing_handler.erl:132-137`（`tenant_id` 入参直读） | 读取任意 tenant 订阅信息 | HIGH |
| B5 | `/v1/billing/usage`（report_usage） | POST | `billing_handler.erl:146-169` | 污染任意订阅用量计数 | HIGH |
| B6 | `/v1/billing/quota`（check_quota） | GET | `billing_handler.erl:173-191` | 读取任意订阅配额 | MEDIUM |
| B7 | `/v1/billing/invoice/generate` | POST | `billing_handler.erl:200-220` | 为任意订阅生成账单 | HIGH |
| B8 | `/v1/billing/invoice/pay` | POST | `billing_handler.erl:225-241`（`invoice_no` 入参直读） | 操作任意账单支付状态 | HIGH |
| B9 | `/v1/billing/invoice/list` | GET | `billing_handler.erl:245-254` | 枚举任意订阅账单 | MEDIUM |

> 说明：`billing_handler` 全部处理函数签名为 `(Req0, _State)`，`State` 中的 `current_uid` 被完全忽略；`tenant_id`/`subscription_id`/`invoice_no` 均来自客户端入参且无「该对象是否属于当前用户/租户」的校验。属 OWASP API1:2023（BOLA）+ API5:2023（BFLA）。
> `plan_list`（`billing_handler.erl:57-60`）为公开可读套餐列表，无对象归属，非越权。

**对照——正确实现的授权（无需修）**：

- 管理后台 handler 均有 RBAC：`adm_admin_handler`/`adm_role_handler` 自带 `ensure_permission/3`（`adm_admin_handler.erl:451-459`、`adm_role_handler.erl:629-637`），其余 adm handler 走 `adm_acl`。此前「0 adm_acl = 无鉴权」为误判（本地实现同等 RBAC）。
- 附件读写：`attach_logic` 写归属用 `object_key` 的 `u<Uid>/` 前缀 + `can_upload/3`，读归属按 scope 分发 `authorize/2`（`imboy/src/logic/attach_logic.erl:12-13,71,228-239`）。

---

## 2. 密码学正确性结论 / Cryptography Assessment

| 项目 | 结论 | 证据 |
|---|---|---|
| JWT 签发/校验 | 正确：HS256 + 强制 `exp`（含 5min leeway），过期返 705 可刷新 | `token_ds.erl:55-97,113-129` |
| JWT 密钥缺省 | 生产安全：空 `jwt_key` 由启动期 fail-fast 阻断 | `imboy_app.erl:316`；缺省 `sys.config:249` = `<<>>` |
| E2EE 私钥托管 | 正确：服务端零接触明文私钥，仅路由存储 `ciphertext`/公钥 | `grep` 明文私钥处理为空 |
| 支付授权 | 正确：owner_uid 绑定 + 单事务原子结算 | `agent_payment_command.erl:46-57` |
| 密码存储（管理端） | **弱**：`elib_password:generate` 用 HMAC-SHA512（快哈希，非记忆硬 KDF）+ 随机盐；旧 md5 回退仍被接受 | `elib_password.erl:28-31,103-108` |
| 密码比较 | 正确：常数时间 `crypto:hash_equals`（防时序） | `elib_password.erl:116` |
| 管理端 Cookie 签名 | HMAC-SHA256(uid)，与 jwt_key 隔离；**但无过期/nonce** | `adm_auth_middleware.erl:182-191` |
| 密钥用途隔离 | **加固缺口**：生产启动仅校验 `jwt_key`、`postgre_aes_key` 非空，未校验二者互异；仓库内没有证据证明生产实际复用 | `imboy_app.erl:311-325` |
| 随机数源 | 正确：`crypto:strong_rand_bytes/1`（盐、DB cipher key） | `elib_password.erl:29` |

---

## 3. 详细发现（按 OWASP + IM 威胁）

### A01 Broken Access Control

**[H-01] 计费 API 对象级越权（IDOR/BOLA）** — 见 §1 B1–B9。
- 影响：任意登录用户可越权读取、修改、取消他人订阅，污染用量，操作任意账单支付/生成——直接影响 SaaS 计费完整性与租户隔离。
- 证据：`imboy/src/api/billing_handler.erl:70,90,116,132,146,173,200,225,245`（全部 `_State`）。
- 级别：**HIGH（P1）**。
- 修复建议：在每个处理函数取 `Uid = auth_ds:current_uid(State)`，并在 `billing_logic` 层按 `subscription_id`/`tenant_id` 校验归属（`WHERE owner_uid = $Uid`），不属于当前用户即返回 `?ERR_FORBIDDEN`；单租户场景 `tenant_id` 应由服务端从 `current_uid` 推导，禁止信任入参。

### A02 Cryptographic Failures

**[M-01] 管理端密码用快哈希 HMAC-SHA512，无记忆硬 KDF；md5 旧格式仍被接受。**
- 影响：管理员口令哈希若随 DB 泄露，可被 GPU 高速离线爆破；md5 旧账户更弱。
- 证据：`imboy/src/lib/elib_password.erl:28-31`（generate=hmac_sha512）、`:49,103-108`（md5 回退）。
- 级别：**MEDIUM（P2）**。
- 建议：管理端口令改用 argon2id/bcrypt/scrypt（记忆硬）；对旧 md5 账户强制下次登录改密并迁移；保留常数时间比较。

**[M-02] 启动校验未强制 JWT 密钥与 DB AES 密钥互异。**
- 影响：若运维误把两个用途配置为同值，一处泄露会同时危及会话签名与 DB 字段加密，违反密钥分域原则。
- 证据：生产启动依次执行两个非空校验，但没有互异性断言（`imboy/src/imboy_app.erl:311-325`）。被 Git 忽略的本机配置不属于可复现的仓库证据，不能据此断言生产已经复用。
- 级别：**MEDIUM（P2，加固缺口；是否已在生产触发须由运维安全核验）**。
- 建议：为每种用途生成独立随机密钥；在 `validate_runtime_config/0` 增加「jwt_key ≠ postgre_aes_key」断言，并用只比较摘要/相等性的安全脚本核验现网，禁止输出密钥值。

### A05 Security Misconfiguration

**[M-03] 硬编码兜底管理 Cookie 密钥。**
- 影响：`signing_key/0` 缺省回退 `<<"imboy-adm-cookie">>`；若 strict-env 判定被误配（非 `pro/prod/production`），攻击者可用已知密钥伪造任意 `adm_user_id` 的管理 Cookie，完全绕过管理端认证。
- 证据：`imboy/src/adm/adm_auth_middleware.erl:220`；strict 判定 `:164-166`；生产 fail-fast 兜底 `imboy_app.erl:318`。
- 级别：**MEDIUM（P2）**——生产由 fail-fast 缓解，但依赖环境名正确。
- 建议：删除硬编码兜底，缺失即拒绝启动（不区分环境）；或对该常量值做启动期显式拒绝。

### A07 Identification & Authentication Failures

**[M-04] 管理端会话 Cookie 无过期与无法吊销。**
- 影响：`adm_user_sig = HMAC-SHA256(uid)` 不含时间戳/nonce/版本，签名永久有效；Cookie 被盗用后无法失效，改密/停用也不能吊销既有会话。
- 证据：`imboy/src/adm/adm_auth_middleware.erl:182-191`（仅签 uid）。
- 级别：**MEDIUM（P2）**。
- 建议：签名负载加入签发时间/会话版本号，服务端校验有效期并支持递增会话版本吊销；配合 `Secure`+`HttpOnly`+`SameSite`（当前 `same_site => lax`、`http_only => true` 已设，`secure` 依 start_mode）。

**[L-01] WebSocket 支持 `?token=` 查询串传 JWT。**
- 影响：URL 中的 token 易经反代/接入日志、Referrer 泄露。
- 证据：`imboy/src/api/websocket_handler.erl:45-48`（Qs `token` → `Bearer`）。
- 级别：**LOW（P3）**。
- 建议：优先 `Authorization` 头或 `Sec-WebSocket-Protocol` 携带；如需查询串则确保接入层不落盘该参数。

### A09 Security Logging & Monitoring

**[L-02] 管理端 IP 白名单为朴素前缀匹配，非真实 CIDR。**
- 影响：`"10.0.0."` 式前缀可能产生意外匹配/绕过（如 `10.0.0.` 也匹配 `10.0.0.99` 之外的构造），语义不精确。
- 证据：`imboy/src/adm/adm_auth_middleware.erl:257-269`（`longest_common_prefix`），代码注释已承认。
- 级别：**LOW（P3）**。
- 建议：接入 `inet_cidr` 做真正的 CIDR 掩码匹配。

### IM 特有威胁 / IM-specific

**[OK] E2EE 前向保密与密钥托管**：Olm/Megolm 迁移线，服务端零明文私钥；合规密钥（`compliance_key_*`）仅存公钥，私钥由审计方本地保管（`adm_admin_handler.erl:679-681` 注释与实现一致）。无发现。

**[OK] 客户端安全（Flutter）**：
- JWT 存 `flutter_secure_storage`（iOS Keychain / Android EncryptedSharedPreferences），`imboyapp/lib/service/secure_token_storage_service.dart:5-6`。
- SQLite 走 `sqflite_sqlcipher`，每用户独立随机密钥存安全存储（`imboyapp/lib/service/db_encryption_key_service.dart:24-33`）。
- 证书校验：生产严格（返 false 拒自签名），仅 dev/local 按精确 CN 白名单放行（`imboyapp/lib/component/http/http_client.dart:34-52`、`http_config.dart:41-50`）。无 MITM 放行。
- 拦截器已防「向公开存储/presigned 请求注入 JWT」导致的 token 泄露（`imboyapp/lib/component/http/http_interceptor.dart:12-54`）。

**[OK] 支付/金钱 DoS**：mandate owner 绑定 + 单事务结算 + 限流（`agent_rate_limiter`，记忆项证）。无越权花钱路径。

---

## 4. 威胁汇总表 / Threat Summary

| 编号 | OWASP/类别 | 问题 | CVSS 定性 | 证据 | 级别(P) |
|---|---|---|---|---|---|
| H-01 | A01 BOLA/BFLA | 计费 API 全族对象级越权（9 端点，忽略 current_uid） | High（未授权跨租户读写，网络可达/低复杂度） | `billing_handler.erl:70,90,116,132,146,173,200,225,245` | P1 |
| M-01 | A02 | 管理端密码快哈希 HMAC-SHA512 + md5 旧回退，无记忆硬 KDF | Medium（需先获 DB） | `elib_password.erl:28-31,103-108` | P2 |
| M-02 | A02 | 启动校验未强制 jwt_key 与 postgre_aes_key 互异 | Medium（误配置时单点泄露双重危害） | `imboy_app.erl:311-325` | P2 |
| M-03 | A05 | 硬编码兜底管理 Cookie 密钥 `imboy-adm-cookie` | Medium（生产 fail-fast 缓解，依赖环境名） | `adm_auth_middleware.erl:220` | P2 |
| M-04 | A07 | 管理端会话无过期/不可吊销 | Medium（需先盗用 Cookie） | `adm_auth_middleware.erl:182-191` | P2 |
| L-01 | A07 | WS 允许 `?token=` 查询串传 JWT | Low（日志/Referrer 泄露面） | `websocket_handler.erl:45-48` | P3 |
| L-02 | A09/A05 | 管理端 IP 白名单为朴素前缀非 CIDR | Low | `adm_auth_middleware.erl:257-269` | P3 |

### 复核通过（无发现，供留档）

| 项 | 证据 |
|---|---|
| JWT HS256 + exp 强制，空密钥生产 fail-fast | `token_ds.erl:55-129`、`imboy_app.erl:311-321` |
| SQL 全参数化、表名内部常量、无注入 | `src/repo/*.erl`、`adm_admin_handler.erl:288-322` |
| 无对外部输入 binary_to_atom | `grep`（仅防御注释 `agent_task_observer.erl:244`） |
| 支付 owner_uid 红线 + 原子结算 | `agent_payment_command.erl:46-57`、`agent_payment_logic.erl` |
| E2EE 服务端零明文私钥 | `grep` 明文私钥处理为空 |
| 密钥未入库（.example 用 CHANGE_ME 占位） | `git ls-files config/`、`sys.local.config.example:44-63` |
| Flutter 安全存储 + sqlcipher 每用户密钥 + 生产证书严格 | `secure_token_storage_service.dart`、`db_encryption_key_service.dart`、`http_client.dart:34-52` |
| 管理端 RBAC 全覆盖（含 admin/role 本地 ensure_permission） | `adm_admin_handler.erl:451-459`、`adm_role_handler.erl:629-637` |
| 附件读写归属 + scope 授权 | `attach_logic.erl:12-13,71,228-239` |
| 生产启动期强校验（默认密码/密钥/支付凭据/签名开关） | `imboy_app.erl:311-340` |

---

## 5. 优先修复顺序 / Remediation Priority

1. **P1 — H-01**：为 `billing_handler` 全部租户端端点补 `current_uid` 归属校验（BOLA 是唯一 HIGH，直接威胁计费/租户隔离）。
2. **P2 — M-03 / M-04**：删除硬编码兜底 Cookie 密钥；管理端会话加过期+可吊销。
3. **P2 — M-01 / M-02**：管理端口令迁记忆硬 KDF；密钥分域（jwt ≠ aes）并加启动断言。
4. **P3 — L-01 / L-02**：WS 弃用查询串 token；IP 白名单改真实 CIDR。

> 免责：本评审基于当前磁盘代码静态审阅；运行期配置（`IMBOY_*` 环境变量、生产 `sys.pro.config`）未在本机核实，涉及密钥的最终值以运维为准。
