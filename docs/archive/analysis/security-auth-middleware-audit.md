# 认证中间件安全审计报告 / Auth Middleware Security Audit Report

> **审计日期 / Audit Date**: 2026-05-27
> **审计范围 / Scope**: `auth_middleware.erl`, `auth_middleware_api_v1.erl`, `cors_middleware.erl`, `auth_ds.erl`
> **严重程度说明 / Severity**: CRITICAL（阻断合并）| HIGH（合并前修复）| MEDIUM（建议修复）| LOW（可选）

---

## 任务 1：认证中间件双版本审计 / Task 1: Dual Auth Middleware Audit

### 1.1 两个中间件的逻辑对比 / Logic Comparison

| 对比维度 | `auth_middleware.erl` | `auth_middleware_api_v1.erl` |
|---------|----------------------|------------------------------|
| 路由范围 | 所有非 `/v1/`、非 `/adm/`、非 `/static/`、非 `/webrtc/` 路径 | 仅 `/v1/` 前缀路由 |
| 签名验证触发条件 | `InOpenLi == false AND Switch == "on"` | 更细粒度：特定路径 + `InOpenLi == false` 均触发 |
| 开放路由处理 | 查询 `imboy_router:open()` | 同样查询，但还额外处理 `/v1/ws`、`/v1/init`、`/v1/refreshtoken`、`/v1/passport/` 前缀 |
| Option 路由处理 | 查询 `imboy_router:option()`，无 token 时放行 | 相同逻辑 |
| Token 最终验证 | 委托 `auth_ds:condition/5` | 委托 `auth_ds:condition/5` |
| 版本前缀剥离 | 无 | 无（路由匹配依赖完整 `/v1/` 路径） |

**主要差异**：`auth_middleware_api_v1.erl` 在签名验证前额外硬编码了 `/v1/ws`、`/v1/init`、`/v1/refreshtoken`、`/v1/passport/` 四个路径的签名开关行为，而主中间件仅依赖 `imboy_router:open()` 列表。这意味着 v1 对这四个路径有独立的签名豁免/强制逻辑，与 open 列表解耦。

**v1 版本引入目的（推断）**：

1. **版本化 API 隔离**：`/v1/` 路由（含 E2EE、e2ee_transfer、e2ee_social）引入后需要独立的认证策略，不污染旧路由逻辑。
2. **WebSocket 特殊处理**：`/v1/ws` 作为新版 WebSocket 端点，需要在签名开关开启时独立控制签名验证，而不依赖全局 open 列表。
3. **迁移路径**：老 API 走 `auth_middleware` → 新 `/v1/` API 走 `auth_middleware_api_v1`，实现灰度迁移。

---

### 1.2 Token 验证流程 / Token Verification Flow

```
请求 → cors_middleware → auth_middleware
                              ↓
                   路径分发（静态/adm/v1/webrtc/_）
                              ↓（非豁免路径）
                   [可选] verify_sign（签名验证）
                              ↓
                   auth_ds:condition/5
                              ↓
                   auth_ds:do_authorization/3
                              ↓
                   auth_ds:verify_token/1
                              ↓
                   token_ds:decrypt_token/1
                              ↓
                   注入 current_uid 到 HandlerOpts
```

**Token 类型区分（重要）**：`verify_token` 明确区分 `<<"tk">>` (access token) 和 `<<"rtk">>` (refresh token)，拒绝用 refresh token 调用普通 API，防止 token 降级攻击。这是一个**安全加分项**。

---

### 1.3 漏洞与风险分析 / Vulnerability Analysis

#### [MEDIUM] 风险 1：`/webrtc/` 路径完全豁免认证

```erlang
<<"/webrtc/", _Tail/binary>> ->
    {ok, Req, Env};
```

`/webrtc/` 下所有路径无论如何都直接放行，无 token 验证、无签名验证。如果 WebRTC 信令端点处理敏感操作（如创建房间、分配媒体流），攻击者可直接访问。

**建议**：若 WebRTC 端点需要认证，应委托给专用中间件或至少验证 token 存在性。若所有 WebRTC 端点均为公开端点，需在注释中明确说明。

---

#### [MEDIUM] 风险 2：签名验证默认关闭（`api_auth_switch` 默认 `off`）

两个中间件中签名验证均依赖配置项 `api_auth_switch`，默认值为 `<<"off">>`：

```erlang
Switch = ec_cnv:to_binary(config_ds:env(api_auth_switch, <<"off">>)),
```

在 `Switch == <<"off">>` 时，非 open 路由**跳过签名验证**，直接进入 token 验证阶段。Token 验证仍会执行（通过 `auth_ds:condition/5`），所以 JWT 认证不受影响。但**签名（设备指纹）这一层防重放/防伪造保护默认不启用**，生产环境若未显式开启此开关，设备签名验证形同虚设。

**建议**：
- 生产环境配置应明确设置 `{api_auth_switch, <<"on">>}`，并在部署文档中标注为**必须配置项**。
- 考虑将默认值改为 `<<"on">>`，需要豁免时显式关闭。

---

#### [LOW] 风险 3：`auth_middleware_api_v1` 中 `Passport` 前缀匹配使用 `string:sub_string`

```erlang
Passport = string:sub_string(binary_to_list(Path), 1, 10),
% ...
Passport == "/v1/passport/", Switch == <<"on">> ->
```

`string:sub_string/3` 是基于 Unicode 代码点的操作，对 URL 路径（纯 ASCII）没有问题，但涉及到：
1. `binary_to_list` 转换开销（每次请求都执行）
2. 若路径包含多字节 UTF-8 字符（理论上 URL 路径不应出现），可能截断不准确。

**建议**：改用 `binary:part(Path, 0, 10)` 或 Erlang 二进制模式匹配：
```erlang
<<"/v1/passport/", _/binary>> -> auth_ds:verify_sign(...)
```

---

#### [LOW] 风险 4：`do_verify_sign` 对未知 `Method` 返回 `false`

```erlang
do_verify_sign(_, _, _, _) ->
    false.
```

当 `sign` 头存在但 `method` 头为非 `sha256`/`sha512` 值时，验证直接失败。这是**安全设计**，但需要确保客户端在升级加密算法时服务端同步更新，否则会导致合法请求被拒绝。

---

#### [INFO] 优点：`condition/5` 的 Option 路由处理

```erlang
condition(true, _, undefined, Req, Env) -> {ok, Req, Env};
condition(true, _, <<>>, Req, Env) -> {ok, Req, Env};
condition(true, _, Authorization, Req, Env) -> do_authorization(Authorization, Req, Env);
```

Option 路由（如 CORS 预检的非 OPTIONS 方法）在无 token 时放行，有 token 时验证。这对需要可选认证的端点是合理设计（如公开内容但认证后显示更多信息）。

---

### 1.4 路由绕过风险 / Route Bypass Risk

#### [MEDIUM] 路径尾斜杠标准化

`auth_ds:remove_last_forward_slash/1` 会将 `/v1/user/info/` 处理为 `/v1/user/info`。这在通常情况下是安全的，但需确认 Cowboy 路由表中的路径定义与中间件处理后的路径一致，避免：

- 中间件匹配 `/v1/user/info`（通过认证）
- Cowboy 路由匹配 `/v1/user/info/`（不同 handler）

**建议**：在路由注册时统一不带尾斜杠，并添加集成测试覆盖带/不带尾斜杠的认证行为。

#### [LOW] `static/admin` 双路径豁免

主中间件中有两条静态资源豁免规则：

```erlang
<<"/static/", _Tail/binary>> ->  {ok, Req, Env};
<<"/static/admin/", _Tail/binary>> -> {ok, Req, Env};  % 实际上被上一条覆盖
```

由于 `/static/admin/` 已被 `/static/` 前缀匹配覆盖，第二条规则实际**永远不会执行**（Erlang `case` 第一个匹配分支成功即返回）。这是死代码，虽然不是安全漏洞，但可能引起误解。

**建议**：删除 `/static/admin/` 分支，或改变匹配顺序（先精确后宽泛）以保持注释意图。

---

### 1.5 迁移路径建议 / Migration Path Recommendation

当前双版本并存是过渡期设计。建议的长期路径：

1. **短期**：保持双版本，但统一签名验证逻辑到 `auth_ds:verify_sign/2`（已实现）。
2. **中期**：将 `auth_middleware_api_v1` 中硬编码的路径列表（`/v1/ws`、`/v1/init` 等）迁移到 `imboy_router:open/0` 或独立的 `imboy_router:v1_open/0` 列表，消除硬编码。
3. **长期**：当所有路由迁移至 `/v1/` 后，将 `auth_middleware` 合并为单一中间件，按版本前缀路由分发。

---

## 任务 2：CORS 和响应头审计 / Task 2: CORS and Response Headers Audit

### 2.1 CORS 配置分析 / CORS Configuration Analysis

#### [INFO] 优点：基于白名单而非通配符

```erlang
% 不使用 Access-Control-Allow-Origin: *
% 只有白名单中的来源才设置 Allow-Origin
{_Origin, true} ->
    cowboy_req:set_resp_header(<<"access-control-allow-origin">>, Origin, Req0)
```

**这是正确做法**。使用动态 Origin 反射（而非 `*`），且配合 `Vary: Origin` 响应头防止缓存污染。

#### [MEDIUM] 风险：`cors_allow_localhost` 开发配置可能泄漏到生产环境

```erlang
AllowLocalhost = config_ds:env(cors_allow_localhost, false),
case AllowLocalhost of
    true -> is_localhost_origin(Origin);
    false -> false
end
```

如果 `cors_allow_localhost: true` 被错误地带入生产配置，攻击者可从任意本地端口发起跨域请求（虽然实际攻击场景有限，但这是配置安全问题）。

**建议**：在部署脚本/CI 中增加检查，确保生产环境 `cors_allow_localhost` 为 `false`。

#### [MEDIUM] 风险：`allowed_origins` 为空列表时，所有跨域请求都不设置 CORS 头

```erlang
case BaseUrl of
    <<>> -> [];   % 返回空列表，所有 Origin 都会被拒绝
    _ -> [BaseUrl]
end
```

若 `base_url` 和 `cors_allowed_origins` 均未配置，前端将无法跨域访问 API（`access-control-allow-origin` 头不会被设置）。这在新环境初始化时可能造成功能性故障，且错误排查困难。

**建议**：在应用启动时验证 `cors_allowed_origins` 或 `base_url` 至少有一个配置，否则输出明显警告日志。

---

### 2.2 安全响应头缺失分析 / Missing Security Headers

对照 Web 安全规范，以下安全响应头**当前未设置**：

| 响应头 | 严重程度 | 说明 |
|--------|---------|------|
| `X-Content-Type-Options: nosniff` | **HIGH** | 防止 MIME 类型嗅探攻击 |
| `X-Frame-Options: DENY` | **HIGH** | 防止 Clickjacking（点击劫持）攻击 |
| `Strict-Transport-Security` (HSTS) | **HIGH** | 强制 HTTPS，防止 SSL 剥离攻击 |
| `Referrer-Policy: strict-origin-when-cross-origin` | MEDIUM | 控制 Referer 信息泄露 |
| `Permissions-Policy` | LOW | 限制浏览器功能（摄像头、麦克风等） |
| `Content-Security-Policy` | MEDIUM | 防止 XSS（需根据实际内容调整） |

当前 `access-control-allow-headers` 中包含了 `Referrer-Policy` 作为**允许的请求头**，但并未设置同名**响应头**——这是两个不同的概念，存在混淆。

#### 建议添加的安全响应头代码（在 `cors_middleware.erl` 中）：

```erlang
%% 在 execute/2 函数中，Req6 之后添加：
Req7 = cowboy_req:set_resp_header(
    <<"x-content-type-options">>, <<"nosniff">>, Req6),
Req8 = cowboy_req:set_resp_header(
    <<"x-frame-options">>, <<"DENY">>, Req7),
Req9 = cowboy_req:set_resp_header(
    <<"referrer-policy">>, <<"strict-origin-when-cross-origin">>, Req8),
%% HSTS 应在生产 HTTPS 环境下启用
%% Req10 = cowboy_req:set_resp_header(
%%     <<"strict-transport-security">>,
%%     <<"max-age=31536000; includeSubDomains">>, Req9),
```

---

### 2.3 CORS 配置总结 / CORS Summary

| 检查项 | 状态 | 说明 |
|--------|------|------|
| `Access-Control-Allow-Origin: *` | ✅ 无此问题 | 使用白名单动态反射 |
| `Vary: Origin` | ✅ 已设置 | 防缓存污染 |
| `Access-Control-Allow-Credentials: true` | ✅ 仅白名单来源设置 | 安全 |
| OPTIONS 非白名单来源返回 403 | ✅ 已实现 | 安全 |
| X-Content-Type-Options | ❌ 缺失 | 需添加 |
| X-Frame-Options | ❌ 缺失 | 需添加 |
| HSTS | ❌ 缺失 | 生产环境需添加 |
| Referrer-Policy | ❌ 缺失响应头 | 需添加 |
| CSP | ❌ 缺失 | 建议根据 API 特性评估 |

---

## 总体安全评分 / Overall Security Rating

| 模块 | 评分 | 主要问题 |
|------|------|---------|
| `auth_middleware.erl` | B+ | 死代码（`/static/admin/`），`/webrtc/` 完全豁免 |
| `auth_middleware_api_v1.erl` | B | 硬编码路径列表，`string:sub_string` 低效 |
| `auth_ds.erl` | A- | Token 类型区分良好，签名算法白名单 |
| `cors_middleware.erl` | B | 缺少关键安全响应头 |

---

## 优先修复清单 / Fix Priority

1. **[HIGH]** 在 `cors_middleware.erl` 中添加 `X-Content-Type-Options`、`X-Frame-Options`、`Referrer-Policy` 响应头
2. **[HIGH]** 生产环境配置文档中明确 `api_auth_switch` 须设置为 `on`，并在 HSTS 说明中提醒 HTTPS 强制跳转
3. **[MEDIUM]** 删除 `auth_middleware.erl` 中的死代码（`/static/admin/` 分支），或注释说明
4. **[MEDIUM]** 评估 `/webrtc/` 路径的认证需求，若有敏感操作应添加 token 验证
5. **[MEDIUM]** 将 `auth_middleware_api_v1.erl` 中硬编码的路径列表迁移到路由配置
6. **[LOW]** 将 `string:sub_string` 替换为二进制模式匹配
