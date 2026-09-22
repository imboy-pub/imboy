# IMBoy OA SSO One-Time Code Contract（EPGZ-05 W1 冻结）

> STATUS: `FROZEN_API_CONTRACT`
>
> IMPLEMENTATION_STATE: `IMPLEMENTED`（EPGZ-05 W2：src/logic/enterprise_oa_sso_logic.erl
> + src/api/enterprise_oa_sso_handler.erl + src/api/enterprise_oa_sso_exchange_handler.erl；
> Router 登记归 A0 W4，见 §3.1/§4.1 登记形态）
>
> AUTHORITY: 本文冻结 OA one-time SSO 的 HTTP 合同、code 生命周期状态机与负例矩阵。
> 上游权威：`plan-gz §7.2 / §6 INT-14 / §4`（RUN_ID `enterprise-internal-20260921T043945Z`）与
> `control/internal-api-manifest.yaml`（A0 BASE-00 冻结）。manifest 行（method/path/scope/grant/
> idempotency/rate/sender_mode/code_binding）不因本文改变；本文只在 manifest 边界内细化字段与
> 判定顺序。任何实施与本文冲突时：manifest > plan > 本文；本文与仓内既有事实冲突时 STOP 并回
> ADR，不得静默改约。
>
> 红线（继承 plan §4.1）：SSO code 明文、nonce 明文、credential、JWT 不得进入日志或审计。

## 0. 词汇与冻结方式

沿用 `2026-09-16-enterprise-organization-v1-core-contract.md` 的合同词汇：`MUST`/`MUST NOT`/
`SOURCE OF TRUTH`/`INVARIANTS`/`ACCEPTANCE`。冻结不等于已实现；RED 骨架见
`test/enterprise_oa_sso_tests.erl`（本提交同步落地，逐条引用 §6 负例编号）。

## 1. 范围与角色

```text
Flutter App（Human JWT 持有者）
   │ ① POST /api/v1/oa/sso/code          （HUMAN-SSO-01，本文 §3）
   ▼
IMBoy 后端 —— 签发 60s opaque one-time code（只落 digest）
   │ ② WebView 打开 exact redirect_uri?code=...&state=<nonce>   （EPGZ-06 Flutter 面）
   ▼
客户 OA H5 ──► 客户 OA backend
   │ ③ POST /api/internal/v1/oa/sso/exchange（INT-14，本文 §4，Application Credential）
   ▼
OA backend 建立自己的 HttpOnly/Secure/SameSite 会话（IMBoy 不发任何会话凭证）
```

- 调用方只有两类：Human JWT（仅 ①）与 Application Credential（仅 ③）（manifest INV-2/INV-3）。
- `MUST NOT`：OA H5、浏览器、Flutter 持有 Application Credential 或 IMBoy JWT 之外的任何中间凭证。
- `MUST NOT`：exchange 响应携带任何 IMBoy token/JWT/session（§4.3 R-6）。

## 2. code 形态与存储合同

- code 为服务端生成 opaque 一次性字符串：前缀 `oa_sso_` + ≥256-bit CSPRNG 随机数的
  base64url（无 `=` padding）。整体长度 8..128，字符集 `[A-Za-z0-9_-]`。
- `MUST`：熵 ≥256-bit，不可预测、不含 TSID、不含任何可解码业务信息（对齐 CONVENTIONS §1
  「防枚举令牌只存摘要」）。
- `MUST`：服务端只持久化 `SHA-256` digest（`code_digest`），形态对齐 `bot_repo:digest_hex/1`
  先例（hex digest + 查找按 digest 等值命中）。明文 code 仅在 ① 的响应出现一次。
- `MUST`：nonce 同样只存 digest（`nonce_digest`）；exchange 时对调用方提交的明文 nonce 计算
  digest 后 constant-time 比较。
- `SOURCE OF TRUTH`：`enterprise_oa_sso_code`（plan §5 建议实体：
  `organization_id, application_id, user_id, code_digest, redirect_uri, nonce_digest,
  expires_at, consumed_at`；最终 schema 归 A1/EPGZ-01，见 §8 handoff）。
- `INVARIANTS`：`(code_digest)` 唯一；绑定五元组 org/app/user/redirect_uri/nonce 在签发时
  一次性固定，签发后不可变；`redirect_uri` 以明文存储（它是 URI 不是秘密，且 exchange 必须
  exact-match 校验）；code/nonce 明文零落库、零日志、零审计。

## 3. HUMAN-SSO-01 `POST /api/v1/oa/sso/code`

### 3.1 认证与前置

- `MUST`：走现有 `/api/v1/*` Human 认证链（`auth_middleware_api_v1` 签名校验 + JWT condition），
  路由注册形态（A0 W4 经 Router lease 串行登记）：

```erlang
{"/api/v1/oa/sso/code", enterprise_oa_sso_handler, #{action => code}}
```

  必须挂在 `/api/v1` 认证块（不得进 `imboy_router:open()/0`）。
- `MUST NOT`：Application Credential 调用本端点——按 manifest `auth_contexts` 双向隔离，返回
  401/403（NEG-13）。

### 3.2 请求字段（JSON body）

| 字段 | 类型 | 必填 | 约束 |
|---|---|---|---|
| `application_key` | string | 是 | 目标企业应用公开标识（非秘密）；8..128 可打印字符 |
| `redirect_uri` | string | 是 | 必须与该 application 预注册 redirect URI **逐字节 exact match**；HTTPS；禁止 fragment；长度 ≤2048 |
| `nonce` | string | 是 | 客户端 CSPRNG 生成，16..128 字符 `[A-Za-z0-9_-]`；即 ② 中 `state` 参数原值 |

### 3.3 响应字段（成功，现有 human 信封 HTTP 200）

`payload`：

| 字段 | 类型 | 说明 |
|---|---|---|
| `code` | string | §2 形态的一次性 code，仅此一次出现 |
| `expires_in` | integer | 固定 `60`（秒）；TTL 不可配置放大 |
| `redirect_uri` | string | 回显已校验的 exact redirect_uri（Flutter 以 `redirect_uri?code=...&state=<nonce>` 打开 WebView） |

`MUST`：`state` 参数即 nonce 原值——Flutter 把 ① 请求中的 `nonce` 原样放进 ② 的 `state`，
OA backend 把收到的 `state` 原样作为 ③ 的 `nonce` 提交（跨端点绑定链，EPGZ-06/A5 联合遵守）。

### 3.4 签发判定顺序（fail-closed，任一步失败立即终止）

```text
JWT/签名认证 -> 请求字段语法校验（application_key/redirect_uri/nonce）
  -> application_key 可解析且 application active
  -> organization active -> 请求者是该 org 的 active Human member
  -> 已存在 (organization_id, application_id, user_id) 的 active identity mapping
  -> redirect_uri exact-match 预注册 allowlist（HTTPS、无 fragment）
  -> 签发（新行，独立 TTL）
```

实现钉住（W2）：① 语法校验前置（语法 400 与语义 403/404/400 分离）；
② `application_key` 仅 Org 内唯一（`uq_ea_org_key`），跨 Org 同 key 候选按
「请求者是该 Org active member」收敛——零候选且存在 active 候选 = 403
（NEG-H03 语义），收敛后多义 = 404 fail-closed（不提供多 Org oracle）；
③ organization 非 active 归 404（与 application 非 active 同族，human 面无
stable 码可承载 org 状态）；④ code 实际形态 `oa_sso_`（7 字节）+ 43 字符
base64url = 50 字符，熵恰 256-bit。

重复签发不幂等也不互斥：同一 Human 可多次签发，每个 code 独立 60s TTL、独立单次消费；
旧 code 不因新签发而失效（NEG-H07 行为合同）。

### 3.5 错误（human 面现有整数信封，`include/error_code.hrl`；HTTP 200 + envelope code，
认证边界用真实 HTTP 状态码——`elib_response:error_with_status/4` 先例）

| 条件 | envelope code（int） | HTTP | 宏 |
|---|---|---|---|
| JWT 缺失/无效 | 401 | 401 | `?ERR_UNAUTHORIZED` |
| `application_key` 无法解析 / application 非 active | 404 | 200 | `?ERR_NOT_FOUND` |
| 请求者非该 org active 成员 | 403 | 200 | `?ERR_FORBIDDEN` |
| 无 active identity mapping（fail early，NEG-H04） | 403 | 200 | `?ERR_FORBIDDEN` |
| `redirect_uri` 未注册/非 HTTPS/带 fragment/exact 不匹配 | 400 | 200 | `?ERR_INVALID_PARAM` |
| `nonce`/`application_key` 格式非法、body 不可解析 | 400 | 200 | `?ERR_INVALID_PARAM` |
| 服务端异常 | 500 | 200 | `?ERR_INTERNAL_ERROR` |

## 4. INT-14 `POST /api/internal/v1/oa/sso/exchange`

### 4.1 认证与前置

- `MUST`：完整经过 A2/EPGZ-02 的 Application Credential 认证链与固定顺序（plan §4.1）：
  credential → credential active/expiry → application active → organization active →
  scope `sso:exchange` → rate bucket `internal_sso`（fail-closed）→ operation → audit。
- `MUST`：请求头 `Authorization: Bearer ib_int_<credential_id>.<secret>`。
- 路由登记形态（A0 W4 经 Router lease 串行登记；必须经
  `enterprise_internal_middleware` 前置，ctx 注入 `handler_opts.enterprise_internal`）：

```erlang
{"/api/internal/v1/oa/sso/exchange",
    enterprise_oa_sso_exchange_handler, #{action => exchange}}
```
- `Idempotency-Key` **不要求**：manifest INT-14 行 `idempotency: single_use_code` 是对 INV-7
  通用 mutation 幂等要求的显式豁免——code 本身就是幂等键，重放语义是「拒绝」而非「回放原响应」
  （NEG-03）。携带该头不报错、也不改变语义。

### 4.2 请求字段（JSON body）

| 字段 | 类型 | 必填 | 约束 |
|---|---|---|---|
| `code` | string | 是 | §2 形态；格式非法（前缀/长度/字符集）→ `invalid_request` |
| `redirect_uri` | string | 是 | 非 HTTPS/带 fragment/超长 → `invalid_request`；格式合法但与签发绑定不 exact-match → `resource_not_found` |
| `nonce` | string | 是 | 同 ② `state` 原值；格式非法 → `invalid_request`；绑定不匹配 → `resource_not_found` |

### 4.3 成功响应（HTTP 200）

`payload`：

| 字段 | 类型 | 说明 |
|---|---|---|
| `organization_id` | int64 | TSID，JSON integer（INV-8；OA JS 侧必须 lossless/BigInt） |
| `application_id` | int64 | TSID |
| `user_id` | int64 | TSID——发起 SSO 的 IMBoy Human |
| `external_user_id` | string | 该 application 的 identity mapping 中绑定的外部员工标识 |
| `consumed_at` | string | ISO-8601 UTC 毫秒（CONVENTIONS §2） |

R-6 `MUST NOT`：响应出现任何 IMBoy token、JWT、refresh token、session id 或可换持 IMBoy
身份的凭证（OA 会话由 OA 自建）。

### 4.4 exchange 判定顺序（fail-closed）

```text
A2 认证链（credential/app/org/scope/rate）
  -> 请求字段语法校验（code/redirect_uri/nonce 形态）    => invalid_request
  -> code_digest 等值查找                                  => 未命中 resource_not_found
  -> expires_at > now（逻辑过期，无需写库）                => 过期 resource_not_found
  -> consumed_at IS NULL                                   => 已消费 resource_not_found（重放）
  -> 绑定五元组校验：
       row.organization_id == credential.org              => 跨 org resource_not_found
       row.application_id == credential.app               => 跨 app resource_not_found
       row.redirect_uri == 请求 redirect_uri（exact）      => 不匹配 resource_not_found
       row.nonce_digest == sha256(请求 nonce) 常时比较     => 不匹配 resource_not_found
  -> 原子消费（CAS，§5）+ 同事务内解析 identity mapping
       mapping 缺失/user 非 active                          => identity_not_mapped（整个事务回滚，code 不消费）
  -> 返回 §4.3
```

「统一不透明拒绝」原则：凡 code 绑定类失败（未知/过期/已消费/跨 org/跨 app/redirect/nonce）
一律 `resource_not_found`，不区分具体原因——不给持有猜测 code 的对端提供存在性、生命周期或
绑定状态的 oracle（OAuth2 `invalid_grant` 同型语义；stable 码表中无 `invalid_grant`，故映射到
`resource_not_found`，见 §7）。`identity_not_mapped` 是唯一在 code 全部绑定校验通过之后才可能
出现的拒绝，不泄露 code 状态。

## 5. code 生命周期状态机（冻结）

```text
                 issue（HUMAN-SSO-01，写新行：consumed_at=NULL, expires_at=now+60s）
                        │
                        ▼
                   ┌─────────┐   now >= expires_at（读时惰性判定，terminal）
                   │ issued  │────────────────────────────► expired（逻辑态，不写库；清理任务可回收）
                   └─────────┘
                        │
                        │ CAS：UPDATE enterprise_oa_sso_code
                        │      SET consumed_at = now()
                        │      WHERE code_digest = $1
                        │        AND consumed_at IS NULL
                        │        AND expires_at > now()
                        │   （影响行数 == 1 才算赢）
                        ▼
                   ┌──────────┐
                   │ consumed │（terminal；不可逆、不可重置、不可重发）
                   └──────────┘
```

- 状态集：`issued`、`consumed`、`expired`。写转移只有一条：`issued -> consumed`（CAS）。
  `expired` 是 `expires_at` 派生的逻辑终态（惰性判定 + 可选异步回收），不产生写转移。
- `MUST`：CAS 与 identity 解析在同一 DB 事务；解析失败整体回滚，code 停留 `issued`。
- `INVARIANTS`：并发 N 个 exchange 携同一 code → 恰好 1 个 CAS 赢家；其余全部拒绝
  （输家观察到 `consumed_at` 非空 → `resource_not_found`，NEG-04）。任一 code 至多一次成功
  exchange；成功响应至多产生一次。
- `MUST NOT`：任何后台任务、运维命令或重试路径把 `consumed` 改回 `issued`；不得对同一
  digest 二次签发。

## 6. 负例矩阵（RED 骨架逐条引用；编号即测试名后缀）

INT-14 面（internal stable 码）：

| 编号 | 场景 | 期望 |
|---|---|---|
| NEG-01 | 未知 code（伪造 256-bit 随机） | `resource_not_found` |
| NEG-02 | code 签发后 >60s 才 exchange | `resource_not_found` |
| NEG-03 | 同 code 第二次 exchange（重放） | `resource_not_found`；**不回放**首次成功响应（single_use_code 幂等豁免） |
| NEG-04 | 同 code 并发双 exchange 竞态 | 恰好 1 成功 1 拒绝；无双重成功 |
| NEG-05 | 跨 org：org B credential 换 org A code | `resource_not_found` |
| NEG-06 | 跨 app 同 org：app B credential 换 app A code | `resource_not_found` |
| NEG-07 | redirect_uri 不匹配（尾斜杠/大小写/query/子路径差异） | `resource_not_found` |
| NEG-08 | nonce 不匹配（≠ 签发 nonce 的 digest） | `resource_not_found` |
| NEG-09 | credential 缺 `sso:exchange` scope | `insufficient_scope`（A2 链） |
| NEG-10 | credential 无效/过期、application/organization 停用 | `invalid_credential` / `credential_expired` / `application_disabled` / `organization_disabled`（A2 链） |
| NEG-11 | 绑定全过但 mapping 缺失或 user 非 active（签发后竞态） | `identity_not_mapped`；code **不被消费**（事务回滚，TTL 内可修复后重试） |
| NEG-12 | 请求字段语法非法（code 前缀/长度/字符集；redirect_uri 非 HTTPS/fragment；nonce 长度） | `invalid_request` |
| NEG-13 | `internal_sso` 限流配置缺失 | fail-closed 拒绝（INV-9；`security_gate_closed`） |
| NEG-14 | exchange 响应含 IMBoy token/session（R-6） | 必须不存在（负向断言） |
| NEG-15 | code 明文/nonce 明文进日志或审计 | 必须为零（zero-leak 断言；红线 §0） |

HUMAN-SSO-01 面（human 整数信封）：

| 编号 | 场景 | 期望 |
|---|---|---|
| NEG-H01 | 无 JWT / JWT 无效调 `/api/v1/oa/sso/code` | 401 |
| NEG-H02 | `application_key` 未知或 application 停用 | 404 `?ERR_NOT_FOUND` |
| NEG-H03 | 请求者非目标 org active 成员 | 403 `?ERR_FORBIDDEN` |
| NEG-H04 | 无 active identity mapping | 403 `?ERR_FORBIDDEN`（fail early） |
| NEG-H05 | redirect_uri 未注册/非 HTTPS/带 fragment/exact 不匹配 | 400 `?ERR_INVALID_PARAM` |
| NEG-H06 | nonce 格式非法（长度/字符集）、body 不可解析 | 400 `?ERR_INVALID_PARAM` |
| NEG-H07 | 同用户连续签发两个 code | 各自独立 TTL + 单次消费，互不失效；`expires_in` 恒 60 |

双向隔离（manifest INV-2/INV-3）：

| 编号 | 场景 | 期望 |
|---|---|---|
| NEG-X01 | Application Credential 调 `/api/v1/oa/sso/code` | 401/403 |
| NEG-X02 | Human JWT 调 `/api/internal/v1/oa/sso/exchange` | 401/403 |

## 7. 错误码对齐表（manifest `stable_error_codes` 全集；MUST NOT 发明新码）

| stable code | SSO 场景用途 |
|---|---|
| `invalid_credential` | INT-14 调用方认证失败（A2 链，NEG-10） |
| `credential_expired` | credential 过期（A2 链，NEG-10） |
| `application_disabled` | application 停用（A2 链，NEG-10） |
| `organization_disabled` | organization 停用（A2 链，NEG-10） |
| `insufficient_scope` | 无 `sso:exchange`（NEG-09） |
| `resource_not_found` | **统一不透明拒绝**：NEG-01..NEG-08 全部 code 绑定类失败 |
| `identity_not_mapped` | NEG-11（code 不消费） |
| `organization_boundary_violation` | SSO exchange **不使用**（统一隐藏归入 `resource_not_found`）；保留给其他 INT 路由 |
| `idempotency_conflict` | INT-14 **不适用**（`single_use_code` 豁免 INV-7；无 Idempotency-Key） |
| `rate_limited` | `internal_sso` bucket 触发（A2 配置数值） |
| `security_gate_closed` | 全局安全门（含 NEG-13 fail-closed） |
| `invalid_request` | 请求字段语法非法（NEG-12） |
| `internal_error` | 未预期故障 |

internal 面错误信封（stable 字符串码的承载形态）归 EPGZ-02/A2 冻结；本文只钉「场景 → 码」映射，
不重复定义信封。

## 8. W2 实现 handoff（依赖 A1 / A2 的精确接口）

对 A1（EPGZ-01，migration/repos）：

1. `enterprise_oa_sso_code` 需落地 §2 字段 + `(code_digest)` 唯一约束（plan §5 建议实体的
   SSO 部分）；CAS 消费谓词必须能表达 `WHERE code_digest=$1 AND consumed_at IS NULL AND
   expires_at > now()`。
2. **本文新增 schema 需求**：`enterprise_application` 需要每 application 的 exact redirect URI
   allowlist（建议 `allowed_redirect_uris TEXT[]` 或子表；HTTPS-only、exact-match、无通配）。
   plan §5 实体清单未列此列，A1 定稿 schema 时必须补入，否则 §3.4 第 6 步无法 fail-closed。
3. 回收：过期未消费行的清理任务可选（惰性过期已保证语义），不得触碰 `consumed_at` 非空行。

对 A2（EPGZ-02，internal 认证/信封/限流）：

1. exchange handler 消费 A2 的 credential 认证链产物：`(organization_id, application_id,
   scopes)`；scope 判定固定 `sso:exchange`。
2. rate bucket `internal_sso` 的数值由 A2 配置；缺配置必须 fail-closed（NEG-13）。
3. stable 字符串码 → internal 错误信封的映射由 A2 冻结后本文 §7 直接引用。

对 A5（本 owner，W2）：handler/logic/ds/repo 四层 + Router 由 lease 串行登记 + 把
`test/enterprise_oa_sso_tests.erl` 的全部 skip 翻绿（逐 NEG 编号）。RED 骨架中的目标函数签名
（`enterprise_oa_sso_logic:issue_code/3` / `exchange/4` 等）是**建议形态、非冻结项**，W2 可按
分层惯例调整；HTTP 行为与 §6 期望值不可调。
**W2 落地形态**：`enterprise_oa_sso_logic:issue_code/2 | issue_code_tx/3 |
exchange/2 | exchange_tx/3 | validate_issue_params/1 | validate_exchange_params/1`；
identity 按 user 反查（repo 缺口）在 logic 层内联只读（镜像 EPGZ-02 先例），
exchange 的 `identity_not_mapped` 以「调用方必须回滚」契约承载（池化入口
`exchange/2` 以 `throw({rollback, ...})` 强制，直连测试以 SAVEPOINT 回滚）。

对 A6（EPGZ-06 Flutter）：§3.3 的 `state=nonce` 跨端点绑定链 + `expires_in=60` 的 UI 倒计时与
过期/退出/域名跳转错误状态。

## 9. 验收门（EPGZ-05 完成门映射）

- plan §8 EPGZ-05 行 `expiry/replay/cross-org 负例 PASS` ⇔ 本文档 NEG-02/03/05（含 04/06/07/08
  同族）转绿，且 `make compile / eunit / rest-contract-check` 不回退。
- RED 证据：W1 提交中 `test/enterprise_oa_sso_tests.erl` 全部用例为 `{skip, "RED pending
  EPGZ-05 impl (<NEG-id>)"}`；`make compile` PASS。
- 实施期发现本文任何条款与 manifest/plan/仓内事实冲突：STOP + 记录 evidence + 回 ADR（对齐
  organization v1 core contract 的证据边界规则）。

## 10. 冻结终态

```text
SSO_CODE_TTL = 60s FIXED
SSO_CODE_STORAGE = DIGEST_ONLY (sha256, code+nonce)
SSO_CODE_BINDING = org/app/user/redirect_uri/nonce IMMUTABLE
SSO_CODE_TRANSITIONS = issued -> consumed (CAS) ; issued -> expired (derived)
SSO_CODE_REPLAY = REJECT (resource_not_found, no response replay)
SSO_BINDING_FAILURE = UNIFORM_OPAQUE resource_not_found (no oracle)
SSO_IDENTITY_FAILURE = identity_not_mapped (code NOT consumed, txn rollback)
SSO_EXCHANGE_RESPONSE = NO IMBOY CREDENTIAL EVER
SSO_IDEMPOTENCY = single_use_code (INV-7 exempt per manifest INT-14 row)
SSO_HUMAN_FACE = existing integer envelope + error_code.hrl macros
SSO_ERROR_CODES = manifest stable_error_codes ONLY (no new codes)
STATUS = FROZEN_API_CONTRACT / IMPLEMENTATION_STATE=IMPLEMENTED (EPGZ-05 W2)
```
