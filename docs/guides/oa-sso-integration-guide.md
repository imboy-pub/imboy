# IMBoy × OA 免登对接协议（OA 团队版）

> **适合阅读对象**：OA 后端、前端、运维、测试工程师
> **文档目的**：指导 OA 完成「IMBoy App → OA」的免登对接
> **版本**：v1.0（2026-09-29）
> **说明**：本文只介绍 OA 需要实现的内容，不要求 OA 团队了解 IMBoy 内部实现。
> 技术真源：IMBoy 侧冻结合同 `docs/architecture/2026-09-21-epgz05-oa-sso-contract.md`（本文是它的工程师友好版）。

---

## 目录

1. [这个功能是干什么的](#1-这个功能是干什么的)
2. [OA 团队需要做的 4 件事](#2-oa-团队需要做的-4-件事)
3. [整体流程](#3-整体流程)
4. [重要：OA 页面的运行环境](#4-重要oa-页面的运行环境imboy-内嵌-webview)
5. [第 1 件事：准备回调地址](#5-第-1-件事准备回调地址)
6. [code 是什么](#6-code-是什么)
7. [state 是什么](#7-state-是什么)
8. [第 2 件事：接收 code](#8-第-2-件事接收-code)
9. [第 3 件事：调用 IMBoy 核实（exchange）](#9-第-3-件事调用-imboy-核实exchange)
10. [错误怎么处理](#10-错误怎么处理)
11. [第 4 件事：创建 OA 自己的登录会话](#11-第-4-件事创建-oa-自己的登录会话)
12. [external_user_id 与员工绑定](#12-external_user_id-与员工绑定谁维护映射)
13. [特殊情况怎么办](#13-特殊情况怎么办)
14. [安全要求](#14-最重要的安全要求)
15. [JavaScript / Node.js 特别注意](#15-javascript--nodejs-特别注意)
16. [一个完整的例子](#16-一个完整的例子)
17. [推荐的 OA 实现方式（伪代码）](#17-推荐的-oa-实现方式伪代码)
18. [OA 联调自测清单](#18-oa-联调自测清单)
19. [常见问题 FAQ](#19-常见问题-faq)
20. [双方职责分工](#20-对接双方最终职责)
21. [术语表](#21-术语表)
22. [联调前确认清单](#22-联调前需要双方确认的信息)
23. [安全底线速记](#23-当前协议的安全底线)

---

## 1. 这个功能是干什么的？

员工已经登录了 IMBoy。

员工在 IMBoy App 中点击：

> **工作台**

然后直接打开 OA。

**不需要员工再次输入 OA 的账号和密码。**

整个过程可以简单理解为：

```text
员工
 │
 │ 1. 在 IMBoy 点击"工作台"
 ▼
IMBoy
 │
 │ 2. 给这次登录生成一张"一次性小票"（code）
 ▼
OA 页面
 │
 │ 3. OA 后端拿小票向 IMBoy 核实
 ▼
IMBoy
 │
 │ 4. 告诉 OA：这是员工 E10023
 ▼
OA
 │
 │ 5. OA 找到自己的员工账号
 │
 │ 6. 创建 OA 自己的登录状态（Session）
 ▼
OA 首页
```

整个过程中：

**IMBoy 不会把自己的登录密码、JWT 或其他登录凭证交给 OA。**

OA 只需要拿到：

```text
"这个人是谁"
```

然后由 OA 自己完成登录。

如果你只打算记住一句话：

> **IMBoy 给 OA 一张只能使用一次、只有 60 秒有效的"登录小票"；OA 后端拿这张小票向 IMBoy 核实员工是谁，然后用自己的登录系统让这个员工进入 OA。**

---

## 2. OA 团队需要做的 4 件事

| # | 事情 | 由谁做 | 工作量 |
|---|------|--------|--------|
| 1 | 准备一个回调地址（redirect_uri） | OA 后端 | 提前登记 |
| 2 | 接收 IMBoy 带来的 `code` 和 `state` | OA 后端 | 1 个 GET 接口 |
| 3 | 拿 `code` 调 IMBoy 核实身份（exchange） | OA 后端 | 1 个服务端调用 |
| 4 | 用返回的员工标识，创建 OA 自己的登录会话 | OA 后端 | 复用 OA 登录体系 |

对，**全部 4 件事都在 OA 后端**。OA 前端几乎不用改（只有少量环境注意事项，见第 4 节）。

---

## 3. 整体流程

```text
┌──────────────┐
│  员工手机     │
│  IMBoy App   │
└──────┬───────┘
       │
       │ ① 点击"工作台"
       ▼
┌──────────────┐
│ IMBoy 后端    │
└──────┬───────┘
       │
       │ ② 生成一次性 code
       │    有效期 60 秒
       ▼
┌──────────────┐
│ OA 回调页面   │   ← IMBoy 在 App 内嵌浏览器中打开
└──────┬───────┘       https://oa.example.com/sso/imboy/callback
       │                     ?code=xxx&state=yyy
       │ ③ code + state
       ▼
┌──────────────┐
│ OA 后端       │
└──────┬───────┘
       │
       │ ④ code + nonce + Application Credential
       │    （服务器对服务器，用户看不见）
       ▼
┌──────────────┐
│ IMBoy 后端    │
└──────┬───────┘
       │
       │ ⑤ 返回员工身份（external_user_id）
       ▼
┌──────────────┐
│ OA 后端       │
└──────┬───────┘
       │
       │ ⑥ 创建 OA Session，设置 Cookie
       ▼
┌──────────────┐
│ OA 首页       │   员工直接进入，全程没输过密码
└──────────────┘
```

---

## 4. 重要：OA 页面的运行环境（IMBoy 内嵌 WebView）

这一点很多团队会忽略，请前端同学务必阅读。

OA 页面**不是在系统浏览器里打开**，而是在 IMBoy App 内嵌的浏览器组件（WebView）里打开。它带来 4 个影响：

### 4.1 只能停留在 OA 自己的域名里

IMBoy 出于安全，**只允许 WebView 停留在登记的那个域名**（协议术语：exact origin 精确同源）。

如果 OA 页面发生以下行为，**会被立即拦截，页面停止加载**，员工会看到"已阻止离开企业 OA"的提示：

```text
❌ 跳转到另一个域名（含子域名不同）
❌ window.open 打开新窗口到外部域名
❌ 重定向到第三方登录页 / 第三方扫码页
❌ 从 http 协议地址加载资源后跳转
```

所以要求 OA：

> **从回调进入 → 登录成功 → 登录后的所有页面，全部保持在同一个域名下完成。**

如果 OA 现有登录流程会跳第三方（例如统一身份平台、短信验证服务商页面），需要单独和 IMBoy 团队沟通，**不要自行绕过**。

### 4.2 页面需要做移动端适配

IMBoy 是手机 App。OA 的 H5 页面要在手机宽度的 WebView 里可用（建议按 375px 宽度设计）。如果 OA 目前只有 PC 页面，请提前评估。

### 4.3 OA 无法（也不需要）探测"自己是在 IMBoy 里打开的"

IMBoy 不会向 OA 页面注入任何 JavaScript 变量、标识或接口（协议术语：零 JSBridge）。OA 的页面行为不应依赖"检测运行环境"，正常按 Web 处理即可。

### 4.4 Cookie 可能被清理

员工在 IMBoy 里退出登录或切换账号时，IMBoy 会清理内嵌浏览器的 Cookie。所以：

> **OA 的登录状态不能假设"Cookie 永远在"。**
> OA 自己的 Session 过期、续期、重新登录逻辑要独立健壮。
> Cookie 没了 = 下次从 IMBoy 进来重新走一遍免登流程，员工无感。

---

## 5. 第 1 件事：准备回调地址

例如：

```text
https://oa.example.com/sso/imboy/callback
```

这个地址就是：

> IMBoy 把员工带到 OA 后，OA 接收登录信息的地址。

它必须满足：

- 必须使用 **HTTPS**
- 不能带 `#` 后面的内容（协议术语：fragment / 锚点）
- 长度不超过 2048 个字符
- 必须**提前登记到 IMBoy**
- 后续调用 IMBoy 时必须使用**完全相同的地址**（逐字符一致，见 9.3 节）

建议：

**不要在这个地址后面自己加 `?xxx=xxx` 参数。**

---

## 6. `code` 是什么？

`code` 可以理解成：

> **一张只能使用一次的临时登录小票。**

例如：

```text
oa_sso_xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
```

它具有几个特点：

- **60 秒有效**
- **只能成功使用一次**
- 不包含员工姓名、手机号等任何信息（就是一串随机字符）
- OA **不需要解析它**——把它当成完全没有含义的字符串
- OA **不需要保存它**
- OA **不应该把它写进日志**

看到它以后：

**不要尝试理解里面的内容。直接原样交给 IMBoy 即可。**

### code 为什么只有 60 秒？

因为 code 出现在浏览器跳转链路里（URL 上），暴露窗口越短越安全：

```text
15:00:00 生成 → 15:00:10 使用 → 成功
15:00:00 生成 → 15:01:01 使用 → 失败（已过期）
```

而且：

> **成功使用一次以后立即失效。**

---

## 7. `state` 是什么？

`state` 是一串随机字符（IMBoy 生成的，约 16~128 个字符）：

```text
AbCdEf123456789
```

它的作用是：

> **让 IMBoy 确认，这张 code 和这次登录请求是对应的**（防止小票被拿到别的会话里错误使用）。

OA 不需要自己生成，也不需要自己计算。OA 只做一件事情：

```text
浏览器收到 state
    ↓
原样传给 IMBoy
    ↓
exchange 请求中的 nonce 字段
```

例如：

```text
浏览器收到：  state = AbCdEf123456
OA 调 IMBoy：nonce = AbCdEf123456
```

**一个字符都不要修改。**

---

## 8. 第 2 件事：接收 code

员工进入 OA 后，IMBoy 内嵌浏览器会访问：

```text
https://oa.example.com/sso/imboy/callback?code=oa_sso_xxx&state=AbCd1234
```

这里有两个参数：

| 参数 | 简单理解 |
| ---- | ---- |
| `code` | IMBoy 发给这次登录的一张"一次性小票" |
| `state` | 这次登录对应的随机编号，原样回传用 |

OA 后端在这个接口里取出这两个值，进入第 3 步。

> 推荐这个接口**直接在服务端完成后续所有步骤**（见 11.5 节），不要先渲染一个页面再用 JavaScript 处理。

---

## 9. 第 3 件事：调用 IMBoy 核实（exchange）

OA 后端向 IMBoy 发一个服务器对服务器的请求（员工浏览器和手机 App 都不参与、也看不见这一步）：

### 9.1 请求

```http
POST /api/internal/v1/oa/sso/exchange
Authorization: Bearer ib_int_<你的凭证>
Content-Type: application/json
```

（完整域名如 `https://pro.imboy.pub/api/internal/v1/oa/sso/exchange`，联调时 IMBoy 提供。）

请求体：

```json
{
  "code": "oa_sso_xxxxxxxxx",
  "redirect_uri": "https://oa.example.com/sso/imboy/callback",
  "nonce": "AbCdEf123456"
}
```

三个字段就是上一节收到的值 + 登记的回调地址：

| 字段 | 从哪来 | 注意 |
| ---- | ---- | ---- |
| `code` | 浏览器收到的 `code` | 原样传递 |
| `redirect_uri` | OA 服务端配置 | 必须与登记地址**逐字符一致** |
| `nonce` | 浏览器收到的 `state` | 原样传递 |

不需要任何幂等头（`Idempotency-Key`）：这张 code 本身就是"只能用一次"的凭证。

### 9.2 成功响应（HTTP 200）

IMBoy 直接返回一个 JSON 对象（五个字段）：

```json
{
  "organization_id": 7301234567890123456,
  "application_id": 7301234567890123457,
  "user_id": 7301234567890123458,
  "external_user_id": "E10023",
  "consumed_at": "2026-09-29T08:15:30.123Z"
}
```

| 字段 | 说明 |
| ---- | ---- |
| `organization_id` | 企业编号（IMBoy 内部 64 位整数，OA 一般用不到） |
| `application_id` | OA 应用编号（同上） |
| `user_id` | IMBoy 内部用户编号（同上） |
| **`external_user_id`** | **OA 最需要的：双方约定的员工唯一标识** |
| `consumed_at` | 小票被核销的时间（ISO-8601 格式） |

> **OA 登录用 `external_user_id`，其他三个字段记日志排查问题时参考即可。**
> 响应里**永远不会**出现 IMBoy 的 token、JWT、session——这是协议红线。

### 9.3 `redirect_uri` 的"完全一致"有多严格

逐字符（byte 级）比较，下面每一行的两个写法都**不算同一个地址**：

```text
https://oa.example.com/callback    ≠  https://oa.example.com/callback/     （尾斜杠）
https://OA.example.com/callback    ≠  https://oa.example.com/callback      （大小写）
http://oa.example.com/callback     ≠  https://oa.example.com/callback      （协议）
https://oa.example.com/callback    ≠  https://oa.example.com/callback?a=1  （查询参数）
oa.example.com/callback            ≠  https://oa.example.com/callback      （协议头）
```

所以最简单的做法：

> **把 redirect_uri 固定写在 OA 服务端配置中，不要动态拼接。**

### 9.4 调用频率限制

exchange 接口对每个 OA 应用限流：**600 次/分钟**（以 IMBoy 侧环境配置为准）。

超过会返回 HTTP 429（见下节），响应带 `Retry-After` 头告诉你等多久。正常免登场景远达不到这个量级，一般只有实现错误（比如死循环重试）才会触发。

---

## 10. 错误怎么处理？

### 10.1 错误响应长什么样

所有错误都是统一的 JSON 信封 + 真实 HTTP 状态码：

```json
{
  "error": {
    "code": "resource_not_found",
    "message": "resource not found"
  }
}
```

`message` 是固定通用文案（不回显你的请求内容），**程序里只判断 `error.code` 即可**。

### 10.2 错误码全表

| HTTP | `error.code` | 简单理解 | OA 怎么处理 |
| ---- | ---- | ---- | ---- |
| 404 | `resource_not_found` | 小票不能用了（不存在/过期/已用过/不匹配） | 提示"登录已失效，请返回 IMBoy 重新进入工作台" |
| 422 | `identity_not_mapped` | 员工还没绑定 OA 账号 | 提示联系企业管理员 |
| 400 | `invalid_request` | OA 发的数据格式不正确 | 检查程序和配置（多半是 redirect_uri/字段问题） |
| 401 | `invalid_credential` | OA 的服务端凭证错误 | 联系 IMBoy 管理员 |
| 401 | `credential_expired` | OA 的服务端凭证过期 | 联系 IMBoy 管理员 |
| 403 | `insufficient_scope` | OA 没有调用权限 | 联系 IMBoy 管理员 |
| 403 | `application_disabled` | OA 应用被停用 | 联系 IMBoy 管理员 |
| 403 | `organization_disabled` | 企业被停用 | 联系 IMBoy 管理员 |
| 429 | `rate_limited` | 调用太频繁 | 按 `Retry-After` 头稍后重试 |
| 503 | `security_gate_closed` | IMBoy 安全配置暂时关闭 | 联系 IMBoy |
| 500 | `internal_error` | IMBoy 内部异常 | 稍后重试，持续异常联系 IMBoy |

> 注意：`resource_not_found` 的 HTTP 状态是 404，但它的意思是"这张小票无效"，**不是**"接口不存在"。

### 10.3 `resource_not_found` 为什么不告诉具体原因？

这张小票可能是：不存在 / 已过期 / 已使用 / 属于其他企业 / 回调地址不匹配。IMBoy **故意**统一返回 `resource_not_found`，不区分具体原因。

原因是：

> 不希望任何人通过错误信息探测"某张小票是否存在、是否已被用过"这类信息（安全上叫"不提供存在性预言机"）。

OA 不需要区分这些情况，统一提示：

> 登录已失效，请返回 IMBoy 重新进入工作台。

---

## 11. 第 4 件事：创建 OA 自己的登录会话

exchange 成功后，OA 拿着 `external_user_id` 完成登录。

### 11.1 找到员工

```text
external_user_id = E10023
        ↓
OA 数据库查询员工（例如工号字段）
        ↓
找到 → 继续
找不到 → 见 13.4 节
已停用 → 见 13.5 节
```

### 11.2 创建 Session 并设置 Cookie

```text
创建新的 Session
        ↓
Set-Cookie + 302 跳转 OA 首页
```

**Session（会话）** 可以简单理解为：

> OA 给浏览器发的一张"已经登录"的临时凭证，通常存在 Cookie 里。

Cookie 建议至少带三个属性：

```http
Set-Cookie: oa_session=NEW_SESSION_ABC; Secure; HttpOnly; SameSite=Lax
```

| 属性 | 作用 |
| ---- | ---- |
| `Secure` | 只允许 HTTPS 发送 |
| `HttpOnly` | 网页 JavaScript 读不到 Cookie（降低被脚本偷走的风险） |
| `SameSite=Lax` | 限制跨网站请求携带，同时兼容从 IMBoy 跳转进来的顶级导航 |

### 11.3 登录成功后必须更换 Session

假设用户进来前浏览器已有一个 `session_id = ABC123`，登录成功后**不要继续用它**，要发一个新的：

```text
登录前：ABC123  →  登录成功：XYZ789
```

这叫 **Session ID 更新**，用来防"会话固定攻击"：

> 不让别人提前给用户准备一个 Session 编号，然后用户登录后继续使用这个编号。

### 11.4 登录成功后要清理 URL

登录前地址栏（WebView 里）是：

```text
https://oa.example.com/sso/imboy/callback?code=xxx&state=yyy
```

登录成功后必须是干净的页面：

```text
https://oa.example.com/
```

**不能继续保留 `?code=xxx` 或 `?state=yyy`。** 推荐用 HTTP 302 直接跳转。

### 11.5 推荐服务端直接处理，不要前端 JS 处理

推荐：

```text
浏览器 → /sso/imboy/callback?code=xxx&state=yyy → OA 服务端直接调 IMBoy → 建 Session → 302 → OA 首页
```

不推荐：

```text
浏览器 → 显示一个网页 → JavaScript 读取 code → AJAX 调接口 → 再登录
```

原因很简单：

> `code` 出现在浏览器地址中，**停留时间越短越好**。前端 JS 方案会让 code 在浏览器里存活更久、面更广。

---

## 12. `external_user_id` 与员工绑定（谁维护映射）

这是双方协作的核心约定：

```text
IMBoy 用户（张三）
        ↓  绑定关系由 IMBoy 侧维护
external_user_id = E10023（OA 员工工号）
        ↓  OA 自己查库
OA 员工（张三）
```

- **IMBoy 负责**：维护"IMBoy 用户 ↔ OA 员工标识"的绑定表（企业管理员提供对应关系给 IMBoy）。
- **OA 负责**：保证拿 `E10023` 能在 OA 数据库里查到唯一员工（例如工号字段）。

如果某员工还没做绑定，IMBoy 在核实时会直接返回 `identity_not_mapped`（422），提示联系管理员即可。

**为什么用 `external_user_id` 而不是 IMBoy 的 `user_id`？**

因为 `user_id` 是 IMBoy 内部编号，OA 不应该依赖别人家的内部数据结构。`external_user_id` 是双方专门约定的"这个人在 OA 里是谁"。

---

## 13. 特殊情况怎么办？

### 13.1 员工连续点击两次工作台

没关系。每次点击都会生成**新的** code：

```text
第一次：code-A（60 秒、一次性）
第二次：code-B（60 秒、一次性）
```

两个 code 互相独立、互不影响。

### 13.2 同一个 code 被使用两次

第一次成功，第二次失败（`resource_not_found`）。IMBoy **不会**把第一次的员工信息再返回一遍。

所以 OA：

**不要把 code 当成可以重复使用的登录凭证。**

### 13.3 OA 调 IMBoy 时超时怎么办？

分两种情况：

**情况一：明确没有发出去**（DNS 解析失败、连接建立失败）

IMBoy 大概率没收到请求。可以在 60 秒内**用同一张 code 谨慎重试一次**；重试也失败就放弃。

**情况二：已经发出去，但 OA 没收到响应**（读超时、连接中断）

> **不要继续用同一张 code 重试。**

因为 IMBoy 可能已经把这张 code 核销了，重试必然失败，还可能造成误解。最安全的做法：

```text
提示用户："登录没有完成，请返回 IMBoy 重新进入工作台。"
```

然后等一张新的 code。

> 实在无法区分是哪种情况时，一律按情况二处理——引导用户重新进入，成本最低、最安全。

### 13.4 OA 找不到这个员工（工号不存在）

**不要自动创建 OA 账号。** 建议提示：

> 你的 OA 账号尚未开通，请联系企业管理员。

避免因人员绑定错误导致错误登录。

### 13.5 OA 员工已停用

不能登录。建议提示：

> 你的 OA 账号已停用，请联系企业管理员。

---

## 14. 最重要的安全要求

### 14.1 全部使用 HTTPS

```text
IMBoy → OA 回调地址
OA 后端 → IMBoy exchange 接口
```

都必须 HTTPS。

### 14.2 Application Credential 只能放服务端

Application Credential 是整个对接中最重要的东西，可以理解为：

> **OA 后端调用 IMBoy 的"服务端密码"。**

它**不是**员工密码，也**不是** IMBoy 用户 JWT、OA 用户密码、Cookie、手机端 Token。它只用于：

```text
OA 后端 → IMBoy 后端
```

的服务器之间通信。格式类似：

```text
ib_int_123456.secret_xxxxxxxxx
```

只能存在 OA 服务端（环境变量、密钥管理系统）。绝对不要放在：

```text
❌ HTML / JavaScript / Flutter
❌ 浏览器 / 手机 App
❌ Git 仓库
❌ 日志 / 工单 / 聊天记录
```

如果这个凭证泄露，**立即联系 IMBoy 处理**（吊销重发）。

### 14.3 不要把 code / state 写进日志

```text
code=oa_sso_xxxxxxxxx   ← 不要出现在任何日志里
state=xxxxxxxx          ← 同样不要
```

尤其检查：

```text
Nginx 访问日志 / 网关 / 应用日志 / APM / 错误监控
```

建议：回调地址的访问日志不要记录 `?` 后面的查询参数。

### 14.4 回调页面不要加载第三方资源

不要在 `/sso/imboy/callback` 页面加载统计脚本、广告、第三方 JS/图片/字体。最好的方式就是 11.5 节的服务端直处理 + 302。

### 14.5 建议回调响应带两个 HTTP 头

```http
Referrer-Policy: no-referrer
Cache-Control: no-store
```

| 头 | 作用 |
| ---- | ---- |
| `Referrer-Policy: no-referrer` | 避免浏览器把带 code 的地址"告诉"下一个网站 |
| `Cache-Control: no-store` | 告诉浏览器和代理：这个响应不要缓存 |

---

## 15. JavaScript / Node.js 特别注意

IMBoy 返回的三个编号字段：

```text
organization_id / application_id / user_id
```

是 **64 位整数**。JavaScript 的普通 `Number` 无法准确保存这么大的整数（超过 2^53 会丢精度）：

```json
{ "user_id": 7301234567890123458 }
```

Node.js 等 JavaScript 环境**不要直接当普通 Number 用**，请用 `BigInt` 或按字符串处理。

不过 OA 登录真正需要的是 `external_user_id`——它是字符串，没有这个问题。（如果 OA 前端某些场景要透传这三个编号，记得用 lossless 的 JSON 解析器。）

---

## 16. 一个完整的例子

假设员工：

```text
OA 工号：E10023
OA 姓名：张三
```

员工在 IMBoy 点击**工作台**，IMBoy 在 App 内嵌浏览器打开：

```text
https://oa.example.com/sso/imboy/callback?code=oa_sso_AAAA&state=XYZ123
```

OA 后端收到 `code=oa_sso_AAAA`、`state=XYZ123`，服务端调用 IMBoy：

```http
POST /api/internal/v1/oa/sso/exchange
Authorization: Bearer ib_int_123456.secret_xxxxxxxxx
Content-Type: application/json

{
  "code": "oa_sso_AAAA",
  "redirect_uri": "https://oa.example.com/sso/imboy/callback",
  "nonce": "XYZ123"
}
```

IMBoy 返回：

```json
{
  "organization_id": 7301234567890123456,
  "application_id": 7301234567890123457,
  "user_id": 7301234567890123458,
  "external_user_id": "E10023",
  "consumed_at": "2026-09-29T08:15:30.123Z"
}
```

OA 用 `E10023` 查到员工张三、账号正常，创建新 Session 并响应：

```http
Set-Cookie: oa_session=NEW_SESSION_ABC; Secure; HttpOnly; SameSite=Lax
Referrer-Policy: no-referrer
Cache-Control: no-store

Location: https://oa.example.com/
```

（HTTP 302）

浏览器最终停在干净的 `https://oa.example.com/`，员工直接进入 OA——**全程没有输入过密码**。

---

## 17. 推荐的 OA 实现方式（伪代码）

```text
收到 GET /sso/imboy/callback 请求
   │
   ├─ 读取 code、state
   ├─ 检查基本格式（非空、长度合理）
   │
   ├─ 调 IMBoy exchange（code + redirect_uri + nonce=state）
   │
   ├─ 成功？
   │     ├─ 否 → 按 error.code 显示对应提示（见第 10 节表格）
   │     │
   │     └─ 是
   │          ├─ 取 external_user_id
   │          ├─ 查询 OA 员工
   │          ├─ 员工不存在 → 提示"账号尚未开通，联系管理员"
   │          ├─ 员工已停用 → 提示"账号已停用，联系管理员"
   │          │
   │          ├─ 创建全新 Session（废弃旧 Session ID）
   │          └─ Set-Cookie(Secure/HttpOnly/SameSite=Lax)
   │             + no-referrer + no-store
   │             + 302 跳转 OA 首页（干净 URL）
   └─ 结束
```

---

## 18. OA 联调自测清单

完成开发后，请至少测试以下情况：

| # | 测试 | 预期 |
| - | ---- | ---- |
| 1 | 正常从 IMBoyd 进入工作台 | 成功登录 OA |
| 2 | 登录成功后查看最终 URL | 没有 `code`、`state` 残留 |
| 3 | 把带 code 的完整 URL 复制后再访问一次 | 登录失败（code 已用） |
| 4 | 等待超过 60 秒后再完成核实 | 登录失败（code 过期） |
| 5 | 修改 `state` 一个字符再核实 | 登录失败 |
| 6 | 修改 `redirect_uri`（如加尾斜杠）再核实 | 登录失败 |
| 7 | 使用错误的 Application Credential | 401 失败并记录告警 |
| 8 | 未绑定的员工（无映射）从 IMBoy 进入 | 422，不允许登录 |
| 9 | OA 已停用的员工登录 | 不允许登录 |
| 10 | 登录前浏览器已有旧 Cookie | 登录成功后 Session ID 已更换 |
| 11 | 检查 Cookie 属性 | `Secure`、`HttpOnly`、`SameSite=Lax` |
| 12 | 检查回调响应头 | `no-store`、`no-referrer` |
| 13 | 全链路搜日志 | 搜不到完整 code / state / credential |
| 14 | 连续点击工作台两次 | 两次都是独立小票，均能成功 |
| 15 | JavaScript 解析返回的 ID 字段 | 没有整数精度丢失 |
| 16 | **在 IMBoy 内打开 OA 后，页内点一个跳到其他域名的链接** | **被拦截并提示"已阻止离开企业 OA"（预期行为）** |
| 17 | **OA 页面在手机宽度（约 375px）下的显示** | **布局可用、可操作** |
| 18 | **在 IMBoy 里退出登录再重新登录，再次进入工作台** | **重新走免登，正常进入** |

（16~18 是 WebView 环境相关，对应第 4 节。）

---

## 19. 常见问题 FAQ

**Q1：OA 需要自己生成 code 吗？**

不需要。code 全部由 IMBoy 生成。

**Q2：OA 需要保存 code 吗？**

不需要。收到后尽快完成核实（exchange）。

**Q3：OA 需要保存 state 吗？**

不需要长期保存。收到以后原样作为 `nonce` 交给 IMBoy。

**Q4：OA 需要保存 Application Credential 吗？**

需要，但只保存在 OA 服务端安全配置中。

**Q5：OA 会拿到 IMBoy 的 JWT 吗？**

**不会。** 本方案不会把任何 IMBoy 登录凭证交给 OA。

**Q6：OA 需要给 IMBoy 提供接口吗？**

**不需要。** 方向是 OA 后端主动调 IMBoy，IMBoy 不回调 OA 的业务接口。

**Q7：OA 能不能在找不到员工时自动创建账号？**

不建议。应提示管理员处理绑定，而不是自动创建。

**Q8：同一个 code 能重复使用吗？**

不能。一张 code 最多成功一次。

**Q9：60 秒到了还能用吗？**

不能。从 IMBoy 重新进入工作台就会获得新 code。

**Q10：OA 登录以后，Session 有效多久？**

由 OA 自己决定。IMBoy 不管理 OA 的 Session。

**Q11：员工绑定关系（external_user_id 映射）谁维护？**

IMBoy 侧维护。OA 只需提供"员工唯一标识用哪个字段（如工号）"和初始对应关系，后续增减人也同步给 IMBoy 管理员。

**Q12：OA 页面怎么知道自己是被 IMBoy 打开的？**

无法可靠知道，也不需要。IMBoy 不注入任何标识（零 JSBridge），OA 按普通 Web 页面处理即可。

**Q13：exchange 需要传幂等头（Idempotency-Key）吗？**

不需要。code 本身就是一次性的，天然幂等（重复=拒绝）。

**Q14：IMBoy 退出登录后，OA 的登录还在吗？**

不一定在——IMBoy 退出时会清理内嵌浏览器 Cookie，OA 的 Session Cookie 可能随之消失。下次进入会重新免登，员工无感。所以 OA 不要把业务状态绑定在"Cookie 永远存在"这个假设上。

---

## 20. 对接双方最终职责

**IMBoy 负责：**

```text
员工身份确认
     ↓
生成一次性 code（60 秒、单次）
     ↓
确认 OA 应用 / 企业 / 员工绑定关系
     ↓
核实 code（exchange）
     ↓
返回 external_user_id
```

**OA 负责：**

```text
接收 code + state
     ↓
服务端调用 IMBoy 核实
     ↓
拿 external_user_id 找到 OA 员工
     ↓
创建 OA 自己的 Session（换新、安全 Cookie、清理 URL）
     ↓
让员工进入 OA 首页
```

---

## 21. 术语表

| 名词 | 通俗解释 |
| ---- | ---- |
| **免登** | 用户已经登录 IMBoy，不需要再次输入 OA 密码 |
| **SSO** | 单点登录（Single Sign-On），登录一次后可以进入另一个系统 |
| **code** | IMBoy 发的一张临时、一次性的"登录小票" |
| **一次性 code** | 一张小票只能成功使用一次 |
| **state** | 这次登录对应的随机编号（IMBoy 生成） |
| **nonce** | OA 调 IMBoy 时传递的 `state`——两者在本协议中是同一个值 |
| **redirect_uri** | OA 接收登录请求的回调地址 |
| **exchange** | OA 拿 code 向 IMBoy"验票"的过程 |
| **exact 匹配** | 逐字符完全一致（大小写、尾斜杠、参数都算差异） |
| **Application Credential** | OA 服务端访问 IMBoy 的"密码"（格式 `ib_int_…`） |
| **external_user_id** | 双方约定的员工唯一标识，例如工号 `E10023` |
| **Session（会话）** | OA 用来表示"这个浏览器已经登录"的状态 |
| **Cookie** | 浏览器保存的一小段数据，OA 通常用它保存 Session 编号 |
| **WebView** | App 内嵌的浏览器组件；OA 页面就运行在这里面 |
| **origin（源）** | 协议 + 域名 + 端口 三件套，完全相同才算同一个源 |
| **HTTPS** | 加密的网络连接（本协议全部要求 HTTPS） |
| **HttpOnly** | Cookie 属性：网页 JavaScript 不能读取该 Cookie |
| **Secure** | Cookie 属性：只能通过 HTTPS 发送 |
| **SameSite** | Cookie 属性：控制跨网站请求时 Cookie 是否发送 |
| **302** | HTTP 状态码，告诉浏览器"请跳转到另一个地址" |
| **Query 参数** | URL 中 `?` 后面的内容，例如 `?code=xxx` |
| **fragment（锚点）** | URL 中 `#` 后面的内容；回调地址不允许有 |
| **Referer** | 浏览器告诉目标网站"我是从哪个页面来的" |
| **Referrer-Policy** | 响应头：控制浏览器是否发送 Referer |
| **JWT** | 一种常见的登录令牌格式；本协议不会把它交给 OA |
| **TSID** | IMBoy 使用的一种 64 位整数 ID（JSON 里是整数，JS 要用 BigInt） |
| **BigInt** | JavaScript 用来准确处理大整数的数据类型 |
| **幂等** | 同一个请求重复执行，结果和执行一次一样 |
| **限流** | 限制单位时间内最多允许调用多少次 |
| **会话固定攻击** | 攻击者提前准备一个 Session 让用户登录后继续使用；登录成功后更换 Session ID 可防御 |

---

## 22. 联调前需要双方确认的信息

### IMBoy 提供

- [ ] exchange 接口完整地址（测试环境）
- [ ] Application Credential（安全渠道交付）
- [ ] 测试企业与测试员工（已完成绑定）
- [ ] 错误响应格式（本文第 10 节即最终格式）

### OA 提供

- [ ] redirect_uri（回调地址）
- [ ] 员工唯一标识用哪个字段（如工号）
- [ ] OA 测试环境地址（HTTPS）
- [ ] 测试员工账号（如 E10023）
- [ ] OA 技术联系人

### 双方确认

- [ ] redirect_uri 字符串完全一致（复制粘贴，不手敲）
- [ ] `external_user_id` 对应 OA 哪个员工字段
- [ ] 测试员工已完成绑定
- [ ] OA 服务端可以访问 IMBoy exchange 地址（网络/防火墙放行）
- [ ] 双方 HTTPS 证书有效
- [ ] 登录成功后的 OA 落地页地址
- [ ] OA 员工停用/不存在时的提示文案
- [ ] OA 登录流程不会跳转第三方域名（如有，提前提出）

---

## 23. 当前协议的安全底线

OA 对接时，请至少保证：

```text
HTTPS（双向）
  + Application Credential 只在服务端
  + code 不进日志
  + state 不进日志
  + code 60 秒有效、只能成功一次（IMBoy 侧保证，OA 不要试图复用）
  + redirect_uri 完全匹配
  + 登录成功后更换 Session
  + 登录成功后清理 URL 中的 code/state
  + OA 自己创建 Session（IMBoy 不发任何会话凭证）
```

**只要按照本文的流程实现，OA 团队不需要了解 IMBoy 内部数据库、Erlang、JWT、路由等实现细节。**
