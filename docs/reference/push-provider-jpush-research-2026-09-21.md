# JPush Provider 调研与合同（EPGZ-07 W1）

> 日期：2026-09-21 ｜ 状态：`RED`（adapter 未实现，本文定义 W2 实现合同）
>
> 范围：计划 §7.4 JPush 的 W1 部分——现状调研、App/后端字段错位定位、
> provider 目标合同、RED 测试骨架。不含任何生产实现。
>
> 权威计划：`.Codex/runs/enterprise-internal-20260921T043945Z/control/plan-gz.snapshot.md` §7.4

---

## 1. 现有 push 链路（imboy 后端，基线 9f5a5fc6）

### 1.1 模块地图

| 层 | 模块 | 职责 |
|---|---|---|
| API | `src/api/user_device_handler.erl` | `POST /api/v1/push/register`、`POST /api/v1/push/unregister`（路由见 `src/imboy_router.erl:160-161`） |
| Logic | `src/logic/push_notification_logic.erl` | token 注册/注销 + 离线判定（`imboy_syn:count_user/1 == 0` 即完全离线） |
| DS | `src/ds/push_token_ds.erl`（thin wrapper）、`src/ds/push_notification_ds.erl` | 多设备 fan-out 与 FCM/APNs 发送 |
| Repo | `src/repo/push_token_repo.erl` | `push_token` 表 CRUD |
| HTTP | `push_notification_ds:http_post/3`（私有，gun + TLS verify_peer + 连接进程字典缓存） | FCM v1 / APNs HTTP2 调用 |

### 1.2 push_token 表（`priv/migrations/00000001_foundation.up.sql:2361`）

```sql
CREATE TABLE public.push_token (
    id, user_id, device_id, device_type, platform, token,
    status (1=活跃/0=无效), created_at, updated_at,
    CONSTRAINT chk_push_token_device_type
      CHECK (device_type IN ('android','ios','web')),
    CONSTRAINT chk_push_token_platform
      CHECK (platform IN ('fcm','apns','web_push')),
    UNIQUE (user_id, device_id) WHERE status = 1
);
```

- **`device_type`**：设备 OS（android/ios/web）。
- **`platform`**：推送 provider（fcm/apns/web_push）——语义上即计划 §7.4 说的 "provider"。
- 同一用户同一设备仅一条活跃 token（upsert 先 deactivate 旧行再插新行）。

### 1.3 token 生命周期

- **注册/刷新**：`push_notification_logic:register_token(Uid, DeviceId, DeviceType, Platform, Token)` → `push_token_repo:upsert/5`（刷新=同一路径，同设备旧 token 置 0，新行置 1）。
- **注销**：`push_notification_logic:unregister_token(Uid, DeviceId)` → `push_token_repo:deactivate/2`。
- **失效 token 下线**：FCM 404/410、APNs 410 → `push_token_repo:deactivate_by_token/1`（`maybe_deactivate_token/2`）。
- **过期清理**：`push_notification_ds:cleanup_inactive_tokens(Days)` → `deactivate_inactive/1`。
- **账号注销**：`push_token_repo:delete_by_uid/1`。

### 1.4 离线判定与 fan-out

- 离线判定：`imboy_syn:count_user(Uid) == 0`（无任何在线连接才推送）；C2C/C2G 分别走 `maybe_push_for_c2c/4`、`maybe_push_for_c2g/4`（elib_async 异步）。
- fan-out：`send_to_user/3` / `send_to_users/3` → `list_by_uid(s)` 拉全部活跃 token 行 → 逐行 `do_send_push/3`（`elib_async:async_retry`，2 次重试间隔 3s）→ 按 `platform` 分派 `send_fcm` / `send_apns`；**未知 platform 落 `_ ->` 静默跳过**——jpush 目前即被跳过。
- 隐私 fail-closed 不变量：推送 title/body 恒为常量 `<<"新消息">>`/`<<"发来一条消息">>`，不携带消息正文/密文/发送者（`push_notification_logic.erl:71-75`）。

### 1.5 配置（`config/sys.config.example:201`）

`{imboy, push}` proplist：`enabled`、`fcm_project_id`、`fcm_access_token`、`apns_*`。无 jpush 键。

---

## 2. 字段错位清单（计划 §7.4「先修复当前 App/后端字段错位」）

### 错位 1（致命）：Flutter 把 device_type 的值塞进了 platform 字段

| 端 | 文件 | 字段 | 期望 | 实际 |
|---|---|---|---|---|
| 后端 | `src/api/user_device_handler.erl:212-225`（push_register） | `platform` | provider 标识：`fcm` \| `apns` \| `web_push`（DB CHECK `chk_push_token_platform`） | 收到 `"android"` 或 `"ios"` |
| Flutter | `lib/store/api/push_api.dart:24`（register body） | `platform` | 同上 | 发送 `platform` 参数 |
| Flutter | `lib/service/push_notification_service.dart:197` | `platform` 值来源 | provider 标识 | `Platform.isIOS ? 'ios' : 'android'` —— **是 device_type 的值** |

后果：`platform='android'` 违反 CHECK `chk_push_token_platform`（fcm/apns/web_push）→ insert 失败 → register 返回 500。**当前 FCM 注册链路实际不可用（中国大陆无 google-services 时更从未走通）。**

### 错位 2（致命）：Flutter 完全不发 device_type

| 端 | 文件 | 字段 | 期望 | 实际 |
|---|---|---|---|---|
| 后端 | `user_device_handler.erl:218` | `device_type` | `android` \| `ios` \| `web`（DB CHECK） | `maps:get(<<"device_type">>, PostVals, <<>>)` 得空串 |
| Flutter | `push_api.dart:24` | body 键 | 含 `device_type` | body 仅 `{token, platform, device_id}`，**无 device_type 键** |

后果：`device_type=''` 同样违反 CHECK `chk_push_token_device_type` → 即使错位 1 修复也仍失败。

### 错位 3（防御缺失）：后端不校验 device_type，也不校验 platform 值域

- `user_device_handler.erl:221` 仅校验 `device_id`/`token`/`platform` 非空，**不校验 `device_type` 非空、不校验两者值域**，错值一律放行到 DB 层炸成 500（而非 400）。

### 错位 4（接线缺失）：Flutter 侧生命周期未接线

- `PushNotificationService.registerToken()` / `unregisterToken()` 在 `lib/` **无任何调用方**（仅 `initialize()` 在 `lib/config/init.dart:645` 被调）——登录成功后不注册、token 刷新链路（FCM onTokenRefresh 分支内）虽有注册逻辑但 `_pushToken` 在无 FCM 时恒空。
- `UserRepoLocal.quitLogin()`（`lib/store/repository/user_repo_local.dart:236-309`）登出清理清单**不含 push token 注销**。
- `loginAfter()`（同文件 :177-234）残留被注释的历史 JPush 集成代码（jpush-flutter-plugin 的 `getRegistrationID` / `setAlias`），证明项目曾试过 JPush 后弃置。

### 修复清单（W2 执行）

| # | 端 | 文件 | 修复 |
|---|---|---|---|
| F1 | Flutter | `lib/store/api/push_api.dart` | register body 增加 `device_type`（android/ios）；`platform` 改传 provider（`fcm`/`jpush`，Android 无 Firebase 配置时用 jpush） |
| F2 | Flutter | `lib/service/push_notification_service.dart` | `_registerTokenToServer` 拆出 device_type 与 provider 两个语义（现在一个 `platform` 变量混用两义） |
| F3 | 后端 | `src/api/user_device_handler.erl` | push_register 校验 `device_type` 非空 ∈ {android,ios,web}、`platform` ∈ {fcm,apns,web_push,jpush}，非法返回 400 而非 500 |
| F4 | Flutter | `user_repo_local.dart` / 登录登出链 | loginAfter 触发 registerToken；quitLogin 先 unregisterToken 再清其余状态 |

---

## 3. JPush provider 目标合同（W2 实现依据）

### 3.1 数据合同（依赖 A1 migration）

- `push_token.platform` CHECK 扩展纳入 `'jpush'`：`('fcm','apns','web_push','jpush')`。
- 目标记录形状：`device_type='android'`、`platform='jpush'`、`token=<JPush RegistrationID>`。
- **FCM/APNs 兼容保留**：现有 CHECK 值不动、`do_send_push` 现有分支不动，jpush 只增不改；`device_type='ios'` 仍走 apns。多 provider 并存由 `platform` 列区分，唯一索引 `(user_id, device_id) WHERE status=1` 不变。

### 3.2 adapter 模块合同：`src/push_provider_jpush.erl`（W2 新建）

```erlang
-export([provider/0, device_type/0, send/3, classify/2]).
provider()       -> <<"jpush">>.          %% push_token.platform 存储值
device_type()    -> <<"android">>.        %% 一期 JPush 仅 Android
send(Token, Title, Body) ->
    ok                                            %% HTTP 200
  | {error, not_configured}                       %% env 缺 jpush_app_key/master_secret（fail-closed，不发请求）
  | {error, {jpush_error, invalid_token}}         %% 400+code 1003 → 调用方 deactivate_by_token
  | {error, {jpush_error, unauthorized}}          %% 401 / code 1004 → 不重试不 deactivate
  | {error, {jpush_error, rate_limited}}          %% 429 / code 1011 → 可重试
  | {error, {jpush_error, {status, N}}}           %% 其余非 200 → 可重试
  | {error, term()}.                              %% 网络层错误原样透传，可重试
classify(StatusCode, RespBody) -> ok | {jpush_error, ...}.   %% 纯函数
```

**HTTP seam**：`src/push_provider_jpush_http.erl`（W2 新建薄封装）
`post(Url, Headers, Body) -> {ok, Status, RespBody} | {error, Reason}`。
默认实现参照 `push_notification_ds:http_post/3`（gun、TLS verify_peer）。独立成模块的唯一目的：测试可 meck 桩（现有 `http_post` 是私有函数无法打桩）。**W2 实现后把 FCM/APNs 也迁到该 seam 属可选优化，不强制。**

### 3.3 请求构造合同（JPush REST API v3 语义）

- `POST {jpush_push_url}`（默认 `https://api.jpush.cn/v3/push`，env 可覆盖）。
- `Authorization: Basic base64(AppKey:MasterSecret)`（占位符注入，见 §5）。
- body 最小形状：

```json
{
  "platform": ["android"],
  "audience": { "registration_id": ["<rid>"] },
  "notification": { "android": { "title": "新消息", "alert": "发来一条消息" } }
}
```

- **隐私不变量（fail-closed）**：`notification.android` 仅允许 `title`/`alert`（+可选 `builder_id`）；**禁止** `extras`、消息正文、密文片段、发送者身份。title/alert 恒为 push_notification_logic 现有常量。计划 §4.1：JPush secret 不得进日志/审计——`send` 错误返回值只含语义原子。

### 3.4 fan-out 接线合同（`push_notification_ds:do_send_push`，W2 修改）

```erlang
<<"jpush">> ->
    case push_provider_jpush:send(Token, Title, Body) of
        ok -> ok;
        {error, not_configured} -> ok;                       %% 不重试
        {error, {jpush_error, invalid_token}} ->             %% 不重试，下线 token
            push_token_repo:deactivate_by_token(Token), ok;
        {error, {jpush_error, unauthorized}} -> ok;          %% 不重试，不下线
        {error, {jpush_error, rate_limited}} -> error({push_failed, rate_limited});
        {error, Reason} -> error({push_failed, Reason})      %% 可重试
    end
```

离线判定、`send_to_user/send_to_users` fan-out、`async_retry` 壳全部复用，不建第二套。

### 3.5 配置合同

`{imboy, push}` proplist 追加（sys.config.example 同步补占位符）：

```erlang
{jpush_app_key, "your-jpush-appkey"},        %% 客户提供，秘密走 SECTION C
{jpush_master_secret, "your-jpush-secret"},
{jpush_push_url, "https://api.jpush.cn/v3/push"}  %% 测试/代理可覆盖
```

---

## 4. RED 测试骨架

`test/push_provider_jpush_tests.erl`（本提交新增，17 条用例）：

- A 组 J1/J2：token 注册/刷新/注销合同固化（现有链路，对 jpush 值立即 GREEN；真 DB CHECK 依赖 A1 migration）。
- B 组 J3/J4/J5：adapter `push_provider_jpush` 的请求构造/响应解析/错误分类——**当前 RED**（模块未实现）。HTTP 全部经 `push_provider_jpush_http` meck 桩；测试 URL 用 RFC 2606 `.invalid` 域；凭证占位符。
- C 组 J6：fan-out 分派与失效 token 下线——**当前 RED**（`do_send_push` 现无 jpush 分支，platform=jpush 被静默跳过）。

运行：`make eunit-local t=push_provider_jpush_tests`。转绿条件：W2 实现 §3.2/3.3/3.4 合同，禁止 skip/删断言。

---

## 5. 安全红线

- 严禁真实 AppKey/MasterSecret 入库/入日志/入测试（本文与测试全用 `*-placeholder` 占位符）；生产凭据走 sys.local / 环境注入。
- 真机发送是第三方影响动作：拿到 AppKey/资质/设备后仍需**人工确认**才执行（计划 §7.4、§12）。
- 推送 payload 不含消息正文（E2EE 元数据不泄露，沿用现有 fail-closed 常量）。

---

## 6. 交接

- **给 A1（migration owner）**：`chk_push_token_platform` 扩展 `'jpush'` 放入本期原子迁移（plan §5.2）；down 需回滚 CHECK。
- **给 W2（A7 自己）**：按 §3.2-3.5 实现；转绿 §4 骨架；随后修 §2 修复清单 F1-F4。
- **给 A6（依赖接线 owner）**：见 imboyapp `docs/push-jpush-flutter-research-2026-09-21.md` 的 SDK 选型与 Gradle 接线建议。
