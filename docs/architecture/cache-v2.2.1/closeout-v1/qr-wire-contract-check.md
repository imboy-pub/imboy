# QR 客户端 wire contract 只读核查（CACHE_V221_BACKEND_CLOSEOUT_V1 / G-QR）

- 核查日期：2026-10-11
- 核查对象：后端 G-QR 改造（提交 `675b0744`，即当前 imboy 仓 HEAD）对移动端（imboyapp）wire contract 的影响
- 硬约束：G-QR 改造（权威表 `qr_login_session` + SSE `confirmed_ready` 服务端代领 token）**不得要求移动端同步改动**
- 核查方式：只读比对。后端 `src/api/qr_login_handler.erl`、`src/api/qr_login_sse_handler.erl`、`src/ds/qr_login_event_ds.erl`、`src/repo/qr_login_session_repo.erl`（HEAD 版本） vs imboyapp `lib/` QR 登录全部消费代码
- 两侧仓库零改动、未运行任何测试/flutter 命令、未执行任何 git 写操作

## 结论速览

```
MATCH     : 14
TOLERATED :  6
BREAK     :  0
```

**总结论：wire contract 保持。imboyapp 在 `qr_login_authority = pg` 切换后零改动可用。**
SSE `confirmed_ready`（无 token）信号在服务端被 `qr_login_sse_handler` 拦截并原子代领，最终下发给客户端的帧形状与 cache 权威的 `confirmed` 事件完全一致（`data: {"status":"confirmed","token":...}`）；`claimed_elsewhere` 以 SSE 注释帧 + 关连接下发，客户端浏览器 EventSource 层忽略注释、onerror 触发既有的降级轮询路径，行为闭环。

---

## 1. 后端 wire 面盘点（HEAD = 675b0744）

### 1.1 REST 端点（`src/api/qr_login_handler.erl`）

权威开关：`authority() -> cache | pg`（handler 行 84-89，默认 cache；pg 要求迁移 00000170_qr_login_session 已 apply）。以下逐端点给出两权威路径的对外 JSON。

#### POST /api/v1/passport/qr_login/create（行 115-152）

请求：`{"device_id": "...", "device_name": "Web Browser"?, "platform": "web"?, "purpose": "human"|"seat"?}`

响应（success，envelope code=0）：

```json
{"qr_token": "<base64>", "session_token": "<base64>", "expires_in": 60}
```

- 两路径响应形状**完全一致**；PG 的 `qr_token` 由 `generate_qr_token_value/0`（行 617-618，`base64:encode(crypto:strong_rand_bytes(32))`）生成，与 cache 路径 `generate_qr_token/1`（行 627-631）调用的是**同一个函数**，字符集/长度无差异。

#### GET /api/v1/passport/qr_login/status?session_token=（行 184-257）

| 会话状态 | cache 路径响应 | pg 路径响应 |
|---|---|---|
| waiting / scanned / cancelled | `{"status": "<status>"}` | `{"status": "<status>"}`（行 223-224） |
| confirmed 且 token 可取 | `{"status":"confirmed","token":...,"uid":...}`，取走即删会话（行 238-253） | 原子 claim（`claim_login_token`，UPDATE...RETURNING）成功：`{"status":"confirmed","token":...,"uid":...}`（行 210-216） |
| confirmed 但 token 已被他人领取 | 会话已删 → NOT_FOUND 错误 envelope | error envelope code=**5203** `ERR_QR_LOGIN_ALREADY_USED`（行 217-221） |
| 不存在/过期 | error envelope `ERR_NOT_FOUND` | error envelope `ERR_NOT_FOUND`（行 203-206） |

注：`uid` 来自 `scanned_by`（PG 侧经 `num/1` 归一为 integer，repo 行 181-184；jsone 编码为 JSON number）。

#### POST /api/v1/passport/qr_login/scan（行 262-387）

请求：`{"qr_token": "..."}`（必须已登录，否则 `ERR_UNAUTHORIZED`，行 269-273）

成功响应：
- 首次 scan（waiting→scanned CAS 成功）：`{"status":"scanned","device_name":"...","platform":"..."}`（cache 行 376-383 / pg 行 297-301，形状一致）
- **同一用户重复 scan（幂等成功）**：pg 仅 `{"status":"scanned"}`（行 317-319，无 device_name/platform）；cache 仍带设备字段

错误码（两路径映射一致）：5200 `INVALID_QR_TOKEN`、5201 `QR_LOGIN_EXPIRED`、5202 `QR_LOGIN_CANCELLED`、5203 `QR_LOGIN_ALREADY_USED`、403 `FORBIDDEN`（他人已扫）。

#### POST /api/v1/passport/qr_login/confirm（行 389-546）

请求：`{"qr_token": "..."}`

成功响应：`{"status":"confirmed"}`（cache 行 536-538 / pg 行 462，**两路径一致，均不带 token**——手机端本来就不消费 token）。

错误码：5200/5201/5202/5203/5204 `QR_LOGIN_NOT_SCANNED`/403。

内部差异（不影响 REST wire）：confirm 成功后的 SSE 广播——cache 发 `event(confirmed, LoginToken)`（事件带 token，行 532-535）；pg 发 `event(confirmed_ready, undefined)`（不带 token，行 458-461），由 SSE handler 服务端代领（见 1.2）。

#### POST /api/v1/passport/qr_login/cancel（行 548-600）

请求：`{"session_token": "..."}`；成功响应：`{"status":"cancelled"}`。

pg 语义修正：confirmed 后不可取消（返回 5203；cache 会把 confirmed 覆盖为 cancelled，属旧行为埋雷，见 handler 行 567-570 注释）。**imboyapp 无任何调用方**（见 2.3）。

### 1.2 SSE 事件流（`src/api/qr_login_sse_handler.erl` + `src/ds/qr_login_event_ds.erl`）

端点：`GET /v1/passport/qr_login/subscribe?session_token=<binary>`（handler 行 45-71）。帧格式：仅默认 `message` 事件（无 `event:`/`id:` 字段），每帧 `data: <json>\n\n`。

| 帧 | 触发源 | 内容 | 连接行为 |
|---|---|---|---|
| 心跳注释帧 | 30s 定时（行 87-94） | `: heartbeat\n\n` | 保持 |
| scanned | scan 成功（两路径均广播） | `data: {"status":"scanned"}` | 保持（行 120-122） |
| confirmed（cache 权威） | cache confirm 广播 `event(confirmed, LoginToken)` | `data: {"status":"confirmed","token":...}` | 推送后 stop（行 113-116） |
| confirmed（pg 权威） | confirm 广播 `event(confirmed_ready)` → **SSE 服务端拦截**（行 97-111）并调 `claim_login_token`：成功后以 confirmed 形状下发 | `data: {"status":"confirmed","token":...}`（行 104-105） | stop |
| claimed_elsewhere | 上述 claim 返回 `not_claimable`（token 已被其他 SSE 连接/轮询者原子领取） | `: claimed_elsewhere\n\n` **SSE 注释帧**（行 110） | stop（关连接） |
| expired | （当前后端无任何 notify(expired) 发射点，两路径均不会推送） | `data: {"status":"expired"}`（仅定义） | stop（行 117-119） |

关键点：裸的 `data: {"status":"confirmed_ready"}` 帧**不会出现在 wire 上**——`event(confirmed_ready,_)` 构造的 map（event_ds 行 75-76）唯一消费点是 SSE handler 的拦截分支，该分支必然转换为 confirmed+token 或 claimed_elsewhere 注释帧。

`token_claimable` 字段：仅是 `qr_login_session_repo.erl:76` SELECT 的内部计算列，**没有任何 handler 把它写入对外响应**。

---

## 2. 客户端消费面盘点（imboyapp，只读）

### 2.1 Web 端（被登录设备，QR 生成 + SSE/轮询）

| 文件 | 职责 | 消费的字段/事件 |
|---|---|---|
| `lib/page/passport/web_login_page.dart:139-146` | POST create | 请求发 `device_id/device_name/platform`（不发 purpose，后端默认 human） |
| `lib/page/passport/qr_login_response_rules.dart:73-94` | create 解析 | `qr_token`（必须非空）、`session_token`（必须非空）、`expires_in`（缺失默认 60） |
| `web_login_page.dart:201-212` + `qr_login_response_rules.dart:162-197` | status 轮询解析（2s） | `status`（waiting/scanned/expired/cancelled）；confirmed 时**必须**有非空 `token`（否则 `QrStatusUnknown('confirmed')`）、可选 `uid`（int>0 或非空 string） |
| `qr_login_polling_rules.dart:66-89` | 状态机决策 | `QrStatusConfirmed` → `RequestCompleteLogin(token, uid)`；`Unknown('confirmed')` → `ProtocolViolation`（failed + 停止） |
| `web_login_page.dart:266-298` | `_completeLogin` | token → SecureTokenStorage；uid 非空才写 `currentUid` |
| `web_login_page.dart:360-362` | SSE URL | `/api/v1/passport/qr_login/subscribe?session_token=$sessionToken`（与后端端点串一致） |
| `lib/page/passport/sse_client_web.dart:54-60` | SSE 接入 | 浏览器 `EventSource.onmessage`（只对 data 帧触发；注释帧在浏览器分帧层被过滤）→ 重加 `data: ` 前缀喂给 parseSseFrame |
| `lib/page/passport/qr_login_sse_rules.dart:31-71` | SSE 帧解析 | 与轮询同构：confirmed 必须有 token；`startsWith(':')` 注释帧 → `QrStatusStopPolling`（防御层，正常不会收到） |
| `lib/page/passport/qr_sse_session.dart:45-79` | SSE 会话编排 | onerror → `_attemptFailed` → 每秒 watcher `shouldFallbackToPolling` → `onFallback` → `_startPolling()`（2 秒轮询兜底） |
| `web_login_page.dart:705` | QR 图像 | `imboy://qr_login/${qrData}` 包装 qr_token，客户端不解析 token 内容 |

### 2.2 手机端（扫码设备）

| 文件 | 职责 | 消费的字段 |
|---|---|---|
| `lib/page/scanner/qr_login_intent.dart:77-123` | QR 意图识别 | 3 种形式解出 `qr_token` 原样透传（不本端解码 base64） |
| `lib/page/scanner/qr_login_confirm_provider.dart:52-102` | POST scan / confirm | 请求体 `{"qr_token": ...}` |
| `lib/page/scanner/qr_login_confirm_rules.dart:153-169` | scan 解析 | 硬性要求 `status == "scanned"`；可选 `device_name`/`platform`（全空 → `deviceInfo: null`，注释明确此为防御性兼容："后端补字段后无需重写"） |
| `qr_login_confirm_rules.dart:174-192` | confirm 解析 | 硬性要求 `status == "confirmed"`（不期待 token） |
| `qr_login_confirm_rules.dart:113-130` | 错误码映射 | 5201→Expired、5202→CancelledByOther、5203→AlreadyUsed、5200/5204/5205/400/403→Failed(msg 透传) |

### 2.3 端点调用全集（grep 实证）

imboyapp 中 qr_login API 引用仅 5 处：create / status / subscribe（SSE）/ scan / confirm。**cancel 端点零调用**（`qr_login_confirm_provider.dart:12-13` 注释明确：cancel 签名是 session_token，手机端没有，用户取消仅本端关闭页面）。

---

## 3. 逐项比对表

| # | 比对项 | 后端现状（HEAD） | 客户端消费 | 判定 |
|---|---|---|---|---|
| 1 | create.qr_token | 两路径同形状同生成函数 | `parseQrCreateResponse` 非空校验后包装进 QR 图像 | MATCH |
| 2 | create.session_token | 两路径一致 | 轮询/SSE URL 拼接 | MATCH |
| 3 | create.expires_in | 恒 60 | 缺失默认 60 | MATCH |
| 4 | status.status（waiting/scanned/cancelled） | 两路径一致 | 状态机正常流转 | MATCH |
| 5 | status.confirmed.token | pg 原子 claim 成功后随 confirmed 下发，与 cache 等价 | confirmed 必须带 token，拿到即落地登录 | MATCH |
| 6 | status.confirmed.uid | pg：`scanned_by`（int）；cache：int | 接受 int>0 或 string | MATCH |
| 7 | status "token 已被他人领取"场景 | pg：error envelope 5203；cache：NOT_FOUND error | 两者均为非成功 envelope → `QrStatusStopPolling` → `StopSilently`，**同一分支** | TOLERATED |
| 8 | scan.status（首次 scan） | `"scanned"` + 设备字段，两路径一致 | 硬性校验通过 | MATCH |
| 9 | scan.device_name/platform（pg 同用户重复扫码时缺省） | pg 幂等分支仅 `{"status":"scanned"}` | `_readDeviceInfo` 全空 → null，UI 设备卡片隐藏，无错误路径 | TOLERATED |
| 10 | confirm.status | `"confirmed"`（两路径一致，无 token） | `parseConfirmResponse` 恰好不期待 token | MATCH |
| 11 | 错误码 5200/5201/5202/5203/5204/403 | 两路径错误码映射一致 | `mapQrLoginErrorCode` 一一对应 | MATCH |
| 12 | cancel 端点（pg 语义：confirmed 后不可取消） | pg 返回 5203，cache 可覆盖 | **imboyapp 零调用**，语义变化不可见 | TOLERATED |
| 13 | SSE scanned 帧 | `data:{"status":"scanned"}`，两路径一致 | `TransitionToScanned` | MATCH |
| 14 | SSE confirmed 帧（cache：事件自带 token 直发） | `data:{"status":"confirmed","token":...}` | `RequestCompleteLogin(token)` | MATCH |
| 15 | SSE confirmed 帧（pg：confirmed_ready 服务端代领后下发） | 拦截转换后仍是 `data:{"status":"confirmed","token":...}`（sse_handler 行 104-105） | 与 #14 同一解析路径，形状无差别 | MATCH |
| 16 | claimed_elsewhere 注释帧 | `: claimed_elsewhere\n\n` + stop 关连接 | EventSource 层忽略注释帧；连接关闭 → onerror → watcher → `_startPolling()` 轮询兜底 → 下轮 status 得 5203 → `StopSilently` 收敛 | MATCH |
| 17 | heartbeat 注释帧 | `: heartbeat\n\n` 每 30s | EventSource 分帧层过滤，不触发 onmessage（`parseSseFrame` 另有 `startsWith(':')` 防御） | TOLERATED |
| 18 | expired SSE 帧 | 后端无发射点（两路径对称，非 G-QR 变化） | 若收到 → `TransitionToExpired`；收不到由本地 60s 倒计时 `MarkExpired` 兜底 | MATCH |
| 19 | token_claimable 字段 | 仅 repo SELECT 内部计算列，**未进入任何对外响应** | grep 全 imboyapp/lib 零引用 | TOLERATED |
| 20 | 裸 `data:{"status":"confirmed_ready"}`（假设泄漏） | 设计上不会出现在 wire（服务端拦截是唯一消费点） | `parseSseFrame` 未知 status → `QrStatusUnknown` → `KeepPolling`（继续等/轮询，无害） | TOLERATED |

计数：**MATCH 14 / TOLERATED 6 / BREAK 0**

---

## 4. 三点特别核查

### (a) 客户端是否假设 confirmed 事件的 data 里有 token？

**是，两个解析层都硬性依赖 token：**
- 轮询：`qr_login_response_rules.dart:180-188` — confirmed 无非空 token → `QrStatusUnknown('confirmed')`；
- SSE：`qr_login_sse_rules.dart:61-67` — 同样规则。

**后端已完整覆盖该路径：** pg 权威下 confirm 广播的是无 token 的 `confirmed_ready`，但 `qr_login_sse_handler.erl:97-111` 在服务端拦截该事件并调 `qr_login_session_repo:claim_login_token/1` 原子代领，**成功后以 `{"status":"confirmed","token":Token}` 形状推送**（行 104-105）——与客户端依赖的 confirmed+token 契约逐字段一致；轮询路径则由 `pg_handle_status`（handler 行 209-216）以同形状响应覆盖。裸 confirmed_ready 帧按设计不会到达客户端；即使因未来改动泄漏，客户端降级为 `KeepPolling` 继续等待，无害。

### (b) claimed_elsewhere 注释帧客户端如何处理？

**按预期回退轮询，链路完整：**
1. `: claimed_elsewhere\n\n` 是 SSE 注释帧，浏览器 EventSource 规范层忽略（不触发 onmessage），`sse_client_web.dart` 不会为其产出 frame；
2. 服务端随后的 `{stop,...}` 关闭连接 → EventSource `onerror` → `sse_client_web.dart:62-65` → `_errorsCtrl.add`；
3. `qr_sse_session.dart:52-55,58-71`：`_attemptFailed = true` → 每秒 watcher → `shouldFallbackToPolling(sseAttemptFailed: true) == true` → `onFallback()` → `web_login_page.dart:354-357` `_startPolling()` 2 秒轮询；
4. 该场景 token 已被其他消费者领取，轮询 status 得 5203 error envelope → `QrStatusStopPolling` → `StopSilently` → `timer.cancel()`，客户端静默收敛，无崩溃、无死循环。与后端注释（sse_handler 行 108-111"客户端走轮询兜底（G-QR 语义）"）逐环吻合。

### (c) 客户端轮询 status 的 token_claimable 字段是否被使用？

**否。** `token_claimable` 仅出现在 `src/repo/qr_login_session_repo.erl:76` 的 SELECT 计算列，且当前没有任何 handler 将其写入对外 JSON；imboyapp 全 lib grep 零引用。属纯服务端内部列，客户端无感知，无害。

---

## 5. 总结论与观察项

### 总结论

**wire contract 保持（MATCH 14 / TOLERATED 6 / BREAK 0）。** G-QR（675b0744）在 `qr_login_authority` 切至 pg 后，imboyapp 无需任何改动：

- create/scan/confirm/cancel 四个 REST 端点的成功响应形状与错误码在两权威路径下对客户端不可区分（唯一字段级差异见观察项 2，已被客户端防御性兼容）；
- SSE 流的 pg 路径（confirmed_ready → 服务端代领 → confirmed+token / claimed_elsewhere 注释帧）收敛到与 cache 路径相同的客户端可见帧集合，且 claimed_elsewhere 的降级轮询链路（onerror → fallback watcher → 2s 轮询 → 5203 → 静默停止）每一环都复用既有代码。

### 观察项（均非 BREAK、非 G-QR 引入）

1. **token 被他人领取时 status 错误码从 NOT_FOUND 变为 5203**（TOLERATED #7）：客户端走同一 `StopSilently` 分支，行为等价；该场景前提是多消费者竞争同一 session_token，正常单页面流程不触发。
2. **pg 同一用户重复 scan 幂等响应缺 device_name/platform**（TOLERATED #9）：客户端 `deviceInfo` 为 null 时 UI 设备卡片隐藏、无错误；且 `qr_login_confirm_rules.dart:26-28` 注释表明客户端早已为此做了"将来后端补字段无需重写"的兼容设计。
3. **SSE confirmed 帧不带 uid（轮询路径带）**：cache 时代即如此（cache SSE 事件也只含 status+token），G-QR 未改变；客户端 `uid` 可空并注释"兼容旧后端"。若 Web 端 SSE 主路径登录后依赖 `currentUid` 的路由守卫有问题，属既有观察项而非本次回归。
4. **本核查范围限 imboyapp**（任务口径）；imboyadmin 坐席面板（seatQrLoginPanel）等其他消费方未在本次范围，启用 pg 权威前如需可另行核查。

## 附：核查文件清单（全部只读）

后端（imboy 仓 HEAD=675b0744）：
- `src/api/qr_login_handler.erl`、`src/api/qr_login_sse_handler.erl`
- `src/ds/qr_login_event_ds.erl`、`src/repo/qr_login_session_repo.erl`
- `src/lib/elib_response.erl`（envelope 形状）、`include/error_code.hrl`（5200-5205）

客户端（imboyapp，只读）：
- `lib/page/passport/`：web_login_page.dart、qr_login_response_rules.dart、qr_login_sse_rules.dart、qr_login_polling_rules.dart、qr_sse_session.dart、sse_client.dart、sse_client_web.dart
- `lib/page/scanner/`：qr_login_intent.dart、qr_login_confirm_provider.dart、qr_login_confirm_rules.dart
- `lib/component/http/`：http_response.dart、http_parse.dart、http_transformer.dart（envelope 解码）
