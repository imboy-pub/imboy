# API: Friend 域回归（`/api/v1/friend/*`）

## Contract

| Field | Value |
| --- | --- |
| Handler | `friend_handler`（`action => add_friend / confirm / reject / delete_friend / list`） |
| Routes | `POST /api/v1/friend/add`、`POST /api/v1/friend/confirm`、`POST /api/v1/friend/reject`、`POST /api/v1/friend/delete`、`GET /api/v1/friend/list` |
| Content-Type | `application/json`（list 为 GET，无 body） |
| Authentication | JWT 必带（Bearer token）；`api_auth_switch=on` 时同时必须带 device-sign 头（`cos`/`did`/`dname`/`vsn`/`pkg`/`sign`/`method`，算法 `auth_ds:verify_sign/2`） |
| Response | HTTP 200 + `{code,msg,sv_ts,payload}` envelope；业务错误不使用 HTTP 4xx，仅认证边界（无/坏 token）返回真实 HTTP 401 |
| Error `field` 键 | `elib_response:error/4` 把人类可读错误信息放在 envelope **顶层** `field` 键，不在 payload 内 |
| Executable suite | `test/rest/suites/api_v1_friend_SUITE.erl` |

行为事实源（取证基线）：

- `src/api/friend_handler.erl` — action 分发、参数读取（`to`/`payload`/`created_at`、`from`/`to`、`user_id`）
- `src/logic/friend_logic.erl` + `src/domain/friend_agg.erl` — 申请状态机 `none -> pending -> friends`；`already_requested` / `already_friends` / `no_pending_request` 语义
- `src/ds/friend_ds.erl` + `src/repo/friend_repo.erl` — pending 行（status=0）、双向 friend 行（status=1）、delete 双向删除
- `src/ds/auth_ds.erl` — `condition/5`：无 token → HTTP 401 + code 401（`ERR_TOKEN_MISSING`）；坏 token → HTTP 401 + code 706（`ERR_TOKEN_MALFORMED`）

The executable contract is the Common Test suite. This Markdown file records intent and review context; it is not a test DSL. Tokens and passwords are redacted from evidence by `rest_evidence`.

## Cases

### FRIEND-001: Happy Path — add → confirm → list → delete

#### Given

Fixture 创建两个独立用户 A、B 并各自通过真实 `POST /api/v1/passport/login` 登录。

#### When

1. A `POST /api/v1/friend/add` `{to: B, payload: {...}, created_at: <ms>}`；
2. B `POST /api/v1/friend/confirm` `{from: A, to: B, payload: {}}`；
3. A `GET /api/v1/friend/list`；
4. A `POST /api/v1/friend/delete` `{user_id: B}`；
5. A 再次 `GET /api/v1/friend/list`。

#### Then

- 步骤 1：HTTP 200，`code=0`，`msg="success."`，`payload={}`。
- 步骤 2：HTTP 200，`code=0`，`payload.id`/`payload.peerId` 均为 A 的 uid，`payload.is_friend=1`（`friend_logic:confirm_friend_resp/2`）。
- 步骤 3：`code=0`，`payload.mine.id` 为 A；`payload.friend` 列表包含 `id == B` 的行。
- 步骤 4：`code=0`，`msg="success"`（`elib_response:success/2` 默认），`payload={}`；删除同时移除双向 friend 行。
- 步骤 5：`payload.friend` 不再包含 B。

### FRIEND-002: Authentication — 无 token / 坏 token

#### Given

已注册的 device-sign 签名键（`rest_fixture:ensure_sign_key/0`），不携带或携带无效 Authorization。

#### When

1. `GET /api/v1/friend/list` 只带 device-sign 头（无 authorization）；
2. 同请求带 `Authorization: Bearer garbage-not-a-jwt`。

#### Then

- 步骤 1：HTTP 401，envelope `code=401`，`msg="未登录，请先登录"`（`auth_ds:do_authorization/4` 的 `ERR_TOKEN_MISSING` 分支）。
- 步骤 2：HTTP 401，envelope `code=706`，`msg="Invalid token."`（`token_ds:decrypt_token/1` 异常路径 → `ERR_TOKEN_MALFORMED`）。
- 两个变体都证明 friend 域不在 open/option 白名单，必须过 JWT 门。

### FRIEND-003: Not Found — 无 pending 申请时 confirm / reject

#### Given

用户 B 已登录；confirm 的 `from` 是一个随机生成、数据库中不存在的 uid；reject 的 `from` 是一个真实存在但从未发起申请的用户。

#### When

1. B `POST /api/v1/friend/confirm` `{from: <ghost uid>, to: B, payload: {}}`；
2. B `POST /api/v1/friend/reject` `{from: <real uid>}`。

#### Then

- 两步均 HTTP 200，`code=1`，`msg="no_pending_request"`。
- `field`（顶层键）分别为 `"无待确认的好友申请"` / `"无待拒绝的好友申请"`。
- 取证注：friend_logic 不校验目标用户是否存在（`user_friend` 无外键），缺失资源语义由缺失的 pending 行派生（`friend_agg:accept/reject -> no_pending_request`）。

### FRIEND-004: Validation — add 缺必填字段

#### Given

用户 A、B 已登录。

#### When

依次 `POST /api/v1/friend/add`，分别缺少 `to`、`payload`、`created_at`。

#### Then

- 三步均 HTTP 200，`code=1`，`msg="Parameter error"`。
- 顶层 `field` 依次为 `"to"` / `"payload"` / `"created_at"`（第一个缺失参数）。

### FRIEND-005: Conflict — 重复申请 / 已是好友再申请

#### Given

A、B 已登录；A 已向 B 发起申请且未确认；随后 B 已 confirm。

#### When

1. pending 未决期间 A 再次 `add` B；
2. confirm 之后再 `add` B。

#### Then

- 步骤 1：HTTP 200，`code=1`，`msg="already_requested"`，`field="您已发送过好友申请，请等待对方确认"`。
- 步骤 2：HTTP 200，`code=1`，`msg="already_friends"`，`field="对方已是您的好友"`。
- （friend_agg 状态机：`pending --request--> already_requested`；`friends --request--> already_friends`。）

### FRIEND-006: Reject 流

#### Given

A、B 已登录；A 已向 B 发起申请。

#### When

1. B `POST /api/v1/friend/reject` `{from: A}`；
2. B 随即 `confirm` 同一申请；
3. A 重新 `add` B。

#### Then

- 步骤 1：HTTP 200，`code=0`，`msg="success."`，pending 行被删除。
- 步骤 2：`code=1`，`msg="no_pending_request"`（证明 pending 已移除）。
- 步骤 3：`code=0`（状态机回到 none，可重新发起申请）。

### FRIEND-007: 删除好友后重新添加

#### Given

A、B 已登录且已是好友（add → confirm 完成）。

#### When

1. A `delete` B；
2. A 再次 `add` B。

#### Then

- 步骤 1：HTTP 200，`code=0`，`msg="success"`（删除永远回成功，即使无关系）。
- 步骤 2：`code=0`，`msg="success."` —— 业务明确允许重新添加已删除的好友（无 Conflict 错误），登记为真实 Validation 语义而非缺陷。

## Throttle / 隔离说明

friend 域端点无限流（`group` 域才有 `three_second_once`/`per_hour_once`）。所有用户、uid 均来自 `rest_fixture` 当次运行返回值；confirm/reject 中"不存在的 uid"由 `rand:uniform/1` 生成（< 2^62，PG bigint 范围内），不硬编码任何资源 ID。
