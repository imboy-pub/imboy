# API: `/api/v1/msg/history` | `/api/v1/msg/reaction/add` | `/api/v1/msg/reaction/remove`

## Contract

| Field | Value |
| --- | --- |
| Handler | `msg_handler` with `action => history / reaction_add / reaction_remove` |
| Method | `GET /api/v1/msg/history`; `POST /api/v1/msg/reaction/add`; `POST /api/v1/msg/reaction/remove` |
| Content-Type | `application/json` |
| Authentication | JWT (`Authorization: Bearer <token>`) + mandatory device signature while `api_auth_switch=on` |
| Query (history) | `chat_type` (`c2c`\|`c2g`), `peer_id` (TSID string), optional `after_seq` (default 0), `limit` (default 50, clamped to 100), optional `did` |
| Body (reaction) | `msg_id`, `msg_type` (`c2c` default), `emoji` |
| Response | Envelope `{code,msg,sv_ts,payload}`; auth errors use real HTTP status |
| Executable suite | `test/rest/suites/api_v1_msg_SUITE.erl` |

The executable contract is the Common Test suite. This Markdown records intent and review
context; it is not a test DSL. Tokens must be redacted from evidence.

## Server behaviour facts (read from source at Base SHA)

- `history` requires `msg_archive_enabled=true` (set in `config/sys.local.config`, the
  config the REST runner loads) and reads `public.msg_store` through
  `msg_archive_ds:history/3`. Payload is `#{messages, next_seq, has_more, conv_key}`.
  `conv_key` for c2c is `c2c:{min_uid}:{max_uid}` (uids sorted). `next_seq` is the max
  `conv_seq` of the full (pre-limit) window or the passed cursor when empty.
- c2c history does not check a conversation row; any positive peer uid is readable by both
  participants because the conv key is symmetric. Missing `chat_type` → code `400`
  `缺少 chat_type 参数`; missing `peer_id` → code `400` `缺少 peer_id 参数`; unknown
  `chat_type` → code `400` `不支持的 chat_type: <value>`.
- `reaction/add` requires an existing `msg_c2c` row (for `msg_type=c2c`) and that the
  caller is the sender or receiver, otherwise `permission_denied`. Success payload is
  `#{msg_id, emoji, user_id, created_at}` with msg `添加表情成功`. Unknown msg id →
  code `404` (`ERR_MESSAGE_NOT_FOUND`) `消息不存在`. Missing msg id → `400`
  `缺少消息ID参数`; missing emoji → `400` `缺少emoji参数`; empty emoji → `400`
  `emoji不能为空`; unsupported msg_type → `400` `不支持的消息类型`.
- `reaction/remove` returns payload `#{msg_id, emoji}` with msg `移除表情成功`; unknown
  msg id → `404` `消息不存在`. Removing a reaction that was never added still succeeds
  (`msg_reaction_repo:remove` is a plain delete).
- `GET /api/v1/msg/reaction/list` currently returns `reactions` as a list of
  `{Emoji, Data}` tuples (`msg_reaction_ds:group_reactions_by_emoji/1`), which
  `jsone:encode/1` cannot serialize. The endpoint is therefore excluded from happy-path
  assertions in this suite (candidate `CONTRACT_MISMATCH` for A0, not asserted here).

## Data seeding

REST msg send belongs to the WebSocket suite, so this suite seeds rows through the
production data layer, not through crafted SQL:

- `msg_c2c` rows: `msg_c2c_ds:write_msg/6` (the same function the live write path uses),
  with plain-text payload maps and no `e2ee` key — no real encrypted traffic is produced.
- `msg_store` archive rows: `msg_archive_repo:archive/1`, which assigns the per-conversation
  `conv_seq` through `msg_store_seq` exactly like the message store worker.

## Cases

### MSG-001: history happy path

#### Given

Fixture users A and B; three c2c messages seeded into `msg_store` between them.

#### When

A sends `GET /api/v1/msg/history?chat_type=c2c&peer_id=<B uid>&after_seq=0&limit=50`.

#### Then

- HTTP `200`, `code` `0`.
- `payload.messages` has exactly 3 entries; every entry has `chat_type` `c2c`, the seeded
  msg ids, a positive integer `conv_seq`, and a `payload` map.
- `payload.conv_key` equals `c2c:<min_uid>:<max_uid>`; `payload.has_more` is `false`;
  `payload.next_seq` is an integer.

### MSG-002: history cursor pagination boundary

#### Given

A dedicated peer user logged in inside this case (not the suite-level B): the
c2c conv key is per-pair and prior cases leave their archived rows behind, so
a fresh pair guarantees the conversation holds exactly this case's three
seeded archive messages (fresh ids).

#### When

A requests `after_seq=0&limit=2`, then requests again with `after_seq=<next_seq returned by
the first response>` and `limit=50`.

#### Then

- First page: exactly 2 messages and `has_more` `true`.
- Second page: the remaining message(s), `has_more` `false`, no overlap with page one
  (conv_seq strictly greater than the first page cursor).

### MSG-003: history parameter validation

#### Given

A logged-in fixture user A.

#### When

A requests history without `chat_type`, then with `chat_type=c2c` but without `peer_id`,
then with `chat_type=xx&peer_id=1`.

#### Then

All three are rejected with HTTP `200` and code `400`, messages `缺少 chat_type 参数`,
`缺少 peer_id 参数` and `不支持的 chat_type: xx` respectively.

### MSG-004: reaction add / remove cycle

#### Given

A seeded `msg_c2c` message between A and B (A is the sender).

#### When

A adds emoji `👍` to the message (`POST /api/v1/msg/reaction/add`), then removes it
(`POST /api/v1/msg/reaction/remove`).

#### Then

- Add returns HTTP `200`, `code` `0`, msg `添加表情成功`, payload `msg_id`/`emoji` echoed,
  `user_id` equal to A's uid and `created_at` as a positive integer millisecond epoch
  (probe2 measured `1789922643788`; not a string).
- Remove returns HTTP `200`, `code` `0`, msg `移除表情成功`, payload `msg_id`/`emoji` echoed.

### MSG-005: reaction on a nonexistent message

#### Given

A logged-in fixture user A and a msg id that was never seeded.

#### When

A adds and then removes a reaction for that id with `msg_type=c2c`.

#### Then

Both requests return HTTP `200` with envelope code `404` (`ERR_MESSAGE_NOT_FOUND`) and msg
`消息不存在`.

### MSG-006: reaction parameter validation

#### Given

A logged-in fixture user A.

#### When

A posts reaction/add without `msg_id`, then without `emoji`, then with an empty `emoji`,
then with an unsupported `msg_type` and a well-formed msg id.

#### Then

Codes are `400` with messages `缺少消息ID参数`, `缺少emoji参数`, `emoji不能为空` and
`不支持的消息类型` respectively; HTTP stays `200` (business-error convention).

### MSG-007: missing token on history

#### Given

The running application with `api_auth_switch=on` and a registered per-run signing key.

#### When

The client sends `GET /api/v1/msg/history?chat_type=c2c&peer_id=1` with a valid device
signature but no `Authorization` header.

#### Then

- HTTP status is `401`; envelope `code` is `401` and `msg` is `未登录，请先登录`.

## Known gaps / not covered here

- `msg/offline`, `msg/offline_ack`, `msg/read_stats`, `msg/pin`, `msg/forward` and c2g
  history belong to later batches (c2g needs group fixtures from RTF-07).
- `reaction/list` is excluded (see server fact above); `reaction/add` cross-user denial
  (`403 无权限访问该消息`) is left to a later batch with an outsider fixture.
- WebSocket message send is out of scope for RTF-08.

## Execution History

Common Test HTML is written to `.reports/rest/<run-id>/ct/`. Structured, redacted case
evidence is written to `.reports/rest/<run-id>/evidence/`.
