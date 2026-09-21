# API: `/api/v1/conversation/mine` | `/api/v1/conversation/pin` | `/api/v1/conversation/unpin`

## Contract

| Field | Value |
| --- | --- |
| Handler | `conversation_handler` with `action => mine / pin_conversation / unpin_conversation` |
| Method | `GET /api/v1/conversation/mine`; `POST /api/v1/conversation/pin`; `POST /api/v1/conversation/unpin` |
| Content-Type | `application/json` |
| Authentication | JWT (`Authorization: Bearer <token>`) + mandatory device signature while `api_auth_switch=on` |
| Device context | `cos`, `did`, `dname`, `vsn`, `pkg`, `sign`, `method` request headers |
| Response | Envelope `{code,msg,sv_ts,payload}`; business errors keep HTTP 200, auth errors use real HTTP status |
| Executable suite | `test/rest/suites/api_v1_conversation_SUITE.erl` |

The executable contract is the Common Test suite. This Markdown records intent and review
context; it is not a test DSL. Tokens must be redacted from evidence.

## Server behaviour facts (read from source at Base SHA)

- `mine` calls `conversation_logic:list/2` with `limit=1000` and the optional
  `last_server_ts` query param; payload is `#{list => [Conversation]}`.
- Each `Conversation` entry carries `conversation_id` (peer uid or group id, integer),
  `conversation_type` (`c2c`/`c2g`), `server_ts` (epoch milliseconds integer),
  `last_msg_id`, `last_msg` (decoded payload map) and `is_pinned` (boolean from
  `conversation_pin_logic:is_pinned/3`).
- `pin`/`unpin` normalize `conversation_id` from the TSID string contract back to integer
  (`conversation_handler:normalize_conversation_id/1`). Non-integer or `<= 0` ids reach the
  logic guard and are rejected with `会话ID无效` mapped to envelope code `500`
  (`ERR_OPERATION_FAILED`) — the handler maps every `{error, Msg}` from
  `conversation_pin_logic` to code `500`, not 4xx.
- `pin` does NOT verify that the conversation actually exists: any positive integer id is
  accepted and written to `conversation_pin` (repeat pin is idempotent via
  `conversation_pin_ds:is_conversation_pinned/3`). There is therefore no Not Found branch on
  this endpoint; the suite asserts the real idempotent-success behaviour.
- `unpin` succeeds for any positive integer id and returns `payload = {updated: true}`.

## Cases

### CONV-001: mine happy path

#### Given

Fixture users A and B exist and A is logged in. One real c2c message row from A to B is
seeded through the production write path `msg_c2c_ds:write_msg/6` (plain text payload, no
E2EE envelope — no real encrypted traffic is generated).

#### When

The client sends `GET /api/v1/conversation/mine` with A's bearer token and a valid device
signature.

#### Then

- HTTP status is `200`, envelope `code` is `0`.
- `payload.list` is a list containing the A/B conversation entry.
- The entry has `conversation_type` `c2c`, `conversation_id` equal to B's uid,
  `last_msg_id` equal to the seeded msg id, an integer `server_ts` and a boolean
  `is_pinned`.

### CONV-002: pin / unpin cycle

#### Given

The same fixture pair A and B with a seeded c2c message (same factory as CONV-001, new
unique ids per run).

#### When

A pins the conversation (`POST /api/v1/conversation/pin` with
`conversation_id = <B uid as TSID string>`, `type = c2c`), then unpins it
(`POST /api/v1/conversation/unpin`), reading `mine` after each mutation.

#### Then

- Pin returns HTTP `200`, `code` `0`, empty payload.
- After pin, the `mine` entry for B has `is_pinned = true`.
- Unpin returns HTTP `200`, `code` `0`, `payload = {updated: true}`.
- After unpin, the `mine` entry has `is_pinned = false`.

### CONV-003: pin is idempotent

#### Given

Fixture pair A and B.

#### When

A pins the same conversation twice in a row.

#### Then

Both requests return HTTP `200` with `code` `0` (the second pin hits the
already-pinned fast path in `conversation_pin_logic:pin/3`).

### CONV-004: missing token

#### Given

The running application with `api_auth_switch=on` and a registered per-run signing key.

#### When

The client sends `GET /api/v1/conversation/mine` with a valid device signature but no
`Authorization` header.

#### Then

- The request is stopped at the middleware auth boundary: HTTP status `401`.
- Envelope `code` is `401` (`ERR_TOKEN_MISSING`) and `msg` is `未登录，请先登录`.

### CONV-005: invalid conversation id

#### Given

A logged-in fixture user A.

#### When

A pins with `conversation_id = "not-a-number"`, and separately with `conversation_id = 0`.

#### Then

Both variants are rejected by the logic guard: HTTP `200`, envelope `code` `500`
(`ERR_OPERATION_FAILED`), `msg` `会话ID无效`.

### CONV-006: pin a nonexistent conversation

#### Given

A logged-in fixture user A and a conversation id that no fixture ever created (random
large positive integer).

#### When

A pins that id, then unpins it.

#### Then

- Pin returns HTTP `200` with `code` `0`: the current implementation persists the pin row
  without checking conversation existence, so there is no Not Found branch (documented
  server fact above, not a 404).
- Unpin returns HTTP `200`, `code` `0`, `payload = {updated: true}`.

## Known gaps / not covered here

- `conversation/online` (admin/ops flavour), `pinned`, `delete`, `restore` belong to later
  batches; RTF-08 scope is mine/pin/unpin.
- c2g conversation entries require group fixtures (RTF-07 domain) and are not seeded here.

## Execution History

Common Test HTML is written to `.reports/rest/<run-id>/ct/`. Structured, redacted case
evidence is written to `.reports/rest/<run-id>/evidence/`.
