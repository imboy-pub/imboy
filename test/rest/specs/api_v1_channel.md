# API: `/api/v1/channel/create` | `/api/v1/channel/:channel_id` | `/api/v1/channel/:channel_id/subscribe` | `/api/v1/channel/:channel_id/messages`

## Contract

| Field | Value |
| --- | --- |
| Handler | `channel_handler` with `action => create / show / subscribe / messages` |
| Method | `POST /api/v1/channel/create`; `GET /api/v1/channel/:channel_id`; `POST /api/v1/channel/:channel_id/subscribe`; `GET /api/v1/channel/:channel_id/messages` |
| Content-Type | `application/json` |
| Authentication | JWT (`Authorization: Bearer <token>`) + mandatory device signature while `api_auth_switch=on` |
| Dynamic path | `:channel_id` always comes from a fixture-created channel (never hardcoded) |
| Response | Envelope `{code,msg,sv_ts,payload}`; business errors keep HTTP 200, auth errors use real HTTP status |
| Executable suite | `test/rest/suites/api_v1_channel_SUITE.erl` |

The executable contract is the Common Test suite. This Markdown records intent and review
context; it is not a test DSL. Tokens must be redacted from evidence.

## Server behaviour facts (read from source at Base SHA)

- `create` with an empty `name` returns code `1` `频道名称不能为空`. Success returns the
  channel through `?CHANNEL_SAFE_COLUMNS`: `id, name, description, avatar, custom_id,
  creator_uid, subscriber_count, is_verified, tags, visibility, access_type, join_policy,
  created_at, updated_at`. The internal `status` column is never exposed.
- `show` enriches the safe columns with `user_role`, `is_subscribed` and `has_purchased`;
  the creator always has `user_role = 3` and `is_subscribed = true` (creator-is-subscribed
  rule). A nonexistent or non-numeric channel id yields code `1` with msg `频道不存在` —
  the current implementation does NOT use HTTP 404 / envelope 404 for unknown channels;
  unknown resources collapse into the generic business error (documented fact).
- `subscribe` on the default `join_policy=0` (open) channel subscribes through an idempotent
  `upsert_active`, so repeating subscribe keeps returning `code 0` with empty payload.
  Unknown channel id also collapses to `频道不存在` / code `1`.
- `messages` requires `ensure_channel_content_access/2`: public (`visibility=0`) channels
  are readable without subscribing; private (`visibility=1`) needs subscription; paid
  (`access_type=1`) needs purchase. Payload is `#{list => [...]}`; an empty channel returns
  an empty list.

## Cases

### CHANNEL-001: create happy path

#### Given

A logged-in fixture user A with fewer than 20 managed channels.

#### When

A posts `POST /api/v1/channel/create` with a run-unique `name` and a `description`.

#### Then

- HTTP `200`, `code` `0`.
- Payload echoes `name`/`description`, `creator_uid` equals A's uid, defaults
  `visibility = 0`, `access_type = 0`, `join_policy = 0`, `is_verified` false, and an id
  that is a positive integer (or the TSID string contract form).

### CHANNEL-002: show by dynamic channel id

#### Given

The channel created in this case via the create endpoint (id taken from its response).

#### When

A sends `GET /api/v1/channel/<channel_id>` with a valid device signature.

#### Then

- HTTP `200`, `code` `0`; `payload.id` matches the created id and `payload.name` matches.
- `payload.user_role` is `3`, `payload.is_subscribed` is `true`, `payload.has_purchased`
  is `false`.

### CHANNEL-003: subscribe is idempotent

#### Given

The channel created in this case (default `join_policy=0`).

#### When

A subscribes twice via `POST /api/v1/channel/<channel_id>/subscribe`.

#### Then

Both requests return HTTP `200`, `code` `0`, empty payload (upsert-active semantics).

### CHANNEL-004: messages of an empty channel

#### Given

The channel created in this case; no message was ever published.

#### When

A sends `GET /api/v1/channel/<channel_id>/messages` (default limit) and a second request
with `limit=1`.

#### Then

Both return HTTP `200`, `code` `0`, `payload.list` exactly `[]`.

### CHANNEL-005: nonexistent channel

#### Given

A logged-in fixture user A and a channel id that no fixture created (random large positive
integer).

#### When

A shows, subscribes to and lists messages of that id.

#### Then

All three return HTTP `200` with envelope `code` `1` and msg `频道不存在` — the real
not-found convention for this domain (no HTTP 404, documented server fact above).

### CHANNEL-006: missing token on create

#### Given

The running application with `api_auth_switch=on` and a registered per-run signing key.

#### When

The client posts a valid create body with a device signature but no `Authorization` header.

#### Then

- HTTP status is `401`; envelope `code` is `401` and `msg` is `未登录，请先登录`.

### CHANNEL-007: create with empty name

#### Given

A logged-in fixture user A.

#### When

A posts `POST /api/v1/channel/create` with `name = ""`.

#### Then

- HTTP `200`, `code` `1`, msg `频道名称不能为空`.

## Known gaps / not covered here

- Publish/message/reaction/comment/admin/order/webhook channel endpoints are later batches
  (paid access and webhooks need external dependencies per RTF-08).
- Private/paid access gates (visibility=1 / access_type=1) need order fixtures and are
  deferred; only the default open channel policy is exercised here.

## Execution History

Common Test HTML is written to `.reports/rest/<run-id>/ct/`. Structured, redacted case
evidence is written to `.reports/rest/<run-id>/evidence/`.
