# ImBoy Channel API Contract (v1)

> Last Updated: 2026-03-08  
> Source of truth: `src/imboy_router.erl` + `src/api/channel_handler.erl`  
> Related docs: `doc/api/rest-api.md`, `doc/api/websocket-api-2.md`, `doc/api/envelope.schema.json`

## 1. Scope

This document is the single contract for Channel APIs used by:

1. Server: `imboy`
2. App: `imboyapp`
3. Admin: `imboy-admin-frontend` (channel management pages)

## 2. Global Rules

### 2.1 Envelope

All responses must follow the canonical envelope schema:

```json
{
  "code": 0,
  "msg": "ok",
  "sv_ts": 1771481621000,
  "payload": {}
}
```

Reference: `doc/api/envelope.schema.json`.

### 2.2 ID & HashID Rules

1. Input IDs received from client are HashID in API layer.
2. API layer decodes HashID before DB operations.
3. Output IDs returned to client are encoded HashID.
4. DB stores raw integer IDs.

### 2.3 Parameter Resolution (Hard Rule)

For Channel APIs with path bindings, server uses:

1. Path parameter first.
2. Body/query fallback only for compatibility.

Current helpers in `channel_handler`:

1. `resolve_channel_id/2`
2. `resolve_message_id/2`
3. `resolve_user_id_bin/2`
4. `resolve_reaction_type/2`

App/Admin callers should pass canonical path params and avoid body duplicates.

### 2.4 Channel Content Access Rule (Private/Paid)

For channel content endpoints, server enforces access based on channel type:

1. Public channel (`type=0`): readable by authenticated users.
2. Private channel (`type=1`): only subscribers/admins/creator can read content.
3. Paid channel (`type=2`): only purchased-or-subscribed users/admins/creator can read content.

Covered endpoints:

1. `GET /v1/channel/:channel_id/messages`
2. `POST /v1/channel/:channel_id/read`
3. `POST /v1/channel/:channel_id/message/:message_id/view`
4. `POST /v1/channel/:channel_id/message/:message_id/reaction`
5. `DELETE /v1/channel/:channel_id/message/:message_id/reaction/:reaction_type`

## 3. Core API Matrix

| Action | Method | Path | Query | Body | Payload (success) |
|---|---|---|---|---|---|
| Create channel | `POST` | `/v1/channel/create` | - | `name`, `type`, `description?`, `avatar?`, `custom_id?`, `tags?` | Channel object |
| Get channel | `GET` | `/v1/channel/:channel_id` | - | - | Channel object |
| Get by custom id | `GET` | `/v1/channel/by_custom_id/:custom_id` | - | - | Channel object |
| Update channel | `PUT/POST` | `/v1/channel/:channel_id/update` | - | `name?`, `description?`, `avatar?`, `tags?` | Channel object |
| Delete channel | `POST` | `/v1/channel/:channel_id/delete` | - | - | `{}` |
| Subscribe | `POST` | `/v1/channel/:channel_id/subscribe` | - | - | `{}` |
| Unsubscribe | `POST` | `/v1/channel/:channel_id/unsubscribe` | - | - | `{}` |
| My subscribed channels | `GET` | `/v1/channels/subscribed` | `cursor?`, `limit?` | - | `{list, cursor, limit}` |
| My managed channels | `GET` | `/v1/channels/managed` | - | - | `{list}` |
| Unread summary | `GET` | `/v1/channels/unread/summary` | - | - | `{total_unread, unread_channels, channels[]}` |
| Publish message | `POST` | `/v1/channel/:channel_id/message` | - | `content`, `msg_type`, `payload?` | Message object |
| Message list | `GET` | `/v1/channel/:channel_id/messages` | `cursor?`, `limit?` | - | `{list}` |
| Mark read | `POST` | `/v1/channel/:channel_id/read` | - | `message_id` | `{}` |
| Search channels | `GET` | `/v1/channels/search` | `keyword`, `limit?` | - | `{list}` |
| Discover channels | `GET` | `/v1/channels/discover` | `category?`, `limit?` | - | `{list}` |

## 3.1 Unread Sync Contract

### Pull

Endpoint:

- `GET /v1/channels/unread/summary`

Response payload:

```json
{
  "total_unread": 23,
  "unread_channels": 4,
  "channels": [
    {
      "channel_id": "hashid",
      "unread_count": 10
    }
  ]
}
```

Rules:

- `total_unread`: sum of unread counts in active subscriptions.
- `unread_channels`: count of channels with `unread_count > 0`.
- `channels`: list of per-channel unread entries sorted by unread desc then channel id asc.

### Push

Action: `channel_unread_count`

Payload:

```json
{
  "channel_id": "hashid",
  "unread_count": 12
}
```

Rules:

- publish message should trigger unread push for affected subscribers.
- mark-read should push unread count `0` for the caller channel scope.
- push and pull must read from the same authoritative unread source.
- client may use push for immediate UI updates and pull for cold-start resync.

## 4. Admin/Subscribers/Stats Matrix

| Action | Method | Path | Query | Body | Payload (success) |
|---|---|---|---|---|---|
| Add admin | `POST` | `/v1/channel/:channel_id/admin` | - | `user_id`, `role` | `{}` |
| Remove admin | `DELETE` | `/v1/channel/:channel_id/admin/:user_id` | - | - | `{}` |
| Update admin role | `PUT` | `/v1/channel/:channel_id/admin/:user_id/role` | - | `role` | `{}` |
| Get admins | `GET` | `/v1/channel/:channel_id/admins` | - | - | `{list}` |
| Get subscribers | `GET` | `/v1/channel/:channel_id/subscribers` | `cursor?`, `limit?` | - | `{list, cursor, limit}` |
| Remove subscriber | `DELETE` | `/v1/channel/:channel_id/subscriber/:user_id` | - | - | `{}` |
| Channel stats | `GET` | `/v1/channel/:channel_id/stats` | - | - | Stats object |
| Channel daily stats | `GET` | `/v1/channel/:channel_id/stats/daily` | `days?` | - | `{list}` |
| Record view | `POST` | `/v1/channel/:channel_id/message/:message_id/view` | - | - | `{}` |
| Add reaction | `POST` | `/v1/channel/:channel_id/message/:message_id/reaction` | - | `reaction_type?` | `{}` |
| Remove reaction | `DELETE` | `/v1/channel/:channel_id/message/:message_id/reaction/:reaction_type` | - | - | `{}` |

## 5. Invitation Matrix (Private Channel)

| Action | Method | Path | Body | Payload (success) |
|---|---|---|---|---|
| Create invitation | `POST` | `/v1/channel/:channel_id/invitation` | `invitee_uid` | Invitation object |
| Accept invitation | `POST` | `/v1/channel/invitation/accept` | `invitation_id` | `{}` |
| Reject invitation | `POST` | `/v1/channel/invitation/reject` | `invitation_id` | `{}` |
| My invitations | `GET` | `/v1/channel/invitations/my` | - | `{list}` |
| Sent invitations | `GET` | `/v1/channel/invitations/sent` | - | `{list}` |

## 6. Order Matrix (Paid Channel)

| Action | Method | Path | Body | Payload (success) |
|---|---|---|---|---|
| Create order | `POST` | `/v1/channel/:channel_id/order` | - | Order object |
| Pay order | `POST` | `/v1/channel/order/pay` | `order_no` | `{}` |
| My orders | `GET` | `/v1/channel/orders/my` | - | `{list}` |
| Get order detail | `GET` | `/v1/channel/order/:order_no` | - | Order object |

## 7. Error Semantics (Business)

1. `code = 0` means success.
2. `code != 0` means business failure and `msg` is authoritative.
3. Typical channel errors include:
4. `频道ID不能为空`
5. `用户ID不能为空`
6. `角色值必须在1-3之间`
7. `订单号不能为空`
8. `频道不存在` / `订单不存在`

## 8. Client Alignment Checklist

### 8.1 App (`imboyapp`)

1. `updateAdminRole` uses `/v1/channel/:channel_id/admin/:user_id/role`.
2. Calls with path IDs no longer duplicate `channel_id` in body unless required by contract.
3. Order endpoints are implemented in `ChannelApi`.
4. Unread badge should prefer websocket push and use unread summary pull for cold-start resync.

### 8.2 Admin (`imboy-admin-frontend`)

1. Channel list pagination uses `DataTablePagination`.
2. `onPageSizeChange` implemented.
3. Default `size=10`.
4. Search resets `page=1`.

## 9. Change Policy

Any channel API contract change must update all:

1. `src/imboy_router.erl` route definition
2. `src/api/channel_handler.erl` parameter parsing and validation
3. App API client and tests
4. Admin API/client pages where applicable
5. This document

## 10. Related Docs

- `doc/api/rest-api.md`
- `doc/api/websocket-api-2.md`
- `doc/api/envelope.schema.json`
- `doc/standards/hashid-encoding.md`
- `doc/operations/security.md`
