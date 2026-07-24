# E2EE Social Recovery Server-Persisted Shard Contract (v1)

> Last Updated: 2026-03-08  
> Status: Long-lived protocol contract  
> Scope: Social recovery shard persistence and retrieval contract  
> Source of truth: `src/imboy_router.erl` + `src/api/e2ee_social_handler.erl` + `src/logic/e2ee_social_logic.erl`  
> Note: `v1` refers to the contract version, not the application release version.  
> Related docs: `docs/reference/rest-api.md`, `docs/reference/websocket-api-2.md`, `docs/operations/security.md`

## 1. Scope

This contract defines the backend behavior for social recovery shards with server persistence.

Covered APIs:

- `POST /api/v1/e2ee/social/create_shards`
- `GET /api/v1/e2ee/social/shards`
- `GET /api/v1/e2ee/social/proxy_shards`
- `POST /api/v1/e2ee/social/decrypt_shard`

## 2. Core Decision

- Social recovery shards are server-persisted records (`e2ee_social_shards`).
- Server stores encrypted shard payload; plaintext shard remains inaccessible without proxy private key.

## 3. Data Rules

- On shard creation:
  - server validates threshold/proxy constraints.
  - server encrypts each shard with proxy public key.
  - server writes each shard record with status `active`.
- `GET /api/v1/e2ee/social/shards` returns user-owned shard records by `key_version`.
- `GET /api/v1/e2ee/social/proxy_shards` returns active proxy-owned shard records.

## 4. Security Constraints

- Decrypt API requires proxy identity match (`proxy_uid` must equal caller).
- Shard status must be active for decrypt.
- Missing private key and decryption failures use existing error code mapping in handler.

## 5. Recovery Decision Rules

- Recovery availability checks should use trusted contacts and persisted shard state.
- Recovery logic must not call non-existent DS APIs; use `list_trusted_contacts/1` and shard DS/repo methods.

## 6. `decrypt_shard` API Detail

### Endpoint

- Method: `POST`
- Path: `/api/v1/e2ee/social/decrypt_shard`
- Auth: required (`current_uid` from auth middleware)

### Request

```json
{
  "shard_id": "uuid-string"
}
```

`shard_id` is required.

### Success Response

```json
{
  "code": 0,
  "msg": "success",
  "payload": {
    "decrypted_shard": "base64-or-json-shard"
  }
}
```

### Error Contract

| Internal reason | API message | API code |
|---|---|---|
| missing `shard_id` | `缺少 shard_id 参数` | `400` (`ERR_BAD_REQUEST`) |
| `not_proxy` | `无权解密此分片` | `403` (`ERR_FORBIDDEN`) |
| `shard_not_active` | `分片不可用` | `400` (`ERR_BAD_REQUEST`) |
| `shard_not_found` / `not_found` | `分片不存在` | `404` (`ERR_NOT_FOUND`) |
| `private_key_not_found` / `device_not_found` | `密钥不存在` | `5054` (`ERR_E2EE_KEY_NOT_FOUND`) |
| `decryption_failed` | `解密失败` | `5053` (`ERR_E2EE_DECRYPTION_FAILED`) |
| shard payload missing (`encrypted_shard` and `encrypted_data` both empty) | `分片数据缺失` | `500` (`ERR_INTERNAL_SERVER_ERROR`) |
| fallback unknown error | `操作失败，请稍后重试` (or mapped message) | `500` (`ERR_INTERNAL_SERVER_ERROR`) |

### Field Compatibility

For backward compatibility, server reads encrypted payload in this order:

1. `encrypted_shard` (canonical field)
2. `encrypted_data` (legacy fallback)

Clients should send/store `encrypted_shard` as the canonical field.

## 7. Change Policy

Any E2EE social shard contract change must update all:

1. `src/imboy_router.erl` route definition
2. `src/api/e2ee_social_handler.erl` request parsing and error mapping
3. `src/logic/e2ee_social_logic.erl` flow and validation
4. App/client integration and tests where applicable
5. This document

## 8. Related Docs

- `docs/reference/rest-api.md`
- `docs/reference/websocket-api-2.md`
- `docs/operations/security.md`
- `docs/standards/error-codes.md`
