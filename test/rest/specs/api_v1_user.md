# API: `/api/v1/user/show` · `/api/v1/user/update` · `/api/v1/user/change_password`

## Contract

| Field | Value |
| --- | --- |
| Handler | `user_handler` with `action => show` / `update` / `change_password` (`src/imboy_router.erl:136-141`) |
| Content-Type | `application/json` |
| `user/show` authentication | **Public**: listed in `imboy_router:open/0` (line 1402). With `api_auth_switch=on` the middleware skips both `verify_sign` and the JWT gate for open paths, so no Bearer token and no device signature are required. It reads `?id=<integer>` from the query string (`imboy_error:validate_id/2` accepts plain integers), and returns the minimized public column set `id, nickname, avatar, background, sign` (`account_type` only for AI agents; `account` is deliberately withheld — PII narrowing comment in `user_handler:show/2`). |
| `user/update`, `user/change_password` authentication | Device signature gate (`verify_sign`, 902 when wrong) **followed by** the JWT gate (`auth_ds:condition/5` → `do_authorization`). The signature gate runs first, so Authentication cases must carry a valid signature to reach the JWT boundary. |
| JWT gate outcomes | Missing `Authorization` header → HTTP 401 + envelope code 401 (`ERR_TOKEN_MISSING`), msg `未登录，请先登录`. Invalid-signature token → HTTP 401 + code 706 (`ERR_TOKEN_MALFORMED`), msg `Invalid token`/`Invalid token.`. Expired token (past 300 s leeway) → HTTP 401 + code 705 (`ERR_TOKEN_EXPIRED_REFRESHABLE`), msg `Please refresh token`. Epoch-revoked token → HTTP 401 + code 401, msg `会话已吊销，请重新登录`. Device-removed token → HTTP 401 + code 401, msg `设备已被移除，请重新登录`. (`auth_middleware_api_v1.erl:95-101`, `auth_ds.erl:186-243`) |
| `user/show` request | `GET /api/v1/user/show?id=<uid>`; missing `id` → HTTP 200, code 1, msg `缺少ID参数`; `id=0` or non-numeric → HTTP 200, code 1, msg `ID 格式有误` |
| `user/update` request | `POST` body `{field, value, code?}`; allowed fields via `user_agg:validate_update/2` (passthrough: `sign, nickname, avatar, background, region, birthday, profession, school, interests`; `gender`/`allow_search`/settings/`email`/`mobile` have dedicated branches). The acting user is always the JWT-injected `current_uid`; there is no body parameter that can target another user. Success: HTTP 200, code 0, msg `success.`, payload `{}`. |
| `user/change_password` request | `POST` body `{existing_pwd, new_pwd, rsa_encrypt}`. `rsa_encrypt` defaults to `"1"` (RSA) and must be `"0"` for plaintext under the current client contract. Success: HTTP 200, code 0, msg `success`, payload `{}`. Success bumps the session epoch in the same transaction (`user_logic:update_password_with_log/4` → `auth_session_ds:bump_in_tx/2` + `kick_all_sessions/1`), which invalidates every previously issued token for that user. |
| Executable suite | `test/rest/suites/api_v1_user_SUITE.erl` |

The executable contract is the Common Test suite. This Markdown file records intent and review context. Passwords and issued tokens must never be persisted in evidence: the suite passes pre-redacted request maps to `rest_evidence:verify/4` because the shared redactor matches `pwd`/`password` but not `existing_pwd`/`new_pwd`.

## Cases

### USER-001: valid token reads public profile via user/show

#### Given

A fixture user exists and holds a login-issued token bound to an active device row (the suite waits for the asynchronous `user_device` write).

#### When

The client sends `GET /api/v1/user/show?id=<own uid>` with the device-signature headers and `Authorization: Bearer <token>` (the shape a real client sends).

#### Then

- HTTP status is `200`, envelope `code` is `0`, `msg` is `success`.
- `payload.id` equals the fixture uid rendered as a binary string (`convert_user_id/1`).
- `payload.nickname` equals the fixture nickname.
- `payload` carries no `account`, `mobile`, `email`, or `account_type` key (public-field minimization for non-agent accounts).

### USER-002: missing Authorization header on user/update

#### Given

A logged-in fixture user; `api_auth_switch=on`; the request carries a **valid** device signature so the signature gate passes and the JWT gate is the boundary under test.

#### When

The client posts a profile update body to `/api/v1/user/update` without the `Authorization` header.

#### Then

- HTTP status is `401` (real status, not 200 — `do_authorization(undefined, ...)` uses `error_with_status`).
- Envelope `code` is `401` (`ERR_TOKEN_MISSING`), `msg` is `未登录，请先登录`, `payload` is empty.

### USER-003: tampered token on user/update

#### Given

A logged-in fixture user; the suite flips the last character of the genuine access token so the JWT signature no longer verifies.

#### When

The client posts a profile update body with `Authorization: Bearer <tampered>` and a valid device signature.

#### Then

- HTTP status is `401`.
- Envelope `code` is `706` (`ERR_TOKEN_MALFORMED`) with empty `payload` and non-empty `msg` (`Invalid token` / `Invalid token.`, branch-dependent).

### USER-EXPIRED: expired token on user/update (deterministic construction)

#### Given

A logged-in fixture user and the running service's JWT key read via `config_ds:env(jwt_key, <<>>)`. `token_ds` signs with `jwerl:sign(..., hs256, JwtKey)` and verifies with a 300 s `exp_leeway`, so a token signed **now** with `exp = now - 400` is signature-valid but reliably expired. This is deterministic — no sleeping for real TTLs — and the exact mechanism `token_ds` itself uses (RTF-06 required the token generation/expiry mechanism be investigated in `src/ds/token_ds.erl` before attempting this class).

#### When

The client posts a profile update body with `Authorization: Bearer <expired token>` and a valid device signature.

#### Then

- HTTP status is `401`.
- Envelope `code` is `705` (`ERR_TOKEN_EXPIRED_REFRESHABLE`), `msg` is `Please refresh token`, `payload` is empty.
- The middleware stops the request before the handler, so the update body never reaches `user_logic` and no data changes.

### USER-005: cross-user isolation on user/update

#### Given

Two independent fixture users A and B, each with its own device and token.

#### When

User A posts an update for `field=nickname` carrying an injected `uid` field pointing at B (an attacker-style attempt to redirect the write), authenticated as A.

#### Then

- The request succeeds: HTTP 200, code 0, msg `success.` — `user_handler:update/2` derives the target from the JWT-injected `current_uid` only; the body `uid` is ignored.
- Read-back through `GET /api/v1/user/show?id=<A>` shows the new nickname.
- Read-back through `GET /api/v1/user/show?id=<B>` still shows B's original nickname: the write never touched B.
- This documents the real semantics: `user/show` is a public lookup-by-id endpoint (it returns whoever `?id` names, not "the token owner"), so cross-user isolation is asserted where it actually lives — the update path cannot be aimed at another user, and the public read exposes only the minimized field set (USER-001).

### USER-006: change_password lifecycle

#### Given

A logged-in fixture user whose token, device row, and session epoch (default 1) are all active.

#### When

The client posts `{existing_pwd, new_pwd, rsa_encrypt="0"}` to `/api/v1/user/change_password` with a valid signature and Authorization header.

#### Then

- The change succeeds: HTTP 200, code 0, msg `success`, payload empty.
- The old access token is immediately rejected on `/api/v1/user/update` with HTTP 401, code 401, msg `会话已吊销，请重新登录` (the epoch bump invalidates pre-change tokens; `auth_session_ds:revoked/2` returns true for `token epoch < current epoch`).
- Login with the old password returns HTTP 200, envelope code 1 (`passport_logic:verify_user/3` failure), non-empty msg.
- Login with the new password returns HTTP 200, code 0, and a non-empty `payload.token`.
- The plaintext passwords exist only in the in-memory request; the evidence request map is pre-redacted to `[REDACTED]` placeholders.

## Deliberately not covered here

- **Signature boundary on user/update (902)**: already covered as LOGIN-005 against the same `auth_ds:verify_sign/2` gate; USER-002/003/USER-EXPIRED intentionally carry valid signatures so they exercise the JWT boundary, not the signature boundary.
- **Rate limit (429)**: deferred to a dedicated suite per plan §4. The CT environment raises `passport_per_ip` to 120/min (`config/sys.local.config`), so the fixture logins in this suite stay far below the production default of 10/min; noted because USER-006 performs three logins.
- **Malformed JSON / invalid field type on user/update**: covered by the login golden suite at the middleware/parsing layer (LOGIN-004) and by `user_agg` EUnit for field validation; the REST batch keeps to the plan's V1 category minimum for this domain.

## Execution History

Common Test HTML is written to `.reports/rest/<run-id>/ct/`. Structured, redacted case evidence is written to `.reports/rest/<run-id>/evidence/`. Generated history is intentionally not committed.

## Evidence

Each case evidence JSON contains the stable Case ID, method/path, redacted request and response, expected/actual result, duration, commit SHA, database name, OTP release, timestamp, and PASS/FAIL result. `existing_pwd`/`new_pwd` and all Bearer/refresh tokens are redacted before `rest_evidence:verify/4` is called.
