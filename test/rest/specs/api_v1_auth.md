# API: `POST /api/v1/refreshtoken`

## Contract

| Field | Value |
| --- | --- |
| Handler | `passport_handler` with `action => refreshtoken` (route: `src/imboy_router.erl:53`) |
| Content-Type | `application/json` |
| Authentication | JWT-open endpoint (no Bearer token) with mandatory device signature while `api_auth_switch=on` (`auth_middleware_api_v1.erl:74`) |
| Refresh token transport | Request header `imboy-refreshtoken` (**not** the request body — `passport_handler:refreshtoken/1` reads `cowboy_req:header(<<"imboy-refreshtoken">>, ...)`). This contradicts the RTF-06 task card assumption of a body field; the real header contract wins. |
| Device context | `cos`, `did`, `dname`, `vsn`, `pkg`, `sign`, `method` request headers |
| Handler-side throttle | `throttle:check(refreshtoken, Rtk)` — 11/min + 5/s per refresh-token value in the CT environment (`config/sys.local.config`); each case uses its own fixture user + login so the keys never collide |
| Success response | HTTP 200 with `{code=0, msg="success", sv_ts, payload=#{token => <new access token>}}`. The payload carries only a fresh access `token`; the refresh token is **not** rotated. The new token keeps the `did` bound to the original refresh token (E2EE-013). |
| Error convention | Business errors are HTTP 200 + envelope code (`elib_response:error/3`); only `error_with_status` paths raise the real HTTP status |
| Executable suite | `test/rest/suites/api_v1_auth_SUITE.erl` |

The executable contract is the Common Test suite. This Markdown file records intent and review context; it is not a test DSL. Refresh tokens and issued access tokens must never be persisted in evidence — the suite passes pre-redacted request maps to `rest_evidence:verify/4` because the shared redactor matches the exact key `refreshtoken` but not the transport header name `imboy-refreshtoken`.

## Handler decision table (from `passport_handler:refreshtoken/1` + `token_ds:decrypt_token/1`)

| Condition | HTTP | envelope code | msg |
| --- | --- | --- | --- |
| Valid refresh token, user enabled, device row active, session epoch valid | 200 | 0 | `success` |
| Missing/garbled header, bad JWT | 200 | 706 (`ERR_TOKEN_MALFORMED`) | `Invalid token` / `Invalid token.` (branch-dependent) |
| Signature-valid but expired JWT (past the 300 s leeway) | 200 | 705 (`ERR_TOKEN_EXPIRED_REFRESHABLE`) | `Please refresh token` |
| User disabled/deleted (status =< -1) | 200 | 1 | `用户被禁用或已删除` |
| Device row removed | 200 | 401 (`ERR_TOKEN_INVALID`) | `设备已被移除，请重新登录` |
| Session epoch revoked (password change etc.) | 200 | 401 (`ERR_TOKEN_INVALID`) | `会话已吊销，请重新登录` |
| Per-key rate limit exceeded | 429 | 429 | `刷新过于频繁，请稍后再试` (Rate Limit is out of V1 scope per plan §4) |

## Cases

### AUTH-001: valid refresh token exchanges for a fresh access token

#### Given

A fixture user created through `user_repo:create` and logged in through the real `POST /api/v1/passport/login`, so the refresh token is bound to a device `did`. The suite waits until the asynchronously written `user_device` row is visible (`user_device_logic:is_active/2`), because the refresh path rejects tokens whose device row is not active yet.

#### When

The client posts an empty JSON body to `/api/v1/refreshtoken` with valid device-signature headers and the login-issued refresh token in the `imboy-refreshtoken` header.

#### Then

- HTTP status is `200`.
- Envelope `code` is `0`, `msg` is `success`.
- `payload.token` is non-empty; a white-box auxiliary check decrypts it with `token_ds:decrypt_token/1` and confirms `sub=<<"tk">>`, the fixture `uid`, and the same bound `did`.
- The refresh token itself is not rotated and never persisted in evidence.

### AUTH-002: missing refresh token

#### Given

The real route and middleware chain are running with `api_auth_switch=on`; the request carries a valid device signature so the signature gate does not fire first.

#### When

The client posts to `/api/v1/refreshtoken` without the `imboy-refreshtoken` header.

#### Then

- HTTP status is `200` (business-error convention).
- Envelope `code` is `706` (`ERR_TOKEN_MALFORMED`) with an empty `payload`.
- `msg` is one of `Invalid token` / `Invalid token.` depending on whether `token_ds:decrypt_token/1` takes the verify-error or the catch branch; the suite asserts the code, non-empty msg, and empty payload rather than pinning the exact wording.

### AUTH-003: tampered refresh token

#### Given

A fixture user logged in and holding a genuine refresh token; the suite flips the last character of the token so the JWT signature no longer verifies.

#### When

The client posts that tampered value in the `imboy-refreshtoken` header with a valid device signature.

#### Then

- HTTP status is `200`.
- Envelope `code` is `706` (`ERR_TOKEN_MALFORMED`) with an empty `payload` and non-empty `msg`.

### Deliberately not covered here

- **Rate limit (429)**: Rate Limit class is deferred to a dedicated suite per plan §4; the per-key window (11/min in the CT environment) is documented in the contract table.
- **Refresh token used as access token**: sending an access token (`sub=<<"tk">>`) to `/api/v1/refreshtoken` matches neither the `{ok, ..., <<"rtk">>, ...}` nor the `{error, ...}` clause of the handler `case`, which crashes the handler with `case_clause` (Cowboy 500). This is a real-code finding reported to A0, not an asserted contract; no case pins a crash as expected behavior.
- **Device removed / epoch revoked refresh rejections**: deterministic in principle (delete `user_device` row / bump epoch), but they duplicate the Authentication coverage already provided by AUTH-003 and the USER suite's revoked-token evidence (USER-006); kept out to hold the first batch minimal per RTF-06.

## Execution History

Common Test HTML is written to `.reports/rest/<run-id>/ct/`. Structured, redacted case evidence is written to `.reports/rest/<run-id>/evidence/`. Generated history is intentionally not committed.

## Evidence

Each case evidence JSON contains the stable Case ID, method/path, redacted request and response, expected/actual result, duration, commit SHA, database name, OTP release, timestamp, and PASS/FAIL result. The `imboy-refreshtoken` header value is pre-redacted by the suite before `rest_evidence:verify/4` is called (the shared redactor does not match the `imboy-` prefixed header name).
