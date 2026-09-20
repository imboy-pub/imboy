# API: `POST /api/v1/passport/login`

## Contract

| Field | Value |
| --- | --- |
| Handler | `passport_handler` with `action => login` |
| Content-Type | `application/json` |
| Authentication | Open endpoint (no Bearer token) with mandatory device signature while `api_auth_switch=on` |
| Device context | `cos`, `did`, `dname`, `vsn`, `pkg`, `sign`, `method` request headers |
| Response | HTTP 200 with `{code,msg,sv_ts,payload}` envelope |
| Executable suite | `test/rest/suites/api_v1_login_SUITE.erl` |

The executable contract is the Common Test suite. This Markdown file records intent and review context; it is not a test DSL. Passwords and issued tokens must be redacted from evidence.

## Cases

### LOGIN-001: valid credentials

#### Given

A deterministic enabled user exists in the run-scoped scratch PostgreSQL database.

#### When

The client posts the correct account and password with `rsa_encrypt=0` and a unique `did`.

#### Then

- HTTP status is `200`.
- Envelope `code` is `0`.
- `payload.uid` and `payload.account` identify the fixture user.
- `payload.token` and `payload.refreshtoken` are non-empty; their values are never persisted in evidence.

### LOGIN-002: wrong password

#### Given

The same enabled fixture user exists.

#### When

The client posts a wrong password.

#### Then

- HTTP status is `200`, matching the current business-error convention.
- Envelope `code` is `1`, `msg` is non-empty, and `payload` is empty.

### LOGIN-003: unknown account

#### Given

No user exists for `rest-login-user-missing` in the scratch database.

#### When

The client posts that account with a non-empty password.

#### Then

- HTTP status is `200`.
- Envelope `code` is `1`, `msg` is `账号不存在`, and `payload` is empty.

### LOGIN-004: malformed JSON

#### Given

The real Cowboy route and middleware chain are running.

#### When

The client posts malformed JSON with `Content-Type: application/json`, carrying a valid device signature.

#### Then

- The request does not crash the handler or listener.
- HTTP status is `200` and envelope `code` is `1`.

### LOGIN-005: device signature boundary

#### Given

`api_auth_switch` is `on` (the production default) and a per-run signing key
is registered through `app_version_ds:set_sign_key/4`.

#### When

The client posts a valid login request either without the `sign`/`method`
headers, or with a signature computed under a different key.

#### Then

- Both variants are rejected at the middleware boundary before reaching the
  business layer: HTTP `200` with envelope `code` `902`
  (`ERR_SIGNATURE_INVALID`) and message `签名验证失败，请更新客户端`.
- A correctly signed request still passes to the business layer (LOGIN-001).

## Execution History

Common Test HTML is written to `.reports/rest/<run-id>/ct/`. Structured, redacted case evidence is written to `.reports/rest/<run-id>/evidence/`. Generated history is intentionally not committed.

## Evidence

Each case evidence JSON contains the stable Case ID, method/path, redacted request and response, expected/actual result, duration, commit SHA, database name, OTP release, timestamp, and PASS/FAIL result.
