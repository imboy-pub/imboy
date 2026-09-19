# API: `POST /api/v1/passport/login`

## Contract

| Field | Value |
| --- | --- |
| Handler | `passport_handler` with `action => login` |
| Content-Type | `application/json` |
| Authentication | Open endpoint; no Bearer token required |
| Device context | `cos`, `did`, and `dname` request headers |
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

The client posts malformed JSON with `Content-Type: application/json`.

#### Then

- The request does not crash the handler or listener.
- HTTP status is `200` and envelope `code` is `1`.

## Execution History

Common Test HTML is written to `.reports/rest/<run-id>/ct/`. Structured, redacted case evidence is written to `.reports/rest/<run-id>/evidence/`. Generated history is intentionally not committed.

## Evidence

Each case evidence JSON contains the stable Case ID, method/path, redacted request and response, expected/actual result, duration, commit SHA, database name, OTP release, timestamp, and PASS/FAIL result.
