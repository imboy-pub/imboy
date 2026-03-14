# Capability Enforcement Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Enforce stage 2 capability rules in policy aggregation, message search, and admin message audit/export flows without changing unrelated routes.

**Architecture:** Centralize capability normalization inside `imboy_policy`, then let `fts_handler` and `adm_message_handler` consume normalized results instead of open-coding capability logic. Search/export gates should return the existing uniform `FEATURE_DISABLED` business response, while admin message audit should distinguish between `none`, `metadata`, and `full` by blocking access or redacting `payload`.

**Tech Stack:** Erlang, Cowboy REST handlers, `elib_response`, EUnit, existing `src/lib/` policy modules

---

## Scope

This plan covers stage 2 only:

- normalize conflicting `capabilities` into effective values;
- enforce `message_search` on `fts_handler`;
- enforce `message_export` on `adm_message_handler`;
- enforce `audit_mode` on admin message list/detail/export payload visibility.

This plan does not cover:

- message storage / ACK deletion semantics;
- repo-level schema changes;
- Flutter-side capability-aware UI handling;
- E2EE transport handler changes.

## Resume Rule

If the session stops:

1. Open this file.
2. Start from the first unfinished task.
3. Re-run the targeted tests from that task before making more changes.

Resume prompt:

`Please continue executing /Users/leeyi/project/imboy.pub/imboy/docs/plans/2026-03-14-capability-enforcement.md from the first unfinished task.`

### Task 1: Lock Stage 2 Behavior With Tests

**Files:**
- Modify: `test/lib/imboy_policy_tests.erl`
- Modify: `test/api/fts_handler_tests.erl`
- Modify: `test/adm/adm_message_handler_tests.erl`

**Step 1: Add policy normalization tests**

Extend `test/lib/imboy_policy_tests.erl` with cases covering:

- `storage_mode = secure_e2ee` forces `message_search = false`
- `storage_mode = secure_e2ee` forces `message_export = false`
- `storage_mode = secure_e2ee` downgrades `audit_mode` from `full` to `metadata`
- `e2ee_mode = required` downgrades body visibility by preventing `audit_mode = full`

**Step 2: Add message search capability tests**

Extend `test/api/fts_handler_tests.erl` with:

- `msg_search_disabled_by_capability_test_`
  - mock `imboy_policy:message_search_enabled/0 -> false`
  - assert `fts_logic:search_msg/6` is not called
  - assert uniform `ERR_FEATURE_DISABLED` path is used
- `msg_search_enabled_test_`
  - mock `imboy_policy:message_search_enabled/0 -> true`
  - assert `fts_logic:search_msg/6` is called and success response is returned

**Step 3: Add admin audit/export capability tests**

Extend `test/adm/adm_message_handler_tests.erl` with:

- `list_redacts_payload_when_audit_mode_metadata_test_`
- `detail_redacts_payload_when_audit_mode_metadata_test_`
- `list_disabled_when_audit_mode_none_test_`
- `export_disabled_when_message_export_false_test_`

For list/detail redaction tests:

- mock `imboy_policy:message_audit_mode/0 -> metadata`
- assert payload becomes empty binary or redacted value
- assert metadata fields still exist

For audit none / export false:

- assert uniform `ERR_FEATURE_DISABLED`
- assert DB query or stream methods are not called when gate blocks

**Step 4: Run failing tests**

Run:

```bash
make eunit EUNIT_MODS='imboy_policy_tests fts_handler_tests adm_message_handler_tests'
```

Expected: FAIL on the new stage 2 assertions.

### Task 2: Normalize Effective Capabilities In Policy

**Files:**
- Modify: `src/lib/imboy_policy.erl`
- Test: `test/lib/imboy_policy_tests.erl`

**Step 1: Add capability helper exports**

Extend exports with:

- `message_search_enabled/0`
- `message_export_enabled/0`
- `message_audit_mode/0`
- `message_audit_enabled/0`
- `message_body_visible/0`

**Step 2: Normalize merged capabilities**

Inside `effective_capabilities/0`, after merging defaults and overrides:

- normalize `storage_mode`
- normalize `e2ee_mode`
- normalize `audit_mode`
- coerce invalid combinations:
  - `secure_e2ee` => search false, export false, `audit_mode` not full
  - `e2ee_mode=required` => search false, `audit_mode` not full

Recommended downgrade rule:

- if `audit_mode = full` but body visibility is disallowed, downgrade to `metadata`

**Step 3: Implement helper functions**

Implement:

- `message_search_enabled/0` from normalized capabilities
- `message_export_enabled/0`
- `message_audit_mode/0`
- `message_audit_enabled/0` where `audit_mode =/= none`
- `message_body_visible/0` where `audit_mode =:= full`

**Step 4: Run tests**

Run:

```bash
make eunit EUNIT_MODS='imboy_policy_tests'
```

Expected: PASS

### Task 3: Enforce Message Search Capability

**Files:**
- Modify: `src/api/fts_handler.erl`
- Test: `test/api/fts_handler_tests.erl`

**Step 1: Add capability gate to `msg/2`**

Before calling `fts_logic:search_msg/6`:

- if `imboy_policy:message_search_enabled/0 = false`
  - return `elib_response:error(Req0, imboy_error:error_msg(?ERR_FEATURE_DISABLED), ?ERR_FEATURE_DISABLED)`
- otherwise continue existing logic unchanged

Do not gate `user_search` or `recently_user`.

**Step 2: Add required include**

Ensure `fts_handler.erl` includes:

- `error_code.hrl`

**Step 3: Run tests**

Run:

```bash
make eunit EUNIT_MODS='fts_handler_tests'
```

Expected: PASS

### Task 4: Enforce Admin Audit And Export Capability

**Files:**
- Modify: `src/adm/adm_message_handler.erl`
- Test: `test/adm/adm_message_handler_tests.erl`

**Step 1: Add internal helpers**

Add helper functions:

- `ensure_message_audit_enabled/1`
- `ensure_message_export_enabled/1`
- `sanitize_row_by_audit_mode/1`
- `sanitize_rows_by_audit_mode/1`

Recommended behavior:

- `audit_mode = none`
  - list/detail return uniform `ERR_FEATURE_DISABLED`
- `audit_mode = metadata`
  - list/detail allowed
  - `payload` redacted to `<<>>`
- `audit_mode = full`
  - list/detail unchanged
- `message_export = false`
  - export returns uniform `ERR_FEATURE_DISABLED`
- export when allowed but `audit_mode = metadata`
  - exported rows contain redacted `payload`

**Step 2: Wire list/detail/export**

- `list/3` checks audit gate before DB work
- `detail/3` checks audit gate before DB work
- `export/3` checks export gate before starting stream
- list/detail/export sanitize rows before serializing output

**Step 3: Preserve existing non-capability behavior**

Do not change:

- SQL shape except for using sanitized rows after query
- pagination structure
- CSV header ordering
- hashid normalization for IDs

**Step 4: Run tests**

Run:

```bash
make eunit EUNIT_MODS='adm_message_handler_tests'
```

Expected: PASS

### Task 5: Run Final Verification

**Files:**
- Verify only

**Step 1: Run full stage 2 targeted suite**

Run:

```bash
make eunit EUNIT_MODS='imboy_policy_tests fts_handler_tests adm_message_handler_tests'
```

Expected: PASS

**Step 2: Run stage 1 compatibility smoke**

Run:

```bash
make eunit EUNIT_MODS='imboy_feature_tests app_feature_handler_tests adm_admin_feature_config_tests'
```

Expected: PASS

**Step 3: Run feature gate smoke modules**

Run:

```bash
make eunit EUNIT_MODS='feature_gate_public_handler_tests feature_gate_admin_handler_tests'
```

Expected: PASS

## Notes

- Use `ERR_FEATURE_DISABLED` for deployment-level capability gates.
- Keep capability evaluation centralized in `imboy_policy`.
- Prefer redaction over new payload shapes when only body visibility changes.
- Do not start the storage/ACK phase in this plan.
