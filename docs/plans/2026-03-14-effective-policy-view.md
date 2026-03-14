# Effective Policy View Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Start stage 3 by exposing a JSON-friendly `effective_policy` view to App and Admin without breaking the existing flat feature matrix endpoints.

**Architecture:** Keep `/v1/app/features` and `/adm/admin/config/features` unchanged for compatibility. Add a new `imboy_policy:effective_view/0` adapter that converts the internal atom-based policy structure into a JSON-friendly payload, then expose that payload through new read-only App/Admin endpoints.

**Tech Stack:** Erlang, Cowboy REST handlers, `imboy_policy`, EUnit, existing router and admin permission checks

---

## Scope

This batch covers:

- add a JSON-friendly policy payload adapter in `imboy_policy`;
- add a public App endpoint for effective policy;
- add an Admin endpoint for effective policy with existing `settings:view` permission;
- wire router entries for the new endpoints;
- keep existing feature-matrix endpoints unchanged.

This batch does not cover:

- Flutter-side policy consumption;
- Admin frontend menu refactor;
- route-level auto-enforcement derived from plugin manifests;
- plugin dependency enforcement beyond current `enabled` calculation.

## Resume Rule

If the session stops:

1. Open this file.
2. Start from the first unfinished task.
3. Re-run the targeted tests from that task before making more changes.

Resume prompt:

`Please continue executing /Users/leeyi/project/imboy.pub/imboy/docs/plans/2026-03-14-effective-policy-view.md from the first unfinished task.`

### Task 1: Lock Policy View Behavior With Tests

**Files:**
- Modify: `test/lib/imboy_policy_tests.erl`
- Modify: `test/api/app_feature_handler_tests.erl`
- Modify: `test/adm/adm_admin_feature_config_tests.erl`

**Step 1: Add policy view adapter tests**

Extend `test/lib/imboy_policy_tests.erl` with a case covering:

- `effective_view/0` returns top-level binary keys:
  - `<<"profile">>`
  - `<<"capabilities">>`
  - `<<"features">>`
  - `<<"plugins">>`
- enum-like atom values are rendered as binaries:
  - `enterprise -> <<"enterprise">>`
  - `archived -> <<"archived">>`
  - `full -> <<"full">>`
- plugin manifests remain consumable:
  - plugin names become binary keys;
  - `kind`, `feature_keys`, `app_entries`, `admin_entries`, `api_handlers` are JSON-friendly.

**Step 2: Add App policy handler tests**

Extend `test/api/app_feature_handler_tests.erl` with:

- `init_policy_returns_effective_policy_payload_test_`
  - mock `imboy_policy:effective_view/0`
  - assert success response payload matches the returned policy map

**Step 3: Add Admin policy handler tests**

Extend `test/adm/adm_admin_feature_config_tests.erl` with:

- `init_config_policy_success_test_`
  - require `settings:view`
  - mock `imboy_policy:effective_view/0`
  - assert success response payload matches
- `init_config_policy_forbidden_without_settings_permission_test_`
  - assert permission denial happens before policy call

**Step 4: Run targeted tests**

Run:

```bash
make eunit EUNIT_MODS='imboy_policy_tests app_feature_handler_tests adm_admin_feature_config_tests'
```

Expected: FAIL on missing policy view behavior and handler actions.

### Task 2: Add JSON-Friendly Policy Adapter

**Files:**
- Modify: `src/lib/imboy_policy.erl`
- Test: `test/lib/imboy_policy_tests.erl`

**Step 1: Add adapter export**

Add:

- `effective_view/0`

**Step 2: Convert internal policy into API payload**

Implement a recursive conversion that:

- keeps booleans and integers unchanged;
- converts atom map keys to UTF-8 binaries;
- converts enum-like atom values to UTF-8 binaries;
- recursively converts nested maps and lists.

Use the existing `effective/0` as the internal source of truth.

**Step 3: Run tests**

Run:

```bash
make eunit EUNIT_MODS='imboy_policy_tests'
```

Expected: PASS

### Task 3: Expose Policy View On App And Admin Endpoints

**Files:**
- Modify: `src/api/app_feature_handler.erl`
- Modify: `src/adm/adm_admin_handler.erl`
- Modify: `src/imboy_router.erl`
- Test: `test/api/app_feature_handler_tests.erl`
- Test: `test/adm/adm_admin_feature_config_tests.erl`

**Step 1: Add App policy action**

In `app_feature_handler`:

- add action `policy`
- `GET` returns `elib_response:success(Req0, imboy_policy:effective_view())`

**Step 2: Add Admin policy action**

In `adm_admin_handler`:

- add action `config_policy`
- require the same `settings:view` permission as `config_features`
- return `imboy_policy:effective_view()`

**Step 3: Add router entries**

Add:

- `/v1/app/policy`
- `/adm/admin/config/policy`

Also add `/v1/app/policy` to the public-route allowlist beside `/v1/app/features`.

**Step 4: Run tests**

Run:

```bash
make eunit EUNIT_MODS='app_feature_handler_tests adm_admin_feature_config_tests'
```

Expected: PASS

### Task 4: Run Final Verification

**Files:**
- Verify only

**Step 1: Run stage 3 targeted suite**

Run:

```bash
make eunit EUNIT_MODS='imboy_policy_tests app_feature_handler_tests adm_admin_feature_config_tests'
```

Expected: PASS

**Step 2: Run compatibility smoke**

Run:

```bash
make eunit EUNIT_MODS='imboy_feature_tests feature_gate_public_handler_tests feature_gate_admin_handler_tests'
```

Expected: PASS
