# Shared Report Handler Gate Alignment Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Close the remaining plugin-governance gap in shared report handlers so both public and admin report flows resolve feature gates from the plugin registry instead of open-coding target-type checks.

**Architecture:** Keep `imboy_plugin_registry` as the single source of truth, but add target-type rule lookup for shared handlers. Use that helper from `report_handler` and `adm_report_handler` after the target type is resolved from route/body/query input.

**Tech Stack:** Erlang, Cowboy REST handlers, `imboy_plugin_registry`, `imboy_feature`, EUnit

---

## Scope

This batch covers:

- add target-type feature rules for shared report handlers in the plugin manifests;
- make `report_handler` and `adm_report_handler` consult the registry helper instead of hardcoding plugin checks;
- add tests for public/admin report gating paths;
- verify the shared-handler rule lookup works when multiple plugin manifests contribute rules to the same handler.

This batch does not cover:

- redesigning report permissions;
- changing report payload formats or route contracts;
- adding new report target types beyond the existing plugin surfaces.

## Resume Rule

If the session stops:

1. Open this file.
2. Re-run the targeted EUnit modules listed below.
3. Resume from the first failing or unfinished task.

Resume prompt:

`Please continue executing /Users/leeyi/project/imboy.pub/imboy/docs/plans/2026-03-14-shared-report-handler-gate-alignment.md from the first unfinished task.`

### Task 1: Lock Registry Lookup Semantics

**Files:**
- Modify: `test/lib/imboy_plugin_registry_tests.erl`

**Steps:**

1. Add coverage for shared handler target lookup:
   - `required_feature_for_target(api, report_handler, <<"moment">>) -> moment`
   - `required_feature_for_target(api, report_handler, <<"channel">>) -> channel`
   - `required_feature_for_target(admin, adm_report_handler, <<"moment">>) -> moment`
   - `required_feature_for_target(admin, adm_report_handler, <<"channel">>) -> channel`
2. Assert non-plugin targets still return `undefined`.

### Task 2: Add Shared Handler Target Rules

**Files:**
- Modify: `src/lib/imboy_plugin_registry.erl`

**Steps:**

1. Add `api_target_feature_rules` for:
   - `report_handler` + `<<"moment">>` => `moment`
   - `report_handler` + `<<"channel">>` => `channel`
2. Add `admin_target_feature_rules` for:
   - `adm_report_handler` + `<<"moment">>` => `moment`
   - `adm_report_handler` + `<<"channel">>` => `channel`
3. Export a helper that resolves target-type rules without leaking the internal rule maps into public policy payloads.

### Task 3: Refactor Shared Report Handlers

**Files:**
- Modify: `src/api/report_handler.erl`
- Modify: `src/adm/adm_report_handler.erl`

**Steps:**

1. Replace direct `moment` hardcoding in `report_handler`.
2. Resolve the effective target type first, then ask the registry which feature is required.
3. Keep success/error payloads and permission behavior unchanged aside from the new feature gate enforcement.

### Task 4: Verify Public/Admin Gating

**Files:**
- Modify: `test/api/report_handler_tests.erl`
- Modify: `test/adm/feature_gate_admin_handler_tests.erl`

**Steps:**

1. Add public report gate tests for:
   - generic `create` with `target_type = channel`
   - `moment_create` route flow
2. Add admin report gate tests for:
   - static `channel_list`
   - generic `list` with `target_type = moment`

### Task 5: Run Serial Verification

Run:

```bash
make eunit EUNIT_MODS='imboy_plugin_registry_tests report_handler_tests feature_gate_admin_handler_tests adm_report_handler_tests'
make eunit EUNIT_MODS='feature_gate_public_handler_tests feature_gate_admin_handler_tests app_feature_handler_tests adm_admin_feature_config_tests'
```

Expected: PASS
