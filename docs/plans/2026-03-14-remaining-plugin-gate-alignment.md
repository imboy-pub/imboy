# Remaining Plugin Gate Alignment Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Extend the manifest-backed gate helper to the remaining first-wave plugins so `moment`, `location`, and `group_collab` API/Admin handlers stop open-coding feature gates.

**Architecture:** Reuse `imboy_plugin_registry:required_feature/3` as the single resolver. Add rule maps for `moment`, `location`, and `group_collab`, then refactor the corresponding handlers to consult the registry helper before dispatching business logic.

**Tech Stack:** Erlang, Cowboy REST handlers, `imboy_plugin_registry`, `imboy_feature`, EUnit

---

## Scope

This batch covers:

- add registry gate rules for `moment`, `location`, and `group_collab`;
- refactor their API/Admin handlers to use the registry helper;
- keep existing behavior and permissions unchanged.

This batch does not cover:

- new routes or new payload contracts;
- app entry rendering changes;
- capability-driven plugin rules.

## Resume Rule

If the session stops:

1. Open this file.
2. Start from the first unfinished task.
3. Re-run the targeted tests from that task before making more changes.

Resume prompt:

`Please continue executing /Users/leeyi/project/imboy.pub/imboy/docs/plans/2026-03-14-remaining-plugin-gate-alignment.md from the first unfinished task.`

### Task 1: Lock Registry Resolution With Tests

**Files:**
- Modify: `test/lib/imboy_plugin_registry_tests.erl`

**Step 1: Add remaining plugin resolution tests**

Extend `test/lib/imboy_plugin_registry_tests.erl` with:

- `moment_and_location_api_required_features_test_`
  - `required_feature(api, moment_handler, feed) -> moment`
  - `required_feature(api, location_handler, people_nearby) -> location`
- `group_collab_required_features_for_api_and_admin_test_`
  - `required_feature(api, group_vote_handler, list) -> group_vote`
  - `required_feature(api, group_schedule_handler, list) -> group_schedule`
  - `required_feature(api, group_task_handler, list) -> group_task`
  - `required_feature(admin, adm_group_handler, vote_list) -> group_vote`
  - `required_feature(admin, adm_group_handler, schedule_list) -> group_schedule`
  - `required_feature(admin, adm_group_handler, task_list) -> group_task`

**Step 2: Run targeted tests**

Run:

```bash
make eunit EUNIT_MODS='imboy_plugin_registry_tests'
```

Expected: FAIL because the registry does not expose those rules yet.

### Task 2: Add Remaining Registry Rules

**Files:**
- Modify: `src/lib/imboy_plugin_registry.erl`
- Test: `test/lib/imboy_plugin_registry_tests.erl`

**Step 1: Add rule maps**

Add:

- `moment` API/Admin defaults => `moment`
- `location` API default => `location`
- `group_collab` API defaults:
  - `group_vote_handler => group_vote`
  - `group_schedule_handler => group_schedule`
  - `group_task_handler => group_task`
- `group_collab` Admin action overrides on `adm_group_handler`
  - vote actions => `group_vote`
  - schedule actions => `group_schedule`
  - task actions => `group_task`

**Step 2: Run tests**

Run:

```bash
make eunit EUNIT_MODS='imboy_plugin_registry_tests'
```

Expected: PASS

### Task 3: Refactor Remaining Handlers

**Files:**
- Modify: `src/api/moment_handler.erl`
- Modify: `src/api/location_handler.erl`
- Modify: `src/api/group_vote_handler.erl`
- Modify: `src/api/group_schedule_handler.erl`
- Modify: `src/api/group_task_handler.erl`
- Modify: `src/adm/adm_moment_handler.erl`
- Modify: `src/adm/adm_group_handler.erl`

**Step 1: Replace direct `imboy_feature:ensure_enabled/2` calls**

Use `imboy_plugin_registry:required_feature/3` with the correct surface/handler/action.

Behavior:

- when helper returns a feature, call `imboy_feature:ensure_enabled/2`;
- when helper returns `undefined`, keep current dispatch behavior.

**Step 2: Preserve all existing action semantics**

Do not change:

- business dispatch branches;
- admin permission checks;
- success/error payloads.

**Step 3: Run tests**

Run:

```bash
make eunit EUNIT_MODS='feature_gate_public_handler_tests feature_gate_admin_handler_tests'
```

Expected: PASS

### Task 4: Run Final Verification

**Files:**
- Verify only

**Step 1: Run the batch suite**

Run:

```bash
make eunit EUNIT_MODS='imboy_plugin_registry_tests feature_gate_public_handler_tests feature_gate_admin_handler_tests group_vote_handler_tests group_schedule_handler_tests group_task_handler_tests'
```

Expected: PASS

**Step 2: Run compatibility smoke**

Run:

```bash
make eunit EUNIT_MODS='channel_handler_tests adm_channel_handler_tests imboy_policy_tests app_feature_handler_tests adm_admin_feature_config_tests'
```

Expected: PASS
