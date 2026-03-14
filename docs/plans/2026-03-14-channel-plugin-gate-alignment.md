# Channel Plugin Gate Alignment Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Start stage 3 API/Admin gate alignment by moving `channel` plugin action-to-feature rules into `imboy_plugin_registry`, then make channel handlers consume the registry instead of local hard-coded maps.

**Architecture:** Treat `plugin_manifest` as the source of truth for channel route gating. `imboy_plugin_registry` should expose a helper for resolving the required feature for a given surface, handler, and action. `channel_handler` and `adm_channel_handler` should use that helper so default channel routes are gated by `channel`, while discover/invitation/order actions still use their specific subfeatures.

**Tech Stack:** Erlang, Cowboy REST handlers, `imboy_plugin_registry`, `imboy_feature`, EUnit

---

## Scope

This batch covers:

- add channel action gate rules to `imboy_plugin_registry`;
- add a helper that resolves required feature by surface/handler/action;
- refactor `channel_handler` and `adm_channel_handler` to use the registry helper;
- ensure default channel routes are blocked when `channel=false`.

This batch does not cover:

- `group_collab` handler refactors;
- `moment` / `location` handler refactors;
- app entry visibility refactors;
- route generation from manifests.

## Resume Rule

If the session stops:

1. Open this file.
2. Start from the first unfinished task.
3. Re-run the targeted tests from that task before making more changes.

Resume prompt:

`Please continue executing /Users/leeyi/project/imboy.pub/imboy/docs/plans/2026-03-14-channel-plugin-gate-alignment.md from the first unfinished task.`

### Task 1: Lock Registry-Backed Channel Gate Behavior With Tests

**Files:**
- Modify: `test/lib/imboy_plugin_registry_tests.erl`
- Modify: `test/api/feature_gate_public_handler_tests.erl`
- Modify: `test/adm/feature_gate_admin_handler_tests.erl`

**Step 1: Add registry resolution tests**

Extend `test/lib/imboy_plugin_registry_tests.erl` with:

- `channel_api_required_feature_defaults_to_channel_test_`
  - `required_feature(api, channel_handler, show) -> channel`
- `channel_api_required_feature_uses_subfeature_override_test_`
  - `required_feature(api, channel_handler, discover) -> channel_discover`
  - `required_feature(api, channel_handler, create_order) -> channel_order`
- `channel_admin_required_feature_defaults_to_channel_test_`
  - `required_feature(admin, adm_channel_handler, list) -> channel`
  - `required_feature(admin, adm_channel_handler, invitations) -> channel_invitation`

**Step 2: Add public handler gate tests**

Extend `test/api/feature_gate_public_handler_tests.erl` with:

- `channel_show_init_uses_channel_feature_test_`
  - mock `imboy_feature:ensure_enabled/2` for `channel`
  - assert the init short-circuits before business logic

Keep the existing discover/order tests unchanged.

**Step 3: Add admin handler gate tests**

Extend `test/adm/feature_gate_admin_handler_tests.erl` with:

- `adm_channel_list_init_uses_channel_feature_test_`
  - mock `imboy_feature:ensure_enabled/2` for `channel`
  - assert init short-circuits before business logic

Keep the existing orders test unchanged.

**Step 4: Run targeted tests**

Run:

```bash
make eunit EUNIT_MODS='imboy_plugin_registry_tests feature_gate_public_handler_tests feature_gate_admin_handler_tests'
```

Expected: FAIL because registry helper and default channel gate are not implemented yet.

### Task 2: Add Channel Gate Resolution To Plugin Registry

**Files:**
- Modify: `src/lib/imboy_plugin_registry.erl`
- Test: `test/lib/imboy_plugin_registry_tests.erl`

**Step 1: Extend manifest data**

Add channel-specific surface gate maps:

- `api_feature_rules`
- `admin_feature_rules`

Recommended mapping:

- API default => `channel`
- API overrides:
  - `discover => channel_discover`
  - invitation actions => `channel_invitation`
  - order actions => `channel_order`
- Admin default => `channel`
- Admin overrides:
  - `invitations => channel_invitation`
  - `orders => channel_order`

**Step 2: Add helper export**

Export:

- `required_feature/3`

Signature:

```erlang
required_feature(api | admin, atom(), atom() | false) -> atom() | undefined.
```

Behavior:

- resolve handler/action from manifest rule maps;
- use action-specific override first;
- fall back to `default` if present;
- return `undefined` when no rule exists.

**Step 3: Run tests**

Run:

```bash
make eunit EUNIT_MODS='imboy_plugin_registry_tests'
```

Expected: PASS

### Task 3: Refactor Channel Handlers To Use Registry Helper

**Files:**
- Modify: `src/api/channel_handler.erl`
- Modify: `src/adm/adm_channel_handler.erl`
- Test: `test/api/feature_gate_public_handler_tests.erl`
- Test: `test/adm/feature_gate_admin_handler_tests.erl`

**Step 1: Replace local feature maps**

In `channel_handler`:

- replace local `required_feature/1` usage with:
  - `imboy_plugin_registry:required_feature(api, channel_handler, Action)`

In `adm_channel_handler`:

- replace local `required_feature/1` usage with:
  - `imboy_plugin_registry:required_feature(admin, adm_channel_handler, Action)`

**Step 2: Preserve existing subfeature behavior**

Do not change:

- discover => `channel_discover`
- invitation actions => `channel_invitation`
- order actions => `channel_order`

Only add the missing default `channel` gate for other channel routes.

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
make eunit EUNIT_MODS='imboy_plugin_registry_tests feature_gate_public_handler_tests feature_gate_admin_handler_tests channel_handler_tests adm_channel_handler_tests'
```

Expected: PASS

**Step 2: Run compatibility smoke**

Run:

```bash
make eunit EUNIT_MODS='imboy_policy_tests app_feature_handler_tests adm_admin_feature_config_tests'
```

Expected: PASS
