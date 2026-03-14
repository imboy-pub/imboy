# Profile Policy Skeleton Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add the first-stage backend skeleton for `product_profile`, `capabilities`, `plugin_manifest`, and `policy_engine` without changing existing feature-gate behavior.

**Architecture:** Keep the current `features` interface and compatibility semantics intact, then add new Erlang lib modules that expose profile defaults, plugin registry metadata, and a unified effective policy object. Existing App/Admin feature endpoints continue returning the same flat feature boolean map while the new policy layer becomes available for later capability enforcement.

**Tech Stack:** Erlang, Cowboy REST handlers, `config_ds`, EUnit, existing `src/lib/` helper modules

**Status:** Completed in this session. Resume only if you want to extend stage 2 capability enforcement.

---

## Scope

This plan only covers stage 1:

- add explicit `product_profile` reading;
- add explicit `capabilities` reading;
- add profile preset defaults;
- add plugin registry metadata;
- add unified policy aggregation;
- keep current `/v1/app/features` and `/adm/admin/config/features` behavior unchanged.

This plan does not yet:

- enforce `capabilities` inside message storage, search, export, or audit flows;
- change routing, UI menus, or API contracts;
- rename existing feature keys.

## Resume Rule

If implementation stops halfway:

1. Open this file.
2. Find the first task that is not fully completed.
3. Continue from that task only.
4. Before resuming, re-run the targeted tests listed under that task.

If a new session has no context, use this sentence:

`Please continue executing /Users/leeyi/project/imboy.pub/imboy/docs/plans/2026-03-13-profile-policy-skeleton.md from the first unfinished task.`

### Task 1: Lock Current Behavior With Tests

**Files:**
- Create: `test/lib/imboy_profile_preset_tests.erl`
- Create: `test/lib/imboy_plugin_registry_tests.erl`
- Create: `test/lib/imboy_policy_tests.erl`
- Modify: `test/lib/imboy_feature_tests.erl`

**Step 1: Write the failing preset tests**

Create `test/lib/imboy_profile_preset_tests.erl` with tests for:

```erlang
community_profile_defaults_test() ->
    Defaults = imboy_profile_preset:defaults(community),
    ?assertMatch(#{capabilities := #{storage_mode := archived}}, Defaults),
    ?assertMatch(#{features := #{channel := false}}, Defaults).

enterprise_profile_defaults_test() ->
    Defaults = imboy_profile_preset:defaults(enterprise),
    ?assertMatch(#{capabilities := #{message_search := true}}, Defaults),
    ?assertMatch(#{features := #{channel := true}}, Defaults).
```

**Step 2: Run preset tests to verify they fail**

Run:

```bash
go test ./...  # do not use; wrong stack
```

Run instead:

```bash
make eunit EUNIT_MODS='imboy_profile_preset_tests'
```

Expected: FAIL because module `imboy_profile_preset` does not exist yet.

**Step 3: Write the failing plugin registry tests**

Create `test/lib/imboy_plugin_registry_tests.erl` with checks for:

- `channel` manifest exists;
- `group_collab` is marked as aggregate plugin;
- `group_collab` contains `group_vote`, `group_schedule`, `group_task`;
- `channel_discover` is listed under `channel` feature keys.

**Step 4: Run registry tests to verify they fail**

Run:

```bash
make eunit EUNIT_MODS='imboy_plugin_registry_tests'
```

Expected: FAIL because module `imboy_plugin_registry` does not exist yet.

**Step 5: Write the failing policy tests**

Create `test/lib/imboy_policy_tests.erl` covering:

- current profile defaults to `community` when config missing;
- explicit `product_profile=enterprise` is respected;
- capabilities merge profile defaults with explicit overrides;
- `effective_features/0` preserves existing compatibility behavior when `features` config is missing;
- `effective/0` returns `profile`, `capabilities`, `features`, and `plugins`.

Use meck around `config_ds:env/2`.

**Step 6: Run policy tests to verify they fail**

Run:

```bash
make eunit EUNIT_MODS='imboy_policy_tests'
```

Expected: FAIL because module `imboy_policy` does not exist yet.

**Step 7: Extend current feature tests for compatibility**

Add one test to `test/lib/imboy_feature_tests.erl` to verify:

- `imboy_feature:all/0` still returns all-known booleans when no `product_profile` or `capabilities` config exists.

**Step 8: Commit**

```bash
git add test/lib/imboy_profile_preset_tests.erl test/lib/imboy_plugin_registry_tests.erl test/lib/imboy_policy_tests.erl test/lib/imboy_feature_tests.erl
git commit -m "test: lock stage1 policy skeleton behavior"
```

### Task 2: Add Profile Preset Module

**Files:**
- Create: `src/lib/imboy_profile_preset.erl`
- Test: `test/lib/imboy_profile_preset_tests.erl`

**Step 1: Write minimal implementation**

Create `src/lib/imboy_profile_preset.erl` with this shape:

```erlang
-module(imboy_profile_preset).

-export([current/0, defaults/0, defaults/1, supported_profiles/0]).

current() ->
    normalize_profile(config_ds:env(product_profile, community)).

defaults() ->
    defaults(current()).
```

Implement:

- `supported_profiles/0 -> [community, enterprise]`
- `defaults(community)` returning:
  - capabilities: archived, optional E2EE, search false, export false, audit metadata, 30-day rolling retention
  - features: `core=true`, `e2ee=false`, `channel=false`, `location=false`, `moment=false`, `group_* = false`
- `defaults(enterprise)` returning:
  - capabilities: archived, E2EE disabled, search true, export true, audit full, 365-day rolling retention
  - features: `core=true`, `channel=true`, `channel_invitation=true`, all others false unless explicitly chosen
- unknown profile falls back to `community`

**Step 2: Run tests**

Run:

```bash
make eunit EUNIT_MODS='imboy_profile_preset_tests'
```

Expected: PASS

**Step 3: Commit**

```bash
git add src/lib/imboy_profile_preset.erl test/lib/imboy_profile_preset_tests.erl
git commit -m "feat: add profile preset skeleton"
```

### Task 3: Add Plugin Registry Module

**Files:**
- Create: `src/lib/imboy_plugin_registry.erl`
- Test: `test/lib/imboy_plugin_registry_tests.erl`

**Step 1: Write minimal implementation**

Create `src/lib/imboy_plugin_registry.erl` with exports:

```erlang
-export([all/0, get/1, plugin_names/0]).
```

Return a static map containing:

- `channel`
- `moment`
- `location`
- `group_collab`

Each manifest should include:

- `kind`
- `feature_keys`
- `requires_capabilities`
- `depends_on_plugins`

For `group_collab`, also include:

- `children => [vote, schedule, task]`

For `channel`, include:

- `feature_keys => [channel, channel_discover, channel_invitation, channel_order]`

**Step 2: Run tests**

Run:

```bash
make eunit EUNIT_MODS='imboy_plugin_registry_tests'
```

Expected: PASS

**Step 3: Commit**

```bash
git add src/lib/imboy_plugin_registry.erl test/lib/imboy_plugin_registry_tests.erl
git commit -m "feat: add plugin registry skeleton"
```

### Task 4: Add Policy Aggregation Module

**Files:**
- Create: `src/lib/imboy_policy.erl`
- Test: `test/lib/imboy_policy_tests.erl`

**Step 1: Write minimal implementation**

Create `src/lib/imboy_policy.erl` with exports:

```erlang
-export([
    effective/0,
    current_profile/0,
    effective_capabilities/0,
    effective_features/0,
    effective_plugins/0
]).
```

Implementation rules:

- `current_profile/0` delegates to `imboy_profile_preset:current/0`
- capabilities:
  - start from `imboy_profile_preset:defaults(Profile)`
  - merge explicit `config_ds:env(capabilities, #{})`
- features:
  - preserve current compatibility mode:
    - missing `features` block => all known feature keys default to `true`
    - missing single item => that item defaults to `true`
  - apply existing dependency rule that `channel_*` requires `channel`
- plugins:
  - derive plugin enabled state from `imboy_plugin_registry`
  - a plugin is enabled if any of its `feature_keys` are enabled

`effective/0` should return:

```erlang
#{
    profile => community | enterprise,
    capabilities => #{...},
    features => #{...},
    plugins => #{...}
}
```

**Step 2: Run tests**

Run:

```bash
make eunit EUNIT_MODS='imboy_policy_tests'
```

Expected: PASS

**Step 3: Commit**

```bash
git add src/lib/imboy_policy.erl test/lib/imboy_policy_tests.erl
git commit -m "feat: add policy aggregation skeleton"
```

### Task 5: Reuse Policy Layer From Feature Module

**Files:**
- Modify: `src/lib/imboy_feature.erl`
- Test: `test/lib/imboy_feature_tests.erl`
- Test: `test/api/app_feature_handler_tests.erl`
- Test: `test/adm/adm_admin_feature_config_tests.erl`

**Step 1: Change `all/0` to read from policy**

Replace direct feature map building with:

```erlang
all() ->
    maps:from_list([
        {atom_to_binary(Name, utf8), enabled(Name)}
        || Name <- feature_names()
    ]).
```

Keep `enabled/1` behavior stable, but delegate raw boolean evaluation through:

```erlang
imboy_policy:effective_features()
```

The result must remain exactly the same flat boolean payload shape as today.

**Step 2: Keep `ensure_enabled/2` unchanged**

Do not change the public error behavior or error code path.

**Step 3: Run targeted tests**

Run:

```bash
make eunit EUNIT_MODS='imboy_feature_tests'
make eunit EUNIT_MODS='app_feature_handler_tests'
make eunit EUNIT_MODS='adm_admin_feature_config_tests'
```

Expected: PASS

**Step 4: Commit**

```bash
git add src/lib/imboy_feature.erl test/lib/imboy_feature_tests.erl test/api/app_feature_handler_tests.erl test/adm/adm_admin_feature_config_tests.erl
git commit -m "refactor: route feature matrix through policy layer"
```

### Task 6: Add Config Skeleton To Example Config

**Files:**
- Modify: `config/sys.config.example`

**Step 1: Add explicit stage1 config blocks**

Insert above current `features`:

```erlang
{product_profile, community},

{capabilities, #{
    storage_mode => archived,
    e2ee_mode => optional,
    message_search => false,
    message_export => false,
    audit_mode => metadata,
    retention_policy => #{
        mode => rolling_days,
        days => 30
    }
}},
```

Add comments that:

- current feature compatibility remains unchanged;
- capabilities are not yet fully enforced by message/search/export flows;
- this is stage1 skeleton config only.

**Step 2: Run format-sensitive review**

No formatter required unless existing workflow demands one.

Manually verify the Erlang config syntax is still valid.

**Step 3: Commit**

```bash
git add config/sys.config.example
git commit -m "docs: add profile and capability config skeleton"
```

### Task 7: Run Final Verification

**Files:**
- Verify only

**Step 1: Run targeted unit tests**

Run:

```bash
make eunit EUNIT_MODS='imboy_profile_preset_tests imboy_plugin_registry_tests imboy_policy_tests imboy_feature_tests app_feature_handler_tests adm_admin_feature_config_tests'
```

Expected: PASS

**Step 2: Run broader smoke check**

Run:

```bash
make eunit EUNIT_MODS='feature_gate_public_handler_tests feature_gate_admin_handler_tests'
```

Expected: PASS

**Step 3: Capture current diff**

Run:

```bash
git status --short
git diff --stat
```

Expected: only the planned files are modified.

**Step 4: Commit**

```bash
git add src/lib/imboy_profile_preset.erl src/lib/imboy_plugin_registry.erl src/lib/imboy_policy.erl src/lib/imboy_feature.erl config/sys.config.example test/lib/imboy_profile_preset_tests.erl test/lib/imboy_plugin_registry_tests.erl test/lib/imboy_policy_tests.erl test/lib/imboy_feature_tests.erl
git commit -m "feat: add stage1 profile policy skeleton"
```

## Notes For The Next Session

- Do not start capability enforcement in message/search/export code during this plan.
- Do not rename `group_vote`, `group_schedule`, `group_task` yet.
- Do not change App/Admin API payload shape yet.
- If any test reveals hidden dependence on `config_ds:env(features, undefined)`, preserve current behavior first and postpone cleanup.

## After This Plan

The next implementation plan should cover stage 2 only:

- enforce `storage_mode`
- enforce `message_search`
- enforce `message_export`
- enforce `audit_mode`
- add capability-aware checks to search/export/admin message views
