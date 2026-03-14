# Stage Commit Split Plan

**Goal:** Split the current staged backend policy/governance work into clean `stage1`, `stage2`, and `stage3` commits without mixing in unrelated docs/runtime work.

**Current Index State:** `git diff --name-status` is empty and `git diff --cached --name-status` contains all active changes. That means the current worktree is effectively "all staged, no unstaged" for the files listed below.

---

## Commit Order

1. `stage1`: profile / capability / plugin registry / policy skeleton
2. `stage2`: capability enforcement
3. `stage3`: effective policy view + plugin gate alignment + shared report gate
4. unrelated docs / cleanup / Prometheus / README churn as separate commits

## Stage 1

### Pure files that belong entirely to `stage1`

- `config/sys.config.example`
- `src/lib/imboy_feature.erl`
- `src/lib/imboy_profile_preset.erl`
- `test/lib/imboy_feature_tests.erl`
- `test/lib/imboy_profile_preset_tests.erl`
- `docs/plans/2026-03-13-profile-policy-skeleton.md`

### Mixed files that contain `stage1` plus later stages

- `src/lib/imboy_policy.erl`
- `src/lib/imboy_plugin_registry.erl`
- `test/lib/imboy_policy_tests.erl`
- `test/lib/imboy_plugin_registry_tests.erl`
- `test/api/app_feature_handler_tests.erl`
- `test/adm/adm_admin_feature_config_tests.erl`

### `stage1` ownership inside mixed files

`src/lib/imboy_policy.erl`

- keep the base skeleton only:
  - `current_profile/0`
  - `effective/0`
  - `effective_capabilities/0`
  - `effective_features/0`
  - `effective_plugins/0`
  - feature compatibility helpers (`feature_names/0`, `feature_enabled/2`, `dependencies/1`, `lookup_feature_switch/2`, `find_in_map/2`, `find_in_proplist/2`, `switch_enabled/1`, `candidate_keys/1`, `to_boolean/2`, `normalize_map/1`)
- remove from the `stage1` commit:
  - `effective_view/0`
  - all `message_*` helper exports/functions
  - capability normalization / constraint enforcement helpers
  - public JSON adapter helpers

`src/lib/imboy_plugin_registry.erl`

- keep only the static catalog:
  - `all/0`
  - `get/1`
  - `plugin_names/0`
  - manifest keys from the original stage1 skeleton:
    - `kind`
    - `feature_keys`
    - `requires_capabilities`
    - `depends_on_plugins`
    - `children` on `group_collab`
- remove from the `stage1` commit:
  - `required_feature/3`
  - `required_feature_for_target/3`
  - all `*_feature_rules`
  - all `*_target_feature_rules`
  - `app_entries`, `admin_entries`, `api_handlers`

`test/lib/imboy_policy_tests.erl`

- keep only:
  - `current_profile_defaults_to_community_when_missing_test_`
  - `current_profile_reads_explicit_enterprise_test_`
  - `effective_capabilities_merge_profile_defaults_and_overrides_test_`
  - `effective_features_preserve_missing_features_block_compatibility_test_`
  - `effective_policy_returns_profile_capabilities_features_and_plugins_test_`
- move to later commits:
  - `secure_e2ee_forces_search_export_off_and_downgrades_audit_test_`
  - `required_e2ee_disables_body_visibility_test_`
  - `effective_view_returns_json_friendly_policy_payload_test_`

`test/lib/imboy_plugin_registry_tests.erl`

- keep only:
  - `channel_manifest_exists_test_`
  - `group_collab_manifest_is_aggregate_plugin_test_`
  - `plugin_names_returns_expected_catalog_test_`
- move to later commits:
  - every `required_feature*` / `required_feature_for_target*` test

`test/api/app_feature_handler_tests.erl`

- keep only the legacy `features` endpoint compatibility coverage in `stage1`
- move the `policy` endpoint test to `stage3`

`test/adm/adm_admin_feature_config_tests.erl`

- keep only the legacy `config_features` coverage in `stage1`
- move the `config_policy` coverage to `stage3`

### Recommended commit message

- `feat: add stage1 profile policy skeleton`

## Stage 2

### Pure files that belong entirely to `stage2`

- `src/api/fts_handler.erl`
- `src/adm/adm_message_handler.erl`
- `test/api/fts_handler_tests.erl`
- `test/adm/adm_message_handler_tests.erl`
- `docs/plans/2026-03-14-capability-enforcement.md`

### Mixed files that contain `stage2` plus other stages

- `src/lib/imboy_policy.erl`
- `test/lib/imboy_policy_tests.erl`

### `stage2` ownership inside mixed files

`src/lib/imboy_policy.erl`

- add capability enforcement / normalization pieces:
  - export and implement:
    - `message_search_enabled/0`
    - `message_export_enabled/0`
    - `message_audit_mode/0`
    - `message_audit_enabled/0`
    - `message_body_visible/0`
  - normalized capability pipeline:
    - `capability_names/0`
    - `normalize_capability_map/1`
    - `normalize_capabilities/2`
    - `capability_value/3`
    - `enforce_capability_constraints/1`
    - `body_visibility_allowed/2`
    - `normalize_storage_mode/2`
    - `normalize_e2ee_mode/2`
    - `normalize_audit_mode/2`
    - `normalize_retention_policy/2`
  - update `effective_capabilities/0` to use normalized defaults + overrides
- do not include `effective_view/0` or JSON/public adapter helpers in `stage2`

`test/lib/imboy_policy_tests.erl`

- add only:
  - `secure_e2ee_forces_search_export_off_and_downgrades_audit_test_`
  - `required_e2ee_disables_body_visibility_test_`
- do not include the `effective_view` test in `stage2`

### Recommended commit message

- `feat: enforce policy capabilities in search export and audit flows`

## Stage 3

### Pure files that belong entirely to `stage3`

- `src/api/app_feature_handler.erl`
- `src/adm/adm_admin_handler.erl`
- `src/adm/adm_channel_handler.erl`
- `src/adm/adm_group_handler.erl`
- `src/adm/adm_moment_handler.erl`
- `src/adm/adm_report_handler.erl`
- `src/api/channel_handler.erl`
- `src/api/group_schedule_handler.erl`
- `src/api/group_task_handler.erl`
- `src/api/group_vote_handler.erl`
- `src/api/location_handler.erl`
- `src/api/moment_handler.erl`
- `src/api/report_handler.erl`
- `src/imboy_router.erl`
- `test/api/feature_gate_public_handler_tests.erl`
- `test/adm/feature_gate_admin_handler_tests.erl`
- `test/api/report_handler_tests.erl`
- `docs/plans/2026-03-14-effective-policy-view.md`
- `docs/plans/2026-03-14-channel-plugin-gate-alignment.md`
- `docs/plans/2026-03-14-remaining-plugin-gate-alignment.md`
- `docs/plans/2026-03-14-shared-report-handler-gate-alignment.md`

### Mixed files that contain `stage3` additions

- `src/lib/imboy_policy.erl`
- `src/lib/imboy_plugin_registry.erl`
- `test/lib/imboy_policy_tests.erl`
- `test/lib/imboy_plugin_registry_tests.erl`
- `test/api/app_feature_handler_tests.erl`
- `test/adm/adm_admin_feature_config_tests.erl`

### `stage3` ownership inside mixed files

`src/lib/imboy_policy.erl`

- add:
  - `effective_view/0`
  - `public_term/1`
  - `public_key/1`
  - `public_plugin_manifest/1`
- keep sanitization that strips:
  - `api_feature_rules`
  - `admin_feature_rules`
  - `api_target_feature_rules`
  - `admin_target_feature_rules`

`src/lib/imboy_plugin_registry.erl`

- add:
  - `required_feature/3`
  - `required_feature_for_target/3`
  - `surface_rules_key/1`
  - `surface_target_rules_key/1`
  - `merged_handler_rules/3`
  - manifest rule maps:
    - `api_feature_rules`
    - `admin_feature_rules`
    - `api_target_feature_rules`
    - `admin_target_feature_rules`
  - UI metadata:
    - `app_entries`
    - `admin_entries`
    - `api_handlers`

`test/lib/imboy_policy_tests.erl`

- add only:
  - `effective_view_returns_json_friendly_policy_payload_test_`

`test/lib/imboy_plugin_registry_tests.erl`

- add only:
  - `channel_api_required_feature_defaults_to_channel_test_`
  - `channel_api_required_feature_uses_subfeature_override_test_`
  - `channel_admin_required_feature_defaults_to_channel_test_`
  - `moment_and_location_api_required_features_test_`
  - `group_collab_required_features_for_api_and_admin_test_`
  - `shared_report_handlers_resolve_target_features_across_manifests_test_`

`test/api/app_feature_handler_tests.erl`

- add the `policy` endpoint coverage

`test/adm/adm_admin_feature_config_tests.erl`

- add the `config_policy` endpoint coverage

### Recommended commit messages

- `feat: add effective policy view endpoints`
- `refactor: align plugin gates with registry rules`
- `refactor: align shared report gates with registry rules`

If you want only one `stage3` commit, use:

- `feat: add policy view and registry-backed plugin gate alignment`

## Unrelated Staged Changes

These should not be mixed into the `stage1/2/3` backend governance chain:

- `.gitignore`
- `README.md`
- `doc/README.md`
- `doc/architecture/current-module-classification.md`
- `doc/architecture/module-layer-cheatsheet.md`
- `doc/architecture/product-profile-and-plugin-registry-design.md`
- `doc/guides/*` deletions
- `doc/operations/security.md`
- `doc/operations/three-end-delivery-plan-2026Q2.md` deletion
- `docker-compose.yml`
- `docker/README.md`
- `priv/README.md` deletion
- `src/lib/elib_prometheus.erl`
- `test/README.md`
- `test/doc/test1.md` deletion
- `test/performance/channel_perf_benchmark.erl` deletion
- `test/performance/channel_ws_push_benchmark.erl` deletion
- `test/test_quality_improvement_tasklist.md` deletion

## Practical Recommendation

Do not keep adding new work on top of the current index.

The next safe move is:

1. split the staged governance chain into `stage1`, `stage2`, `stage3`
2. leave unrelated docs/runtime churn out of those commits
3. only then continue with the next monetization-facing task, such as writable admin policy config

