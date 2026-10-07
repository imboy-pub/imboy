# Integration Merge Record — run ews-20261004T065500Z

> 本文件为 merge 后新增记录，不修改 final/manifest.sha256（152 文件不可变清单）中任何既有文件。
> Executed: 2026-10-04 (post-verdict integration, per 用户分支→提交→合并main→删分支 workflow)

## Merges (true merge, ff not possible due to main drift)

| Repo | Base | Branch tip (deleted) | Merge commit on main | Overlap with main drift |
|---|---|---|---|---|
| imboy | 0be001e3 | 1b8b8c0d | **7f4b7656** `chore(acceptance): merge run/ews-acceptance into main` | 0 files (drift = 481 files) |
| imboyadmin | c9e73ad | 2750da6 | **0c05c23** `merge: land enterprise acceptance e2e fixture parameterization` | 0 files (drift = 3 files: AGENTS.md/CLAUDE.md/package.json) |

Brought-in commits:
- imboy: fd53e9b3 (contract resync: /api/v1/attachment/content gateway route), 1b8b8c0d (eb db_it frozen counts 0:2:1:7 / 0:30)
- imboyadmin: b89141e1, a132da2, 2750da6 (ADM-03 / ent-int01 fixture parameterization)

Pre-merge safety checks:
- Plan doc SHA re-verified: afefa4d977397567db7e207da12c06dffc2f1b5c20b8d4049e8a67e8a06679e9 ✓
- `git diff <base> run/ews-acceptance .. main` precise file intersection = empty (both repos)
- ews branch touches no path under docs/design|plans → main-ws untracked RUN_ROOT not in merge write set
- main-ws dirty files (group_ds.erl, elib_pg_sql.erl, ct.cover.spec deletion, 2 untracked docs) pre-existed, outside merge write set, untouched

## Post-merge smoke / affected-gate re-verification

- `git diff run/ews-acceptance main -- <ews files>` = empty in both repos → merged content byte-identical to W2–W4-verified candidate trees (zero-regression surface by construction)
- imboy: `.contract/api_contract.json` valid JSON; domains = adm, api_internal, api_v1, main, test_dev_only; attachment/content route present in file (byte-identical to gate-PASS version); `enterprise_business_db_it.sh` `bash -n` OK, frozen expectations `0:2:1:7` (line 1078) and `0:30` (line 1088) in place
- imboyadmin: playwright `--list` parses merged specs — eadm-v21-nine-leaves-real: 10 tests; ent-int01 (via run-scoped config, main config testIgnore excludes by design): 22 tests
- Hooks: lefthook gitleaks PASS, conventional PASS (merge message reformatted to `chore(acceptance):` to satisfy repo's commit-msg hook)

## Cleanup

- Worktrees removed: /private/tmp/imboy-ews-be, /private/tmp/imboy-ews-admin (untracked configs verified byte-identical to archived copies first: sys.ews.config f69cd125…, ews.int01.config.ts 31ef7b17…)
- Branches deleted (safe `-d`, merged): run/ews-acceptance in both repos
- seat-20261004 worktrees/branches belong to the parallel seat line — untouched

## Retained (deliberate)

- PG database `imboy_ews_20261004T0655Z`: kept for EWS-BE-D2 (OA SSO dual-clock CHECK race) fix re-verification
- /tmp/ews_seed/ (adm seed escripts, entw_reset.sql, adm_sessions.env) and run logs: kept while D2 fix is pending
- RUN_ROOT evidence (untracked in main-ws): 152-file immutable manifest intact
- imboy local main was ahead of gitcode/main at merge time; this run did NOT push.
  [Review correction 2026-10-04] Afterwards the main-line owner pushed the chain — gitcode/main now at
  e9b2e83a (2 further main-line commits on top of this merge); 7f4b7656 confirmed in main ancestry and
  both merged files byte-identical at current tip. Push performed by main-line owner, not by this run.
