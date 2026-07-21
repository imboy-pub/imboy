# E2EE-019 Evidence — G1 自动化基线（automated subset）

- **Task**: E2EE-019（G1 安全出口）— **仅自动化验证子集**（第 1 步 + 验收标准 2）
- **Date**: 2026-07-21
- **Nature**: 验证/证据，不新增功能
- **Decision**: **PARTIAL** — 自动化基线全绿；真机矩阵（验收标准 3）为人工/真机门，本 Slice 明确不做，E2EE-019 整体保持 **Pending**。

> 用户在本会话选定「E2EE-019 自动化基线」方向：在两仓 clean HEAD 重跑静态分析 + 后端 EUnit + Flutter 测试，产出「自动化验收全绿」证据，为 G1 真机旅程铺路。真机 fail-closed / room-key / logout / backup 旅程需 Android/iOS 真机矩阵 + TEST_PHONE 凭证，不在自动化子集内。

## 1. 仓库基线

| 仓库 | HEAD（复验时） | 工作树 |
|---|---|---|
| imboy | `606df87b`（本 evidence 提交前） | clean |
| imboyapp | `ecdc1805` → `fe870001`（analyzer 修复后） | 仅未跟踪 `.codex/`、`AGENTS.md`（与本任务无关） |

本 Slice 唯一代码改动：imboyapp `fe870001` — 为 `dart analyze lib` 归零补 `lib/service/sqlite.dart` uid-guard early-return 的花括号（info 级 `curly_braces_in_flow_control_structures`，由 S1 修复 `b05f7180` 引入），无行为变更。

## 2. 静态分析

| 检查 | 命令 | 结果 |
|---|---|---|
| imboy erlfmt format-check（5 个 E2EE 源文件） | `erlfmt --check src/api/e2ee_trust_handler.erl src/api/olm_handler.erl src/ds/trust_audit_ds.erl src/logic/e2ee_trust_logic.erl src/repo/trust_audit_repo.erl` | **All matched files use erlfmt code style!** |
| imboy compile | `make app` | **exit 0**（含 `ERLC e2ee_trust_logic.erl` / `APP imboy`） |
| imboyapp analyze | `dart analyze lib` | 修复前 1 info（sqlite.dart:98）→ 修复后 **No issues found!** |

## 3. 后端 EUnit（S1 相关模块）

| 模块 | 结果 | 覆盖任务 |
|---|---|---|
| `e2ee_trust_logic_tests` | **22/22** | E2EE-014（canonical/freshness/event_id 幂等/单调/撤销/newline 守卫） |
| `olm_handler_tests` | **5/5** | E2EE-013（device_write_decision DID 绑定） |
| `token_ds_tests` | **11/11** | E2EE-013（token DID claim 往返） |
| `auth_ds_tests` | **12/13**（见 §3.1） | E2EE-013（verify_token 三元组 / current_did） |
| `trust_audit_repo_integration_tests`（真 PG `imboy_v1`，非 meck） | **4/4** | E2EE-014 #1（inserted / duplicate 幂等 / event_id_conflict / identity_version_rollback） |

命令（真 PG 骨架，绕开 boot，不动 dev schema）：
`IMBOYENV=local make eunit t=trust_audit_repo_integration_tests EUNIT_ERL_OPTS="-config config/sys.local -pa ebin -pa test"`。

### 3.1 auth_ds_tests 唯一失败 = 预存基线死测试（非回归）

- 失败用例：`get_token_with_assets_resource_test_`（`test/ds/auth_ds_tests.erl:19`），`error:undef`。
- **根因**：`rg 'get_token\(' src/` **全库零命中** — 生产代码已无 `get_token/N`（资源上传 token 早已重构移除），测试残留未删 → 调不存在的函数必然 `undef`。
- **非本轨道回归**：E2EE-013 `0b67aade` 对 `auth_ds.erl` 的 diff **没有删除 `get_token`**（只新增 `verify_token/1` 三元组 + `current_did/1` + `do_authorization` 三元组处理）；该函数在 E2EE 轨道起点之前就已不在模块内。
- **E2EE-013 触及面全绿**：`current_did_default_test` / `verify_token_with_valid_token_test` / `verify_token_with_refresh_token_test` 均 ok。
- 处置：不在 E2EE-019 范围内清理（playbook §1.2「不顺手重构」）。记录为预存基线，归入死代码清理待办。与 passport 7 个预存基线失败同类。

## 4. Flutter S1 测试集

命令：`flutter test test/service/e2ee/ test/service/e2ee_local_backup_boundary_test.dart test/service/sqlite_uid_isolation_test.dart test/integration/room_key_olm_roundtrip_test.dart`

结果：**All tests passed! — 109 passed / 10 skipped / 0 failed**（exit 0）。覆盖 policy_gate(010) / room_key olm roundtrip(011) / trust_event canonical+client(014) / secret_inventory(015) / local_backup_boundary(016，含 10k 固定 seed fuzz) / sqlite uid isolation(S1 CRITICAL b05f7180)。

## 5. E2EE-019 验收标准核对

- [x] `dart analyze lib` 全绿；S1 相关 Flutter tests 全绿；`make compile`（=`make app`）/ format-check 全绿。`make eunit` S1 相关模块全绿（auth_ds 唯一失败为预存死测试，已定位非回归）。
- [x] reviewer Critical/High = 0（`E2EE-S1-security-review.md`，3 Medium 已处置，3 Low 记录）。
- [x] E2EE-010..016 evidence 主体为 PASS（E2EE-015 真机 logout 旅程 / E2EE-014 HTTP handler wire + 真 Ed25519 验签仍标真机待人工）。
- [ ] **Android/iOS 真机矩阵**（设备型号 / OS / commit / 结果）— **待人工**，真机门。
- [x] 产品等级仍为 Preview，本 Slice 未改任何宣传等级文案。

**结论**：G1 自动化子集全绿，E2EE-019 整体保持 Pending，解锁项 = 真机矩阵旅程（需真机 + 凭证，另获人工授权后执行）。
