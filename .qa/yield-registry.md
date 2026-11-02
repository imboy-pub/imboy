# yield-registry — 全量轨道受控让位登记表（验收对照）

> 建立日期：2026-11-02（CI run 41393644512 验收复核）
> 性质：**正式验收证据**，随让位改动同提交维护；入仓保留。

## 判定边界（钉死，防止扩大化）

1. **让位 ≠ 通过**。nightly 全量轨道（`make eunit-local-legacy`，公网 runner
   无钉库资源）变绿只证明"已执行的 14214+ 用例通过 + 让位套件未执行"，
   **不构成**全量通过的判据。
2. 只有同时满足以下三条的测试才允许让位：
   a. 需要**专用外部资源**（钉名 PG 库 / gRPC 服务 / 证据文件），公网 runner
      结构性无法提供；
   b. 在本表登记了**独立执行通道**（gate 脚本 / 官方启动器 / 钉库本地命令）；
   c. 通道状态为 ✅ 或 ⚠️（见下），⚠️ 项必须补通道后才能关闭。
   纯逻辑测试、mock 可覆盖的测试**一律不让位**。
3. eunit 无 skip 语义：让位的唯一合法形式是生成器返回空清单（`[]`）并打
   `*_EUNIT_YIELD` 日志；`{skip,...}` 是 Common Test 概念，在 eunit 中计为
   cancel 使整轮失败（run 41388448424 实证）。
4. CI 轨道让位日志可 grep：`*_EUNIT_YIELD|*_PG_YIELD`——每次 nightly 的
   让位面从日志可审计，与上表对照防漂移。

## 登记表

| # | 套件 / 模块 | 让位条件（缺失即让位） | 独立执行通道 | 最近独立执行证据 | 通道状态 |
|---|---|---|---|---|---|
| 1 | attachment_lifecycle_v5 家族（repo/attachment_lifecycle_v5_*.erl，含 pg/w401/w402/hold/w403/w601 等） | `V5_INTTEST_DB` 等各槽 `<PREFIX>_DB`；w403 另需 `W403_RESOURCE_MANIFEST` | `bash scripts/test/attachment_lifecycle_v5_gate.sh --task TASK_ID --run-dir ABS --slot SLOT_ID`（钉名导出后执行） | 按任务号留证据（gate 输出 + evidence/ 目录） | ✅ 脚本在仓；w403/w601 的 task 覆盖待逐项核对 |
| 2 | textguard 三套件（textguard_business/integration/shadow_tests） | `TG16_PG_*`、`TG19_*`、`TEXTGUARD_*_SECRET_FILE`（全套） | `python3 scripts/verify_textguard_business.py` / `verify_textguard_shadow.py` / `verify_textguard_dual.py`（官方启动器，`TEXTGUARD_TEST_OWN_VM=1` 自持 VM） | 启动器输出 + golden 对账 | ✅ 脚本在仓 |
| 3 | talk_record_owned 映射批（talk_record_*_tests 共 27+ 模块） | 具名钉库 `talk_record_t05`（T05 系 support 直连）+ `TALK_UPLOAD_DISK_ROOT`（绝对真实路径） | `bash scripts/test/pinned_pg_eunit_gate.sh talk_record_capture_tests ...`（逐模块；env 清单见脚本头注与本行） | 2026-11-02 本地实测 `All 31 tests passed.`（talk_record_capture_tests，talk_record_t05@4323） | ✅ 通道在仓 |
| 4 | talk_record_attachment_lifecycle_tests（T06） | `T06_PG_HOST/PORT/USER/DATABASE`（指向**已迁移**的具名库）+ `TALK_UPLOAD_DISK_ROOT` | 同上 runner：`bash scripts/test/pinned_pg_eunit_gate.sh talk_record_attachment_lifecycle_tests` | 2026-11-02 本地 `All 28 tests passed.`（t06_demo@4323，drill_migrate 后） | ✅ 通道实测 |
| 5 | enterprise_directory_sync_pg_tests | `DIRECTORY_SYNC_INTTEST_DB`（钉名复用库）+ `DIRECTORY_SYNC_INTTEST_PG_*` 四项 | 同上 runner：`bash scripts/test/pinned_pg_eunit_gate.sh enterprise_directory_sync_pg_tests` | 2026-11-02 本地 `2 tests passed.`（directory_sync_demo@4323） | ✅ 通道实测 |
| 6 | inttest_marker_db 直调批（各 `<PREFIX>_DB` + `<PREFIX>_PG_*`） | 各套件 `env_prefix` 对应 `<PREFIX>_DB` + `<PREFIX>_PG_*` | 同上 runner，按套件传模块名并导出该套件 PREFIX env（PREFIX 清单：FULL02/EPGZ03/FULL07/EPGZ04/FULL03/ADM_ORG_CREATE_INTTEST/GZAPP06/ATTACH_BIND_INTTEST/EPGZ05_INTTEST/IMBOY_INTTEST_MOMENT/CS_CONSOLE_MIGRATION/CS_WIDGET_MIGRATION/HIRDPOC） | 2026-11-02 adm 组实测 `All 14 tests passed.` ×2（t6_adm_demo@4323，双 PREFIX 同库代表）；其余组待按同款回填 | ✅ 通道实测（adm 组） |
| 7 | oa_direct_cloud_retention_pg_tests | `OA_CLOUD_MARKER_PORT` + `INTBE02_INTTEST_*` | 导出入口已核实：`scripts/quality.escript:431`（钉本 run PG 端口）与 `scripts/ci/native-eunit-run.sh:151`（55432）；或经本 runner 导出后单跑 | 专项 C 取证（run 41393644512 日志） | ✅ 通道在仓 |
| 8 | c2c_action_receipt_pg_tests | 无 env 让位（共享 PG 可跑）；问题为 pool 顺序耦合（修复中） | 同全量轨道 | run 41393644512 之前正常 | ✅ 修复后随全量执行 |

## 待办（通道补齐）

- [x] T3-T7：统一 runner `scripts/test/pinned_pg_eunit_gate.sh` 已建（2026-11-02，
  守卫/防零用例假绿沿用 group_schedule_remind_pg_gate 惯例；T3 已本地实测
  `All 31 tests passed.`，T4/T5/T6 env 清单已登记待首跑）
- [ ] T4/T5/T6 各套件首次经 runner 真跑并回填证据列（含 T06/T5 的完整 env 清单确认）
- [ ] 每次让位类提交：本表 + 同提交的独立通道证据（脚本/文档/最近 run id）

## 与 known-failures.yaml 的关系

- `known-failures.yaml`：**已执行且失败**的已知失败清单（五分类棘轮）。
- 本表：**环境性未执行**清单。两者正交；本表条目不得进入 known-failures
  （未执行不是失败，不参与五分类）。
