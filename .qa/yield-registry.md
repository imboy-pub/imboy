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
| 3 | talk_record_owned 映射批（talk_record_*_tests 共 27+ 模块，经 talk_record_owned_eunit_fixture） | `TALK_OWNED_PG_HOST/PORT/USER/PASSWORD/DB` | ❗ 仓内未找到执行脚本/文档 | 无 | ⚠️ 通道缺失，待补 |
| 4 | talk_record_attachment_lifecycle_tests（T06） | `T06_PG_HOST/PORT/USER/DATABASE` | ❗ 仓内未找到执行脚本/文档 | 无 | ⚠️ 通道缺失，待补 |
| 5 | enterprise_directory_sync_pg_tests | `DIRECTORY_SYNC_INTTEST_PG_*` 四项 | ❗ 仓内未找到执行脚本/文档 | 无 | ⚠️ 通道缺失，待补 |
| 6 | inttest_marker_db 直调批（enterprise_full_api/identity_group/message_push/msg_asset_webhook/webhook_governance、enterprise_oa_sso、moment_delete_atomic、cs_seat_console/cs_pg_widget、hird p1/p2/p3、adm_organization_create/adm_owner_activation 等，各自 `<PREFIX>_DB`） | 各套件 `env_prefix` 对应 `<PREFIX>_DB` + `<PREFIX>_PG_*` | ❗ 无统一 gate；历史上为开发者本地配 env 后 `IMBOYENV=local make eunit t=<模块>` | 无统一记录 | ⚠️ 通道缺失，待补（逐套件登记或建统一 runner） |
| 7 | oa_direct_cloud_retention_pg_tests | `OA_CLOUD_MARKER_PORT`（marker 隔离容器端口合同） | ❗ 待核实（phase1 遗产） | 无 | ⚠️ 待核实 |
| 8 | c2c_action_receipt_pg_tests | 无 env 让位（共享 PG 可跑）；问题为 pool 顺序耦合（修复中） | 同全量轨道 | run 41393644512 之前正常 | ✅ 修复后随全量执行 |

## 待办（通道补齐）

- [ ] T3：为 talk_record_owned 批建立钉库执行脚本（对齐 T06 的 TALK_OWNED 钉名合同），或在 docs/reference 登记执行手册
- [ ] T4：T06 同上
- [ ] T5：directory_sync 同上
- [ ] T6：inttest_marker_db 批——建统一 runner（枚举套件→PREFIX→钉库→t= 单跑→汇总），或逐套件登记本地执行手册
- [ ] T7：核实 oa_direct 的 marker 容器启动来源（phase1 文档/脚本），补登记
- [ ] 每次让位类提交：本表 + 同提交的独立通道证据（脚本/文档/最近 run id）

## 与 known-failures.yaml 的关系

- `known-failures.yaml`：**已执行且失败**的已知失败清单（五分类棘轮）。
- 本表：**环境性未执行**清单。两者正交；本表条目不得进入 known-failures
  （未执行不是失败，不参与五分类）。
