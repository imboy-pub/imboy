# STEP-16 命令与测试证据 — 学员账号绑定与历史复核

> Agent D（MOYA，Coordinator 特批后端文件域）。基线：imboy 工作区（B 的教学域文件未跟踪/进行中，本波零改动）；无 git 写操作。

## 质量门

| 命令 | 结果 |
|---|---|
| `make compile` | 0（teaching_learner_bind_logic/repo 两个新模块编译通过） |
| 定向 eunit：`teaching_learner_bind_integration_tests`（真库 4323 scratch，BEGIN/ROLLBACK） | **All 8 tests passed** |
| 复跑 B 的套件：`[teaching_flow_integration_tests, teaching_acl_tests]` | **All 19 tests passed（不回归）** |

注：`make eunit-local` 的 test-build 当前被 **B 的 WIP 文件** `test/logic/teaching_attach_logic_tests.erl`（?WITH_MECKS 宏 badly formed，未跟踪、20:08 修改）挡住——非本波文件；我的定向 eunit 采用 erlc 手动编译 + `erl -pa ebin -pa test -pa deps/*/ebin` 直跑（与 make 同 beam 路径），结果有效。**建议 Coordinator 提醒 B 修复后恢复 make 入口。**

## 交付文件（全部新建，B 零改动）

- `src/repo/teaching_learner_bind_repo.erl`：find_tx / operator_role_tx（owner|manager|unauthorized 守卫）/ bind_tx / unbind_tx / classify_bind_error（23505+uk_learner_org_user → duplicate_bind_in_org；23503 → invalid_target_user；兼容 epgsql map 与 tuple 两种错误形态）。
- `src/logic/teaching_learner_bind_logic.erl`：bind_learner/3 / unbind_learner/2（with_tx 包装；守卫 deny-by-default；审计=结构化日志【仅 ID 无 PII】+ learner.account_bound_at/by 行级痕迹）。
- `test/repo/teaching_learner_bind_integration_tests.erl`：8 用例（下表）。

## 用例 → 验收映射（8/8 绿）

| 用例 | 验收 |
|---|---|
| guard_test：owner✓ manager✓；teacher(非 manager)/assistant/家长/陌生人/OrgB-owner 对 OrgA learner 全部 unauthorized | deny-by-default（BIND 守卫） |
| bind01_no_copy_no_move：绑定前后 homework_submission（id/learner_id/assignment/attempt）与 teacher_review（id/submission/reviewer/status）快照逐字节相等 | **BIND-01**（无复制无迁移） |
| bind02_unbind：解绑 user_id=NULL；learner/submission×2/review×2 全保留；监护人 guardian_learner 行与已发布回评可读；账号本人 guardian+staff 两分支关系均为 0 行（入口立即失效）；account_bound_* 痕迹保留 | **BIND-02** |
| dup_same_org：同 Org 第二 learner 绑同 user → duplicate_bind_in_org（23505 分类） | §6.4 UNIQUE 语义 |
| rebind_after_unbind：解绑后重新绑定成功（幂等场景） | 同上 |
| cross_org_bind：**同 OrgA 已绑 + OrgB 再绑同 user 成功**（且 A/B 各自 org 正确） | DB-BIND-01 跨机构独立 |
| invalid_target_user：不存在用户 → FK 23503 分类 | 防御 |
| history01：同 Org 跨两 Workspace 已发布回评按 learner 聚合返回 2 条（ws 集合=[A1,A2]）；跨 Org guardian 关系 0 行 + 跨 Org staff 关系 0 行 + OrgB learner 无 OrgA 提交 | **HISTORY-01 复核** |

## 过程中的真实失败与修复

1. **badarg（atom 进 binary 段）**：直连测试模式 config_ds 未初始化，public_tablename 原样返回 atom → tb(learner) 拼接崩溃；修为 B 同构的 `ec_cnv:to_binary` 先转（这也说明 B 的 repo 之所以那样写的原因）。
2. **epgsql 错误形态**：23505 实际是 tuple `{error, Sev, Code, Name, Msg, Extra}` 非 map → classify 补 tuple 子句（map 形态保留兼容）。
3. **25P02 陷阱**：同一事务里故意触发 23505 后事务 abort，后续断言全部 in_failed_sql_transaction → dup/rebind/cross/FK 拆为 4 个独立事务用例。
4. 种子数据中文触发 UTF8 编码错（binary 拼接裸段截断 codepoint）→ 种子名改 ASCII（断言语义不依赖名称）。
