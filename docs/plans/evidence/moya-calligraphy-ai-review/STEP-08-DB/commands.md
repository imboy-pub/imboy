# STEP-08-DB 证据 — 00000098 修复迁移（R2）

> **泳道声明**：本目录名带 `-DB` 后缀以区别于 Agent B 的 `STEP-08/`；内容归 **DATABASE 泳道（Agent C）** 所有。
> 修复对象：`homework_submission` 幂等/撤回审计（对齐 Step 4 冻结契约 §7.2）；schema gap 由用户现场审计坐实。

Owner: Agent C (DATABASE) | 日期: 2026-09-09 | 环境: docker PG `imboy_pg18`（127.0.0.1:4323，PG 18）/ scratch 库 `moya_mig_test`（00000001→00000097 全量态基础上叠加）

## 迁移编号复核

```
$ ls priv/migrations/ | sort | tail -3
00000096_teaching_identity.up.sql
00000097_homework_review_loop.down.sql
00000097_homework_review_loop.up.sql     # 基线，占用 00000098
```

## up / down / up（含幂等）

```
$ psql -d moya_mig_test -v ON_ERROR_STOP=1 --single-transaction -q -f $MIG/00000098_submission_idempotency_withdraw.up.sql
UP1 OK / UP2 idempotent OK / DOWN OK / UP-after-DOWN OK   # 全部 exit 0
```

- 插曲 1：up 初稿第 23 行笔误 `ADD COLUMN IF EXISTS`（应为 IF NOT EXISTS），psql 语法错误即时暴露后修复。
- 插曲 2：up-after-down 曾报 `ck_homework_submission_withdraw_audit is violated by some row`——根因是上一轮并发测试遗留的已提交 withdrawn 数据（withdrawn_by 竞态穿透行）在 down 掉列后重加列时全为 NULL。清理夹具数据后通过。**这同时实证了 `ADD CONSTRAINT` 会对存量行做校验（fail-fast 正确行为），且该遗留行本身就是要修的竞态产物（见 tests.md）。**

## 行为测试

```
$ psql -d moya_mig_test -v ON_ERROR_STOP=1 -f /tmp/moya_mig/step8db_behavior_test.sql
EXIT=0, 输出 STEP98DB_BASIC_TESTS_PASSED（T1..T6）
$ /tmp/moya_mig/step8db_T6_runner.sh     # 并发取号（两会话真并发）
s1_exit=0 s2_exit=0；attempt_no=2,3 无重复无死锁
$ /tmp/moya_mig/step8db_T7_runner.sh     # 撤回/发布互斥（三场景真并发）
场景A withdraw_exit=0 publish_exit=3（PUBLISH_BLOCKED）
场景B publish_exit=0 withdraw_naive_exit=3（撤回互斥触发器拦截竞态穿透）
场景C publish_exit=0 withdraw_lockfirst_exit=0（UPDATE 0 干净拒绝）
```

## make compile 与 eunit

```
$ cd imboy && make compile                 # EXIT=0
$ erlc -I include -o test/ test/repo/moya_teaching_migration_tests.erl   # exit 0
$ erl ... eunit:test([moya_teaching_migration_tests, group_task_repo_tests,
    group_task_assignment_repo_tests, message_dedup_migration_tests,
    channel_reaction_summary_migration_tests, imboy_migrate_tests], [])
All 64 tests passed.   # exit 0（新增 4 个 00000098 断言；既有 group_task 套件无回归；
                       # 输出中的 "Error: syntax_error" 为 imboy_migrate_tests mock 的预期用例输出）
```
