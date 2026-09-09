# STEP-07 证据 — 命令记录

Owner: Agent C (DATABASE) | 日期: 2026-09-09 | 环境: 同 STEP-05/06（127.0.0.1:4323 / moya_mig_test）

## 迁移编号复核

```
$ ls priv/migrations/ | sort | tail -4
00000096_teaching_identity.{down,up}.sql    # Step 6 产物
00000097_homework_review_loop.{down,up}.sql # 本 Step 占用
```

## 前置代码依赖排查（错误契约兼容）

```
$ grep -rn "task_id_user_id_key" test/repo/group_task_assignment_repo_tests.erl
:292  {error, {unique_violation, <<"group_task_assignment_task_id_user_id_key">>}}
```
→ 现有测试依赖原约束名错误契约：部分唯一索引沿用同名 `group_task_assignment_task_id_user_id_key`。

```
$ grep -n "group_task_assignment" src/lib/user_deletion_executor.erl
:274  {<<"group_task_assignment">>, [<<"user_id">>]}   # 用户删除=物理 DELETE assignment 行
```
→ 新表 FK 设计规避用户删除阻塞：submission→assignment CASCADE 保持现有流程；引用 "user" 的列（submitted_by/created_by/reviewer_uid）可空 + SET NULL。

## up / down / up（含幂等）

```
$ psql -d moya_mig_test -v ON_ERROR_STOP=1 --single-transaction -q -f $MIG/00000097_homework_review_loop.up.sql
UP1 OK / UP2 idempotent OK / DOWN OK / UP-after-DOWN OK   # 全部 exit 0
```

## down 数据保护验证（有教学数据时）

```
# 制造同 (task_id,user_id) 两行教学 assignment 后执行 down：
ERROR: down 00000097 无法恢复原唯一约束 ... 存在同 (task_id,user_id) 多行（教学作业数据）。
HINT: 本 down 迁移不删除任何业务数据
# 随后确认 2 行数据仍在；清理测试数据后 down 成功、再 up 成功（scratch 库保持 97 全量最终态）
```

## 约束行为测试

```
$ psql -d moya_mig_test -v ON_ERROR_STOP=1 -f /tmp/moya_mig/step7_behavior_test.sql
EXIT=0, 输出 STEP7_ALL_TESTS_PASSED（T1..T17）
```

## make compile

```
$ cd /Users/leeyi/project/imboy.pub/imboy && make compile
EXIT=0（仅既有 erlang.mk 目标覆盖 warning，非本次引入）
```

## eunit 定向套件

```
$ erlc -I include -o test/ test/repo/moya_teaching_migration_tests.erl   # exit 0
$ erl -noshell -pa ebin -pa test -pa deps/*/ebin -eval 'eunit:test([moya_teaching_migration_tests,
    group_task_repo_tests, group_task_assignment_repo_tests,
    message_dedup_migration_tests, channel_reaction_summary_migration_tests], [])'
All 53 tests passed.  # exit 0 —— 含 DB-COMPAT-01 所需现有 group_task 套件
$ erl ... -eval 'eunit:test([imboy_migrate_tests], [])'
All 7 tests passed.   # exit 0（ERROR REPORT 为用例内 mock SQL syntax_error 的预期输出）
```
