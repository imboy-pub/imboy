# STEP-06 证据 — 命令记录

Owner: Agent C (DATABASE) | 日期: 2026-09-09 | 环境: 同 STEP-05（127.0.0.1:4323 / moya_mig_test scratch 库）

## 迁移编号复核

```
$ ls priv/migrations/ | sort | tail -6
00000094_moderation_appeal.{down,up}.sql
00000095_organization_foundation.{down,up}.sql   # Step 5 产物
00000096_teaching_identity.{down,up}.sql         # 本 Step 占用
```

## up / down / up（含幂等重跑）

工作目录 `/Users/leeyi/project/imboy.pub/imboy`，MIG=priv/migrations：

```
$ psql -d moya_mig_test -v ON_ERROR_STOP=1 --single-transaction -q -f $MIG/00000096_teaching_identity.up.sql
UP1 OK                 # exit 0（NOTICE 均为幂等 DROP IF EXISTS 预期输出）
$ psql ... -f $MIG/00000096_teaching_identity.up.sql
UP2 idempotent OK      # exit 0
$ psql ... -f $MIG/00000096_teaching_identity.down.sql
DOWN OK                # exit 0（触发器→函数→表逆序清理）
$ psql ... -f $MIG/00000096_teaching_identity.up.sql
UP-after-DOWN OK       # exit 0
```

## 约束行为测试

```
$ psql -d moya_mig_test -v ON_ERROR_STOP=1 -f /tmp/moya_mig/step6_behavior_test.sql
EXIT=0, 输出 STEP6_ALL_TESTS_PASSED（T1..T13，脚本 BEGIN...ROLLBACK 不留数据）
```

## 历史迁移文件不变

Step 5 时记录的 `shasum` 基线复查：00000001..00000094 未变（见 STEP-05/commands.md，本 Step 仅新增 00000096 两文件）。
