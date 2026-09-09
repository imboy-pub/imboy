# STEP-05 证据 — 命令记录

Owner: Agent C (DATABASE) | 日期: 2026-09-09 | 环境: 本机 docker PG `imboy_pg18` (127.0.0.1:4323, PG 18, user=imboy_user)

## 迁移编号复核（开工前置检查）

```
$ ls /Users/leeyi/project/imboy.pub/imboy/priv/migrations/ | sort | tail -3
00000093_channel_webhook_token_digest.down.sql
00000094_moderation_appeal.down.sql
00000094_moderation_appeal.up.sql
```
结论：最新编号 00000094，与任务基线一致，无冲突。本 Step 占用 **00000095**。

## 环境探测

| 命令 | 退出码 | 摘要 |
|---|---|---|
| `pg_isready -h 127.0.0.1 -p 4323` | 0 | accepting connections（docker 容器 imboy_pg18，up 26h） |
| `psql -h 127.0.0.1 -p 4323 -U imboy_user -d postgres -tAc "SELECT ..."` | 0 | imboy_user 为超级用户（usesuper=t），仅新建专用测试库，未触碰 imboy_v1 等现有库 |

## 空库全链历史迁移基线（DB-ORG-01 前置）

```
# scratch 库创建 + 扩展安装（timescaledb/postgis/pgcrypto/pg_jieba/pg_trgm）
$ psql -d postgres -c "CREATE DATABASE moya_mig_test"                      # exit 0
$ psql -d moya_mig_test -c "CREATE EXTENSION IF NOT EXISTS <ext>"  # ×5    # exit 0

# 按序执行 00000001..00000094 全部 up.sql（每文件单事务 ON_ERROR_STOP=1）
$ /tmp/moya_mig/run_all_up.sh moya_mig_test   # 工作目录: imboy 仓
EXIT=0, OK 93/93（00000001_foundation → 00000094_moderation_appeal）
```

## Step 5 迁移 up / down / up（DB-ORG-01）

工作目录 `/Users/leeyi/project/imboy.pub/imboy`，MIG=priv/migrations：

```
$ psql -d moya_mig_test -v ON_ERROR_STOP=1 --single-transaction -q -f $MIG/00000095_organization_foundation.up.sql
UP1 OK                          # exit 0（NOTICE: 幂等 DROP IF EXISTS 跳过，符合预期）
$ psql ... -f $MIG/00000095_organization_foundation.up.sql
UP2 idempotent OK               # exit 0（重复执行幂等）
$ psql ... -f $MIG/00000095_organization_foundation.down.sql
DOWN OK                         # exit 0
$ psql ... -f $MIG/00000095_organization_foundation.up.sql
UP-after-DOWN OK                # exit 0
```

## 历史迁移文件不变（DB-ORG-01）

```
$ shasum $MIG/*.sql | grep -v "0000009[567]" > before/after
$ diff before after
HISTORICAL_MIGRATIONS_UNCHANGED   # exit 0（94 个历史文件校验和完全一致）
```

## 约束行为测试（DB-ORG-02 / DB-ORG-03）

```
$ psql -d moya_mig_test -v ON_ERROR_STOP=1 -f /tmp/moya_mig/step5_behavior_test.sql
EXIT=0, 输出 STEP5_ALL_TESTS_PASSED（T1..T8，详见 tests.md）
```
