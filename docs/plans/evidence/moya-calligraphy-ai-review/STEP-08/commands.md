# STEP-08 证据 — 命令记录

Owner: Agent B (API-ACL) | 日期: 2026-09-09 | imboy=5b7e2055（main）moya=9644a2e（未触碰）

> 环境注记：按 Coordinator 指令全程未执行任何 git 命令。曾按要求新建 scratch 库
> `moya_s8_test`，后发现 4323 集群上 `moya_mig_test` 完好（167 表；此前误连默认
> 5432 集群同名空库导致误判），已 `DROP DATABASE moya_s8_test` 清理，未写任何既有业务库。

## make compile

```
$ cd /Users/leeyi/project/imboy.pub/imboy && make compile
COMPILE_EXIT=0
```
（warning_as_errors 开启下零告警；期间修复 4 类新模块告警：kernel logger include、
deprecated catch、binary construction、group 保留字引用）

## 定向 eunit（新套件）

```
$ erlc -I include -o test/ test/logic/teaching_acl_tests.erl test/logic/teaching_auth_logic_tests.erl
$ erl -noshell -pa ebin -pa test -pa deps/*/ebin -eval \
  'R = eunit:test([teaching_acl_tests], [verbose]), io:format("RESULT: ~p~n",[R]), halt(0)'
  All 13 tests passed. RESULT: ok
$ erl ... -eval 'R = eunit:test([teaching_auth_logic_tests, teaching_acl_tests], [no_tty]), ...'
  RESULT: ok   # 8 auth + 13 acl
```

## 波及既有套件（路由/中间件/认证）

```
$ erl -noshell -pa ebin -pa test -pa deps/*/ebin -eval \
  'R = eunit:test([auth_middleware_tests, billing_route_tests, channel_webhook_handler_tests,
                   feature_route_http_tests, auth_ds_tests], [no_tty]), ...'
  RESULT: ok
```

## scratch 真库 SQL 验证（moya_mig_test@127.0.0.1:4323）

```
$ PGPASSWORD=*** psql -h 127.0.0.1 -p 4323 -U imboy_user -d moya_mig_test \
    -v ON_ERROR_STOP=1 -f docs/plans/evidence/moya-calligraphy-ai-review/STEP-08/behavior-acl.sql
PSQL_EXIT=0   # BEGIN...ROLLBACK，不留数据；输出见 tests.md
```
脚本内容：9 组查询 = teaching_context_repo 全部 SQL 原文（submission_scope /
guardian_contexts / staff_contexts / owner_contexts / guardian_relation / staff_relation /
org_owner_uid / learner_org / group_org）在真实 00000001→00000097 schema 上执行。

## 最终组合复跑

```
$ make compile; COMPILE_EXIT=0
$ erl ... eunit:test([teaching_acl_tests, teaching_auth_logic_tests, auth_middleware_tests,
                      billing_route_tests, auth_ds_tests], [no_tty])
EUNIT_RESULT: ok
```

## 迭代修复记录

| 问题 | 修复 |
|---|---|
| `?LOG_ERROR/2` 未定义（log.hrl 无此宏） | 补 `-include_lib("kernel/include/logger.hrl")`（sso_identity_repo 同款） |
| `ec_str:trim/1` 不存在 | 改用 OTP `string:trim/1` |
| `catch expr` deprecated（warning_as_errors） | 改 `try ... catch` |
| `<<"\"", Bin/binary, "\"">>` 前段类型不匹配警告 | 先绑定变量再构造 |
| `elib_str:trim` 测试失败 7 例 | 同 trim 修复 + replay 用例 find_uid 改回 {ok, UID} |
| behavior SQL：user 表"不存在" | 误连 5432 集群；改 4323 显式连接 |
| behavior SQL：group_member 列名/类型（gid→group_id、role/status smallint）+ workspace_member 子集约束 | 按真实 schema 修正并补 workspace_member 前置行 |
