# STEP-09 证据 — 命令记录

Owner: Agent B (API-ACL) | 日期: 2026-09-09 | imboy 工作区基线 5b7e2055（未 git 操作）

## make compile

```
$ cd /Users/leeyi/project/imboy.pub/imboy && make compile
COMPILE_EXIT=0   # warning_as_errors 下零告警
```

## 真库集成测试（moya_mig_test@127.0.0.1:4323，直连 epgsql，BEGIN...ROLLBACK）

```
$ erlc -I include -o test/ test/repo/teaching_flow_integration_tests.erl   # EXIT=0
$ erl -noshell -pa ebin -pa test -pa deps/*/ebin -eval \
  'R = eunit:test([teaching_flow_integration_tests], [verbose]), io:format("RESULT: ~p~n",[R]), halt(0)'
  All 6 tests passed. RESULT: ok
```

覆盖：FLOW-01 主链 / IDEMP-01 / STATE-01×4（见 tests.md）。

## 全量回归（教学 + 受波及既有套件）

```
$ erl -noshell -pa ebin -pa test -pa deps/*/ebin -eval \
  'R = eunit:test([teaching_flow_integration_tests, teaching_acl_tests,
     teaching_auth_logic_tests, auth_middleware_tests, billing_route_tests,
     auth_ds_tests], [no_tty]), ...'
  ALL_RESULT: ok
```

## scratch 库残留检查（ROLLBACK 验证）

```
$ psql -d moya_mig_test -tAc "SELECT count(*) FROM homework_submission WHERE submitted_by IN (980001,980002,980003)"
0
$ psql -d moya_mig_test -tAc "SELECT count(*) FROM \"user\" WHERE account LIKE 't99_%'"
0
```

## 迭代修复记录

| 问题 | 修复 |
|---|---|
| `catch expr` deprecated ×2（handler int_param） | try/catch |
| logic 未用变量 / `{ok,_Row}` 吞 `{ok,undefined}` | `_GroupId` / `is_map` guard |
| review repo 缺 tb/1、导出 publish_tx/2→/3 | 补齐 |
| submission repo 导出 arity 不符（insert_assets_tx/4、history/3、find/assets 池版） | 修正导出表 |
| TSID 命名生成器未注册（elib_tsid_not_initialized） | repo 改用默认生成器 `elib_tsid:generate()`；测试 setup 直连模式 `elib_tsid:init(#{dc_id=>1,node_id=>1,dc_bits=>3})` |
| 测试中文二进制缺 `/utf8`（PG 22021） | 全部补 /utf8（含多段 binary 的末段） |
| seed SQL task_id 未加引号（42703） | 补引号 |
| upsert 返回 `{ok,[Row]}` 未解包 | repo 内 unwrap_row |
| 集成测试调池版 find_published（noproc pooler） | publish_zero_rows 改用 find_published_tx（同连接），测试同 |
| IDEMP-01 附件计数预期 2 → 实际 4（2 次真实提交×2 附件；重放未复制才是断言点） | 修预期并注明语义 |
