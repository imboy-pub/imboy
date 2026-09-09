# STEP-10 证据 — 命令记录

Owner: Agent B (API-ACL) | 日期: 2026-09-09 | imboy 工作区基线 5b7e2055（未 git 操作）

## make compile

```
$ cd /Users/leeyi/project/imboy.pub/imboy && make compile
COMPILE_EXIT=0   # warning_as_errors 下零告警
```

## 新套件

```
$ erlc -I include -o test/ test/logic/teaching_attach_logic_tests.erl \
    test/repo/teaching_attach_integration_tests.erl          # EXIT=0
$ erl -noshell -pa ebin -pa test -pa deps/*/ebin -eval \
  'eunit:test([teaching_attach_logic_tests], [no_tty])'      # UNIT: ok（19 用例）
$ erl ... 'eunit:test([teaching_attach_integration_tests], [verbose])'
  All 3 tests passed. INTEG: ok   # 真库 moya_mig_test@4323，BEGIN/ROLLBACK
```

## 回归（教学全量 + 附件波及）

```
$ erl -noshell -pa ebin -pa test -pa deps/*/ebin -eval 'eunit:test([
    attach_logic_tests, attach_pending_cleanup_tests,
    teaching_attach_logic_tests, teaching_attach_integration_tests,
    teaching_flow_integration_tests, teaching_acl_tests, teaching_auth_logic_tests,
    auth_middleware_tests, billing_route_tests, auth_ds_tests], [no_tty])'
```

结果：
- attach_pending_cleanup_tests = ok
- attach_logic_tests = **36 passed / 1 failed**；唯一失败
  `authorize_moment_visible_grants_test_`，失败原因是 meck 报
  `{undefined_module, moment_ds}` —— moment 功能经 BUILD-00R 编译期物理裁剪未进
  本构建（attach_logic 头部注释明载该设计），属**既有环境性失败，与本次教学
  钩子无关**（本钩子只新增 teaching 子句，未触碰 moment 路径；presign/confirm/
  authorize 其余 36 用例含 can_upload/authorize 各 scope 全过）
- 其余全部 ok（teaching_flow_integration 6/6、teaching_acl 13、teaching_auth 8、
  auth_middleware/billing_route/auth_ds）

## 迭代修复记录

| 问题 | 修复 |
|---|---|
| `catch expr` deprecated ×3 | try/catch |
| presign 拆分后残留多余 `end` | 删除 |
| 集成测试中文段缺 /utf8（PG 22021） | 补齐（含机构A/校区/班级/标题） |
| AgeClause 拼进 VALUES 尾部（42601） | 改 INSERT 后单独 UPDATE created_at |
| `id =< 989005`（Erlang 操作符误入 SQL，42883） | `<=` |
| 单 tuple 传入 WITH_MECKS（期望 list） | 包一层 list |
| media01 Status 传原子非 binary | `<<"submitted">>` |
| media03 生成器返回非 test | 改 ?_assert 列表 |
| 新 repo 函数走池连接，直连测试 noproc | 加 `_tx/2` 变体（run-pattern，同 find/assets 先例） |
