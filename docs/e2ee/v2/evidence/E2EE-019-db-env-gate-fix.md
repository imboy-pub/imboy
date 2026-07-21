# E2EE-019 附:DB 环境门系统解 + `make eunit-local`

> 归属:#4 横切质量基础设施 / 支撑 B05 (E2EE-019) 自动化基线。
> 目标:让本地 eunit 的 `?TEST_WITH_APP` / `?TEST_WITH_DB` 类用例真跑,而非 setup cancelled。
> 日期:2026-07-21。用户授权真跑迁移(含 DROP)。

## 问题:两层 DB 环境门

纯 `make eunit`(无 `-config`)时,一大批 `?TEST_WITH_APP` 用例 setup failed cancelled。根因是**两层门**,非单一:

1. **config 门**:生效的 `src/lib/eunit_runner.erl:eunit_setup/0` 在启动 app **之前** 调
   `ensure_config_loaded/0`,`application:get_env(imboy, pg_conf)=undefined` 即
   `error({missing_config, pg_conf})` **硬失败**(不像 `?TEST_WITH_DB` 优雅 skip)。
   `test/common/eunit_runner.erl` 有主动 `file:consult` 读 sys.config 的版本,但未生效(code path 顺序)。
2. **迁移乱序门**:即便加 `-config` 过了 config 门,`ensure_all_started(imboy)` 起全量 app →
   `imboy_app:start/2:22` 的 `ok = imboy_migrate:migrate()` 是 **fail-fast**;
   `imboy_migrate:migrate/0` 用 `strict => true`,`erlang_migrate:up` 检测到
   `{out_of_order, Versions}` → `erlang:error({migration_failed, ...})` → app 启动 crash。

## dev 库 imboy_v1 事实(只读核查)

- `schema_migrations` 主表 = `{37, 44, 47}`,`current_version`(=`ORDER BY version DESC LIMIT 1`) = **47**。
- `schema_migrations_history` 表 = **仅 1-37**(37 行)。44/47 是历史手动 INSERT 主表、未走 strict 流程,故不在 history。
- `check_out_of_order(Applied=1..37, Current=47, All=1..40,42..47)` →
  `Missing = {38,39,40,42,43,44,45,46,47}` ≠ [] → out_of_order → app 起不来。
- 迁移文件号:1-40, 42-47(**41 缺失**,历史 renumber,非本轨道)。

## 关键判断:必须真跑迁移,不能只补占位

"只补 history 占位"会让 schema 与 history 声称不一致 → 测试碰缺失表即炸。SELECT 证实 5 个对象 schema **真缺失**:

| 迁移 | 目标对象 | dev 库现状 | 操作性质 |
|---|---|---|---|
| 38 | e2ee_shard_transmission_log / e2ee_social_shards / e2ee_trusted_contacts / e2ee_transfer_sessions | **均存在** | DROP(社交恢复死表,删除即迁移本意) |
| 39 | moment_post.at_uids | 不存在 | 新建列 |
| 40 | user.background | 不存在 | 新建列 |
| 42 | olm_identity / olm_one_time_key / olm_fallback_key | 不存在 | 新建表 |
| 43 | user_device.trust_state(+capabilities/identity_blob…) | 不存在 | 新建列+约束+device_id 增宽 128 |
| 45 | olm_one_time_key.status(+consumed_at/claimed_by) | 不存在 | 新建列+约束+索引 |
| 46 | compliance_key.private_key_encrypted | **存在** | DROP 列(零信任删私钥,删除即迁移本意) |

7 个迁移 up.sql **全幂等**(`DROP TABLE IF EXISTS` / `ADD COLUMN IF NOT EXISTS` /
`CREATE TABLE IF NOT EXISTS` / `DROP COLUMN IF EXISTS`;`ADD CONSTRAINT` 均配对
前置 `DROP CONSTRAINT IF EXISTS`;`ALTER COLUMN … TYPE varchar(128)` 增宽幂等)。

## 修复(用户授权,dev 库 imboy_v1,非生产)

1. **真跑 7 个幂等迁移** up.sql,版本升序:38/39/40/42/43/45/46(`psql -v ON_ERROR_STOP=1 -f`)。
   全部成功;NOTICE 为幂等跳过约束、38 cascade 删关联 view `v_e2ee_shard_transmission_stats`(正常)。
   dev 库 schema 真达 migration 47 状态。
2. **补 history**:`INSERT INTO schema_migrations_history (version) VALUES (38..47) ON CONFLICT DO NOTHING`。
   history 现 46 行 = `{1..40, 42..47}`,恰等于目录全部版本 → `Missing = []`。
3. **加 `make eunit-local` target**(Makefile,唯一代码改动):
   封装 `IMBOYENV=local $(MAKE) eunit EUNIT_ERL_OPTS="-config config/sys.local -pa ebin -pa test"`。
   命令行 `t=<模块>` 经 MAKEFLAGS 自动传子 make。

## 验证

- `env IMBOYENV=local make eunit t=elib_uri_tests EUNIT_ERL_OPTS="-config config/sys.local -pa ebin -pa test"`
  → `Application imboy started on node nonode@nohost`(**迁移干净通过,不再 out_of_order**)+ `All 26 tests passed.`
  含之前受 DB 门 cancelled 的 `?TEST_WITH_APP` 用例(download_success / http_error / network_error)。
- `make eunit-local t=elib_uri_tests` → 同上(target + `t=` 传递生效)。
- 全量 `make eunit-local` 真实基线(2026-07-21):**Passed 752 / Failed 160 / Skipped 0**,
  make 返回 Error 2(有失败)。cancelled 仅 3 处(`noproc pgsql take_member`,pool 时序零星,非系统性门残留)。

### 全量基线 160 失败定性(非 #3 回归)

- 失败集中在 **handler 层**:`channel_handler_tests` 88 + `adm_group_handler_tests` 40 = 128(占 80%),
  其余为零散 adm/handler 测试。
- 根因签名:**98 function_clause** + 14 meck + 5 undef 为主。抽样
  `adm_channel_handler` → `channel_logic:get_channel_stats(<<"11">>)` → `error:function_clause`:
  测试的 meck expectation fun 只匹配旧参数模式,生产 handler 现传 `<<"11">>` 不匹配 → function_clause。
- 定性:**mock / API 漂移的预存失败**,与 dead-tests-census.md A 类(group_logic 改名)同根因
  (生产重构后 handler 测试未同步)。**非 #3 引入的回归**——这些 handler 测试在 DB 门解除前
  一律 setup cancelled、从未真跑;#3 解锁后才首次暴露。#3 自身只碰 dev 库 schema + Makefile。
- **E2EE 主线增量验证**:#3 解锁的唯一 E2EE 主线受门模块 `user_device_repo_tests` = **16/16 绿**
  (迁移 43 加 user_device.trust_state/capabilities/identity_blob 后真 PG 通过);
  `elib_uri_tests` 26/26 绿。E2EE 相关无新失败。
- **处置**:160 个 handler 测试失败修复(mock 重对齐 / 判定过时删除)是独立大工程,
  超出 #3 范围,记入 census backlog 待用户单独立项;不在本轮修。

## 备注

- dev 库真 PG 原始日志含库口令/进度,本文只记结果不贴日志(playbook §1.3)。
- **前置约束(写入 Makefile 注释)**:eunit-local 依赖本地 imboy_v1 schema 已至最新迁移;
  若他人本地库迁移号不连续,同样会 out_of_order,须先按本文修复。
- 41 号缺失(历史 renumber)不影响:`check_out_of_order` 只比对目录存在的版本。
