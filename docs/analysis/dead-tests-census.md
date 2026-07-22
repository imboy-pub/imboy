# 死测试普查 backlog（Dead-Test Census）

- **日期**：2026-07-21
- **方法**：只读静态分析——解析全部 `src/**/*.erl` 的 `-export` 名单，扫描 `test/**` 跨模块调用 `Mod:Fn(`，标记「函数名完全不在该 src 模块导出中」的调用（运行时必 `error:undef`），并排除同用例内被 `meck:new/expect` 的 mock 目标。
- **结论**：**132 处死调用点，14 个 test 文件，约 16 个已被清理/改名的生产函数**。根因是生产代码重构（函数改名/删除）后对应测试未同步更新。
- **已清理（本轨道，eunit 层、污染 `make eunit`）**：`auth_ds_tests:get_token_with_assets_resource_test_`（`ba8af9fe`）、`elib_uri_tests:check_auth_valid_url_test_`（`d6c8c3ac`）。

> ⚠️ 本文件是 backlog，非执行记录。以下清理**需产品/授权决策**，未执行。

## A 类 — Common Test suite（6 个，不进 `make eunit`，整体对旧 API 失效）✅ 已全部重写完成（2026-07-22）

这些 CT suite 是对**旧 API** 写的集成测试；生产已把相关函数重构（`create/invite_members/remove_member/update_info/mute_member/unmute_member` → `add/4`、`edit/3` 等；消息/认证层同理），导致整套 suite `undef` 失效。

**决策已定（用户授权 loop 会话）：全部重写匹配当前生产 API，非删除。** 6/6 完成，均对真 PG 跑 0 failed。重写过程另抓出 **4 个真生产 bug**（改实现非改测试，见下）。

| 文件 | 原失效旧调用（样本） | 状态 | 提交 |
|---|---|---|---|
| `test/msg_delivery_SUITE.erl` | `msg_c2c_ds:write/5` | ✅ 重写 | `1f294d39` |
| `test/group_vote_SUITE.erl` | `group_logic:create/2`、`invite_members/3` | ✅ 重写 | `fe510cc1` |
| `test/group_notice_SUITE.erl` | `group_logic:create/2`、`invite_members/3`、`group_member_logic:set_role/4` | ✅ 重写 | `59b1aca8` |
| `test/group_management_flow_SUITE.erl` | `group_logic:create/2,3`、`invite_members/3`、`remove_member/3`、`update_info/3`、`mute_member/3`、`unmute_member/3`、`group_member_ds:find_by_users/2`、`group_notice_logic:add/3` | ✅ 重写 (17/0) | `d7fe2abf`(+fix `764f92e7`) |
| `test/messaging_flow_SUITE.erl` | `group_logic:create/2`、`invite_members/3`、`msg_c2c_repo:find_by_msg_id/1`、`msg_c2c_logic:recall/3`、`msg_store_ds:page/3`、`msg_ack_logic:ack/3` | ✅ 重写 (14/0) | `7266f98f`(+fix `4d160374`) |
| `test/user_auth_flow_SUITE.erl` | `token_ds:refresh_token/1`、`user_logic:reset_password/3`、`change_password/3`、`verification_code_logic` | ✅ 重写 (14/0) | `148a2d15`(+fix `b3567197`) |

**重写抓出的真生产 bug（CT 覆盖的价值）**：
1. `group_logic:do_transfer/5`（transfer/3 群主转让生产路径）：原子键 `#{id := _}` 匹配 repo 二进制键 map 恒失败，且传 binary role 给 smallint 列崩连接 → 转让群主一直坏。`764f92e7` 修。
2. `group_log_repo:batch_add`：INSERT 缺 `VALUES` 关键字（42601）+ 漏 NOT NULL id 列 + 参数序颠倒，自引入从未跑通；在 dissolve 事务内致 25P02 连锁。`764f92e7` 修。
3. `group_ds:dissolve_group`：忽略 `with_tx` 返回值恒返 ok，事务回滚仍报成功 → 解散群静默失效。`764f92e7` 修。
4. `msg_c2c_ds:revoke_offline_msg/9`：spec 声明 `ok|{error}` 但恒返 `{ok,N}`，唯一调用方 `c2c_revoke` 只匹配前两者 → **离线 C2C 撤回必 case_clause 崩溃**（原文永不被覆盖）。`4d160374` 修。
5. `user_logic:change_password/2`：E2EE-013 把 `verify_user/2→/3` 后漏改此调用点 → **改密自 E2EE-013 起 undef 崩溃**。`b3567197` 修。

（bug 1-3 属 group_management 轮次，4 属 messaging_flow 轮次，5 属 user_auth_flow 轮次。均未被单测发现：离线/转让/改密路径要么被 mock 走在线分支跳过，要么无对应单测。）

## B 类 — eunit 类死测试（7 个，可能污染 `make eunit` 全量，同 auth_ds/elib_uri 模式）

| 文件 | 死调用 |
|---|---|
| `test/api/fts_logic_tests_simple.erl` | `fts_logic:search/4`（实为 `search_msg/5,6`）|
| `test/ds/adm_user_ds_tests.erl` | `adm_user_ds:list/2` |
| `test/lib/imboy_cache_sync_tests_simple.erl` | `imboy_cache_sync:set/3`、`handle_message/1` |
| `test/integration/group_notice_integration_tests.erl` | `group_notice_logic:create/3`、`group_member_ds:set_role/3` |
| `test/performance/db_query_performance_tests.erl` | `fts_logic:search/3`、`friend_repo:list/1`、`msg_c2c_repo:list/4` |
| `test/performance/msg_send_performance_tests.erl` | `msg_c2c_repo:list/4` |
| `test/performance/websocket_performance_tests.erl` | `websocket_ds:connect/3`、`disconnect/1`、`heartbeat/1` |

**决策项**：这些与已清的 auth_ds/elib_uri 同模式（调已删函数必 undef）。可逐个确认后删除（每个需先核对该函数确无新等价实现、非漏改名）。**授权后**可批量清理。

## B' 类 — 已排除（非死测试，勿删）

- `Mod:module_info/1`、`imboy_plugin:behaviour_info/1`：编译器/behaviour 自动导出，永远存在。
- `eunit_runner:ct_suite_setup/1`、`ct_suite_cleanup/1`（9 个 SUITE，18 处）：**模块重名**——`src/lib/eunit_runner.erl` 无，但 `test/common/eunit_runner.erl` 有并导出，运行时提供，不 undef。
- `websocket_connection_flow_SUITE.erl`（28 处 `websocket_logic:*` / `token_ds:refresh_token`）：多数被 `meck:new/expect`，排除；但 `connect`/`heartbeat` 未见对应 `meck:expect`，若无 passthrough 仍可能 undef → **人工确认**。
- arity 不符类（374 处，名字存在但参数疑似不符）：静态计数对多行/宏不可靠，**未采信**。

## C 类 — handler 层 mock/API 漂移失败（DB 门解除后暴露，160 失败）

- **来源**：2026-07-21 全量 `make eunit-local` 基线（DB 环境门系统解后首次真跑，见
  `docs/e2ee/v2/evidence/E2EE-019-db-env-gate-fix.md`）。**Passed 752 / Failed 160**。
- **分布**：`channel_handler_tests` 88 + `adm_group_handler_tests` 40 = 128（占 80%），其余零散
  adm/handler 测试（adm_auth_middleware 5、adm_attach 4、adm_stats 3…）。
- **根因签名**：98 `function_clause` + 14 `meck` + 5 `undef`。样本
  `adm_channel_handler` → `channel_logic:get_channel_stats(<<"11">>)` → `function_clause`：
  测试 meck expectation fun 只匹配旧参数模式，生产 handler 现传的参数不匹配。
- **性质**：与 A 类同根因（生产重构后测试未同步），但发生在 **handler 层且进 `make eunit`**。
  DB 门解除前这些 `?TEST_WITH_APP` 用例一律 setup cancelled 从未真跑，故此前未暴露。
- **处置（需授权）**：逐 handler 文件甄别——meck 重对齐（生产仍在用）vs 判定过时删除。
  独立大工程，未执行。E2EE 主线相关测试（`user_device_repo_tests` 16/16、`elib_uri_tests` 26/26）
  无新失败，不受影响。

### C 类清理完成（2026-07-21，160/160 全清）

- **前序会话清 140**（channel 88 + adm_group 40 + 零散 12），本会话清最后 **20**（5 批，均 meck/断言
  重对齐到当前生产契约，非删除）：metrics/group_album/group_schedule（cowboy_req 缺 `peer/1`/`method/1`
  meck 透传崩）、conversation（mine 薄适配委托 `conversation_logic:list/2`）、friend_category（→
  `friend_category_logic:add/2`）、mention（信封 `list`→`items`）、adm_channel（`get_channel_stats`
  收 binary）、websocket（`send_next` 第 6 参 `[DID]+true`）、adm_user_ds（`count/0` 委托 repo）、
  adm_passport（redirect `/adm/`、logout cookie path `/`）、msg（c2c/s2c 走 `read_msg_for_device/4`）、
  channel_ds（`is_subscribed/2` 边界）、passport（signup/find_password ip=`127.0.0.1`）。
  提交 `005ffbdc`/`53ad3f40`/`73c51886`/`18a6466b`/`d1cdd664`。全量 `make eunit-local` 清跑，13 模块全绿。
- ⚠️ 快 harness 会把 helper（`meck_helper`/`cowboy_req_h` 等）编到 `test/` 扁平；`rm -f test/*.beam`
  后须 `find test -name '*.erl' -exec touch {} +` 强制全量重编，否则增量 `make eunit` 只重编改动文件、
  helper beam 缺失 → 大量 `context setup failed`（假象非回归）。

### D 类 — 全量清跑浮现的 4 个预存失败（非 C 类、非本轨道回归）

清跑 `make eunit-local` 全量后暴露（首轮 partial 增量运行未执行到；git 证本会话 5 提交未碰这 3 文件）：

| 文件 | 失败 | 定性 | 处置 |
|---|---|---|---|
| `test/ds/websocket_ds_tests.erl:75` | `idle_timeout(1)` 断言 `128000` 漂移 | 确定性：生产 `config_ds:env(ws_idle_timeout_ms,180000)` 无 config 覆盖 | **已修 `f373216c`**（→180000） |
| `test/lib/imboy_plugin_sup_tests.erl:63` | supervisor 子进程数断言 `5` 漂移 | 确定性：生产新增 `imboy_ws_action_registry`（WS 路由查表前置）→6 子进程 | **已修 `f373216c`**（→6+断言新 child） |
| `test/ds/group_category_ds_tests.erl:48/:66` | `noproc pgsql take_member` | **确定性缺 mock**（非 flake）：`find_by_uid/1` 先调 `count_groups_grouped_by_category/1` 聚合群数，测试只 mock `list_by_uid/2`，未列函数 passthrough 打真 elib_pg | **已修 `2e4d0da4`**（补 count mock） |

**处置**：D 类 4 个失败**全部已修**（`f373216c` + `2e4d0da4`，均为断言/mock 对齐当前生产、非回归）。
⚠️group_category 初判「pool flake」有误——noproc 是缺 mock 打真 DB 的**症状**，根因是确定性 mock 漂移（同 C 类模式）。

## 复现命令（样本）

```bash
grep -E "^-export" src/logic/group_logic.erl | grep -E "create|invite_members"   # 空 = 已不存在
grep -cE "meck:(new|expect)\(group_logic" test/group_management_flow_SUITE.erl    # 0 = 非 mock
find . -name eunit_runner.erl                                                     # src/lib + test/common 两份（重名陷阱）
```
