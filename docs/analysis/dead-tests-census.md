# 死测试普查 backlog（Dead-Test Census）

- **日期**：2026-07-21
- **方法**：只读静态分析——解析全部 `src/**/*.erl` 的 `-export` 名单，扫描 `test/**` 跨模块调用 `Mod:Fn(`，标记「函数名完全不在该 src 模块导出中」的调用（运行时必 `error:undef`），并排除同用例内被 `meck:new/expect` 的 mock 目标。
- **结论**：**132 处死调用点，14 个 test 文件，约 16 个已被清理/改名的生产函数**。根因是生产代码重构（函数改名/删除）后对应测试未同步更新。
- **已清理（本轨道，eunit 层、污染 `make eunit`）**：`auth_ds_tests:get_token_with_assets_resource_test_`（`ba8af9fe`）、`elib_uri_tests:check_auth_valid_url_test_`（`d6c8c3ac`）。

> ⚠️ 本文件是 backlog，非执行记录。以下清理**需产品/授权决策**，未执行。

## A 类 — Common Test suite（6 个，不进 `make eunit`，整体对旧 API 失效）

这些 CT suite 是对**旧 `group_logic` API** 写的集成测试；生产已把 `create/invite_members/remove_member/update_info/mute_member/unmute_member` 重构为 `add/4`、`edit/3` 等，导致整套 suite `undef` 失效。

| 文件 | 失效的旧调用（样本） |
|---|---|
| `test/group_management_flow_SUITE.erl` | `group_logic:create/2,3`、`invite_members/3`、`remove_member/3`、`update_info/3`、`mute_member/3`、`unmute_member/3`、`group_member_ds:find_by_users/2`、`list/1`、`group_notice_logic:add/3` |
| `test/group_notice_SUITE.erl` | `group_logic:create/2`、`invite_members/3`、`group_member_logic:set_role/4` |
| `test/group_vote_SUITE.erl` | `group_logic:create/2`、`invite_members/3` |
| `test/messaging_flow_SUITE.erl` | `group_logic:create/2`、`invite_members/3`、`msg_c2c_repo:find_by_msg_id/1`、`msg_c2c_logic:recall/3`、`msg_store_ds:page/3`、`msg_ack_logic:ack/3` |
| `test/msg_delivery_SUITE.erl` | `msg_c2c_ds:write/5` |
| `test/user_auth_flow_SUITE.erl` | `token_ds:refresh_token/1`、`user_logic:reset_password/3` |

**决策项**：这些群组/消息/认证流集成测试是**删除**（承认这些流程当前无 CT 覆盖）还是**重写匹配新 API**（大工程）？→ 产品决策，不在死代码清理范围内。

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

## 复现命令（样本）

```bash
grep -E "^-export" src/logic/group_logic.erl | grep -E "create|invite_members"   # 空 = 已不存在
grep -cE "meck:(new|expect)\(group_logic" test/group_management_flow_SUITE.erl    # 0 = 非 mock
find . -name eunit_runner.erl                                                     # src/lib + test/common 两份（重名陷阱）
```
