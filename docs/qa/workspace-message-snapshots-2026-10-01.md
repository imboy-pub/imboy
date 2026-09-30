# 工作区消息摘要与 App 分域列表 / Workspace message snapshots

日期：2026-10-01。后端基线 f84a70fcc59069ab2b21a29dabdf09d0f66714cd；App 基线 2f6f621abd139011b2f8c8fe4e091960da8f85bb。
状态：本地局部验证通过；完整企业导航、客服、Internal API 和投产验收仍未完成。

## 行为 / Behavior

复用 Human GET /api/v1/workspaces/:workspace_id/groups 的本人模式，增加 preview=1。默认目录和原本人资源列表保持兼容。preview 只允许 member_only=1；UID 仍来自 JWT，客户端不能指定其他用户。

同一 SQL 快照验证群、群成员、未关闭的成员历史代次、工作区成员、工作区及所属企业状态。先按 ID 游标限制群，再通过 LATERAL 读取该用户当前代次中的最新有效消息。来源为 msg_c2g_timeline 与 msg_c2g 的 msg_id、created_at、群 ID 联结，包含已送达记录；ACK 不被伪装为已读。本人已删除的时间线、所有人删除的消息、过期消息和重新入群前的序号不进入摘要。

采用当前投递表的原因：现有永久归档不统一跟随编辑、撤回、删除和 expire_at 更新，不能安全回落到其旧 payload。无有效消息时 latest_message=null；遵循既有投递表保留策略（迁移基线为一年），没有更长历史摘要的承诺。异步 worker 尚未写入时可能暂时没有新摘要。

App Workspace 首页替换掉全局 ConversationPage，读取完整授权分页、校验 member_start_seq 与最新序号，用独立的临时 DTO 显示列表。个人会话、全局草稿和温缓存不作为企业摘要来源。切账号使请求失效，退出停止读取；切工作区使用独立数据族。刷新及失败时不继续显示旧快照，403 明确显示错误，不自动重试成无限加载。

列表支持本工作区名称 / 当前摘要搜索、下拉刷新、时间排序，点击复用已有 C2G 聊天路由。新消息、聊天变化和重连触发权威重读；活动消息页以 30 秒周期兜底异步 worker / 漏事件。到期摘要安排刷新，客户端也隐藏已过期与 burn 内容；兼容 E2EE 的占位，不显示密文或新增加密操作入口。没有伪造未读、置顶或免打扰状态。

English: Add optional member-generation-bound message snapshots to the existing authorized group cursor endpoint. Use live delivery records including acknowledged rows, preserving deletion and expiry semantics. The App workspace feed reads these snapshots without consulting global conversation caches; account/scope changes invalidate old responses and authorization errors stay visible.

## 验证 / Checks

- 41 项后端 EUnit 通过，exit=0：Handler 分派、JWT 用户、preview 参数拒绝、目录兼容及群功能回归。
- 5 项临时 PostgreSQL 18 EUnit 通过，exit=0：实际 Logic → DS → Repo SQL；205 群在普通与摘要模式完整分页、跨范围排除、工作区 / 群 / 企业 / 代次撤销、ACK 后摘要、过期、本人删除、所有人删除、撤回 payload、重入群边界、索引幂等创建与回滚。
- 39 项 App 测试通过，exit=0：摘要契约、无效 / 入群前序号拒绝、密文与 burn / 到期预览、实际 Widget 搜索和路由、工作区迟到响应、撤权错误、账号迟到响应及退出、既有群分页 / 企业选择 / 壳回归。
- 受影响 Dart 分析 No issues found；格式与 git diff --check 通过；OpenAPI preview 参数及 schema 解析通过；架构门禁通过。
- 按真实调用链进行本地人工复核，没有独立审查代理。

App 复跑：

```sh
flutter test test/unit_test/store/api/workspace_conversations_api_test.dart test/unit_test/page/workspace/workspace_conversations_page_test.dart test/unit_test/store/api/workspace_member_groups_api_test.dart test/unit_test/page/workspace/workspace_member_groups_account_test.dart test/unit_test/page/workspace_shell/workspace_shell_page_test.dart test/unit_test/page/workspace_shell/workspace_organization_selection_test.dart
```

后端编译受影响四个源码模块、上述两份测试及 test/common/meck_helper.erl，并编译现有 group_scope_tests、workspace_create_handler_tests、group_repo_tests、group_logic_tests；使用 +debug_info、-DTEST、-DEUNIT、-I include 及项目依赖路径。EUnit 入口：

```erlang
eunit:test([workspace_group_handler_tests,group_scope_tests,workspace_create_handler_tests,group_repo_tests,group_logic_tests],[verbose]).
workspace_member_groups_pg_tests:run(os:getenv("OA_TEST_SOCKET")).
```

数据库测试要求空独立数据库、departure_test 用户、postgres 库和 Unix socket；测试在真实 epgsql 连接上运行，仅替换连接池与配置；调用方负责初始化与停止临时数据库。migration 158 为投递时间线全部非空 conv_seq（含已 ACK 行）增加读取索引，SQL 文件已准备，仅在临时测试库执行，未操作业务数据库。

## 局限与后续 / Limits

没有完成三 / 四项主导航、统一个人 / 企业选择器、公告合并、真实未读 / 置顶 / 草稿和治理选人选部门。个人页面保持原能力，工作区当前页只承载群会话。此次复用的聊天页面、本地完整历史 / 附件缓存、远程撤权的即时通知和真实设备尚未验收；周期重读不等于立即撤销本地数据。

没有真实 HTTP/JWT、Timescale 压缩分块性能或真机证明。PostgreSQL 测试为普通表，不能冒充 Timescale 部署迁移验证。新增索引需在实际目标环境进行迁移与性能验证后才能声称投产。无 push、发布或生产迁移。

## 内容绑定 / Source hashes

| 仓库与文件 | SHA256 |
|---|---|
| imboy/src/api/workspace_handler.erl | `9cdc132eda768b2b92acccb890916c26ac3977022ff23e9e77110705333452f0` |
| imboy/src/ds/workspace_ds.erl | `dcdb370a3ed36d3f9f69aee8b8ca42d3ee2ac441249bb3c13a35a96757c192ba` |
| imboy/src/logic/group_logic.erl | `cd459879c0532efae1b4aa59607fdf5f24a56950ba2e4e69ab5e537c918c8d6c` |
| imboy/src/repo/group_repo.erl | `6595046ca9b5da99fdb238640841bc6a5f546d4085b5a50a0a000286b50bc037` |
| imboy/test/api/workspace_group_handler_tests.erl | `b970c3a95d684e25b7de69ef3976a09d3036c9cbcf532d40bbb0322247c467c1` |
| imboy/test/repo/workspace_member_groups_pg_tests.erl | `d9f5221b0f70de32839961174daca86e18dab3f42577df04ab9358f1717d2d98` |
| imboy/api/paths/api/v1/workspace/groups.yaml | `f60ee1a2d1b8e19466baee2a118ba037609810ac0dac0ebbe0665c0afd194d69` |
| imboy/priv/migrations/00000158_workspace_message_preview_index.up.sql | `c828c60d07ed76b371d7e9996009aae59648e0d39a5ca0dbd5e39eb61612f293` |
| imboy/priv/migrations/00000158_workspace_message_preview_index.down.sql | `770f1f80922445222ff77e3ba66bce6585c4e300c26c1ab06275a18075772c93` |
| imboyapp/lib/store/api/workspace_api.dart | `84d1a232687982791d4cb45ea34d9d93cf161665abc069e507bd561da28b2c3e` |
| imboyapp/lib/store/model/workspace_conversation_model.dart | `519158d47e885285c1c529ac4bf1bdc3c6a6c131ef91fc984a0487a9fa99c26c` |
| imboyapp/lib/page/workspace/workspace_data_providers.dart | `5eea66cafe130b2e000fb35efa4d774850d65dca7b42aca84a1e26d1305d3916` |
| imboyapp/lib/page/workspace/workspace_conversations_page.dart | `ec7bdaf56b0da7649a008fa001f1417c7c5f231a612ad977bbb3cf99287a1427` |
| imboyapp/lib/page/workspace_shell/workspace_shell_page.dart | `e96e41e000ff878afb8352f714baa9974fbb767b2003c93e3aa0cf967ec4ffd7` |
| imboyapp/lib/page/workspace_shell/workspace_shell_nav_items.dart | `94bd02c7eeead786faadbbe848635aa616870ddf9219dc347c8077161163148b` |
| imboyapp/test/unit_test/store/api/workspace_conversations_api_test.dart | `ab4feaf86acf991ec39f99582daa5268006494faf3108a92672d79c229603fea` |
| imboyapp/test/unit_test/page/workspace/workspace_conversations_page_test.dart | `30f07ed68b01af3bd66513b4a6ebb3b72436413384177825516ab5f52dc0491a` |
| imboyapp/test/unit_test/page/workspace_shell/workspace_shell_page_test.dart | `cd0543a90760a5ee7619ce35f142c974dac3c9e5b1b2e91e28f73a3348573ca5` |
