# 工作区本人群列表 / Workspace member group list

日期：2026-10-01。后端基线 035d7299c05412aa179bae9792a8e99d68cfdcdf；App 基线 e9cec27c2f39dedadcaae7a6d4ea46deb6b9a290。
状态：本地局部验证通过；企业导航与完整投产验收仍未完成。

## 行为 / Behavior

现有 GET /api/v1/workspaces/:workspace_id/groups 保留默认资源目录语义。新增 member_only=1，当前用户只来自 Human JWT，参数不能指定其他用户；cursor 为排他群 ID 下界，limit 默认 100、收敛到 1–200。

本人模式在 SQL 中同时检查有效群、有效群成员、未关闭历史代次、当前工作区成员、工作区 active/archived、所属企业 active；个人群、其他工作区群与未加入群不进入结果。企业成员与工作区成员保持独立，外部协作者仍按真实工作区资格读取，不能因此获取企业通讯录、OA 或文件。

返回 list、has_more、next_cursor，ID 升序；末页 next_cursor=0。修复原 handler 将 elib_param:int 的 {ok, Value} 当整数参与 min/max 导致实际 limit 始终为 200 的问题。群目录 SQL 从 Logic 移至 Repo，通过 DS 调用。

App 工作区群页使用完整本人分页，不再只取目录前 100 个群。邀请页继续使用原目录函数。任何一页失败都不返回部分结果；响应工作区、资源归属、状态、顺序、游标或分页结构无效均报错。数据 provider 监听当前 UID，退出立即清空；同工作区旧账号迟到响应不覆盖新账号。

English: Add an authenticated member-only cursor mode while preserving the existing directory mode. Recheck current membership and parent status in SQL. The App group page consumes all pages and rejects malformed or cross-scope responses; account changes invalidate the data provider.

## 本轮证据 / Evidence

- 后端相关 EUnit：41 passed；包含 handler 用户取值、模式分派、条数收敛、拒绝未授权调用及旧群逻辑 / Repo 回归。
- 临时 PostgreSQL 18：3 passed；实际 Handler 以下 Logic → DS → Repo 查询，205 个本人群按三页完整读取，个人 / 其他工作区 / 未加入 / 停用群排除，撤销工作区或群资格、关闭历史代次、归档企业不返回资源；归档工作区只读保留。游标和限额非法参数拒绝。
- App：30 tests passed；多页完整读取、中途 403、旧服务端缺分页、跨范围响应、重复 ID 与错误游标拒绝；真实 Riverpod 切账号迟到响应 / 退出清空；现有工作区壳和企业选择回归。
- 受影响 Dart 与测试分析 No issues found；Erlang 格式与差异空白检查通过。OpenAPI YAML 解析、两份入口引用及 Envelope 文件引用校验通过。
- 本地按调用链复核；没有独立审查代理或设备验收。

数据库复跑（空独立数据库，账号 departure_test、库 postgres、Unix socket，配置 epgsql_codec_rfc3339_bin；调用方负责创建和停止数据库）：

```erlang
workspace_member_groups_pg_tests:run(os:getenv("OA_TEST_SOCKET")).
```

后端 EUnit：workspace_group_handler_tests、group_scope_tests、workspace_create_handler_tests、group_repo_tests、group_logic_tests。先编译这些测试与四个受影响源码模块并设置 ebin 依赖路径。App 可复跑：

```sh
flutter test test/unit_test/store/api/workspace_member_groups_api_test.dart test/unit_test/page/workspace/workspace_member_groups_account_test.dart test/unit_test/page/workspace_shell/workspace_shell_page_test.dart test/unit_test/page/workspace_shell/workspace_organization_selection_test.dart
```

本轮源码 / 数据库测试 SHA256：

| 文件 | SHA256 |
|---|---|
| src/api/workspace_handler.erl | `571776a63dfda2d1683b54fa7befa14ee23a4d247cc7c9e6aee5b95588406dcc` |
| src/ds/workspace_ds.erl | `bb2394b4280f86d0b02ae5dbad0586ff99d5364849d6e7f6f32fe438dfc46162` |
| src/logic/group_logic.erl | `9b960a61cc9dd20667548e99159ef6d3c6f51823b25007363e153b71452b8390` |
| src/repo/group_repo.erl | `b5e992d1ace06047d8d45e9290ac80bb45682d4ddf99be487469ba5350f392a0` |
| test/api/workspace_group_handler_tests.erl | `0cf0aa6951097852b34c0c04079165e6afc0ea893d4d8bbf5d882369b093bb05` |
| test/repo/workspace_member_groups_pg_tests.erl | `cefeaa30163f885dac4395556c8f92db5095a912327217ae79ace1ed43d864e9` |

## 尚未证明 / Limits

新 App 群页依赖新后端本人分页契约；旧服务端缺字段时显示重试错误，不回落到全部群目录。此接口仅返回资源资料，不能单凭它显示旧会话摘要、未读或历史消息；重入群的历史代次边界还需接入消息投影。

尚未把 Workspace 全局 ConversationPage 替换成分域消息页，也没有完成三 / 四项导航、公告分类和个人统一切换器。未做真实 HTTP / 真机、全集 Internal API 或客服生产验收。本轮无新增迁移、无业务数据库操作、无 push / 部署。
