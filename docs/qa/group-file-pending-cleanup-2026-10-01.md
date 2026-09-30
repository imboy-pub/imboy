# 群文件上传登记与清理 / Group file pending cleanup

日期：2026-10-01。基线：71afe565f522f769c923a4d5a267a126825370ec。
状态：局部本地检查通过；整体投产验收未完成。

## 行为 / Behavior

群文件上传前复用 build_object_key/4 生成真实路径，复用 attach_pending 记录桶、路径、范围与上传人。登记失败不执行 PUT；大小、MIME 和上传前权限检查仍生效。服务端 PUT 限时 30 秒。

元数据事务沿用企业 / 工作区 / 群资格重验，群文件与 attachment 一起提交。attachment.path 使用上传时选定的完整路径，含 key_prefix；业务 file_id 保留原有生成规则，不再据它猜测对象路径。

PUT 超时、撤权或元数据回滚保留 pending，超龄后交给现有清理器处理。提交成功才销账；销账异常记录日志并保留成功结果。清理器的 NOT EXISTS attachment 条件保护已确认对象。存储删除失败保留登记，可重试。

English: Register the exact object key before PUT, commit both metadata rows atomically, and remove pending only after commit. Failed uploads remain visible to the existing cleanup job. Confirmed attachments remain protected if pending removal fails.

## 验证 / Verification

- 相关 EUnit：205 passed、0 failed、0 skipped；格式与差异空白检查通过。初次测试因遗漏编译 attach_pending_cleanup_tests 被取消；补编译后完整重跑通过，取消结果未作成功证据。
- 临时独立 PostgreSQL 18：10 passed。实际执行 DS / Repo / 元数据事务与待清理表 SQL，存储 PUT、签名与删除为 mock；没有连接业务库或实际 Garage。
- PostgreSQL 场景：文件读范围、正常上传、登记约束失败且零 PUT、PUT 超时、销账 trigger 报错但已确认文件不被清理、附件写失败、群文件写失败、企业资格在 PUT 中撤销、群资格在 PUT 中撤销、绑定索引迁移往返。
- 所有回滚场景都检查两张元数据表为空、pending 仍在；清理失败保留记录，重试成功销账。销账失败场景检查清理没有调用存储删除。
- 当前会话按调用链进行了本地代码复核，未启动独立审查代理；不把本地复核称作独立审查。

最小数据库复跑（仓库根；先编译受影响模块和测试并配置依赖 ebin 路径）：

```erlang
group_file_atomic_pg_tests:run(os:getenv("OA_TEST_SOCKET")).
```

OA_TEST_SOCKET 指向独立 PostgreSQL Unix socket；测试账号 departure_test、库 postgres、配置 epgsql_codec_rfc3339_bin。测试需要空数据库，并读取仓库真实迁移 157。调用方负责创建和停止临时数据库。

EUnit 模块：group_file_download_auth_tests、group_file_repo_tests、group_file_scope_tests、group_file_ds_tests、group_file_logic_tests、attachment_repo_tests、attach_logic_tests、enterprise_channel_attachment_tests、elib_oss_tests、attach_pending_cleanup_tests，另执行 workspace_archive_closure_tests 的附件补写、群文件上传和名称包含 file 的子域用例。

本轮源文件 SHA256：

| 文件 | SHA256 |
|---|---|
| src/ds/group_file_ds.erl | `48a2a6cc781a5300471d4ed7083470aa63dae606ab96e9f88f054b24c282b043` |
| src/lib/elib_oss.erl | `2ad86f2802dcf89aacce0145bef671b7ba08f519525075d613336f7e2443795e` |
| test/ds/group_file_ds_tests.erl | `a3c9173445823c100591268e77698665250430a2221d0911c8f53db035d07f49` |
| test/ds/group_file_scope_tests.erl | `902118a0d995e77c1b1797263cd54f9c16e3ed1226988a08a804b9c7b75bda16` |
| test/ds/workspace_archive_closure_tests.erl | `243a6abc207acaa70ff566385a05a4a101c6fc595e4e00dcf6fae7e3bfc53a61` |
| test/lib/elib_oss_tests.erl | `a3aef335e788e14ac0588c2bc64f60cbd16186d4c0faaecea1159c5278348561` |
| test/repo/group_file_atomic_pg_tests.erl | `640be08d3f972aabec468b23c8c5e8322cc888236489c4cd733b52f0bccd84a7` |

## 边界 / Limits

仅闭合群文件服务端上传登记；群相册与 AI 图片的 upload/3 调用保持原行为，它们的业务引用不同，不能统一登记后误删已使用图片。未证明全部文件存储链路闭合。

待清理使用既有定时任务与年龄门槛，不是失败后立即删除。没有新增迁移，没有执行业务数据库迁移、推送或部署。长事务与超龄清理并发的全链路行为未在本轮证明；真实 Garage、客户 OA、真机和总体客服 / 企业 / Internal API 的生产验收仍需继续完成。
