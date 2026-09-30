# 群文件上传事务验证 / Group file upload transaction verification

日期 / Date: 2026-10-01。范围 / Scope: 本地候选，非投产验收 / Local candidate, not production acceptance.

## 问题与修复 / Problem and change

原流程先提交群文件，再用独立事务补写附件授权，失败被吞掉；附件 Repo 本身也忽略写入错误。现在复用既有事务和权限守卫，群文件与附件授权一起提交，任一写入失败都回滚并返回失败。上传期间企业成员失权，最终落库守卫拒绝写入。

Previously the group-file row committed before attachment authorization, whose failure was swallowed. The attachment repository also ignored write errors. Both records now share the existing transaction; write failure rolls both back and returns an error. The final scope guard rejects membership revoked during storage upload.

## 证据 / Evidence

- RED: `attachment_repo_tests` 新增写入失败断言在修复前实际得到 `ok`；日志 `/tmp/gz-group-file-atomic-red.log`。
- GREEN: `attachment_repo_tests`、`attach_logic_tests`、`enterprise_channel_attachment_tests`、`group_file_ds_tests` 与 `workspace_archive_closure_tests` 的 attachment save / group file upload 两个 focused generators，共 86 项通过，退出码 0；日志 `/tmp/gz-group-file-atomic-unit.log`。
- 真实 PostgreSQL: `group_file_atomic_pg_tests:run(Socket)` 四项通过，退出码 0；日志 `/tmp/gz-group-file-atomic-pg.log`。成功同时写入两表并核对授权关联；附件写失败、群文件写失败、存储上传期间企业成员停用均无数据库残留。

PG 使用新建合成数据库、真实 DS / Repo / 事务及元数据 INSERT，按应用配置加载 RFC3339 timestamp codec。替身限于存储上传、预检群成员、TSID 分配和连接池定位；表结构覆盖相关列，不是全部迁移及触发器。

PG uses a fresh synthetic database and real DS/repository transactions and metadata inserts, with the configured RFC3339 timestamp codec. Storage, preflight group membership, TSID allocation and pool lookup are mocked. The schema covers relevant columns, not the full migration and trigger set.

## 仍待闭环 / Remaining

数据库回滚不会删除已经上传的存储对象，孤立对象清理待补；独立群文件列表、读取、删除的完整父级权限仍需继续验证。群成员和频道订阅的并发撤销、旧静态链接、已签发 URL、HTTP / 真机 / 真实存储及全迁移验证不在上述通过结论内。

Database rollback does not delete an uploaded storage object; orphan cleanup remains. Independent group-file list/read/delete authorization, concurrent group or channel entitlement revocation, legacy links, issued URLs, HTTP/device/storage journeys and full migrations remain unproved.
