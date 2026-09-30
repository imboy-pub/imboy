# 企业群附件即时撤权 / Enterprise group attachment revocation

日期 / Date: 2026-10-01。状态 / Status: LOCAL_FOCUSED_PASS，整体交付未完成。

## 问题与修复 / Problem and change

暂停企业成员仅修改组织资格，保留工作区和群关系。旧群附件 SQL 只检查群成员及 generation，因此暂停后仍允许签发新下载链接。
Suspension retains Workspace and Group relationships. The previous attachment predicate checked only Group membership and generation, allowing new download authorization after suspension.

现有 `attachment_repo:group_access_sql/1` 在同一语句内重验：个人群保持原模型；个人工作区要求有效工作区成员；企业工作区要求有效企业成员、有效企业和工作区成员资格，或企业 Owner/Admin 治理资格。企业治理资格不替代原群成员与 generation 条件。工作区归档保留只读；企业归档拒绝读取。企业资料的非企业成员访问按设计合同 FILE-01 拒绝，包括仍有旧下级关系的用户。
The same SQL statement now validates parent scope eligibility. Personal Groups retain existing semantics; personal Workspaces require active membership. Enterprise Workspaces require active Organization membership and an active Organization, plus Workspace membership or Organization Owner/Admin governance. Existing Group membership and generation checks remain mandatory. Archived Workspaces remain readable; archived Organizations are denied. Enterprise nonmembers are denied under FILE-01 even if stale child relationships remain.

本次不删除附件、不改上传人、不改归属、不迁移历史记录。上传、confirm、频道附件和已签发链接的实时撤销尚待核对；此查询只影响新的授权判定，不能撤销已经签发的对象存储 URL。
No attachments, uploader identities, ownership or historical records are changed. Upload, confirmation, Channel attachments and revocation of previously signed URLs remain outside this focused result. This query affects new authorization decisions, not already issued storage URLs.

## 证据 / Evidence

- 修复前：新 PostgreSQL 18 合成库中的暂停成员读取断言失败，实际 allowed=true。日志 `/tmp/gz-group-attachment-red.log`。
- 修复后：`enterprise_group_attachment_pg_tests:run(SocketPath)` 通过一个数据库场景，覆盖聊天附件、独立群文件、暂停 / 恢复 / 移除 / 无企业成员、工作区失权、治理角色、企业归档、工作区只读、个人域、其他企业、generation 历史边界与文件软删除。最后校验五条资料记录仍在。日志 `/tmp/gz-group-attachment-green.log`。
- `eunit:test([attachment_repo_tests,attach_logic_tests],[verbose])`：55/55 PASS，退出码 0。日志 `/tmp/gz-group-attachment-unit.log`。
- Test modules and affected sources compile; erlfmt checked through normal commit hooks. Existing meck_helper compile warnings were observed, not introduced by this change.

测试只连接任务新建的本地 Unix socket 数据库；使用生产授权 SQL，但最小合成表不证明完整迁移、HTTP JWT、存储签名、并发、真机和生产环境已通过。
The database is freshly initialized on a task-local Unix socket. The test executes production authorization SQL against minimal synthetic tables; it does not establish full migration, HTTP JWT, storage signing, concurrency, device or production acceptance.
