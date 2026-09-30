# 群文件范围资格 / Group file scope access

日期 / Date: 2026-10-01。基线 / Base: `cdb7fe75e05ec88987b1d45f95325a17d75d865f`。本地验证，不是投产验收 / Local validation, not production acceptance.

## 修复 / Change

复用附件 Repo 的父级资格 SQL，群文件列表、搜索、分类统计、下载和上传预检重新查询有效群成员及企业 / 工作区资格，缓存的群成员身份不能单独授权。上传者删除文件也需有效资格，写入事务继续按 Org → Org member → Workspace → Workspace member 的现有锁序重验父级，并查询有效群成员。共享上传确认守卫也加入群成员复核，覆盖独立群文件与常规群附件。

移除没有当前用户参数的分类统计 DS 入口，Human Logic 改用两参数授权入口。软删除文件不能再走下载或删除入口。归档工作区保持获授权成员只读，写操作仍返回 980；Admin 的治理入口保留其原认证边界。

Reuse the attachment repository's parent-scope predicate for file operations. Cached group membership alone no longer grants access. Deletion requires current eligibility even for the uploader and rechecks eligibility in the write transaction. The shared upload-confirm guard now also rechecks active group membership. Category statistics require the caller identity. Deleted files cannot be downloaded. Authorized archived-workspace reads remain available; writes still return 980. Admin governance keeps its original authentication boundary.

## 验证 / Verification

- RED：六条失权入口断言在修复前全部失败；`/tmp/gz-group-file-scope-red.log`，退出码 1。
- EUnit：文件 DS / Logic、六条失权回归、附件 Repo / Logic、频道附件，以及归档测试中附件保存、群文件上传和两条文件读写案例，共 110 项；最终日志 `/tmp/gz-group-file-scope-unit.log`。
- 新合成 PG：`group_file_atomic_pg_tests:run(Socket)` 六项，包括真实元数据双表提交 / 回滚、OSS 阶段停用与退群、列表 / 搜索 / 分类 / 下载 / 删除拒绝、归档只读与删除状态；日志 `/tmp/gz-group-file-scope-pg.log`。
- 独立 PG 回归：`enterprise_group_attachment_pg_tests:run(Socket)` 六项，原群 / 频道父级授权以及双连接暂停 / 工作区移除锁等待；日志 `/tmp/gz-group-file-scope-parent-pg.log`。

两个 PG 都是新建 Unix socket 数据库；元数据案例使用应用配置的 RFC3339 codec。外部 OSS、预检缓存成员、分配 ID 和连接池定位为替身；执行真实 DS / Repo / SQL。完整迁移、触发器、HTTP 和设备验证未由此证明。

Both PG suites use fresh Unix-socket databases. Metadata tests use the configured RFC3339 codec. External storage, cached preflight membership, ID allocation and pool lookup are mocked; DS/repository SQL executes against PostgreSQL. These results do not establish full migrations, triggers, HTTP or device acceptance.

## 剩余限制 / Remaining limits

独立群成员 / 管理角色变更与写事务的完整并发撤销仍待验证；现有列表授权检查和列表查询是两个 statement，不承诺撤销过程的瞬时线性化。当前下载仍返回历史 `file_url`，未统一成短期受控链接；已签发链接的即时撤销、存储孤立对象清理、前端缓存失权处理仍未闭环。

Full concurrent group-member/role revocation is unproved. Authorization and list retrieval remain separate statements. Download still returns the stored legacy URL; short-lived authorized links, issued-link revocation, orphan storage cleanup and client cache revocation remain unfinished.
