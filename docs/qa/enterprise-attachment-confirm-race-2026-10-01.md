# 企业附件确认撤权竞态 / Enterprise attachment confirmation revocation race

日期 / Date: 2026-10-01。状态 / Status: LOCAL_FOCUSED_PASS，整体交付尚未完成。

## 修复 / Change

群/频道附件 confirm 原来仅在外部 HEAD 前检查上传权。HEAD 期间撤销企业资格后，元数据事务只检查归档，仍可能成功保存。本次最终事务调用 `attachment_ds:ensure_upload_scope_tx/3`，先解析真实资源归属，以 Org 行 → Org 成员 → Workspace → 工作区成员的顺序锁定事实，复用已有组织成员共享锁和工作区归档守卫，再用同一连接重验范围授权，之后才保存附件。
Previously, upload eligibility was checked before external HEAD, while the final transaction checked only archival. The final transaction now resolves authoritative ownership, locks Organization, Organization membership, Workspace and Workspace membership in that order, reuses existing shared membership locks and archive guards, and rechecks scope authorization on the write connection before attachment persistence.

父级拒绝抛 abort_tx，事务回滚，不写附件也不移除 pending 记录；已上传但未确认的对象继续由既有清理流程负责。个人附件不经过企业守卫，原有元数据与密文套件校验保留。
Denial aborts the transaction before attachment persistence or pending-record removal. Unconfirmed objects remain subject to existing cleanup. Personal attachment behavior, metadata verification and ciphertext-suite validation remain intact.

## 验证 / Validation

- 修复前：群、频道两条新回归均得到成功 confirm，本应拒绝；日志 `/tmp/gz-attachment-confirm-red.log`。
- `eunit:test([attachment_repo_tests,attach_logic_tests,enterprise_channel_attachment_tests],[verbose])`：60/60 PASS，退出码 0。HEAD 后资格拒绝时 save 与 pending_remove 调用数均为 0；日志 `/tmp/gz-attachment-confirm-unit.log`。
- 独立 PostgreSQL 18 合成库：6/6 场景 PASS，含已有群/频道 SQL 回归，以及 group/channel × suspend/remove_workspace 四个双连接锁竞争场景。通过 `pg_blocking_pids` 证明撤权 UPDATE 被上传事务持有的锁阻塞；上传提交后撤权完成，随后新上传事务被拒绝。使用实际 DS/Repo/SQL，仅连接池定位到合成连接；日志 `/tmp/gz-attachment-confirm-pg.log`。
- 格式、编译及正常提交 hooks 完成。没有连接生产、真实存储或外部 OA。

## 证明边界 / Limits

数据库最小表的写入 oracle 是合成 attachment 行，未执行完整生产 attachment upsert/迁移链；单元回归另验证实际 confirm 对新守卫的调用与零保存。双连接撤权通过真实 membership UPDATE，未覆盖全部离岗交接业务及多工作区锁竞争。不宣称全部事务已经线性一致。
The database oracle inserts a synthetic attachment row, not the full production upsert/migration chain. Unit regression separately verifies actual confirmation wiring and zero-save behavior. Concurrent revocation uses actual membership UPDATE statements, not all offboarding workflows or multi-Workspace contention.

仍需核对群成员单独移除、频道订阅/管理角色变更、群上传预检、独立群文件入口、历史静态 URL 和已签发对象存储链接撤销；本轮不通过删除企业资料来撤权。真实 HTTP/JWT、存储、设备及生产验收仍未完成。
Group-only removal, Channel entitlement changes, Group upload preflight, independent Group-file entry points, historical static URLs and existing signed URLs remain to be addressed. Enterprise data is not deleted to revoke access. HTTP/JWT, storage, device and production acceptance remain incomplete.
