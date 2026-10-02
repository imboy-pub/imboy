# 企业历史文件迁移与恢复 / Enterprise legacy file migration recovery

本方案针对迁移 `00000108_group_attachment_anchor`。本地合成 PostgreSQL 的原始 up/down 检查已经通过；它不表示现有库、实际旧对象或生产恢复已验证。对任何现有数据库或对象存储执行操作前，必须取得用户针对目标环境的授权；生产数据与备份不得放入工作区或仓库。

## 归属判定 / Ownership

迁移只在四项完全一致时绑定：attachment 的 group scope、scope_ref 对应 group_file.group_id、上传者相同、path 精确等于 file_id/文件名。不会从当前登录企业、默认工作区或相似文件名推断归属。

无法可靠匹配的记录保留原 metadata，不补造 group_file、组织或工作区关联。不匹配的历史 group 附件遵循迁移既有 anchor_conv_seq=1 兼容规则，仍受真实成员 generation 与授权网关限制；这不是企业群资料归属证明。缺失对象、旧裸URL或无法匹配的记录单独列为待人工核定，不能改成公开读取，也不能批量重写对象。

迁移后只读核验：以下计数必须为0；若不为0，停止依赖功能，不执行猜测修复。

```sql
SELECT count(*) AS invalid_group_file_binding
FROM public.attachment a
WHERE a.group_file_id IS NOT NULL
  AND NOT EXISTS (
    SELECT 1 FROM public.group_file gf
    WHERE gf.id = a.group_file_id
      AND a.scope = 'group'
      AND a.scope_ref = gf.group_id::text
      AND a.creator_user_id = gf.uploader_id
      AND a.path = gf.file_id || '/' || regexp_replace(gf.file_name, '^.*/', '')
  );
```

## 上线前恢复演练 / Recovery rehearsal

1. 明确数据库、对象存储、代码提交与维护窗口，停止相关写入。记录迁移前完整DB快照、group_file/attachment行数和逐行metadata摘要；备份路径、时间及SHA256保存在仓库外的受控位置。仅schema备份不能恢复数据。
2. 复用 `scripts/backup_pg.sh --full` 与既有对象备份流程；先审查其 metrics/offsite 设置、保留清理和目标，不能把这些会执行外向动作的脚本当成纯读取。`pg_restore --list`只能证明备份可解析，不代表可恢复。
3. 在全新隔离数据库恢复完整备份，复用 `scripts/restore_pg.sh` 的 timescaledb pre/post_restore流程，显式指定独立目标。脚本会DROP目标库，不能指向已有业务库；恢复与 TimescaleDB 收尾非零时脚本必须返回失败。返回0与表数量抽样仍不足以证明恢复完整，任何告警必须调查，还须核对实际业务记录和对象字节。
4. 比较迁移版本及相关表逐行metadata，而不只比较总行数；验证对象仍存在、大小/内容摘要一致，数据库定位与实际对象匹配。演练企业成员可读、跨企业及已退出成员不可读、既有授权票据重新校验，以及历史聊天generation边界。
5. 在恢复副本执行完整迁移链，再核验本页不变量与真实历史文件下载。保存冻结代码/迁移SHA、命令退出码、DB和对象oracle、授权拒绝结果。所有必需检查通过才形成该环境的恢复证据；当前仅有合成迁移108证据，真实历史下载与这套恢复演练仍待执行。

## 失败后的选择 / Failure recovery

- 尚无迁移后业务写入：优先在隔离副本验证代码回退与完整快照恢复，再由用户批准现有环境的恢复目标。迁移108 down会删除三个绑定列；它只用于已证明没有新写入的回退演练，不作为无损生产回退。
- 已有新写入：停止相关写入，先保留当前完整DB和对象快照及增量记录。直接down或恢复旧快照会丢失新增锚点/业务数据，禁止自动执行；选择经演练的向前修复，或人工确定增量保全后再批准恢复。不得因为全量备份存在就假定可以无损恢复。
- 归属或对象无法证明：保留记录与对象，保持授权失败，明确列出人工核定项。不要执行 `scripts/migrate_legacy_attachments.erl` 来“修复归属”：它涉及旧对象加密/覆盖，不属于本方案。

恢复后重新验证群资料列表、授权下载、删除/归档拒绝、成员退出及历史聊天边界。记录LOCAL、DEVICE、EXTERNAL与PRODUCTION结果；未运行的恢复或外部测试不算PASS。

English: Exact metadata matching is required; unknown ownership stays unresolved. A synthetic down/up success is not a lossless production rollback. Full database/object snapshots must be restored and checked in an isolated target, with explicit approval before touching existing systems. Preserve post-migration writes and keep unidentified files private.
