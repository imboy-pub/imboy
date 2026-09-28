-- 00000155_organization_status_pending.up.sql
-- organization.status 枚举扩展 pending | rejected：APP 端注册企业需运营
-- 后台审核（通过前禁止邀请新成员 / 生成邀请码；驳回为终态）。
-- 迁移契约：up=可重复执行，down=安全回滚（fail-closed 预检）。禁止
-- BEGIN/COMMIT——erlang_migrate 外层单事务包裹。
--
-- 设计决策：
--   * 审核状态直接落在 organization.status（不另建 request 表）：
--     门禁单点（各写入口按 status='active' 裁决，与 C16 archived 禁新写
--     同一插桩位置），平台列表/搜索天然可见待审核组织（运营要审核它们）。
--   * 状态机：
--       APP 创建 → pending
--       pending --approve--> active（唯一放行出口）
--       pending --reject--> rejected（终态；组织名可被再次注册）
--       存量组织 / adm 平台创建的组织 → active（视为已审核，行为不变）
--   * pending/rejected 不允许 archive/restore（lifecycle 堵洞：restore
--     不得把 pending 直接推成 active 绕过审核）；出口只有审核操作。
--   * 存量行全为 active/archived，CHECK 重建不影响现有数据。

ALTER TABLE organization DROP CONSTRAINT IF EXISTS ck_organization_status;
ALTER TABLE organization ADD CONSTRAINT ck_organization_status
    CHECK (status = ANY (ARRAY[
        'active'::text, 'archived'::text, 'pending'::text, 'rejected'::text
    ]));

COMMENT ON COLUMN organization.status IS
    '生命周期: active 正常 | archived 归档 | pending 待审核(APP注册，禁止邀新/出码) | rejected 审核驳回(终态)';
