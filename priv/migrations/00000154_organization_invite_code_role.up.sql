-- 00000154_organization_invite_code_role.up.sql
-- 组织邀请码增加 role 列：码上携带"凭码加入后的初始角色"（默认 member）。
-- 迁移契约：up=可重复执行，down=安全回滚（fail-closed 预检）。禁止
-- BEGIN/COMMIT——erlang_migrate 外层单事务包裹，文件内 COMMIT 会提前
-- 提交外层事务，其后的失败将无法回滚前置 DDL。
--
-- 设计决策：
--   * 角色由**发码方**决定（adm 平台面 / org Owner-Admin 生成码时指定），
--     码校验通过后加入编排（organization_join_orchestrator:join_tx）按码上
--     role 落 organization_member.role——扫码者不能自选角色，防止越权自提。
--   * 枚举仅 admin | member（默认 member）：owner 是唯一身份，只能经
--     owner_transfer 转让获得，不在码角色枚举内；校验与
--     organization_member_logic:valid_managed_role/1 同口径。
--   * 默认 'member' NOT NULL：存量码行自动补齐为普通成员语义，行为与
--     升级前完全一致（升级前 join_tx 硬编码 role=member）。
--   * 不加索引：role 不作为查询谓词，仅随行读取。

ALTER TABLE organization_invite_code
    ADD COLUMN IF NOT EXISTS role text DEFAULT 'member' NOT NULL;

ALTER TABLE organization_invite_code DROP CONSTRAINT IF EXISTS ck_organization_invite_code_role;
ALTER TABLE organization_invite_code ADD CONSTRAINT ck_organization_invite_code_role
    CHECK (role = ANY (ARRAY['admin'::text, 'member'::text]));

COMMENT ON COLUMN organization_invite_code.role IS
    '凭码加入后的初始角色: member 普通 | admin 管理员（默认 member；owner 仅能经转让获得，不入枚举）';
