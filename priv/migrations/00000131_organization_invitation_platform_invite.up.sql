-- 迁移 00000131: organization_invitation.invited_by 放开为可空（ORG-ADM-ORG-API）。
--
-- 背景：Platform Admin 治理面 POST /api/adm/organizations/:organization_id/invitations
-- 由平台管理员（adm_user.id，不是租户 user）创建邀请。00000128 冻结的
-- invited_by bigint NOT NULL 只表达租户 inviter 事实；平台侧硬边界是
-- 「不映射 Platform Admin 为租户身份、绝不写任何指向 user 表的 actor 列」
-- （organization_admin_logic 头注释），invited_by = NULL 是唯一如实形态，
-- NOT NULL 约束会把平台创建路径变成 500（23502）。
--
-- 语义：invited_by IS NULL = 「平台治理通道创建，无租户 inviter」；
-- 租户面（organization_invitation_app:create_tx）仍恒写真实 inviter，行为不变。
-- 对齐仓内既有惯例：workspace_member.invited_by / project_member.invited_by
-- 均为可空（00000076 / 00000081，NULL = 非租户邀请人路径）。
-- FK 保持 00000128 的 ON DELETE CASCADE 不变（最小改动，不触删除语义与
-- agent-domain 删除 preflight 的裁决面）。
--
-- 迁移契约：up=可重复执行（对已可空列 DROP NOT NULL 幂等），down=安全回滚。
-- 禁止 BEGIN/COMMIT——erlang_migrate 外层单事务包裹。

SET lock_timeout = '5s';
SET statement_timeout = '15min';

ALTER TABLE organization_invitation
    ALTER COLUMN invited_by DROP NOT NULL;
