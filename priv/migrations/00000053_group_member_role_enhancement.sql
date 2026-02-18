-- ===================================================================
-- 群成员角色增强迁移
-- ===================================================================
--
-- 角色定义更新：
-- 0 = 非成员/未定义
-- 1 = 普通成员
-- 2 = 嘉宾
-- 3 = 管理员
-- 4 = 群主
-- 5 = 副群主 (新增)
--
-- 权限矩阵：
-- | 角色      | 踢人 | 禁言 | 公告 | 解散群 | 转让群 | 修改群信息 |
-- |-----------|------|------|------|--------|--------|------------|
-- | 群主 (4)  | ✓    | ✓    | ✓    | ✓      | ✓      | ✓          |
-- | 副群主 (5)| ✓    | ✓    | ✓    | ✗      | ✗      | ✓          |
-- | 管理员 (3)| ✓    | ✓    | ✓    | ✗      | ✗      | ✗          |
-- | 嘉宾 (2)  | ✗    | ✗    | ✗    | ✗      | ✗      | ✗          |
-- | 成员 (1)  | ✗    | ✗    | ✗    | ✗      | ✗      | ✗          |
--
-- ===================================================================

-- 1. 更新 role 字段注释
COMMENT ON COLUMN public.group_member.role IS '角色: 0 未定义 1 普通成员 2 嘉宾 3 管理员 4 群主 5 副群主';

-- 2. 确保角色字段有索引（用于权限查询）
CREATE INDEX IF NOT EXISTS idx_group_member_role ON public.group_member(group_id, role);

-- 3. 添加检查约束确保角色值在有效范围内
ALTER TABLE public.group_member DROP CONSTRAINT IF EXISTS chk_group_member_role;
ALTER TABLE public.group_member ADD CONSTRAINT chk_group_member_role
  CHECK (role >= 0 AND role <= 5);

-- 4. 创建视图用于查询群管理员（包括副群主）
CREATE OR REPLACE VIEW v_group_admins AS
SELECT
    gm.group_id,
    gm.user_id,
    gm.role,
    u.nickname,
    u.avatar,
    gm.created_at
FROM public.group_member gm
LEFT JOIN public.user u ON u.id = gm.user_id
WHERE gm.role IN (3, 4, 5) AND gm.status = 1
ORDER BY gm.group_id, gm.role DESC, gm.created_at ASC;

COMMENT ON VIEW v_group_admins IS '群管理员视图（包括管理员、群主、副群主）';

-- 5. 创建视图用于查询高级管理员（群主和副群主）
CREATE OR REPLACE VIEW v_group_senior_admins AS
SELECT
    gm.group_id,
    gm.user_id,
    gm.role,
    u.nickname,
    u.avatar,
    gm.created_at
FROM public.group_member gm
LEFT JOIN public.user u ON u.id = gm.user_id
WHERE gm.role IN (4, 5) AND gm.status = 1
ORDER BY gm.group_id, gm.role DESC, gm.created_at ASC;

COMMENT ON VIEW v_group_senior_admins IS '群高级管理员视图（仅群主和副群主）';
