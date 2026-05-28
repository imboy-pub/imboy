-- Table: public.group_member

-- DROP TABLE IF EXISTS public."group_member";

-- 导入数据后，需要更新自增长ID
-- select setval('"group_member_id_seq"', (select max(id) from public."group_member"));

CREATE TABLE IF NOT EXISTS public."group_member"
(
    id BIGSERIAL PRIMARY KEY,
    group_id bigint NOT NULL,
    user_id bigint NOT NULL,
    invite_code varchar(40) DEFAULT '',
    alias varchar(120) DEFAULT '',
    description varchar(400) DEFAULT '',
    role smallint DEFAULT 0,
    is_join smallint DEFAULT 0,
    join_mode varchar(120) DEFAULT '', -- 进群方式 :  invite_[uid]_[nickname]
    status smallint NOT NULL DEFAULT 1,
    updated_at timestamptz DEFAULT CURRENT_TIMESTAMP NULL,
    created_at timestamptz DEFAULT CURRENT_TIMESTAMP NOT NULL
)

TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.group_member OWNER to imboy_user;

COMMENT ON TABLE public.group_member IS '群组成员表';

COMMENT ON COLUMN public.group_member.join_mode IS '进群方式 :  invite_[uid]_[nickname] <a>leeyi</a>邀请进群  scan_qr_code 扫描二维码加入 face2face_join 面对面建群';
COMMENT ON COLUMN public.group_member.id IS '主键 自增长ID';

COMMENT ON COLUMN public.group_member.group_id IS '群组ID';

COMMENT ON COLUMN public.group_member.user_id IS '群组成员用户ID';
COMMENT ON COLUMN public.group_member.invite_code IS '入群邀请码';

COMMENT ON COLUMN public.group_member.alias IS '群内别名';
COMMENT ON COLUMN public.group_member.description IS '群内描述';
COMMENT ON COLUMN public.group_member.role IS '角色: 1 成员  2 嘉宾  3  管理员 4 群主';
COMMENT ON COLUMN public.group_member.is_join IS '是否加入的群： 1 是 0 否 （0 是群创建者或者拥有者 1 是 成员 嘉宾 管理员等）';

COMMENT ON COLUMN public.group_member.updated_at IS '最后更新记录时间 2025-02-21 08:33:16.268288+08:00';
COMMENT ON COLUMN public.group_member.created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';
COMMENT ON COLUMN public.group_member.status IS '状态: -1 删除  0 禁用  1 启用 ';

-- index
CREATE UNIQUE INDEX IF NOT EXISTS uk_Gid_Uid ON public.group_member (group_id, user_id);
CREATE INDEX IF NOT EXISTS i_Uid_Gid_IsJoin ON public.group_member (user_id, group_id, is_join);
CREATE INDEX IF NOT EXISTS i_Uid_Status ON public.group_member (user_id, status);

-- remark column (originally added in migration 000414)
-- 为 group_member 表添加 remark 字段（群备注，仅自己可见）
-- 与 alias（群内别名）区别：alias 是用户在群内的显示名称，remark 是用户对该群的个人备注
ALTER TABLE public.group_member
    ADD COLUMN IF NOT EXISTS remark varchar(200) DEFAULT '';

COMMENT ON COLUMN public.group_member.remark IS '群备注，仅该成员自己可见';

-- role enhancement indexes (000412)
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

-- mute column and indexes (000413)
ALTER TABLE group_member ADD COLUMN IF NOT EXISTS mute_until TIMESTAMPTZ DEFAULT NULL;
CREATE INDEX IF NOT EXISTS idx_group_member_mute ON group_member(group_id, user_id) WHERE mute_until IS NOT NULL;
