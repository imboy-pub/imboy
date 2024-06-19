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
    updated_at bigint DEFAULT 0,
    created_at bigint NOT NULL
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

COMMENT ON COLUMN public.group_member.updated_at IS '更新记录Unix时间戳毫秒单位';
COMMENT ON COLUMN public.group_member.created_at IS '创建记录Unix时间戳毫秒单位';
COMMENT ON COLUMN public.group_member.status IS '状态: -1 删除  0 禁用  1 启用 ';

-- index
CREATE UNIQUE INDEX uk_Gid_Uid ON public.group_member (group_id, user_id);
CREATE INDEX i_Uid_Gid_IsJoin ON public.group_member (user_id, group_id, is_join);
