-- Table: public.group_member

-- DROP TABLE IF EXISTS public."group_member";

-- 导入数据后，需要更新自增长ID
-- select setval('"group_member_id_seq"', (select max(id) from public."group_member"));

CREATE TABLE IF NOT EXISTS public."group_member"
(
    id BIGSERIAL PRIMARY KEY,
    group_id bigint NOT NULL,
    user_id bigint NOT NULL,
    alias varchar(80) DEFAULT '',
    description varchar(80) DEFAULT '',
    role smallint DEFAULT 0,
    status smallint NOT NULL DEFAULT 1,
    updated_at bigint DEFAULT 0,
    created_at bigint NOT NULL
)

TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.group_member OWNER to imboy_user;

COMMENT ON TABLE public.group_member IS '群组成员表（删除即表示成员被踢出群组，updated_at为被踢出的时间，同时有踢出记录）';

COMMENT ON COLUMN public.group_member.id IS '主键 自增长ID';

COMMENT ON COLUMN public.group_member.group_id IS '群组ID';

COMMENT ON COLUMN public.group_member.user_id IS '群组成员用户ID';

COMMENT ON COLUMN public.group_member.alias IS '圈内别名';
COMMENT ON COLUMN public.group_member.description IS '成员描述';
COMMENT ON COLUMN public.group_member.role IS '角色: 1 成员  2 嘉宾  3  管理员 4 群主';

COMMENT ON COLUMN public.group_member.updated_at IS '更新记录Unix时间戳毫秒单位';
COMMENT ON COLUMN public.group_member.created_at IS '创建记录Unix时间戳毫秒单位';
COMMENT ON COLUMN public.group_member.status IS '状态: -1 删除  0 禁用  1 启用';

-- index
CREATE UNIQUE INDEX i_Status_Gid_Uid ON public.group_member (status, group_id, user_id);
