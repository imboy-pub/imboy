-- Table: public.group_member_log

-- DROP TABLE IF EXISTS public."group_member_log";


-- 导入数据后，需要更新自增长ID
-- select setval('"group_member_log_id_seq"', (select max(id) from public."group_member_log"));

CREATE TABLE IF NOT EXISTS public."group_member_log"
(
    id BIGSERIAL PRIMARY KEY,
    type smallint NOT NULL,
    option_uid bigint NOT NULL DEFAULT 0,
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

ALTER TABLE IF EXISTS public.group_member_log OWNER to imboy_user;

COMMENT ON TABLE public.group_member_log IS '群组成员表（删除即表示成员被踢出群组，updated_at为被踢出的时间，同时有踢出记录）';

COMMENT ON COLUMN public.group_member_log.id IS '主键 自增长ID';

COMMENT ON COLUMN public.group_member_log.type IS '日志类型: 1 转让  2 被退出  3 主动退出';
COMMENT ON COLUMN public.group_member_log.option_uid IS '操作者用户ID（0 表示主动退出）';
COMMENT ON COLUMN public.group_member_log.group_id IS '群组ID';

COMMENT ON COLUMN public.group_member_log.user_id IS '群组成员用户ID';

COMMENT ON COLUMN public.group_member_log.alias IS '圈内别名';
COMMENT ON COLUMN public.group_member_log.description IS '成员描述';
COMMENT ON COLUMN public.group_member_log.role IS '角色: 1 成员  2 嘉宾  3  管理员 4 群主';

COMMENT ON COLUMN public.group_member_log.updated_at IS '更新记录Unix时间戳毫秒单位';
COMMENT ON COLUMN public.group_member_log.created_at IS '创建记录Unix时间戳毫秒单位';
COMMENT ON COLUMN public.group_member_log.status IS '状态: -1 删除  0 禁用  1 启用';

-- index
