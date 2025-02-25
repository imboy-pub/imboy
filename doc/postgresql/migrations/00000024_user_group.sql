-- Table: public.user_group

-- DROP TABLE IF EXISTS public."user_group";

-- 导入数据后，需要更新自增长ID
-- select setval('"user_group_id_seq"', (select max(id) from public."user_group"));

CREATE TABLE IF NOT EXISTS public."user_group"
(
    id BIGSERIAL PRIMARY KEY,
    user_id bigint NOT NULL,
    group_id bigint NOT NULL,
    remark varchar(400) DEFAULT '',
    setting text NOT NULL,
    status smallint NOT NULL DEFAULT 1,
    updated_at timestamptz DEFAULT CURRENT_TIMESTAMP NULL,
    created_at timestamptz DEFAULT CURRENT_TIMESTAMP NOT NULL
)

TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.user_group OWNER to imboy_user;

COMMENT ON TABLE public.user_group IS '用户保存到通讯录的群';

COMMENT ON COLUMN public.user_group.id IS '主键 自增长ID';

COMMENT ON COLUMN public.user_group.group_id IS '群组ID';

COMMENT ON COLUMN public.user_group.user_id IS '用户ID';
COMMENT ON COLUMN public.user_group.remark IS '群聊的备注仅自己可见';
COMMENT ON COLUMN public.user_group.setting IS '用户对群的一些配置';

COMMENT ON COLUMN public.user_group.updated_at IS '最后更新记录时间 2025-02-21 08:33:16.268288+08:00';
COMMENT ON COLUMN public.user_group.created_at IS '创建记录时间 2025-02-21 08:33:16.268288+08:00';
COMMENT ON COLUMN public.user_group.status IS '状态: -1 删除  0 禁用  1 启用 ';

-- index
CREATE UNIQUE INDEX uk_ug_Uid_Gid ON public.user_group (user_id, group_id);
