-- Table: public.group

-- DROP TABLE IF EXISTS public."group";

CREATE TABLE IF NOT EXISTS public."group"
(
    id BIGSERIAL PRIMARY KEY,
    type smallint DEFAULT 1,
    join_limit smallint DEFAULT 2,
    content_limit smallint DEFAULT 2,
    owner_uid bigint NOT NULL,
    creater_uid bigint NOT NULL,
    member_max bigint NOT NULL DEFAULT 300,
    member_count bigint NOT NULL DEFAULT 1,
    notification varchar(800) NOT NULL DEFAULT '',
    introduction varchar(2000) NOT NULL DEFAULT '',
    avatar varchar(200) NOT NULL DEFAULT '',
    groupname varchar(80) NOT NULL DEFAULT '',
    status smallint NOT NULL DEFAULT 1,
    updated_at bigint DEFAULT 0,
    created_at bigint NOT NULL,
    CONSTRAINT uk_Groupname UNIQUE  (groupname)
)

TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.group OWNER to imboy_user;

COMMENT ON TABLE public.group IS '用户登录设备表';

COMMENT ON COLUMN public.group.id IS '主键 自增长ID';

COMMENT ON COLUMN public.group.type IS '类型: 1 公开群组  2 私有群组';

COMMENT ON COLUMN public.group.join_limit IS '加入限制: 1 不需审核  2 需要审核  3 只允许邀请加入';

COMMENT ON COLUMN public.group.content_limit IS '内部发布限制: 1 圈内不需审核  2 圈内需要审核  3 圈外需要审核';
COMMENT ON COLUMN public.group.owner_uid IS '群组拥有者ID';
COMMENT ON COLUMN public.group.creater_uid IS '群组创建者ID';
COMMENT ON COLUMN public.group.member_max IS '允许最大成员数量';
COMMENT ON COLUMN public.group.member_count IS '成员数量';
COMMENT ON COLUMN public.group.notification IS '公告';
COMMENT ON COLUMN public.group.introduction IS '简介';
COMMENT ON COLUMN public.group.avatar IS '群组头像';
COMMENT ON COLUMN public.group.groupname IS '群组名称';

COMMENT ON COLUMN public.group.updated_at IS '更新记录Unix时间戳毫秒单位';
COMMENT ON COLUMN public.group.created_at IS '创建记录Unix时间戳毫秒单位';
COMMENT ON COLUMN public.group.status IS '状态: -1 删除  0 禁用  1 启用';

-- index
