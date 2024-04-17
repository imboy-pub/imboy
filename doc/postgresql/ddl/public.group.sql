        -- Table: public.group

-- DROP TABLE IF EXISTS public."group";


-- 导入数据后，需要更新自增长ID
-- select setval('"group_id_seq"', (select max(id) from public."group"));
--  select nextval('group_id_seq');

CREATE TABLE IF NOT EXISTS public."group"
(
    id BIGSERIAL PRIMARY KEY,
    type smallint DEFAULT 1,
    join_limit smallint DEFAULT 2,
    content_limit smallint DEFAULT 2,
    user_id_sum bigint NOT NULL DEFAULT 0, -- 主要用于添加群聊的时候排重
    owner_uid bigint NOT NULL,
    creator_uid bigint NOT NULL,
    member_max int NOT NULL DEFAULT 1000,
    member_count int NOT NULL DEFAULT 1,
    introduction varchar(2000) NOT NULL DEFAULT '',
    avatar varchar(320) NOT NULL DEFAULT '',
    title varchar(200) NOT  NULL DEFAULT '',
    status smallint NOT NULL DEFAULT 1,
    updated_at bigint DEFAULT 0,
    created_at bigint NOT NULL
)

TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.group OWNER to imboy_user;

COMMENT ON TABLE public.group IS '群组表';

COMMENT ON COLUMN public.group.id IS '主键 自增长ID';

COMMENT ON COLUMN public.group.type IS '类型: 1 公开群组  2 私有群组';

COMMENT ON COLUMN public.group.join_limit IS '加入限制: 1 不需审核  2 需要审核  3 只允许邀请加入';

COMMENT ON COLUMN public.group.content_limit IS '内部发布限制: 1 圈内不需审核  2 圈内需要审核  3 圈外需要审核';
COMMENT ON COLUMN public.group.owner_uid IS '群组拥有者ID';
COMMENT ON COLUMN public.group.creator_uid IS '群组创建者ID';
COMMENT ON COLUMN public.group.member_max IS '允许最大成员数量';
COMMENT ON COLUMN public.group.member_count IS '成员数量';
COMMENT ON COLUMN public.group.introduction IS '简介';
COMMENT ON COLUMN public.group.avatar IS '群组头像';
COMMENT ON COLUMN public.group.title IS '群组名称';

COMMENT ON COLUMN public.group.updated_at IS '更新记录Unix时间戳毫秒单位';
COMMENT ON COLUMN public.group.created_at IS '创建记录Unix时间戳毫秒单位';
COMMENT ON COLUMN public.group.status IS '状态: -1 删除  0 禁用  1 启用';

-- index

CREATE INDEX i_Status_OwnerUid_Type ON public.group(status,owner_uid,type);
CREATE INDEX i_CreatorId_MemberIdSum ON public.group(creator_uid, user_id_sum);
