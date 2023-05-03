-- Table: public.user_friend

-- DROP TABLE IF EXISTS public."user_friend";

CREATE TABLE IF NOT EXISTS public."user_friend"
(
    id BIGSERIAL PRIMARY KEY,
    from_user_id bigint NOT NULL,
    to_user_id bigint NOT NULL,
    category_id bigint DEFAULT 0,
    remark varchar(80) DEFAULT '',
    status smallint NOT NULL DEFAULT 1,
    updated_at bigint DEFAULT 0,
    created_at bigint NOT NULL,
    setting json
)

TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.user_friend OWNER to imboy_user;


COMMENT ON TABLE public.user_friend IS '用户登录设备表';

COMMENT ON COLUMN public.user_friend.id IS '主键 自增长ID';

COMMENT ON COLUMN public.user_friend.from_user_id IS '发起人';

COMMENT ON COLUMN public.user_friend.to_user_id IS '接受人';

COMMENT ON COLUMN public.user_friend.category_id IS '用户分组ID friend_category主键';
COMMENT ON COLUMN public.user_friend.remark IS '备注名';

COMMENT ON COLUMN public.user_friend.updated_at IS '更新记录Unix时间戳毫秒单位';
COMMENT ON COLUMN public.user_friend.created_at IS '创建记录Unix时间戳毫秒单位';
COMMENT ON COLUMN public.user_friend.status IS '状态: -1 删除  0 禁用  1 启用';


-- index

CREATE UNIQUE INDEX uk_FromUID_ToUID ON public.user_friend (from_user_id, to_user_id);
CREATE INDEX i_Status_FromUid_Cid ON public.user_friend (status, from_user_id, category_id);
