-- Table: public.user_denylist

-- DROP TABLE IF EXISTS public."user_denylist";

CREATE TABLE IF NOT EXISTS public."user_denylist"
(
    id BIGSERIAL PRIMARY KEY,
    user_id bigint NOT NULL,
    denied_user_id bigint NOT NULL DEFAULT '0',
    created_at bigint DEFAULT 0,
    CONSTRAINT uk_UserId_DeniedUserId UNIQUE  (user_id,denied_user_id)
)

TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.user_denylist OWNER to imboy_user;

COMMENT ON TABLE public.user_denylist IS '用户的拒绝聊天名单';

COMMENT ON COLUMN public.user_denylist.id IS '主键 自增长ID';

COMMENT ON COLUMN public.user_denylist.user_id IS '归属用户ID';

COMMENT ON COLUMN public.user_denylist.denied_user_id IS '被列入名单的用户ID';
COMMENT ON COLUMN public.user_denylist.created_at IS '创建记录UTC时间';
