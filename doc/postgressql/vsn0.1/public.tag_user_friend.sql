-- Table: public.tag_user_friend

-- DROP TABLE IF EXISTS public."tag_user_friend";

CREATE TABLE IF NOT EXISTS public."tag_user_friend"
(
    id BIGSERIAL PRIMARY KEY,
    user_id bigint DEFAULT 0,
    tag_id bigint DEFAULT 0,
    tag_user_id bigint DEFAULT 0,
    created_at bigint NOT NULL
)

TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.tag_user_friend OWNER to imboy_user;

COMMENT ON TABLE public.tag_user_friend IS '朋友标签记录表';

COMMENT ON COLUMN public.tag_user_friend.id IS '主键 自增长ID';

COMMENT ON COLUMN public.tag_user_friend.user_id IS '记录所属用户ID';
COMMENT ON COLUMN public.tag_user_friend.tag_id IS '标签ID';
COMMENT ON COLUMN public.tag_user_friend.tag_user_id IS '被打标签用户ID';

COMMENT ON COLUMN public.tag_user_friend.created_at IS '创建记录Unix时间戳毫秒单位';
