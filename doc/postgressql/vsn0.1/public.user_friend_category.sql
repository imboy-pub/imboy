-- Table: public.user_friend_category

-- DROP TABLE IF EXISTS public."user_friend_category";

CREATE TABLE IF NOT EXISTS public."user_friend_category"
(
    id BIGSERIAL PRIMARY KEY,
    name varchar(80) DEFAULT '',
    owner_user_id bigint DEFAULT 0
)

TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.user_friend_category OWNER to imboy_user;

COMMENT ON TABLE public.user_friend_category IS '朋友分组表';

COMMENT ON COLUMN public.user_friend_category.id IS '主键 自增长ID';

COMMENT ON COLUMN public.user_friend_category.name IS '分组名称';

COMMENT ON COLUMN public.user_friend_category.owner_user_id IS '分组所属用户ID';
