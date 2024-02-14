-- Table: public.group_random_code

-- DROP TABLE IF EXISTS public."group_random_code";

-- 导入数据后，需要更新自增长ID
-- select setval('"group_random_code_id_seq"', (select max(id) from public."group_random_code"));

CREATE TABLE IF NOT EXISTS public."group_random_code"
(
    id BIGSERIAL PRIMARY KEY,
    group_id bigint NOT NULL,
    user_id bigint NOT NULL,
    code varchar(20) DEFAULT '',
    location geometry,
    validity_at bigint DEFAULT 0,
    created_at bigint NOT NULL
)

TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.group_random_code OWNER to imboy_user;

COMMENT ON TABLE public.group_random_code IS '存储面对面建群的随机码和关联的群 ID';

COMMENT ON COLUMN public.group_random_code.id IS '主键 自增长ID';

COMMENT ON COLUMN public.group_random_code.group_id IS '群组ID';

COMMENT ON COLUMN public.group_random_code.user_id IS '创建用户ID';

COMMENT ON COLUMN public.group_random_code.code IS '随机码';
COMMENT ON COLUMN public.group_random_code.validity_at IS '有效期截止时间';

COMMENT ON COLUMN public.group_random_code.created_at IS '创建记录Unix时间戳毫秒单位';

-- index

CREATE INDEX i_group_random_code_location ON public.group_random_code USING GIST(location);
