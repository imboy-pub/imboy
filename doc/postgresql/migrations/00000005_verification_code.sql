-- Table: public.verification_code

DROP TABLE IF EXISTS public."verification_code";

CREATE TABLE IF NOT EXISTS public."verification_code"
(
    id varchar(80) PRIMARY KEY,
    code varchar(40) DEFAULT '',
    validity_at bigint DEFAULT 0,
    created_at timestamptz DEFAULT CURRENT_TIMESTAMP NOT NULL
)

TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.verification_code OWNER to imboy_user;

COMMENT ON TABLE public.verification_code IS '验证码记录表';

COMMENT ON COLUMN public.verification_code.id IS '主键 唯一标示';
COMMENT ON COLUMN public.verification_code.code IS '随机验证码';
COMMENT ON COLUMN public.verification_code.validity_at IS '有效期截止时间';
COMMENT ON COLUMN public.verification_code.created_at IS '创建记录UTC时间';
