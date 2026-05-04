-- 用户表添加 birthday 列
ALTER TABLE public."user" ADD COLUMN IF NOT EXISTS birthday varchar(20) DEFAULT ''::character varying NOT NULL;

COMMENT ON COLUMN public."user".birthday IS '生日，格式 YYYY-MM-DD';
