-- ============================================================
-- 迁移 000013: attachment 资源访问控制（scope/scope_ref）
--   为统一读鉴权模型新增两列：
--     scope     读鉴权范围 public|private|c2c|group|channel|moment
--     scope_ref scope 绑定实体（group→group_id, c2c→conv_key,
--               channel→channel_id, moment→moment_id; public/private 为 NULL）
--   默认 'private' 兼容存量行（最严，零越权风险）；不引入冗余 bucket 列。
-- ============================================================

ALTER TABLE public.attachment
    ADD COLUMN scope character varying(16) NOT NULL DEFAULT 'private';
--;

ALTER TABLE public.attachment
    ADD COLUMN scope_ref text;
--;

COMMENT ON COLUMN public.attachment.scope IS '读鉴权范围: public|private|c2c|group|channel|moment';
--;

COMMENT ON COLUMN public.attachment.scope_ref IS 'scope 绑定实体: group→group_id, c2c→conv_key, channel→channel_id, moment→moment_id; public/private 为 NULL';
