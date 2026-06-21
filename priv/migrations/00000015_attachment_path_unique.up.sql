-- ============================================================
-- 迁移 000015: attachment 去重键 md5 -> path(object_key)
--   背景：object_key 每次上传天然唯一（含毫秒时间戳+随机串）。
--   旧 uk_attachment_md5 全局唯一在多 scope/多 owner 上传相同内容时，
--   confirm 落库 ON CONFLICT(md5) DO UPDATE 会把 path/scope 锁死成首条 →
--   上传者下载自己文件 404 + Garage 孤儿 + 潜在越权读。
--   改为 path 唯一：每个 object_key 独立成行（鉴权/归属正确），
--   ON CONFLICT(path) 仅在 confirm 重试/同附件收藏命中（幂等递增 referer）。
--   md5 保留为普通列，仅作文件 hash 完整性参考。
--   同时移除 000014 的 partial 索引 i_attachment_path（被全量唯一索引取代）。
-- ============================================================

ALTER TABLE public.attachment
    DROP CONSTRAINT IF EXISTS uk_attachment_md5;
--;

DROP INDEX IF EXISTS public.i_attachment_path;
--;

ALTER TABLE public.attachment
    ADD CONSTRAINT uk_attachment_path UNIQUE (path);
