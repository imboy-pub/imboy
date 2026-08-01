-- 00000054_attach_pending.up.sql
-- #20：presign 签发后无任何记录 → 未 confirm 的对象永久留在桶里且无法清理
--
-- 背景：
--   attach_logic:presign/5 生成 presigned PUT URL 后不落任何库行；只有
--   confirm/5 成功时 do_save 才写 attachment 表。因此"PUT 上去但从不 confirm"
--   的对象在数据库里不存在，而 attachment_repo:orphan_list_for_delete/1 是
--   `SELECT ... FROM attachment WHERE status = 1 AND referer_time = 0`——
--   只扫表不扫桶，这类对象对清理器完全不可见，占用的空间永远收不回来。
--
--   confirm 路径本身已有兜底（verify_and_save 做 HEAD 核实，超过
--   elib_oss:max_file_size/0 会 delete_object），所以缺口不在"大小无约束"，
--   而在"根本不走 confirm"。
--
-- 为什么不复用 attachment 表（用 status=0 存占位行）：
--   attachment 有 uk_attachment_path UNIQUE (path)，键是够用的。但
--   attachment_repo:save 的 `ON CONFLICT (path) DO UPDATE` 只做
--   `referer_time = referer_time + 1` 加更新引用者/时间戳，**刻意不覆盖**
--   status / size / mime_type / cipher —— 那是"同一附件被再次收藏"的幂等语义。
--   要让 confirm 把 status=0 的占位行转正，就得改这条共享子句去覆盖这些列，
--   等于改动收藏路径的行为，blast radius 比新建一张小表大。
--   （注意：foundation 迁移里的 uk_attachment_md5 / md5 列早已不存在，
--     md5 已改名 file_hash256 且不再唯一，见迁移 000015 的说明。）
--
-- 生命周期：
--   presign  → INSERT
--   confirm  → DELETE（对象已转正，登记在 attachment 表）
--   定时清理 → 超龄仍在表里 = 从未 confirm，先删 S3 对象再删行

CREATE TABLE IF NOT EXISTS public.attach_pending (
    object_key text NOT NULL,
    bucket text DEFAULT ''::text NOT NULL,
    scope text DEFAULT ''::text NOT NULL,
    creator_user_id bigint DEFAULT 0 NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT attach_pending_pkey PRIMARY KEY (object_key)
);

--;

-- 清理任务按 created_at 扫描
CREATE INDEX IF NOT EXISTS i_attach_pending_created_at
    ON public.attach_pending USING btree (created_at);

--;

COMMENT ON TABLE public.attach_pending IS
    '已签发 presigned PUT 但尚未 confirm 的对象登记表；超龄行由 attach_cleanup_logic 连同 S3 对象一并回收';
