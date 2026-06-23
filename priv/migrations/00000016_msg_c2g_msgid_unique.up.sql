-- ============================================================
-- 迁移 000016: msg_c2g 主表增加 msg_id 幂等唯一索引
--   背景：msg_store_worker 写入 msg_c2g 主表成功后、标记 processed_at 前若崩溃，
--   重启会重新处理 staging 行 → 主表重复入库（脏行）。msg_c2c 主表已有
--   uk_c2c_msgid_createdat 兜底，msg_c2g 主表此前缺失（仅 timeline 表有唯一约束）。
--   本迁移与 msg_c2c 对齐，使 repo 层 INSERT ... ON CONFLICT (msg_id, created_at)
--   DO NOTHING 生效，实现服务端幂等去重，不再依赖客户端去重。
--   注意：若主表已存在 (msg_id, created_at) 重复行，创建唯一索引会失败，需先清理。
-- ============================================================

CREATE UNIQUE INDEX IF NOT EXISTS uk_c2g_msgid_createdat
    ON public.msg_c2g USING btree (msg_id, created_at);
