-- 迁移 233 down: 回滚会话偏好世代绑定（W09.b）
--
-- 仅回退结构性增量（两列 + 两索引）；无数据删除（列值随列删除）。

DROP INDEX IF EXISTS public.i_conversation_delete_user_gen;
DROP INDEX IF EXISTS public.i_conversation_pin_user_gen;

ALTER TABLE public.conversation_delete
    DROP COLUMN IF EXISTS generation_id;

ALTER TABLE public.conversation_pin
    DROP COLUMN IF EXISTS generation_id;
