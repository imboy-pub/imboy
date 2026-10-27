-- 审核队列保留 opaque 业务消息 ID；历史实体 TSID 保留十进制原值。
-- Preserve opaque message IDs and decimal values of existing entity references.
ALTER TABLE public.review_queue
    ALTER COLUMN msg_id DROP DEFAULT,
    ALTER COLUMN msg_id TYPE text USING msg_id::text,
    ALTER COLUMN msg_id SET DEFAULT '0';
