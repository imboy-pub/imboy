-- 00000198 down: 回退为 TSID bigint 死列形态（与 00000001 建表一致）。
-- 注意: 若 up 之后有业务写入了非数字 Xid 字符串，回退会失败——需先清理。
ALTER TABLE public.conversation
    ALTER COLUMN last_msg_id TYPE bigint
        USING CASE WHEN last_msg_id IS NULL THEN 0 ELSE last_msg_id::bigint END,
    ALTER COLUMN last_msg_id SET DEFAULT 0;

COMMENT ON COLUMN public.conversation.last_msg_id IS '最后消息ID (TSID bigint)';
