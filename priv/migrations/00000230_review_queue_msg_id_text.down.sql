-- 仅可无损回滚规范 bigint 字符串；禁止截断、重编号或清空 opaque ID。
-- Reject rollback unless every ID round-trips through bigint without changes.
DO $$
BEGIN
    LOCK TABLE public.review_queue IN ACCESS EXCLUSIVE MODE;
    IF EXISTS (
        SELECT 1 FROM public.review_queue
        WHERE NOT CASE
            WHEN msg_id ~ '^(0|-?[1-9][0-9]{0,18})$'
            THEN msg_id::numeric BETWEEN -9223372036854775808 AND 9223372036854775807
            ELSE false
        END
    ) THEN
        RAISE EXCEPTION 'review_queue.msg_id rollback refused: IDs must be canonical bigint decimal strings';
    END IF;
    ALTER TABLE public.review_queue
        ALTER COLUMN msg_id DROP DEFAULT,
        ALTER COLUMN msg_id TYPE bigint USING msg_id::bigint,
        ALTER COLUMN msg_id SET DEFAULT 0;
END
$$;
