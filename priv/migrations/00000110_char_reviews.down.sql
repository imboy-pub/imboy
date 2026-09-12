-- 00000110_char_reviews.down.sql
-- 回滚 00000110：删除 teacher_review.char_reviews jsonb 列。
-- fail-closed 预检：存在非 NULL char_reviews 数据时拒绝回滚——逐字点评数据
-- 在旧 schema 无处安放，删列即静默丢弃，不可静默丢数据（00000105 down 同哲学）。
-- 禁止 BEGIN/COMMIT——erlang_migrate 外层单事务包裹。

DO $$
BEGIN
    IF EXISTS (SELECT 1 FROM teacher_review WHERE char_reviews IS NOT NULL) THEN
        RAISE EXCEPTION
            '回滚保护：teacher_review 存在非 NULL char_reviews 逐字点评数据，删列将静默丢弃，拒绝回滚'
            USING ERRCODE = '23514',
                  CONSTRAINT = 'trg_review_char_reviews_down_guard',
                  HINT = '需先人工确认丢弃逐字点评数据（或导出备份）后重试';
    END IF;
END;
$$;

ALTER TABLE teacher_review DROP COLUMN IF EXISTS char_reviews;
