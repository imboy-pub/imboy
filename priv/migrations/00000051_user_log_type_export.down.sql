-- 回滚：user_log.type 允许值退回不含 130
--
-- ⚠️ 回滚前若已存在 type=130 的导出审计行，ADD CONSTRAINT 会失败。
-- 这是刻意为之：审计是不可变追加记录，不能为了回滚 schema 而静默删审计。
-- 确需回滚，请先由人工决定这些审计行的归档去向。

ALTER TABLE public.user_log
    DROP CONSTRAINT IF EXISTS chk_user_log_type;
--;

ALTER TABLE public.user_log
    ADD CONSTRAINT chk_user_log_type
    CHECK (type = ANY (ARRAY[100, 102, 110, 901, 902, 903]));
--;

COMMENT ON COLUMN public.user_log.type IS
    '日志类型: 100 用户注销备份  102 用户注销申请记录 110 修改密码';
--;
