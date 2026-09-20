-- 00000110_char_reviews.up.sql
-- 逐字点评 Phase A（char_reviews 契约）：teacher_review 增 jsonb 列
-- 迁移契约：up=可重复执行，down=安全回滚（fail-closed 预检）。禁止
-- BEGIN/COMMIT——erlang_migrate 外层单事务包裹。
--
-- 设计决策：
--   * jsonb 而非子表：整卡读写、无逐字查询/统计需求；与
--     calligraphy_review_draft.result_json jsonb 先例一致。
--   * NULL = 无逐字数据（旧点评/老师未逐字点评），客户端不渲染字卡区；
--     存量行不回填（保持 NULL 兼容，PublishedReview.char_reviews=null 向后兼容）。
--   * 结构白名单校验在应用层（moya_review_logic:parse_char_reviews/1：
--     index≥0 整数、char 非空≤8字节、grade∈good|fair|poor、comment≤300 字节、
--     数组≤50 项，单项越界丢弃不整体拒绝=AI 输出容错）；DB 层不做 jsonb 结构
--     CHECK——"越界项丢弃"容错语义无法用静态约束表达，写侧是唯一入口。

ALTER TABLE teacher_review ADD COLUMN IF NOT EXISTS char_reviews jsonb;

COMMENT ON COLUMN teacher_review.char_reviews IS
    '逐字点评字卡数组（jsonb；[{index,char,grade,comment}]≤50 项，应用层白名单校验；NULL=无逐字数据，不渲染字卡区）';
