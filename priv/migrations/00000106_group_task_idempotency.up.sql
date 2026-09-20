-- 00000106_group_task_idempotency.up.sql
-- 墨芽 P0-3（MN-TASK-02）：group_task 补教师发布教学作业的持久幂等列。
-- 行为契约：
--   "写请求必须带 Idempotency-Key …… 同 key 同 digest 返回原结果并置 replayed=true，
--    同 key 不同 body 返回 5460，缺 key 返回 5461"；教学作业的 task+assignments
--   必须同一事务零半成品，幂等真源必须是持久列（禁止 ETS/进程缓存/先查后写）。
-- 迁移范本：00000098_submission_idempotency_withdraw（homework_submission 幂等配方）。
-- 迁移契约：up=可重复执行，down=安全回滚。禁止 BEGIN/COMMIT——erlang_migrate 外层单事务包裹。
--
-- 设计决策（与 00000098 同语义，MN-TASK-02）：
--   * 幂等键三元组 (creator_id, group_id, idempotency_key) 部分唯一索引：
--     同老师同班同键只允许一条 group_task，重放命中返回原 task 与其 assignments
--     （应用层 ON CONFLICT DO NOTHING + 回读）；request_digest 不进唯一键——
--     同 key 不同载荷由应用层回读比对后拒绝（错误码 5460 语义），
--     DB 层只保证"同 key 不会产生第二行"。
--   * idempotency_key 可空：兼容存量普通群作业（group_task_ds:insert 无幂等键写入）
--     与内部/测试写入；唯一性仅在键非空时生效（NULL 在唯一索引中互不冲突）。
--   * 通用群作业（/api/v1/group/task/create）不受影响：该路径不写幂等列，两列保持 NULL。

ALTER TABLE group_task ADD COLUMN IF NOT EXISTS idempotency_key character varying(128);
ALTER TABLE group_task ADD COLUMN IF NOT EXISTS request_digest  character varying(128);

COMMENT ON COLUMN group_task.idempotency_key IS '教学作业发布幂等键（可空=普通群作业/内部写入；同 (creator_id,group_id,idempotency_key) 唯一，重放返回原 task+assignments 集合）';
COMMENT ON COLUMN group_task.request_digest  IS '请求载荷摘要（sha256 hex；同 key 不同 digest 由应用层判 5460 冲突，DB 不参与唯一性）';

-- 有效唯一约束（幂等键非空时生效）
CREATE UNIQUE INDEX IF NOT EXISTS uk_group_task_idempotency
    ON group_task USING btree (creator_id, group_id, idempotency_key)
    WHERE idempotency_key IS NOT NULL;
