-- 00000106_group_task_idempotency.down.sql
-- 安全回滚：仅撤除本迁移新增的幂等列/索引（group_task 原有结构不动）。
-- 注意：down 会丢教学作业发布的幂等键数据（expand 列，无历史回填负担）；
-- down 后教学作业发布接口（依赖持久幂等）必须停用或回退，否则同 key 重复发布会产生重复 task。

DROP INDEX IF EXISTS uk_group_task_idempotency;

ALTER TABLE group_task DROP COLUMN IF EXISTS request_digest;
ALTER TABLE group_task DROP COLUMN IF EXISTS idempotency_key;
