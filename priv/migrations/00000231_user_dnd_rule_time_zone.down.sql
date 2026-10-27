-- 00000231_user_dnd_rule_time_zone.down.sql
-- 仅限隔离演练库执行（00000229 down 同约束）：生产 down 会丢弃用户显式
-- 设置的 zone 事实，回退后旧行统一按 fallback zone 解释（行为不坏但丢设置）。

ALTER TABLE IF EXISTS public.user_dnd_rule
    DROP COLUMN IF EXISTS time_zone;
