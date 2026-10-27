-- 00000231_user_dnd_rule_time_zone.up.sql
-- D02（IMBOY-TIMEZONE-DECISIONS-V1，2026-10-27 冻结）：账号级 IANA 时区。
--
-- 账号级固定 zone（1c：显式设置 + 设备初值预填，绝不自动跟随）；
-- 旧 NULL 行按部署 fallback zone 解释（dnd_fallback_zone，本部署 =
-- Asia/Shanghai，用户 2026-10-27 确认），不回填、不改写旧行（3a 冻结）。
--
-- expand 阶段：仅加可空列。判定侧（user_dnd_rule_ds:is_dnd_now）读该列，
-- NULL → fallback；upsert 写路径校验 pg_timezone_names 合法性。
-- 本轮范围 5b：仅后端合同与迁移，无 HTTP API、无 FL 设置界面。

ALTER TABLE IF EXISTS public.user_dnd_rule
    ADD COLUMN IF NOT EXISTS time_zone text;

COMMENT ON COLUMN public.user_dnd_rule.time_zone IS 'D02：账号级 IANA 时区（显式设置，设备仅初值预填不自动跟随）；NULL=旧行，按部署 fallback zone（应用配置 dnd_fallback_zone）解释，禁止从 offset 推断';
