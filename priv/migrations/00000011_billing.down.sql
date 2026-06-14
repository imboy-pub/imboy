-- ============================================================
-- 迁移回滚 000011: billing SaaS 计费子系统
-- ============================================================

DROP TABLE IF EXISTS public."billing_invoice" CASCADE;
--;

DROP TABLE IF EXISTS public."billing_usage" CASCADE;
--;

DROP TABLE IF EXISTS public."billing_subscription" CASCADE;
--;

DROP TABLE IF EXISTS public."billing_plan" CASCADE;
