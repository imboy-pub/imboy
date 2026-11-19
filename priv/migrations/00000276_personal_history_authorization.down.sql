-- 00000276 down：expand-only 新表回退（fresh 部署无存量；有数据按卡回退纪律先保留/补偿，不直接 drop）
DROP TABLE IF EXISTS public.personal_history_authorization;
