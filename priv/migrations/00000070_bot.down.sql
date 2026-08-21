-- 回滚 Bot 表 + OAuth 授权表 + account_type 注释恢复

BEGIN;
DROP TABLE IF EXISTS public.bot_oauth_grant;
DROP TABLE IF EXISTS public.bot;
COMMENT ON COLUMN public."user".account_type IS '账号类型 0=human 1=ai_agent 2=system_bot';
COMMIT;