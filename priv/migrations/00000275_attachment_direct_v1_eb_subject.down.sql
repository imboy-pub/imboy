-- 00000275 down：删除 EB 企业域主体 direct 会话受限函数
DROP FUNCTION IF EXISTS fn_dv1_quota_check_eb(text, text);
DROP FUNCTION IF EXISTS fn_dv1_register_eb(text, text, text, text, text, text, text, text, text, uuid, text, text, text, text, text, text);
