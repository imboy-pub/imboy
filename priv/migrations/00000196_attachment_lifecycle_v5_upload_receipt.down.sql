-- 回退会恢复终态/到期后写入漏洞；只能向前修复。
-- Restoring the old pin function would reopen terminal/expired writes.
DO $$
BEGIN
    RAISE EXCEPTION 'V5_DOWN_BLOCKED_USE_FORWARD_FIX';
END;
$$;
