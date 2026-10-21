-- 00000215_talk_record_cleanup_t20.down.sql
-- 成对回滚：纯索引删除，无数据依赖。
SET LOCAL lock_timeout = '5s';

DROP INDEX IF EXISTS i_trau_action;
DROP INDEX IF EXISTS i_tr_cleanup_pending;
