-- 迁移 00000142 回滚: push_token 活动 token 唯一绑定（FULL-06）。
--
-- 只回滚本迁移新增的部分唯一索引，不动 push_token 的任何既有列/约束/索引/
-- 触发器，也不撤销 §1 的历史数据处置（status 的降级属于安全方向的单向收紧，
-- 恢复旧 status 只会重建违反 plan-full §7 的状态；token 是瞬时凭据，
-- 客户端下次登录即重新注册）。
--
-- 禁止 BEGIN/COMMIT——erlang_migrate 外层单事务包裹。

SET lock_timeout = '5s';
SET statement_timeout = '15min';

DROP INDEX IF EXISTS public.uq_push_token_active_token;
