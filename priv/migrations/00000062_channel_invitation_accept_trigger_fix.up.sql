-- 迁移 000062: 修复频道邀请接受失败（触发器漏 id 列）
-- Drop the broken tr_channel_invitation_accept trigger.
--
-- 背景 / Background:
--   00000001 定义了 fn_channel_invitation_accept()：当 channel_invitation
--   status 0→1 时向 channel_subscription 插入订阅行。但该 INSERT 未提供 id 列，
--   TSID 迁移（e72a4884，BIGSERIAL→BIGINT NOT NULL）后 channel_subscription.id
--   为 bigint NOT NULL 且无默认值 → 每次接受邀请必抛
--   "null value in column \"id\" of relation \"channel_subscription\" violates
--    not-null constraint"，accept 全链路失败。
--   真机实锤：117 真机接受邀请点「接受」→ 服务端 23502 not_null_violation。
--
-- 修复选择删触发器而非补 id：
--   1. 应用层 channel_ds:subscribe/2（P0-2 修复）已在 accept 事务后调用
--      channel_subscription_repo:upsert_active（生成 TSID id + 幂等 +
--      订阅者计数 increment_subscribers），触发器插入既不生成 id 也不维护
--      订阅者计数，是残缺旧实现，纯冗余。
--   2. 补 id 需在 SQL 层伪造 TSID，存在与应用层 id 冲突风险，违背 KISS。
--
-- 受影响流程 / Affected flow:
--   POST /api/v1/channel/invitation/accept —— 接受邀请（真机验证失败）
--   修复后链路: UPDATE status=1 → channel_ds:subscribe（带 id）→ notify。

DROP TRIGGER IF EXISTS tr_channel_invitation_accept ON public.channel_invitation;
DROP FUNCTION IF EXISTS public.fn_channel_invitation_accept();
