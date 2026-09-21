-- 迁移 00000141 down: 对称回滚（只 DROP 本迁移新建的对象/列/约束/索引，不动
-- bot_delivery 既有列、不动 bot 域语义、不动迁移 92/104 的对象）。
--
-- 命名与 up 一一对应（ewh 前缀）。残留核查口径（见 FULL-03 checkpoint）：
--   bot.ewh_endpoint_generation 1 列 + bot_delivery 6 列 + 5 约束 + 2 索引
--   + fn_ewh_delivery_guard 1 函数 + trg_ewh_delivery_guard 1 触发器 = 全 0。

SET lock_timeout = '5s';
SET statement_timeout = '15min';

-- 4) 自证临时表（up 末尾已 DROP；此处兜底，无副作用）
DROP TABLE IF EXISTS ewh_preexisting_fn;

-- 3) 账本守卫函数与触发器
DROP TRIGGER IF EXISTS trg_ewh_delivery_guard ON bot_delivery;
DROP FUNCTION IF EXISTS fn_ewh_delivery_guard();

-- 2) bot_delivery 索引
DROP INDEX IF EXISTS uq_ewh_delivery_replay_inflight;
DROP INDEX IF EXISTS bot_delivery_ewh_owner_idx;

-- 2) bot_delivery 约束
ALTER TABLE bot_delivery DROP CONSTRAINT IF EXISTS fk_ewh_delivery_owner;
ALTER TABLE bot_delivery DROP CONSTRAINT IF EXISTS ck_ewh_delivery_replay_not_self;
ALTER TABLE bot_delivery DROP CONSTRAINT IF EXISTS ck_ewh_delivery_endpoint_generation;
ALTER TABLE bot_delivery DROP CONSTRAINT IF EXISTS ck_ewh_delivery_ledger_version;
ALTER TABLE bot_delivery DROP CONSTRAINT IF EXISTS ck_ewh_delivery_owner_pair;

-- 2) bot_delivery 列
ALTER TABLE bot_delivery DROP COLUMN IF EXISTS ewh_claimed_at;
ALTER TABLE bot_delivery DROP COLUMN IF EXISTS ewh_ledger_version;
ALTER TABLE bot_delivery DROP COLUMN IF EXISTS ewh_endpoint_generation;
ALTER TABLE bot_delivery DROP COLUMN IF EXISTS ewh_replay_of;
ALTER TABLE bot_delivery DROP COLUMN IF EXISTS ewh_owner_application_id;
ALTER TABLE bot_delivery DROP COLUMN IF EXISTS ewh_owner_organization_id;

-- 1) bot 列
ALTER TABLE bot DROP COLUMN IF EXISTS ewh_endpoint_generation;
