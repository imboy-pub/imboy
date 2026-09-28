-- 迁移 00000150: 企业 Webhook 投递列表 keyset 分页索引（INT-23 / CP-CON-02）。
--
-- 冻结决策（DEC-INT23-COMPAT / 计划 v1.2 §6 CP-CON-02）：
--   * INT-23 GET /api/internal/v1/webhook/deliveries 切 CURSOR-V2 签名游标
--     keyset 分页（排序键 created_at DESC, delivery_id DESC）；
--   * 本迁移只落服务该排序的复合 partial index：
--     (organization_id, application_id, created_at DESC, delivery_id DESC)；
--   * 旧 offset 读面（LIMIT/OFFSET + count(*)）随之移除，旧 page/size 参数
--     按 DEC-INT23-COMPAT 返回 versioned 400（cursor_required_v1）。
--
-- partial 条件说明（对齐 00000141 bot_delivery_ewh_owner_idx 的「企业行限定」
-- 口径）：bot_delivery 与 bot 域物理混表；企业投递行 ownership 两列成对非空
-- （00000141 约束 ck_ewh_delivery_owner_paired：(org IS NULL) = (app IS NULL)，
-- 复合 FK 只对成对非空行生效）。partial 谓词同时要求两列非空，bot 域行
-- （成对 NULL）不入索引，与既有 141 索引语义一致。
--
-- 命名纪律（00000141/149 同款）：不新建函数/触发器，只建索引；幂等用
-- IF NOT EXISTS；down 精确删本索引（不动 141 的 bot_delivery_ewh_owner_idx）。
--
-- 禁止 BEGIN/COMMIT——erlang_migrate 外层单事务包裹。

SET lock_timeout = '5s';
SET statement_timeout = '15min';

CREATE INDEX IF NOT EXISTS bot_delivery_ewh_keyset_idx
    ON bot_delivery (
        ewh_owner_organization_id,
        ewh_owner_application_id,
        created_at DESC,
        delivery_id DESC
    )
    WHERE ewh_owner_organization_id IS NOT NULL
      AND ewh_owner_application_id IS NOT NULL;

COMMENT ON INDEX bot_delivery_ewh_keyset_idx IS
    'INT-23 投递列表 keyset 分页：org+app 前缀等值 + (created_at DESC, delivery_id DESC) 排序，企业行限定（bot 域行成对 NULL 不入索引）';
