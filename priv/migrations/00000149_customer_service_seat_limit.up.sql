-- 迁移 00000149: 客服席位 entitlement（CS-BE-06，组织级人工配置 seat_limit）。
--
-- 冻结决策（CS-DEC-03 / 计划 v1.1 CS-BE-06）：
--   * 第一版席位 entitlement = **人工配置的 seat_limit**（组织级一行）；
--     无行 = 现存组织默认 **unlimited**（不回填数值，不新建默认行）。
--   * 降低 limit 不自动停用既有 Seat：检查点只在「新增/恢复超额 Seat」
--     （create/resume/provision 修复），存量超额**可读可减不可增**。
--   * 不接计费、不自动扣费、不自动扩缩（计划 2.2 禁区；BLOCKED_SCOPE_
--     EXPANSION）。
--   * 并发裁决：应用层在「计数 + 插入」同一事务内取 pg_advisory_xact_lock
--     （per-org）——N 并发开 N+1 恰一失败；本表只存配置事实，无计数列
--     （used 由 enabled seat 计数现算，杜绝双写漂移，与 CS-BE-04 同纪律）。
--
-- 命名纪律（FULL-02 / 00000147/148 同款）：不新建函数/触发器，只建表/约束/
-- 注释；幂等用 IF NOT EXISTS；limit 下界 1（0 不允许——用「删行」表达 unlimited）。
--
-- 禁止 BEGIN/COMMIT——erlang_migrate 外层单事务包裹。

SET lock_timeout = '5s';
SET statement_timeout = '15min';

CREATE TABLE IF NOT EXISTS customer_service_seat_limit (
    organization_id bigint NOT NULL,
    seat_limit      integer NOT NULL,
    updated_at      timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT pk_customer_service_seat_limit PRIMARY KEY (organization_id),
    CONSTRAINT ck_cssl_limit_positive CHECK (seat_limit >= 1)
);

COMMENT ON TABLE customer_service_seat_limit IS
    '客服席位 entitlement（CS-BE-06，组织级人工配置）：无行=unlimited（现存组织默认）；降低 limit 不自动停用既有 Seat，只阻止新增/恢复超额并返回 seat_limit_exceeded；used 由 enabled seat 现算（无计数列）；不接计费（CS-DEC-03）';
COMMENT ON COLUMN customer_service_seat_limit.seat_limit IS
    '本组织允许的启用坐席上限（>=1）；降低不自动停用存量，只封新增/恢复';
