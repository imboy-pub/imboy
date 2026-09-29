-- 迁移 00000153_customer_service_seat_console.down.sql: 安全回滚坐席控制台表。
-- fail-closed：表内仍有行（含 revoked 行）时拒绝回滚——public_seat_console_id
-- 可能已被分发嵌入到客户页面，静默删行会让已分发 ID 全部失效且不可恢复。
-- down=安全回滚：清空（应用层先删除全部行）后才允许 DROP。

SET lock_timeout = '5s';
SET statement_timeout = '15min';

DO $$
BEGIN
    IF EXISTS (SELECT 1 FROM customer_service_seat_console) THEN
        RAISE EXCEPTION
            'customer_service_seat_console 仍含数据行：已分发的 public_seat_console_id 不得静默丢弃，先经应用层删除全部行（含 revoked）再回滚';
    END IF;
END;
$$;

-- 次序修正（REVIEW-3 F-5 车道 r3-f5，回改先例：56508b83 修 143 down 次序语义、
-- d8e45ad7 补已应用迁移的 down）：DROP TABLE 会级联删除其上全部索引，原次序
-- （先 DROP TABLE 后 DROP INDEX）使两条 DROP INDEX 成为静默 no-op；按 147
-- 同款「先索引后表」次序排列，使 down 真实按声明语义逐项回滚。
DROP INDEX IF EXISTS i_cssc_org_ws_status;
DROP INDEX IF EXISTS uq_cssc_org_ws_active;
DROP TABLE IF EXISTS customer_service_seat_console;
