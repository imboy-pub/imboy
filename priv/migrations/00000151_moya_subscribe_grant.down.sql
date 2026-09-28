-- 00000151_moya_subscribe_grant.down.sql
-- 安全回滚：仅当表内已无数据时允许 DROP（fail-closed 预检）。
-- 已消费的授权额度是「微信侧已发生的事实」的本地镜像，静默删除会让
-- 对账失去依据——有数据时应显式导出后再人工 DROP。

DO $$
DECLARE
    v_count bigint;
BEGIN
    SELECT COUNT(*) INTO v_count FROM moya_subscribe_grant;
    IF v_count > 0 THEN
        RAISE EXCEPTION 'moya_subscribe_grant has % rows; export them explicitly before dropping', v_count;
    END IF;
END $$;

DROP TABLE IF EXISTS moya_subscribe_grant;
