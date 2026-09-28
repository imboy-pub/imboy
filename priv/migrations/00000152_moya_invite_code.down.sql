-- 00000152_moya_invite_code.down.sql
-- 安全回滚：仅当表内已无数据时允许 DROP（fail-closed 预检）。
-- 未撤销的邀请码是"仍可进入班级确认页并绑定监护关系"的活凭证，
-- 静默删除会让老师群里已发出的码失去撤销面（无据可查）——有数据时
-- 应显式确认后人工 DROP。

DO $$
DECLARE
    v_count bigint;
BEGIN
    SELECT COUNT(*) INTO v_count FROM moya_invite_code;
    IF v_count > 0 THEN
        RAISE EXCEPTION 'moya_invite_code has % rows; confirm explicitly before dropping', v_count;
    END IF;
END $$;

DROP TABLE IF EXISTS moya_invite_code;
