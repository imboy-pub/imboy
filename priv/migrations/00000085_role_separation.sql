-- Migration: 00000085_role_separation.sql
-- 数据库角色权限分离
-- 创建只读角色和应用角色，最小权限原则
--
-- ⚠️ 不要纳入自动迁移管道！由 DBA 手动执行
-- 使用方式:
--   1. 由 DBA 在 psql 中以 superuser 身份执行
--   2. 执行后立即为每个角色设置强密码:
--      ALTER ROLE imboy_app PASSWORD 'your_strong_password';
--      ALTER ROLE imboy_admin PASSWORD 'your_strong_password';
--      ALTER ROLE imboy_readonly PASSWORD 'your_strong_password';
--   3. 应用连接池配置使用 imboy_app 角色
--   4. 管理后台配置使用 imboy_admin 角色
--   5. 只读查询/监控使用 imboy_readonly 角色
--
-- 回滚:
--   REVOKE ALL ON ALL TABLES IN SCHEMA public FROM imboy_app, imboy_admin, imboy_readonly;
--   REVOKE ALL ON ALL SEQUENCES IN SCHEMA public FROM imboy_app, imboy_admin, imboy_readonly;
--   REVOKE USAGE ON SCHEMA public FROM imboy_app, imboy_admin, imboy_readonly;
--   DROP ROLE IF EXISTS imboy_readonly;
--   DROP ROLE IF EXISTS imboy_admin;
--   DROP ROLE IF EXISTS imboy_app;

-- ============================================================
-- 1. 创建角色（幂等，不含密码 — 执行后由 DBA 手动设置）
-- ============================================================

-- 应用角色 — 业务读写
DO $$
BEGIN
    IF NOT EXISTS (SELECT 1 FROM pg_roles WHERE rolname = 'imboy_app') THEN
        CREATE ROLE imboy_app LOGIN;
        RAISE NOTICE 'Created role imboy_app — please set password: ALTER ROLE imboy_app PASSWORD ''...''';
    END IF;
END
$$;

-- 管理后台角色 — 管理操作
DO $$
BEGIN
    IF NOT EXISTS (SELECT 1 FROM pg_roles WHERE rolname = 'imboy_admin') THEN
        CREATE ROLE imboy_admin LOGIN;
        RAISE NOTICE 'Created role imboy_admin — please set password: ALTER ROLE imboy_admin PASSWORD ''...''';
    END IF;
END
$$;

-- 只读角色 — 监控/分析
DO $$
BEGIN
    IF NOT EXISTS (SELECT 1 FROM pg_roles WHERE rolname = 'imboy_readonly') THEN
        CREATE ROLE imboy_readonly LOGIN;
        RAISE NOTICE 'Created role imboy_readonly — please set password: ALTER ROLE imboy_readonly PASSWORD ''...''';
    END IF;
END
$$;

-- ============================================================
-- 2. Schema 权限
-- ============================================================

GRANT USAGE ON SCHEMA public TO imboy_app;
GRANT USAGE ON SCHEMA public TO imboy_admin;
GRANT USAGE ON SCHEMA public TO imboy_readonly;

-- 防御性撤销 CREATE 权限（PG15+ 默认已撤销，PG14 及以下需要）
REVOKE CREATE ON SCHEMA public FROM PUBLIC;
REVOKE CREATE ON SCHEMA public FROM imboy_app;
REVOKE CREATE ON SCHEMA public FROM imboy_admin;
REVOKE CREATE ON SCHEMA public FROM imboy_readonly;

-- ============================================================
-- 3. 应用角色权限（最小 CRUD）
-- ============================================================

-- 核心业务表：SELECT, INSERT, UPDATE, DELETE
GRANT SELECT, INSERT, UPDATE, DELETE ON ALL TABLES IN SCHEMA public TO imboy_app;

-- 序列（用于 BIGSERIAL 遗留字段，新表用 TSID 不需要）
GRANT USAGE ON ALL SEQUENCES IN SCHEMA public TO imboy_app;

-- 未来新建表自动授权（仅对当前执行者将来创建的对象生效）
ALTER DEFAULT PRIVILEGES IN SCHEMA public
    GRANT SELECT, INSERT, UPDATE, DELETE ON TABLES TO imboy_app;
ALTER DEFAULT PRIVILEGES IN SCHEMA public
    GRANT USAGE ON SEQUENCES TO imboy_app;

-- ============================================================
-- 4. 管理后台角色权限（CRUD + 管理操作由 DBA 按需额外授权）
-- ============================================================

GRANT SELECT, INSERT, UPDATE, DELETE ON ALL TABLES IN SCHEMA public TO imboy_admin;
GRANT USAGE ON ALL SEQUENCES IN SCHEMA public TO imboy_admin;

ALTER DEFAULT PRIVILEGES IN SCHEMA public
    GRANT SELECT, INSERT, UPDATE, DELETE ON TABLES TO imboy_admin;
ALTER DEFAULT PRIVILEGES IN SCHEMA public
    GRANT USAGE ON SEQUENCES TO imboy_admin;

-- ============================================================
-- 5. 只读角色权限
-- ============================================================

GRANT SELECT ON ALL TABLES IN SCHEMA public TO imboy_readonly;

ALTER DEFAULT PRIVILEGES IN SCHEMA public
    GRANT SELECT ON TABLES TO imboy_readonly;

-- ============================================================
-- 6. 撤销危险权限
-- ============================================================

-- 应用角色不能 TRUNCATE（防御性，GRANT ALL 不含 TRUNCATE，但显式撤销更安全）
REVOKE TRUNCATE ON ALL TABLES IN SCHEMA public FROM imboy_app;

-- 只读角色不能写入
REVOKE INSERT, UPDATE, DELETE, TRUNCATE ON ALL TABLES IN SCHEMA public FROM imboy_readonly;

-- ============================================================
-- 验证（执行后手动检查）
-- ============================================================

-- SELECT grantee, privilege_type, table_name
-- FROM information_schema.role_table_grants
-- WHERE grantee IN ('imboy_app', 'imboy_admin', 'imboy_readonly')
-- ORDER BY grantee, table_name;
--
-- 确认:
--   imboy_app: SELECT, INSERT, UPDATE, DELETE (无 TRUNCATE)
--   imboy_admin: SELECT, INSERT, UPDATE, DELETE
--   imboy_readonly: SELECT only
