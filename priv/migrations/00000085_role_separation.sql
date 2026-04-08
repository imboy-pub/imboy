-- Migration: 00000085_role_separation.sql
-- 数据库角色权限分离
-- 创建只读角色和应用角色，最小权限原则
--
-- 使用方式:
--   由 DBA 在 psql 中以 superuser 身份执行
--   应用连接池配置使用 imboy_app 角色
--   管理后台配置使用 imboy_admin 角色
--   只读查询/监控使用 imboy_readonly 角色

-- ============================================================
-- 1. 创建角色（幂等）
-- ============================================================

-- 应用角色 — 业务读写
DO $$
BEGIN
    IF NOT EXISTS (SELECT 1 FROM pg_roles WHERE rolname = 'imboy_app') THEN
        CREATE ROLE imboy_app LOGIN PASSWORD 'CHANGE_ME_APP';
    END IF;
END
$$;

-- 管理后台角色 — 管理操作
DO $$
BEGIN
    IF NOT EXISTS (SELECT 1 FROM pg_roles WHERE rolname = 'imboy_admin') THEN
        CREATE ROLE imboy_admin LOGIN PASSWORD 'CHANGE_ME_ADMIN';
    END IF;
END
$$;

-- 只读角色 — 监控/分析
DO $$
BEGIN
    IF NOT EXISTS (SELECT 1 FROM pg_roles WHERE rolname = 'imboy_readonly') THEN
        CREATE ROLE imboy_readonly LOGIN PASSWORD 'CHANGE_ME_READONLY';
    END IF;
END
$$;

-- ============================================================
-- 2. Schema 权限
-- ============================================================

GRANT USAGE ON SCHEMA public TO imboy_app;
GRANT USAGE ON SCHEMA public TO imboy_admin;
GRANT USAGE ON SCHEMA public TO imboy_readonly;

-- ============================================================
-- 3. 应用角色权限（最小 CRUD）
-- ============================================================

-- 核心业务表：SELECT, INSERT, UPDATE, DELETE
GRANT SELECT, INSERT, UPDATE, DELETE ON ALL TABLES IN SCHEMA public TO imboy_app;

-- 序列（用于 BIGSERIAL 遗留字段，新表用 TSID 不需要）
GRANT USAGE ON ALL SEQUENCES IN SCHEMA public TO imboy_app;

-- 未来新建表自动授权
ALTER DEFAULT PRIVILEGES IN SCHEMA public
    GRANT SELECT, INSERT, UPDATE, DELETE ON TABLES TO imboy_app;
ALTER DEFAULT PRIVILEGES IN SCHEMA public
    GRANT USAGE ON SEQUENCES TO imboy_app;

-- ============================================================
-- 4. 管理后台角色权限
-- ============================================================

-- 管理后台：全部 CRUD + DDL 操作由 DBA 手动处理
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

-- 应用角色不能 TRUNCATE、DROP
REVOKE TRUNCATE ON ALL TABLES IN SCHEMA public FROM imboy_app;

-- 只读角色不能写入
REVOKE INSERT, UPDATE, DELETE, TRUNCATE ON ALL TABLES IN SCHEMA public FROM imboy_readonly;

-- ============================================================
-- 验证
-- ============================================================

-- 验证权限分配（运行后手动检查）:
-- SELECT grantee, privilege_type, table_name
-- FROM information_schema.role_table_grants
-- WHERE grantee IN ('imboy_app', 'imboy_admin', 'imboy_readonly')
-- ORDER BY grantee, table_name;
