-- 迁移 00000129 down：回滚 Department Directory（纯 expand 迁移，可直接整表回滚）。
-- 无生产数据时安全；有数据后按 C10 ROLLBACK 走 feature-disable，不做 destructive down。
-- 本文件仅在本迁移是当前 head 时被 erlang_migrate（strict）执行。

SET lock_timeout = '5s';
SET statement_timeout = '15min';

DROP TRIGGER IF EXISTS trg_organization_department_member_active_guard
    ON organization_department_member;
DROP FUNCTION IF EXISTS fn_organization_department_member_active_guard();

DROP TRIGGER IF EXISTS trg_organization_department_cycle_guard ON organization_department;
DROP FUNCTION IF EXISTS fn_organization_department_cycle_guard();

DROP TABLE IF EXISTS organization_department_member;
DROP TABLE IF EXISTS organization_department;
