-- V5.5 M3-guards 正式激活（W4-04）：guards-contract.sql 原文成为正式迁移。
-- M2 (00000178/186/187) 已安装此处引用的全部函数定义；本迁移只 ATTACH
-- 最终守卫并收紧 legacy 权限，不 CREATE OR REPLACE 任何 enterprise 拥有的
-- 函数。幂等：W1-02 guards-contract phase 已在隔离库应用过同内容的库，
-- 本迁移在全新库重放（SET LOCAL lock_timeout 5s / statement_timeout 15min）。
SET LOCAL lock_timeout = '5s';
SET LOCAL statement_timeout = '15min';

-- ---------------------------------------------------------------- attach triggers
CREATE TRIGGER trg_v5_attachment_location_immutable
    BEFORE UPDATE OF storage_driver, storage_namespace, storage_container,
        storage_locator_sha256 ON attachment
    FOR EACH ROW EXECUTE FUNCTION fn_v5_attachment_location_immutable();

CREATE TRIGGER trg_v5_attachment_identity_immutable
    BEFORE UPDATE ON attachment
    FOR EACH ROW EXECUTE FUNCTION fn_v5_attachment_identity_immutable();

CREATE TRIGGER trg_v5_attachment_first_bind
    BEFORE UPDATE OF group_file_id, scope_ref, anchor_conv_seq ON attachment
    FOR EACH ROW EXECUTE FUNCTION fn_v5_first_bind_guard();

CREATE TRIGGER trg_v5_pending_session_guard
    BEFORE INSERT OR UPDATE ON attach_pending
    FOR EACH ROW EXECUTE FUNCTION fn_v5_pending_session_guard();

CREATE TRIGGER trg_v5_eb_registration_guard
    BEFORE INSERT OR UPDATE ON enterprise_asset
    FOR EACH ROW EXECUTE FUNCTION fn_v5_eb_registration_guard();

CREATE TRIGGER trg_v5_eb_audit_actor_guard
    BEFORE UPDATE OF uploaded_by_user_id ON enterprise_asset
    FOR EACH ROW EXECUTE FUNCTION fn_v5_eb_audit_actor_guard();

CREATE TRIGGER trg_v5_aod_job_guard
    BEFORE UPDATE OR DELETE ON attachment_object_deletion
    FOR EACH ROW EXECUTE FUNCTION fn_v5_queue_job_guard();

CREATE TRIGGER trg_v5_ebdq_job_guard
    BEFORE UPDATE OR DELETE ON enterprise_asset_delete_queue
    FOR EACH ROW EXECUTE FUNCTION fn_v5_queue_job_guard();

CREATE TRIGGER trg_v5_metadata_delete_guard
    BEFORE DELETE ON attachment
    FOR EACH ROW EXECUTE FUNCTION fn_v5_metadata_delete_guard();

-- ---------------------------------------------------------------- tighten DML
-- Runtime may not write queues or protected columns directly; all queue
-- mutations go through restricted functions. Proof role reads only.
REVOKE INSERT, UPDATE, DELETE, TRUNCATE ON attachment_object_deletion FROM PUBLIC;
REVOKE INSERT, UPDATE, DELETE, TRUNCATE ON enterprise_asset_delete_queue FROM PUBLIC;
GRANT SELECT ON attachment_object_deletion TO v5_app_runtime, v5_worker, v5_proof;
GRANT SELECT ON enterprise_asset_delete_queue TO v5_app_runtime, v5_worker, v5_proof;
GRANT SELECT, INSERT ON attachment_lifecycle_v5_audit TO v5_fn_owner;

-- Session tables: revoke broad UPDATE, grant unprotected-column list only.
REVOKE UPDATE ON attach_pending FROM PUBLIC;
GRANT SELECT ON attach_pending TO v5_app_runtime, v5_worker;
GRANT SELECT ON enterprise_asset TO v5_app_runtime, v5_worker, v5_proof;
GRANT SELECT ON attachment TO v5_app_runtime, v5_worker, v5_proof;
GRANT SELECT, INSERT ON attachment TO v5_app_runtime;
GRANT UPDATE (referer_time, last_referer_user_id, last_referer_at,
    last_referer_at, status) ON attachment TO v5_app_runtime;
GRANT SELECT, INSERT ON enterprise_asset TO v5_app_runtime;
GRANT UPDATE (status, retain_until, pending_object_delete, deleted_at, version,
    updated_at) ON enterprise_asset TO v5_app_runtime;

-- Workers cannot enqueue/intent (no execute), cannot become owners.
REVOKE EXECUTE ON FUNCTION fn_v5_intent_ordinary_delete(text, text) FROM v5_worker;
REVOKE EXECUTE ON FUNCTION fn_v5_intent_eb_delete(bigint, bigint, bigint, text)
    FROM v5_worker;
REVOKE EXECUTE ON FUNCTION fn_v5_register_pending(text, text, text, bigint, uuid,
    jsonb, timestamptz, timestamptz, text, text, text, text) FROM v5_worker, v5_proof;
REVOKE EXECUTE ON FUNCTION fn_v5_proof_activate_ordinary(text, text, text, text, text)
    FROM v5_app_runtime, v5_worker;
