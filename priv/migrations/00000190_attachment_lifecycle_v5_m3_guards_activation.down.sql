-- V5 DOWN 阻断（03 M4）：M3 守卫激活后禁止回退（守卫即安全边界——移除
-- 即打开直接 DML 面）。任何非空/空库一律 FORWARD FIX。
DO $$
BEGIN
    RAISE EXCEPTION 'V5_DOWN_BLOCKED_USE_FORWARD_FIX';
END;
$$;

DROP TRIGGER IF EXISTS trg_v5_metadata_delete_guard ON attachment;
DROP TRIGGER IF EXISTS trg_v5_ebdq_job_guard ON enterprise_asset_delete_queue;
DROP TRIGGER IF EXISTS trg_v5_aod_job_guard ON attachment_object_deletion;
DROP TRIGGER IF EXISTS trg_v5_eb_audit_actor_guard ON enterprise_asset;
DROP TRIGGER IF EXISTS trg_v5_eb_registration_guard ON enterprise_asset;
DROP TRIGGER IF EXISTS trg_v5_pending_session_guard ON attach_pending;
DROP TRIGGER IF EXISTS trg_v5_attachment_first_bind ON attachment;
DROP TRIGGER IF EXISTS trg_v5_attachment_identity_immutable ON attachment;
DROP TRIGGER IF EXISTS trg_v5_attachment_location_immutable ON attachment;
