-- Isolated clone only; the test waits for the real retry interval.
BEGIN;
SET LOCAL imboy.enterprise_purge = 'on';
DO $$
DECLARE v_link enterprise_asset_direct_link%ROWTYPE;
BEGIN
    SELECT * INTO STRICT v_link FROM enterprise_asset_direct_link WHERE binding_state='bound' LIMIT 1;
    PERFORM fn_eb_direct_purge_defer(v_link.organization_id,v_link.workspace_id,'asset',v_link.asset_id);
    IF NOT fn_eb_direct_purge_deferred(v_link.organization_id,v_link.workspace_id,'asset',v_link.asset_id) THEN
        RAISE EXCEPTION 'fresh deferral missing';
    END IF;
END $$;
COMMIT;
SELECT pg_sleep(61);
BEGIN;
DO $$
DECLARE v_link enterprise_asset_direct_link%ROWTYPE;
BEGIN
    SELECT * INTO STRICT v_link FROM enterprise_asset_direct_link WHERE binding_state='bound' LIMIT 1;
    IF fn_eb_direct_purge_deferred(v_link.organization_id,v_link.workspace_id,'asset',v_link.asset_id) THEN
        RAISE EXCEPTION 'deferral did not expire';
    END IF;
    PERFORM fn_eb_direct_purge_clear(v_link.organization_id,v_link.workspace_id,'asset',v_link.asset_id);
END $$;
COMMIT;
