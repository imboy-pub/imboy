-- Isolated clone; force only the synthetic retry timestamp, then roll back.
BEGIN;
SET LOCAL imboy.enterprise_purge = 'on';
DO $$
DECLARE v_link enterprise_asset_direct_link%ROWTYPE;
BEGIN
    SELECT * INTO STRICT v_link FROM enterprise_asset_direct_link WHERE binding_state='bound' LIMIT 1;
    IF fn_eb_direct_purge_deferred(v_link.organization_id,v_link.workspace_id,'asset',v_link.asset_id) THEN
        RAISE EXCEPTION 'fixture already deferred';
    END IF;
    PERFORM fn_eb_direct_purge_defer(v_link.organization_id,v_link.workspace_id,'asset',v_link.asset_id);
    IF NOT fn_eb_direct_purge_deferred(v_link.organization_id,v_link.workspace_id,'asset',v_link.asset_id)
       OR fn_eb_direct_purge_deferred(v_link.organization_id,v_link.workspace_id,'asset',v_link.asset_id+1) THEN
        RAISE EXCEPTION 'purge deferral scope mismatch';
    END IF;
    UPDATE enterprise_direct_purge_wait SET next_retry_at=clock_timestamp()-interval '1 second'
     WHERE organization_id=v_link.organization_id AND workspace_id=v_link.workspace_id
       AND item_kind='asset' AND item_id=v_link.asset_id;
    IF fn_eb_direct_purge_deferred(v_link.organization_id,v_link.workspace_id,'asset',v_link.asset_id) THEN
        RAISE EXCEPTION 'due purge was not readmitted';
    END IF;
    PERFORM fn_eb_direct_purge_clear(v_link.organization_id,v_link.workspace_id,'asset',v_link.asset_id);
    IF EXISTS (SELECT 1 FROM enterprise_direct_purge_wait WHERE item_kind='asset' AND item_id=v_link.asset_id) THEN
        RAISE EXCEPTION 'completed deferral not cleared';
    END IF;
END $$;
ROLLBACK;
