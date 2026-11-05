-- Isolated clone with a synthetic confirmed/bound asset. Everything rolls back.
BEGIN;
SET LOCAL imboy.enterprise_purge='on';
DO $$
DECLARE v_link enterprise_asset_direct_link%ROWTYPE;
BEGIN
    SELECT * INTO STRICT v_link FROM enterprise_asset_direct_link WHERE binding_state='bound' LIMIT 1;
    INSERT INTO enterprise_direct_scan_proof(asset_id,object_key,winner_fence,receipt_sha256,ruleset_sha256,expires_at)
    VALUES(v_link.asset_id,v_link.object_key,v_link.winner_fence,v_link.scan_receipt_sha256,
           v_link.scan_ruleset_digest,clock_timestamp()+interval '10 minutes');
    INSERT INTO enterprise_direct_scan_retry(asset_id,failures,retry_after)
    VALUES(v_link.asset_id,1,clock_timestamp()+interval '1 minute');
    UPDATE attach_pending SET upload_expires_at=clock_timestamp()-interval '1 hour' WHERE object_key=v_link.object_key;
    UPDATE attachment SET gc_verified=true WHERE path=v_link.object_key;
    PERFORM fn_eb_direct_retire(v_link.organization_id,v_link.workspace_id,v_link.asset_id);
    BEGIN
        DELETE FROM enterprise_asset WHERE id=v_link.asset_id;
        RAISE EXCEPTION 'asset deleted with an active scan retry';
    EXCEPTION WHEN SQLSTATE '23510' THEN NULL;
    END;
    DELETE FROM enterprise_direct_scan_retry WHERE asset_id=v_link.asset_id;
    DELETE FROM enterprise_direct_scan_proof WHERE asset_id=v_link.asset_id;
    BEGIN
        DELETE FROM enterprise_asset WHERE id=v_link.asset_id;
        RAISE EXCEPTION 'asset deleted with missing bound proof';
    EXCEPTION WHEN SQLSTATE '23510' THEN NULL;
    END;
    INSERT INTO enterprise_direct_scan_proof(asset_id,object_key,winner_fence,receipt_sha256,ruleset_sha256,expires_at)
    VALUES(v_link.asset_id,v_link.object_key,v_link.winner_fence,v_link.scan_receipt_sha256,
           v_link.scan_ruleset_digest,clock_timestamp()+interval '10 minutes');
    UPDATE enterprise_direct_scan_proof SET receipt_sha256=repeat('f',64) WHERE asset_id=v_link.asset_id;
    BEGIN
        DELETE FROM enterprise_asset WHERE id=v_link.asset_id;
        RAISE EXCEPTION 'asset deleted with mismatched proof';
    EXCEPTION WHEN SQLSTATE '23510' THEN NULL;
    END;
    UPDATE enterprise_direct_scan_proof SET receipt_sha256=v_link.scan_receipt_sha256 WHERE asset_id=v_link.asset_id;
    DELETE FROM enterprise_asset WHERE id=v_link.asset_id;
    IF NOT FOUND OR EXISTS (SELECT 1 FROM enterprise_direct_scan_proof WHERE asset_id=v_link.asset_id)
       OR EXISTS (SELECT 1 FROM enterprise_direct_scan_retry WHERE asset_id=v_link.asset_id)
       OR NOT EXISTS (SELECT 1 FROM enterprise_direct_retirement_audit
                      WHERE asset_id=v_link.asset_id AND binding_state='bound'
                        AND scan_receipt_sha256=v_link.scan_receipt_sha256
                        AND scan_ruleset_digest=v_link.scan_ruleset_digest)
       OR NOT EXISTS (SELECT 1 FROM attachment_object_deletion WHERE path=v_link.object_key AND job_state='ready') THEN
        RAISE EXCEPTION 'proof retirement audit or queue mismatch';
    END IF;
END $$;
ROLLBACK;
