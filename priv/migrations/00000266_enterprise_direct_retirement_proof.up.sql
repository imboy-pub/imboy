SET LOCAL lock_timeout = '5s';
SET LOCAL statement_timeout = '15min';

-- Existing purge authorization and the retirement audit are checked before
-- proof rows are removed with their already-retired asset.
ALTER TABLE public.enterprise_direct_scan_proof
    DROP CONSTRAINT enterprise_direct_scan_proof_asset_id_fkey,
    ADD CONSTRAINT enterprise_direct_scan_proof_asset_id_fkey
        FOREIGN KEY (asset_id) REFERENCES public.enterprise_asset(id) ON DELETE CASCADE;
ALTER TABLE public.enterprise_direct_scan_retry
    DROP CONSTRAINT enterprise_direct_scan_retry_asset_id_fkey,
    ADD CONSTRAINT enterprise_direct_scan_retry_asset_id_fkey
        FOREIGN KEY (asset_id) REFERENCES public.enterprise_asset(id) ON DELETE CASCADE;
GRANT SELECT ON public.enterprise_direct_scan_proof, public.enterprise_direct_scan_retry TO eb_direct_retire_owner;

CREATE FUNCTION public.fn_eb_direct_proof_purge_guard() RETURNS trigger
LANGUAGE plpgsql SECURITY DEFINER SET search_path=pg_catalog,public,pg_temp AS $$
DECLARE v_audit public.enterprise_direct_retirement_audit%ROWTYPE;
DECLARE v_proof public.enterprise_direct_scan_proof%ROWTYPE;
BEGIN
    IF OLD.upload_registration->>'protocol' IS DISTINCT FROM 'direct-v1' THEN RETURN OLD; END IF;
    SELECT * INTO v_audit FROM public.enterprise_direct_retirement_audit
     WHERE asset_id=OLD.id AND organization_id=OLD.organization_id
       AND workspace_id=OLD.workspace_id AND object_key=OLD.object_key;
    IF NOT FOUND THEN RAISE EXCEPTION 'enterprise direct retirement audit missing' USING ERRCODE='23510'; END IF;
    IF EXISTS (SELECT 1 FROM public.enterprise_direct_scan_retry WHERE asset_id=OLD.id) THEN
        RAISE EXCEPTION 'enterprise direct scan retry still active' USING ERRCODE='23510';
    END IF;
    SELECT * INTO v_proof FROM public.enterprise_direct_scan_proof WHERE asset_id=OLD.id;
    IF NOT FOUND AND v_audit.binding_state='bound' THEN
        RAISE EXCEPTION 'enterprise direct scan proof missing' USING ERRCODE='23510';
    END IF;
    IF FOUND AND (v_audit.binding_state <> 'bound' OR v_proof.object_key IS DISTINCT FROM v_audit.object_key
        OR v_proof.receipt_sha256 IS DISTINCT FROM v_audit.scan_receipt_sha256
        OR v_proof.ruleset_sha256 IS DISTINCT FROM v_audit.scan_ruleset_digest) THEN
        RAISE EXCEPTION 'enterprise direct scan proof differs from retirement audit' USING ERRCODE='23510';
    END IF;
    RETURN OLD;
END;
$$;
ALTER FUNCTION public.fn_eb_direct_proof_purge_guard() OWNER TO eb_direct_retire_owner;
REVOKE ALL ON FUNCTION public.fn_eb_direct_proof_purge_guard() FROM PUBLIC;
CREATE TRIGGER eb_direct_proof_purge_guard BEFORE DELETE ON public.enterprise_asset
    FOR EACH ROW EXECUTE FUNCTION public.fn_eb_direct_proof_purge_guard();
