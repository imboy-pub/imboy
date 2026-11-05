SET LOCAL lock_timeout = '5s';
SET LOCAL statement_timeout = '15min';
DROP TRIGGER eb_direct_proof_purge_guard ON public.enterprise_asset;
DROP FUNCTION public.fn_eb_direct_proof_purge_guard();
REVOKE SELECT ON public.enterprise_direct_scan_proof, public.enterprise_direct_scan_retry FROM eb_direct_retire_owner;
ALTER TABLE public.enterprise_direct_scan_proof
    DROP CONSTRAINT enterprise_direct_scan_proof_asset_id_fkey,
    ADD CONSTRAINT enterprise_direct_scan_proof_asset_id_fkey
        FOREIGN KEY (asset_id) REFERENCES public.enterprise_asset(id);
ALTER TABLE public.enterprise_direct_scan_retry
    DROP CONSTRAINT enterprise_direct_scan_retry_asset_id_fkey,
    ADD CONSTRAINT enterprise_direct_scan_retry_asset_id_fkey
        FOREIGN KEY (asset_id) REFERENCES public.enterprise_asset(id);
