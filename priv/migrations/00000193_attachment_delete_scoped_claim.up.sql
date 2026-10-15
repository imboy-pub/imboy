-- Tenant purge must not claim another tenant's deletion job.
CREATE OR REPLACE FUNCTION fn_v5_claim_eb(p_token uuid, p_org bigint, p_ws bigint)
RETURNS TABLE(out_organization_id bigint, out_workspace_id bigint, out_asset_id bigint,
              out_object_key text, out_storage_driver text, out_storage_namespace text,
              out_storage_container text, out_storage_locator_sha256 text,
              out_attempts integer)
LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
BEGIN
    RETURN QUERY
    WITH candidate AS (
        SELECT q.organization_id, q.workspace_id, q.asset_id
        FROM enterprise_asset_delete_queue q
        WHERE q.organization_id = p_org AND q.workspace_id = p_ws
          AND attempts < 8
          AND job_state IN ('ready', 'retry_wait') AND next_retry_at <= clock_timestamp()
        ORDER BY next_retry_at, organization_id, workspace_id, asset_id
        FOR UPDATE SKIP LOCKED LIMIT 1)
    UPDATE enterprise_asset_delete_queue q
        SET job_state = 'leased', lease_token = p_token,
            lease_until = clock_timestamp() + interval '120 seconds',
            attempts = q.attempts + 1, last_error_code = NULL
        FROM candidate c
        WHERE (q.organization_id, q.workspace_id, q.asset_id)
            = (c.organization_id, c.workspace_id, c.asset_id)
        RETURNING q.organization_id, q.workspace_id, q.asset_id, q.object_key,
                  q.storage_driver, q.storage_namespace, q.storage_container,
                  q.storage_locator_sha256, q.attempts;
END;
$$;

ALTER FUNCTION fn_v5_claim_eb(uuid, bigint, bigint) OWNER TO v5_fn_owner;
REVOKE ALL ON FUNCTION fn_v5_claim_eb(uuid, bigint, bigint) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION fn_v5_claim_eb(uuid, bigint, bigint) TO v5_worker;
