-- W2-03 真跑发现：fn_v5_claim_ordinary / fn_v5_claim_eb 函数体为裸
-- WITH...UPDATE...RETURNING（数据返回语句无 RETURN QUERY），PL/pgSQL 丢弃
-- 结果且 SELECT 调用报 42601「no destination for result data」——W1 阶段
-- 该断言从未真正跑绿故未暴露。加 RETURN QUERY 修复（签名不变，可 OR REPLACE）。

DROP FUNCTION IF EXISTS fn_v5_claim_ordinary(uuid);
DROP FUNCTION IF EXISTS fn_v5_claim_eb(uuid);

CREATE OR REPLACE FUNCTION fn_v5_claim_ordinary(p_token uuid)
RETURNS TABLE(out_path text, out_bucket text, out_storage_driver text,
              out_storage_namespace text, out_storage_container text,
              out_storage_locator_sha256 text, out_attempts integer)
LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
BEGIN
    RETURN QUERY
    WITH candidate AS (
        SELECT q.path FROM attachment_object_deletion q
        WHERE attempts < 8
          AND job_state IN ('ready', 'retry_wait') AND next_retry_at <= clock_timestamp()
        ORDER BY next_retry_at, path
        FOR UPDATE SKIP LOCKED LIMIT 1)
    UPDATE attachment_object_deletion q
        SET job_state = 'leased', lease_token = p_token,
            lease_until = clock_timestamp() + interval '120 seconds',
            attempts = q.attempts + 1, last_error_code = NULL
        FROM candidate c WHERE q.path = c.path
        RETURNING q.path, q.bucket, q.storage_driver, q.storage_namespace,
                  q.storage_container, q.storage_locator_sha256, q.attempts;
END;
$$;

ALTER FUNCTION fn_v5_claim_ordinary(uuid) OWNER TO v5_fn_owner;
REVOKE ALL ON FUNCTION fn_v5_claim_ordinary(uuid) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION fn_v5_claim_ordinary(uuid) TO v5_worker;

CREATE OR REPLACE FUNCTION fn_v5_claim_eb(p_token uuid)
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
        WHERE attempts < 8
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

ALTER FUNCTION fn_v5_claim_eb(uuid) OWNER TO v5_fn_owner;
REVOKE ALL ON FUNCTION fn_v5_claim_eb(uuid) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION fn_v5_claim_eb(uuid) TO v5_worker;
