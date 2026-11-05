GRANT SELECT ON public.channel_message TO v5_fn_owner;

CREATE INDEX idx_channel_message_live_payload ON public.channel_message
    USING gin (payload jsonb_path_ops) WHERE status = 1;

CREATE FUNCTION public.fn_v5_channel_reference_blocks(p_path text) RETURNS boolean
LANGUAGE sql VOLATILE SECURITY DEFINER SET search_path = pg_catalog, public, pg_temp AS $$
    SELECT EXISTS (
        SELECT 1 FROM public.channel_message m
        WHERE m.status = 1
          AND (m.payload @> jsonb_build_object('uri', p_path)
               OR m.payload @> jsonb_build_object('thumb', jsonb_build_object('uri', p_path))
               OR m.payload @> jsonb_build_object('images', jsonb_build_array(jsonb_build_object('uri', p_path))))
    )
$$;
ALTER FUNCTION public.fn_v5_channel_reference_blocks(text) OWNER TO v5_fn_owner;
REVOKE ALL ON FUNCTION public.fn_v5_channel_reference_blocks(text) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION public.fn_v5_channel_reference_blocks(text) TO v5_app_runtime, talk_record_retry_definer;

CREATE OR REPLACE FUNCTION public.fn_v5_intent_ordinary_delete(p_path text, p_reason text)
RETURNS void LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
DECLARE v_a attachment%ROWTYPE; v_p attach_pending%ROWTYPE; v_bucket text;
BEGIN
    PERFORM fn_v5_lock_ordinary_upload(p_path);
    SELECT * INTO v_a FROM attachment WHERE path = p_path FOR UPDATE;
    IF NOT FOUND THEN
        RAISE EXCEPTION 'v5 intent unknown attachment' USING ERRCODE = 'P0002';
    END IF;
    -- Eligibility conjunction, re-checked under lock; unknown => not eligible.
    IF v_a.lifecycle_state <> 'live' THEN
        RAISE EXCEPTION 'v5 intent non-live asset' USING ERRCODE = '23510';
    END IF;
    IF NOT v_a.gc_verified THEN
        RAISE EXCEPTION 'v5 intent requires proven reference-zero (gc_verified)'
            USING ERRCODE = '23510';
    END IF;
    IF fn_talk_record_war_blocks(v_a.id) THEN
        RAISE EXCEPTION 'v5 intent blocked by workspace retention'
            USING ERRCODE = '23510';
    END IF;
    IF EXISTS (SELECT 1 FROM enterprise_attachment_retention
               WHERE attachment_id = v_a.id
                 AND (hold_state = 'held'
                      OR purge_state IS DISTINCT FROM 'purged')) THEN
        RAISE EXCEPTION 'v5 intent blocked by EAR' USING ERRCODE = '23510';
    END IF;
    IF v_a.group_file_id IS NOT NULL
       AND EXISTS (SELECT 1 FROM group_file WHERE id = v_a.group_file_id) THEN
        RAISE EXCEPTION 'v5 intent blocked by live group_file binding'
            USING ERRCODE = '23510';
    END IF;
    -- T06（00000212）：talk_record 有效业务引用（含未到期 processing lease）
    -- 阻止普通域删除意图——pre-lock 快速拒绝。
    IF EXISTS (SELECT 1 FROM talk_record_attachment_ref trar
               WHERE trar.attachment_id = v_a.id
                 AND trar.state = 'bound'
                 AND (trar.lease_until IS NULL OR trar.lease_until > now())) THEN
        RAISE EXCEPTION 'v5 intent blocked by talk_record reference'
            USING ERRCODE = '23510';
    END IF;
    SELECT * INTO v_p FROM attach_pending WHERE object_key = p_path;
    IF FOUND AND (v_p.upload_expires_at IS NULL OR v_p.upload_expires_at > now()) THEN
        RAISE EXCEPTION 'v5 intent upload capability not exhausted'
            USING ERRCODE = '23510';
    END IF;
    IF EXISTS (SELECT 1 FROM attachment_object_deletion WHERE path = p_path) THEN
        RAISE EXCEPTION 'v5 intent job already exists (tombstone)'
            USING ERRCODE = '23505';
    END IF;
    -- Object lock ordering: identity lock, then queue path lock (seed 168).
    PERFORM fn_v5_lock_identity(v_a.storage_driver, v_a.storage_namespace,
                                v_a.storage_container, v_a.path);
    PERFORM pg_advisory_xact_lock(hashtextextended(v_a.path, 168));
    -- Cross-domain same-identity rejection.
    IF EXISTS (SELECT 1 FROM enterprise_asset_delete_queue q
               WHERE q.storage_namespace = v_a.storage_namespace
                 AND q.storage_container = v_a.storage_container
                 AND q.object_key = v_a.path) THEN
        RAISE EXCEPTION 'v5 intent cross-domain identity conflict'
            USING ERRCODE = '23510';
    END IF;
    -- W3-01：锁后资格复核（02 §4——取锁前的候选查询不构成授权）：
    -- 绑定事务与 intent 在同一对象锁上线性化，先提交者唯一胜出。
    IF v_a.group_file_id IS NOT NULL
       AND EXISTS (SELECT 1 FROM group_file gf WHERE gf.id = v_a.group_file_id) THEN
        RAISE EXCEPTION 'v5 intent blocked by live group_file binding'
            USING ERRCODE = '23510';
    END IF;
    IF EXISTS (SELECT 1 FROM user_collect uc
               WHERE uc.attach_file_hash256 <> '' AND v_a.file_hash256 = ANY(string_to_array(uc.attach_file_hash256, ','))
                 AND uc.status = 1) THEN
        RAISE EXCEPTION 'v5 intent blocked by collect reference'
            USING ERRCODE = '23510';
    END IF;
    IF EXISTS (SELECT 1 FROM public.group_album_photo gap
               WHERE gap.photo_url = v_a.path AND gap.status = 1) THEN
        RAISE EXCEPTION 'v5 intent blocked by group album reference'
            USING ERRCODE = '23510';
    END IF;
    IF public.fn_v5_channel_reference_blocks(v_a.path) THEN
        RAISE EXCEPTION 'v5 intent blocked by channel message reference'
            USING ERRCODE = '23510';
    END IF;
    IF public.fn_v5_moment_reference_blocks(v_a.path) THEN
        RAISE EXCEPTION 'v5 intent blocked by moment media reference'
            USING ERRCODE = '23510';
    END IF;
    -- T06（00000212）：锁后复核 talk_record 有效引用（gc_verified 为缓存标志，
    -- 绑定可能晚于上次 refresh —— 02 §5"intent 在对象锁内重查全部引用"）。
    IF EXISTS (SELECT 1 FROM talk_record_attachment_ref trar
               WHERE trar.attachment_id = v_a.id
                 AND trar.state = 'bound'
                 AND (trar.lease_until IS NULL OR trar.lease_until > now())) THEN
        RAISE EXCEPTION 'v5 intent blocked by talk_record reference'
            USING ERRCODE = '23510';
    END IF;
    UPDATE attachment SET lifecycle_state = 'delete_intent'
        WHERE id = v_a.id AND lifecycle_state = 'live';
    IF NOT FOUND THEN
        RAISE EXCEPTION 'v5 intent lost eligibility race' USING ERRCODE = '40001';
    END IF;
    INSERT INTO attachment_object_deletion
        (path, bucket, storage_driver, storage_namespace, storage_container,
         storage_locator_sha256, storage_physical_key, job_state, next_retry_at, last_error_code)
    VALUES (v_a.path, v_a.url, v_a.storage_driver, v_a.storage_namespace,
            v_a.storage_container, v_a.storage_locator_sha256, v_a.storage_physical_key,
            'ready', now(), NULL);
    INSERT INTO attachment_lifecycle_v5_audit
        (queue_domain, object_key, storage_namespace, action, reason, actor, details)
    VALUES ('ordinary', v_a.path, v_a.storage_namespace, 'delete_intent',
            p_reason, current_user,
            jsonb_build_object('attachment_id', v_a.id,
                               'locator', v_a.storage_locator_sha256));
END;
$$;
