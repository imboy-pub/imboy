-- Acquire workspace, physical identity and object locks before attachment rows.
SET LOCAL lock_timeout = '5s';
-- PostgreSQL 18 terminates this migration transaction before writer blocking reaches five seconds.
SET LOCAL transaction_timeout = '4s';
-- Large installations may prebuild these exact indexes CONCURRENTLY before migration.
-- A timeout rolls back the transaction; inspect migration history before retrying.
SET LOCAL statement_timeout = '5s';
CREATE INDEX IF NOT EXISTS i_v5_attachment_file_hash256 ON public.attachment (file_hash256);
CREATE INDEX IF NOT EXISTS i_v5_attachment_group_file_id ON public.attachment (group_file_id);
CREATE INDEX IF NOT EXISTS i_v5_aod_physical_key ON public.attachment_object_deletion
    ((coalesce(storage_physical_key, path)));
CREATE INDEX IF NOT EXISTS i_v5_ebdq_physical_key ON public.enterprise_asset_delete_queue
    ((coalesce(storage_physical_key, object_key)));
DO $$
DECLARE spec record;
BEGIN
    FOR spec IN SELECT * FROM (VALUES
        ('i_v5_attachment_file_hash256', 'file_hash256'),
        ('i_v5_attachment_group_file_id', 'group_file_id')
    ) AS wanted(index_name, column_name) LOOP
        IF NOT EXISTS (
            SELECT 1 FROM pg_catalog.pg_index i
            JOIN pg_catalog.pg_class c ON c.oid = i.indexrelid
            JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace
            JOIN pg_catalog.pg_am am ON am.oid = c.relam
            WHERE n.nspname = 'public' AND c.relname = spec.index_name
              AND i.indrelid = 'public.attachment'::regclass AND am.amname = 'btree'
              AND i.indisvalid AND i.indisready AND i.indnatts = 1
              AND i.indpred IS NULL AND i.indexprs IS NULL
              AND pg_catalog.pg_get_indexdef(i.indexrelid, 1, true) = spec.column_name
        ) THEN
            RAISE EXCEPTION 'invalid prerequisite index %', spec.index_name;
        END IF;
    END LOOP;
    FOR spec IN SELECT * FROM (VALUES
        ('i_v5_aod_physical_key', 'attachment_object_deletion', 'COALESCE(storage_physical_key, path)'),
        ('i_v5_ebdq_physical_key', 'enterprise_asset_delete_queue', 'COALESCE(storage_physical_key, object_key)')
    ) AS wanted(index_name, table_name, expression) LOOP
        IF NOT EXISTS (
            SELECT 1 FROM pg_catalog.pg_index i
            JOIN pg_catalog.pg_class c ON c.oid=i.indexrelid
            JOIN pg_catalog.pg_namespace n ON n.oid=c.relnamespace
            JOIN pg_catalog.pg_am am ON am.oid=c.relam
            WHERE n.nspname='public' AND c.relname=spec.index_name
              AND i.indrelid=pg_catalog.to_regclass('public.' || spec.table_name)
              AND am.amname='btree' AND i.indisvalid AND i.indisready AND i.indnatts=1
              AND i.indpred IS NULL AND pg_catalog.pg_get_expr(i.indexprs,i.indrelid)=spec.expression
        ) THEN
            RAISE EXCEPTION 'invalid prerequisite index %', spec.index_name;
        END IF;
    END LOOP;
END $$;
SET LOCAL statement_timeout = '15min';

-- Decode JSON strings before comparison; JSON text escapes quotes and backslashes.
CREATE FUNCTION public.fn_v5_moment_reference_blocks(p_path text) RETURNS boolean
LANGUAGE sql VOLATILE SECURITY DEFINER SET search_path = pg_catalog, public, pg_temp AS $$
    SELECT EXISTS (
        SELECT 1 FROM public.moment_post mp
        WHERE position(p_path IN mp.media::text) > 0
           OR EXISTS (SELECT 1 FROM jsonb_path_query(mp.media,
               '$.** ? (@.type() == "string")') AS value
               WHERE position(p_path IN (value #>> '{}')) > 0)
    )
$$;
ALTER FUNCTION public.fn_v5_moment_reference_blocks(text) OWNER TO v5_fn_owner;
REVOKE ALL ON FUNCTION public.fn_v5_moment_reference_blocks(text) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION public.fn_v5_moment_reference_blocks(text)
    TO v5_app_runtime, talk_record_retry_definer;

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

CREATE OR REPLACE FUNCTION public.fn_talk_record_cleanup_retry(p_actor bigint,p_record bigint,p_revision bigint,
 p_generation bigint,p_key text,p_hash text,p_reason text,p_job bigint) RETURNS jsonb
LANGUAGE plpgsql SECURITY DEFINER SET search_path=pg_catalog,public,pg_temp AS $$
DECLARE r public.talk_record%ROWTYPE; a public.attachment%ROWTYPE;
 q public.attachment_object_deletion%ROWTYPE; org bigint; role text;
 prior public.talk_record_cleanup_retry%ROWTYPE; resources jsonb:='[]';
 result jsonb; plan jsonb; count_reopened integer:=0;
BEGIN
 IF p_actor IS NULL OR p_job IS NULL OR p_generation IS NULL OR p_revision IS NULL
  OR p_hash IS NULL OR p_actor<=0 OR p_job<=0 OR p_generation<0 OR p_revision<=0 OR p_key IS NULL
  OR length(p_key) NOT BETWEEN 1 AND 128 OR p_hash !~ '^[0-9a-f]{64}$'
  OR p_reason IS NULL OR length(p_reason)>200 THEN
  RETURN jsonb_build_object('error','invalid_body');
 END IF;
 SELECT organization_id INTO org FROM public.talk_record WHERE id=p_record;
 IF org IS NOT NULL THEN
  PERFORM id FROM public.organization WHERE id=org AND status='active' FOR SHARE;
  IF NOT FOUND THEN RETURN jsonb_build_object('error','not_found'); END IF;
  SELECT m.role INTO role FROM public.organization_member m
   WHERE m.organization_id=org AND m.user_id=p_actor AND m.status='active' FOR SHARE;
  IF role IS NULL OR role NOT IN ('owner','admin') THEN RETURN jsonb_build_object('error','not_found'); END IF;
 END IF;
 SELECT * INTO r FROM public.talk_record WHERE id=p_record FOR UPDATE;
 IF NOT FOUND OR (r.scope='personal' AND r.owner_user_id<>p_actor)
  OR (r.scope='organization' AND (org IS NULL OR r.organization_id<>org)) THEN
  RETURN jsonb_build_object('error','not_found');
 END IF;
 IF r.deleted_at IS NULL THEN RETURN jsonb_build_object('error','record_not_deleted'); END IF;
 SELECT * INTO prior FROM public.talk_record_cleanup_retry
  WHERE record_id=p_record AND actor_user_id=p_actor AND request_key=p_key;
 IF FOUND THEN
  IF prior.payload_hash<>p_hash THEN RETURN jsonb_build_object('error','idempotency_conflict'); END IF;
  RETURN prior.response||jsonb_build_object('replayed',true);
 END IF;
 IF r.revision<>p_revision THEN RETURN jsonb_build_object('error','revision_mismatch'); END IF;
 IF r.cleanup_generation<>p_generation THEN RETURN jsonb_build_object('error','cleanup_generation_conflict'); END IF;
 IF r.cleanup_status NOT IN ('failed','pending','retained_by_other_reference','blocked_hold','blocked_retention') THEN RETURN jsonb_build_object('error','cleanup_not_retryable'); END IF;
 -- Identity locks precede attachment row locks in the shared deletion protocol.
 FOR a IN SELECT * FROM public.attachment WHERE id IN (
  SELECT attachment_id FROM public.talk_record_attachment_ref WHERE record_id=p_record
  UNION SELECT dw.attachment_id FROM public.attachment_derived_write dw
   JOIN public.talk_record_job j ON j.id=dw.source_job_id WHERE j.record_id=p_record)
  ORDER BY storage_driver NULLS FIRST,storage_namespace NULLS FIRST,
   storage_container NULLS FIRST,path LOOP
  PERFORM public.fn_v5_lock_identity(a.storage_driver,a.storage_namespace,a.storage_container,a.path);
 END LOOP;
 -- Lock all authoritative assets in stable order before taking any queue lock.
 FOR a IN SELECT * FROM public.attachment WHERE id IN (
  SELECT attachment_id FROM public.talk_record_attachment_ref WHERE record_id=p_record
  UNION SELECT dw.attachment_id FROM public.attachment_derived_write dw
   JOIN public.talk_record_job j ON j.id=dw.source_job_id WHERE j.record_id=p_record)
  ORDER BY id FOR UPDATE LOOP
  PERFORM attachment_id FROM public.enterprise_attachment_retention WHERE attachment_id=a.id FOR UPDATE;
  PERFORM attachment_id FROM public.workspace_attachment_retention WHERE attachment_id=a.id FOR UPDATE;
 END LOOP;
 -- Queue locks follow the identity/asset order. Readonly caps
 -- use the same plan without these locks; POST always re-evaluates after locking.
 FOR a IN SELECT * FROM public.attachment WHERE id IN (
  SELECT attachment_id FROM public.talk_record_attachment_ref WHERE record_id=p_record
  UNION SELECT dw.attachment_id FROM public.attachment_derived_write dw
   JOIN public.talk_record_job j ON j.id=dw.source_job_id WHERE j.record_id=p_record)
  ORDER BY path LOOP
  PERFORM path FROM public.attachment_object_deletion WHERE path=a.path FOR UPDATE;
 END LOOP;
 plan:=public.fn_talk_record_retry_plan(p_record);
 IF plan?'error' THEN RETURN plan; END IF;
 resources:=plan->'resources';
 FOR result IN SELECT value FROM jsonb_array_elements(resources) LOOP
  UPDATE public.attachment_object_deletion SET retry_generation=retry_generation+1,attempt_base=attempts,
   job_state='ready',next_retry_at=now(),last_error_code=NULL,lease_token=NULL,lease_until=NULL
   WHERE path=result->>'path' AND retry_generation=(result->>'generation')::bigint
    AND job_state='blocked' AND attempts=(result->>'attempts')::integer;
  IF NOT FOUND THEN RAISE EXCEPTION 'cleanup_generation_conflict' USING ERRCODE='40001'; END IF;
  INSERT INTO public.attachment_lifecycle_v5_audit(queue_domain,object_key,storage_namespace,action,reason,actor,details)
   SELECT 'ordinary',path,storage_namespace,'talk_record_retry',p_reason,p_actor::text,
    jsonb_build_object('record_id',p_record,'generation',retry_generation,'attempt_base',attempt_base,'request_key',p_key)
    FROM public.attachment_object_deletion WHERE path=result->>'path';
  count_reopened:=count_reopened+1;
 END LOOP;
 UPDATE public.talk_record SET cleanup_generation=cleanup_generation+1,revision=revision+1,
  cleanup_status=plan->>'cleanup_status',updated_at=now() WHERE id=p_record;
 INSERT INTO public.talk_record_job(id,record_id,kind,dedupe_key)
  VALUES(p_job,p_record,'purge','purge-recovery:'||p_record||':'||(r.cleanup_generation+1));
 result:=jsonb_build_object('job_id',p_job,'revision',r.revision+1,'cleanup_generation',r.cleanup_generation+1,
  'cleanup_status',plan->>'cleanup_status','reopened_resources',count_reopened,
  'protected_resources',(plan->>'protected_resources')::integer,'reason',plan->>'reason','replayed',false);
 INSERT INTO public.talk_record_cleanup_retry(record_id,actor_user_id,request_key,payload_hash,generation,reason,resources,response)
  VALUES(p_record,p_actor,p_key,p_hash,r.cleanup_generation+1,p_reason,resources,result);
 RETURN result;
END $$;

-- A directly recreated group_file row can revive a historical attachment binding.
CREATE FUNCTION public.fn_v5_group_file_reference_guard() RETURNS trigger
LANGUAGE plpgsql SET search_path = pg_catalog, public, pg_temp AS $$
DECLARE v_path text;
BEGIN
    FOR v_path IN SELECT a.path FROM public.attachment a WHERE a.group_file_id = NEW.id
        ORDER BY a.path COLLATE "C"
    LOOP
        PERFORM pg_advisory_xact_lock(hashtextextended(v_path, 168));
        PERFORM 1 FROM public.attachment a WHERE a.path = v_path FOR UPDATE;
        IF EXISTS (SELECT 1 FROM public.attachment a
                   WHERE a.path = v_path AND a.lifecycle_state <> 'live') THEN
            RAISE EXCEPTION 'v5 group file reference requires live attachment'
                USING ERRCODE = '23510';
        END IF;
    END LOOP;
    RETURN NEW;
END $$;

CREATE TRIGGER trg_v5_group_file_reference_guard BEFORE INSERT OR UPDATE OF id
    ON public.group_file FOR EACH ROW EXECUTE FUNCTION public.fn_v5_group_file_reference_guard();

CREATE FUNCTION public.fn_v5_enterprise_message_reference_guard() RETURNS trigger
LANGUAGE plpgsql SET search_path = pg_catalog, public, pg_temp AS $$
BEGIN
    IF NEW.message_id IS NOT NULL AND NEW.message_id IS DISTINCT FROM OLD.message_id
       AND (OLD.lifecycle_state <> 'live' OR NEW.lifecycle_state <> 'live') THEN
        RAISE EXCEPTION 'v5 enterprise message reference requires live asset'
            USING ERRCODE = '23510';
    END IF;
    RETURN NEW;
END $$;

CREATE TRIGGER trg_v5_enterprise_message_reference_guard BEFORE UPDATE OF message_id
    ON public.enterprise_asset FOR EACH ROW
    EXECUTE FUNCTION public.fn_v5_enterprise_message_reference_guard();

CREATE FUNCTION public.fn_v5_collect_reference_guard() RETURNS trigger
LANGUAGE plpgsql SET search_path = pg_catalog, public, pg_temp AS $$
DECLARE v_path text;
BEGIN
    IF NEW.status IS DISTINCT FROM 1 OR coalesce(NEW.attach_file_hash256, '') = '' THEN
        RETURN NEW;
    END IF;
    FOR v_path IN SELECT a.path FROM public.attachment a
        WHERE a.file_hash256 = ANY(string_to_array(NEW.attach_file_hash256, ','))
        ORDER BY a.path COLLATE "C"
    LOOP
        PERFORM pg_advisory_xact_lock(hashtextextended(v_path, 168));
        PERFORM 1 FROM public.attachment a WHERE a.path = v_path FOR UPDATE;
        IF EXISTS (SELECT 1 FROM public.attachment a
                   WHERE a.path = v_path AND a.lifecycle_state <> 'live') THEN
            RAISE EXCEPTION 'v5 collect reference requires live attachment'
                USING ERRCODE = '23510';
        END IF;
    END LOOP;
    RETURN NEW;
END $$;

CREATE TRIGGER trg_v5_collect_reference_guard
    BEFORE INSERT OR UPDATE OF attach_file_hash256, status ON public.user_collect
    FOR EACH ROW EXECUTE FUNCTION public.fn_v5_collect_reference_guard();

CREATE FUNCTION public.fn_v5_moment_media_reference_guard() RETURNS trigger
LANGUAGE plpgsql SET search_path = pg_catalog, public, pg_temp AS $$
DECLARE item jsonb; v_path text; v_state text; v_scope text; v_creator bigint;
        v_status smallint; v_scope_ref text;
BEGIN
    IF TG_OP = 'UPDATE' AND NEW.media IS NOT DISTINCT FROM OLD.media
       AND NEW.author_uid IS NOT DISTINCT FROM OLD.author_uid THEN
        RETURN NEW;
    END IF;
    IF jsonb_typeof(NEW.media) IS DISTINCT FROM 'array' THEN
        RAISE EXCEPTION 'v5 moment media must be an array' USING ERRCODE = '23510';
    END IF;
    FOR item IN SELECT value FROM jsonb_array_elements(NEW.media) LOOP
        IF jsonb_typeof(item) IS DISTINCT FROM 'object'
           OR jsonb_typeof(item->'url') IS DISTINCT FROM 'string'
           OR coalesce(item->>'url', '') = ''
           OR (item ? 'cover_url' AND jsonb_typeof(item->'cover_url') IS DISTINCT FROM 'string') THEN
            RAISE EXCEPTION 'v5 moment media path is invalid' USING ERRCODE = '23510';
        END IF;
    END LOOP;
    FOR v_path IN SELECT DISTINCT path COLLATE "C" AS path FROM (
        SELECT value->>'url' AS path FROM jsonb_array_elements(NEW.media)
        UNION ALL SELECT value->>'cover_url' FROM jsonb_array_elements(NEW.media)
    ) paths WHERE path IS NOT NULL AND path <> '' ORDER BY path LOOP
        PERFORM pg_advisory_xact_lock(hashtextextended(v_path, 168));
        SELECT lifecycle_state, scope, creator_user_id, status, scope_ref
            INTO v_state, v_scope, v_creator, v_status, v_scope_ref
            FROM public.attachment WHERE path=v_path FOR UPDATE;
        IF NOT FOUND OR v_state <> 'live' OR v_scope <> 'moment'
           OR v_creator <> NEW.author_uid OR v_status <> 1
           OR (v_scope_ref IS NOT NULL AND v_scope_ref <> NEW.id::text) THEN
            RAISE EXCEPTION 'v5 moment media requires an owned live attachment' USING ERRCODE = '23510';
        END IF;
    END LOOP;
    RETURN NEW;
END $$;

CREATE TRIGGER trg_v5_moment_media_reference_guard
    BEFORE INSERT OR UPDATE OF media, author_uid ON public.moment_post
    FOR EACH ROW EXECUTE FUNCTION public.fn_v5_moment_media_reference_guard();

-- The application inserts the post before binding scope_ref in the same transaction.
CREATE FUNCTION public.fn_v5_moment_binding_at_commit() RETURNS trigger
LANGUAGE plpgsql SET search_path = pg_catalog, public, pg_temp AS $$
DECLARE current_post public.moment_post%ROWTYPE; v_path text;
BEGIN
    SELECT * INTO current_post FROM public.moment_post WHERE id=NEW.id;
    IF NOT FOUND THEN RETURN NEW; END IF;
    FOR v_path IN SELECT DISTINCT path COLLATE "C" AS path FROM (
        SELECT value->>'url' AS path FROM jsonb_array_elements(current_post.media)
        UNION ALL SELECT value->>'cover_url' FROM jsonb_array_elements(current_post.media)
    ) paths WHERE path IS NOT NULL AND path <> '' ORDER BY path LOOP
        IF NOT EXISTS (SELECT 1 FROM public.attachment a WHERE a.path=v_path
            AND a.scope='moment' AND a.creator_user_id=current_post.author_uid
            AND a.lifecycle_state='live' AND a.status=1 AND a.scope_ref=current_post.id::text) THEN
            RAISE EXCEPTION 'v5 moment media binding incomplete' USING ERRCODE='23510';
        END IF;
    END LOOP;
    RETURN NEW;
END $$;
CREATE CONSTRAINT TRIGGER trg_v5_moment_binding_at_commit
    AFTER INSERT OR UPDATE OF media, author_uid ON public.moment_post
    DEFERRABLE INITIALLY DEFERRED FOR EACH ROW
    EXECUTE FUNCTION public.fn_v5_moment_binding_at_commit();

-- Both domains serialize on the physical key, even for legacy unknown locations.
CREATE FUNCTION public.fn_v5_cross_queue_guard() RETURNS trigger
LANGUAGE plpgsql SECURITY DEFINER SET search_path = pg_catalog, public, pg_temp AS $$
DECLARE v_key text;
BEGIN
    IF TG_TABLE_NAME = 'attachment_object_deletion' THEN
        v_key := coalesce(NEW.storage_physical_key, NEW.path);
        IF TG_OP = 'UPDATE' AND (NEW.path IS DISTINCT FROM OLD.path
           OR v_key IS DISTINCT FROM coalesce(OLD.storage_physical_key, OLD.path)) THEN
            RAISE EXCEPTION 'v5 delete job key immutable' USING ERRCODE='23510';
        END IF;
    ELSE
        v_key := coalesce(NEW.storage_physical_key, NEW.object_key);
        IF TG_OP = 'UPDATE' AND (NEW.object_key IS DISTINCT FROM OLD.object_key
           OR v_key IS DISTINCT FROM coalesce(OLD.storage_physical_key, OLD.object_key)) THEN
            RAISE EXCEPTION 'v5 delete job key immutable' USING ERRCODE='23510';
        END IF;
    END IF;
    IF v_key IS NULL OR v_key = '' THEN
        RAISE EXCEPTION 'v5 delete job missing physical key' USING ERRCODE='23510';
    END IF;
    PERFORM pg_advisory_xact_lock(hashtextextended(v_key, 1699));
    IF TG_TABLE_NAME = 'attachment_object_deletion' THEN
        IF EXISTS (SELECT 1 FROM public.enterprise_asset_delete_queue q
            WHERE coalesce(q.storage_physical_key, q.object_key) = v_key
              AND (q.storage_namespace IS NULL OR NEW.storage_namespace IS NULL
                   OR q.storage_namespace = NEW.storage_namespace)
              AND (q.storage_container IS NULL OR NEW.storage_container IS NULL
                   OR q.storage_container = NEW.storage_container)) THEN
            RAISE EXCEPTION 'v5 cross-domain physical deletion conflict' USING ERRCODE='23510';
        END IF;
    ELSE
        IF EXISTS (SELECT 1 FROM public.attachment_object_deletion q
            WHERE coalesce(q.storage_physical_key, q.path) = v_key
              AND (q.storage_namespace IS NULL OR NEW.storage_namespace IS NULL
                   OR q.storage_namespace = NEW.storage_namespace)
              AND (q.storage_container IS NULL OR NEW.storage_container IS NULL
                   OR q.storage_container = NEW.storage_container)) THEN
            RAISE EXCEPTION 'v5 cross-domain physical deletion conflict' USING ERRCODE='23510';
        END IF;
    END IF;
    RETURN NEW;
END $$;
ALTER FUNCTION public.fn_v5_cross_queue_guard() OWNER TO v5_fn_owner;
REVOKE ALL ON FUNCTION public.fn_v5_cross_queue_guard() FROM PUBLIC;
CREATE TRIGGER trg_v5_aod_cross_queue_guard
    BEFORE INSERT OR UPDATE OF path, storage_physical_key, storage_namespace, storage_container
    ON public.attachment_object_deletion FOR EACH ROW EXECUTE FUNCTION public.fn_v5_cross_queue_guard();
CREATE TRIGGER trg_v5_ebdq_cross_queue_guard
    BEFORE INSERT OR UPDATE OF object_key, storage_physical_key, storage_namespace, storage_container
    ON public.enterprise_asset_delete_queue FOR EACH ROW EXECUTE FUNCTION public.fn_v5_cross_queue_guard();

CREATE OR REPLACE FUNCTION public.fn_talk_record_retry_plan(p_record bigint) RETURNS jsonb
LANGUAGE plpgsql STABLE SET search_path=pg_catalog,public,pg_temp AS $$
DECLARE r public.talk_record%ROWTYPE; a public.attachment%ROWTYPE;
 q public.attachment_object_deletion%ROWTYPE; resources jsonb:='[]'; protected_count integer:=0; has_queue boolean;
BEGIN
 SELECT * INTO r FROM public.talk_record WHERE id=p_record;
 IF NOT FOUND OR r.deleted_at IS NULL THEN RETURN jsonb_build_object('error','record_not_deleted'); END IF;
 IF r.cleanup_status NOT IN ('failed','pending','retained_by_other_reference','blocked_hold','blocked_retention') THEN
  RETURN jsonb_build_object('error','cleanup_not_retryable'); END IF;
 IF r.retention_until>now() THEN RETURN jsonb_build_object('error','cleanup_protected'); END IF;
 IF EXISTS(SELECT 1 FROM public.talk_record_processor_run pr JOIN public.talk_record_job j ON j.id=pr.job_id
  WHERE j.record_id=p_record AND pr.state IN ('prepared','running','unknown')) THEN
  RETURN jsonb_build_object('error','cleanup_identity_unverified'); END IF;
 FOR a IN SELECT * FROM public.attachment WHERE id IN (
  SELECT attachment_id FROM public.talk_record_attachment_ref WHERE record_id=p_record
  UNION SELECT dw.attachment_id FROM public.attachment_derived_write dw
   JOIN public.talk_record_job j ON j.id=dw.source_job_id WHERE j.record_id=p_record)
  ORDER BY path LOOP
  IF EXISTS(SELECT 1 FROM public.attachment_derived_write WHERE attachment_id=a.id AND state<>'complete') THEN
   RETURN jsonb_build_object('error','cleanup_identity_unverified'); END IF;
  SELECT * INTO q FROM public.attachment_object_deletion WHERE path=a.path;
  has_queue:=FOUND;
  IF has_queue AND ((q.storage_driver,q.storage_namespace,q.storage_container,q.storage_locator_sha256,coalesce(q.storage_physical_key,q.path))
    IS DISTINCT FROM (a.storage_driver,a.storage_namespace,a.storage_container,a.storage_locator_sha256,coalesce(a.storage_physical_key,a.path))) THEN
   RETURN jsonb_build_object('error','cleanup_identity_unverified'); END IF;
  IF has_queue AND q.job_state='done' AND q.completed_at IS NOT NULL THEN CONTINUE; END IF;
  IF public.fn_talk_record_war_blocks(a.id)
   OR EXISTS(SELECT 1 FROM public.enterprise_attachment_retention WHERE attachment_id=a.id
     AND (hold_state='held' OR purge_state IS DISTINCT FROM 'purged' OR retention_until>now()))
   OR EXISTS(SELECT 1 FROM public.talk_record_attachment_ref WHERE attachment_id=a.id AND state='bound'
     AND (lease_until IS NULL OR lease_until>now()))
   OR EXISTS(SELECT 1 FROM public.attach_pending WHERE object_key=a.path AND (upload_expires_at IS NULL OR upload_expires_at>now()))
   OR EXISTS(SELECT 1 FROM public.group_file WHERE id=a.group_file_id)
   OR EXISTS(SELECT 1 FROM public.user_collect WHERE attach_file_hash256<>'' AND a.file_hash256=ANY(string_to_array(attach_file_hash256,',')) AND status=1)
   OR public.fn_v5_moment_reference_blocks(a.path) THEN
    protected_count:=protected_count+1; CONTINUE; END IF;
  IF NOT has_queue THEN RETURN jsonb_build_object('error','cleanup_identity_unverified'); END IF;
  IF a.lifecycle_state<>'delete_intent' OR NOT a.gc_verified
   OR (q.storage_driver,q.storage_namespace,q.storage_container,q.storage_locator_sha256,coalesce(q.storage_physical_key,q.path))
    IS DISTINCT FROM (a.storage_driver,a.storage_namespace,a.storage_container,a.storage_locator_sha256,coalesce(a.storage_physical_key,a.path))
   OR q.storage_driver IS NULL OR q.storage_namespace IS NULL OR q.storage_container IS NULL
   OR q.storage_locator_sha256 IS NULL
   OR NOT EXISTS(SELECT 1 FROM public.attachment_lifecycle_v5_audit i WHERE i.queue_domain='ordinary'
    AND i.object_key=a.path AND i.action='delete_intent' AND i.storage_namespace=a.storage_namespace
    AND i.details->>'attachment_id'=a.id::text AND i.details->>'locator'=a.storage_locator_sha256) THEN RETURN jsonb_build_object('error','cleanup_identity_unverified'); END IF;
  IF q.job_state='blocked' AND q.last_error_code='max_attempts' AND q.attempts-q.attempt_base>=8
    AND q.lease_token IS NULL AND q.lease_until IS NULL AND q.completed_at IS NULL THEN
   resources:=resources||jsonb_build_array(jsonb_build_object('path',q.path,'generation',q.retry_generation,
    'attempts',q.attempts,'locator',q.storage_locator_sha256));
  ELSIF q.job_state NOT IN ('ready','leased','retry_wait') THEN
   RETURN jsonb_build_object('error','cleanup_identity_unverified');
  END IF;
 END LOOP;

 IF jsonb_array_length(resources)=0 THEN
  RETURN jsonb_build_object('error',CASE WHEN protected_count>0 THEN 'cleanup_protected' ELSE 'cleanup_not_retryable' END);
 END IF;
 RETURN jsonb_build_object('resources',resources,'protected_resources',protected_count,
  'cleanup_status',CASE WHEN protected_count>0 THEN 'retained_by_other_reference' ELSE 'pending' END,
  'reason',CASE WHEN protected_count>0 THEN 'partial_protection' ELSE 'eligible' END);
END $$;
