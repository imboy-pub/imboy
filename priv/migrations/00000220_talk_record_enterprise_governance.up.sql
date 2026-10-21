-- Real enterprise talk-record source; no synthetic enterprise_application.
ALTER TABLE talk_record ADD CONSTRAINT uq_talk_record_org_id UNIQUE(organization_id,id);
ALTER TABLE enterprise_attachment_retention
 ADD COLUMN source_kind text NOT NULL DEFAULT 'application',
 ADD COLUMN talk_record_id bigint,
 ALTER COLUMN application_id DROP NOT NULL,
 ADD CONSTRAINT ck_ear_source CHECK (
  (source_kind='application' AND application_id IS NOT NULL AND talk_record_id IS NULL) OR
  (source_kind='talk_record' AND application_id IS NULL AND talk_record_id IS NOT NULL)),
 ADD CONSTRAINT fk_ear_talk_record FOREIGN KEY(organization_id,talk_record_id)
  REFERENCES talk_record(organization_id,id) ON DELETE RESTRICT;
CREATE INDEX i_ear_talk_record ON enterprise_attachment_retention(talk_record_id) WHERE source_kind='talk_record';
ALTER TABLE workspace_attachment_retention DROP CONSTRAINT workspace_attachment_retention_scope_check;
ALTER TABLE workspace_attachment_retention ADD CONSTRAINT workspace_attachment_retention_scope_check
 CHECK(scope IN ('group','channel','talk_record'));

CREATE FUNCTION fn_talk_record_governance_source_guard() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
 IF TG_TABLE_NAME='enterprise_attachment_retention' THEN
 IF NEW.source_kind='talk_record' THEN
  IF NOT EXISTS(SELECT 1 FROM talk_record r JOIN attachment a ON a.id=NEW.attachment_id
   WHERE r.id=NEW.talk_record_id AND r.scope='organization' AND r.organization_id=NEW.organization_id
    AND a.scope='talk_record' AND a.scope_ref=r.id::text) THEN
   RAISE EXCEPTION 'talk record EAR source mismatch' USING ERRCODE='23514';
  END IF;
 END IF;
 ELSIF TG_TABLE_NAME='workspace_attachment_retention' THEN
 IF NEW.scope='talk_record' THEN
  IF NOT EXISTS(SELECT 1 FROM talk_record r JOIN enterprise_attachment_retention e ON e.talk_record_id=r.id
   WHERE r.id::text=NEW.scope_ref AND r.organization_id=NEW.organization_id AND r.workspace_id=NEW.workspace_id
    AND e.attachment_id=NEW.attachment_id AND e.source_kind='talk_record' AND e.organization_id=NEW.organization_id) THEN
   RAISE EXCEPTION 'talk record WAR source mismatch' USING ERRCODE='23514';
  END IF;
 END IF;
 END IF;
 RETURN NEW;
END $$;
CREATE TRIGGER trg_talk_record_ear_source BEFORE INSERT OR UPDATE ON enterprise_attachment_retention
 FOR EACH ROW EXECUTE FUNCTION fn_talk_record_governance_source_guard();
CREATE TRIGGER trg_talk_record_war_source BEFORE INSERT ON workspace_attachment_retention
 FOR EACH ROW EXECUTE FUNCTION fn_talk_record_governance_source_guard();

CREATE FUNCTION fn_talk_record_governed_identity_guard() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
 IF TG_TABLE_NAME='attachment' THEN
  IF ROW(NEW.scope,NEW.scope_ref) IS DISTINCT FROM ROW(OLD.scope,OLD.scope_ref)
   AND EXISTS(SELECT 1 FROM enterprise_attachment_retention e WHERE e.attachment_id=OLD.id AND e.source_kind='talk_record') THEN
   RAISE EXCEPTION 'governed talk attachment scope is immutable' USING ERRCODE='23514';
  END IF;
 ELSIF TG_TABLE_NAME='talk_record' THEN
  IF ROW(NEW.scope,NEW.organization_id,NEW.workspace_id) IS DISTINCT FROM ROW(OLD.scope,OLD.organization_id,OLD.workspace_id)
   AND EXISTS(SELECT 1 FROM enterprise_attachment_retention e WHERE e.talk_record_id=OLD.id AND e.source_kind='talk_record') THEN
   RAISE EXCEPTION 'governed talk record attribution is immutable' USING ERRCODE='23514';
  END IF;
 END IF;
 RETURN NEW;
END $$;
CREATE TRIGGER trg_talk_record_governed_asset BEFORE UPDATE OF scope,scope_ref ON attachment
 FOR EACH ROW EXECUTE FUNCTION fn_talk_record_governed_identity_guard();
CREATE TRIGGER trg_talk_record_governed_record BEFORE UPDATE OF scope,organization_id,workspace_id ON talk_record
 FOR EACH ROW EXECUTE FUNCTION fn_talk_record_governed_identity_guard();

-- WAR remains immutable audit evidence. Only exact, explicitly purged talk EAR releases its GC hold.
CREATE FUNCTION fn_talk_record_war_blocks(p_attachment_id bigint) RETURNS boolean
 LANGUAGE sql STABLE SET search_path=public AS $$
 SELECT EXISTS(SELECT 1 FROM workspace_attachment_retention w WHERE w.attachment_id=p_attachment_id
  AND (w.scope<>'talk_record' OR NOT EXISTS(SELECT 1 FROM enterprise_attachment_retention e
   WHERE e.attachment_id=w.attachment_id AND e.organization_id=w.organization_id
    AND e.source_kind='talk_record' AND e.talk_record_id::text=w.scope_ref AND e.purge_state='purged')))
$$;
GRANT EXECUTE ON FUNCTION fn_talk_record_war_blocks(bigint) TO v5_app_runtime,v5_fn_owner,v5_worker;
CREATE OR REPLACE FUNCTION fn_efapi_attachment_retention_guard() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF TG_OP = 'DELETE' THEN
        RAISE EXCEPTION 'enterprise_attachment_retention 行禁止物理删除（purge 走 purge_state=purged）'
            USING ERRCODE = '23514';
    END IF;

    IF OLD.purge_state = 'purged' AND NEW.purge_state <> 'purged' THEN
        RAISE EXCEPTION 'purged 为终态，不可回退（% -> %）', OLD.purge_state, NEW.purge_state
            USING ERRCODE = '23514';
    END IF;

    IF OLD.purge_state <> 'purged' AND NEW.purge_state = 'purged' THEN
        IF OLD.hold_state = 'held' THEN
            RAISE EXCEPTION '法务 hold 生效中禁止 purge（attachment_id=%）', OLD.attachment_id
                USING ERRCODE = '23514';
        END IF;
        IF CURRENT_TIMESTAMP < OLD.retention_until THEN
            RAISE EXCEPTION 'retention 窗口未满禁止 purge（attachment_id=% until=%）',
                OLD.attachment_id, OLD.retention_until
                USING ERRCODE = '23514';
        END IF;
    END IF;

    IF NEW.retention_until < OLD.retention_until THEN
        RAISE EXCEPTION 'retention_until 只可延长（% -> %）', OLD.retention_until, NEW.retention_until
            USING ERRCODE = '23514';
    END IF;

    IF ROW(NEW.attachment_id,NEW.organization_id,NEW.application_id,NEW.source_kind,NEW.talk_record_id)
       IS DISTINCT FROM ROW(OLD.attachment_id,OLD.organization_id,OLD.application_id,OLD.source_kind,OLD.talk_record_id) THEN
        RAISE EXCEPTION '治理行的 (attachment_id, organization_id, application_id) 不可变更'
            USING ERRCODE = '23514';
    END IF;

    RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS trg_efapi_attachment_retention_guard ON enterprise_attachment_retention;
CREATE TRIGGER trg_efapi_attachment_retention_guard
    BEFORE UPDATE OR DELETE ON enterprise_attachment_retention
    FOR EACH ROW EXECUTE FUNCTION fn_efapi_attachment_retention_guard();

CREATE OR REPLACE FUNCTION fn_v5_intent_ordinary_delete(p_path text, p_reason text)
RETURNS void LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
DECLARE v_a attachment%ROWTYPE; v_p attach_pending%ROWTYPE; v_bucket text;
BEGIN
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
               WHERE uc.attach_file_hash256 <> '' AND uc.attach_file_hash256 = v_a.file_hash256
                 AND uc.status = 1) THEN
        RAISE EXCEPTION 'v5 intent blocked by collect reference'
            USING ERRCODE = '23510';
    END IF;
    IF EXISTS (SELECT 1 FROM moment_post mp
               WHERE position(v_a.path in mp.media::text) > 0) THEN
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
         storage_locator_sha256, job_state, next_retry_at, last_error_code)
    VALUES (v_a.path, v_a.url, v_a.storage_driver, v_a.storage_namespace,
            v_a.storage_container, v_a.storage_locator_sha256,
            'ready', now(), NULL);
    INSERT INTO attachment_lifecycle_v5_audit
        (queue_domain, object_key, storage_namespace, action, reason, actor, details)
    VALUES ('ordinary', v_a.path, v_a.storage_namespace, 'delete_intent',
            p_reason, current_user,
            jsonb_build_object('attachment_id', v_a.id,
                               'locator', v_a.storage_locator_sha256));
END;
$$;

ALTER FUNCTION fn_v5_intent_ordinary_delete(text, text) OWNER TO v5_fn_owner;
REVOKE ALL ON FUNCTION fn_v5_intent_ordinary_delete(text, text) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION fn_v5_intent_ordinary_delete(text, text) TO v5_app_runtime;

-- 锁后复核新增读取的引用表授权（SECURITY DEFINER 以 v5_fn_owner 执行；对齐 185 惯例）
GRANT SELECT ON public.talk_record_attachment_ref TO v5_fn_owner;

CREATE OR REPLACE FUNCTION fn_v5_intent_purged_delete(p_path text, p_reason text)
RETURNS void LANGUAGE plpgsql SECURITY DEFINER SET search_path = public AS $$
DECLARE v_a attachment%ROWTYPE; v_ready boolean;
BEGIN
    PERFORM fn_v5_lock_ordinary_upload(p_path);
    SELECT * INTO v_a FROM attachment WHERE path = p_path FOR UPDATE;
    IF NOT FOUND THEN
        RAISE EXCEPTION 'v5 purged intent unknown attachment' USING ERRCODE = 'P0002';
    END IF;
    IF v_a.lifecycle_state <> 'live' THEN
        RAISE EXCEPTION 'v5 purged intent non-live asset' USING ERRCODE = '23510';
    END IF;
    IF NOT EXISTS (
        SELECT 1 FROM enterprise_attachment_retention ear
        WHERE ear.attachment_id = v_a.id AND ear.purge_state = 'purged'
    ) THEN
        RAISE EXCEPTION 'v5 purged intent requires EAR purge terminal state'
            USING ERRCODE = '23510';
    END IF;
    -- Talk governance approval does not consume live content refs or upload capabilities.
    IF EXISTS(SELECT 1 FROM enterprise_attachment_retention e WHERE e.attachment_id=v_a.id
              AND e.source_kind='talk_record') THEN
        PERFORM fn_v5_intent_ordinary_delete(p_path,p_reason);
        RETURN;
    END IF;
    IF EXISTS (SELECT 1 FROM attachment_object_deletion WHERE path = p_path) THEN
        RAISE EXCEPTION 'v5 intent job already exists (tombstone)'
            USING ERRCODE = '23505';
    END IF;
    PERFORM fn_v5_lock_identity(v_a.storage_driver, v_a.storage_namespace,
                                v_a.storage_container, v_a.path);
    PERFORM pg_advisory_xact_lock(hashtextextended(v_a.path, 168));
    IF EXISTS (SELECT 1 FROM enterprise_asset_delete_queue q
               WHERE q.storage_namespace = v_a.storage_namespace
                 AND q.storage_container = v_a.storage_container
                 AND q.object_key = v_a.path) THEN
        RAISE EXCEPTION 'v5 intent cross-domain identity conflict'
            USING ERRCODE = '23510';
    END IF;
    UPDATE attachment SET lifecycle_state = 'delete_intent'
        WHERE id = v_a.id AND lifecycle_state = 'live';
    IF NOT FOUND THEN
        RAISE EXCEPTION 'v5 purged intent lost eligibility race' USING ERRCODE = '40001';
    END IF;
    v_ready := v_a.storage_driver IS NOT NULL AND v_a.storage_namespace IS NOT NULL
        AND v_a.storage_container IS NOT NULL
        AND v_a.storage_locator_sha256 ~ '^[0-9a-f]{64}$';
    INSERT INTO attachment_object_deletion
        (path, bucket, storage_driver, storage_namespace, storage_container,
         storage_locator_sha256, job_state, next_retry_at, last_error_code)
    VALUES (v_a.path, v_a.url, v_a.storage_driver, v_a.storage_namespace,
            v_a.storage_container, v_a.storage_locator_sha256,
            CASE WHEN v_ready THEN 'ready' ELSE 'blocked' END,
            CASE WHEN v_ready THEN now() ELSE now() END,
            CASE WHEN v_ready THEN NULL ELSE 'legacy_location_unverified' END);
    INSERT INTO attachment_lifecycle_v5_audit
        (queue_domain, object_key, storage_namespace, action, reason, actor, details)
    VALUES ('ordinary', v_a.path, v_a.storage_namespace, 'delete_intent',
            p_reason, current_user,
            jsonb_build_object('attachment_id', v_a.id, 'purge_domain', 'ear',
                               'job_state', CASE WHEN v_ready THEN 'ready'
                                                  ELSE 'blocked' END));
END;
$$;

-- Historical organization artifacts require the approved instance policy, pinned to original creation.
-- Supply imboy.talk_record_backfill_retention_days (1..36500); no implicit deployment default.
-- A source that cannot be attributed is not silently adopted or downgraded.
DO $$ BEGIN
 IF EXISTS(SELECT 1 FROM attachment a LEFT JOIN talk_record r ON a.scope_ref=r.id::text
  WHERE a.scope='talk_record' AND r.id IS NULL) THEN
  RAISE EXCEPTION 'unattributed talk record attachments require reconciliation' USING ERRCODE='23514';
 END IF;
END $$;
DO $$ DECLARE policy_days text;
BEGIN
 IF EXISTS(SELECT 1 FROM attachment a JOIN talk_record r ON a.scope_ref=r.id::text
  WHERE a.scope='talk_record' AND r.scope='organization') THEN
  policy_days:=current_setting('imboy.talk_record_backfill_retention_days',true);
  IF policy_days IS NULL OR policy_days !~ '^[1-9][0-9]{0,4}$' THEN
   RAISE EXCEPTION 'approved talk record backfill retention days required' USING ERRCODE='23514';
  END IF;
  IF policy_days::integer>36500 THEN
   RAISE EXCEPTION 'talk record backfill retention days out of range' USING ERRCODE='23514';
  END IF;
 END IF;
END $$;
INSERT INTO enterprise_attachment_retention(attachment_id,organization_id,application_id,source_kind,
 talk_record_id,retention_until,hold_state,purge_state)
 SELECT a.id,r.organization_id,NULL,'talk_record',r.id,
 greatest(a.created_at+make_interval(days=>nullif(current_setting('imboy.talk_record_backfill_retention_days',true),'')::integer),coalesce(r.retention_until,'-infinity')),'none','live'
 FROM attachment a JOIN talk_record r ON a.scope_ref=r.id::text
 WHERE a.scope='talk_record' AND r.scope='organization'
 ON CONFLICT(attachment_id) DO NOTHING;
INSERT INTO workspace_attachment_retention(attachment_id,organization_id,workspace_id,scope,scope_ref)
 SELECT e.attachment_id,r.organization_id,r.workspace_id,'talk_record',r.id::text
 FROM enterprise_attachment_retention e JOIN talk_record r ON r.id=e.talk_record_id
 WHERE e.source_kind='talk_record' AND r.workspace_id IS NOT NULL
 ON CONFLICT(attachment_id) DO NOTHING;

DO $$ BEGIN
 IF EXISTS(SELECT 1 FROM attachment a JOIN talk_record r ON a.scope_ref=r.id::text
  WHERE a.scope='talk_record' AND r.scope='organization' AND NOT EXISTS(
   SELECT 1 FROM enterprise_attachment_retention e WHERE e.attachment_id=a.id
    AND e.organization_id=r.organization_id AND e.source_kind='talk_record' AND e.talk_record_id=r.id))
 OR EXISTS(SELECT 1 FROM enterprise_attachment_retention e JOIN talk_record r ON r.id=e.talk_record_id
  WHERE e.source_kind='talk_record' AND r.workspace_id IS NOT NULL AND NOT EXISTS(
   SELECT 1 FROM workspace_attachment_retention w WHERE w.attachment_id=e.attachment_id
    AND w.organization_id=r.organization_id AND w.workspace_id=r.workspace_id
    AND w.scope='talk_record' AND w.scope_ref=r.id::text)) THEN
  RAISE EXCEPTION 'conflicting legacy talk governance facts require reconciliation' USING ERRCODE='23514';
 END IF;
END $$;
