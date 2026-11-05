-- Accepted forward references are inserted in the same transaction as the message.
CREATE TABLE public.forward_attachment_reference (
    object_key text NOT NULL REFERENCES public.attachment(path) ON DELETE CASCADE,
    source_type text NOT NULL CHECK (source_type IN ('c2c','c2g')),
    source_msg_id text NOT NULL,
    target_type text NOT NULL CHECK (target_type IN ('c2c','c2g')),
    target_msg_id text NOT NULL,
    target_sender bigint NOT NULL,
    target_id bigint NOT NULL,
    target_seq bigint NOT NULL CHECK (target_seq > 0),
    PRIMARY KEY (target_type, target_msg_id, object_key)
);
CREATE INDEX forward_attachment_reference_key_idx ON public.forward_attachment_reference(object_key);
-- Runtime must not forge a reference and thereby grant itself attachment access.
GRANT SELECT ON public.forward_attachment_reference TO v5_app_runtime;

-- Group edit acknowledgements do not rewrite the original formal payload.
-- Keep a durable fact; old content is no longer authoritative for reading,
-- while GC retains it until a revoke or message expiry proves it releasable.
CREATE TABLE public.c2g_forward_edit_fact (
  group_id bigint NOT NULL,
  original_msg_id text NOT NULL,
  action_msg_id text PRIMARY KEY,
  actor_uid bigint NOT NULL,
  accepted_seq bigint NOT NULL
);
CREATE INDEX c2g_forward_edit_fact_target_idx ON public.c2g_forward_edit_fact(group_id,original_msg_id);
CREATE FUNCTION public.fn_c2g_forward_edit_capture() RETURNS trigger
LANGUAGE plpgsql SECURITY DEFINER SET search_path=pg_catalog,public,pg_temp AS $$
DECLARE v_original text;
BEGIN
  IF NEW.type='c2g' AND NEW.action IN ('message_edit_ack','message_edit') THEN
    -- The encrypted path authenticates e2ee.edit_of, never the untrusted
    -- message payload. Plain acknowledgements carry the accepted original ID.
    v_original := CASE NEW.action
      WHEN 'message_edit' THEN NEW.e2ee->>'edit_of'
      WHEN 'message_edit_ack' THEN coalesce(NEW.payload->'payload'->>'original_msg_id',NEW.payload->>'original_msg_id')
      ELSE NULL END;
    IF v_original IS NULL OR v_original='' OR NEW.to_id IS NULL OR NEW.from_id IS NULL OR NEW.conv_seq IS NULL THEN
      RAISE EXCEPTION 'group edit fact missing trusted identity' USING ERRCODE='23514';
    END IF;
    PERFORM pg_advisory_xact_lock(hashtextextended('c2g_forward_revoke:'||NEW.to_id::text||':'||v_original,0));
    INSERT INTO public.c2g_forward_edit_fact(group_id,original_msg_id,action_msg_id,actor_uid,accepted_seq)
      VALUES(NEW.to_id,v_original,NEW.msg_id,NEW.from_id,NEW.conv_seq);
  END IF;
  RETURN NEW;
END $$;
REVOKE ALL ON FUNCTION public.fn_c2g_forward_edit_capture() FROM PUBLIC;
CREATE TRIGGER c2g_forward_edit_capture AFTER INSERT ON public.msg_store_staging
FOR EACH ROW EXECUTE FUNCTION public.fn_c2g_forward_edit_capture();
GRANT SELECT ON public.c2g_forward_edit_fact TO v5_app_runtime;

-- A reference remains live only while the durable message still contains it.
CREATE FUNCTION public.fn_forward_payload_contains(p_payload text, p_key text) RETURNS boolean
LANGUAGE plpgsql IMMUTABLE SECURITY DEFINER SET search_path=pg_catalog,public,pg_temp AS $$
DECLARE v_payload jsonb;
BEGIN
  IF p_payload IS NULL OR p_key IS NULL THEN RETURN NULL; END IF;
  BEGIN v_payload := p_payload::jsonb;
  EXCEPTION WHEN invalid_text_representation THEN RETURN NULL; END;
  IF jsonb_typeof(v_payload) <> 'object' THEN RETURN NULL; END IF;
  RETURN jsonb_path_exists(v_payload, '$.**.uri ? (@ == $key)', jsonb_build_object('key',p_key))
      OR jsonb_path_exists(v_payload, '$.**.url ? (@ == $key)', jsonb_build_object('key',p_key))
      OR jsonb_path_exists(v_payload, '$.**.thumb ? (@ == $key)', jsonb_build_object('key',p_key));
END $$;
REVOKE ALL ON FUNCTION public.fn_forward_payload_contains(text,text) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION public.fn_forward_payload_contains(text,text) TO v5_app_runtime, v5_fn_owner;

-- Missing key is a proven release only when an accepted, consumed edit
-- exactly matches this message, actor, peer, and current durable payload.
CREATE FUNCTION public.fn_forward_c2c_gc_blocks(
  p_payload text, p_key text, p_msg text, p_sender bigint, p_peer bigint
) RETURNS boolean LANGUAGE plpgsql STABLE SECURITY DEFINER SET search_path=pg_catalog,public,pg_temp AS $$
DECLARE v_contains boolean;
BEGIN
  v_contains := public.fn_forward_payload_contains(p_payload,p_key);
  IF v_contains IS DISTINCT FROM false THEN RETURN true; END IF;
  RETURN NOT EXISTS (SELECT 1 FROM public.c2c_action_receipt x
    WHERE x.original_msg_id=p_msg AND x.actor_uid=p_sender AND x.peer_uid=p_peer
      AND x.action='edit' AND x.consumed_at IS NOT NULL AND x.result_payload=p_payload::jsonb);
END $$;
REVOKE ALL ON FUNCTION public.fn_forward_c2c_gc_blocks(text,text,text,bigint,bigint) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION public.fn_forward_c2c_gc_blocks(text,text,text,bigint,bigint) TO v5_app_runtime,v5_fn_owner;

-- Read authorization and GC share the same live-message predicate.
CREATE FUNCTION public.fn_forward_attachment_target_live(
    p_type text, p_msg text, p_sender bigint, p_target bigint, p_seq bigint, p_key text, p_gc boolean
) RETURNS boolean LANGUAGE sql STABLE SECURITY DEFINER SET search_path=pg_catalog,public,pg_temp AS $$
SELECT CASE p_type
    WHEN 'c2c' THEN NOT EXISTS (
        SELECT 1 FROM public.c2c_action_receipt x WHERE x.original_msg_id=p_msg AND x.action='revoke'
    ) AND (
        EXISTS (SELECT 1 FROM public.msg_store_staging s
            WHERE s.type='c2c' AND s.msg_id=p_msg AND s.from_id=p_sender AND s.to_id=p_target
              AND s.conv_seq=p_seq AND (s.processed_at IS NULL OR s.error_msg IS NOT NULL)
              AND (CASE WHEN p_gc AND s.error_msg IS NOT NULL THEN true
                        WHEN p_gc THEN public.fn_forward_c2c_gc_blocks(s.payload::text,p_key,p_msg,p_sender,p_target)
                        ELSE public.fn_forward_payload_contains(s.payload::text,p_key) IS TRUE END)
              AND (s.expire_at IS NULL OR s.expire_at>clock_timestamp()))
        OR EXISTS (SELECT 1 FROM public.msg_c2c m
            WHERE m.msg_id=p_msg AND m.from_id=p_sender AND m.to_id=p_target
              AND m.conv_seq=p_seq AND (CASE WHEN p_gc THEN public.fn_forward_c2c_gc_blocks(m.payload,p_key,p_msg,p_sender,p_target)
                        ELSE public.fn_forward_payload_contains(m.payload,p_key) IS TRUE END)
              AND (m.expire_at IS NULL OR m.expire_at>clock_timestamp()))
    )
    WHEN 'c2g' THEN NOT EXISTS (
        SELECT 1 FROM public.c2g_revoke_fact x WHERE x.group_id=p_target AND x.original_msg_id=p_msg
    ) AND (
        EXISTS (SELECT 1 FROM public.msg_store_staging s
            WHERE s.type='c2g' AND s.msg_id=p_msg AND s.from_id=p_sender AND s.to_id=p_target
              AND s.conv_seq=p_seq AND (s.processed_at IS NULL OR s.error_msg IS NOT NULL)
              AND (CASE WHEN p_gc AND s.error_msg IS NOT NULL THEN true
                        WHEN EXISTS (SELECT 1 FROM public.c2g_forward_edit_fact e
                        WHERE e.group_id=p_target AND e.original_msg_id=p_msg) THEN p_gc
                        WHEN p_gc THEN public.fn_forward_payload_contains(s.payload::text,p_key) IS NOT FALSE
                        ELSE public.fn_forward_payload_contains(s.payload::text,p_key) IS TRUE END)
              AND (s.expire_at IS NULL OR s.expire_at>clock_timestamp()))
        OR EXISTS (SELECT 1 FROM public.msg_c2g m
            JOIN public.msg_c2g_recipient_snapshot snap
              ON snap.msg_id=m.msg_id AND snap.from_id=m.from_id AND snap.to_gid=m.to_id
            WHERE m.msg_id=p_msg AND m.from_id=p_sender AND m.to_id=p_target
              AND snap.conv_seq=p_seq AND (CASE WHEN EXISTS (SELECT 1 FROM public.c2g_forward_edit_fact e
                        WHERE e.group_id=p_target AND e.original_msg_id=p_msg) THEN p_gc
                        WHEN p_gc THEN public.fn_forward_payload_contains(m.payload::text,p_key) IS NOT FALSE
                        ELSE public.fn_forward_payload_contains(m.payload::text,p_key) IS TRUE END)
              AND (m.expire_at IS NULL OR m.expire_at>clock_timestamp()))
    )
    ELSE false END
$$;
REVOKE ALL ON FUNCTION public.fn_forward_attachment_target_live(text,text,bigint,bigint,bigint,text,boolean) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION public.fn_forward_attachment_target_live(text,text,bigint,bigint,bigint,text,boolean) TO v5_app_runtime, v5_fn_owner;

CREATE FUNCTION public.fn_v5_forward_reference_blocks(p_key text) RETURNS boolean
LANGUAGE sql STABLE SECURITY DEFINER SET search_path=pg_catalog,public,pg_temp AS $$
SELECT EXISTS (SELECT 1 FROM public.forward_attachment_reference r WHERE r.object_key=p_key
    AND public.fn_forward_attachment_target_live(r.target_type,r.target_msg_id,r.target_sender,r.target_id,r.target_seq,r.object_key,true))
$$;
REVOKE ALL ON FUNCTION public.fn_v5_forward_reference_blocks(text) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION public.fn_v5_forward_reference_blocks(text) TO v5_app_runtime, v5_fn_owner;

-- Read is restricted to the accepted audience and live parent entitlements.
CREATE FUNCTION public.fn_forward_attachment_authorize(p_key text, p_uid bigint, p_discussion boolean)
RETURNS boolean LANGUAGE sql STABLE SECURITY DEFINER SET search_path=pg_catalog,public,pg_temp AS $$
SELECT EXISTS (
  SELECT 1 FROM public.forward_attachment_reference r
  WHERE r.object_key=p_key
    AND public.fn_forward_attachment_target_live(r.target_type,r.target_msg_id,r.target_sender,r.target_id,r.target_seq,r.object_key,false)
    AND (
      (r.target_type='c2c' AND (p_uid=r.target_sender OR p_uid=r.target_id)
       AND EXISTS (SELECT 1 FROM public.c2c_forward_source_fact f
          WHERE f.msg_id=r.target_msg_id AND f.sender_id=r.target_sender
            AND f.recipient_id=r.target_id AND f.conv_seq=r.target_seq))
      OR
      (r.target_type='c2g' AND EXISTS (
        SELECT 1 FROM public.c2g_forward_source_fact f
        JOIN public.msg_c2g_recipient_snapshot s ON s.msg_id=f.msg_id AND s.to_gid=f.group_id
          AND s.from_id=f.sender_id AND s.conv_seq=f.conv_seq
        JOIN public."group" grp ON grp.id=f.group_id AND grp.status=1
        JOIN public.group_member gm ON gm.group_id=grp.id AND gm.user_id=p_uid AND gm.status=1
        JOIN public.group_member_generation g ON g.group_id=grp.id AND g.user_id=p_uid
          AND g.end_seq IS NULL AND g.start_seq<=f.conv_seq
        WHERE f.msg_id=r.target_msg_id AND f.group_id=r.target_id AND f.sender_id=r.target_sender
          AND f.conv_seq=r.target_seq AND p_uid=ANY(s.recipient_uids)
          AND ((grp.scope='personal' AND grp.workspace_id IS NULL) OR
            (grp.scope='workspace' AND EXISTS (
              SELECT 1 FROM public.workspace w
              LEFT JOIN public.organization o ON o.id=w.organization_id
              LEFT JOIN public.organization_member om ON om.organization_id=o.id AND om.user_id=p_uid AND om.status='active'
              WHERE w.id=grp.workspace_id AND w.status IN ('active','archived')
                AND (w.organization_id IS NULL OR (o.status='active' AND om.user_id=p_uid))
                AND (EXISTS (SELECT 1 FROM public.workspace_member wm
                    WHERE wm.workspace_id=w.id AND wm.user_id=p_uid AND wm.status='active')
                    OR om.role IN ('owner','admin')))))
          AND (NOT EXISTS (SELECT 1 FROM public.channel mc WHERE mc.discussion_group_id=grp.id)
            OR (p_discussion AND EXISTS (
              SELECT 1 FROM public.channel mc JOIN public."group" mg ON mg.id=mc.discussion_group_id
              WHERE mc.discussion_group_id=grp.id AND mc.status=1 AND mc.scope='personal'
                AND mg.scope='personal' AND mg.owner_uid=mc.creator_uid AND mg.status=1
                AND mg.type=2 AND mg.join_limit=3 AND mg.e2ee_mode=0
                AND ((COALESCE(NULLIF((SELECT ma.role FROM public.channel_admin ma
                       WHERE ma.channel_id=mc.id AND ma.user_id=p_uid LIMIT 1),0),
                       CASE WHEN mc.creator_uid=p_uid THEN 3 ELSE 0 END)>=2
                     AND (mg.owner_uid=p_uid OR EXISTS (SELECT 1 FROM public.group_member mm
                          WHERE mm.group_id=mg.id AND mm.user_id=p_uid AND mm.status=1 AND mm.role>=3)))
                  OR (EXISTS (SELECT 1 FROM public.channel_subscription ms
                        WHERE ms.channel_id=mc.id AND ms.user_id=p_uid AND ms.status=1)
                     AND (mc.access_type<>1 OR EXISTS (SELECT 1 FROM public.channel_order mo
                          WHERE mo.channel_id=mc.id AND mo.user_id=p_uid AND mo.status=1
                            AND (mo.subscription_end_at IS NULL OR mo.subscription_end_at>clock_timestamp()))))))))
      ))
    )
)
$$;
REVOKE ALL ON FUNCTION public.fn_forward_attachment_authorize(text,bigint,boolean) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION public.fn_forward_attachment_authorize(text,bigint,boolean) TO v5_app_runtime;

CREATE FUNCTION public.fn_forward_attachment_lifecycle_guard() RETURNS trigger
LANGUAGE plpgsql SECURITY DEFINER SET search_path=pg_catalog,public,pg_temp AS $$
BEGIN
    IF (TG_OP='DELETE' OR (OLD.lifecycle_state='live' AND NEW.lifecycle_state <> 'live'))
       AND public.fn_v5_forward_reference_blocks(OLD.path) THEN
        RAISE EXCEPTION 'live forwarded attachment reference' USING ERRCODE='23510';
    END IF;
    IF TG_OP='DELETE' THEN RETURN OLD; END IF;
    RETURN NEW;
END $$;
REVOKE ALL ON FUNCTION public.fn_forward_attachment_lifecycle_guard() FROM PUBLIC;
CREATE TRIGGER forward_attachment_lifecycle_guard BEFORE UPDATE OF lifecycle_state OR DELETE
ON public.attachment FOR EACH ROW EXECUTE FUNCTION public.fn_forward_attachment_lifecycle_guard();

-- Validate and lock a C2C source without exposing its row to the runtime role.
-- The backend supplies p_uid from the authenticated server context.
CREATE FUNCTION public.fn_forward_source_c2c_validate(
    p_msg text, p_uid bigint, p_from bigint, p_to bigint, p_type text, p_payload jsonb
) RETURNS boolean LANGUAGE plpgsql SECURITY DEFINER SET search_path=pg_catalog,public,pg_temp AS $$
DECLARE v_allowed boolean; v_count bigint;
BEGIN
    IF p_uid IS NULL OR p_uid <= 0 OR p_uid NOT IN (p_from,p_to) OR p_payload IS NULL
       OR jsonb_typeof(p_payload) <> 'object' THEN RETURN false; END IF;
    PERFORM pg_advisory_xact_lock(hashtextextended('c2c_forward_revoke:' || p_msg,0));
    PERFORM 1 FROM public.msg_c2c m
      WHERE m.msg_id=p_msg AND m.from_id=p_from AND m.to_id=p_to
        AND m.msg_type=p_type AND (m.e2ee IS NULL OR m.e2ee='{}'::jsonb)
        AND m.payload::jsonb=p_payload
      FOR SHARE;
    GET DIAGNOSTICS v_count = ROW_COUNT;
    IF v_count <> 1 THEN RETURN false; END IF;
    SELECT EXISTS (SELECT 1 FROM public.c2c_forward_source_fact f
      JOIN public.msg_c2c m ON m.msg_id=f.msg_id AND m.from_id=f.sender_id
        AND m.to_id=f.recipient_id AND m.conv_seq=f.conv_seq
      WHERE f.msg_id=p_msg AND f.sender_id=p_from AND f.recipient_id=p_to
        AND (m.expire_at IS NULL OR m.expire_at > clock_timestamp())
        AND NOT EXISTS (SELECT 1 FROM public.c2c_action_receipt r
          WHERE r.original_msg_id=p_msg AND r.action='revoke')) INTO v_allowed;
    RETURN v_allowed;
END $$;
REVOKE ALL ON FUNCTION public.fn_forward_source_c2c_validate(text,bigint,bigint,bigint,text,jsonb) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION public.fn_forward_source_c2c_validate(text,bigint,bigint,bigint,text,jsonb) TO v5_app_runtime;

-- Group source validation holds the same parent and generation locks as the
-- message writer. Historical messages without recipient facts fail closed.
CREATE FUNCTION public.fn_forward_source_c2g_validate(
    p_msg text, p_uid bigint, p_from bigint, p_gid bigint, p_type text,
    p_payload jsonb, p_discussion boolean
) RETURNS boolean LANGUAGE plpgsql SECURITY DEFINER SET search_path=pg_catalog,public,pg_temp AS $$
DECLARE v_workspace bigint; v_org bigint; v_locked_org bigint; v_status text; v_count bigint;
  v_scope text; v_locked_workspace bigint; v_locked_scope text; v_org_role text;
  v_workspace_member boolean := false; v_parent_ok boolean := false;
  v_channel_id bigint; v_channel_creator bigint; v_channel_scope text; v_channel_status smallint;
  v_access_type smallint; v_admin_role smallint; v_subscriber boolean := false;
  v_paid boolean := false; v_paid_indef boolean := false; v_paid_until timestamptz;
  v_order record; v_member_role smallint; v_group_owner bigint;
  v_group_type smallint; v_group_join_limit smallint; v_group_e2ee smallint;
  v_discussion_ok boolean := false;
BEGIN
    IF p_uid IS NULL OR p_uid <= 0 OR p_gid IS NULL OR p_gid <= 0
       OR p_payload IS NULL OR jsonb_typeof(p_payload) <> 'object' THEN RETURN false; END IF;
    SELECT g.workspace_id, w.organization_id, g.scope INTO v_workspace, v_org, v_scope
      FROM public."group" g LEFT JOIN public.workspace w ON w.id=g.workspace_id
      WHERE g.id=p_gid AND g.status=1;
    IF NOT FOUND THEN RETURN false; END IF;
    IF v_org IS NOT NULL THEN
      PERFORM 1 FROM public.organization WHERE id=v_org AND status='active' FOR SHARE;
      IF NOT FOUND THEN RETURN false; END IF;
      SELECT role INTO v_org_role FROM public.organization_member WHERE organization_id=v_org
        AND user_id=p_uid AND status='active' FOR SHARE;
      IF NOT FOUND THEN RETURN false; END IF;
    END IF;
    IF v_workspace IS NOT NULL THEN
      SELECT status,organization_id INTO v_status,v_locked_org FROM public.workspace WHERE id=v_workspace FOR UPDATE;
      IF NOT FOUND OR v_status <> 'active' OR v_locked_org IS DISTINCT FROM v_org THEN RETURN false; END IF;
      PERFORM 1 FROM public.workspace_member WHERE workspace_id=v_workspace AND user_id=p_uid
        AND status='active' FOR SHARE;
      v_workspace_member := FOUND;
    END IF;
    -- Parent was discovered before locks; re-read the group under lock and
    -- reject if it moved while the organization/workspace locks were acquired.
    SELECT g.workspace_id,g.scope,g.owner_uid,g.type,g.join_limit,g.e2ee_mode
      INTO v_locked_workspace,v_locked_scope,v_group_owner,v_group_type,v_group_join_limit,v_group_e2ee
      FROM public."group" g WHERE g.id=p_gid AND g.status=1 FOR SHARE;
    IF NOT FOUND OR v_locked_workspace IS DISTINCT FROM v_workspace OR v_locked_scope IS DISTINCT FROM v_scope THEN RETURN false; END IF;
    v_parent_ok := (v_scope='personal' AND v_workspace IS NULL) OR
      (v_scope='workspace' AND v_workspace IS NOT NULL AND
       (v_workspace_member OR coalesce(v_org_role IN ('owner','admin'),false)));
    IF NOT v_parent_ok THEN RETURN false; END IF;
    -- Prevent a new discussion binding appearing after our lookup.
    LOCK TABLE public.channel IN SHARE ROW EXCLUSIVE MODE;
    SELECT id,creator_uid,scope,status,access_type
      INTO v_channel_id,v_channel_creator,v_channel_scope,v_channel_status,v_access_type
      FROM public.channel WHERE discussion_group_id=p_gid FOR UPDATE;
    IF FOUND THEN
      INSERT INTO public.msg_store_seq(conv_key,seq) VALUES('c2g:'||p_gid::text,0)
        ON CONFLICT(conv_key) DO UPDATE SET seq=public.msg_store_seq.seq;
      SELECT role INTO v_admin_role FROM public.channel_admin
        WHERE channel_id=v_channel_id AND user_id=p_uid FOR SHARE;
      PERFORM 1 FROM public.channel_subscription
        WHERE channel_id=v_channel_id AND user_id=p_uid AND status=1 FOR SHARE;
      v_subscriber := FOUND;
      FOR v_order IN SELECT subscription_end_at FROM public.channel_order
        WHERE channel_id=v_channel_id AND user_id=p_uid AND status=1
        ORDER BY subscription_end_at NULLS FIRST FOR SHARE
      LOOP
        IF v_order.subscription_end_at IS NULL THEN v_paid_indef := true;
        ELSIF v_paid_until IS NULL OR v_order.subscription_end_at>v_paid_until THEN
          v_paid_until := v_order.subscription_end_at;
        END IF;
      END LOOP;
    END IF;
    SELECT gm.role INTO v_member_role FROM public.group_member gm JOIN public.group_member_generation gen
      ON gen.group_id=gm.group_id AND gen.user_id=gm.user_id
      WHERE gm.group_id=p_gid AND gm.user_id=p_uid AND gm.status=1 AND gen.end_seq IS NULL
      FOR SHARE OF gm,gen;
    GET DIAGNOSTICS v_count = ROW_COUNT;
    IF v_count <> 1 THEN RETURN false; END IF;
    PERFORM pg_advisory_xact_lock(hashtextextended('c2g_forward_revoke:'||p_gid::text||':'||p_msg,0));
    PERFORM 1 FROM public.msg_c2g m WHERE m.msg_id=p_msg AND m.from_id=p_from AND m.to_id=p_gid
      AND m.msg_type=p_type AND (m.e2ee IS NULL OR m.e2ee='{}'::jsonb)
      AND m.payload=p_payload FOR SHARE;
    GET DIAGNOSTICS v_count = ROW_COUNT;
    IF v_count <> 1 THEN RETURN false; END IF;
    v_paid := v_paid_indef OR coalesce(v_paid_until>clock_timestamp(),false);
    v_discussion_ok := v_channel_id IS NULL OR (p_discussion AND v_channel_status=1
      AND v_channel_scope='personal' AND v_scope='personal' AND v_group_owner=v_channel_creator
      AND v_group_type=2 AND v_group_join_limit=3 AND v_group_e2ee=0 AND
      ((coalesce(nullif(v_admin_role,0),CASE WHEN v_channel_creator=p_uid THEN 3 ELSE 0 END)>=2
          AND (v_group_owner=p_uid OR v_member_role>=3))
       OR (v_subscriber AND (v_access_type<>1 OR v_paid))));
    IF NOT coalesce(v_discussion_ok,false) THEN RETURN false; END IF;
    RETURN EXISTS (
      SELECT 1 FROM public.c2g_forward_source_fact f
      JOIN public.msg_c2g_recipient_snapshot s ON s.msg_id=f.msg_id AND s.to_gid=f.group_id
        AND s.from_id=f.sender_id AND s.conv_seq=f.conv_seq
      JOIN public.msg_c2g m ON m.msg_id=f.msg_id AND m.from_id=f.sender_id AND m.to_id=f.group_id
        AND (m.expire_at IS NULL OR m.expire_at > clock_timestamp())
      JOIN public."group" grp ON grp.id=f.group_id AND grp.status=1
      JOIN public.group_member gm ON gm.group_id=grp.id AND gm.user_id=p_uid AND gm.status=1
      JOIN public.group_member_generation g ON g.group_id=grp.id AND g.user_id=p_uid
        AND g.end_seq IS NULL AND g.start_seq<=f.conv_seq
      WHERE f.msg_id=p_msg AND f.group_id=p_gid AND f.sender_id=p_from
        AND p_uid=ANY(s.recipient_uids)
        AND NOT EXISTS (SELECT 1 FROM public.c2g_revoke_fact r
          WHERE r.group_id=p_gid AND r.original_msg_id=p_msg)
        AND NOT EXISTS (SELECT 1 FROM public.c2g_forward_edit_fact e
          WHERE e.group_id=p_gid AND e.original_msg_id=p_msg)
          AND v_parent_ok AND v_discussion_ok
    );
END $$;
REVOKE ALL ON FUNCTION public.fn_forward_source_c2g_validate(text,bigint,bigint,bigint,text,jsonb,boolean) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION public.fn_forward_source_c2g_validate(text,bigint,bigint,bigint,text,jsonb,boolean) TO v5_app_runtime;

-- The restricted runtime cannot INSERT references directly. The writer must
-- prove the source, confirmed winner, target staging, and payload in one tx.
CREATE FUNCTION public.fn_forward_attachment_bind(
    p_key text, p_source_type text, p_source_msg text, p_source_from bigint,
    p_source_target bigint, p_source_type_msg text, p_source_payload jsonb,
    p_target_type text, p_target_msg text, p_sender bigint, p_target bigint,
    p_seq bigint, p_discussion boolean
) RETURNS boolean LANGUAGE plpgsql SECURITY DEFINER SET search_path=pg_catalog,public,pg_temp AS $$
DECLARE a public.attachment%ROWTYPE; v_source_ok boolean; v_scope_ok boolean;
  v_file_ok boolean := false; v_album_ok boolean := false; v_photo record; v_content jsonb;
BEGIN
    v_content := CASE WHEN p_source_payload ? 'msg_type'
      AND jsonb_typeof(p_source_payload->'payload')='object'
      THEN p_source_payload->'payload' ELSE p_source_payload END;
    IF p_key IS NULL OR p_source_payload IS NULL OR p_target_type NOT IN ('c2c','c2g')
       OR p_sender IS NULL OR p_sender<=0 OR p_target IS NULL OR p_target<=0 OR p_seq<=0
       OR p_source_type_msg NOT IN ('image','imageMulti','video','voice','audio','file','location','expression')
       OR NOT (jsonb_path_exists(v_content, '$.**.uri ? (@ == $key)', jsonb_build_object('key',p_key))
            OR jsonb_path_exists(v_content, '$.**.url ? (@ == $key)', jsonb_build_object('key',p_key))
            OR jsonb_path_exists(v_content, '$.**.thumb ? (@ == $key)', jsonb_build_object('key',p_key)))
    THEN RETURN false; END IF;
    IF p_source_type='c2c' THEN
      v_source_ok := public.fn_forward_source_c2c_validate(p_source_msg,p_sender,p_source_from,p_source_target,p_source_type_msg,p_source_payload);
    ELSIF p_source_type='c2g' THEN
      v_source_ok := public.fn_forward_source_c2g_validate(p_source_msg,p_sender,p_source_from,p_source_target,p_source_type_msg,p_source_payload,p_discussion);
    ELSE RETURN false; END IF;
    IF NOT v_source_ok THEN RETURN false; END IF;
    IF NOT EXISTS (SELECT 1 FROM public.msg_store_staging s
      WHERE s.type=p_target_type AND s.msg_id=p_target_msg AND s.from_id=p_sender AND s.to_id=p_target
        AND s.conv_seq=p_seq AND s.msg_type=p_source_type_msg AND (s.e2ee IS NULL OR s.e2ee='{}'::jsonb)
        AND (CASE WHEN p_target_type='c2g' AND s.payload ? 'msg_type' AND jsonb_typeof(s.payload->'payload')='object'
          THEN s.payload->'payload' ELSE s.payload END)
          =(v_content || jsonb_build_object('forwarded',true,'original_msg_id',p_source_msg,'original_type',p_source_type))
        AND (s.expire_at IS NULL OR s.expire_at>clock_timestamp())) THEN RETURN false; END IF;
    PERFORM pg_advisory_xact_lock(hashtextextended(p_key,168));
    SELECT * INTO a FROM public.attachment WHERE path=p_key FOR UPDATE;
    IF NOT FOUND OR a.status<>1 OR a.lifecycle_state<>'live' THEN RETURN false; END IF;
    IF NOT EXISTS (SELECT 1 FROM public.attach_pending p JOIN public.attach_direct_attempt w
      ON w.object_key=p.object_key AND w.attempt_state='WINNER'
      WHERE p.object_key=a.path AND p.confirmed_attachment_id=a.id AND p.direct_state='CONFIRMED'
        AND p.session_state='confirmed' AND p.storage_driver=a.storage_driver
        AND p.storage_namespace=a.storage_namespace AND p.storage_container=a.storage_container
        AND p.storage_locator_sha256=a.storage_locator_sha256 AND p.direct_expected->>'sha256'=a.file_hash256
        AND p.direct_expected->>'size_bytes'=a.size::text AND p.direct_expected->>'mime'=a.mime_type
        AND w.target_key=a.storage_physical_key AND w.target_driver=a.storage_driver
        AND w.target_namespace=a.storage_namespace AND w.target_container=a.storage_container
        AND w.proof_sha256=a.file_hash256 AND w.proof_size=a.size) THEN RETURN false; END IF;
    -- Qualification comes exclusively from rows locked here, never from a
    -- later snapshot which could contain newly inserted, unlocked bindings.
    IF a.scope='group' AND p_source_type='c2g' AND a.scope_ref=p_source_target::text THEN
      IF a.group_file_id IS NOT NULL THEN
        PERFORM 1 FROM public.group_file gf WHERE gf.id=a.group_file_id
          AND gf.group_id=p_source_target AND gf.status=1 FOR SHARE;
        v_file_ok := FOUND;
      END IF;
      FOR v_photo IN SELECT gap.album_id FROM public.group_album_photo gap
        WHERE gap.photo_url=a.path AND gap.group_id=p_source_target AND gap.status=1
        ORDER BY gap.album_id,gap.id FOR SHARE
      LOOP
        PERFORM 1 FROM public.group_album ga WHERE ga.album_id=v_photo.album_id
          AND ga.group_id=p_source_target AND ga.status=1 FOR SHARE;
        IF FOUND THEN v_album_ok := true; END IF;
      END LOOP;
    END IF;
    v_scope_ok := a.scope='public' OR (a.scope='private' AND a.creator_user_id=p_sender)
      OR (a.scope='c2c' AND p_source_type='c2c' AND a.scope_ref=
        'c2c:'||least(p_source_from,p_source_target)::text||':'||greatest(p_source_from,p_source_target)::text)
      OR (a.scope='group' AND p_source_type='c2g' AND a.scope_ref=p_source_target::text
        AND EXISTS (SELECT 1 FROM public.group_member_generation gen
          JOIN public.group_member gm ON gm.group_id=gen.group_id AND gm.user_id=gen.user_id AND gm.status=1
          WHERE gen.group_id=p_source_target AND gen.user_id=p_sender AND gen.end_seq IS NULL
            AND ((a.group_file_id IS NULL AND a.anchor_conv_seq IS NOT NULL AND gen.start_seq<=a.anchor_conv_seq)
              OR (a.group_file_id IS NOT NULL AND v_file_ok)
              OR v_album_ok)))
      OR EXISTS (SELECT 1 FROM public.forward_attachment_reference r
        WHERE r.object_key=p_key AND r.target_type=p_source_type AND r.target_msg_id=p_source_msg
          AND r.target_sender=p_source_from AND r.target_id=p_source_target);
    IF NOT v_scope_ok THEN RETURN false; END IF;
    -- The source may have expired while this transaction waited for the object lock.
    IF p_source_type='c2c' THEN
      v_source_ok := public.fn_forward_source_c2c_validate(p_source_msg,p_sender,p_source_from,p_source_target,p_source_type_msg,p_source_payload);
    ELSE
      v_source_ok := public.fn_forward_source_c2g_validate(p_source_msg,p_sender,p_source_from,p_source_target,p_source_type_msg,p_source_payload,p_discussion);
    END IF;
    IF NOT v_source_ok THEN RETURN false; END IF;
    -- Lock and recheck the exact staging row after waiting for the object.
    -- C2G stages a full wire envelope; C2C stages the content itself.
    PERFORM 1 FROM public.msg_store_staging s
      WHERE s.type=p_target_type AND s.msg_id=p_target_msg AND s.from_id=p_sender AND s.to_id=p_target
        AND s.conv_seq=p_seq AND s.msg_type=p_source_type_msg AND (s.e2ee IS NULL OR s.e2ee='{}'::jsonb)
        AND (CASE WHEN p_target_type='c2g' AND s.payload ? 'msg_type' AND jsonb_typeof(s.payload->'payload')='object'
          THEN s.payload->'payload' ELSE s.payload END)
          =(v_content || jsonb_build_object('forwarded',true,'original_msg_id',p_source_msg,'original_type',p_source_type))
        FOR SHARE;
    IF NOT FOUND THEN RETURN false; END IF;
    -- All object and target locks are held; source or paid entitlement may
    -- have expired while waiting for the target staging row.
    IF p_source_type='c2c' THEN
      v_source_ok := public.fn_forward_source_c2c_validate(p_source_msg,p_sender,p_source_from,p_source_target,p_source_type_msg,p_source_payload);
    ELSE
      v_source_ok := public.fn_forward_source_c2g_validate(p_source_msg,p_sender,p_source_from,p_source_target,p_source_type_msg,p_source_payload,p_discussion);
    END IF;
    IF NOT v_source_ok THEN RETURN false; END IF;
    -- The expiry predicate must run after the staging row lock wait and the
    -- final source check, both of which can consume time.
    IF NOT EXISTS (SELECT 1 FROM public.msg_store_staging s
      WHERE s.type=p_target_type AND s.msg_id=p_target_msg AND s.from_id=p_sender AND s.to_id=p_target
        AND s.conv_seq=p_seq AND (s.expire_at IS NULL OR s.expire_at>clock_timestamp())) THEN RETURN false; END IF;
    INSERT INTO public.forward_attachment_reference(object_key,source_type,source_msg_id,target_type,target_msg_id,target_sender,target_id,target_seq)
      VALUES(p_key,p_source_type,p_source_msg,p_target_type,p_target_msg,p_sender,p_target,p_seq)
      ON CONFLICT DO NOTHING;
    RETURN FOUND;
END $$;
REVOKE ALL ON FUNCTION public.fn_forward_attachment_bind(text,text,text,bigint,bigint,text,jsonb,text,text,bigint,bigint,bigint,boolean) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION public.fn_forward_attachment_bind(text,text,text,bigint,bigint,text,jsonb,text,text,bigint,bigint,bigint,boolean) TO v5_app_runtime;
