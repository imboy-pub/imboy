CREATE TABLE public.forward_request_batch (
    id bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    requester_uid bigint NOT NULL CHECK (requester_uid > 0),
    request_token text COLLATE "C" NOT NULL CHECK (request_token ~ '^[A-Za-z0-9_-]{16,64}$'),
    target_type text NOT NULL CHECK (target_type IN ('c2c', 'c2g')),
    target_id bigint NOT NULL CHECK (target_id > 0),
    source_ids text[] NOT NULL CHECK (cardinality(source_ids) BETWEEN 1 AND 10),
    requested_at timestamptz NOT NULL DEFAULT clock_timestamp(),
    UNIQUE (requester_uid, request_token)
);

CREATE INDEX i_forward_request_batch_user_time
    ON public.forward_request_batch (requester_uid, requested_at);

-- Record exactly the new shared-table column grants, so down keeps prior ACLs.
CREATE TABLE public.forward_request_batch_acl_added (
    relation_name regclass NOT NULL,
    column_name name NOT NULL,
    privilege_name text NOT NULL
);

INSERT INTO public.forward_request_batch_acl_added
SELECT relation_name, column_name, privilege_name
FROM (
    VALUES
        ('public.forward_request_identity'::regclass, 'SELECT',
         ARRAY['request_id','requester_uid','source_type','source_msg_id','target_type',
               'target_id','content_sha256','settled_at','result_msg_id']),
        ('public.forward_request_identity'::regclass, 'INSERT',
         ARRAY['request_id','requester_uid','source_type','source_msg_id','target_type',
               'target_id','content_sha256','settled_at','result_msg_id']),
        ('public.msg_store_staging'::regclass, 'SELECT', ARRAY['type','msg_id','from_id','to_id'])
) AS requirements(relation_name, privilege_name, columns)
CROSS JOIN LATERAL unnest(columns) AS column_names(column_name)
WHERE NOT has_column_privilege('v5_fn_owner', relation_name, column_name, privilege_name);

DO $$
DECLARE
    added record;
BEGIN
    FOR added IN SELECT * FROM public.forward_request_batch_acl_added LOOP
        EXECUTE format('GRANT %s (%I) ON TABLE %s TO v5_fn_owner',
                       added.privilege_name, added.column_name, added.relation_name);
    END LOOP;
END;
$$;

REVOKE ALL ON TABLE public.forward_request_batch_acl_added FROM PUBLIC, v5_app_runtime, v5_fn_owner;
GRANT SELECT (id,requester_uid,request_token,target_type,target_id,source_ids,requested_at),
      INSERT (requester_uid,request_token,target_type,target_id,source_ids)
ON public.forward_request_batch TO v5_fn_owner;
GRANT USAGE ON SEQUENCE public.forward_request_batch_id_seq TO v5_fn_owner;

CREATE FUNCTION public.fn_forward_batch_bind(
    p_uid bigint, p_token text, p_target_type text, p_target_id bigint, p_source_ids text[]
) RETURNS text LANGUAGE plpgsql SECURITY DEFINER SET search_path = pg_catalog, pg_temp AS $$
BEGIN
    IF p_uid IS NULL OR p_uid <= 0 OR p_token IS NULL
       OR p_token COLLATE "C" !~ '^[A-Za-z0-9_-]{16,64}$'
       OR p_target_type IS NULL OR p_target_type NOT IN ('c2c', 'c2g')
       OR p_target_id IS NULL OR p_target_id <= 0
       OR p_source_ids IS NULL OR cardinality(p_source_ids) NOT BETWEEN 1 AND 10
       OR array_ndims(p_source_ids) IS DISTINCT FROM 1
       OR array_lower(p_source_ids, 1) IS DISTINCT FROM 1
       OR EXISTS (
           SELECT FROM unnest(p_source_ids) AS item(id)
           WHERE id IS NULL OR char_length(id) NOT BETWEEN 1 AND 40
       )
    THEN
        RETURN 'conflict';
    END IF;

    -- Existing retries never allocate another entry or consume the per-user budget.
    PERFORM pg_advisory_xact_lock(hashtextextended('forward_batch:' || p_uid::text, 0));
    IF EXISTS (
        SELECT FROM public.forward_request_batch
        WHERE requester_uid = p_uid AND request_token = p_token
    ) THEN
        IF EXISTS (
            SELECT FROM public.forward_request_batch
            WHERE requester_uid = p_uid AND request_token = p_token
              AND target_type = p_target_type AND target_id = p_target_id AND source_ids = p_source_ids
        ) THEN
            RETURN 'bound';
        END IF;
        RETURN 'conflict';
    END IF;

    IF (
        SELECT count(*) FROM public.forward_request_batch
        WHERE requester_uid = p_uid AND requested_at >= clock_timestamp() - interval '1 minute'
    ) >= 60 THEN
        RETURN 'rate_limited';
    END IF;

    INSERT INTO public.forward_request_batch (requester_uid, request_token, target_type, target_id, source_ids)
    VALUES (p_uid, p_token, p_target_type, p_target_id, p_source_ids);
    RETURN 'bound';
END;
$$;

CREATE FUNCTION public.fn_forward_batch_result(p_uid bigint, p_token text, p_position integer)
RETURNS text LANGUAGE sql STABLE SECURITY DEFINER SET search_path = pg_catalog, pg_temp AS $$
    SELECT item.result_msg_id::text
      FROM public.forward_request_batch AS batch
      JOIN public.forward_request_identity AS item
        ON item.request_id = 'forward-batch:' || batch.id::text || ':' || p_position::text
       AND item.requester_uid = batch.requester_uid
       AND item.target_type = batch.target_type
       AND item.target_id = batch.target_id
       AND item.source_msg_id = batch.source_ids[p_position]
     WHERE batch.requester_uid = p_uid AND batch.request_token = p_token
       AND item.settled_at IS NOT NULL;
$$;

CREATE FUNCTION public.fn_forward_batch_settle(
    p_uid bigint, p_token text, p_position integer, p_source_type text, p_source_id text,
    p_content_sha256 text, p_target_type text, p_target_id bigint, p_result_msg_id text
) RETURNS text LANGUAGE plpgsql SECURITY DEFINER SET search_path = pg_catalog, pg_temp AS $$
DECLARE
    batch_id bigint;
    item_id text;
BEGIN
    IF p_position IS NULL OR p_position NOT BETWEEN 1 AND 10
       OR p_source_type IS NULL OR p_source_type NOT IN ('c2c', 'c2g')
       OR p_content_sha256 IS NULL OR p_content_sha256 COLLATE "C" !~ '^[0-9a-f]{64}$'
       OR p_result_msg_id IS NULL OR char_length(p_result_msg_id) NOT BETWEEN 1 AND 40
    THEN
        RETURN 'conflict';
    END IF;

    SELECT id INTO batch_id FROM public.forward_request_batch
    WHERE requester_uid = p_uid AND request_token = p_token
      AND target_type = p_target_type AND target_id = p_target_id
      AND source_ids[p_position] = p_source_id;
    IF NOT FOUND THEN
        RETURN 'conflict';
    END IF;

    -- 结果必须已进入本次发送使用的同一暂存链，失败由调用方回滚。
    IF NOT EXISTS (
        SELECT FROM public.msg_store_staging
        WHERE type = p_target_type AND msg_id = p_result_msg_id
          AND from_id = p_uid AND to_id = p_target_id
    ) THEN
        RETURN 'conflict';
    END IF;

    item_id := 'forward-batch:' || batch_id::text || ':' || p_position::text;
    INSERT INTO public.forward_request_identity (
        request_id, requester_uid, source_type, source_msg_id, target_type, target_id,
        content_sha256, settled_at, result_msg_id
    ) VALUES (
        item_id, p_uid, p_source_type, p_source_id, p_target_type, p_target_id,
        p_content_sha256, clock_timestamp(), p_result_msg_id
    ) ON CONFLICT (request_id) DO NOTHING;
    IF FOUND THEN
        RETURN 'accepted';
    END IF;
    IF EXISTS (
        SELECT FROM public.forward_request_identity
        WHERE request_id = item_id AND requester_uid = p_uid
          AND source_type = p_source_type AND source_msg_id = p_source_id
          AND target_type = p_target_type AND target_id = p_target_id
          AND content_sha256 = p_content_sha256 AND settled_at IS NOT NULL
    ) THEN
        RETURN 'replay';
    END IF;
    RETURN 'conflict';
END;
$$;

REVOKE ALL ON TABLE public.forward_request_batch FROM PUBLIC, v5_app_runtime;
ALTER FUNCTION public.fn_forward_batch_bind(bigint,text,text,bigint,text[]) OWNER TO v5_fn_owner;
ALTER FUNCTION public.fn_forward_batch_result(bigint,text,integer) OWNER TO v5_fn_owner;
ALTER FUNCTION public.fn_forward_batch_settle(bigint,text,integer,text,text,text,text,bigint,text) OWNER TO v5_fn_owner;
REVOKE ALL ON FUNCTION public.fn_forward_batch_bind(bigint,text,text,bigint,text[]) FROM PUBLIC;
REVOKE ALL ON FUNCTION public.fn_forward_batch_result(bigint,text,integer) FROM PUBLIC;
REVOKE ALL ON FUNCTION public.fn_forward_batch_settle(bigint,text,integer,text,text,text,text,bigint,text) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION public.fn_forward_batch_bind(bigint,text,text,bigint,text[]) TO v5_app_runtime;
GRANT EXECUTE ON FUNCTION public.fn_forward_batch_result(bigint,text,integer) TO v5_app_runtime;
GRANT EXECUTE ON FUNCTION public.fn_forward_batch_settle(bigint,text,integer,text,text,text,text,bigint,text) TO v5_app_runtime;

-- Forwarding old plain text uses the same exact-content, source and lock checks.
DO $migration$
DECLARE
    signature regprocedure;
    definition text;
    c2c_signature regprocedure := 'public.fn_forward_source_c2c_validate(text,bigint,bigint,bigint,text,jsonb)'::regprocedure;
    payload_anchor text := $anchor$m.payload::jsonb=p_payload$anchor$;
    anchor text := $anchor$jsonb_typeof(p_payload) <> 'object'$anchor$;
BEGIN
    FOREACH signature IN ARRAY ARRAY[
        'public.fn_forward_source_c2c_validate(text,bigint,bigint,bigint,text,jsonb)'::regprocedure,
        'public.fn_forward_source_c2g_validate(text,bigint,bigint,bigint,text,jsonb,boolean)'::regprocedure
    ] LOOP
        definition := pg_get_functiondef(signature);
        IF length(definition) - length(replace(definition, anchor, '')) <> length(anchor) THEN
            RAISE EXCEPTION 'forward source text guard changed: %', signature;
        END IF;
        EXECUTE replace(definition, anchor, $replacement$(jsonb_typeof(p_payload) <> 'object' AND NOT (p_type = 'text' AND jsonb_typeof(p_payload) = 'string'))$replacement$);
    END LOOP;
    definition := pg_get_functiondef(c2c_signature);
    IF length(definition) - length(replace(definition, payload_anchor, '')) <> length(payload_anchor) THEN
        RAISE EXCEPTION 'forward C2C payload equality changed: %', c2c_signature;
    END IF;
    EXECUTE replace(definition, payload_anchor, $replacement$(CASE WHEN p_type = 'text' AND jsonb_typeof(p_payload) = 'string'
          THEN m.payload = (p_payload #>> '{}')
            OR CASE WHEN m.payload IS JSON THEN m.payload::jsonb = p_payload ELSE false END
          ELSE m.payload::jsonb = p_payload END)$replacement$);
END;
$migration$;
