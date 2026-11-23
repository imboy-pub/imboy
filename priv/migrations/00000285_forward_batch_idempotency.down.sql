LOCK TABLE public.forward_request_batch IN ACCESS EXCLUSIVE MODE;
DO $$
BEGIN
    IF EXISTS (SELECT FROM public.forward_request_batch) THEN
        RAISE EXCEPTION 'forward batch ledger is in use; preserve it before rollback';
    END IF;
END;
$$;

DROP FUNCTION public.fn_forward_batch_settle(bigint,text,integer,text,text,text,text,bigint,text);
DROP FUNCTION public.fn_forward_batch_result(bigint,text,integer);
DROP FUNCTION public.fn_forward_batch_bind(bigint,text,text,bigint,text[]);
DO $$
DECLARE
    added record;
BEGIN
    FOR added IN SELECT * FROM public.forward_request_batch_acl_added LOOP
        EXECUTE format('REVOKE %s (%I) ON TABLE %s FROM v5_fn_owner',
                       added.privilege_name, added.column_name, added.relation_name);
    END LOOP;
END;
$$;
DROP TABLE public.forward_request_batch_acl_added;
DROP TABLE public.forward_request_batch;

-- Forwarding old plain text uses the same exact-content, source and lock checks.
DO $migration$
DECLARE
    signature regprocedure;
    definition text;
    c2c_signature regprocedure := 'public.fn_forward_source_c2c_validate(text,bigint,bigint,bigint,text,jsonb)'::regprocedure;
    payload_anchor text := $anchor$(CASE WHEN p_type = 'text' AND jsonb_typeof(p_payload) = 'string'
          THEN m.payload = (p_payload #>> '{}')
            OR CASE WHEN m.payload IS JSON THEN m.payload::jsonb = p_payload ELSE false END
          ELSE m.payload::jsonb = p_payload END)$anchor$;
    anchor text := $anchor$(jsonb_typeof(p_payload) <> 'object' AND NOT (p_type = 'text' AND jsonb_typeof(p_payload) = 'string'))$anchor$;
BEGIN
    FOREACH signature IN ARRAY ARRAY[
        'public.fn_forward_source_c2c_validate(text,bigint,bigint,bigint,text,jsonb)'::regprocedure,
        'public.fn_forward_source_c2g_validate(text,bigint,bigint,bigint,text,jsonb,boolean)'::regprocedure
    ] LOOP
        definition := pg_get_functiondef(signature);
        IF length(definition) - length(replace(definition, anchor, '')) <> length(anchor) THEN
            RAISE EXCEPTION 'forward source text guard changed: %', signature;
        END IF;
        EXECUTE replace(definition, anchor, $replacement$jsonb_typeof(p_payload) <> 'object'$replacement$);
    END LOOP;
    definition := pg_get_functiondef(c2c_signature);
    IF length(definition) - length(replace(definition, payload_anchor, '')) <> length(payload_anchor) THEN
        RAISE EXCEPTION 'forward C2C payload equality changed: %', c2c_signature;
    END IF;
    EXECUTE replace(definition, payload_anchor, $replacement$m.payload::jsonb=p_payload$replacement$);
END;
$migration$;
