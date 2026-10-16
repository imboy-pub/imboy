-- Protect future C2C writes, including old binaries and direct SQL. Existing
-- legacy rows are preserved; no inferred order or automatic historical repair.
SET LOCAL lock_timeout = '5s';
SET LOCAL statement_timeout = '15min';

CREATE INDEX IF NOT EXISTS i_msg_c2c_order_slot ON public.msg_c2c
    (least(from_id,to_id),greatest(from_id,to_id),conv_seq) WHERE conv_seq IS NOT NULL;
CREATE INDEX IF NOT EXISTS i_msg_staging_c2c_order_slot ON public.msg_store_staging
    (least(from_id,to_id),greatest(from_id,to_id),conv_seq) WHERE type='c2c' AND conv_seq IS NOT NULL;

CREATE OR REPLACE FUNCTION public.fn_c2c_order_assert(
    p_msg_id text, p_from bigint, p_to bigint, p_seq bigint
) RETURNS void LANGUAGE plpgsql AS $$
DECLARE conflict_found boolean; existing_message boolean; current_head bigint;
    key text := 'c2c:' || least(p_from,p_to)::text || ':' || greatest(p_from,p_to)::text;
BEGIN
    -- Same lock order as the allocator; serializes legacy/direct writes too.
    PERFORM pg_advisory_xact_lock(hashtextextended('c2c_order:' || key,0));
    PERFORM pg_advisory_xact_lock(hashtextextended('msg_c2c:' || p_msg_id,0));
    SELECT bool_or(r.msg_id=p_msg_id AND r.conv_seq IS NOT NULL), bool_or(
        (r.msg_id=p_msg_id AND ((r.from_id,r.to_id) IS DISTINCT FROM (p_from,p_to)
            OR (r.conv_seq IS NOT NULL AND r.conv_seq<>p_seq))) OR r.msg_id<>p_msg_id
    ) INTO existing_message,conflict_found FROM (
            SELECT msg_id,from_id,to_id,conv_seq FROM public.msg_store_staging
            WHERE type='c2c' AND (msg_id=p_msg_id OR
                (least(from_id,to_id)=least(p_from,p_to) AND greatest(from_id,to_id)=greatest(p_from,p_to) AND conv_seq=p_seq))
            UNION ALL SELECT msg_id,from_id,to_id,conv_seq FROM public.msg_c2c
            WHERE msg_id=p_msg_id OR
                (least(from_id,to_id)=least(p_from,p_to) AND greatest(from_id,to_id)=greatest(p_from,p_to) AND conv_seq=p_seq)
            UNION ALL SELECT msg_id,from_id,to_id,conv_seq FROM public.msg_store
            WHERE chat_type='c2c' AND (msg_id=p_msg_id OR
                (conv_key='c2c:' || least(p_from,p_to)::text || ':' || greatest(p_from,p_to)::text AND conv_seq=p_seq))
        ) r;
    SELECT greatest(coalesce((SELECT seq FROM public.msg_store_seq WHERE conv_key=key),0),
        coalesce((SELECT max(conv_seq) FROM public.msg_store WHERE chat_type='c2c' AND conv_key=key),0),
        coalesce((SELECT max(conv_seq) FROM public.msg_store_staging WHERE type='c2c'
            AND least(from_id,to_id)=least(p_from,p_to) AND greatest(from_id,to_id)=greatest(p_from,p_to)),0),
        coalesce((SELECT max(conv_seq) FROM public.msg_c2c
            WHERE least(from_id,to_id)=least(p_from,p_to) AND greatest(from_id,to_id)=greatest(p_from,p_to)),0))
        INTO current_head;
    IF conflict_found OR (NOT coalesce(existing_message,false) AND p_seq<=current_head) THEN
        RAISE EXCEPTION 'C2C message identity or conversation sequence conflicts'
            USING ERRCODE='23514', CONSTRAINT='c2c_order_conflict';
    END IF;
END $$;

CREATE OR REPLACE FUNCTION public.fn_c2c_order_write_guard()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE current_c2c boolean; prior_c2c boolean;
BEGIN
    current_c2c := true;
    IF TG_ARGV[0]='msg_store_staging' THEN current_c2c := NEW.type='c2c';
    ELSIF TG_ARGV[0]='msg_store' THEN current_c2c := NEW.chat_type='c2c'; END IF;
    IF TG_OP='UPDATE' THEN
        prior_c2c := true;
        IF TG_ARGV[0]='msg_store_staging' THEN prior_c2c := OLD.type='c2c';
        ELSIF TG_ARGV[0]='msg_store' THEN prior_c2c := OLD.chat_type='c2c'; END IF;
        IF prior_c2c AND (NOT current_c2c OR
            (NEW.msg_id,NEW.from_id,NEW.to_id) IS DISTINCT FROM (OLD.msg_id,OLD.from_id,OLD.to_id) OR
            (OLD.conv_seq IS NOT NULL AND NEW.conv_seq IS DISTINCT FROM OLD.conv_seq)) THEN
            RAISE EXCEPTION 'Accepted C2C identity and sequence are immutable'
                USING ERRCODE='23514', CONSTRAINT='c2c_order_immutable';
        END IF;
    END IF;
    IF NOT current_c2c THEN RETURN NEW; END IF;
    IF NEW.from_id IS NULL OR NEW.to_id IS NULL OR NEW.conv_seq IS NULL OR NEW.conv_seq<=0 THEN
        RAISE EXCEPTION 'C2C writes require a positive server accepted sequence'
            USING ERRCODE='23514', CONSTRAINT='c2c_sequence_required';
    END IF;
    IF TG_ARGV[0]='msg_store' THEN
        IF NEW.conv_key IS DISTINCT FROM
            'c2c:' || least(NEW.from_id,NEW.to_id)::text || ':' || greatest(NEW.from_id,NEW.to_id)::text THEN
            RAISE EXCEPTION 'C2C archive conversation key differs from peers'
                USING ERRCODE='23514', CONSTRAINT='c2c_conversation_mismatch';
        END IF;
    END IF;
    PERFORM public.fn_c2c_order_assert(NEW.msg_id,NEW.from_id,NEW.to_id,NEW.conv_seq);
    RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_00_c2c_order_guard ON public.msg_store_staging;
CREATE TRIGGER trg_00_c2c_order_guard
BEFORE INSERT OR UPDATE OF type,msg_id,from_id,to_id,conv_seq ON public.msg_store_staging
FOR EACH ROW EXECUTE FUNCTION public.fn_c2c_order_write_guard('msg_store_staging');
DROP TRIGGER IF EXISTS trg_00_c2c_order_guard ON public.msg_c2c;
CREATE TRIGGER trg_00_c2c_order_guard
BEFORE INSERT OR UPDATE OF msg_id,from_id,to_id,conv_seq ON public.msg_c2c
FOR EACH ROW EXECUTE FUNCTION public.fn_c2c_order_write_guard('msg_c2c');
DROP TRIGGER IF EXISTS trg_00_c2c_order_guard ON public.msg_store;
CREATE TRIGGER trg_00_c2c_order_guard
BEFORE INSERT OR UPDATE OF chat_type,conv_key,msg_id,from_id,to_id,conv_seq ON public.msg_store
FOR EACH ROW EXECUTE FUNCTION public.fn_c2c_order_write_guard('msg_store');
