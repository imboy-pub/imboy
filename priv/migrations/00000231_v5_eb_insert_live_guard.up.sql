-- V5.5 forward fix (W1-02): fn_v5_eb_registration_guard only policed UPDATE,
-- so a forged INSERT could enter enterprise_asset directly in a non-live
-- lifecycle_state (e.g. delete_intent), bypassing the intent authority that
-- is supposed to be the only writer of that state (contract leaf
-- forge_gc_verified_on_insert, oracle: "unauthorized DML rejected, no
-- mutation").  Rows must START live; the state machine keeps its UPDATE-side
-- rules unchanged.
SET LOCAL lock_timeout = '5s';
SET LOCAL statement_timeout = '15min';

CREATE OR REPLACE FUNCTION public.fn_v5_eb_registration_guard() RETURNS trigger
LANGUAGE plpgsql SET search_path = public AS $$
BEGIN
    IF TG_OP = 'INSERT' THEN
        IF NEW.lifecycle_state IS DISTINCT FROM 'live' THEN
            RAISE EXCEPTION 'v5 eb asset must start live'
                USING ERRCODE = '23510';
        END IF;
        RETURN NEW;
    END IF;
    IF OLD.client_upload_id IS NOT NULL
       AND NEW.client_upload_id IS DISTINCT FROM OLD.client_upload_id THEN
        RAISE EXCEPTION 'v5 eb client_upload_id immutable' USING ERRCODE = '23510';
    END IF;
    IF OLD.upload_registration IS NOT NULL
       AND NEW.upload_registration IS DISTINCT FROM OLD.upload_registration THEN
        RAISE EXCEPTION 'v5 eb registration immutable' USING ERRCODE = '23510';
    END IF;
    IF OLD.upload_content IS NOT NULL
       AND NEW.upload_content IS DISTINCT FROM OLD.upload_content THEN
        RAISE EXCEPTION 'v5 eb content pin immutable' USING ERRCODE = '23510';
    END IF;
    RETURN NEW;
END;
$$;
