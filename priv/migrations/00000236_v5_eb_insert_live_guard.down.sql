-- Down: restore the 00000179 fn_v5_eb_registration_guard (UPDATE-only
-- policing, no INSERT lifecycle start constraint).
SET LOCAL lock_timeout = '5s';
SET LOCAL statement_timeout = '15min';

CREATE OR REPLACE FUNCTION public.fn_v5_eb_registration_guard() RETURNS trigger
LANGUAGE plpgsql SET search_path = public AS $$
BEGIN
    IF TG_OP = 'UPDATE' THEN
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
    END IF;
    RETURN NEW;
END;
$$;
