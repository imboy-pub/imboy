-- Facts created only when the group message is durably accepted after migration.
CREATE TABLE public.c2g_forward_source_fact (
    msg_id varchar(40) PRIMARY KEY,
    group_id bigint NOT NULL,
    sender_id bigint NOT NULL,
    conv_seq bigint NOT NULL CHECK (conv_seq > 0),
    accepted_at timestamptz NOT NULL DEFAULT clock_timestamp()
);

CREATE FUNCTION public.fn_c2g_forward_source_capture() RETURNS trigger
LANGUAGE plpgsql SECURITY DEFINER SET search_path = pg_catalog, public, pg_temp AS $$
BEGIN
    IF NEW.type = 'c2g' AND coalesce(NEW.action, '') = '' THEN
        IF NEW.to_id IS NULL OR NEW.conv_seq IS NULL OR NEW.conv_seq <= 0 THEN
            RAISE EXCEPTION 'group source fact requires accepted sequence' USING ERRCODE = '23514';
        END IF;
        INSERT INTO public.c2g_forward_source_fact(msg_id, group_id, sender_id, conv_seq)
        VALUES (NEW.msg_id, NEW.to_id, NEW.from_id, NEW.conv_seq);
    END IF;
    RETURN NEW;
END $$;
REVOKE ALL ON FUNCTION public.fn_c2g_forward_source_capture() FROM PUBLIC;
CREATE TRIGGER trg_c2g_forward_source_capture AFTER INSERT ON public.msg_store_staging
FOR EACH ROW EXECUTE FUNCTION public.fn_c2g_forward_source_capture();
GRANT SELECT ON public.c2g_forward_source_fact TO v5_app_runtime;
