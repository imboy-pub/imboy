-- DBA: run after provisioning imboy_enterprise_purge_worker if it did not
-- exist when migration 264 ran. No purge function is public by default.
DO $$ BEGIN
    IF to_regrole('imboy_enterprise_purge_worker') IS NULL THEN
        RAISE EXCEPTION 'provision imboy_enterprise_purge_worker first' USING ERRCODE='23510';
    END IF;
END $$;
GRANT EXECUTE ON FUNCTION public.fn_eb_direct_retire(bigint,bigint,bigint),
    public.fn_eb_direct_purge_candidates(bigint,bigint,bigint[],bigint[]),
    public.fn_eb_direct_refresh_gc(bigint,bigint,bigint),
    public.fn_eb_direct_purge_deferred(bigint,bigint,text,bigint),
    public.fn_eb_direct_purge_defer(bigint,bigint,text,bigint),
    public.fn_eb_direct_purge_clear(bigint,bigint,text,bigint)
    TO imboy_enterprise_purge_worker;
