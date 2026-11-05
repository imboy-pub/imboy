DO $$ BEGIN
    IF EXISTS (SELECT 1 FROM public.admin_avatar_session_owner) THEN
        RAISE EXCEPTION 'admin avatar ownership must be preserved before downgrade';
    END IF;
END $$;
DROP TABLE public.admin_avatar_session_owner;
