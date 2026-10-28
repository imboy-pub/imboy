-- Restoring the previous function reintroduces a confirmed deadlock.
DO $$ BEGIN
    RAISE EXCEPTION 'unsafe downgrade: attachment intent lock order';
END $$;
