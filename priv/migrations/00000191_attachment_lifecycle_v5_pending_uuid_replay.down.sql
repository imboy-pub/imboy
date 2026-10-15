-- Reverting restores broken UUID recovery; use a forward fix.
DO $$ BEGIN RAISE EXCEPTION 'V5_DOWN_BLOCKED_USE_FORWARD_FIX'; END; $$;
