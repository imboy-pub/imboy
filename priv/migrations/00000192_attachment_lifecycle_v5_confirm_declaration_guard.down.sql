-- Dropping this guard reopens scope substitution; forward fix only.
DO $$ BEGIN RAISE EXCEPTION 'V5_DOWN_BLOCKED_USE_FORWARD_FIX'; END; $$;
