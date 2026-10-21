-- Root supplies an existing real WAR-protected asset ID from a lawful owned fixture.
-- psql -X -v ON_ERROR_STOP=1 -v protected_attachment_id=<actual> -f ...
-- This script does not invent a protected row or alter public governance metadata.
\set ON_ERROR_STOP on
BEGIN;
SET LOCAL ROLE v5_app_runtime;
SELECT set_config('test.t20.protected_attachment_id', :'protected_attachment_id', true);
DO $$ BEGIN
 IF current_user<>'v5_app_runtime' OR (SELECT rolsuper FROM pg_roles WHERE rolname=current_user) THEN
  RAISE EXCEPTION 'must execute with non-superuser runtime effective role'; END IF;
 IF NOT public.fn_talk_record_war_blocks(current_setting('test.t20.protected_attachment_id')::bigint) THEN
  RAISE EXCEPTION 'real protected fixture required'; END IF;
END $$;
CREATE TEMP TABLE workspace_attachment_retention (LIKE public.workspace_attachment_retention INCLUDING DEFAULTS);
CREATE TEMP TABLE enterprise_attachment_retention (LIKE public.enterprise_attachment_retention INCLUDING DEFAULTS);
-- Both shadows are empty: a vulnerable unqualified helper would now return false.
GRANT SELECT ON pg_temp.workspace_attachment_retention,pg_temp.enterprise_attachment_retention
 TO talk_record_retry_definer,v5_fn_owner;
DO $$ BEGIN
 IF NOT public.fn_talk_record_war_blocks(current_setting('test.t20.protected_attachment_id')::bigint) THEN
  RAISE EXCEPTION 'TEMP governance tables bypassed real protection'; END IF;
END $$;
ROLLBACK;
