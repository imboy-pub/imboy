-- Read-only verification with actual cleanup credentials; never claim/delete work.
\set ON_ERROR_STOP on
\set ECHO none
\getenv cleanup_login CLEANUP_LOGIN
BEGIN READ ONLY;
SET LOCAL statement_timeout = '30s';
SELECT session_user = :'cleanup_login' AND current_user = session_user AS expected_identity \gset
\if :expected_identity
\else
  DO $$ BEGIN RAISE EXCEPTION 'unexpected cleanup session identity'; END $$;
\endif
DO $$
BEGIN
  IF NOT EXISTS (SELECT 1 FROM pg_roles WHERE rolname = session_user AND rolcanlogin
      AND NOT (rolinherit OR rolsuper OR rolcreatedb OR rolcreaterole OR rolreplication OR rolbypassrls))
     OR (SELECT count(*) FROM pg_auth_members WHERE member = session_user::regrole) <> 1
     OR NOT EXISTS (SELECT 1 FROM pg_auth_members WHERE member = session_user::regrole
          AND roleid = 'v5_worker'::regrole AND NOT admin_option AND NOT inherit_option AND set_option)
     OR EXISTS (SELECT 1 FROM pg_roles r WHERE r.rolname <> session_user
          AND r.rolname <> 'v5_worker' AND pg_has_role(session_user, r.oid, 'MEMBER'))
     OR EXISTS (SELECT 1 FROM pg_shdepend WHERE refclassid = 'pg_authid'::regclass
          AND refobjid = session_user::regrole AND deptype = 'o')
     OR has_database_privilege(session_user, current_database(), 'CREATE')
     OR has_schema_privilege(session_user, 'public', 'CREATE') THEN
    RAISE EXCEPTION 'unsafe cleanup LOGIN attributes, memberships, ownership or CREATE privileges';
  END IF;
END $$;
SET LOCAL ROLE v5_worker;
DO $$
DECLARE f text;
BEGIN
  IF current_user <> 'v5_worker' OR NOT has_schema_privilege(current_user, 'public', 'USAGE')
     OR has_schema_privilege(current_user, 'public', 'CREATE') THEN
    RAISE EXCEPTION 'cleanup SET ROLE or schema access failed';
  END IF;
  FOREACH f IN ARRAY ARRAY[
      'public.fn_dv1_tombstone_claim(uuid)',
      'public.fn_dv1_tombstone_ack(bigint,uuid,boolean)',
      'public.fn_cross_rescan_claim(uuid)',
      'public.fn_cross_rescan_ack(bigint,uuid,boolean)'] LOOP
    IF to_regprocedure(f) IS NULL OR NOT has_function_privilege(current_user, f, 'EXECUTE')
       OR to_regprocedure(substr(f, 8)) IS DISTINCT FROM to_regprocedure(f) THEN
      RAISE EXCEPTION 'cleanup function EXECUTE missing';
    END IF;
  END LOOP;
  -- Migration 190 legitimately grants v5_worker SELECT on five lifecycle tables.
  -- Never mistake PUBLIC or direct write grants for the required function access.
  IF EXISTS (SELECT 1 FROM pg_class c JOIN pg_namespace n ON n.oid = c.relnamespace
      WHERE n.nspname = 'public' AND c.relkind IN ('r', 'p', 'v', 'm', 'f')
        AND (has_table_privilege(session_user, c.oid, 'INSERT,UPDATE,DELETE,TRUNCATE,REFERENCES,TRIGGER')
             OR has_table_privilege(current_user, c.oid, 'INSERT,UPDATE,DELETE,TRUNCATE,REFERENCES,TRIGGER')
             OR has_any_column_privilege(session_user, c.oid, 'INSERT,UPDATE,REFERENCES')
             OR has_any_column_privilege(current_user, c.oid, 'INSERT,UPDATE,REFERENCES'))) THEN
    RAISE EXCEPTION 'cleanup LOGIN/v5_worker/PUBLIC has direct table write privileges';
  END IF;
END $$;
ROLLBACK;
\echo CLEANUP_VERIFIED
