-- One transaction; run only after the normal migration runner has completed.
\set ON_ERROR_STOP on
\set ECHO none
\getenv cleanup_login CLEANUP_LOGIN
\getenv cleanup_create CLEANUP_CREATE
\getenv cleanup_versions CLEANUP_VERSIONS
BEGIN;
SET LOCAL lock_timeout = '10s';
SET LOCAL statement_timeout = '30s';
SELECT pg_advisory_xact_lock(hashtextextended('imboy-cleanup-login:' || :'cleanup_login', 0));
\echo CLEANUP_STAGE=migrations
-- The oracle comes from the running backend release, not a guessed minimum head.
SELECT set_config('imboy.cleanup_versions', :'cleanup_versions', true);
DO $$
DECLARE expected bigint[] := string_to_array(current_setting('imboy.cleanup_versions'), ',')::bigint[];
BEGIN
  IF to_regclass('public.schema_migrations') IS NULL
     OR to_regclass('public.schema_migrations_history') IS NULL THEN
    RAISE EXCEPTION 'cleanup migrations not ready';
  END IF;
  IF NOT ARRAY[179, 267, 270, 271, 274]::bigint[] <@ expected
     OR (SELECT count(*) FROM public.schema_migrations) <> 1
     OR NOT EXISTS (SELECT 1 FROM public.schema_migrations
          WHERE version = (SELECT max(v) FROM unnest(expected) v) AND NOT dirty)
     OR (SELECT array_agg(version ORDER BY version) FROM public.schema_migrations_history) IS DISTINCT FROM expected THEN
    RAISE EXCEPTION 'cleanup migration history differs from the running release or is dirty';
  END IF;
END $$;
\echo CLEANUP_STAGE=worker_role
DO $$
DECLARE f text;
BEGIN
  IF NOT EXISTS (SELECT 1 FROM pg_roles WHERE rolname = 'v5_worker'
      AND NOT (rolcanlogin OR rolinherit OR rolsuper OR rolcreatedb OR rolcreaterole
               OR rolreplication OR rolbypassrls))
     OR EXISTS (SELECT 1 FROM pg_auth_members WHERE member = 'v5_worker'::regrole)
     OR EXISTS (SELECT 1 FROM pg_shdepend WHERE refclassid = 'pg_authid'::regclass
          AND refobjid = 'v5_worker'::regrole AND deptype = 'o') THEN
    RAISE EXCEPTION 'v5_worker missing or unsafe; do not create or repair it outside migrations';
  END IF;
  IF NOT has_schema_privilege('v5_worker', 'public', 'USAGE')
     OR has_schema_privilege('v5_worker', 'public', 'CREATE')
     OR has_database_privilege('v5_worker', current_database(), 'CREATE') THEN
    RAISE EXCEPTION 'v5_worker schema access missing';
  END IF;
  FOREACH f IN ARRAY ARRAY[
      'public.fn_dv1_tombstone_claim(uuid)',
      'public.fn_dv1_tombstone_ack(bigint,uuid,boolean)',
      'public.fn_cross_rescan_claim(uuid)',
      'public.fn_cross_rescan_ack(bigint,uuid,boolean)'] LOOP
    IF to_regprocedure(f) IS NULL OR NOT has_function_privilege('v5_worker', f, 'EXECUTE') THEN
      RAISE EXCEPTION 'cleanup function contract missing';
    END IF;
  END LOOP;
END $$;
SELECT EXISTS (SELECT 1 FROM pg_roles WHERE rolname = :'cleanup_login') AS role_exists \gset
\if :role_exists
  -- Existing accounts are immutable here: no password rotation or permission repair.
  \echo CLEANUP_REUSED
\else
  \echo CLEANUP_STAGE=create_authority
  \if :cleanup_create
    DO $$
    BEGIN
      IF NOT EXISTS (SELECT 1 FROM pg_roles WHERE rolname = current_user AND
          (rolsuper OR (rolcreaterole AND EXISTS (
            SELECT 1 FROM pg_auth_members WHERE roleid = 'v5_worker'::regrole
              AND member = current_user::regrole AND admin_option))))
         OR NOT has_database_privilege(current_user, current_database(), 'CONNECT WITH GRANT OPTION') THEN
        RAISE EXCEPTION 'CREATEROLE, v5_worker ADMIN OPTION and database CONNECT grant authority required';
      END IF;
    END $$;
    \echo CLEANUP_STAGE=create_transaction
    CREATE ROLE :"cleanup_login" LOGIN NOINHERIT NOSUPERUSER NOCREATEDB NOCREATEROLE
      NOREPLICATION NOBYPASSRLS;
    SET LOCAL password_encryption = 'scram-sha-256';
    -- Client-side hashing keeps plaintext out of PostgreSQL statement/error logs.
    \password :cleanup_login
    GRANT v5_worker TO :"cleanup_login" WITH ADMIN FALSE, INHERIT FALSE, SET TRUE;
    SELECT current_database() AS cleanup_database \gset
    GRANT CONNECT ON DATABASE :"cleanup_database" TO :"cleanup_login";
    \echo CLEANUP_CREATED
  \else
    DO $$ BEGIN RAISE EXCEPTION 'cleanup LOGIN missing; authorize --create or ask DBA'; END $$;
  \endif
\endif
COMMIT;
