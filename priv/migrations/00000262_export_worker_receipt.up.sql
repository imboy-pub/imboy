-- A confirmed receipt is written only after the unique export PUT returns success.
CREATE TABLE attachment_export_write_receipt (
    attachment_id bigint PRIMARY KEY REFERENCES attachment_derived_write(attachment_id),
    source_job_id bigint NOT NULL,
    source_attempt integer NOT NULL,
    token_digest bytea NOT NULL CHECK (octet_length(token_digest) = 32),
    physical_key text NOT NULL,
    locator_digest text NOT NULL,
    sha256 text NOT NULL,
    size bigint NOT NULL CHECK (size > 0),
    confirmed_at timestamptz
);
REVOKE ALL ON attachment_export_write_receipt FROM PUBLIC, v5_app_runtime, v5_worker, v5_proof;
GRANT SELECT, INSERT, UPDATE ON attachment_export_write_receipt TO v5_fn_owner;
CREATE TABLE attachment_export_receipt_grant_restore (
    add_derived_update boolean NOT NULL,
    add_job_columns text[] NOT NULL
);
REVOKE ALL ON attachment_export_receipt_grant_restore FROM PUBLIC, v5_app_runtime, v5_worker, v5_proof;
INSERT INTO attachment_export_receipt_grant_restore
SELECT NOT has_table_privilege('v5_fn_owner','attachment_derived_write','UPDATE'),
       ARRAY(SELECT c FROM unnest(ARRAY['id','state','attempt','lease_until','kind']) c
             WHERE NOT has_column_privilege('v5_fn_owner','talk_record_job',c,'SELECT'));
DO $$ DECLARE r record; c text; BEGIN
    SELECT * INTO r FROM attachment_export_receipt_grant_restore;
    IF r.add_derived_update THEN GRANT UPDATE ON attachment_derived_write TO v5_fn_owner; END IF;
    FOREACH c IN ARRAY r.add_job_columns LOOP
        EXECUTE format('GRANT SELECT (%I) ON talk_record_job TO v5_fn_owner', c);
    END LOOP;
END $$;

CREATE FUNCTION fn_export_issue_receipt(p_aid bigint, p_digest bytea) RETURNS boolean
LANGUAGE plpgsql SECURITY DEFINER SET search_path = pg_catalog, public AS $$
DECLARE r record;
BEGIN
    IF octet_length(p_digest) <> 32 THEN RETURN false; END IF;
    SELECT a.path, a.storage_physical_key, a.storage_locator_sha256, a.file_hash256,
           a.size, w.source_job_id, w.source_attempt, w.state, j.kind
      INTO r FROM public.attachment a
      JOIN public.attachment_derived_write w ON w.attachment_id=a.id
      JOIN public.talk_record_job j ON j.id=w.source_job_id
      WHERE a.id=p_aid AND a.lifecycle_state='live' FOR UPDATE OF a,w;
    IF NOT FOUND OR r.state <> 'writing' OR r.kind <> 'export'
       OR r.path NOT LIKE 'talk-record/export/%' OR r.storage_physical_key IS NULL
       OR r.storage_locator_sha256 IS NULL OR r.file_hash256 IS NULL OR r.size <= 0 THEN
        RETURN false;
    END IF;
    INSERT INTO public.attachment_export_write_receipt
      (attachment_id,source_job_id,source_attempt,token_digest,physical_key,locator_digest,sha256,size)
    VALUES (p_aid,r.source_job_id,r.source_attempt,p_digest,r.storage_physical_key,
            r.storage_locator_sha256,r.file_hash256,r.size)
    ON CONFLICT DO NOTHING;
    RETURN FOUND;
END $$;

CREATE FUNCTION fn_export_confirm_receipt(p_aid bigint, p_token bytea, p_job bigint,
                                          p_attempt integer, p_key text, p_sha text, p_size bigint)
RETURNS boolean LANGUAGE plpgsql SECURITY DEFINER SET search_path = pg_catalog, public AS $$
DECLARE r record;
BEGIN
    IF octet_length(p_token) <> 32 THEN RETURN false; END IF;
    SELECT a.storage_physical_key, a.storage_locator_sha256, a.file_hash256, a.size,
           w.source_job_id, w.source_attempt, w.state
      INTO r FROM public.attachment a JOIN public.attachment_derived_write w ON w.attachment_id=a.id
      WHERE a.id=p_aid AND a.lifecycle_state='live' FOR UPDATE OF a,w;
    IF NOT FOUND OR r.state NOT IN ('writing','unknown') OR r.source_job_id <> p_job
       OR r.source_attempt <> p_attempt OR r.storage_physical_key <> p_key
       OR r.file_hash256 <> p_sha OR r.size <> p_size THEN RETURN false; END IF;
    UPDATE public.attachment_export_write_receipt SET confirmed_at=clock_timestamp()
      WHERE attachment_id=p_aid AND source_job_id=p_job AND source_attempt=p_attempt
        AND physical_key=p_key AND locator_digest=r.storage_locator_sha256
        AND sha256=p_sha AND size=p_size AND confirmed_at IS NULL
        AND token_digest=public.digest(p_token, 'sha256');
    RETURN FOUND;
END $$;

CREATE OR REPLACE FUNCTION fn_attachment_derived_write_transition() RETURNS trigger
LANGUAGE plpgsql SET search_path = pg_catalog, public AS $$
BEGIN
    IF NEW.attachment_id IS DISTINCT FROM OLD.attachment_id
       OR NEW.source_job_id IS DISTINCT FROM OLD.source_job_id
       OR NEW.source_attempt IS DISTINCT FROM OLD.source_attempt THEN
        RAISE EXCEPTION 'derived write source identity is immutable' USING ERRCODE='23514';
    END IF;
    IF NEW.state<>OLD.state AND NOT (
        (OLD.state='reserved' AND NEW.state IN ('writing','abandoned')) OR
        (OLD.state='writing' AND NEW.state IN ('complete','unknown')) OR
        (OLD.state='unknown' AND NEW.state='complete' AND EXISTS (
            SELECT 1 FROM public.attachment_export_write_receipt r
            JOIN public.attachment a ON a.id=r.attachment_id
            WHERE r.attachment_id=OLD.attachment_id AND r.confirmed_at IS NOT NULL
              AND r.source_job_id=OLD.source_job_id AND r.source_attempt=OLD.source_attempt
              AND r.physical_key=a.storage_physical_key AND r.locator_digest=a.storage_locator_sha256
              AND r.sha256=a.file_hash256 AND r.size=a.size AND a.lifecycle_state='live'
        ))) THEN
        RAISE EXCEPTION 'illegal derived write transition' USING ERRCODE='23514';
    END IF;
    RETURN NEW;
END $$;

CREATE FUNCTION fn_export_reconcile_receipts() RETURNS integer
LANGUAGE plpgsql SECURITY DEFINER SET search_path = pg_catalog, public AS $$
DECLARE r record; n integer := 0;
BEGIN
    FOR r IN SELECT x.attachment_id FROM public.attachment_export_write_receipt x
             JOIN public.attachment_derived_write w ON w.attachment_id=x.attachment_id
             WHERE x.confirmed_at IS NOT NULL AND w.state='unknown'
             ORDER BY x.attachment_id LIMIT 100 LOOP
        PERFORM 1 FROM public.attachment WHERE id=r.attachment_id FOR UPDATE;
        PERFORM 1 FROM public.attachment_derived_write WHERE attachment_id=r.attachment_id FOR UPDATE;
        UPDATE public.attachment_derived_write w SET state='complete',updated_at=clock_timestamp()
         WHERE w.attachment_id=r.attachment_id AND w.state='unknown'
           AND NOT EXISTS (SELECT 1 FROM public.talk_record_job j WHERE j.id=w.source_job_id
               AND j.state='running' AND j.attempt=w.source_attempt AND j.lease_until>clock_timestamp());
        IF FOUND THEN n := n + 1; END IF;
    END LOOP;
    RETURN n;
END $$;

ALTER FUNCTION fn_export_issue_receipt(bigint,bytea) OWNER TO v5_fn_owner;
ALTER FUNCTION fn_export_confirm_receipt(bigint,bytea,bigint,integer,text,text,bigint) OWNER TO v5_fn_owner;
ALTER FUNCTION fn_export_reconcile_receipts() OWNER TO v5_fn_owner;
REVOKE ALL ON FUNCTION fn_export_issue_receipt(bigint,bytea) FROM PUBLIC, v5_worker, v5_proof;
REVOKE ALL ON FUNCTION fn_export_confirm_receipt(bigint,bytea,bigint,integer,text,text,bigint) FROM PUBLIC, v5_worker, v5_proof;
REVOKE ALL ON FUNCTION fn_export_reconcile_receipts() FROM PUBLIC, v5_worker, v5_proof;
GRANT EXECUTE ON FUNCTION fn_export_issue_receipt(bigint,bytea) TO v5_app_runtime;
GRANT EXECUTE ON FUNCTION fn_export_confirm_receipt(bigint,bytea,bigint,integer,text,text,bigint) TO v5_app_runtime;
GRANT EXECUTE ON FUNCTION fn_export_reconcile_receipts() TO v5_app_runtime;
