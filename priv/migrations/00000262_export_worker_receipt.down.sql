DO $$ BEGIN
    IF EXISTS (SELECT 1 FROM public.attachment_export_write_receipt r
               JOIN public.attachment_derived_write w ON w.attachment_id=r.attachment_id
               WHERE w.state IN ('reserved','writing','unknown')) THEN
        RAISE EXCEPTION 'export write receipt is still needed' USING ERRCODE='55000';
    END IF;
END $$;
DROP FUNCTION fn_export_reconcile_receipts();
DROP FUNCTION fn_export_confirm_receipt(bigint,bytea,bigint,integer,text,text,bigint);
DROP FUNCTION fn_export_issue_receipt(bigint,bytea);
CREATE OR REPLACE FUNCTION fn_attachment_derived_write_transition() RETURNS trigger
LANGUAGE plpgsql AS $$
BEGIN
    IF NEW.attachment_id IS DISTINCT FROM OLD.attachment_id
       OR NEW.source_job_id IS DISTINCT FROM OLD.source_job_id
       OR NEW.source_attempt IS DISTINCT FROM OLD.source_attempt THEN
        RAISE EXCEPTION 'derived write source identity is immutable' USING ERRCODE='23514';
    END IF;
    IF NEW.state<>OLD.state AND NOT (
        (OLD.state='reserved' AND NEW.state IN ('writing','abandoned')) OR
        (OLD.state='writing' AND NEW.state IN ('complete','unknown'))) THEN
        RAISE EXCEPTION 'illegal derived write transition' USING ERRCODE='23514';
    END IF;
    RETURN NEW;
END $$;
DROP TABLE attachment_export_write_receipt;
DO $$ DECLARE r record; c text; BEGIN
    SELECT * INTO r FROM attachment_export_receipt_grant_restore;
    IF r.add_derived_update THEN REVOKE UPDATE ON attachment_derived_write FROM v5_fn_owner; END IF;
    FOREACH c IN ARRAY r.add_job_columns LOOP
        EXECUTE format('REVOKE SELECT (%I) ON talk_record_job FROM v5_fn_owner', c);
    END LOOP;
END $$;
DROP TABLE attachment_export_receipt_grant_restore;
