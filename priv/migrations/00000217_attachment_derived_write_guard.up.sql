-- Server generated artifacts use the ordinary attachment lifecycle, without a
-- fabricated user upload session. Unknown writes remain quarantined just like
-- direct-v1 unknown Copy attempts; lease expiry never proves remote I/O ended.
CREATE TABLE attachment_derived_write (
    attachment_id bigint PRIMARY KEY REFERENCES attachment(id),
    source_job_id bigint NOT NULL,
    source_attempt integer NOT NULL,
    state text NOT NULL DEFAULT 'reserved'
      CHECK(state IN ('reserved','writing','complete','unknown','abandoned')),
    created_at timestamptz NOT NULL DEFAULT now(),
    updated_at timestamptz NOT NULL DEFAULT now()
);
CREATE FUNCTION fn_attachment_derived_write_guard() RETURNS trigger
LANGUAGE plpgsql SET search_path=public AS $$
BEGIN
    IF OLD.lifecycle_state='live' AND NEW.lifecycle_state<>'live'
       AND EXISTS(SELECT 1 FROM attachment_derived_write w
                  WHERE w.attachment_id=OLD.id AND w.state IN ('reserved','writing','unknown')) THEN
        RAISE EXCEPTION 'attachment derived write not drained' USING ERRCODE='23510';
    END IF;
    RETURN NEW;
END $$;
CREATE TRIGGER attachment_derived_write_guard BEFORE UPDATE OF lifecycle_state ON attachment
FOR EACH ROW EXECUTE FUNCTION fn_attachment_derived_write_guard();
CREATE UNIQUE INDEX IF NOT EXISTS u_talk_record_export_job ON talk_record_export(job_id)
WHERE job_id IS NOT NULL;
-- Historic inline content is deliberately not accepted by the new download path.
-- Legacy content cleanup is owned by the existing retention/purge policy.

GRANT SELECT ON attachment_derived_write TO v5_fn_owner;
GRANT SELECT,INSERT,UPDATE ON attachment_derived_write TO v5_app_runtime;

CREATE FUNCTION fn_attachment_derived_write_transition() RETURNS trigger
LANGUAGE plpgsql AS $$
BEGIN
    IF NEW.attachment_id IS DISTINCT FROM OLD.attachment_id
       OR NEW.source_job_id IS DISTINCT FROM OLD.source_job_id
       OR NEW.source_attempt IS DISTINCT FROM OLD.source_attempt THEN
        RAISE EXCEPTION 'derived write source identity is immutable' USING ERRCODE='23514';
    END IF;
    IF NEW.state<>OLD.state AND NOT (
        (OLD.state='reserved' AND NEW.state IN ('writing','abandoned'))
        OR (OLD.state='writing' AND NEW.state IN ('complete','unknown'))) THEN
        RAISE EXCEPTION 'illegal derived write transition' USING ERRCODE='23514';
    END IF;
    RETURN NEW;
END $$;
CREATE TRIGGER attachment_derived_write_transition BEFORE UPDATE ON attachment_derived_write
FOR EACH ROW EXECUTE FUNCTION fn_attachment_derived_write_transition();
