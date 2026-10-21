DO $$ BEGIN
    IF EXISTS(SELECT 1 FROM attachment_derived_write WHERE state IN ('reserved','writing','unknown')) THEN
        RAISE EXCEPTION 'cannot discard outstanding derived writes';
    END IF;
END $$;
DROP INDEX IF EXISTS u_talk_record_export_job;
DROP TRIGGER attachment_derived_write_guard ON attachment;
DROP FUNCTION fn_attachment_derived_write_guard();
DROP TRIGGER attachment_derived_write_transition ON attachment_derived_write;
DROP FUNCTION fn_attachment_derived_write_transition();
DROP TABLE attachment_derived_write;
