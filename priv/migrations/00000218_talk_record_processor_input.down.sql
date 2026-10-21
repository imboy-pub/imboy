DO $$ BEGIN
IF EXISTS(SELECT 1 FROM talk_record_job WHERE processor_input IS NOT NULL) THEN
    RAISE EXCEPTION 'processor inputs exist: rollback refused';
END IF;
END $$;
ALTER TABLE talk_record_job DROP COLUMN processor_input;
