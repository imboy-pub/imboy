DO $$ BEGIN
IF EXISTS(SELECT 1 FROM talk_record_processor_run WHERE state<>'stopped') THEN
    RAISE EXCEPTION 'processor execution/cleanup not confirmed';
END IF;
END $$;
DROP TABLE talk_record_processor_run;
DROP FUNCTION fn_talk_record_processor_run_transition();
