-- Historical NULL is not consent. Only fresh requests can pin a real opt-in.
ALTER TABLE talk_record_job ADD COLUMN processor_input jsonb;
ALTER TABLE talk_record_job ADD CONSTRAINT ck_trj_processor_input
CHECK(processor_input IS NULL OR jsonb_typeof(processor_input)='object');
