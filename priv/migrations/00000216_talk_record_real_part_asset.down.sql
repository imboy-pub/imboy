-- Refuse rollback while unconfirmed parts exist; never fabricate asset metadata.
ALTER TABLE talk_record_audio_part ALTER COLUMN attachment_id SET NOT NULL;
ALTER TABLE talk_record_audio_part DROP CONSTRAINT ck_trap_confirmed_asset;
