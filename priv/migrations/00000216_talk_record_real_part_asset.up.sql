-- Relay-local temporary number: controller assigns the final migration number.
-- Registration records a declaration; only V5 confirmation creates the asset.
ALTER TABLE talk_record_audio_part ALTER COLUMN attachment_id DROP NOT NULL;
ALTER TABLE talk_record_audio_part ADD CONSTRAINT ck_trap_confirmed_asset
    CHECK (upload_status <> 'confirmed' OR attachment_id IS NOT NULL);
