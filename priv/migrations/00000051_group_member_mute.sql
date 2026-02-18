ALTER TABLE group_member ADD COLUMN IF NOT EXISTS mute_until TIMESTAMPTZ DEFAULT NULL;
CREATE INDEX IF NOT EXISTS idx_group_member_mute ON group_member(group_id, user_id) WHERE mute_until IS NOT NULL;