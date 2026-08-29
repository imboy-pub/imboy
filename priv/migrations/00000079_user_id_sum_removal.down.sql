-- 00000079 down: 还原 user_id_sum 列与索引（历史 sum 值不可恢复，置 0）
ALTER TABLE "group" ADD COLUMN IF NOT EXISTS user_id_sum bigint DEFAULT 0 NOT NULL;
CREATE INDEX IF NOT EXISTS i_creatorid_memberidsum
    ON "group" USING btree (creator_uid, user_id_sum);
