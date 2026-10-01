DO $$ BEGIN
    IF EXISTS (SELECT 1 FROM enterprise_asset WHERE pending_object_delete) THEN
        RAISE EXCEPTION 'pending object cleanup must complete before rollback';
    END IF;
END $$;
DROP INDEX i_enterprise_asset_pending_delete;
ALTER TABLE enterprise_asset DROP CONSTRAINT ck_enterprise_asset_pending_delete;
ALTER TABLE enterprise_asset DROP COLUMN pending_object_delete;
