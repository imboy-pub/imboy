DO $$ BEGIN
    LOCK TABLE enterprise_asset_delete_queue IN ACCESS EXCLUSIVE MODE;
    IF EXISTS (SELECT 1 FROM enterprise_asset_delete_queue) THEN
        RAISE EXCEPTION 'object deletion queue must be drained before rollback';
    END IF;
END $$;
DROP TABLE enterprise_asset_delete_queue;
