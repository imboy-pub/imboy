ALTER TABLE enterprise_asset ADD COLUMN pending_object_delete boolean NOT NULL DEFAULT false;
ALTER TABLE enterprise_asset ADD CONSTRAINT ck_enterprise_asset_pending_delete
    CHECK (NOT pending_object_delete OR status = 'deleted');
CREATE INDEX i_enterprise_asset_pending_delete ON enterprise_asset (organization_id, workspace_id, id)
    WHERE pending_object_delete;
COMMENT ON COLUMN enterprise_asset.pending_object_delete IS
    'Durable cleanup intent for expired unconfirmed assets only; clear after object deletion succeeds. Existing tombstones are not backfilled.';
