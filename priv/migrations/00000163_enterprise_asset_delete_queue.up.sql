CREATE TABLE enterprise_asset_delete_queue (
    organization_id bigint NOT NULL,
    workspace_id bigint NOT NULL,
    asset_id bigint NOT NULL,
    object_key text NOT NULL,
    created_at timestamptz NOT NULL DEFAULT now(),
    PRIMARY KEY (organization_id, workspace_id, asset_id),
    FOREIGN KEY (organization_id, workspace_id)
        REFERENCES workspace (organization_id, id) ON DELETE RESTRICT
);
COMMENT ON TABLE enterprise_asset_delete_queue IS
    'Committed retention purge object deletion intents. Enqueued with metadata deletion and audit; drained only after commit.';
