-- Enterprise workspace attachments remain cloud-owned after local history cleanup.
-- This ledger has no automatic expiry; it must not impersonate an OA Application.
CREATE TABLE workspace_attachment_retention (
    attachment_id bigint PRIMARY KEY REFERENCES attachment(id) ON DELETE RESTRICT,
    organization_id bigint NOT NULL REFERENCES organization(id) ON DELETE RESTRICT,
    workspace_id bigint NOT NULL,
    scope text NOT NULL CHECK (scope IN ('group', 'channel')),
    scope_ref text NOT NULL,
    retained_at timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (organization_id, workspace_id)
        REFERENCES workspace(organization_id, id) ON DELETE RESTRICT
);

CREATE INDEX i_war_organization_workspace
    ON workspace_attachment_retention(organization_id, workspace_id, attachment_id);

CREATE FUNCTION fn_workspace_attachment_retention_immutable() RETURNS trigger
LANGUAGE plpgsql AS $$
BEGIN
    RAISE EXCEPTION 'Workspace attachment retention records are immutable'
        USING ERRCODE = '23514';
END;
$$;

CREATE TRIGGER trg_workspace_attachment_retention_immutable
    BEFORE UPDATE OR DELETE ON workspace_attachment_retention
    FOR EACH ROW EXECUTE FUNCTION fn_workspace_attachment_retention_immutable();

CREATE FUNCTION fn_register_workspace_attachment_retention() RETURNS trigger
LANGUAGE plpgsql AS $$
BEGIN
    INSERT INTO workspace_attachment_retention
        (attachment_id, organization_id, workspace_id, scope, scope_ref)
    SELECT NEW.id, w.organization_id, w.id, NEW.scope, NEW.scope_ref
    FROM workspace w
    WHERE w.organization_id IS NOT NULL AND (
        (NEW.scope = 'group' AND EXISTS (
            SELECT 1 FROM "group" g WHERE g.id::text = NEW.scope_ref
                AND g.scope = 'workspace' AND g.workspace_id = w.id)) OR
        (NEW.scope = 'channel' AND EXISTS (
            SELECT 1 FROM channel c WHERE c.id::text = NEW.scope_ref
                AND c.scope = 'workspace' AND c.workspace_id = w.id)))
    ON CONFLICT (attachment_id) DO NOTHING;
    RETURN NEW;
END;
$$;

CREATE TRIGGER trg_register_workspace_attachment_retention
    AFTER INSERT OR UPDATE OF scope, scope_ref ON attachment
    FOR EACH ROW EXECUTE FUNCTION fn_register_workspace_attachment_retention();

CREATE FUNCTION fn_retained_workspace_attachment_scope_guard() RETURNS trigger
LANGUAGE plpgsql AS $$
BEGIN
    IF (NEW.scope, NEW.scope_ref) IS DISTINCT FROM (OLD.scope, OLD.scope_ref)
       AND EXISTS (SELECT 1 FROM workspace_attachment_retention
                   WHERE attachment_id = OLD.id) THEN
        RAISE EXCEPTION 'Retained workspace attachment scope cannot change'
            USING ERRCODE = '23514';
    END IF;
    RETURN NEW;
END;
$$;

CREATE TRIGGER trg_retained_workspace_attachment_scope_guard
    BEFORE UPDATE OF scope, scope_ref ON attachment
    FOR EACH ROW EXECUTE FUNCTION fn_retained_workspace_attachment_scope_guard();

-- Backfill archived and soft-deleted sources as well as active sources.
INSERT INTO workspace_attachment_retention
    (attachment_id, organization_id, workspace_id, scope, scope_ref)
SELECT a.id, w.organization_id, w.id, a.scope, a.scope_ref
FROM attachment a
JOIN workspace w ON w.organization_id IS NOT NULL
WHERE (a.scope = 'group' AND EXISTS (
    SELECT 1 FROM "group" g WHERE g.id::text = a.scope_ref
        AND g.scope = 'workspace' AND g.workspace_id = w.id))
   OR (a.scope = 'channel' AND EXISTS (
    SELECT 1 FROM channel c WHERE c.id::text = a.scope_ref
        AND c.scope = 'workspace' AND c.workspace_id = w.id));

-- A committed tombstone prevents old cleanup jobs deleting reused object keys.
CREATE TABLE attachment_object_deletion (
    path text PRIMARY KEY,
    bucket text NOT NULL,
    attempts integer NOT NULL DEFAULT 0 CHECK (attempts>=0),
    next_retry_at timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP,
    completed_at timestamptz,
    created_at timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP
);
CREATE INDEX i_attachment_object_deletion_pending
    ON attachment_object_deletion(next_retry_at,created_at,path)
    WHERE completed_at IS NULL;
CREATE FUNCTION fn_attachment_deleted_path_guard() RETURNS trigger
LANGUAGE plpgsql AS $$
BEGIN
    PERFORM pg_advisory_xact_lock(hashtextextended(NEW.path, 168));
    IF EXISTS (SELECT 1 FROM attachment_object_deletion WHERE path=NEW.path) THEN
        RAISE EXCEPTION 'Deleted attachment object keys cannot be reused'
            USING ERRCODE='23514';
    END IF;
    RETURN NEW;
END;
$$;
CREATE TRIGGER trg_attachment_deleted_path_guard
    BEFORE INSERT OR UPDATE OF path ON attachment
    FOR EACH ROW EXECUTE FUNCTION fn_attachment_deleted_path_guard();
