-- Populated retention ledgers require backup restoration, never silent disposal.
DO $$
BEGIN
    IF EXISTS (SELECT 1 FROM workspace_attachment_retention)
       OR EXISTS (SELECT 1 FROM attachment_object_deletion) THEN
        RAISE EXCEPTION 'Restore a verified backup to roll back populated retention'
            USING ERRCODE = '23514';
    END IF;
END;
$$;
DROP TRIGGER trg_retained_workspace_attachment_scope_guard ON attachment;
DROP FUNCTION fn_retained_workspace_attachment_scope_guard();
DROP TRIGGER trg_register_workspace_attachment_retention ON attachment;
DROP FUNCTION fn_register_workspace_attachment_retention();
DROP TABLE workspace_attachment_retention;
DROP FUNCTION fn_workspace_attachment_retention_immutable();

DROP TRIGGER trg_attachment_deleted_path_guard ON attachment;
DROP FUNCTION fn_attachment_deleted_path_guard();
DROP TABLE attachment_object_deletion;
