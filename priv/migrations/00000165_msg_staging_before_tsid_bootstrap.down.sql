-- Preserve accepted messages on rollback. Previous versions use this same table
-- through runtime ensure_table_exists, so dropping it would lose durable data.
SELECT 1;
