\set ON_ERROR_STOP on
-- Focused migration-108 probe: synthetic pre-108 columns, not a full-schema gate.
CREATE TABLE public.attachment (
    id bigint PRIMARY KEY, path text NOT NULL, scope text NOT NULL,
    scope_ref text, creator_user_id bigint, status smallint NOT NULL
);
CREATE TABLE public.group_file (
    id bigint PRIMARY KEY, group_id bigint NOT NULL,
    file_id varchar(40) UNIQUE NOT NULL, file_name text NOT NULL,
    uploader_id bigint NOT NULL
);
CREATE TABLE public.group_member_generation (
    group_id bigint, user_id bigint, end_seq bigint
);

-- Empty database up/down, then synthetic legacy rows and a second roundtrip.
\i priv/migrations/00000108_group_attachment_anchor.up.sql
\i priv/migrations/00000108_group_attachment_anchor.down.sql
INSERT INTO group_file VALUES (11, 21, 'legacy-id', 'folder/report.pdf', 31);
INSERT INTO attachment VALUES
    (1, 'legacy-id/report.pdf', 'group', '21', 31, 1),
    (2, 'legacy-id/report.pdf', 'group', '22', 31, 1),
    (3, 'legacy-id/report.pdf', 'group', '21', 32, 1),
    (4, 'legacy-id/report.pdf', 'private', '21', 31, 1),
    (5, 'unknown/report.pdf', 'group', '21', 31, 1),
    (6, 'legacy-id/report.pdf', 'group', 'invalid-group', 31, 1);
CREATE TEMP TABLE original_attachment AS TABLE attachment;
\i priv/migrations/00000108_group_attachment_anchor.up.sql
DO $$ BEGIN
    IF (SELECT group_file_id FROM attachment WHERE id=1) IS DISTINCT FROM 11
       OR EXISTS (SELECT FROM attachment WHERE id<>1 AND group_file_id IS NOT NULL)
       OR EXISTS (SELECT FROM attachment WHERE id=1 AND anchor_conv_seq IS NOT NULL)
       OR EXISTS (SELECT FROM attachment WHERE id IN (2,3,5,6)
                  AND anchor_conv_seq IS DISTINCT FROM 1)
       OR EXISTS (SELECT FROM attachment WHERE id=4 AND anchor_conv_seq IS NOT NULL)
    THEN RAISE EXCEPTION 'legacy binding or chat compatibility mismatch'; END IF;
END $$;
\i priv/migrations/00000108_group_attachment_anchor.down.sql
DO $$ BEGIN
    IF EXISTS ((TABLE attachment EXCEPT TABLE original_attachment)
               UNION ALL (TABLE original_attachment EXCEPT TABLE attachment))
    THEN RAISE EXCEPTION 'rollback changed original metadata'; END IF;
END $$;
\i priv/migrations/00000108_group_attachment_anchor.up.sql
DO $$ BEGIN
    IF (SELECT count(*) FROM attachment WHERE group_file_id=11) <> 1
       OR EXISTS (SELECT FROM attachment WHERE id<>1 AND group_file_id IS NOT NULL)
    THEN RAISE EXCEPTION 'reapply changed exact legacy binding'; END IF;
END $$;
SELECT 'PASS_LEGACY_GROUP_FILE_MIGRATION_EMPTY_SYNTHETIC_ROUNDTRIP' AS result;
