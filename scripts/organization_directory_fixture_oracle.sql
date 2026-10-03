-- Preparation only. Run exclusively on an explicitly authorized isolated
-- synthetic fixture database after independent resource/lease verification.
-- No real-user data or contacts may be emitted. This query returns booleans only.
\set ON_ERROR_STOP on
BEGIN TRANSACTION ISOLATION LEVEL REPEATABLE READ READ ONLY;
SET LOCAL statement_timeout = '15s';
SELECT 1 / CASE WHEN
    jsonb_typeof(:'context_json'::jsonb) = 'object'
    AND jsonb_typeof(:'context_json'::jsonb -> 'expected_uid') = 'string'
    AND jsonb_typeof(:'context_json'::jsonb -> 'organization_id') = 'string'
    AND jsonb_typeof(:'context_json'::jsonb -> 'member_id') = 'string'
    AND jsonb_typeof(:'context_json'::jsonb -> 'root_page_limit') = 'string'
    AND jsonb_typeof(:'context_json'::jsonb -> 'fixture_id') = 'string'
    AND :'expected_uid' ~ '^[1-9][0-9]{0,18}$'
    AND :'organization_id' ~ '^[1-9][0-9]{0,18}$'
    AND :'member_id' ~ '^[1-9][0-9]{0,18}$'
    AND :'root_page_limit' ~ '^[1-9][0-9]{0,2}$'
    AND :'root_page_limit'::integer BETWEEN 1 AND 100
    AND :'context_json'::jsonb ->> 'expected_uid' = :'expected_uid'
    AND :'context_json'::jsonb ->> 'organization_id' = :'organization_id'
    AND :'context_json'::jsonb ->> 'member_id' = :'member_id'
    AND :'context_json'::jsonb ->> 'root_page_limit' = :'root_page_limit'
    AND length(:'context_json'::jsonb ->> 'fixture_id') > 0
    THEN 1 ELSE 0 END AS valid
\gset org_fixture_guard_
WITH roots AS (
    SELECT om.user_id
    FROM public.organization_member om
    JOIN public."user" u ON u.id = om.user_id
    WHERE om.organization_id = :'organization_id'::bigint
      AND om.status = 'active' AND om.user_id > 0
      AND NOT EXISTS (
        SELECT 1 FROM public.organization_department_member dm
        JOIN public.organization_department d ON d.id = dm.department_id
          AND d.status = 'active' AND d.organization_id = om.organization_id
        WHERE dm.organization_id = om.organization_id AND dm.user_id = om.user_id
      )
    ORDER BY om.user_id ASC
    LIMIT :'root_page_limit'::integer
)
SELECT jsonb_build_object(
    'schema_version', 1,
    'organization_active', EXISTS (
        SELECT 1 FROM public.organization
        WHERE id = :'organization_id'::bigint AND status = 'active'),
    'viewer_active_member', EXISTS (
        SELECT 1 FROM public.organization_member
        WHERE organization_id = :'organization_id'::bigint
          AND user_id = :'expected_uid'::bigint AND status = 'active'),
    'fixture_member_on_root_first_page', EXISTS (
        SELECT 1 FROM roots WHERE user_id = :'member_id'::bigint)
);
COMMIT;
