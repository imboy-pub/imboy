-- Export TRACE-00 records from the runtime audit source only.
-- Invoke with: psql -v correlation_id='corr-...' -f this-file

\if :{?correlation_id}
\else
\echo 'correlation_id is required'
\quit 2
\endif

SELECT jsonb_pretty(coalesce(jsonb_agg(jsonb_build_object(
    'correlation_id', correlation_id,
    'entity_type', entity_type,
    'entity_id', entity_id,
    'parent_entity_id', parent_entity_id,
    'timestamp', to_char(
        updated_at AT TIME ZONE 'UTC',
        'YYYY-MM-DD"T"HH24:MI:SS.US"Z"'
    ),
    'status', status
) ORDER BY occurred_at, entity_id), '[]'::jsonb))
FROM public.agent_hub_audit
WHERE correlation_id = :'correlation_id';
