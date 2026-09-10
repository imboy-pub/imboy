-- Project persisted fixture rows into the TRACE-00 shape for preflight checks.
-- request/execution/outcome are derived here, not runtime audit records; this
-- export alone cannot satisfy TRACE-00 or E2E-01-A01.
-- Invoke with: psql -v correlation_id='corr-...' -f this-file

\if :{?correlation_id}
\else
\echo 'correlation_id is required'
\quit 2
\endif

WITH tasks AS (
    SELECT *
    FROM public.agent_task
    WHERE correlation_id = :'correlation_id'
),
request_rows AS (
    SELECT
        10 AS sort_order,
        min(created_at) AS event_time,
        jsonb_build_object(
            'correlation_id', :'correlation_id',
            'entity_type', 'request',
            'entity_id', 'req-' || md5(:'correlation_id'),
            'parent_entity_id', NULL,
            'timestamp', to_char(
                min(created_at) AT TIME ZONE 'UTC',
                'YYYY-MM-DD"T"HH24:MI:SS.US"Z"'
            ),
            'status', 'accepted'
        ) AS record
    FROM tasks
    HAVING count(*) > 0
),
task_rows AS (
    SELECT
        20 AS sort_order,
        created_at AS event_time,
        jsonb_build_object(
            'correlation_id', correlation_id,
            'entity_type', 'task',
            'entity_id', id,
            'parent_entity_id', 'req-' || md5(correlation_id),
            'timestamp', to_char(
                created_at AT TIME ZONE 'UTC',
                'YYYY-MM-DD"T"HH24:MI:SS.US"Z"'
            ),
            'status', 'created'
        ) AS record
    FROM tasks
),
event_rows AS (
    SELECT
        30 AS sort_order,
        event.created_at AS event_time,
        jsonb_build_object(
            'correlation_id', event.correlation_id,
            'entity_type', 'event',
            'entity_id', event.id,
            'parent_entity_id', event.task_id,
            'timestamp', to_char(
                event.created_at AT TIME ZONE 'UTC',
                'YYYY-MM-DD"T"HH24:MI:SS.US"Z"'
            ),
            'status', event.status
        ) AS record
    FROM public.agent_task_event AS event
    JOIN tasks ON tasks.id = event.task_id
),
approval_rows AS (
    SELECT
        40 AS sort_order,
        decision.decided_at AS event_time,
        jsonb_build_object(
            'correlation_id', decision.correlation_id,
            'entity_type', 'approval',
            'entity_id', decision.id,
            'parent_entity_id', decision.task_id,
            'timestamp', to_char(
                decision.decided_at AT TIME ZONE 'UTC',
                'YYYY-MM-DD"T"HH24:MI:SS.US"Z"'
            ),
            'status', decision.decision
        ) AS record
    FROM public.agent_task_decision AS decision
    JOIN tasks ON tasks.id = decision.task_id
),
execution_source AS (
    SELECT
        tasks.*,
        decision.id AS decision_id,
        decision.decided_at,
        terminal.created_at AS terminal_at
    FROM tasks
    LEFT JOIN public.agent_task_decision AS decision
        ON decision.task_id = tasks.id
    LEFT JOIN LATERAL (
        SELECT event.created_at
        FROM public.agent_task_event AS event
        WHERE event.task_id = tasks.id
          AND event.status IN ('completed', 'failed', 'cancelled')
        ORDER BY event.created_at DESC
        LIMIT 1
    ) AS terminal ON true
    WHERE tasks.status IN ('completed', 'failed', 'cancelled')
),
execution_rows AS (
    SELECT
        50 AS sort_order,
        greatest(
            coalesce(decided_at, created_at),
            coalesce(terminal_at, updated_at)
        ) AS event_time,
        jsonb_build_object(
            'correlation_id', correlation_id,
            'entity_type', 'execution',
            'entity_id', 'exe-' || md5(id),
            'parent_entity_id', coalesce(decision_id, id),
            'timestamp', to_char(
                greatest(
                    coalesce(decided_at, created_at),
                    coalesce(terminal_at, updated_at)
                ) AT TIME ZONE 'UTC',
                'YYYY-MM-DD"T"HH24:MI:SS.US"Z"'
            ),
            'status', CASE status
                WHEN 'completed' THEN 'succeeded'
                ELSE status
            END
        ) AS record
    FROM execution_source
),
delivery_rows AS (
    SELECT
        60 AS sort_order,
        delivery.updated_at AS event_time,
        jsonb_build_object(
            'correlation_id', delivery.correlation_id,
            'entity_type', 'delivery',
            'entity_id', delivery.delivery_id,
            'parent_entity_id', 'exe-' || md5(tasks.id),
            'timestamp', to_char(
                delivery.updated_at AT TIME ZONE 'UTC',
                'YYYY-MM-DD"T"HH24:MI:SS.US"Z"'
            ),
            'status', CASE delivery.status
                WHEN 'success' THEN 'delivered'
                WHEN 'dead' THEN 'failed'
                ELSE delivery.status
            END
        ) AS record
    FROM public.bot_delivery AS delivery
    JOIN execution_source AS tasks
        ON tasks.correlation_id = delivery.correlation_id
),
outcome_rows AS (
    SELECT
        70 AS sort_order,
        greatest(execution.event_time, delivery.updated_at) AS event_time,
        jsonb_build_object(
            'correlation_id', execution.correlation_id,
            'entity_type', 'outcome',
            'entity_id', 'out-' || md5(execution.correlation_id),
            'parent_entity_id', delivery.delivery_id,
            'timestamp', to_char(
                greatest(execution.event_time, delivery.updated_at) AT TIME ZONE 'UTC',
                'YYYY-MM-DD"T"HH24:MI:SS.US"Z"'
            ),
            'status', CASE execution.status
                WHEN 'completed' THEN 'succeeded'
                ELSE execution.status
            END
        ) AS record
    FROM (
        SELECT
            source.*,
            greatest(
                coalesce(decided_at, created_at),
                coalesce(terminal_at, updated_at)
            ) AS event_time
        FROM execution_source AS source
    ) AS execution
    JOIN LATERAL (
        SELECT candidate.delivery_id, candidate.updated_at
        FROM public.bot_delivery AS candidate
        WHERE candidate.correlation_id = execution.correlation_id
          AND candidate.status IN ('success', 'dead')
        ORDER BY candidate.updated_at DESC, candidate.delivery_id DESC
        LIMIT 1
    ) AS delivery ON true
),
records AS (
    SELECT * FROM request_rows
    UNION ALL SELECT * FROM task_rows
    UNION ALL SELECT * FROM event_rows
    UNION ALL SELECT * FROM approval_rows
    UNION ALL SELECT * FROM execution_rows
    UNION ALL SELECT * FROM delivery_rows
    UNION ALL SELECT * FROM outcome_rows
)
SELECT coalesce(
    jsonb_pretty(jsonb_agg(record ORDER BY event_time, sort_order)),
    '[]'::text
)
FROM records;
