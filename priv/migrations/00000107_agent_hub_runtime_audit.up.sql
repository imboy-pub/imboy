-- TRACE-00 / E2E-01: Agent Hub runtime audit source of truth.
-- Only frozen chain metadata has columns; payloads, credentials, URLs, and PII cannot be stored.

CREATE TABLE IF NOT EXISTS public.agent_hub_audit (
    entity_id        text        PRIMARY KEY,
    correlation_id   text        NOT NULL,
    entity_type      text        NOT NULL
                     CONSTRAINT agent_hub_audit_entity_type_check CHECK (entity_type IN (
                         'request', 'task', 'event', 'approval',
                         'execution', 'delivery', 'outcome')),
    parent_entity_id text,
    status           text        NOT NULL,
    occurred_at      timestamptz NOT NULL DEFAULT now(),
    updated_at       timestamptz NOT NULL DEFAULT now(),
    CONSTRAINT agent_hub_audit_correlation_format_check
        CHECK (correlation_id ~ '^[A-Za-z0-9_-]{16,64}$'),
    CONSTRAINT agent_hub_audit_entity_id_format_check
        CHECK (entity_id ~ '^[A-Za-z0-9_-]{16,64}$'),
    CONSTRAINT agent_hub_audit_parent_shape_check
        CHECK ((entity_type = 'request' AND parent_entity_id IS NULL)
            OR (entity_type <> 'request' AND parent_entity_id IS NOT NULL)),
    CONSTRAINT agent_hub_audit_status_format_check
        CHECK (status ~ '^[a-z0-9_]{1,32}$'),
    CONSTRAINT agent_hub_audit_corr_entity_uq UNIQUE (correlation_id, entity_id),
    CONSTRAINT agent_hub_audit_parent_fk
        FOREIGN KEY (correlation_id, parent_entity_id)
        REFERENCES public.agent_hub_audit (correlation_id, entity_id)
);

CREATE INDEX IF NOT EXISTS agent_hub_audit_corr_time_idx
    ON public.agent_hub_audit (correlation_id, occurred_at, entity_id);
CREATE UNIQUE INDEX IF NOT EXISTS agent_hub_audit_one_request_uq
    ON public.agent_hub_audit (correlation_id) WHERE entity_type = 'request';
CREATE UNIQUE INDEX IF NOT EXISTS agent_hub_audit_one_outcome_uq
    ON public.agent_hub_audit (correlation_id) WHERE entity_type = 'outcome';

-- Upgrade path: tasks created before this audit table existed still need a
-- trusted request/task root so every later transition can remain fail-closed.
INSERT INTO public.agent_hub_audit (
    entity_id, correlation_id, entity_type, parent_entity_id, status,
    occurred_at, updated_at
)
SELECT
    'req-' || left(encode(sha256(t.correlation_id::bytea), 'hex'), 32),
    t.correlation_id,
    'request',
    NULL,
    'accepted',
    t.created_at,
    t.created_at
FROM public.agent_task AS t
ON CONFLICT DO NOTHING;

INSERT INTO public.agent_hub_audit (
    entity_id, correlation_id, entity_type, parent_entity_id, status,
    occurred_at, updated_at
)
SELECT
    t.id,
    t.correlation_id,
    'task',
    'req-' || left(encode(sha256(t.correlation_id::bytea), 'hex'), 32),
    'created',
    t.created_at,
    t.updated_at
FROM public.agent_task AS t
ON CONFLICT DO NOTHING;

DO $$
BEGIN
    IF EXISTS (
        SELECT 1
        FROM public.agent_task AS t
        LEFT JOIN public.agent_hub_audit AS a
          ON a.correlation_id = t.correlation_id
         AND a.entity_id = t.id
         AND a.entity_type = 'task'
         AND a.parent_entity_id =
             'req-' || left(encode(sha256(t.correlation_id::bytea), 'hex'), 32)
         AND a.status = 'created'
        WHERE a.entity_id IS NULL
    ) THEN
        RAISE EXCEPTION 'agent_hub_audit backfill left tasks without an audit root';
    END IF;
END
$$;

COMMENT ON TABLE public.agent_hub_audit IS
    'TRACE-00 runtime audit source; chain metadata only, never payload, secret, URL, or PII';
