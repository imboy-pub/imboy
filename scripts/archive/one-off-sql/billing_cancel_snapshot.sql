-- Preparation only: run solely on an independently authorized isolated fixture.
-- Required psql variables: expected_uid, tenant_id, context_json. No defaults.
-- context_json is the externally pinned snapshot context, not an authorization.
-- Use psql -X -qAt -v ON_ERROR_STOP=1; supply connection via approved local env.
-- Never use shared/default connection settings or collect real personal data.
\set ON_ERROR_STOP on
BEGIN TRANSACTION ISOLATION LEVEL REPEATABLE READ READ ONLY;
SET LOCAL TIME ZONE 'UTC';
SET LOCAL statement_timeout = '15s';
-- A mismatched scope must fail before any table is read, including empty sets.
-- gset consumes the guard result so stdout remains one snapshot JSON record.
SELECT 1 / CASE WHEN
    jsonb_typeof(:'context_json'::jsonb) = 'object'
    AND jsonb_typeof(:'context_json'::jsonb -> 'owner_uid') = 'string'
    AND jsonb_typeof(:'context_json'::jsonb -> 'tenant_id') = 'string'
    AND :'expected_uid' ~ '^[1-9][0-9]{0,18}$'
    AND :'tenant_id' ~ '^(0|[1-9][0-9]{0,18})$'
    AND :'context_json'::jsonb ->> 'owner_uid' = :'expected_uid'
    AND :'context_json'::jsonb ->> 'tenant_id' = :'tenant_id'
    THEN 1 ELSE 0 END AS valid
\gset billing_guard_
WITH owned AS (
    SELECT id, tenant_id, plan_id, owner_uid, status,
           current_period_start, current_period_end, auto_renew, created_at, updated_at
    FROM public.billing_subscription
    WHERE owner_uid = :'expected_uid'::bigint AND tenant_id = :'tenant_id'::bigint
), subscriptions AS (
    SELECT id, jsonb_build_object(
        'id', id::text, 'tenant_id', tenant_id::text,
        'plan_id', plan_id::text, 'owner_uid', owner_uid::text,
        'status', status, 'auto_renew', auto_renew,
        'current_period_start', to_char(current_period_start AT TIME ZONE 'UTC',
                                       'YYYY-MM-DD"T"HH24:MI:SS.US"Z"'),
        'current_period_end', to_char(current_period_end AT TIME ZONE 'UTC',
                                     'YYYY-MM-DD"T"HH24:MI:SS.US"Z"'),
        'created_at', to_char(created_at AT TIME ZONE 'UTC',
                              'YYYY-MM-DD"T"HH24:MI:SS.US"Z"'),
        'updated_at', to_char(updated_at AT TIME ZONE 'UTC',
                              'YYYY-MM-DD"T"HH24:MI:SS.US"Z"')
    ) AS row_data FROM owned
), invoices AS (
    SELECT i.id, jsonb_build_object(
        'id', i.id::text, 'invoice_no', i.invoice_no,
        'subscription_id', i.subscription_id::text, 'amount', i.amount,
        'currency', i.currency, 'status', i.status, 'payment_no', i.payment_no,
        'period_start', to_char(i.period_start AT TIME ZONE 'UTC',
                                'YYYY-MM-DD"T"HH24:MI:SS.US"Z"'),
        'period_end', to_char(i.period_end AT TIME ZONE 'UTC',
                              'YYYY-MM-DD"T"HH24:MI:SS.US"Z"'),
        'paid_at', to_char(i.paid_at AT TIME ZONE 'UTC',
                           'YYYY-MM-DD"T"HH24:MI:SS.US"Z"'),
        'created_at', to_char(i.created_at AT TIME ZONE 'UTC',
                              'YYYY-MM-DD"T"HH24:MI:SS.US"Z"'),
        'updated_at', to_char(i.updated_at AT TIME ZONE 'UTC',
                              'YYYY-MM-DD"T"HH24:MI:SS.US"Z"')
    ) AS row_data
    FROM public.billing_invoice i JOIN owned s ON s.id = i.subscription_id
)
SELECT jsonb_build_object(
    'schema_version', 1, 'context', :'context_json'::jsonb,
    'subscriptions', COALESCE((SELECT jsonb_agg(row_data ORDER BY id) FROM subscriptions),
                              '[]'::jsonb),
    'invoices', COALESCE((SELECT jsonb_agg(row_data ORDER BY id) FROM invoices), '[]'::jsonb)
);
COMMIT;
