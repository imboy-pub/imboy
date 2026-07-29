DROP INDEX IF EXISTS idx_billing_sub_owner;
--;

ALTER TABLE public.billing_subscription
    DROP COLUMN IF EXISTS owner_uid;
--;
