-- 迁移 169：C2C 持久接受序号及旧数据一致回填。
-- Migration 169: durable C2C acceptance order and legacy reconciliation.
-- Archive order is authoritative. Never infer ordering from sender timestamps.
-- erlang_migrate_pg executes this entire file in one transaction.
-- Lock allocator first, then staging/formal/archive: an accepted transaction
-- finishes before the snapshot or waits until repair has committed.
LOCK TABLE public.msg_store_seq IN SHARE ROW EXCLUSIVE MODE;
LOCK TABLE public.msg_store_staging IN SHARE ROW EXCLUSIVE MODE;
LOCK TABLE public.msg_c2c IN ACCESS EXCLUSIVE MODE;
LOCK TABLE public.msg_store IN SHARE ROW EXCLUSIVE MODE;

ALTER TABLE public.msg_c2c ADD COLUMN IF NOT EXISTS conv_seq bigint;

-- Transaction-local map only; no new durable table, TSID entity or PK change.
CREATE TEMPORARY TABLE personal_order_169_rows ON COMMIT DROP AS
SELECT id, msg_id::text, from_id, to_id, conv_seq FROM public.msg_c2c
UNION ALL
SELECT id, msg_id::text, from_id, to_id, conv_seq
FROM public.msg_store_staging WHERE type = 'c2c'
UNION ALL
SELECT id, msg_id::text, from_id, to_id, conv_seq
FROM public.msg_store WHERE chat_type = 'c2c';

DO $$
BEGIN
    IF EXISTS (SELECT 1 FROM personal_order_169_rows
               WHERE to_id IS NULL OR (conv_seq IS NOT NULL AND conv_seq <= 0)) THEN
        RAISE EXCEPTION 'personal_order169_invalid_identity_or_sequence';
    END IF;
    IF EXISTS (SELECT 1 FROM public.msg_store WHERE chat_type = 'c2c'
               AND conv_key <> 'c2c:' || least(from_id,to_id)::text || ':' || greatest(from_id,to_id)::text) THEN
        RAISE EXCEPTION 'personal_order169_archive_key_conflict';
    END IF;
    IF EXISTS (SELECT msg_id FROM personal_order_169_rows GROUP BY msg_id
               HAVING count(DISTINCT (from_id,to_id)) > 1) THEN
        RAISE EXCEPTION 'personal_order169_message_identity_conflict';
    END IF;
    -- Do not choose an arbitrary archive copy or rewrite an existing seq.
    -- Existing contradictory data requires explicit repair before retry.
    IF EXISTS (SELECT msg_id FROM personal_order_169_rows GROUP BY msg_id
               HAVING count(DISTINCT conv_seq) > 1) THEN
        RAISE EXCEPTION 'personal_order169_message_sequence_conflict';
    END IF;
END $$;

CREATE TEMPORARY TABLE personal_order_169_map ON COMMIT DROP AS
SELECT msg_id, min(id) AS durable_id, min(from_id) AS from_id,
       min(to_id) AS to_id, max(conv_seq) AS conv_seq,
       'c2c:' || least(min(from_id),min(to_id))::text || ':' ||
       greatest(min(from_id),min(to_id))::text AS conv_key
FROM personal_order_169_rows GROUP BY msg_id;
CREATE UNIQUE INDEX ON personal_order_169_map(msg_id);

DO $$
BEGIN
    IF EXISTS (SELECT conv_key,conv_seq FROM personal_order_169_map
               WHERE conv_seq IS NOT NULL GROUP BY conv_key,conv_seq HAVING count(*) > 1) THEN
        RAISE EXCEPTION 'personal_order169_conversation_sequence_collision';
    END IF;
END $$;

-- Repair lower counters, including archive-only conversations; never lower a
-- counter that also represents messages whose transient row was cleaned up.
INSERT INTO public.msg_store_seq (conv_key,seq)
SELECT conv_key,coalesce(max(conv_seq),0) FROM personal_order_169_map GROUP BY conv_key
ON CONFLICT (conv_key) DO UPDATE SET seq = greatest(msg_store_seq.seq,EXCLUDED.seq);

-- The actual legacy acceptance time is unrecoverable for unsequenced rows.
-- Use a deterministic persisted-id proxy only for those rows, above every
-- existing sequence/counter. The migration completes before clear is enabled.
WITH ordered AS (
    SELECT msg_id,conv_key,row_number() OVER
           (PARTITION BY conv_key ORDER BY durable_id,msg_id) AS ordinal
    FROM personal_order_169_map WHERE conv_seq IS NULL
)
UPDATE personal_order_169_map m SET conv_seq = c.seq + o.ordinal
FROM ordered o,public.msg_store_seq c
WHERE m.msg_id=o.msg_id AND c.conv_key=o.conv_key;

UPDATE public.msg_store_seq c SET seq = greatest(c.seq,m.head)
FROM (SELECT conv_key,max(conv_seq) AS head FROM personal_order_169_map GROUP BY conv_key) m
WHERE c.conv_key=m.conv_key;

UPDATE public.msg_c2c f SET conv_seq=m.conv_seq
FROM personal_order_169_map m WHERE f.msg_id=m.msg_id AND f.conv_seq IS NULL;
UPDATE public.msg_store_staging s SET conv_seq=m.conv_seq
FROM personal_order_169_map m WHERE s.type='c2c' AND s.msg_id=m.msg_id AND s.conv_seq IS NULL;

COMMENT ON COLUMN public.msg_c2c.conv_seq IS
    'Server accepted C2C order; nullable for legacy binaries. Migration169 preserves archived sequences; non-destructive down keeps order metadata.';
