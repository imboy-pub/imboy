-- 迁移 00000111: 补齐 C2G 请求账本、权威收件人快照和可重复边界校验
--
-- 不回填历史行：created_at/现有 ACK 状态都不能证明成员世代。NULL 必须由读取侧
-- fail-closed，避免退群后重入时重新取得旧世代消息，尤其是 Megolm room key。

SET lock_timeout = '5s';
SET statement_timeout = '15min';

ALTER TABLE public.msg_c2g_timeline
    ADD COLUMN IF NOT EXISTS conv_seq bigint;

ALTER TABLE public.msg_c2g
    ADD COLUMN IF NOT EXISTS sender_did character varying(128);

DO $constraints$
BEGIN
    IF NOT EXISTS (
        SELECT 1 FROM pg_constraint
        WHERE conname = 'chk_msg_c2g_timeline_conv_seq_positive'
          AND conrelid = 'public.msg_c2g_timeline'::regclass
    ) THEN
        ALTER TABLE public.msg_c2g_timeline
            ADD CONSTRAINT chk_msg_c2g_timeline_conv_seq_positive
            CHECK (conv_seq IS NULL OR conv_seq >= 1);
    END IF;
END
$constraints$;

CREATE INDEX IF NOT EXISTS idx_c2g_timeline_generation_pending
    ON public.msg_c2g_timeline (to_uid, to_gid, conv_seq, created_at)
    WHERE client_ack = false AND conv_seq IS NOT NULL;

COMMENT ON COLUMN public.msg_c2g_timeline.conv_seq
    IS 'C2G persistent-accept sequence; NULL legacy rows are not eligible for offline delivery';

-- 请求账本覆盖正式 msg_c2g 的一年生命周期；收件人快照只负责 action 授权，
-- 不复用会被 offline ACK 删除、且只有 30 天 retention 的 timeline。
CREATE TABLE IF NOT EXISTS public.msg_c2g_request_ledger (
    msg_id varchar(40) PRIMARY KEY,
    from_id bigint NOT NULL,
    to_gid bigint NOT NULL,
    action text,
    request_hash bytea NOT NULL,
    created_at timestamptz NOT NULL,
    CONSTRAINT chk_msg_c2g_request_ledger_msg_id
        CHECK (octet_length(msg_id) BETWEEN 1 AND 40),
    CONSTRAINT chk_msg_c2g_request_ledger_hash
        CHECK (octet_length(request_hash) = 32)
);

CREATE INDEX IF NOT EXISTS idx_msg_c2g_request_ledger_created_at
    ON public.msg_c2g_request_ledger (created_at);

CREATE TABLE IF NOT EXISTS public.msg_c2g_recipient_snapshot (
    msg_id varchar(40) PRIMARY KEY,
    from_id bigint NOT NULL,
    to_gid bigint NOT NULL,
    conv_seq bigint NOT NULL CHECK (conv_seq >= 1),
    recipient_uids bigint[] NOT NULL,
    created_at timestamptz NOT NULL,
    CONSTRAINT chk_msg_c2g_recipient_snapshot_msg_id
        CHECK (octet_length(msg_id) BETWEEN 1 AND 40),
    CONSTRAINT chk_msg_c2g_recipient_snapshot_size
        CHECK (cardinality(recipient_uids) BETWEEN 1 AND 5000),
    CONSTRAINT chk_msg_c2g_recipient_snapshot_shape
        CHECK (array_ndims(recipient_uids) = 1 AND array_position(recipient_uids, NULL) IS NULL),
    CONSTRAINT chk_msg_c2g_recipient_snapshot_positive
        CHECK (0 < ALL (recipient_uids))
);

DO $snapshot_preflight$
BEGIN
    IF EXISTS (
        SELECT 1 FROM public.msg_c2g_recipient_snapshot s
        WHERE octet_length(s.msg_id) NOT BETWEEN 1 AND 40
           OR array_ndims(s.recipient_uids) IS DISTINCT FROM 1
           OR cardinality(s.recipient_uids) NOT BETWEEN 1 AND 5000
           OR array_position(s.recipient_uids, NULL) IS NOT NULL
           OR NOT (0 < ALL (s.recipient_uids))
           OR EXISTS (
               SELECT 1 FROM unnest(s.recipient_uids) AS u(uid)
               GROUP BY uid HAVING count(*) > 1
           )
    ) THEN
        RAISE EXCEPTION 'invalid existing C2G recipient snapshot; migration 111 aborted';
    END IF;
END
$snapshot_preflight$;

ALTER TABLE public.msg_c2g_recipient_snapshot
    ALTER COLUMN msg_id TYPE varchar(40);

DO $snapshot_constraints$
BEGIN
    IF NOT EXISTS (
        SELECT 1 FROM pg_constraint
        WHERE conname = 'chk_msg_c2g_recipient_snapshot_msg_id'
          AND conrelid = 'public.msg_c2g_recipient_snapshot'::regclass
    ) THEN
        ALTER TABLE public.msg_c2g_recipient_snapshot
            ADD CONSTRAINT chk_msg_c2g_recipient_snapshot_msg_id
            CHECK (octet_length(msg_id) BETWEEN 1 AND 40);
    END IF;
    IF NOT EXISTS (
        SELECT 1 FROM pg_constraint
        WHERE conname = 'chk_msg_c2g_recipient_snapshot_size'
          AND conrelid = 'public.msg_c2g_recipient_snapshot'::regclass
    ) THEN
        ALTER TABLE public.msg_c2g_recipient_snapshot
            ADD CONSTRAINT chk_msg_c2g_recipient_snapshot_size
            CHECK (cardinality(recipient_uids) BETWEEN 1 AND 5000);
    END IF;
    IF NOT EXISTS (
        SELECT 1 FROM pg_constraint
        WHERE conname = 'chk_msg_c2g_recipient_snapshot_shape'
          AND conrelid = 'public.msg_c2g_recipient_snapshot'::regclass
    ) THEN
        ALTER TABLE public.msg_c2g_recipient_snapshot
            ADD CONSTRAINT chk_msg_c2g_recipient_snapshot_shape
            CHECK (array_ndims(recipient_uids) = 1
                   AND array_position(recipient_uids, NULL) IS NULL);
    END IF;
    IF NOT EXISTS (
        SELECT 1 FROM pg_constraint
        WHERE conname = 'chk_msg_c2g_recipient_snapshot_positive'
          AND conrelid = 'public.msg_c2g_recipient_snapshot'::regclass
    ) THEN
        ALTER TABLE public.msg_c2g_recipient_snapshot
            ADD CONSTRAINT chk_msg_c2g_recipient_snapshot_positive
            CHECK (0 < ALL (recipient_uids));
    END IF;
END
$snapshot_constraints$;

CREATE INDEX IF NOT EXISTS idx_msg_c2g_recipient_snapshot_group_seq
    ON public.msg_c2g_recipient_snapshot (to_gid, conv_seq);

COMMENT ON TABLE public.msg_c2g_recipient_snapshot
    IS 'Immutable committed C2G recipient snapshot for action authorization';

COMMENT ON TABLE public.msg_c2g_request_ledger
    IS 'Durable C2G message-id and canonical request-identity ledger';

-- 首次启用 111 的发布门会先停止旧节点。旧代码只写 to_id_list，先从服务端已
-- 接受的 C2G 信封恢复 GID，再按每群 staging id 顺序原子分配 conv_seq。
DO $legacy_boundary$
DECLARE
    r record;
    base bigint;
BEGIN
    IF to_regclass('public.msg_store_staging') IS NULL THEN
        RETURN;
    END IF;

    UPDATE public.msg_store_staging
    SET to_id = (payload ->> 'to')::bigint
    WHERE type = 'c2g'
      AND to_id IS NULL
      AND jsonb_typeof(payload) = 'object'
      AND pg_input_is_valid(payload ->> 'to', 'bigint');

    IF EXISTS (
        SELECT 1 FROM public.msg_store_staging s
        WHERE s.type = 'c2g'
          AND (
              s.to_id IS NULL
              OR jsonb_typeof(s.payload) IS DISTINCT FROM 'object'
              OR pg_input_is_valid(s.payload ->> 'to', 'bigint') IS NOT TRUE
              OR (s.payload ->> 'to')::bigint IS DISTINCT FROM s.to_id
              OR s.msg_id IS NULL
              OR octet_length(s.msg_id) NOT BETWEEN 1 AND 40
              OR s.to_id_list IS NULL
              OR array_ndims(s.to_id_list) IS DISTINCT FROM 1
              OR cardinality(s.to_id_list) NOT BETWEEN 1 AND 5000
              OR array_position(s.to_id_list, NULL) IS NOT NULL
              OR NOT (0 < ALL (s.to_id_list))
              OR EXISTS (
                  SELECT 1 FROM unnest(s.to_id_list) AS u(uid)
                  GROUP BY uid HAVING count(*) > 1
              )
          )
    ) THEN
        RAISE EXCEPTION 'invalid retained C2G staging backlog; migration 111 aborted';
    END IF;

    FOR r IN
        SELECT to_id, count(*) AS cnt
        FROM public.msg_store_staging
        WHERE type = 'c2g' AND conv_seq IS NULL AND to_id IS NOT NULL
        GROUP BY to_id
    LOOP
        INSERT INTO public.msg_store_seq (conv_key, seq)
        VALUES ('c2g:' || r.to_id::text, r.cnt)
        ON CONFLICT (conv_key) DO UPDATE
            SET seq = public.msg_store_seq.seq + EXCLUDED.seq
        RETURNING seq - r.cnt INTO base;

        WITH ordered AS (
            SELECT id, row_number() OVER (ORDER BY id) AS rn
            FROM public.msg_store_staging
            WHERE type = 'c2g' AND conv_seq IS NULL AND to_id = r.to_id
        )
        UPDATE public.msg_store_staging s
        SET conv_seq = base + ordered.rn
        FROM ordered
        WHERE s.id = ordered.id;
    END LOOP;

    IF EXISTS (
        SELECT 1 FROM public.msg_store_staging
        WHERE type = 'c2g' AND (conv_seq IS NULL OR conv_seq < 1)
    ) THEN
        RAISE EXCEPTION 'C2G staging backlog lacks a valid conv_seq; migration 111 aborted';
    END IF;
END
$legacy_boundary$;

-- 所有仍保留在 staging 的 C2G 行都携带接受时的完整权威快照，可以安全回填；
-- 已清理且只剩不完整 timeline 的 legacy 消息不回填，旧消息 action fail-closed。
DO $migrate$
BEGIN
    IF to_regclass('public.msg_store_staging') IS NOT NULL THEN
        IF EXISTS (
            SELECT 1
            FROM public.msg_store_staging s
            JOIN public.msg_c2g m ON m.msg_id = s.msg_id
            WHERE s.type = 'c2g'
              AND (
                  m.from_id,
                  m.to_id,
                  digest(
                      convert_to(
                          jsonb_build_array(
                              m.msg_type::text,
                              COALESCE(m.e2ee, 'null'::jsonb),
                              CASE
                                  WHEN jsonb_typeof(m.payload) = 'object'
                                  THEN m.payload - 'server_ts' - 'revoked_at' - 'edited_at'
                                      #- '{payload,server_ts}'
                                      #- '{payload,revoked_at}'
                                      #- '{payload,edited_at}'
                                  ELSE m.payload
                              END,
                              COALESCE(m.sender_did::text, '')
                          )::text,
                          'UTF8'
                      ),
                      'sha256'
                  )
              ) IS DISTINCT FROM (
                  s.from_id,
                  s.to_id,
                  digest(
                      convert_to(
                          jsonb_build_array(
                              s.msg_type::text,
                              COALESCE(s.e2ee, 'null'::jsonb),
                              CASE
                                  WHEN jsonb_typeof(s.payload) = 'object'
                                  THEN s.payload - 'server_ts' - 'revoked_at' - 'edited_at'
                                      #- '{payload,server_ts}'
                                      #- '{payload,revoked_at}'
                                      #- '{payload,edited_at}'
                                  ELSE s.payload
                              END,
                              COALESCE(s.sender_did::text, '')
                          )::text,
                          'UTF8'
                      ),
                      'sha256'
                  )
              )
        ) THEN
            RAISE EXCEPTION 'formal and staged C2G identities conflict; migration 111 aborted';
        END IF;

        IF EXISTS (
            SELECT 1
            FROM public.msg_store_staging s
            JOIN public.msg_c2g_request_ledger ledger ON ledger.msg_id = s.msg_id
            WHERE s.type = 'c2g'
              AND (
                  ledger.from_id,
                  ledger.to_gid,
                  ledger.action,
                  ledger.request_hash
              ) IS DISTINCT FROM (
                  s.from_id,
                  s.to_id,
                  COALESCE(s.action::text, ''),
                  digest(
                      convert_to(
                          jsonb_build_array(
                              s.msg_type::text,
                              COALESCE(s.e2ee, 'null'::jsonb),
                              CASE
                                  WHEN jsonb_typeof(s.payload) = 'object'
                                  THEN s.payload - 'server_ts' - 'revoked_at' - 'edited_at'
                                      #- '{payload,server_ts}'
                                      #- '{payload,revoked_at}'
                                      #- '{payload,edited_at}'
                                  ELSE s.payload
                              END,
                              COALESCE(s.sender_did::text, '')
                          )::text,
                          'UTF8'
                      ),
                      'sha256'
                  )
              )
        ) THEN
            RAISE EXCEPTION 'C2G request ledger conflicts with retained staging; migration 111 aborted';
        END IF;

        IF EXISTS (
            SELECT 1
            FROM public.msg_store_staging s
            JOIN public.msg_c2g_recipient_snapshot snap ON snap.msg_id = s.msg_id
            WHERE s.type = 'c2g'
              AND (
                  snap.from_id,
                  snap.to_gid,
                  snap.conv_seq,
                  snap.recipient_uids,
                  snap.created_at
              )
                  IS DISTINCT FROM
                  (
                      s.from_id,
                      s.to_id,
                      s.conv_seq,
                      s.to_id_list,
                      s.created_at
                  )
        ) THEN
            RAISE EXCEPTION 'C2G snapshot conflicts with retained staging; migration 111 aborted';
        END IF;

        INSERT INTO public.msg_c2g_request_ledger
            (msg_id, from_id, to_gid, action, request_hash, created_at)
        SELECT
            msg_id,
            from_id,
            to_id,
            COALESCE(action::text, ''),
            digest(
                convert_to(
                    jsonb_build_array(
                        msg_type::text,
                        COALESCE(e2ee, 'null'::jsonb),
                        CASE
                            WHEN jsonb_typeof(payload) = 'object'
                            THEN payload - 'server_ts' - 'revoked_at' - 'edited_at'
                                #- '{payload,server_ts}'
                                #- '{payload,revoked_at}'
                                #- '{payload,edited_at}'
                            ELSE payload
                        END,
                        COALESCE(sender_did::text, '')
                    )::text,
                    'UTF8'
                ),
                'sha256'
            ),
            created_at
        FROM public.msg_store_staging
        WHERE type = 'c2g'
          AND to_id IS NOT NULL
          AND conv_seq IS NOT NULL
          AND cardinality(to_id_list) BETWEEN 1 AND 5000
        ON CONFLICT (msg_id) DO NOTHING;

        INSERT INTO public.msg_c2g_recipient_snapshot
            (msg_id, from_id, to_gid, conv_seq, recipient_uids, created_at)
        SELECT msg_id, from_id, to_id, conv_seq, to_id_list, created_at
        FROM public.msg_store_staging
        WHERE type = 'c2g'
          AND to_id IS NOT NULL
          AND conv_seq IS NOT NULL
          AND cardinality(to_id_list) BETWEEN 1 AND 5000
        ON CONFLICT (msg_id) DO NOTHING;
    END IF;
END
$migrate$;

DO $formal_ledger$
BEGIN
    IF EXISTS (
        SELECT msg_id
        FROM public.msg_c2g m
        GROUP BY msg_id
        HAVING count(DISTINCT (
            from_id,
            to_id,
            encode(
                digest(
                    convert_to(
                        jsonb_build_array(
                            msg_type::text,
                            COALESCE(e2ee, 'null'::jsonb),
                            CASE
                                WHEN jsonb_typeof(payload) = 'object'
                                THEN payload - 'server_ts' - 'revoked_at' - 'edited_at'
                                    #- '{payload,server_ts}'
                                    #- '{payload,revoked_at}'
                                    #- '{payload,edited_at}'
                                ELSE payload
                            END,
                            COALESCE(sender_did::text, '')
                        )::text,
                        'UTF8'
                    ),
                    'sha256'
                ),
                'hex'
            )
        )) > 1
    ) THEN
        RAISE EXCEPTION 'historical C2G msg_id has conflicting identities; migration 111 aborted';
    END IF;

    IF EXISTS (
        SELECT 1
        FROM public.msg_c2g m
        JOIN public.msg_c2g_request_ledger ledger ON ledger.msg_id = m.msg_id
        WHERE (
              ledger.from_id,
              ledger.to_gid,
              ledger.request_hash
          ) IS DISTINCT FROM (
              m.from_id,
              m.to_id,
              digest(
                  convert_to(
                      jsonb_build_array(
                          m.msg_type::text,
                          COALESCE(m.e2ee, 'null'::jsonb),
                          CASE
                              WHEN jsonb_typeof(m.payload) = 'object'
                              THEN m.payload - 'server_ts' - 'revoked_at' - 'edited_at'
                                  #- '{payload,server_ts}'
                                  #- '{payload,revoked_at}'
                                  #- '{payload,edited_at}'
                              ELSE m.payload
                          END,
                          COALESCE(m.sender_did::text, '')
                      )::text,
                      'UTF8'
                  ),
                  'sha256'
              )
          )
    ) THEN
        RAISE EXCEPTION 'C2G request ledger conflicts with formal message; migration 111 aborted';
    END IF;

    INSERT INTO public.msg_c2g_request_ledger
        (msg_id, from_id, to_gid, action, request_hash, created_at)
    SELECT DISTINCT ON (m.msg_id)
        m.msg_id,
        m.from_id,
        m.to_id,
        NULL,
        digest(
            convert_to(
                jsonb_build_array(
                    m.msg_type::text,
                    COALESCE(m.e2ee, 'null'::jsonb),
                    CASE
                        WHEN jsonb_typeof(m.payload) = 'object'
                        THEN m.payload - 'server_ts' - 'revoked_at' - 'edited_at'
                            #- '{payload,server_ts}'
                            #- '{payload,revoked_at}'
                            #- '{payload,edited_at}'
                        ELSE m.payload
                    END,
                    COALESCE(m.sender_did::text, '')
                )::text,
                'UTF8'
            ),
            'sha256'
        ),
        m.created_at
    FROM public.msg_c2g m
    ORDER BY m.msg_id, m.created_at
    ON CONFLICT (msg_id) DO NOTHING;
END
$formal_ledger$;

COMMENT ON COLUMN public.msg_c2g.sender_did
    IS 'Server-authenticated sender device id for PFv3 offline context binding';

RESET lock_timeout;
RESET statement_timeout;
