-- E2EE-2026-012 / D3: server-authoritative Megolm session attestation.
-- The server stores identifiers and membership generations only, never room-key plaintext.

SET lock_timeout = '5s';
SET statement_timeout = '15min';

CREATE TABLE IF NOT EXISTS public.e2ee_group_session_attestation (
    group_id bigint NOT NULL,
    session_id varchar(256) NOT NULL,
    sender_uid bigint NOT NULL,
    sender_did varchar(128) NOT NULL,
    room_key_msg_id varchar(40) NOT NULL,
    recipient_uids bigint[] NOT NULL,
    start_seq bigint NOT NULL,
    end_seq bigint NOT NULL,
    created_at timestamptz NOT NULL,
    updated_at timestamptz NOT NULL,
    PRIMARY KEY (group_id, session_id),
    UNIQUE (session_id),
    UNIQUE (room_key_msg_id),
    CONSTRAINT chk_e2ee_group_session_ids
        CHECK (group_id > 0 AND sender_uid > 0
               AND octet_length(session_id) BETWEEN 1 AND 256
               AND octet_length(sender_did) BETWEEN 1 AND 128
               AND octet_length(room_key_msg_id) BETWEEN 1 AND 40),
    CONSTRAINT chk_e2ee_group_session_range
        CHECK (start_seq >= 1 AND end_seq >= start_seq),
    CONSTRAINT chk_e2ee_group_session_recipients
        CHECK (array_ndims(recipient_uids) = 1
               AND cardinality(recipient_uids) BETWEEN 1 AND 5000
               AND array_position(recipient_uids, NULL) IS NULL
               AND 0 < ALL (recipient_uids))
);

CREATE TABLE IF NOT EXISTS public.e2ee_group_session_member (
    group_id bigint NOT NULL,
    session_id varchar(256) NOT NULL,
    user_id bigint NOT NULL,
    generation_no integer NOT NULL,
    generation_start_seq bigint NOT NULL,
    PRIMARY KEY (group_id, session_id, user_id),
    CONSTRAINT fk_e2ee_group_session_member_session
        FOREIGN KEY (group_id, session_id)
        REFERENCES public.e2ee_group_session_attestation (group_id, session_id),
    CONSTRAINT chk_e2ee_group_session_member_values
        CHECK (group_id > 0 AND user_id > 0 AND generation_no > 0
               AND generation_start_seq >= 1)
);

CREATE INDEX IF NOT EXISTS idx_e2ee_group_session_member_grant
    ON public.e2ee_group_session_member (group_id, user_id, generation_no, session_id);

COMMENT ON TABLE public.e2ee_group_session_attestation
    IS 'Server-authoritative Megolm session origin, recipient snapshot and monotonic conv_seq range; no key plaintext';

COMMENT ON TABLE public.e2ee_group_session_member
    IS 'Immutable per-recipient membership generation captured when a Megolm room-key message is accepted';
