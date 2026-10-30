CREATE TABLE public.c2c_action_receipt (
    request_id       varchar(50) PRIMARY KEY,
    original_msg_id  varchar(40) NOT NULL,
    actor_uid        bigint NOT NULL,
    peer_uid         bigint NOT NULL,
    action           varchar(10) NOT NULL,
    content_sha256   char(64),
    accepted_at      timestamptz NOT NULL DEFAULT clock_timestamp(),
    consumed_at      timestamptz,
    result_payload   jsonb,
    CONSTRAINT ck_c2c_action_receipt_action CHECK (action IN ('revoke', 'edit')),
    CONSTRAINT ck_c2c_action_receipt_content_hash CHECK (
        content_sha256 IS NULL OR content_sha256 ~ '^[0-9a-f]{64}$'
    ),
    CONSTRAINT ck_c2c_action_receipt_consumed CHECK (
        (consumed_at IS NULL AND result_payload IS NULL)
        OR (consumed_at IS NOT NULL AND result_payload IS NOT NULL)
    )
);

CREATE INDEX i_c2c_action_receipt_consumed
    ON public.c2c_action_receipt (original_msg_id, consumed_at DESC)
    WHERE consumed_at IS NOT NULL;

COMMENT ON TABLE public.c2c_action_receipt IS
    '服务端受理的 C2C 编辑/撤回事实；ACK 只能原子消费一次。';
