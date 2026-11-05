-- Record accepted group revocations before asynchronous delivery removes staging.
CREATE TABLE public.c2g_revoke_capture_epoch (
    singleton boolean PRIMARY KEY DEFAULT true CHECK (singleton),
    captured_since timestamptz NOT NULL DEFAULT clock_timestamp()
);
INSERT INTO public.c2g_revoke_capture_epoch(singleton) VALUES (true);

CREATE TABLE public.c2g_revoke_fact (
    group_id bigint NOT NULL,
    original_msg_id varchar(40) NOT NULL,
    action_msg_id varchar(40) NOT NULL,
    actor_uid bigint NOT NULL,
    accepted_seq bigint NOT NULL CHECK (accepted_seq > 0),
    accepted_at timestamptz NOT NULL DEFAULT clock_timestamp(),
    PRIMARY KEY (group_id, original_msg_id),
    UNIQUE (action_msg_id)
);
GRANT SELECT ON public.c2g_revoke_capture_epoch TO v5_app_runtime;
-- Message staging and its trusted writer use the configured message-writer
-- role. The attachment runtime may read revocations but must not forge them.
GRANT SELECT ON public.c2g_revoke_fact TO v5_app_runtime;
