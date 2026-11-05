-- A session ID is reserved before registration, under the same lock as direct-v1.
CREATE TABLE public.admin_avatar_session_owner (
    client_upload_id uuid PRIMARY KEY,
    admin_user_id bigint NOT NULL CHECK (admin_user_id > 0),
    created_at timestamptz NOT NULL DEFAULT now()
);
GRANT SELECT, INSERT ON public.admin_avatar_session_owner TO v5_app_runtime;
