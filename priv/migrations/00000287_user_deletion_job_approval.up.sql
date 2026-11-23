ALTER TABLE public.user_deletion_job
    ADD COLUMN request_status_at_execution text,
    ADD COLUMN request_approved_at timestamptz,
    ADD CONSTRAINT user_deletion_job_request_snapshot_check CHECK (
        CASE request_status_at_execution
            WHEN 'requested' THEN request_approved_at IS NULL AND status = 'completed'
            WHEN 'approved' THEN request_approved_at IS NOT NULL AND status = 'completed'
            ELSE request_status_at_execution IS NULL AND request_approved_at IS NULL
        END
    );
