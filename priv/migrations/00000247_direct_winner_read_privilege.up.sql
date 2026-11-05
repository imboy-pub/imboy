-- The runtime validates a confirmed winner without granting access to the full attempt row.
GRANT SELECT (object_key, attempt_state, target_key, target_driver, target_namespace,
              target_container, proof_sha256, proof_size)
    ON public.attach_direct_attempt TO v5_app_runtime;
