REVOKE SELECT (object_key, attempt_state, target_key, target_driver, target_namespace,
               target_container, proof_sha256, proof_size)
    ON public.attach_direct_attempt FROM v5_app_runtime;
