#!/usr/bin/env escript
-mode(compile).
main(_) ->
    code:add_pathsa(["ebin","test","deps/acceptor_pool/ebin","deps/bbmustache/ebin","deps/cf/ebin","deps/chatterbox/ebin","deps/cowboy/ebin","deps/cowlib/ebin","deps/ctx/ebin","deps/depcache/ebin","deps/ecron/ebin","deps/epgsql/ebin","deps/erlang_migrate/ebin","deps/erlang_pay/ebin","deps/erlware_commons/ebin","deps/erlydtl/ebin","deps/fs/ebin","deps/gen_smtp/ebin","deps/goldrush/ebin","deps/gpb/ebin","deps/gproc/ebin","deps/grpcbox/ebin","deps/gun/ebin","deps/hex_core/ebin","deps/hpack/ebin","deps/jose/ebin","deps/jsone/ebin","deps/lager/ebin","deps/meck/ebin","deps/observer_cli/ebin","deps/opentelemetry/ebin","deps/opentelemetry_api/ebin","deps/opentelemetry_exporter/ebin","deps/pooler/ebin","deps/ranch/ebin","deps/recon/ebin","deps/redbug/ebin","deps/relx/ebin","deps/simple_captcha/ebin","deps/ssl_verify_fun/ebin","deps/syn/ebin","deps/sync/ebin","deps/telemetry/ebin","deps/throttle/ebin","deps/tls_certificate_check/ebin","deps/uid/ebin"]),
    true = code:add_patha("/var/folders/8m/wbjj0qmn4ml0mgn56wm56j2m0000gn/T/imboy-e2ee-realcompile-h3ny4nqe"),
    Modules = [e2ee_claimant_scope_drift_tests,e2ee_failure_metric_exposition_tests,e2ee_handler_capability_tests,e2ee_handler_device_binding_tests,e2ee_handler_tests,e2ee_metrics_ip_gate_tests,e2ee_otk_count_tests,e2ee_otk_metric_exposition_tests,e2ee_otk_target_throttle_tests,e2ee_throttle_scope_config_tests,olm_handler_claim_throttle_tests,olm_handler_tests,device_revocation_tests,e2ee_c2g_passthrough_contract_tests,e2ee_offline_sender_did_tests,e2ee_sender_device_envelope_tests,e2ee_v3_passthrough_contract_tests,message_ds_tests,e2ee_safety_contract_tests,e2ee_error_code_tests,e2ee_kt_merkle_tests,e2ee_presign_mime_binding_tests,elib_cipher_e2ee_v2_tests,imboy_codec_tests,olm_otk_cleanup_worker_tests,e2ee_backup_logic_tests,e2ee_batch_claim_idempotency_tests,e2ee_error_privacy_tests,e2ee_fallback_signature_tests,e2ee_logic_tests,e2ee_otk_claim_idempotency_tests,e2ee_otk_exhaustion_metric_tests,e2ee_recovery_logic_tests,e2ee_trust_logic_tests,olm_identity_log_privacy_tests,olm_identity_logic_tests,olm_otk_lifecycle_tests,push_notification_logic_tests,e2ee_backup_repo_tests,msg_store_jsonb_roundtrip_tests,olm_identity_repo_tests],
    lists:foreach(fun(M) ->
        {module, M} = code:ensure_loaded(M),
        true = filename:dirname(code:which(M)) =:= "/var/folders/8m/wbjj0qmn4ml0mgn56wm56j2m0000gn/T/imboy-e2ee-realcompile-h3ny4nqe"
    end, Modules),
    ok = meck:new([epgsql, httpc], [no_link]),
    io:format("OFFLINE_[e2ee_claimant_scope_drift_tests,e2ee_failure_metric_exposition_tests,e2ee_handler_capability_tests,e2ee_handler_device_binding_tests,e2ee_handler_tests,e2ee_metrics_ip_gate_tests,e2ee_otk_count_tests,e2ee_otk_metric_exposition_tests,e2ee_otk_target_throttle_tests,e2ee_throttle_scope_config_tests,olm_handler_claim_throttle_tests,olm_handler_tests,device_revocation_tests,e2ee_c2g_passthrough_contract_tests,e2ee_offline_sender_did_tests,e2ee_sender_device_envelope_tests,e2ee_v3_passthrough_contract_tests,message_ds_tests,e2ee_safety_contract_tests,e2ee_error_code_tests,e2ee_kt_merkle_tests,e2ee_presign_mime_binding_tests,elib_cipher_e2ee_v2_tests,imboy_codec_tests,olm_otk_cleanup_worker_tests,e2ee_backup_logic_tests,e2ee_batch_claim_idempotency_tests,e2ee_error_privacy_tests,e2ee_fallback_signature_tests,e2ee_logic_tests,e2ee_otk_claim_idempotency_tests,e2ee_otk_exhaustion_metric_tests,e2ee_recovery_logic_tests,e2ee_trust_logic_tests,olm_identity_log_privacy_tests,olm_identity_logic_tests,olm_otk_lifecycle_tests,push_notification_logic_tests,e2ee_backup_repo_tests,msg_store_jsonb_roundtrip_tests,olm_identity_repo_tests] ~p~n", [length(Modules)]),
    Result = eunit:test(Modules, [verbose]),
    PgCalls = meck:history(epgsql),
    HttpCalls = meck:history(httpc),
    Started = lists:keymember(imboy, 1, application:which_applications()),
    io:format("BOUNDARY pg_calls=~p http_calls=~p imboy_started=~p~n",
        [length(PgCalls), length(HttpCalls), Started]),
    case {Result, PgCalls, HttpCalls, Started} of
        {ok, [], [], false} -> halt(0);
        _ -> halt(1)
    end.
