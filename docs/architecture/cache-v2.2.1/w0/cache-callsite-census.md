# V2.2.1 W0 缓存调用清单（候选 closeout-34858496 重新生成；词法清单，不冒充 AST/动态数据流证明）

生产 literal 调用 126；全范围总计 206（含测试与脚本）；每个调用附外层函数线索。
已知词法限制：宏展开键按文件粒度归属（如 login_attempt_ds:181/186 实际写 {login_attempt_ip_limit,Ip}）；子句头与调用同行时外层函数为 unknown。
动态/不可静态解析形态见 unresolved-forms.json（forwarded_key、derived_string_key、random_binary_key、runtime_keys_via_syn、legacy_set_broadcast、anonymous_memo_mfa）。

| 文件:行 | 函数 | 调用 | 候选命名空间/例外 |
|---|---|---|---|
| scripts/cache_w0_bench.erl:40 | cold_memo_check | depcache:memo | non_production |
| scripts/cache_w0_bench.erl:57 | invalidation_race_check | depcache:memo | non_production |
| scripts/cache_w0_bench.erl:64 | invalidation_race_check | depcache:memo | non_production |
| scripts/cache_w0_bench.erl:69 | invalidation_race_check | depcache:flush | non_production |
| scripts/cache_w0_bench.erl:74 | invalidation_race_check | depcache:get | non_production |
| scripts/cache_w0_bench.erl:85 | loader_exception_check | depcache:memo | non_production |
| scripts/cache_w0_bench.erl:92 | loader_exception_check | depcache:memo | non_production |
| scripts/cache_w0_bench.erl:103 | loader_exception_check | depcache:get | non_production |
| scripts/cache_w0_bench.erl:113 | writer_crash_check | depcache:get_wait | non_production |
| scripts/cache_w0_bench.erl:118 | writer_crash_check | depcache:get_wait | non_production |
| scripts/cache_w0_bench.erl:127 | writer_crash_check | depcache:memo | non_production |
| scripts/cache_w0_bench.erl:147 | handfill_get_wait_check | depcache:get_wait | non_production |
| scripts/cache_w0_bench.erl:149 | handfill_get_wait_check | depcache:get_wait | non_production |
| scripts/cache_w0_bench.erl:153 | handfill_get_wait_check | depcache:set | non_production |
| scripts/cache_w0_bench.erl:162 | setup | depcache:start_link | non_production |
| scripts/cache_w0_bench.erl:164 | setup | depcache:set | non_production |
| scripts/cache_w0_bench.erl:176 | receive_loop | depcache:set | non_production |
| scripts/cache_w0_bench.erl:251 | bench | depcache:size | non_production |
| scripts/cache_w0_bench.erl:252 | bench | depcache:size | non_production |
| scripts/cache_w0_bench.erl:262 | operation | depcache:get | non_production |
| scripts/cache_w0_bench.erl:263 | operation | depcache:get_wait | non_production |
| scripts/cache_w0_bench.erl:264 | operation | depcache:set | non_production |
| scripts/cache_w0_bench.erl:266 | operation | depcache:set | non_production |
| scripts/cache_w0_bench.erl:267 | operation | depcache:get | non_production |
| scripts/cache_w0_bench.erl:268 | operation | depcache:memo | non_production |
| src/adm/adm_acl.erl:111 | flush_cache | imboy_cache:flush | adm_user_permission |
| src/adm/adm_admin_handler.erl:596 | flush_admin_permission_cache | imboy_cache:flush | group, channel, adm_user_permission, adm_user_admin_permission, adm_user_current, adm_user_rbac, adm_user_group_permission, adm_user_moment_permission, adm_user_report_permission, adm_user_feedback_permission, adm_user_stats_permission |
| src/adm/adm_channel_handler.erl:1091 | flush_channel_cache | imboy_cache:flush | channel, channel_subs |
| src/adm/adm_channel_handler.erl:1092 | flush_channel_cache | imboy_cache:flush | channel, channel_subs |
| src/adm/adm_passport_handler.erl:135 | login | imboy_cache:get | csrf |
| src/adm/adm_passport_handler.erl:147 | login | imboy_cache:flush | csrf |
| src/adm/adm_passport_handler.erl:294 | build_login_meta | imboy_cache:set | csrf |
| src/api/passport_handler.erl:104 | validate_bind_mail_params | imboy_cache:get | bind_mail |
| src/api/passport_handler.erl:154 | process_bind_mail | imboy_cache:set | bind_mail |
| src/api/qr_login_handler.erl:438 | generate_qr_token | imboy_cache:set | qr_login, qr_login_qr |
| src/api/qr_login_handler.erl:444 | parse_qr_token | imboy_cache:get | qr_login, qr_login_qr |
| src/api/qr_login_handler.erl:461 | cache_session | imboy_cache:set | qr_login, qr_login_qr |
| src/api/qr_login_handler.erl:468 | get_session | imboy_cache:get | qr_login, qr_login_qr |
| src/api/qr_login_handler.erl:477 | delete_session | imboy_cache:delete | qr_login, qr_login_qr |
| src/ds/account_ds.erl:45 | allocate | imboy_cache:get_wait | local_cache/account_list |
| src/ds/account_ds.erl:54 | allocate | imboy_cache:set | local_cache/account_list |
| src/ds/account_ds.erl:58 | allocate | imboy_cache:set | local_cache/account_list |
| src/ds/account_ds.erl:68 | allocate | imboy_cache:set | local_cache/account_list |
| src/ds/account_ds.erl:71 | allocate | imboy_cache:set | local_cache/account_list |
| src/ds/account_ds.erl:76 | allocate | imboy_cache:set | local_cache/account_list |
| src/ds/adm_session_ds.erl:215 | safe_cache_get | imboy_cache:get | adm_session_epoch |
| src/ds/adm_session_ds.erl:223 | safe_cache_memo | imboy_cache:memo | adm_session_epoch |
| src/ds/adm_session_ds.erl:256 | evict | imboy_cache:delete | adm_session_epoch |
| src/ds/adm_session_ds.erl:257 | evict | imboy_cache:broadcast | adm_session_epoch |
| src/ds/auth_session_ds.erl:66 | safe_cache_get | imboy_cache:get | auth_session_epoch |
| src/ds/auth_session_ds.erl:75 | safe_cache_memo | imboy_cache:memo | auth_session_epoch |
| src/ds/auth_session_ds.erl:170 | evict | imboy_cache:delete | auth_session_epoch |
| src/ds/auth_session_ds.erl:171 | evict | imboy_cache:broadcast | auth_session_epoch |
| src/ds/channel_ds.erl:168 | subscriber_uids | imboy_cache:get | channel, channel_subs |
| src/ds/channel_ds.erl:175 | subscriber_uids | imboy_cache:set | channel, channel_subs |
| src/ds/channel_ds.erl:209 | subscribe | imboy_cache:flush | channel, channel_subs |
| src/ds/channel_ds.erl:210 | subscribe | imboy_cache:flush | channel, channel_subs |
| src/ds/channel_ds.erl:214 | subscribe | imboy_cache:flush | channel, channel_subs |
| src/ds/channel_ds.erl:215 | subscribe | imboy_cache:flush | channel, channel_subs |
| src/ds/channel_ds.erl:256 | unsubscribe | imboy_cache:flush | channel, channel_subs |
| src/ds/channel_ds.erl:257 | unsubscribe | imboy_cache:flush | channel, channel_subs |
| src/ds/channel_ds.erl:261 | unsubscribe | imboy_cache:flush | channel, channel_subs |
| src/ds/channel_ds.erl:262 | unsubscribe | imboy_cache:flush | channel, channel_subs |
| src/ds/channel_ds.erl:576 | flush_subscription_cache | imboy_cache:flush | channel, channel_subs |
| src/ds/channel_ds.erl:577 | flush_subscription_cache | imboy_cache:flush | channel, channel_subs |
| src/ds/config_ds.erl:112 | get | imboy_cache:get | config5 |
| src/ds/config_ds.erl:121 | get | imboy_cache:set | config5 |
| src/ds/config_ds.erl:135 | set | imboy_cache:flush | config5 |
| src/ds/config_ds.erl:167 | save | imboy_cache:flush | config5 |
| src/ds/conversation_delete_ds.erl:35 | delete_conversation | imboy_cache:flush | conv_deleted |
| src/ds/conversation_delete_ds.erl:49 | restore_conversation | imboy_cache:flush | conv_deleted |
| src/ds/conversation_delete_ds.erl:68 | is_conversation_deleted | imboy_cache:memo | conv_deleted |
| src/ds/friend_ds.erl:94 | is_friend | imboy_cache:memo | is_friend2, is_friend_fields2, friend_rel_dep, check_relationship3, is_friend, in_denylist |
| src/ds/friend_ds.erl:120 | is_friend_fields | imboy_cache:memo | is_friend2, is_friend_fields2, friend_rel_dep, check_relationship3, is_friend, in_denylist |
| src/ds/friend_ds.erl:421 | check_relationship | imboy_cache:memo | is_friend2, is_friend_fields2, friend_rel_dep, check_relationship3, is_friend, in_denylist |
| src/ds/friend_ds.erl:507 | invalidate_cache | imboy_cache:flush | is_friend2, is_friend_fields2, friend_rel_dep, check_relationship3, is_friend, in_denylist |
| src/ds/friend_ds.erl:508 | invalidate_cache | imboy_cache:flush | is_friend2, is_friend_fields2, friend_rel_dep, check_relationship3, is_friend, in_denylist |
| src/ds/friend_ds.erl:510 | invalidate_cache | imboy_cache:flush | is_friend2, is_friend_fields2, friend_rel_dep, check_relationship3, is_friend, in_denylist |
| src/ds/friend_ds.erl:511 | invalidate_cache | imboy_cache:flush | is_friend2, is_friend_fields2, friend_rel_dep, check_relationship3, is_friend, in_denylist |
| src/ds/friend_ds.erl:513 | invalidate_cache | imboy_cache:flush | is_friend2, is_friend_fields2, friend_rel_dep, check_relationship3, is_friend, in_denylist |
| src/ds/friend_ds.erl:514 | invalidate_cache | imboy_cache:flush | is_friend2, is_friend_fields2, friend_rel_dep, check_relationship3, is_friend, in_denylist |
| src/ds/group_ds.erl:153 | member_uids_cached | imboy_cache:get | group, group_e2ee_mode |
| src/ds/group_ds.erl:164 | member_uids_cached | imboy_cache:set | group, group_e2ee_mode |
| src/ds/group_ds.erl:233 | e2ee_mode | imboy_cache:get | group, group_e2ee_mode |
| src/ds/group_ds.erl:239 | e2ee_mode | imboy_cache:set | group, group_e2ee_mode |
| src/ds/group_ds.erl:251 | flush_e2ee_mode | imboy_cache:flush | group, group_e2ee_mode |
| src/ds/group_ds.erl:266 | join | imboy_cache:set | group, group_e2ee_mode |
| src/ds/group_ds.erl:272 | join | imboy_cache:set | group, group_e2ee_mode |
| src/ds/group_ds.erl:291 | leave | imboy_cache:set | group, group_e2ee_mode |
| src/ds/group_ds.erl:304 | dissolve | imboy_cache:flush | group, group_e2ee_mode |
| src/ds/login_attempt_ds.erl:179 | check_ip_rate_limit | imboy_cache:get | login_attempt, login_attempt_ip_limit |
| src/ds/login_attempt_ds.erl:181 | check_ip_rate_limit | imboy_cache:set | login_attempt, login_attempt_ip_limit |
| src/ds/login_attempt_ds.erl:186 | check_ip_rate_limit | imboy_cache:set | login_attempt, login_attempt_ip_limit |
| src/ds/moment_ds.erl:384 | list_post_acl | imboy_cache:memo | is_friend, in_denylist, moment_acl |
| src/ds/msg_reaction_ds.erl:186 | clear_reaction_cache | imboy_cache:delete | group, msg_reaction |
| src/ds/push_notification_ds.erl:493 | get_apns_jwt | imboy_cache:get | group, apns_jwt |
| src/ds/push_notification_ds.erl:500 | get_apns_jwt | imboy_cache:set | group, apns_jwt |
| src/ds/user_device_ds.erl:197 | is_active | imboy_cache:get | user_device_active |
| src/ds/user_device_ds.erl:203 | is_active | imboy_cache:memo | user_device_active |
| src/ds/user_dnd_rule_ds.erl:37 | find_by_uid | imboy_cache:get | user_dnd_rule |
| src/ds/user_dnd_rule_ds.erl:40 | find_by_uid | imboy_cache:set | user_dnd_rule |
| src/ds/user_dnd_rule_ds.erl:52 | save | imboy_cache:delete | user_dnd_rule |
| src/ds/user_dnd_rule_ds.erl:62 | delete | imboy_cache:delete | user_dnd_rule |
| src/ds/workspace_ds.erl:674 | member_removed | imboy_cache:flush | group, channel, channel_subs |
| src/ds/workspace_ds.erl:675 | member_removed | imboy_cache:flush | group, channel, channel_subs |
| src/lib/imboy_cache.erl:60 | start_link | depcache:start_link | infrastructure |
| src/lib/imboy_cache.erl:80 | memo | depcache:memo | infrastructure |
| src/lib/imboy_cache.erl:90 | memo | depcache:memo | infrastructure |
| src/lib/imboy_cache.erl:97 | memo | depcache:memo | infrastructure |
| src/lib/imboy_cache.erl:106 | memo | depcache:memo | infrastructure |
| src/lib/imboy_cache.erl:116 | memo | depcache:memo | infrastructure |
| src/lib/imboy_cache.erl:158 | set | depcache:set | infrastructure |
| src/lib/imboy_cache.erl:192 | get_wait | depcache:get_wait | infrastructure |
| src/lib/imboy_cache.erl:200 | get | depcache:get | infrastructure |
| src/lib/imboy_cache.erl:208 | get_subkey | depcache:get_subkey | infrastructure |
| src/lib/imboy_cache.erl:216 | get | depcache:get | infrastructure |
| src/lib/imboy_cache.erl:223 | flush | depcache:flush | infrastructure |
| src/lib/imboy_cache.erl:241 | flush | depcache:flush | infrastructure |
| src/lib/imboy_cache.erl:257 | size | depcache:size | infrastructure |
| src/lib/imboy_cache.erl:263 | in_process_server | depcache:in_process_server | infrastructure |
| src/lib/imboy_cache.erl:270 | in_process_server | depcache:in_process_server | infrastructure |
| src/lib/imboy_cache.erl:277 | in_process | depcache:in_process | infrastructure |
| src/lib/imboy_cache.erl:283 | flush_process_dict | depcache:flush_process_dict | infrastructure |
| src/lib/imboy_cache_sync.erl:93 | handle_sync_message | depcache:set | group |
| src/lib/imboy_cache_sync.erl:96 | handle_sync_message | depcache:flush | group |
| src/lib/imboy_cache_sync.erl:99 | handle_sync_message | depcache:flush | group |
| src/lib/imboy_cluster.erl:263 | update_node_info_cache | imboy_cache:set | cluster_node_info |
| src/lib/qianfan_api.erl:92 | access_token | imboy_cache:memo | qianfan_access_token |
| src/logic/adm_user_logic.erl:33 | find | imboy_cache:memo | adm_user_permission, adm_user_admin_permission, adm_user_account, adm_user_sample, adm_user_current, adm_user_rbac, adm_user_role_ids, adm_user_role_permission, adm_user_group_permission, adm_user_moment_permission, adm_user_report_permission, adm_user_feedback_permission, adm_user_stats_permission |
| src/logic/billing_meter.erl:74 | active_sub_id | imboy_cache:get | billing_meter |
| src/logic/billing_meter.erl:79 | active_sub_id | imboy_cache:set | billing_meter |
| src/logic/channel_logic_subscription.erl:344 | flush_remove_subscriber_cache | imboy_cache:flush | channel, channel_subs |
| src/logic/channel_logic_subscription.erl:345 | flush_remove_subscriber_cache | imboy_cache:flush | channel, channel_subs |
| src/logic/friend_logic.erl:271 | confirm_friend_do | imboy_cache:flush | is_friend |
| src/logic/friend_logic.erl:272 | confirm_friend_do | imboy_cache:flush | is_friend |
| src/logic/friend_logic.erl:352 | delete_friend | imboy_cache:flush | is_friend |
| src/logic/friend_logic.erl:353 | delete_friend | imboy_cache:flush | is_friend |
| src/logic/login_security_logic.erl:160 | build_lock_info | imboy_cache:get | login_attempt |
| src/logic/moderation_policy.erl:113 | cached_words | imboy_cache:memo | channel, moderation_sensitive_words |
| src/logic/user_denylist_logic.erl:71 | add | imboy_cache:flush | in_denylist |
| src/logic/user_denylist_logic.erl:92 | remove | imboy_cache:flush | in_denylist |
| src/logic/user_denylist_logic.erl:107 | in_denylist | imboy_cache:memo | in_denylist |
| src/logic/user_device_logic.erl:65 | device_name | imboy_cache:memo | user_device_active, user_device_name |
| src/logic/user_device_logic.erl:80 | change_name | imboy_cache:flush | user_device_active, user_device_name |
| src/logic/user_device_logic.erl:108 | delete | imboy_cache:flush | user_device_active, user_device_name |
| src/logic/user_device_logic.erl:112 | delete | imboy_cache:flush | user_device_active, user_device_name |
| src/logic/user_device_logic.erl:115 | delete | imboy_cache:broadcast | user_device_active, user_device_name |
| src/push_provider_fcm_token.erl:52 | get_access_token | imboy_cache:get | fcm_access_token_dyn |
| src/push_provider_fcm_token.erl:244 | cache_on_success | imboy_cache:set | fcm_access_token_dyn |
| src/repo/user_tag_relation_repo.erl:296 | tag_subtitle | imboy_cache:memo | tag_subtitle_2, tag_subtitle_3 |
| src/repo/user_tag_relation_repo.erl:302 | flush_subtitle | imboy_cache:flush | tag_subtitle_2, tag_subtitle_3 |
| test/adm/adm_organization_create_tests.erl:235 | ensure_depcache | imboy_cache:start_link | non_production |
| test/adm/adm_organization_create_tests.erl:244 | ensure_depcache | imboy_cache:start_link | non_production |
| test/adm/adm_owner_activation_tests.erl:984 | ensure_depcache | imboy_cache:start_link | non_production |
| test/adm/adm_owner_activation_tests.erl:992 | ensure_depcache | imboy_cache:start_link | non_production |
| test/adm/adm_session_ds_tests.erl:44 | bump_advances_and_survives_cache_flush_test_ | imboy_cache:flush | non_production |
| test/adm/adm_session_ds_tests.erl:72 | fail_closed_when_store_unavailable_test_ | imboy_cache:flush | non_production |
| test/common/eunit_runner.erl:265 | do_ensure_cache | imboy_cache:start_link | non_production |
| test/ds/auth_session_ds_tests.erl:45 | epoch_survives_cache_flush_test_ | imboy_cache:flush | non_production |
| test/friend_management_flow_SUITE.erl:345 | delete_friend_clears_cache | imboy_cache:set | non_production |
| test/friend_management_flow_SUITE.erl:346 | delete_friend_clears_cache | imboy_cache:set | non_production |
| test/friend_management_flow_SUITE.erl:352 | delete_friend_clears_cache | imboy_cache:get | non_production |
| test/friend_management_flow_SUITE.erl:353 | delete_friend_clears_cache | imboy_cache:get | non_production |
| test/integration/conversation_pin_delete_integration_tests.erl:295 | ensure_friends | imboy_cache:flush | non_production |
| test/integration/conversation_pin_delete_integration_tests.erl:296 | ensure_friends | imboy_cache:flush | non_production |
| test/integration/e2ee_message_pipeline_integration_tests.erl:346 | ensure_friends | imboy_cache:flush | non_production |
| test/integration/e2ee_message_pipeline_integration_tests.erl:347 | ensure_friends | imboy_cache:flush | non_production |
| test/integration/micro_community_android_fixture.erl:480 | restore_sign_config | imboy_cache:flush | non_production |
| test/integration/micro_community_owned_http_fixture.erl:135 | run_owned | imboy_cache:start_link | non_production |
| test/integration/micro_community_owned_http_fixture.erl:225 | set_profile | imboy_cache:flush | non_production |
| test/integration/micro_community_owned_http_fixture.erl:614 | cleanup_all | imboy_cache:flush | non_production |
| test/integration/msg_forward_integration_tests.erl:406 | ensure_friends | imboy_cache:flush | non_production |
| test/integration/msg_forward_integration_tests.erl:407 | ensure_friends | imboy_cache:flush | non_production |
| test/integration/msg_reaction_integration_tests.erl:328 | ensure_friends | imboy_cache:flush | non_production |
| test/integration/msg_reaction_integration_tests.erl:329 | ensure_friends | imboy_cache:flush | non_production |
| test/integration/msg_reply_integration_tests.erl:323 | ensure_friends | imboy_cache:flush | non_production |
| test/integration/msg_reply_integration_tests.erl:324 | ensure_friends | imboy_cache:flush | non_production |
| test/lib/imboy_cache_tests.erl:15 | memo_function_forwards_to_depcache_test_ | imboy_cache:memo | non_production |
| test/lib/imboy_cache_tests.erl:33 | memo_tuple_variant_uses_undefined_key_test_ | imboy_cache:memo | non_production |
| test/lib/imboy_cache_tests.erl:50 | memo_with_key_uses_default_hour_test_ | imboy_cache:memo | non_production |
| test/lib/imboy_cache_tests.erl:69 | memo_with_dependencies_forwards_all_arguments_test_ | imboy_cache:memo | non_production |
| test/lib/imboy_cache_tests.erl:92 | set_local_cache_key_skips_broadcast_test_ | imboy_cache:set | non_production |
| test/lib/imboy_cache_tests.erl:114 | set_broadcasts_when_dsync_enabled_test_ | imboy_cache:set | non_production |
| test/lib/imboy_cache_tests.erl:134 | get_variants_forward_to_depcache_test_ | imboy_cache:get | non_production |
| test/lib/imboy_cache_tests.erl:135 | get_variants_forward_to_depcache_test_ | imboy_cache:get | non_production |
| test/lib/imboy_cache_tests.erl:136 | get_variants_forward_to_depcache_test_ | imboy_cache:get_wait | non_production |
| test/lib/imboy_cache_tests.erl:137 | get_variants_forward_to_depcache_test_ | imboy_cache:get_subkey | non_production |
| test/lib/imboy_cache_tests.erl:159 | flush_and_delete_forward_to_depcache_test_ | imboy_cache:flush | non_production |
| test/lib/imboy_cache_tests.erl:160 | flush_and_delete_forward_to_depcache_test_ | imboy_cache:flush | non_production |
| test/lib/imboy_cache_tests.erl:161 | flush_and_delete_forward_to_depcache_test_ | imboy_cache:delete | non_production |
| test/lib/imboy_cache_tests.erl:189 | start_link_extracts_memory_max_test_ | imboy_cache:start_link | non_production |
| test/lib/imboy_cache_tests.erl:207 | size_and_process_helpers_forward_test_ | imboy_cache:size | non_production |
| test/lib/imboy_cache_tests.erl:208 | size_and_process_helpers_forward_test_ | imboy_cache:in_process_server | non_production |
| test/lib/imboy_cache_tests.erl:209 | size_and_process_helpers_forward_test_ | imboy_cache:in_process_server | non_production |
| test/lib/imboy_cache_tests.erl:210 | size_and_process_helpers_forward_test_ | imboy_cache:in_process | non_production |
| test/lib/imboy_cache_tests.erl:211 | size_and_process_helpers_forward_test_ | imboy_cache:flush_process_dict | non_production |
| test/lib/imboy_cache_tests.erl:216 | record_depcache_event_returns_ok_test | imboy_cache:record_depcache_event | non_production |
| test/performance/db_query_performance_tests.erl:228 | ensure_friends | imboy_cache:flush | non_production |
| test/performance/db_query_performance_tests.erl:229 | ensure_friends | imboy_cache:flush | non_production |
| test/performance/msg_send_performance_tests.erl:341 | ensure_friends | imboy_cache:flush | non_production |
| test/performance/msg_send_performance_tests.erl:342 | ensure_friends | imboy_cache:flush | non_production |
| test/rest/support/rest_fixture.erl:185 | ensure_sign_key | imboy_cache:flush | non_production |
| test/rest/support/rest_fixture.erl:186 | ensure_sign_key | imboy_cache:set | non_production |
| test/rest/support/rest_fixture.erl:187 | ensure_sign_key | depcache:flush_process_dict | non_production |
| test/stress/high_concurrency_stress_tests.erl:407 | ensure_friends | imboy_cache:flush | non_production |
| test/stress/high_concurrency_stress_tests.erl:408 | ensure_friends | imboy_cache:flush | non_production |
