# 需要充值 id_seq 的表

select setval('"app_version_id_seq"', (select max(id) from public."app_version"));

select setval('"app_ddl_id_seq"', (select max(id) from public."app_ddl"));

select setval('"feedback_id_seq"', (select max(id) from public."feedback"));

select setval('"feedback_reply_id_seq"', (select max(id) from public."feedback_reply"));

select setval('"user_id_seq"', (select max(id) from public."user"));

select setval('"user_device_id_seq"', (select max(id) from public."user_device"));

select setval('"user_denylist_id_seq"', (select max(id) from public."user_denylist"));

select setval('"user_friend_category_id_seq"', (select max(id) from public."user_friend_category"));

select setval('"user_friend_id_seq"', (select max(id) from public."user_friend"));

select setval('"group_id_seq"', (select max(id) from public."group"));

select setval('"group_member_id_seq"', (select max(id) from public."group_member"));

select setval('"group_log_id_seq"', (select max(id) from public."group_log"));

select setval('"group_random_code_id_seq"', (select max(id) from public."group_random_code"));

select setval('"group_notice_id_seq"', (select max(id) from public."group_notice"));

select setval('"user_group_id_seq"', (select max(id) from public."user_group"));

select setval('"conversation_id_seq"', (select max(id) from public."conversation"));

select setval('"msg_c2c_id_seq"', (select max(id) from public."msg_c2c"));

select setval('"msg_c2g_id_seq"', (select max(id) from public."msg_c2g"));

select setval('"msg_c2s_id_seq"', (select max(id) from public."msg_c2s"));

select setval('"msg_s2c_id_seq"', (select max(id) from public."msg_s2c"));

select setval('"msg_topic_id_seq"', (select max(id) from public."msg_topic"));

select setval('"adm_user_id_seq"', (select max(id) from public."adm_user"));

select setval('"adm_role_id_seq"', (select max(id) from public."adm_role"));
