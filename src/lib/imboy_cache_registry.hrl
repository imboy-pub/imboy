%% W0 Registry 投影：namespace => {Policy, Sensitive}
%% local 9 / invalidate 36 / replicate 1 / G-QR_BLOCKED 2，共 48 条（19.4 零静默缺漏）。
-define(REGISTRY, #{
    %% --- local（本地重建，禁止跨节点广播）---
    local_cache => {local, true},
    apns_jwt => {local, true},
    bind_mail => {local, true},
    cluster_node_info => {local, false},
    fcm_access_token_dyn => {local, true},
    qianfan_access_token => {local, true},
    login_attempt => {local, true},
    login_attempt_ip_limit => {local, true},
    account_list => {local, true},
    %% --- invalidate（数据库派生，变更后失效）---
    adm_session_epoch => {invalidate, true},
    adm_user_account => {invalidate, true},
    adm_user_admin_permission => {invalidate, true},
    adm_user_current => {invalidate, true},
    adm_user_feedback_permission => {invalidate, true},
    adm_user_group_permission => {invalidate, true},
    adm_user_moment_permission => {invalidate, true},
    adm_user_permission => {invalidate, true},
    adm_user_rbac => {invalidate, true},
    adm_user_report_permission => {invalidate, true},
    adm_user_role_ids => {invalidate, true},
    adm_user_role_permission => {invalidate, true},
    adm_user_sample => {invalidate, true},
    adm_user_stats_permission => {invalidate, true},
    auth_session_epoch => {invalidate, true},
    billing_meter => {invalidate, true},
    channel => {invalidate, false},
    channel_subs => {invalidate, true},
    check_relationship3 => {invalidate, true},
    config5 => {invalidate, true},
    conv_deleted => {invalidate, true},
    friend_rel_dep => {invalidate, false},
    group => {invalidate, true},
    group_e2ee_mode => {invalidate, true},
    in_denylist => {invalidate, true},
    is_friend => {invalidate, true},
    is_friend2 => {invalidate, true},
    is_friend_fields2 => {invalidate, true},
    moderation_sensitive_words => {invalidate, true},
    moment_acl => {invalidate, true},
    msg_reaction => {invalidate, false},
    tag_subtitle_2 => {invalidate, true},
    tag_subtitle_3 => {invalidate, true},
    user_device_active => {invalidate, true},
    user_device_name => {invalidate, true},
    user_dnd_rule => {invalidate, true},
    user_tag_relation => {invalidate, true},
    %% --- replicate（仅显式白名单启用的非敏感、可重建数据）---
    %% Never use for authorization, tokens, counters, locks or atomic consumption.
    shared_cache => {replicate, false},
    %% --- blocked（G-QR 未决：19.2 QR 两类 Key 禁止 replicate，
    %%     不因测试开关或配置绕过；修复须走独立方案与 Schema 决策）---
    qr_login => {blocked, true},
    qr_login_qr => {blocked, true}
}).

%% 冻结本地预算默认值（原policy常量逐字迁移；非生产SLO）。
-define(SKEW_SEC, 5).
-define(DEFAULT_MAX_TTL_SEC, 600).
-define(DEFAULT_MAX_VALUE_BYTES, 32768).
-define(DEFAULT_BATCH_SIZE, 256).
-define(DEFAULT_RECOVERY_WINDOW_SEC, 300).
-define(DEFAULT_TOMBSTONE_TTL_SEC, 720).
-define(DEFAULT_N_MAX, 50000).
-define(DEFAULT_T_MAX, 20000).
-define(DEFAULT_B_MAX, 2048).
-define(DEFAULT_P_MAX, 256).
-define(DEFAULT_P_MAX_BYTES, 4194304).
-define(DEFAULT_R_MAX_ROUNDS, 3).
-define(DEFAULT_HIGH_WATERMARK, 10000).
