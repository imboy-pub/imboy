# W0 Key Registry（迁移决策；不是已实现的运行时策略）

复制白名单为空；所有未闭合 mutation hook/大小预算明确阻塞后续切换。

| Namespace | Key | 策略 | 权威 | TTL 秒 | 敏感 | Owner |
|---|---|---|---|---|---|---|
| local_cache/account_list | {local_cache,account_list} | local | PG sequence；现有热命中读改写非原子 | 8640000/空值3 | True | ['src/ds/account_ds.erl'] |
| is_friend2 | {is_friend2,FromUid,ToUid} | invalidate | PostgreSQL；缓存非权威 | 300 | True | ['src/ds/friend_ds.erl'] |
| is_friend_fields2 | {is_friend_fields2,FromUid,ToUid,Fields} | invalidate | PostgreSQL；缓存非权威 | 300 | True | ['src/ds/friend_ds.erl'] |
| friend_rel_dep | {friend_rel_dep,FromUid,ToUid} | invalidate | 依赖标签；不存业务值 | 随依赖 | False | ['src/ds/friend_ds.erl'] |
| check_relationship3 | {check_relationship3,FromUid,ToUid} | invalidate | PostgreSQL；缓存非权威 | 300 | True | ['src/ds/friend_ds.erl'] |
| is_friend | {is_friend,FromUid,ToUid} | invalidate | legacy 失效标签；未发现生产写入 | 无写入 | True | ['src/ds/friend_ds.erl', 'src/ds/moment_ds.erl', 'src/logic/friend_logic.erl'] |
| in_denylist | {in_denylist,Uid,DeniedUid} | invalidate | PostgreSQL；缓存非权威 | 864000 | True | ['src/ds/friend_ds.erl', 'src/ds/moment_ds.erl', 'src/logic/user_denylist_logic.erl'] |
| auth_session_epoch | {auth_session_epoch,Uid} | invalidate | PostgreSQL；缓存非权威 | 60 | True | ['src/ds/auth_session_ds.erl'] |
| adm_session_epoch | {adm_session_epoch,AdmId} | invalidate | PostgreSQL；缓存非权威 | 60 | True | ['src/ds/adm_session_ds.erl'] |
| user_device_active | {user_device_active,Uid,DID} | invalidate | PostgreSQL；缓存非权威 | 60 | True | ['src/ds/user_device_ds.erl', 'src/logic/user_device_logic.erl'] |
| user_device_name | {user_device_name,2,Uid,DID} | invalidate | PostgreSQL；缓存非权威 | 864000 | True | ['src/logic/user_device_logic.erl'] |
| group | {group,Gid} | invalidate | PostgreSQL；缓存非权威 | 3600 | True | ['src/adm/adm_admin_handler.erl', 'src/adm/adm_index_handler.erl', 'src/ds/group_ds.erl', 'src/ds/msg_reaction_ds.erl', 'src/ds/push_notification_ds.erl', 'src/ds/workspace_ds.erl', 'src/lib/imboy_cache_sync.erl'] |
| group_e2ee_mode | {group_e2ee_mode,Gid} | invalidate | PostgreSQL；缓存非权威 | 60 | True | ['src/ds/group_ds.erl'] |
| channel | {channel,Cid} | invalidate | 失效标签；未发现生产填充 | 无写入 | False | ['src/adm/adm_admin_handler.erl', 'src/adm/adm_channel_handler.erl', 'src/adm/adm_index_handler.erl', 'src/ds/channel_ds.erl', 'src/ds/workspace_ds.erl', 'src/logic/channel_logic_subscription.erl', 'src/logic/moderation_policy.erl'] |
| channel_subs | {channel_subs,Cid} | invalidate | PostgreSQL；缓存非权威 | 3600 | True | ['src/adm/adm_channel_handler.erl', 'src/ds/channel_ds.erl', 'src/ds/workspace_ds.erl', 'src/logic/channel_logic_subscription.erl'] |
| config5 | {config5,K} | invalidate | 配置文件/application env/PG；按子项审查 | 864000 | True | ['src/ds/config_ds.erl'] |
| conv_deleted | {conv_deleted,Uid,ConversationId,Type} | invalidate | PostgreSQL；缓存非权威 | 300 | True | ['src/ds/conversation_delete_ds.erl'] |
| moment_acl | {moment_acl,PostId} | invalidate | PostgreSQL；缓存非权威 | 300 | True | ['src/ds/moment_ds.erl'] |
| msg_reaction | {msg_reaction,MsgId,MsgType} | invalidate | 仅删除标签；未发现填充 | 无写入 | False | ['src/ds/msg_reaction_ds.erl'] |
| user_dnd_rule | {user_dnd_rule,Uid} | invalidate | PostgreSQL；缓存非权威 | 3600 | True | ['src/ds/user_dnd_rule_ds.erl'] |
| billing_meter | {billing_meter,active_sub,0} | invalidate | PostgreSQL；缓存非权威 | 300 | True | ['src/logic/billing_meter.erl'] |
| moderation_sensitive_words | {moderation_sensitive_words} | invalidate | PostgreSQL；缓存非权威 | 60 | True | ['src/logic/moderation_policy.erl'] |
| cluster_node_info | {cluster_node_info,SourceNode} | local | 节点运行时；复制候选需字段投影 | 300 | False | ['src/lib/imboy_cluster.erl'] |
| apns_jwt | {apns_jwt,KeyId} | local | 本地签名凭据 | 3000 | True | ['src/ds/push_notification_ds.erl'] |
| fcm_access_token_dyn | {fcm_access_token_dyn} | local | OAuth 服务 | 3000 | True | ['src/push_provider_fcm_token.erl'] |
| qianfan_access_token | qianfan_access_token | local | OAuth 服务 | 2591997 | True | ['src/lib/qianfan_api.erl'] |
| qr_login | {qr_login,SessionToken} | G-QR_BLOCKED | 现有缓存是权威；需 PG 原子消费 | 60 | True | ['src/api/qr_login_handler.erl', 'src/ds/qr_login_event_ds.erl'] |
| qr_login_qr | {qr_login_qr,QRToken} | G-QR_BLOCKED | 现有缓存映射；须与 QR authority 绑定 | 60 | True | ['src/api/qr_login_handler.erl'] |
| login_attempt | {login_attempt,Identifier,Ip} | local | login_attempt_ds 自有 ETS；login_security_logic 读 depcache 无同源写入 | 自有 ETS 窗口；无 depcache 写入 | True | ['src/ds/login_attempt_ds.erl', 'src/logic/login_security_logic.erl'] |
| login_attempt_ip_limit | {login_attempt_ip_limit,Ip} | local | 当前缓存计数；读改写非原子 | 3600 | True | ['src/ds/login_attempt_ds.erl'] |
| bind_mail | {bind_mail,Mail,Ts} | local | 当前缓存一次性标志；不可复制模拟领取 | 86400 | True | ['src/api/passport_handler.erl'] |
| csrf | Csrf 随机 binary（无 namespace） | local | 当前管理员 CSRF 缓存 | 默认3600 | True | ['src/adm/adm_passport_handler.erl'] |
| tag_subtitle_2 | implode(tag_subtitle_2,TagId) | invalidate | PostgreSQL；缓存非权威 | 86400 | True | ['src/repo/user_tag_relation_repo.erl'] |
| tag_subtitle_3 | implode(tag_subtitle_3,TagId) | invalidate | legacy 删除标签 | 无写入 | True | ['src/repo/user_tag_relation_repo.erl'] |
| adm_user_permission | {adm_user_permission,AdmId} | invalidate | PostgreSQL；缓存非权威 | 7200 | True | ['src/adm/adm_acl.erl', 'src/adm/adm_admin_handler.erl'] |
| adm_user_admin_permission | {adm_user_admin_permission,AdmId} | invalidate | PostgreSQL；缓存非权威 | 7200 | True | ['src/adm/adm_admin_handler.erl'] |
| adm_user_account | {adm_user_account,AdmId} | invalidate | PostgreSQL；缓存非权威 | 7200 | True | ['src/adm/adm_enterprise_application_handler.erl'] |
| adm_user_sample | {adm_user_sample,AdmId} | invalidate | PostgreSQL；缓存非权威 | 7200 | True | ['src/adm/adm_feedback_handler.erl', 'src/lib/imboy_dtl.erl'] |
| adm_user_current | {adm_user_current,AdmId} | invalidate | PostgreSQL；缓存非权威 | 7200 | True | ['src/adm/adm_admin_handler.erl', 'src/adm/adm_index_handler.erl'] |
| adm_user_rbac | {adm_user_rbac,AdmId} | invalidate | PostgreSQL；缓存非权威 | 7200 | True | ['src/adm/adm_admin_handler.erl', 'src/adm/adm_index_handler.erl'] |
| adm_user_role_ids | {adm_user_role_ids,AdmId} | invalidate | PostgreSQL；缓存非权威 | 7200 | True | ['src/adm/adm_role_handler.erl'] |
| adm_user_role_permission | {adm_user_role_permission,AdmId} | invalidate | PostgreSQL；缓存非权威 | 7200 | True | ['src/adm/adm_role_handler.erl'] |
| adm_user_group_permission | {adm_user_group_permission,AdmId} | invalidate | 仅失效标签；未发现填充 | 无写入 | True | ['src/adm/adm_admin_handler.erl'] |
| adm_user_moment_permission | {adm_user_moment_permission,AdmId} | invalidate | 仅失效标签；未发现填充 | 无写入 | True | ['src/adm/adm_admin_handler.erl'] |
| adm_user_report_permission | {adm_user_report_permission,AdmId} | invalidate | 仅失效标签；未发现填充 | 无写入 | True | ['src/adm/adm_admin_handler.erl'] |
| adm_user_feedback_permission | {adm_user_feedback_permission,AdmId} | invalidate | 仅失效标签；未发现填充 | 无写入 | True | ['src/adm/adm_admin_handler.erl'] |
| adm_user_stats_permission | {adm_user_stats_permission,AdmId} | invalidate | 仅失效标签；未发现填充 | 无写入 | True | ['src/adm/adm_admin_handler.erl'] |
