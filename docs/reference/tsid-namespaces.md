# TSID V10静态Namespace清单

> 更新：2026-10-11。从src/lib/elib_tsid_inventory.erl的namespaces/0与aliases/0核对；源码是运行时真源。本表不推断物理表结构，不替代迁移审核。

149个canonical各一个游标，3个alias复用既有游标；default测试/脚手架尾槽使总游标数150。下表顺序是当前源码顺序，槽号不是外部API，也不编码入ID。生产实体写入须明确标签，未知报错；新增实体同步源码及专项守护。

| Canonical Namespace |
|---|
| `user` |
| `user_device` |
| `user_collect` |
| `user_denylist` |
| `user_tag` |
| `user_tag_relation` |
| `user_deletion_request` |
| `user_deletion_job` |
| `moderation_action` |
| `moderation_appeal` |
| `review_queue` |
| `friend` |
| `friend_category` |
| `group_info` |
| `group_member` |
| `group_notice` |
| `group_log` |
| `group_random_code` |
| `group_category` |
| `group_tag` |
| `group_vote` |
| `group_vote_option` |
| `group_schedule` |
| `group_schedule_reminder` |
| `group_album` |
| `group_album_photo` |
| `group_album_comment` |
| `group_file` |
| `group_task` |
| `group_task_assignment` |
| `msg_c2c` |
| `msg_c2g` |
| `msg_s2c` |
| `msg_store` |
| `msg_mention` |
| `msg_forward` |
| `msg_reaction` |
| `conversation_pin` |
| `conversation_delete` |
| `attachment` |
| `moment_post` |
| `moment_comment` |
| `moment_like` |
| `moment_timeline` |
| `moment_post_acl` |
| `moment_report` |
| `channel` |
| `channel_message` |
| `channel_subscription` |
| `channel_admin` |
| `channel_message_view` |
| `channel_order` |
| `channel_invitation` |
| `channel_webhook` |
| `channel_comment` |
| `channel_price` |
| `organization` |
| `organization_invitation` |
| `organization_department` |
| `workspace` |
| `workspace_invite` |
| `organization_invite_code` |
| `owner_activation_invite` |
| `project` |
| `project_task` |
| `project_event` |
| `project_milestone` |
| `feedback` |
| `feedback_reply` |
| `e2ee_key_backup` |
| `report_ticket` |
| `report_action_log` |
| `wallet` |
| `wallet_transaction` |
| `recharge_order` |
| `payment_transaction` |
| `red_packet` |
| `red_packet_receive` |
| `transfer_order` |
| `agent_payment_compensation` |
| `agent_payment_mandate` |
| `billing_plan` |
| `billing_subscription` |
| `billing_invoice` |
| `billing_usage` |
| `live_room` |
| `push_token` |
| `compliance_key` |
| `sensitive_word` |
| `olm_identity` |
| `olm_one_time_key` |
| `olm_fallback_key` |
| `trust_audit` |
| `adm_user` |
| `adm_role` |
| `admin_op_log` |
| `plugin_audit_log` |
| `app_version` |
| `app_ddl` |
| `app_upgrade_log` |
| `mcp_client` |
| `mcp_client_grant` |
| `mcp_audit_log` |
| `sso_identity` |
| `sso_config` |
| `ai_agent_role_version` |
| `agent_task` |
| `moya_task` |
| `moya_assignment` |
| `moya_submission` |
| `submission_asset` |
| `calligraphy_review_draft` |
| `teacher_review` |
| `review_asset` |
| `teaching_admin_audit` |
| `business_identity` |
| `cs_event` |
| `cs_read_cursor` |
| `cs_widget_nonce` |
| `organization_business_identity_assignment` |
| `enterprise_message` |
| `enterprise_message_delivery` |
| `enterprise_message_origin` |
| `enterprise_audit_event` |
| `enterprise_application` |
| `enterprise_application_grant` |
| `enterprise_application_credential` |
| `enterprise_external_identity` |
| `enterprise_oa_sso_code` |
| `enterprise_asset` |
| `enterprise_conversation` |
| `enterprise_offboarding_case` |
| `enterprise_contact_assignment` |
| `enterprise_note` |
| `enterprise_retention_hold` |
| `enterprise_retention_policy` |
| `enterprise_offboarding_item` |
| `cs_session` |
| `cs_visit_token` |
| `cs_shop_key` |
| `cs_seat_console` |
| `cs_seat_console_public` |
| `cs_widget_installation` |
| `cs_widget_installation_public` |
| `agent_grant` |
| `agent_grant_event` |
| `agent_run` |
| `agent_run_event` |
| `agent_effect` |

## 别名（不增加游标）

| Alias | Canonical |
|---|---|
| `group` | `group_info` |
| `audit` | `enterprise_audit_event` |
| `enterprise_audit` | `enterprise_audit_event` |

同实体创建入口须共用canonical；跨域同值合法，引用、缓存、关联须带Domain。默认入口不是未知标签兜底。风险见[字段规范](./tsid-field-convention.md)，设计见[V10说明](../explanation/tsid-v10.md)。
