-module(elib_tsid_inventory).

%%%
% TSID Namespace 静态清单（V10 / W1 冻结，见 .Codex/tsid_v10/W1-namespace-design.md）
%
% 一个 canonical 名称 = 一个业务实体的主键 ID 空间 = 一个独立游标槽位。
% 追加新实体时只能在 namespaces/0 列表尾部追加（顺序即槽位号，保持稳定
% 便于诊断）。未在此清单且不在 aliases/0 的名称一律 generate 报错。
%
% 本模块是纯数据合同（elib_tsid:init/1 构建槽位表时读取），无进程、
% 无持久化、无动态注册；外部统一经 elib_tsid:namespaces/aliases 委托访问。
%%%

-export([namespaces/0, aliases/0]).

-spec namespaces() -> [atom()].
namespaces() ->
    [
        %% ── 用户 ──
        user,
        user_device,
        user_collect,
        user_denylist,
        user_tag,
        user_tag_relation,
        user_deletion_request,
        user_deletion_job,
        %% ── 审核治理 ──
        moderation_action,
        moderation_appeal,
        review_queue,
        %% ── 好友 ──
        friend,
        friend_category,
        %% ── 群组（group 为 group_info 的别名：同写 public.group 表）──
        group_info,
        group_member,
        group_notice,
        group_log,
        group_random_code,
        group_category,
        group_tag,
        group_vote,
        group_vote_option,
        group_schedule,
        group_schedule_reminder,
        group_album,
        group_album_photo,
        group_album_comment,
        group_file,
        group_task,
        group_task_assignment,
        %% ── 消息（消息行实体主键，不做排序）──
        msg_c2c,
        msg_c2g,
        msg_s2c,
        msg_store,
        msg_mention,
        msg_forward,
        msg_reaction,
        %% ── 会话 ──
        conversation_pin,
        conversation_delete,
        %% ── 附件 / 媒体 ──
        attachment,
        %% ── 朋友圈 ──
        moment_post,
        moment_comment,
        moment_like,
        moment_timeline,
        moment_post_acl,
        moment_report,
        %% ── 频道 ──
        channel,
        channel_message,
        channel_subscription,
        channel_admin,
        channel_message_view,
        channel_order,
        channel_invitation,
        channel_webhook,
        channel_comment,
        channel_price,
        %% ── Organization / 工作区 ──
        organization,
        organization_invitation,
        organization_department,
        workspace,
        workspace_invite,
        organization_invite_code,
        owner_activation_invite,
        %% ── 项目 ──
        project,
        project_task,
        project_event,
        project_milestone,
        %% ── 反馈 ──
        feedback,
        feedback_reply,
        %% ── E2EE 恢复 ──
        e2ee_key_backup,
        %% ── 举报 ──
        report_ticket,
        report_action_log,
        %% ── 钱包 / 支付 ──
        wallet,
        wallet_transaction,
        recharge_order,
        payment_transaction,
        red_packet,
        red_packet_receive,
        transfer_order,
        agent_payment_compensation,
        agent_payment_mandate,
        %% ── SaaS 计费 ──
        billing_plan,
        billing_subscription,
        billing_invoice,
        billing_usage,
        %% ── 直播 ──
        live_room,
        %% ── 推送 ──
        push_token,
        %% ── 合规 ──
        compliance_key,
        sensitive_word,
        %% ── Olm（X3DH + Double Ratchet）单聊 E2EE ──
        olm_identity,
        olm_one_time_key,
        olm_fallback_key,
        trust_audit,
        %% ── 管理员 ──
        adm_user,
        adm_role,
        admin_op_log,
        %% ── 插件 ──
        plugin_audit_log,
        %% ── 应用配置 ──
        app_version,
        app_ddl,
        app_upgrade_log,
        %% ── MCP 治理 ──
        mcp_client,
        mcp_client_grant,
        mcp_audit_log,
        %% ── SSO ──
        sso_identity,
        sso_config,
        %% ── AI Agent ──
        ai_agent_role_version,
        agent_task,
        %% ── Moya（教学域）──
        moya_task,
        moya_assignment,
        moya_submission,
        submission_asset,
        calligraphy_review_draft,
        teacher_review,
        review_asset,
        teaching_admin_audit,
        %% ── Customer Service（cs_id_port 的 Kind）──
        business_identity,
        cs_event,
        cs_read_cursor,
        cs_widget_nonce,
        organization_business_identity_assignment,
        %% ── Enterprise Business ──
        enterprise_message,
        enterprise_message_delivery,
        enterprise_message_origin,
        enterprise_audit_event,
        enterprise_application,
        enterprise_application_grant,
        enterprise_application_credential,
        enterprise_external_identity,
        enterprise_oa_sso_code,
        %% ── Enterprise Business（动态 Kind 补录，W7 全量取证：这些 Kind 经
        %% eb_*_app:new_id(Kind) 变量传入，W0 字面量扫描未覆盖）──
        enterprise_asset,
        enterprise_conversation,
        enterprise_offboarding_case,
        enterprise_contact_assignment,
        enterprise_note,
        enterprise_retention_hold,
        enterprise_retention_policy,
        enterprise_offboarding_item,
        %% ── Customer Service（动态 Kind 补录，同上）──
        cs_session,
        cs_visit_token,
        cs_shop_key,
        cs_seat_console,
        cs_seat_console_public,
        cs_widget_installation,
        cs_widget_installation_public,
        agent_grant,
        agent_grant_event,
        agent_run,
        agent_run_event,
        agent_effect,
        %% ── 客服 AI：customer_service_ai_agent / customer_service_faq / customer_service_ai_job ──
        ai_cs_config,
        ai_cs_faq,
        cs_ai_job
    ].

%% @doc 别名 → canonical 映射：写同一张表的旧标签静态归并到同一游标。
%% group → group_info（同写 public.group）；
%% audit / enterprise_audit → enterprise_audit_event（同写 enterprise_audit_event 表）。
-spec aliases() -> #{atom() => atom()}.
aliases() ->
    #{
        group => group_info,
        audit => enterprise_audit_event,
        enterprise_audit => enterprise_audit_event
    }.
