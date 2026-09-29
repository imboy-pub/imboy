%%%-------------------------------------------------------------------
%%% @doc TSID 实体 catalog（v1 = 现状口径）
%%%
%%% == 治理目标（未来 catalog 递减的唯一准绳） ==
%%% TSID 只用于「需要跨数据中心 / 跨地域做分布式同步」的实体表主键
%%% （用户 ID、群 ID、频道 ID 这一类）。不做分布式同步的实体不应使用
%%% TSID；治理路径是把对应实体的主键迁移为数据库本地生成
%%% （bigserial / uuid），每完成一个治理批次就同步递减本 catalog
%%% 版本。已登记为独立后续事项（见
%%% docs/architecture/2026-09-28-tsid-correctness-hardening-status.md）。
%%%
%%% == v1 为什么取「现状口径」（全部运行时调用点对应的主键列） ==
%%% 第一次 cutover 的唯一安全要求：凡可能已写入历史 TSID 的主键列，
%%% 必须全部纳入自举扫描与保护范围；任何漏扫列中的历史 TSID 都会在
%%% 重启自举（auto_scan 取 max(id)+1）时被撞号。因此 catalog 收缩
%%% 只能发生在治理迁移完成之后，绝不能先于它。
%%%
%%% == 数据来源与绑定 ==
%%% 由静态扫描（call-sites × migrations DDL）生成并经人工逐条核对；
%%% version() 与 digest() 绑定进割接 manifest（elib_tsid_bootstrap），
%%% manifest 中的 digest 与本模块不符时启动即 FAIL（catalog_changed）。
%%%-------------------------------------------------------------------
-module(elib_tsid_catalog).

-export([version/0, primary_keys/0, is_tsid_table/1, digest/0]).

-define(CATALOG_VERSION, 1).

-spec version() -> pos_integer().
version() ->
    ?CATALOG_VERSION.

%% 每项为一个「需要 TSID 主键的表」：{表名 atom, 列名 atom}。
%% 绝大多数为 id；mcp_client 为 client_id（见下方特殊形态说明）。
-spec primary_keys() -> [{Table :: atom(), Column :: atom()}].
%% v1（现状口径）：104 个运行时 TSID 主键表，逐一映射到 migrations 真实
%% 表名并验证主键形态（核对报告见证据树 gate-supplement）。
%% 特殊形态：mcp_client 主键列为 client_id；msg_c2c / msg_c2g / msg_s2c /
%% msg_store 为 TimescaleDB hypertable 复合主键 (id, created_at)，id 为
%% bigint 首列（elib_tsid_scan 对该分区形态放行）。
primary_keys() ->
    [
        {adm_role, id},
        {adm_user, id},
        {admin_operation_logs, id},
        {agent_payment_compensation, id},
        {ai_agent_role_version, id},
        {app_ddl, id},
        {app_upgrade_log, id},
        {app_version, id},
        {attachment, id},
        {billing_invoice, id},
        {billing_plan, id},
        {billing_subscription, id},
        {billing_usage, id},
        {channel, id},
        {channel_admin, id},
        {channel_comment, id},
        {channel_invitation, id},
        {channel_message, id},
        {channel_message_view, id},
        {channel_order, id},
        {channel_price, id},
        {channel_subscription, id},
        {channel_webhook, id},
        {compliance_key, id},
        {conversation_delete, id},
        {conversation_pin, id},
        {e2ee_key_backups, id},
        {enterprise_application, id},
        {enterprise_application_credential, id},
        {enterprise_application_grant, id},
        {enterprise_audit_event, id},
        {enterprise_external_identity, id},
        {enterprise_message, id},
        {enterprise_message_origin, id},
        {enterprise_oa_sso_code, id},
        {feedback, id},
        {feedback_reply, id},
        {group, id},
        {group_album, id},
        {group_album_photo, id},
        {group_album_photo_comment, id},
        {group_category, id},
        {group_file, id},
        {group_log, id},
        {group_member, id},
        {group_notice, id},
        {group_random_code, id},
        {group_schedule, id},
        {group_schedule_remind, id},
        {group_tag, id},
        {group_task, id},
        {group_task_assignment, id},
        {group_vote, id},
        {group_vote_option, id},
        {live_room, id},
        {mcp_audit_log, id},
        {mcp_client, client_id},
        {mcp_client_grant, id},
        {moderation_action, id},
        {moderation_appeal, id},
        {moment_comment, id},
        {moment_like, id},
        {moment_post, id},
        {moment_report, id},
        {moment_timeline, id},
        {msg_c2c, id},
        {msg_c2g, id},
        {msg_forward, id},
        {msg_mention, id},
        {msg_reaction, id},
        {msg_s2c, id},
        {msg_store, id},
        {olm_fallback_key, id},
        {olm_identity, id},
        {olm_one_time_key, id},
        {organization, id},
        {organization_invite_code, id},
        {owner_activation_invite, id},
        {payment_transaction, id},
        {plugin_audit_log, id},
        {project, id},
        {project_event, id},
        {project_task, id},
        {push_token, id},
        {recharge_order, id},
        {red_packet, id},
        {red_packet_receive, id},
        {report_action_log, id},
        {report_ticket, id},
        {sso_identity, id},
        {transfer_order, id},
        {user, id},
        {user_deletion_job, id},
        {user_deletion_request, id},
        {user_denylist, id},
        {user_device, id},
        {user_friend, id},
        {user_friend_category, id},
        {user_tag, id},
        {user_tag_relation, id},
        {wallet, id},
        {wallet_transaction, id},
        {workspace, id},
        {workspace_invite, id}
    ].

-spec is_tsid_table(Table :: atom()) -> boolean().
is_tsid_table(Table) ->
    lists:keymember(Table, 1, primary_keys()).

%% 对 {version, 排序后的列表} 做 SHA-256，返回 32 字节原始摘要。
%% 恒定：同 version + 同列表 → 同 digest；用于割接 manifest 绑定与证据锚定。
-spec digest() -> <<_:256>>.
digest() ->
    Rows = lists:sort(primary_keys()),
    crypto:hash(sha256, term_to_binary({?CATALOG_VERSION, Rows})).
