%%%-------------------------------------------------------------------
%%% @doc TSID 实体 catalog（v3 = 现状口径·全量补全版）
%%%
%%% == 治理目标（未来 catalog 递减的唯一准绳） ==
%%% TSID 只用于「需要跨数据中心 / 跨地域做分布式同步」的实体表主键
%%% （用户 ID、群 ID、频道 ID 这一类）。不做分布式同步的实体不应使用
%%% TSID；治理路径是把对应实体的主键迁移为数据库本地生成
%%% （bigserial / uuid），每完成一个治理批次就同步递减本 catalog
%%% 版本。已登记为独立后续事项（见
%%% docs/architecture/2026-09-28-tsid-correctness-hardening-status.md）。
%%%
%%% == v2 为什么取「现状口径」（schema 全量单列 bigint 主键） ==
%%% 第一次 cutover 的唯一安全要求：凡可能已写入历史 TSID 的主键列，
%%% 必须全部纳入自举扫描与保护范围；任何漏扫列中的历史 TSID 都会在
%%% 重启自举（auto_scan 取 max(id)+1）时被撞号。因此 catalog 收缩
%%% 只能发生在治理迁移完成之后，绝不能先于它。
%%% 同时，elib_tsid_scan 的反向发现（discover_and_compare）对 schema 中
%%% 所有「单列 bigint 主键」表与 catalog 求差，发现 catalog 未登记的
%%% 单列 bigint 主键表即 {error, {unclassified_primary_keys, L}} 拒启
%%% （FAIL 级、无豁免清单）。因此 catalog 必须覆盖 migrations 产出的
%%% 真实 schema 的全部单列 bigint 主键表，否则首次启动即 fail-closed。
%%%
%%% == v2 变更记录（2026-09-30，第三方审计修复） ==
%%% v1 收录 104 表（运行时 elib_tsid:generate 调用点口径），漏登 79 张
%%% 单列 bigint 主键表——含 project_milestone / organization_department /
%%% agent_grant / agent_grant_event / customer_service_seat_console 等
%%% 运行时 TSID 写入表，以及 bot / user_setting / fts_group 等主键值
%%% 来自其他表 TSID（或遗留）的表。真实完整 schema 首启时这些表会被
%%% 扫描器判为 unclassified_primary_keys 拒启。v2 逐表核对 migrations
%%% DDL 主键列名后全量补全：104 + 79 = 183 项（179 张单列 bigint 主键
%%% + 4 张 hypertable 复合主键特例）。
%%%
%%% == v3 变更记录（2026-09-30，生产首启 unclassified 修复） ==
%%% v2 的静态扫描只覆盖 priv/migrations，漏掉三个 migrations 之外的
%%% 单列 bigint 主键表：msg_store_staging（msg_store_repo 运行时
%%% ensure_table_exists 建表，id 由 elib_tsid:generate(msg_store) 写入）、
%%% schema_migrations / schema_migrations_history（erlang_migrate 台账表，
%%% version BIGINT PRIMARY KEY，值为迁移号非 TSID）。生产真实 schema 首启
%%% 被反向发现判为 unclassified_primary_keys 拒启；v3 补全为 186 项
%%% （182 张单列 bigint 主键 + 4 张 hypertable 复合主键特例）。台账表
%%% 入册只为通过「无豁免清单」的反向发现，version 值极小、对 floor
%%% 无实质影响。
%%%
%%% == 数据来源与绑定 ==
%%% 由静态扫描（call-sites × migrations DDL 顺序模拟：CREATE / DROP /
%%% ALTER ADD PRIMARY KEY）生成并经人工逐条核对；version() 与 digest()
%%% 绑定进割接 manifest（elib_tsid_bootstrap），manifest 中的 digest 与
%%% 本模块不符时启动即 FAIL（catalog_changed）——v1 → v2 扩容后既有
%%% manifest 失配属预期，按 runbook 重新割接/绑定。
%%%-------------------------------------------------------------------
-module(elib_tsid_catalog).

-export([version/0, primary_keys/0, is_tsid_table/1, digest/0]).
%% catalog rebind 溯源（elib_tsid_bootstrap catalog_changed 显式重绑路径）：
%% 历史版本 digest 锚点 + 已验证迁移邻接表。均为纯函数，不连库。
-export([known_versions/0, verified_rebind_transitions/0]).

-define(CATALOG_VERSION, 3).

%% v1/v2 历史 digest（32 字节 SHA-256，与 digest() 同一哈希链），溯源取证：
%% 分别由 git 历史版本 352bee53（v1，104 项）与 8f1a164d（v2，183 项）的
%% primary_keys/0 独立编译重算得出，v2 常量另由 eunit 用「当前清单剔除
%% v3 新增 3 项」自证（见 elib_tsid_catalog_tests provenance 段）。
%% v1 常量无法在当前代码内重构自证（v1→v2 为 79 项扩容），以 git blob
%% 取证为准；两个常量相互独立且与 v3 digest 互异（测试钉住）。
-define(V1_DIGEST,
    <<16#B0, 16#B5, 16#95, 16#E3, 16#05, 16#6D, 16#F3, 16#0D, 16#86, 16#6F, 16#FE, 16#BE, 16#32,
        16#01, 16#1B, 16#B1, 16#13, 16#88, 16#F0, 16#32, 16#6C, 16#D2, 16#A8, 16#03, 16#58, 16#9F,
        16#F8, 16#11, 16#D8, 16#73, 16#BD, 16#B5>>
).
-define(V2_DIGEST,
    <<16#96, 16#E3, 16#05, 16#1F, 16#D9, 16#20, 16#80, 16#22, 16#28, 16#30, 16#77, 16#9D, 16#9F,
        16#5B, 16#A9, 16#88, 16#84, 16#C2, 16#2E, 16#8A, 16#7A, 16#9C, 16#E0, 16#E3, 16#AA, 16#46,
        16#17, 16#73, 16#88, 16#5D, 16#6D, 16#1C>>
).

-spec version() -> pos_integer().
version() ->
    ?CATALOG_VERSION.

%% 每项为一个「需要 TSID 主键的表」：{表名 atom, 列名 atom}。
%% 绝大多数为 id；主键列名以 migrations DDL 为准（见各特例说明）。
-spec primary_keys() -> [{Table :: atom(), Column :: atom()}].
%% v3（现状口径·全量补全）：186 项 = 真实 schema 的全部单列 bigint 主键
%% 表 182 张 + hypertable 复合主键特例 4 张。除 priv/migrations 外还覆盖
%% 运行时建表（msg_store_staging）与 erlang_migrate 台账表（两个
%% schema_migrations*，主键列 version）。
%% 逐表映射 migrations 真实表名并验证主键形态（核对证据：gate-supplement
%% catalog-v1-addendum + 2026-09-30 审计补全对照）。
%% 特殊形态（主键列非 id）：mcp_client 为 client_id；adm_auth_epoch 为
%% admin_id；ai_agent / bot / fts_user / geo_people_nearby / user_auth_epoch /
%% user_setting 为 user_id；class_profile / enterprise_group_origin / fts_group
%% 为 group_id；fts_channel 为 channel_id；customer_service_seat 为
%% business_identity_id；customer_service_seat_limit / organization_default_workspace
%% 为 organization_id；enterprise_attachment_retention 为 attachment_id。
%% hypertable 复合主键：msg_c2c / msg_c2g / msg_s2c / msg_store 为
%% (id, created_at)，id 为 bigint 首列（elib_tsid_scan 对该分区形态放行）。
%% 其余复合主键表（organization_member、agent_grant_workspace 等）不触发
%% 反向发现的 unclassified 判定，不入 catalog（入册反而 composite_pk FAIL）。
primary_keys() ->
    [
        {adm_auth_epoch, admin_id},
        {adm_role, id},
        {adm_user, id},
        {admin_operation_logs, id},
        {agent_effect, id},
        {agent_grant, id},
        {agent_grant_event, id},
        {agent_payment_compensation, id},
        {agent_payment_mandate, id},
        {agent_run, id},
        {agent_run_event, id},
        {ai_agent, user_id},
        {ai_agent_role_version, id},
        {announcement, id},
        {app_ddl, id},
        {app_upgrade_log, id},
        {app_version, id},
        {app_version_policy, id},
        {attachment, id},
        {billing_invoice, id},
        {billing_plan, id},
        {billing_subscription, id},
        {billing_usage, id},
        {bot, user_id},
        {bot_oauth_grant, id},
        {calligraphy_review_draft, id},
        {channel, id},
        {channel_admin, id},
        {channel_category, id},
        {channel_comment, id},
        {channel_invitation, id},
        {channel_message, id},
        {channel_message_view, id},
        {channel_order, id},
        {channel_price, id},
        {channel_reaction, id},
        {channel_stats_daily, id},
        {channel_subscription, id},
        {channel_webhook, id},
        {class_profile, group_id},
        {compliance_key, id},
        {conversation, id},
        {conversation_delete, id},
        {conversation_pin, id},
        {customer_service_event, id},
        {customer_service_read_cursor, id},
        {customer_service_seat, business_identity_id},
        {customer_service_seat_console, id},
        {customer_service_seat_limit, organization_id},
        {customer_service_session, id},
        {customer_service_shop_key, id},
        {customer_service_visit_token, id},
        {customer_service_widget_identity_key, id},
        {customer_service_widget_installation, id},
        {customer_service_widget_nonce, id},
        {e2ee_key_backups, id},
        {e2ee_key_shares, id},
        {enterprise_application, id},
        {enterprise_application_credential, id},
        {enterprise_application_grant, id},
        {enterprise_asset, id},
        {enterprise_attachment_retention, attachment_id},
        {enterprise_audit_event, id},
        {enterprise_contact, id},
        {enterprise_contact_assignment, id},
        {enterprise_contact_identity, id},
        {enterprise_conversation, id},
        {enterprise_external_identity, id},
        {enterprise_group_origin, group_id},
        {enterprise_message, id},
        {enterprise_message_delivery, id},
        {enterprise_message_origin, id},
        {enterprise_note, id},
        {enterprise_oa_sso_code, id},
        {enterprise_offboarding_case, id},
        {enterprise_offboarding_item, id},
        {enterprise_retention_hold, id},
        {enterprise_retention_policy, id},
        {feedback, id},
        {feedback_reply, id},
        {fts_channel, channel_id},
        {fts_group, group_id},
        {fts_user, user_id},
        {geo_people_nearby, user_id},
        {group, id},
        {group_album, id},
        {group_album_photo, id},
        {group_album_photo_comment, id},
        {group_album_photo_like, id},
        {group_category, id},
        {group_file, id},
        {group_log, id},
        {group_member, id},
        {group_member_generation, id},
        {group_notice, id},
        {group_random_code, id},
        {group_schedule, id},
        {group_schedule_participant, id},
        {group_schedule_remind, id},
        {group_tag, id},
        {group_task, id},
        {group_task_assignment, id},
        {group_vote, id},
        {group_vote_option, id},
        {group_vote_record, id},
        {homework_submission, id},
        {learner, id},
        {live_room, id},
        {mcp_audit_log, id},
        {mcp_client, client_id},
        {mcp_client_grant, id},
        {moderation_action, id},
        {moderation_appeal, id},
        {moment_comment, id},
        {moment_like, id},
        {moment_post, id},
        {moment_post_acl, id},
        {moment_report, id},
        {moment_timeline, id},
        {moya_subscribe_grant, id},
        {msg_c2c, id},
        {msg_c2g, id},
        {msg_forward, id},
        {msg_mention, id},
        {msg_reaction, id},
        {msg_s2c, id},
        {msg_store, id},
        %% 运行时建表（msg_store_repo:ensure_table_exists），非 migrations 产物
        {msg_store_staging, id},
        {msg_topic, id},
        {olm_fallback_key, id},
        {olm_identity, id},
        {olm_one_time_key, id},
        {organization, id},
        {organization_business_identity, id},
        {organization_business_identity_assignment, id},
        {organization_default_workspace, organization_id},
        {organization_department, id},
        {organization_invitation, id},
        {organization_invite_code, id},
        {owner_activation_invite, id},
        {payment_transaction, id},
        {plugin_audit_log, id},
        {project, id},
        {project_event, id},
        {project_milestone, id},
        {project_task, id},
        {push_token, id},
        {recharge_order, id},
        {red_packet, id},
        {red_packet_receive, id},
        {report_action_log, id},
        {report_ticket, id},
        {review_asset, id},
        {review_queue, id},
        %% erlang_migrate 台账表（version BIGINT PRIMARY KEY，值非 TSID；
        %% 入册仅为通过反向发现的无豁免判定）
        {schema_migrations, version},
        {schema_migrations_history, version},
        {sensitive_word, id},
        {sso_config, id},
        {sso_identity, id},
        {submission_asset, id},
        {system_datacenter_log, id},
        {system_id_segment, id},
        {system_id_segment_stats, id},
        {teacher_review, id},
        {teaching_admin_audit, id},
        {transfer_order, id},
        {trust_audit, id},
        {user, id},
        {user_auth_epoch, user_id},
        {user_collect, id},
        {user_deletion_job, id},
        {user_deletion_request, id},
        {user_denylist, id},
        {user_device, id},
        {user_dnd_rule, id},
        {user_friend, id},
        {user_friend_category, id},
        {user_group, id},
        {user_group_category, id},
        {user_setting, user_id},
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

%%--------------------------------------------------------------------
%% @doc catalog rebind 溯源锚点：全部已发布 catalog 版本的 {版本, digest}。
%%
%% 供 elib_tsid_bootstrap 的 catalog_changed 显式重绑路径验证 transition
%% 溯源——manifest 中绑定的旧 digest 必须能在本表找到对应已发布版本，
%% 否则该 transition 未经验证（BLOCKED_CATALOG_TRANSITION_UNRECOGNIZED）。
%% 当前版本条目直接引用 digest()（权威源），历史版本为静态常量。
%% catalog 递减/扩容时：新增版本须在此追加历史 digest 并同步
%% verified_rebind_transitions/0。
%%--------------------------------------------------------------------
-spec known_versions() -> [{Version :: pos_integer(), Digest :: binary()}].
known_versions() ->
    [{1, ?V1_DIGEST}, {2, ?V2_DIGEST}, {?CATALOG_VERSION, digest()}].

%%--------------------------------------------------------------------
%% @doc 已验证的 catalog rebind 迁移邻接表（allowlist）。
%%
%% 只登记「真实发布过且经人工核对」的单步版本迁移；跨步（如 v1→v3）、
%% 降级（高版本 manifest 绑回低版本 digest）与未知 digest 一律不在册，
%% 重绑路径必须 STOP（BLOCKED_CATALOG_TRANSITION_UNRECOGNIZED），不得
%% 自动放行。v1→v2（2026-09-30 审计扩容）与 v2→v3（2026-09-30 生产首启
%% 补全）为已发布历史迁移；未来 v4 发布时在两处同步追加 {3, 4}。
%%--------------------------------------------------------------------
-spec verified_rebind_transitions() -> [{FromV :: pos_integer(), ToV :: pos_integer()}].
verified_rebind_transitions() ->
    [{1, 2}, {2, 3}].
