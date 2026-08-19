-module(imboy_router).

-export([get_routes/0]).
-export([open/0]).
-export([option/0]).
%% Phase 2 切片 2：供测试与 admin introspection
-export([plugin_routes/0]).

%% @doc 获取所有路由定义
-spec get_routes() -> list().
get_routes() ->
    Host = config_ds:env(host, '_'),
    %% 2026-07-08：v0 裸 /api/* 业务路由（不带 v1 段）已下架，
    %% 全部迁移到 ApiV1Routes 的 /api/v1/* 等价物；三个客户端
    %% （imboyapp/imboyadmin/imboy-sdk-js）均已确认不再调用 v0 路径。
    %% MainRoutes 只保留网站白名单（根路径，不加 /api）与静态资源。
    MainRoutes =
        [
            {"/", index_handler, #{action => help}},
            {"/help", index_handler, #{action => help}},

            % 品牌配置（运行时白标，公开，客户端启动拉取，见 open/0）
            {"/brand", brand_handler, #{action => info}},

            % Prometheus 指标端点
            {"/metrics", metrics_handler, #{}},

            % C-49 健康检查：200=依赖就绪可接流量 / 503=进程活着但 PG 不可用。
            % 此前 /healthz 只在 throttle_middleware 白名单里出现，没有路由 → 404，
            % compose/helm 的 healthcheck 配了也永远不健康。
            {"/healthz", healthz_handler, #{}},

            {"/privacy-policy", cowboy_static,
                {priv_file, imboy, "static/legal/privacy_policy.html"}},
            {"/account-deletion", cowboy_static,
                {priv_file, imboy, "static/legal/account_deletion.html"}},
            {"/static/[...]", cowboy_static,
                {priv_dir, imboy, "static", [{mimetypes, cow_mimetypes, all}]}}
        ],

    % Api v1 routes
    ApiV1Routes =
        [
            %% MCP Server（Phase 3 T3.2）：barrel_mcp 协议引擎的 cowboy 桥接，
            %% 核心固定端点进静态 ApiV1Routes（不走 imboy_router_registry）。JWT 见 T3.3。
            {"/api/v1/mcp", mcp_handler, #{}},
            {"/api/v1/init", index_handler, #{action => init}},
            {"/api/v1/refreshtoken", passport_handler, #{action => refreshtoken}},
            {"/api/v1/app/features", app_feature_handler, #{action => features}},
            {"/api/v1/app/manifest", app_manifest_handler, #{action => manifest}},
            %% Phase 4 T4.1：Agent 能力发现端点（外部 AI 发现 MCP 端点 + 插件 tools）
            {"/api/v1/agent-card", agent_card_handler, #{action => card}},
            %% Phase 4 T4.1：标准 A2A 发现端点（匿名可达，见 open/0）
            {"/.well-known/agent.json", agent_card_handler, #{action => well_known}},
            {"/api/v1/app/policy", app_feature_handler, #{action => policy}},
            {"/api/v1/app/ice_servers", app_feature_handler, #{action => ice_servers}},
            {"/api/v1/app_version/check", app_version_handler, #{action => check}},
            {"/api/v1/app_upgrade/report", app_upgrade_log_handler, #{action => report}},

            % 【新增】Prometheus 指标端点
            {"/api/v1/metrics", metrics_handler, #{}},

            {"/api/v1/passport/quick_login", passport_handler, #{action => quick_login}},
            {"/api/v1/passport/alipay_login", passport_handler, #{action => alipay_login}},
            {"/api/v1/passport/alipay_authinfo", passport_handler, #{action => alipay_authinfo}},
            {"/api/v1/passport/login", passport_handler, #{action => login}},
            {"/api/v1/passport/signup", passport_handler, #{action => signup}},
            {"/api/v1/passport/getcode", passport_handler, #{action => getcode}},
            {"/api/v1/passport/findpassword", passport_handler, #{action => find_password}},
            {"/api/v1/passport/bind_mail", passport_handler, #{action => bind_mail}},

            % QR 码登录（WhatsApp Web 风格）
            {"/api/v1/passport/qr_login/create", qr_login_handler, #{action => create}},
            {"/api/v1/passport/qr_login/status", qr_login_handler, #{action => status}},
            {"/api/v1/passport/qr_login/scan", qr_login_handler, #{action => scan}},
            {"/api/v1/passport/qr_login/confirm", qr_login_handler, #{action => confirm}},
            {"/api/v1/passport/qr_login/cancel", qr_login_handler, #{action => cancel}},
            %% PR-3β: SSE 推送端点（cowboy_loop），替代轮询 /status
            %% Web 端 EventSource 长连接，scan/confirm 后实时推送状态
            {"/api/v1/passport/qr_login/subscribe", qr_login_sse_handler, #{}},

            %% P0-C: 企业 SSO OIDC 登录流（Authorization Code + PKCE），见 open/0 白名单
            {"/api/v1/auth/oidc/authorize", auth_oidc_handler, #{action => authorize}},
            {"/api/v1/auth/oidc/callback", auth_oidc_handler, #{action => callback}},
            {"/api/v1/auth/oidc/exchange", auth_oidc_handler, #{action => exchange}},

            {"/api/v1/ws", websocket_handler, #{}}
        ] ++
            test_routes_v1() ++
            [
                {"/api/v1/conversation/online", conversation_handler, #{action => online}},
                {"/api/v1/conversation/mine", conversation_handler, #{action => mine}},
                {"/api/v1/conversation/pin", conversation_handler, #{action => pin_conversation}},
                {"/api/v1/conversation/unpin", conversation_handler, #{
                    action => unpin_conversation
                }},
                {"/api/v1/conversation/pinned", conversation_handler, #{action => pinned_list}},
                {"/api/v1/conversation/delete", conversation_handler, #{
                    action => delete_conversation
                }},
                {"/api/v1/conversation/restore", conversation_handler, #{
                    action => restore_conversation
                }},
                {"/api/v1/msg/offline", msg_handler, #{action => offline}},
                {"/api/v1/msg/offline_ack", msg_handler, #{action => offline_ack}},
                {"/api/v1/msg/read_stats", msg_handler, #{action => read_stats}},
                {"/api/v1/msg/pin", msg_handler, #{action => pin}},
                {"/api/v1/msg/forward", msg_handler, #{action => forward}},
                {"/api/v1/msg/reaction/add", msg_handler, #{action => reaction_add}},
                {"/api/v1/msg/reaction/remove", msg_handler, #{action => reaction_remove}},
                {"/api/v1/msg/reaction/list", msg_handler, #{action => reaction_list}},
                {"/api/v1/msg/history", msg_handler, #{action => history}},

                %% AI 助手发现：供客户端列出可发起 C2S 会话的助手（JWT 认证，owner 无关）
                {"/api/v1/agent/list", ai_agent_handler, #{action => list}},

                {"/api/v1/user/qrcode", user_handler, #{action => qrcode}},
                {"/api/v1/user/update", user_handler, #{action => update}},
                {"/api/v1/user/show", user_handler, #{action => show}},
                {"/api/v1/user/change_state", user_handler, #{action => change_state}},
                {"/api/v1/user/setting", user_handler, #{action => setting}},
                {"/api/v1/user/credential", user_handler, #{action => credential}},
                {"/api/v1/user/change_password", user_handler, #{action => change_password}},
                {"/api/v1/user/set_password", user_handler, #{action => set_password}},
                {"/api/v1/user/apply_logout", user_handler, #{action => apply_logout}},
                {"/api/v1/user/cancel_logout", user_handler, #{action => cancel_logout}},
                {"/api/v1/user/export_data", user_handler, #{action => export_data}},
                {"/api/v1/user/search", user_handler, #{action => search}},

                {"/api/v1/user_device/page", user_device_handler, #{action => page}},
                {"/api/v1/user_device/change_name", user_device_handler, #{action => change_name}},
                {"/api/v1/user_device/delete", user_device_handler, #{action => delete}},

                % 设备会话管理
                {"/api/v1/user_device/sessions", user_device_handler, #{action => sessions}},
                {"/api/v1/user_device/check_login", user_device_handler, #{action => check_login}},
                {"/api/v1/user_device/kick", user_device_handler, #{action => kick}},
                {"/api/v1/user_device/kick-others", user_device_handler, #{action => kick_others}},

                % 推送 Token 管理
                {"/api/v1/push/register", user_device_handler, #{action => push_register}},
                {"/api/v1/push/unregister", user_device_handler, #{action => push_unregister}},

                {"/api/v1/e2ee/user_keys", e2ee_handler, #{action => user_keys}},
                {"/api/v1/e2ee/group_member_keys", e2ee_handler, #{action => group_member_keys}},
                {"/api/v1/e2ee/report_device_key", e2ee_handler, #{action => report_device_key}},
                {"/api/v1/e2ee/key/status", e2ee_handler, #{action => key_status}},
                {"/api/v1/e2ee/notifications/pull", e2ee_handler, #{action => pull_notifications}},
                {"/api/v1/e2ee/recovery/start", e2ee_handler, #{action => start_recovery}},

                % 合规密钥分发（三层加密架构）
                {"/api/v1/e2ee/compliance_key", e2ee_handler, #{action => compliance_key}},

                % 加密密钥备份（4S 模式，服务端只存密文）
                {"/api/v1/e2ee/backup/put", e2ee_backup_handler, #{action => put}},
                {"/api/v1/e2ee/backup/get", e2ee_backup_handler, #{action => get}},
                {"/api/v1/e2ee/backup/info", e2ee_backup_handler, #{action => info}},
                {"/api/v1/e2ee/backup/delete", e2ee_backup_handler, #{action => delete}},

                % Olm（X3DH + Double Ratchet）单聊 E2EE：身份键 / prekey / fallback / claim
                % 服务端只存公钥侧，无私钥；不做任何加解密。
                {"/api/v1/e2ee/olm/identity", olm_handler, #{action => report_identity}},
                {"/api/v1/e2ee/olm/prekeys", olm_handler, #{action => report_prekeys}},
                {"/api/v1/e2ee/olm/fallback_key", olm_handler, #{action => report_fallback}},
                {"/api/v1/e2ee/olm/get_identity", olm_handler, #{action => get_identity}},
                {"/api/v1/e2ee/olm/claim", olm_handler, #{action => claim_key}},
                {"/api/v1/e2ee/olm/prekey_count", olm_handler, #{action => prekey_count}},
                % 统一 Device API（ADR 03 §8）：多设备列表 + batch claim（现有 5 端点保留兼容）
                {"/api/v1/e2ee/devices", olm_handler, #{action => list_devices}},
                {"/api/v1/e2ee/devices/batch_claim", olm_handler, #{action => batch_claim}},
                % Device Trust 决策事件（ADR 06 §8）：服务端验签+审计+广播,零算法
                {"/api/v1/e2ee/trust/record", e2ee_trust_handler, #{action => record}},

                {"/api/v1/user_collect/page", user_collect_handler, #{action => page}},
                {"/api/v1/user_collect/add", user_collect_handler, #{action => add}},
                {"/api/v1/user_collect/remove", user_collect_handler, #{action => remove}},
                {"/api/v1/user_collect/change", user_collect_handler, #{action => change}},

                {"/api/v1/feedback/page", feedback_handler, #{action => page}},
                {"/api/v1/feedback/add", feedback_handler, #{action => add}},
                {"/api/v1/feedback/change", feedback_handler, #{action => change}},
                {"/api/v1/feedback/remove", feedback_handler, #{action => remove}},
                {"/api/v1/feedback/reply", feedback_handler, #{action => reply}},
                {"/api/v1/feedback/page_reply", feedback_handler, #{action => page_reply}},

                {"/api/v1/user_tag/page", user_tag_handler, #{action => page}},
                {"/api/v1/user_tag/add", user_tag_handler, #{action => add}},
                {"/api/v1/user_tag/change_name", user_tag_handler, #{action => change_name}},
                {"/api/v1/user_tag/delete", user_tag_handler, #{action => delete}},

                {"/api/v1/user_tag_relation/collect_page", user_tag_relation_handler, #{
                    action => collect_page
                }},
                {"/api/v1/user_tag_relation/friend_page", user_tag_relation_handler, #{
                    action => friend_page
                }},
                {"/api/v1/user_tag_relation/add", user_tag_relation_handler, #{action => add}},
                {"/api/v1/user_tag_relation/set", user_tag_relation_handler, #{action => set}},
                {"/api/v1/user_tag_relation/remove", user_tag_relation_handler, #{action => remove}},

                {"/api/v1/location/makeMyselfVisible", location_handler, #{
                    action => make_myself_visible
                }},
                {"/api/v1/location/makeMyselfUnvisible", location_handler, #{
                    action => make_myself_unvisible
                }},
                {"/api/v1/location/peopleNearby", location_handler, #{action => people_nearby}},

                {"/api/v1/friend/add", friend_handler, #{action => add_friend}},
                {"/api/v1/friend/confirm", friend_handler, #{action => confirm}},
                {"/api/v1/friend/reject", friend_handler, #{action => reject}},
                {"/api/v1/friend/delete", friend_handler, #{action => delete_friend}},
                {"/api/v1/friend/list", friend_handler, #{action => list}},
                {"/api/v1/friend/information", friend_handler, #{action => information}},
                {"/api/v1/friend/change_remark", friend_handler, #{action => change_remark}},

                {"/api/v1/friend/denylist/add", user_denylist_handler, #{action => add}},
                {"/api/v1/friend/denylist/remove", user_denylist_handler, #{action => remove}},
                {"/api/v1/friend/denylist/page", user_denylist_handler, #{action => page}},

                {"/api/v1/friend/move", friend_handler, #{action => move}},
                {"/api/v1/friend/category/add", friend_category_handler, #{action => add}},
                {"/api/v1/friend/category/delete", friend_category_handler, #{action => delete}},
                {"/api/v1/friend/category/rename", friend_category_handler, #{action => rename}},

                % 搜索"用户允许被搜索"的用户
                {"/api/v1/fts/user_search", fts_handler, #{action => user_search}},
                % 最近新注册的并且允许被搜索到的朋友
                {"/api/v1/fts/recently_user", fts_handler, #{action => recently_user}},
                % 消息全文搜索
                {"/api/v1/fts/msg", fts_handler, #{action => msg}},

                % RTC 房间（LiveKit SFU：群通话/1:1 通话入场券）
                {"/api/v1/rtc/room/join", rtc_room_handler, #{action => join}},

                {"/api/v1/group/remark", group_handler, #{action => remark}},
                {"/api/v1/group/qrcode", group_handler, #{action => qrcode}},
                {"/api/v1/group/face2face", group_handler, #{action => face2face}},
                {"/api/v1/group/face2face_save", group_handler, #{action => face2face_save}},
                {"/api/v1/group/add", group_handler, #{action => add}},
                {"/api/v1/group/edit", group_handler, #{action => edit}},
                % 群级 E2EE 开关（仅群主，0→1 单向，P0-B B4）
                {"/api/v1/group/set_e2ee_mode", group_handler, #{action => set_e2ee_mode}},
                {"/api/v1/group/dissolve", group_handler, #{action => dissolve}},
                {"/api/v1/group/detail", group_handler, #{action => detail}},
                {"/api/v1/group/page", group_handler, #{action => page}},
                {"/api/v1/group/msg_page", group_handler, #{action => msg_page}},

                % 群组分类管理
                {"/api/v1/group/category/create", group_category_handler, #{action => create}},
                {"/api/v1/group/category/list", group_category_handler, #{action => list}},
                {"/api/v1/group/category/rename", group_category_handler, #{action => rename}},
                {"/api/v1/group/category/delete", group_category_handler, #{action => delete}},
                {"/api/v1/group/category/move_group", group_category_handler, #{
                    action => move_group
                }},
                {"/api/v1/group/category/sort", group_category_handler, #{action => sort}},

                {"/api/v1/group_member/join", group_member_handler, #{action => join}},
                {"/api/v1/group_member/leave", group_member_handler, #{action => leave}},
                {"/api/v1/group_member/page", group_member_handler, #{action => page}},
                {"/api/v1/group_member/alias", group_member_handler, #{action => alias}},
                {"/api/v1/group_member/same_group", group_member_handler, #{action => same_group}},
                {"/api/v1/group_member/mute", group_member_handler, #{action => mute}},
                {"/api/v1/group_member/unmute", group_member_handler, #{action => unmute}},
                {"/api/v1/group_member/role", group_member_handler, #{action => role}},
                {"/api/v1/group/transfer", group_handler, #{action => transfer}},
                % 群组标签
                {"/api/v1/group/tag/add", group_tag_handler, #{action => add}},
                {"/api/v1/group/tag/remove", group_tag_handler, #{action => remove}},
                {"/api/v1/group/tag/list", group_tag_handler, #{action => list}},
                {"/api/v1/group/tag/search", group_tag_handler, #{action => search}},
                {"/api/v1/group/tag/hot", group_tag_handler, #{action => hot}},
                % 群组公告
                {"/api/v1/group_notice/add", group_notice_handler, #{action => add}},
                {"/api/v1/group_notice/edit", group_notice_handler, #{action => edit}},
                {"/api/v1/group_notice/delete", group_notice_handler, #{action => delete}},
                {"/api/v1/group_notice/page", group_notice_handler, #{action => page}},
                {"/api/v1/group_notice/publish", group_notice_handler, #{action => publish}},
                {"/api/v1/group_notice/latest", group_notice_handler, #{action => latest}},
                {"/api/v1/group/notice/list", group_notice_handler, #{action => list}},
                {"/api/v1/group/notice/detail", group_notice_handler, #{action => detail}},
                {"/api/v1/group/notice/pin", group_notice_handler, #{action => pin}},
                {"/api/v1/group/notice/unpin", group_notice_handler, #{action => unpin}},
                {"/api/v1/group/notice/mark_read", group_notice_handler, #{action => mark_read}},
                % 群投票 API
                {"/api/v1/group/vote/create", group_vote_handler, #{action => create}},
                {"/api/v1/group/vote/list", group_vote_handler, #{action => list}},
                {"/api/v1/group/vote/detail", group_vote_handler, #{action => detail}},
                {"/api/v1/group/vote/cast", group_vote_handler, #{action => cast}},
                {"/api/v1/group/vote/update", group_vote_handler, #{action => update}},
                {"/api/v1/group/vote/cancel", group_vote_handler, #{action => cancel}},
                {"/api/v1/group/vote/close", group_vote_handler, #{action => close}},
                {"/api/v1/group/vote/my_vote", group_vote_handler, #{action => my_vote}},
                % 群组日程 API
                {"/api/v1/group_schedule/create", group_schedule_handler, #{action => create}},
                {"/api/v1/group_schedule/update", group_schedule_handler, #{action => update}},
                {"/api/v1/group_schedule/cancel", group_schedule_handler, #{action => cancel}},
                {"/api/v1/group_schedule/detail", group_schedule_handler, #{action => detail}},
                {"/api/v1/group_schedule/list", group_schedule_handler, #{action => list}},
                {"/api/v1/group_schedule/my_list", group_schedule_handler, #{action => my_list}},
                {"/api/v1/group_schedule/confirm", group_schedule_handler, #{action => confirm}},
                % @提及功能 API
                {"/api/v1/mention/list", mention_handler, #{action => list}},
                {"/api/v1/mention/unread", mention_handler, #{action => unread}},
                {"/api/v1/mention/mark_read", mention_handler, #{action => mark_read}},
                {"/api/v1/mention/suggest", mention_handler, #{action => suggest}},
                % 群相册 API
                {"/api/v1/group_album/create", group_album_handler, #{action => create_album}},
                {"/api/v1/group_album/list", group_album_handler, #{action => list_albums}},
                {"/api/v1/group_album/rename", group_album_handler, #{action => rename_album}},
                {"/api/v1/group_album/delete", group_album_handler, #{action => delete_album}},
                {"/api/v1/group_album/photo/upload", group_album_handler, #{action => upload_photo}},
                {"/api/v1/group_album/photo/batch", group_album_handler, #{action => batch_upload}},
                {"/api/v1/group_album/photo/list", group_album_handler, #{action => list_photos}},
                {"/api/v1/group_album/photo/detail", group_album_handler, #{action => photo_detail}},
                {"/api/v1/group_album/photo/delete", group_album_handler, #{action => delete_photo}},
                {"/api/v1/group_album/photo/like", group_album_handler, #{action => like_photo}},
                {"/api/v1/group_album/photo/unlike", group_album_handler, #{action => unlike_photo}},
                {"/api/v1/group_album/photo/comment", group_album_handler, #{action => add_comment}},
                {"/api/v1/group_album/photo/comments", group_album_handler, #{
                    action => list_comments
                }},
                {"/api/v1/group_album/cover/update", group_album_handler, #{action => update_cover}},

                % 频道功能 API
                {"/api/v1/channel/create", channel_handler, #{action => create}},
                {"/api/v1/channel/:channel_id", channel_handler, #{action => show}},
                {"/api/v1/channel/by_custom_id/:custom_id", channel_handler, #{
                    action => by_custom_id
                }},
                {"/api/v1/channel/:channel_id/update", channel_handler, #{action => update}},
                {"/api/v1/channel/:channel_id/delete", channel_handler, #{action => delete}},
                {"/api/v1/channel/:channel_id/subscribe", channel_handler, #{action => subscribe}},
                {"/api/v1/channel/:channel_id/unsubscribe", channel_handler, #{
                    action => unsubscribe
                }},
                {"/api/v1/channels/subscribed", channel_handler, #{action => subscribed}},
                {"/api/v1/channels/managed", channel_handler, #{action => managed}},
                {"/api/v1/channels/unread/summary", channel_handler, #{action => unread_summary}},
                {"/api/v1/channel/:channel_id/message", channel_handler, #{
                    action => publish_message
                }},
                {"/api/v1/channel/:channel_id/messages", channel_handler, #{action => messages}},
                {"/api/v1/channel/:channel_id/read", channel_handler, #{action => mark_read}},
                {"/api/v1/channels/search", channel_handler, #{action => search}},
                {"/api/v1/channels/discover", channel_handler, #{action => discover}},
                {"/api/v1/channel/:channel_id/admin", channel_handler, #{action => add_admin}},
                {"/api/v1/channel/:channel_id/admins", channel_handler_admin, #{action => admins}},
                {"/api/v1/channel/:channel_id/admin/:user_id/role", channel_handler_admin, #{
                    action => update_admin_role
                }},
                {"/api/v1/channel/:channel_id/admin/:user_id", channel_handler_admin, #{
                    action => remove_admin
                }},
                % 频道统计 API
                {"/api/v1/channel/:channel_id/stats", channel_handler, #{action => stats}},
                {"/api/v1/channel/:channel_id/stats/daily", channel_handler, #{
                    action => stats_daily
                }},
                {"/api/v1/channel/:channel_id/message/:message_id/view", channel_handler_message, #{
                        action => record_view
                    }},
                {"/api/v1/channel/:channel_id/message/:message_id/reaction",
                    channel_handler_message, #{
                        action => add_reaction
                    }},
                {"/api/v1/channel/:channel_id/message/:message_id/reaction/:reaction_type",
                    channel_handler_message, #{action => remove_reaction}},
                % 消息管理 API
                {"/api/v1/channel/:channel_id/message/:message_id/pin", channel_handler_message, #{
                    action => pin_message
                }},
                {"/api/v1/channel/:channel_id/message/:message_id/delete", channel_handler_message,
                    #{
                        action => delete_message
                    }},
                {"/api/v1/channel/:channel_id/message/:message_id/revoke", channel_handler_message,
                    #{
                        action => revoke_message
                    }},
                {"/api/v1/channel/:channel_id/message/:message_id/edit", channel_handler_message, #{
                        action => edit_message
                    }},
                % 评论 API（对标公众号/知识星球评论）
                {"/api/v1/channel/:channel_id/message/:message_id/comments",
                    channel_handler_comment, #{
                        action => list_comments
                    }},
                {"/api/v1/channel/:channel_id/message/:message_id/comment", channel_handler_comment,
                    #{
                        action => create_comment
                    }},
                {"/api/v1/channel/:channel_id/comment/:comment_id/delete", channel_handler_comment,
                    #{
                        action => delete_comment
                    }},
                {"/api/v1/channel/:channel_id/comment/:comment_id/like", channel_handler_comment, #{
                        action => like_comment
                    }},
                {"/api/v1/channel/:channel_id/comment/:comment_id/unlike", channel_handler_comment,
                    #{
                        action => unlike_comment
                    }},
                % 订阅者管理 API
                {"/api/v1/channel/:channel_id/subscribers", channel_handler_message, #{
                    action => subscribers
                }},
                {"/api/v1/channel/:channel_id/subscriber/:user_id", channel_handler_admin, #{
                    action => remove_subscriber
                }},
                % 邀请相关 API（私有频道）
                {"/api/v1/channel/:channel_id/invitation", channel_handler_admin, #{
                    action => create_invitation
                }},
                {"/api/v1/channel/invitation/accept", channel_handler_admin, #{
                    action => accept_invitation
                }},
                {"/api/v1/channel/invitation/reject", channel_handler_admin, #{
                    action => reject_invitation
                }},
                {"/api/v1/channel/invitations/my", channel_handler_admin, #{
                    action => my_invitations
                }},
                {"/api/v1/channel/invitations/sent", channel_handler_admin, #{
                    action => sent_invitations
                }},
                % 订单相关 API（付费频道）
                {"/api/v1/channel/:channel_id/order", channel_handler_order, #{
                    action => create_order
                }},
                {"/api/v1/channel/order/pay", channel_handler_order, #{action => pay_order}},
                {"/api/v1/channel/order/cancel", channel_handler_order, #{action => cancel_order}},
                % 退款必须置于 :order_no 通配之前，否则 "refund" 会被当作 order_no
                {"/api/v1/channel/order/refund", channel_handler_order, #{action => refund_order}},
                {"/api/v1/channel/orders/my", channel_handler_order, #{action => my_orders}},
                {"/api/v1/channel/order/:order_no", channel_handler_order, #{action => get_order}},
                {"/api/v1/channels/sync", channel_handler_admin, #{action => sync}},

                % 频道 incoming webhook 管理（须频道管理员 role>=2）
                {"/api/v1/channel/:channel_id/webhook/create", channel_webhook_handler, #{
                    action => create
                }},
                {"/api/v1/channel/:channel_id/webhook/list", channel_webhook_handler, #{
                    action => list
                }},
                {"/api/v1/channel/:channel_id/webhook/:webhook_id/disable", channel_webhook_handler,
                    #{action => disable}},
                % 频道 incoming webhook 入站（token 即凭证，免 JWT/免 902 签名，
                % 放行见 auth_middleware_api_v1 的 IsChannelWebhook 前缀）
                {"/api/v1/webhook/channel/:token", channel_webhook_handler, #{
                    action => incoming
                }},

                % 群文件管理 API
                {"/api/v1/group/file/upload", group_file_handler, #{action => upload}},
                {"/api/v1/group/file/download", group_file_handler, #{action => download}},
                {"/api/v1/group/file/list", group_file_handler, #{action => list}},
                {"/api/v1/group/file/delete", group_file_handler, #{action => delete}},
                {"/api/v1/group/file/search", group_file_handler, #{action => search}},
                {"/api/v1/group/file/categories", group_file_handler, #{action => categories}},

                % 群作业管理 API
                {"/api/v1/group/task/create", group_task_handler, #{action => create}},
                {"/api/v1/group/task/update", group_task_handler, #{action => update}},
                {"/api/v1/group/task/assign", group_task_handler, #{action => assign}},
                {"/api/v1/group/task/submit", group_task_handler, #{action => submit}},
                {"/api/v1/group/task/review", group_task_handler, #{action => review}},
                {"/api/v1/group/task/list", group_task_handler, #{action => list}},
                {"/api/v1/group/task/detail", group_task_handler, #{action => detail}},
                {"/api/v1/group/task/my", group_task_handler, #{action => my_tasks}},
                %% Phase 4 T4.2：群内 agent 任务审批卡片端点
                {"/api/v1/agent_task/approve", agent_task_handler, #{action => approve}},
                {"/api/v1/agent_task/reject", agent_task_handler, #{action => reject}},
                {"/api/v1/group/task/pending", group_task_handler, #{action => pending_review}},

                {"/api/v1/report/create", report_handler, #{action => create}},
                {"/api/v1/moment/report/create", report_handler, #{action => moment_create}},
                {"/api/v1/moment/create", moment_handler, #{action => create}},
                {"/api/v1/moment/:moment_id", moment_handler, #{action => show}},
                {"/api/v1/moment/:moment_id/delete", moment_handler, #{action => delete}},
                {"/api/v1/moments/feed", moment_handler, #{action => feed}},
                {"/api/v1/moments/user/:uid", moment_handler, #{action => user_posts}},
                {"/api/v1/moment/:moment_id/like", moment_handler, #{action => like}},
                {"/api/v1/moment/:moment_id/unlike", moment_handler, #{action => unlike}},
                {"/api/v1/moment/:moment_id/comment", moment_handler, #{action => add_comment}},
                {"/api/v1/moment/:moment_id/comments", moment_handler, #{action => comments}},
                {"/api/v1/moment/:moment_id/comment/:comment_id/delete", moment_handler, #{
                    action => delete_comment
                }},
                {"/api/v1/moment/:moment_id/report", moment_handler, #{action => report}},

                % 直播间 API
                {"/api/v1/live_room/list", live_room_handler, #{action => list}},
                {"/api/v1/live_room/my_list", live_room_handler, #{action => my_list}},
                {"/api/v1/live_room/create", live_room_handler, #{action => create}},
                {"/api/v1/live_room/start", live_room_handler, #{action => start}},
                {"/api/v1/live_room/stop", live_room_handler, #{action => stop}},
                {"/api/v1/live_room/detail", live_room_handler, #{action => detail}},

                % 钱包 API
                {"/api/v1/wallet/balance", wallet_handler, #{action => balance}},
                {"/api/v1/wallet/transactions", wallet_handler, #{action => transactions}},
                % topup 为 mock 充值（仅非生产）；真实充值走下方 recharge/* 链路
                {"/api/v1/wallet/topup", wallet_handler, #{action => topup}},
                % 充值（真实网关）：创建充值订单 -> 拉起第三方支付 -> 查询
                {"/api/v1/wallet/recharge/order", wallet_handler, #{action => recharge_order}},
                {"/api/v1/wallet/recharge/pay", wallet_handler, #{action => recharge_pay}},
                {"/api/v1/wallet/recharge/:order_no", wallet_handler, #{action => recharge_query}},
                % 红包与转账/提现 API
                {"/api/v1/wallet/red_packet/send", wallet_handler, #{action => red_packet_send}},
                {"/api/v1/wallet/red_packet/open", wallet_handler, #{action => red_packet_open}},
                {"/api/v1/wallet/red_packet/:id/detail", wallet_handler, #{
                    action => red_packet_detail
                }},
                {"/api/v1/wallet/transfer/send", wallet_handler, #{action => transfer_send}},
                {"/api/v1/wallet/transfer/accept", wallet_handler, #{action => transfer_accept}},
                {"/api/v1/wallet/withdraw", wallet_handler, #{action => withdraw}},

                % Agent 受控支付授权 API（JWT 认证，owner 管理自己 agent 的代付额度；
                % owner_uid 恒取 JWT current_uid，金钱红线全在 agent_payment_mandate_logic）
                {"/api/v1/agent/mandate/authorize", agent_mandate_handler, #{action => authorize}},
                {"/api/v1/agent/mandate/revoke", agent_mandate_handler, #{action => revoke}},
                {"/api/v1/agent/mandate/active", agent_mandate_handler, #{action => active}},

                % 统一支付回调 webhook（第三方支付服务器回调，无 JWT，见 open/0）
                {"/api/v1/payment/callback/:gateway", payment_callback_handler, #{action => notify}},

                % SaaS 计费 API（支付线E：套餐订阅 + 用量配额 + 账单）
                % 管理端套餐 CRUD（plan_create/plan_update）已迁至 /adm/finance/billing/*
                % （走 adm_acl finance:write RBAC 门），此处不再暴露于 /v1（BILL-01）。
                % plan_list 保留：套餐目录对所有登录用户可见（仅需 JWT）。
                {"/api/v1/billing/plan/list", billing_handler, #{action => plan_list}},
                % 租户端：订阅
                {"/api/v1/billing/subscribe", billing_handler, #{action => subscribe}},
                {"/api/v1/billing/renew", billing_handler, #{action => renew}},
                {"/api/v1/billing/cancel", billing_handler, #{action => cancel}},
                {"/api/v1/billing/subscription", billing_handler, #{action => subscription}},
                % 用量与配额
                {"/api/v1/billing/usage", billing_handler, #{action => report_usage}},
                {"/api/v1/billing/quota", billing_handler, #{action => check_quota}},
                % 账单
                {"/api/v1/billing/invoice/generate", billing_handler, #{action => invoice_generate}},
                {"/api/v1/billing/invoice/pay", billing_handler, #{action => invoice_pay}},
                {"/api/v1/billing/invoice/list", billing_handler, #{action => invoice_list}},

                % 附件 presign API（需 JWT 认证）
                {"/api/v1/attachment/presign", attach_handler, #{action => presign}},
                % 附件直传成功回调落库（需 JWT 认证）
                {"/api/v1/attachment/confirm", attach_handler, #{action => confirm}},
                % 附件下载短时签发 URL，替代 bucket 公开读（需 JWT 认证）
                {"/api/v1/attachment/view_url", attach_handler, #{action => view_url}}
            ],

    % Admin routes (原 imadm)
    AdmRoutes = [
        {"/api/adm", adm_index_handler, #{action => index}},
        {"/api/adm/index", adm_index_handler, #{action => index}},
        % 首启初始化向导（P0-5）— 免鉴权，见 open/0
        {"/api/adm/setup/status", adm_setup_handler, #{action => status}},
        {"/api/adm/setup/init", adm_setup_handler, #{action => init_setup}},
        {"/api/adm/current", adm_index_handler, #{action => current}},
        {"/api/adm/rbac/me", adm_index_handler, #{action => rbac}},
        {"/api/adm/welcome", adm_index_handler, #{action => welcome}},
        {"/api/adm/feedback/index", adm_feedback_handler, #{action => index}},
        {"/api/adm/admin/config/features", adm_admin_handler, #{action => config_features}},
        {"/api/adm/admin/config/policy/bootstrap", adm_admin_handler, #{
            action => config_policy_bootstrap
        }},
        {"/api/adm/admin/config/policy/meta", adm_admin_handler, #{action => config_policy_meta}},
        {"/api/adm/admin/config/policy/preview", adm_admin_handler, #{
            action => config_policy_preview
        }},
        {"/api/adm/admin/config/policy/saved", adm_admin_handler, #{action => config_policy_saved}},
        {"/api/adm/admin/config/policy", adm_admin_handler, #{action => config_policy}},
        {"/api/adm/admin/config/sidebar", adm_admin_handler, #{action => config_sidebar}},
        {"/api/adm/admin/config/feedback-workflow", adm_admin_handler, #{
            action => config_feedback_workflow
        }},
        % UX 埋点上报（前端 uxTelemetryReporter 按 5s 批量 POST 到此端点）
        {"/api/adm/admin/ux/events", adm_stats_handler, #{
            action => ux_events
        }},
        % 禁言用户管理 API
        {"/api/adm/admin/muted_users/list", adm_admin_handler, #{action => muted_users_list}},
        {"/api/adm/admin/muted_users/unmute", adm_admin_handler, #{action => muted_users_unmute}},
        {"/api/adm/admin/muted_users/unmute_batch", adm_admin_handler, #{
            action => muted_users_unmute_batch
        }},
        % 推送 Token 管理 API
        {"/api/adm/admin/push_token/list", adm_admin_handler, #{action => push_token_list}},
        % 合规密钥管理 API
        {"/api/adm/admin/compliance_key/list", adm_admin_handler, #{action => compliance_key_list}},
        {"/api/adm/admin/compliance_key/create", adm_admin_handler, #{
            action => compliance_key_create
        }},
        {"/api/adm/admin/compliance_key/revoke", adm_admin_handler, #{
            action => compliance_key_revoke
        }},
        {"/api/adm/admin/list", adm_admin_handler, #{action => list}},
        {"/api/adm/admin/create", adm_admin_handler, #{action => create}},
        {"/api/adm/admin/assign_role", adm_admin_handler, #{action => assign_role}},
        {"/api/adm/admin/disable", adm_admin_handler, #{action => disable}},
        {"/api/adm/feedback/reply", adm_feedback_handler, #{action => reply}},
        {"/api/adm/feedback/status", adm_feedback_handler, #{action => status}},
        {"/api/adm/feedback/delete", adm_feedback_handler, #{action => delete}},
        {"/api/adm/role/list", adm_role_handler, #{action => list}},
        {"/api/adm/roles/list", adm_role_handler, #{action => list}},
        {"/api/adm/role/create", adm_role_handler, #{action => create}},
        {"/api/adm/roles/create", adm_role_handler, #{action => create}},
        {"/api/adm/role/permissions/save", adm_role_handler, #{action => permissions_save}},
        {"/api/adm/role/permission/update", adm_role_handler, #{action => permissions_save}},
        {"/api/adm/roles/permissions/save", adm_role_handler, #{action => permissions_save}},
        {"/api/adm/role/disable", adm_role_handler, #{action => disable}},
        {"/api/adm/roles/disable", adm_role_handler, #{action => disable}},
        {"/api/adm/role/delete", adm_role_handler, #{action => delete}},
        {"/api/adm/roles/delete", adm_role_handler, #{action => delete}},
        {"/api/adm/ai_agent/list", adm_ai_agent_handler, #{action => list}},
        {"/api/adm/ai_agent/detail", adm_ai_agent_handler, #{action => detail}},
        {"/api/adm/ai_agent/create", adm_ai_agent_handler, #{action => create}},
        {"/api/adm/ai_agent/update", adm_ai_agent_handler, #{action => update}},
        {"/api/adm/ai_agent/set_status", adm_ai_agent_handler, #{action => set_status}},
        % ai_roles 人格 KV 管理：GET 读 / POST 保存删除（users:read|update）
        {"/api/adm/ai_agent/roles", adm_ai_agent_handler, #{action => roles}},
        % 头像上传（multipart → Garage）：POST（users:update）
        {"/api/adm/ai_agent/upload_avatar", adm_ai_agent_handler, #{action => upload_avatar}},
        % 新手引导配置（AI 冷启动）：GET 读 / POST 半量保存（users:read|update）
        {"/api/adm/ai_agent/onboarding_config", adm_ai_agent_handler, #{
            action => onboarding_config
        }},
        % 知识库配置（A3-1）：群规/FAQ，供 @管家 答疑注入（users:read|update）
        {"/api/adm/ai_agent/knowledge_config", adm_ai_agent_handler, #{
            action => knowledge_config
        }},
        % 版本化 AI 角色模板：分页、详情、创建、草稿、发布、状态
        {"/api/adm/ai_agent/role/list", adm_ai_agent_handler, #{action => role_list}},
        {"/api/adm/ai_agent/role/detail", adm_ai_agent_handler, #{action => role_detail}},
        {"/api/adm/ai_agent/role/create", adm_ai_agent_handler, #{action => role_create}},
        {"/api/adm/ai_agent/role/draft", adm_ai_agent_handler, #{action => role_draft}},
        {"/api/adm/ai_agent/role/publish", adm_ai_agent_handler, #{action => role_publish}},
        {"/api/adm/ai_agent/role/set_status", adm_ai_agent_handler, #{
            action => role_set_status
        }},
        % admin 应急入口(c)：代运营为 agent 创建受控支付授权（finance:write RBAC）
        {"/api/adm/ai_agent/mandate_create", adm_ai_agent_handler, #{action => mandate_create}},
        {"/api/adm/mcp/clients", adm_mcp_handler, #{action => list}},
        {"/api/adm/mcp/clients/approve", adm_mcp_handler, #{action => approve}},
        {"/api/adm/mcp/clients/reject", adm_mcp_handler, #{action => reject}},
        {"/api/adm/mcp/clients/revoke", adm_mcp_handler, #{action => revoke}},
        {"/api/adm/mcp/clients/grants", adm_mcp_handler, #{action => grants}},
        {"/api/adm/mcp/audit", adm_mcp_handler, #{action => audit}},
        {"/api/adm/app_ddl/index", adm_app_ddl_handler, #{action => index}},
        {"/api/adm/app_ddl/save", adm_app_ddl_handler, #{action => save}},
        {"/api/adm/app_ddl/delete", adm_app_ddl_handler, #{action => delete}},
        {"/api/adm/app_version/index", adm_app_version_handler, #{action => index}},
        {"/api/adm/app_version/save", adm_app_version_handler, #{action => save}},
        {"/api/adm/app_version/delete", adm_app_version_handler, #{action => delete}},
        {"/api/adm/app_version/version_stats", adm_app_version_handler, #{action => version_stats}},
        % 存储管理 API
        {"/api/adm/storage/stats", adm_attach_handler, #{action => stats}},
        {"/api/adm/storage/index", adm_attach_handler, #{action => index}},
        {"/api/adm/storage/disable", adm_attach_handler, #{action => disable}},
        {"/api/adm/storage/enable", adm_attach_handler, #{action => enable}},
        {"/api/adm/storage/delete", adm_attach_handler, #{action => delete}},
        {"/api/adm/storage/download", adm_attach_handler, #{action => download}},
        {"/api/adm/storage/orphan", adm_attach_handler, #{action => orphan}},
        {"/api/adm/storage/orphan/cleanup", adm_attach_handler, #{action => orphan_cleanup}},
        {"/api/adm/passport/meta", adm_passport_handler, #{action => meta}},
        {"/api/adm/passport/login", adm_passport_handler, #{action => login}},
        {"/api/adm/passport/captcha", adm_passport_handler, #{action => captcha}},
        {"/api/adm/passport/do_login", adm_passport_handler, #{action => do_login}},
        {"/api/adm/passport/logout", adm_passport_handler, #{action => logout}},
        % 用户管理 API
        {"/api/adm/user/list", adm_user_handler, #{action => list}},
        {"/api/adm/user/detail", adm_user_handler, #{action => detail}},
        {"/api/adm/user/ban", adm_user_handler, #{action => ban}},
        {"/api/adm/user/unban", adm_user_handler, #{action => unban}},
        {"/api/adm/user/force_logout", adm_user_handler, #{action => force_logout}},
        {"/api/adm/user/devices", adm_user_handler, #{action => devices}},
        {"/api/adm/user/device/kick", adm_user_handler, #{action => device_kick}},
        {"/api/adm/operation_logs", adm_operation_log_handler, #{action => list}},
        {"/api/adm/user/search", adm_user_handler, #{action => search}},
        {"/api/adm/user/tag/list", adm_user_handler, #{action => tag_list}},
        {"/api/adm/user/tag/delete", adm_user_handler, #{action => tag_delete}},
        {"/api/adm/user/collect/list", adm_user_handler, #{action => collect_list}},
        {"/api/adm/user/collect/remove", adm_user_handler, #{action => collect_remove}},
        {"/api/adm/user/logout_apply/list", adm_logout_apply_handler, #{action => list}},
        {"/api/adm/user/logout_apply/export", adm_logout_apply_handler, #{action => export}},
        {"/api/adm/user/logout_apply/reject", adm_logout_apply_handler, #{action => reject}},
        {"/api/adm/user/logout_apply/approve", adm_logout_apply_handler, #{action => approve}},
        % 群组管理 API
        {"/api/adm/group/list", adm_group_handler, #{action => list}},
        {"/api/adm/group/detail", adm_group_handler, #{action => detail}},
        {"/api/adm/group/dissolve", adm_group_handler, #{action => dissolve}},
        {"/api/adm/group/update", adm_group_handler, #{action => update}},
        {"/api/adm/group/search", adm_group_handler, #{action => search}},
        {"/api/adm/group/member/kick", adm_group_handler, #{action => kick_member}},
        {"/api/adm/group/members", adm_group_handler, #{action => members}},
        % 群组子功能 API
        {"/api/adm/group/vote/list", adm_group_vote_handler, #{action => vote_list}},
        {"/api/adm/group/vote/detail", adm_group_vote_handler, #{action => vote_detail}},
        {"/api/adm/group/vote/close", adm_group_vote_handler, #{action => vote_close}},
        {"/api/adm/group/notice/list", adm_group_notice_handler, #{action => notice_list}},
        {"/api/adm/group/notice/detail", adm_group_notice_handler, #{action => notice_detail}},
        {"/api/adm/group/notice/delete", adm_group_notice_handler, #{action => notice_delete}},
        {"/api/adm/group/tag/list", adm_group_content_handler, #{action => tag_list}},
        {"/api/adm/group/tag/delete", adm_group_content_handler, #{action => tag_delete}},
        {"/api/adm/group/category/list", adm_group_content_handler, #{action => category_list}},
        {"/api/adm/group/category/delete", adm_group_content_handler, #{action => category_delete}},
        {"/api/adm/group/file/list", adm_group_content_handler, #{action => file_list}},
        {"/api/adm/group/file/detail", adm_group_content_handler, #{action => file_detail}},
        {"/api/adm/group/file/delete", adm_group_content_handler, #{action => file_delete}},
        {"/api/adm/group/album/list", adm_group_content_handler, #{action => album_list}},
        {"/api/adm/group/album/detail", adm_group_content_handler, #{action => album_detail}},
        {"/api/adm/group/album/delete", adm_group_content_handler, #{action => album_delete}},
        {"/api/adm/group/schedule/list", adm_group_schedule_handler, #{action => schedule_list}},
        {"/api/adm/group/schedule/detail", adm_group_schedule_handler, #{action => schedule_detail}},
        {"/api/adm/group/schedule/cancel", adm_group_schedule_handler, #{action => schedule_cancel}},
        {"/api/adm/group/schedule/restore", adm_group_schedule_handler, #{
            action => schedule_restore
        }},
        {"/api/adm/group/governance_log/list", adm_group_schedule_handler, #{
            action => governance_log_list
        }},
        {"/api/adm/group/task/list", adm_group_task_handler, #{action => task_list}},
        {"/api/adm/group/task/detail", adm_group_task_handler, #{action => task_detail}},
        {"/api/adm/group/task/pending_review", adm_group_task_handler, #{
            action => task_pending_review
        }},
        {"/api/adm/group/task/review", adm_group_task_handler, #{action => task_review}},
        {"/api/adm/group/task/restore", adm_group_task_handler, #{action => task_restore}},
        {"/api/adm/group/task/close", adm_group_task_handler, #{action => task_close}},
        {"/api/adm/group/task/delete", adm_group_task_handler, #{action => task_delete}},
        % 消息管理 API
        {"/api/adm/message/list", adm_message_handler, #{action => list}},
        {"/api/adm/message/detail", adm_message_handler, #{action => detail}},
        {"/api/adm/message/export", adm_message_handler, #{action => export}},
        % 频道管理 API
        {"/api/adm/channel/list", adm_channel_handler, #{action => list}},
        %% 固定段路由置于 :channel_id 通配之前，避免 order 被当作 channel_id
        {"/api/adm/channel/order/refund", adm_channel_handler, #{action => refund_order}},
        {"/api/adm/channel/detail/:channel_id", adm_channel_handler, #{action => detail}},
        {"/api/adm/channel/:channel_id/messages", adm_channel_handler, #{action => messages}},
        {"/api/adm/channel/:channel_id/subscribers", adm_channel_handler, #{action => subscribers}},
        {"/api/adm/channel/:channel_id/subscriber/:user_id", adm_channel_handler, #{
            action => remove_subscriber
        }},
        {"/api/adm/channel/:channel_id/admins", adm_channel_handler, #{action => admins}},
        {"/api/adm/channel/:channel_id/admin/:user_id/role", adm_channel_handler, #{
            action => update_admin_role
        }},
        {"/api/adm/channel/:channel_id/admin/:user_id", adm_channel_handler, #{
            action => remove_admin
        }},
        {"/api/adm/channel/:channel_id/invitations", adm_channel_handler, #{action => invitations}},
        {"/api/adm/channel/:channel_id/orders", adm_channel_handler, #{action => orders}},
        {"/api/adm/channel/:channel_id/stats", adm_channel_handler, #{action => stats}},
        {"/api/adm/channel/:channel_id/message/:message_id/pin", adm_channel_handler, #{
            action => pin_message
        }},
        {"/api/adm/channel/:channel_id/message/:message_id/delete", adm_channel_handler, #{
            action => delete_message
        }},
        {"/api/adm/channel/:channel_id/price", adm_channel_handler, #{action => set_price}},
        {"/api/adm/channel/search", adm_channel_handler, #{action => search}},
        {"/api/adm/channel/delete", adm_channel_handler, #{action => delete}},
        % 运营财务 API（跨用户钱包/充值/支付/SaaS 计费查询）
        {"/api/adm/finance/wallets", adm_finance_handler, #{action => wallets}},
        {"/api/adm/finance/wallet/:user_id/transactions", adm_finance_handler, #{
            action => wallet_transactions
        }},
        {"/api/adm/finance/recharge-orders", adm_finance_handler, #{action => recharge_orders}},
        {"/api/adm/finance/recharge-orders/refund", adm_finance_handler, #{
            action => recharge_order_refund
        }},
        {"/api/adm/finance/payment-transactions", adm_finance_handler, #{
            action => payment_transactions
        }},
        {"/api/adm/finance/payment-transactions/refund", adm_finance_handler, #{
            action => payment_transaction_refund
        }},
        {"/api/adm/finance/wallets/freeze", adm_finance_handler, #{action => wallet_freeze}},
        {"/api/adm/finance/wallets/unfreeze", adm_finance_handler, #{action => wallet_unfreeze}},
        {"/api/adm/finance/billing/plans", adm_finance_handler, #{action => billing_plans}},
        {"/api/adm/finance/billing/plan", adm_finance_handler, #{action => billing_plan_create}},
        {"/api/adm/finance/billing/plan/update", adm_finance_handler, #{
            action => billing_plan_update
        }},
        {"/api/adm/finance/billing/subscriptions", adm_finance_handler, #{
            action => billing_subscriptions
        }},
        {"/api/adm/finance/billing/invoices", adm_finance_handler, #{action => billing_invoices}},
        {"/api/adm/finance/withdrawals", adm_finance_handler, #{action => withdrawals}},
        {"/api/adm/finance/withdrawals/complete", adm_finance_handler, #{
            action => withdrawal_complete
        }},
        {"/api/adm/finance/withdrawals/reject", adm_finance_handler, #{action => withdrawal_reject}},
        % Moment 与举报治理 API
        {"/api/adm/moment/list", adm_moment_handler, #{action => list}},
        {"/api/adm/moment/detail/:moment_id", adm_moment_handler, #{action => detail}},
        {"/api/adm/moment/delete", adm_moment_handler, #{action => delete}},
        {"/api/adm/moment/report/list", adm_moment_handler, #{action => report_list}},
        {"/api/adm/moment/report/resolve", adm_moment_handler, #{action => report_resolve}},
        {"/api/adm/moment/report/batch_resolve", adm_moment_handler, #{
            action => report_batch_resolve
        }},
        {"/api/adm/report/create", adm_report_handler, #{action => create}},
        {"/api/adm/report/list", adm_report_handler, #{action => list}},
        {"/api/adm/report/resolve", adm_report_handler, #{action => resolve}},
        {"/api/adm/report/batch_resolve", adm_report_handler, #{action => batch_resolve}},
        {"/api/adm/group/report/list", adm_report_handler, #{action => group_list}},
        {"/api/adm/group/report/resolve", adm_report_handler, #{action => group_resolve}},
        {"/api/adm/group/report/batch_resolve", adm_report_handler, #{
            action => group_batch_resolve
        }},
        {"/api/adm/channel/report/list", adm_report_handler, #{action => channel_list}},
        {"/api/adm/channel/report/resolve", adm_report_handler, #{action => channel_resolve}},
        {"/api/adm/channel/report/batch_resolve", adm_report_handler, #{
            action => channel_batch_resolve
        }},
        {"/api/adm/user/report/list", adm_report_handler, #{action => user_list}},
        {"/api/adm/user/report/resolve", adm_report_handler, #{action => user_resolve}},
        {"/api/adm/user/report/batch_resolve", adm_report_handler, #{action => user_batch_resolve}},
        % 统计 API
        {"/api/adm/announcement/index", adm_announcement_handler, #{action => index}},
        {"/api/adm/announcement/create", adm_announcement_handler, #{action => create}},
        {"/api/adm/announcement/update", adm_announcement_handler, #{action => update}},
        {"/api/adm/announcement/delete", adm_announcement_handler, #{action => delete}},
        {"/api/adm/announcement/publish", adm_announcement_handler, #{action => publish}},
        {"/api/adm/announcement/unpublish", adm_announcement_handler, #{action => unpublish}},
        % 内容审核：敏感词黑名单 + 消息人工复审队列（import/:id 须置于通配路由之前）
        {"/api/adm/moderation/sensitive-words/import", adm_moderation_handler, #{
            action => sensitive_words_import
        }},
        {"/api/adm/moderation/sensitive-words/:id", adm_moderation_handler, #{
            action => sensitive_word_delete
        }},
        {"/api/adm/moderation/sensitive-words", adm_moderation_handler, #{
            action => sensitive_words
        }},
        {"/api/adm/moderation/review-queue/:id/moderate", adm_moderation_handler, #{
            action => review_moderate
        }},
        {"/api/adm/moderation/review-queue", adm_moderation_handler, #{action => review_queue}},
        % SSO 外部认证配置（GET/POST 同路径按 method 分派）
        {"/api/adm/sso/config", adm_sso_handler, #{action => config}},
        {"/api/adm/sso/test", adm_sso_handler, #{action => test}},
        % 插件生命周期管理 API (lifecycle.md §10)
        {"/api/adm/plugin/list", adm_plugin_handler, #{action => list}},
        {"/api/adm/plugin/detail", adm_plugin_handler, #{action => detail}},
        {"/api/adm/plugin/state", adm_plugin_handler, #{action => state_query}},
        {"/api/adm/plugin/health", adm_plugin_handler, #{action => health}},
        {"/api/adm/plugin/install", adm_plugin_handler, #{action => install}},
        {"/api/adm/plugin/enable", adm_plugin_handler, #{action => enable}},
        {"/api/adm/plugin/disable", adm_plugin_handler, #{action => disable}},
        {"/api/adm/plugin/upgrade", adm_plugin_handler, #{action => upgrade}},
        {"/api/adm/plugin/uninstall", adm_plugin_handler, #{action => uninstall}},
        {"/api/adm/plugin/reset", adm_plugin_handler, #{action => reset}},
        {"/api/adm/plugin/force_uninstall", adm_plugin_handler, #{action => force_uninstall}},
        {"/api/adm/plugin/logs", adm_plugin_handler, #{action => logs}},
        {"/api/adm/stats/overview", adm_stats_handler, #{action => overview}},
        {"/api/adm/stats/user", adm_stats_handler, #{action => user}},
        {"/api/adm/stats/message", adm_stats_handler, #{action => message}},
        {"/api/adm/stats/group", adm_stats_handler, #{action => group}},
        {"/api/adm/stats/ranking", adm_stats_handler, #{action => ranking}},
        {"/api/adm/stats/license", adm_stats_handler, #{action => license}},
        {"/api/adm/stats/finance", adm_stats_handler, #{action => finance_summary}},
        {"/api/adm/stats/finance/report", adm_stats_handler, #{action => finance_report}},
        {"/static/admin/[...]", cowboy_static,
            {priv_dir, imboy, "static/admin", [{mimetypes, cow_mimetypes, all}]}}
    ],
    CoreRoutes = MainRoutes ++ ApiV1Routes ++ AdmRoutes,
    %% 源路由已统一在 /api 命名空间下（双路过渡已撤，无存量老客户端）。
    %% 网站白名单（/、/help、/brand、/privacy-policy、/account-deletion、/metrics、/static/*）保留根路径。
    [{Host, CoreRoutes ++ plugin_routes()}].

%% @doc Phase 2 切片 2：从 imboy_router_registry ETS 读取所有插件路由并转 cowboy 格式。
%% Phase 2 slice 2: read all plugin routes from imboy_router_registry ETS and convert.
%% registry 未启动时返回 []（dev/测试/启动早期友好，不阻塞 core 路由表构建）。
%% Returns [] when registry is not started (dev/test/early-startup safe).
%% 详见 docs/plugin/contract.md §3 / .claude/plan/industrial-plugin-architecture-roadmap.md P2-T2
plugin_routes() ->
    case erlang:whereis(imboy_router_registry) of
        undefined ->
            [];
        _ ->
            All = imboy_router_registry:all_routes(),
            [route_spec_to_cowboy(R) || {_PluginName, R} <- All]
    end.

%% @doc 转换 contract.md v1.0 route_spec map → cowboy {Path, Handler, Opts} tuple。
%% Convert v1.0 route_spec map to cowboy {Path, Handler, Opts} tuple.
%% required_feature 字段透传到 Opts，供 auth_middleware 做 feature gate 判定。
route_spec_to_cowboy(#{path := Path, handler := Handler, action := Action} = Spec) ->
    BaseOpts = #{action => Action},
    Opts =
        case maps:get(required_feature, Spec, undefined) of
            undefined -> BaseOpts;
            Feature -> BaseOpts#{required_feature => Feature}
        end,
    {binary_to_list(Path), Handler, Opts}.

%% 因为 除去 option 和 open 的路由，就是必须要 auth 的路由了
%% 所以 这里不需要定义 auth/0 方法

%% @doc 如果请求头里面有 authorization 字段，就需要认证的API
%% 列表元素必须为binary
%% auth_middleware 去除了path 最后的斜杆，所以不用以 / 结尾了
-spec option() -> [binary()].
option() ->
    [
        % 没有登录也可以提交反馈建议
        <<"/api/v1/feedback/add">>,
        <<"/api/v1/app_version/check">>,
        <<"/api/v1/app_upgrade/report">>
    ].

%% @doc 不需要认证的API
%% 列表元素必须为binary
%% auth_middleware 去除了path 最后的斜杆，所以不用以 / 结尾了
-spec open() -> [binary()].
open() ->
    [
        %% 网站白名单（保留根路径，不加 /api）
        <<"/help">>,
        <<"/brand">>,
        <<"/privacy-policy">>,
        <<"/account-deletion">>,
        <<"/healthz">>,
        <<"/metrics">>,
        %% Phase 4 T4.1：A2A 发现端点按规范匿名可达
        <<"/.well-known/agent.json">>,
        <<"/">>,

        %% 免鉴权 API（/api/v1 前缀，v0 裸路径已下架）
        % /ws 有自己的auth
        <<"/api/v1/ws">>,
        %% /api/v1/conversation/online 已移出免鉴权白名单：
        %% 它返回全站在线用户的 uid / did / pid / node —— did 是 E2EE 与设备
        %% 绑定体系的关键标识，pid/node 直接暴露集群拓扑。无 token 即可
        %% ?type=list&limit=99999 批量拉取，等于把在线用户与设备清单公开。
        %% 端点本身保留，登录用户与管理端照常可用。
        <<"/api/v1/init">>,
        <<"/api/v1/app/features">>,
        <<"/api/v1/app/manifest">>,
        <<"/api/v1/app/policy">>,
        <<"/api/v1/user/show">>,
        <<"/api/v1/refreshtoken">>,
        <<"/api/v1/passport/login">>,
        <<"/api/v1/passport/quick_login">>,
        <<"/api/v1/passport/alipay_login">>,
        <<"/api/v1/passport/alipay_authinfo">>,
        <<"/api/v1/passport/signup">>,
        <<"/api/v1/passport/getcode">>,
        <<"/api/v1/passport/findpassword">>,
        <<"/api/v1/passport/bind_mail">>,
        <<"/api/v1/passport/qr_login/create">>,
        <<"/api/v1/passport/qr_login/status">>,
        %% scan/confirm 是手机端调用，handler 强制要求 current_uid != 0
        %% （见 qr_login_handler:handle_scan/handle_confirm 的 {0, _} 分支）。
        %% 若留在此白名单内，auth_middleware_api_v1 会跳过 token 解析，
        %% current_uid 恒为 0 → scan 必返回 401「未登录」→ 客户端 _checkAuthExpired
        %% 误判为会话失效触发 quitLogin，删除本地数据库（BUG#批次78-1）。
        %% 故 scan/confirm 必须走鉴权链；create/status/cancel/subscribe 仍是
        %% Web 端未登录场景使用，保留白名单。
        <<"/api/v1/passport/qr_login/cancel">>,
        %% PR-3β: SSE 端点免登录（EventSource 在握手完成前没有 token）
        <<"/api/v1/passport/qr_login/subscribe">>,
        %% P0-C: OIDC 登录流——callback 是浏览器重定向，无 sign/did 头，必须免 902 签名门
        <<"/api/v1/auth/oidc/authorize">>,
        <<"/api/v1/auth/oidc/callback">>,
        <<"/api/v1/auth/oidc/exchange">>,

        <<"/api/v1/metrics">>,

        %% 首启初始化向导（P0-5）— 部署后首次访问必须免鉴权
        <<"/api/adm/setup/status">>,
        <<"/api/adm/setup/init">>
    ] ++ test_open_routes().

%% ===================================================================
%% 测试路由（仅非生产环境注册）
%% ===================================================================

%% @doc 判断当前是否为开发/测试环境
-spec is_dev_env() -> boolean().
is_dev_env() ->
    Env =
        case imboy_env:current() of
            %% 缺省按生产环境对待，禁用测试路由
            <<>> -> <<"pro">>;
            E -> E
        end,
    not lists:member(Env, [<<"pro">>, <<"prod">>, <<"production">>]).

%% @doc 测试路由（v1）- 仅非生产环境
-spec test_routes_v1() -> list().
test_routes_v1() ->
    case is_dev_env() of
        true ->
            [
                {"/api/v1/test/req_get", test_handler, #{action => req_get}},
                {"/api/v1/test/req_post", test_handler, #{action => req_post}},
                %% Phase 4 T4.2：agent 任务 emit 驱动 PoC（demo 触发生命周期）
                %% 仅非生产环境注册；生产返回 404（无此路由）
                {"/api/v1/agent_task/demo", agent_task_demo_handler, #{action => demo}}
            ];
        false ->
            []
    end.

%% @doc 测试端点开放路由（用于 open/0）- 仅非生产环境
-spec test_open_routes() -> [binary()].
test_open_routes() ->
    case is_dev_env() of
        true ->
            [
                <<"/api/v1/test/req_get">>,
                <<"/api/v1/test/req_post">>
            ];
        false ->
            []
    end.
