-module(imboy_router).

-export([get_routes/0]).
-export([open/0]).
-export([option/0]).
%% Phase 2 切片 2：供测试与 admin introspection
-export([plugin_routes/0]).

%% BUILD-00R：编译期 feature 门。未选中的 feature 其专属路由子句被预处理
%% 剔除，路由路径字符串不进 beam（物理裁剪），与 imboy_feature:compiled_routes
%% 运行时过滤双保险。
-include("generated/imboy_product_features.hrl").

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

            % 墨芽习字：微信小程序登录（免 Bearer，见 open/0；code 一次性换 IMBoy token）
            {"/api/v1/auth/wechat-mini/login", moya_auth_handler, #{
                action => wechat_mini_login
            }},

            % EPGZ-08 W4（INT-HUMAN-SSO-01，plan §7.2）：Human 侧签发一次性
            % OA SSO code —— 人类 JWT 门（**不进 open()**），60 秒 opaque
            % code，只存 digest，绑定 org/app/user/redirect_uri/nonce；
            % 客户 OA backend 再用 Application Credential 调 INT-14 原子消费。
            % Flutter WebView 只拿 code，不注入 IMBoy JWT / Application secret。
            {"/api/v1/oa/sso/code", enterprise_oa_sso_handler, #{action => code}},

            % 墨芽习字：微信小程序「消息推送」接收端点（免 Bearer，见 open/0）
            % GET  = 保存配置时的验签（原样回 echostr）
            % POST = 客服消息 / 进入会话等事件（兼容/安全模式 + JSON）
            {"/api/v1/wechat/mini/events", moya_wechat_msg_handler, #{}},

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
                {"/api/v1/agent/discover", ai_agent_handler, #{action => discover}},
                {"/api/v1/agent/search", ai_agent_handler, #{action => search}},
                {"/api/v1/agent/categories", ai_agent_handler, #{action => categories}},

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
                {"/api/v1/user/deletion_status", user_handler, #{action => deletion_status}},
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
                {"/api/v1/e2ee/group_history_grant", e2ee_handler, #{
                    action => group_history_grant
                }},
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
                % 群组发现（公开群搜索/浏览/分类）
                {"/api/v1/group/search", group_discovery_handler, #{action => search}},
                {"/api/v1/group/discover", group_discovery_handler, #{action => discover}},
                {"/api/v1/group/featured", group_discovery_handler, #{action => featured}},
                {"/api/v1/group/hot", group_discovery_handler, #{action => hot}},
                {"/api/v1/group/categories", group_discovery_handler, #{action => categories}},
                {"/api/v1/group/preview", group_discovery_handler, #{action => preview}},
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
                {"/api/v1/channel/qrcode", channel_handler, #{action => qrcode}},
                {"/api/v1/channel/:channel_id", channel_handler, #{action => show}},
                {"/api/v1/channel/by_custom_id/:custom_id", channel_handler, #{
                    action => by_custom_id
                }},
                {"/api/v1/channel/:channel_id/update", channel_handler, #{action => update}},
                {"/api/v1/channel/:channel_id/delete", channel_handler, #{action => delete}},
                %% GZAPP-02/G4：频道归档/恢复（仅创建者；status 0↔1，与删除 -1 区分）
                {"/api/v1/channel/:channel_id/archive", channel_handler, #{action => archive}},
                {"/api/v1/channel/:channel_id/restore", channel_handler, #{action => restore}},
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
                {"/api/v1/channels/search", channel_discovery_handler, #{action => search}},
                {"/api/v1/channels/discover", channel_discovery_handler, #{action => discover}},
                {"/api/v1/channels/featured", channel_discovery_handler, #{action => featured}},
                {"/api/v1/channels/trending", channel_discovery_handler, #{action => trending}},
                {"/api/v1/channels/categories", channel_discovery_handler, #{action => categories}},
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
                {"/api/v1/channel/:channel_id/webhook/:webhook_id/rotate", channel_webhook_handler,
                    #{action => rotate}},
                % 频道 incoming webhook 入站（token 即凭证，免 JWT/免 902 签名，
                % 放行见 auth_middleware_api_v1 的 IsChannelWebhook 前缀）
                {"/api/v1/webhook/channel/:token", channel_webhook_handler, #{
                    action => incoming
                }},

                % Bot 管理 API
                {"/api/v1/bot/register", bot_handler, #{action => register}},
                {"/api/v1/bot/get", bot_handler, #{action => get}},
                {"/api/v1/bot/update", bot_handler, #{action => update}},
                {"/api/v1/bot/disable", bot_handler, #{action => disable}},
                {"/api/v1/bot/enable", bot_handler, #{action => enable}},
                {"/api/v1/bot/list_mine", bot_handler, #{action => list_mine}},
                {"/api/v1/bot/search", bot_handler, #{action => search}},
                % Bot 发消息（api_token 认证，路由在 open/0 白名单，校验收敛于 handler）
                {"/api/v1/bot/send_message", bot_handler, #{action => send_message}},

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

                % 墨芽习字教学域 API（Step 8：登录/上下文/ACL；Step 9：作业/提交/回评；
                % 错误码契约见 include/error_code.hrl 墨芽教学域 5400-5519）
                % wechat-mini login 免 Bearer（open/0 白名单）；其余教学端点全部走 JWT
                {"/api/v1/moya/contexts", moya_context_handler, #{action => contexts}},
                {"/api/v1/moya/context/switch", moya_context_handler, #{action => switch}},
                {"/api/v1/moya/assignments", moya_assignment_handler, #{action => list}},
                {"/api/v1/moya/assignments/:id", moya_assignment_handler, #{
                    action => detail
                }},
                {"/api/v1/moya/assignments/:id/submissions", moya_assignment_handler, #{
                    action => create_submission
                }},
                {"/api/v1/moya/submissions/:id", moya_assignment_handler, #{
                    action => submission_detail
                }},
                {"/api/v1/moya/submissions/:id/withdraw", moya_assignment_handler, #{
                    action => withdraw
                }},
                {"/api/v1/moya/submissions/:id/review-workbench", moya_review_handler, #{
                    action => workbench
                }},
                %% 老师手动触发 AI 整理（此前 AI 草稿只在家长提交时入队，老师端无入口）
                {"/api/v1/moya/submissions/:id/ai-draft", moya_review_handler, #{
                    action => ai_draft
                }},
                {"/api/v1/moya/submissions/:id/review-draft", moya_review_handler, #{
                    action => save_draft
                }},
                {"/api/v1/moya/submissions/:id/reviews/publish", moya_review_handler, #{
                    action => publish
                }},
                {"/api/v1/moya/review-queue", moya_review_handler, #{action => queue}},
                {"/api/v1/moya/learners/:id/history", moya_assignment_handler, #{
                    action => history
                }},
                {"/api/v1/moya/learners/:id/history/unread-count", moya_assignment_handler, #{
                    action => history_unread_count
                }},
                %% 教学学员账号绑定（Step 16：管理侧最小动作；JWT；logic/repo 由 D 泳道
                %% 就绪，错误映射与码段决定见 STEP-16/notes.md「B 接线完成」）
                {"/api/v1/moya/learners/:id/bind", moya_learner_bind_handler, #{
                    action => bind
                }},
                {"/api/v1/moya/learners/:id/unbind", moya_learner_bind_handler, #{
                    action => unbind
                }},
                %% 教学班级学员名单（MN-ROSTER-01）：只读；binding 名 `id` 以
                %% moya_roster_handler 实际（cowboy_req:binding(id, _)）为准
                {"/api/v1/moya/classes/:id/learners", moya_roster_handler, #{
                    action => list
                }},
                %% 教学作业列表/发布（MN-TASK-01/02）：集合路径同路径双语义
                %% GET=list / POST=create，其余 method 405。cowboy 路由匹配只看
                %% path（同路径双条目的第二条恒被遮蔽），故 method 分派在 handler
                %% 侧（moya_task_handler:resolve_action/2，同 project_task_handler）。
                {"/api/v1/moya/tasks", moya_task_handler, #{action => tasks}},

                {"/api/v1/report/create", report_handler, #{action => create}},

                % R-04 处置申诉（可用性门：imboy_feature:enabled(appeal)）
                {"/api/v1/appeal/create", appeal_handler, #{action => create}},
                {"/api/v1/appeal/my", appeal_handler, #{action => my}},
                {"/api/v1/appeal/actions", appeal_handler, #{action => my_actions}}
            ] ++
            %% BUILD-00R：moment 路由按编译期宏物理裁剪（helper 见文件底部）
            moment_api_routes() ++
            [
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
                {"/api/v1/wallet/recharge/confirm", wallet_handler, #{action => recharge_confirm}},
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
                {"/api/v1/attachment/view_url", attach_handler, #{action => view_url}},
                % 附件 multipart 直传（大文件流式：form-data file 字段 → 临时文件
                % → Garage；需 JWT 认证；confirm 仍是唯一落库真源）
                {"/api/v1/attachment/upload", attach_handler, #{action => upload}},

                %% ============================================================
                %% 工作区 / 项目 / 任务（双体验 v2.5.2 WP3+WP4）
                %% T7 统一注册：按 workspace_handler / project_handler /
                %% project_task_handler 三个文件顶部路由片段清单合并（唯一一次
                %% router 修改）；全部走 /api/v1/* JWT 默认门。
                %% mine / join 固定路径必须注册在 :workspace_id 通配之前防遮蔽；
                %% branding / projects / tasks 集合路径同路径双语义由
                %% handler 按 method 分派。
                %% ============================================================
                {"/api/v1/organizations", organization_handler, #{action => collection}},
                {"/api/v1/organizations/mine", organization_handler, #{action => mine}},
                %% ============================================================
                %% Organization V1（ORG-10 一次性集中注册）：
                %% lifecycle / invitation / department / default-workspace v2 面。
                %% 术语冻结（Core Contract C18）：invite / accept / reject /
                %% revoke / restore / member / department / archive 严格区分；
                %% 既有 POST /organizations/:id/members 为 legacy direct-add
                %% adapter（C11 TRANSITION），观测归零后移除，不在 v2 重复暴露。
                %% 固定路径（deletion-preflight）必须注册在 :organization_id
                %% 通配之前防遮蔽（同 channel/qrcode 先例）。
                %% ============================================================
                {"/api/v1/organizations/deletion-preflight", organization_handler, #{
                    action => deletion_preflight
                }},
                {"/api/v1/organizations/invitations/mine", organization_api_handler, #{
                    action => invitation_mine
                }},
                {"/api/v1/organizations/:organization_id", organization_handler, #{
                    action => detail
                }},
                {"/api/v1/organizations/:organization_id/members", organization_member_handler, #{
                    action => collection
                }},
                {"/api/v1/organizations/:organization_id/members/transfer_owner",
                    organization_member_handler, #{action => owner_transfer}},
                {"/api/v1/organizations/:organization_id/members/:user_id/role",
                    organization_member_handler, #{action => role}},
                %% 成员生命周期命令（EB-D07/EB-08）：suspend/restore/offboard
                %% （offboard 复用既有 remove 终态语义，两步离场 S3）
                {"/api/v1/organizations/:organization_id/members/:user_id/suspend",
                    organization_member_handler, #{action => member_suspend}},
                {"/api/v1/organizations/:organization_id/members/:user_id/restore",
                    organization_member_handler, #{action => member_restore}},
                {"/api/v1/organizations/:organization_id/members/:user_id/offboard",
                    organization_member_handler, #{action => member_offboard}},
                {"/api/v1/organizations/:organization_id/members/:user_id",
                    organization_member_handler, #{action => member}},
                %% —— ORG-10 集中注册（续）：org 域内 v2 面 ——
                %% lifecycle（C16：archive/restore 幂等 command；deletion-preflight
                %% 见上方固定路径块，GET）
                {"/api/v1/organizations/:organization_id/archive", organization_handler, #{
                    action => archive
                }},
                {"/api/v1/organizations/:organization_id/restore", organization_handler, #{
                    action => restore
                }},
                %% invitation（C11：invite / accept / reject / revoke / list 是
                %% 不同 command；同路径 GET=list（org 治理面）/ POST=create）
                {"/api/v1/organizations/:organization_id/invitations", organization_api_handler, #{
                    action => invitation_collection
                }},
                {"/api/v1/organizations/:organization_id/invitations/accept",
                    organization_api_handler, #{
                        action => invitation_accept
                    }},
                {"/api/v1/organizations/:organization_id/invitations/:invitation_id/reject",
                    organization_api_handler, #{
                        action => invitation_reject
                    }},
                {"/api/v1/organizations/:organization_id/invitations/:invitation_id/revoke",
                    organization_api_handler, #{
                        action => invitation_revoke
                    }},
                %% department（C10：树形目录 + member 多归属 + 局部 admin；
                %% 同路径 GET=list / POST=create）
                {"/api/v1/organizations/:organization_id/departments", organization_api_handler, #{
                    action => department_collection
                }},
                {"/api/v1/organizations/:organization_id/departments/:department_id",
                    organization_api_handler, #{
                        action => department_item
                    }},
                {"/api/v1/organizations/:organization_id/departments/:department_id/move",
                    organization_api_handler, #{
                        action => department_move
                    }},
                {"/api/v1/organizations/:organization_id/departments/:department_id/archive",
                    organization_api_handler, #{
                        action => department_archive
                    }},
                {"/api/v1/organizations/:organization_id/departments/:department_id/members",
                    organization_api_handler, #{
                        action => department_member_collection
                    }},
                {"/api/v1/organizations/:organization_id/departments/:department_id/members/:user_id",
                    organization_api_handler, #{
                        action => department_member_item
                    }},
                {"/api/v1/organizations/:organization_id/departments/:department_id/members/:user_id/admin",
                    organization_api_handler, #{
                        action => department_member_admin
                    }},
                %% default workspace（C05：显式 default relation，GET 读 /
                %% POST set / DELETE clear 同路径按 method 分派）
                {"/api/v1/organizations/:organization_id/default-workspace",
                    organization_api_handler, #{
                        action => default_workspace
                    }},
                %% invite code（GZAPP-01：org 可复用加入凭证，8 位 A-Z2-9、
                %% 7 天有效、一码多人、owner/admin 可撤销；join 统一走
                %% organization_join_orchestrator 编排——org member → 默认
                %% WS → 全员群 → 公告频道；GET/POST/DELETE 同路径分派，
                %% join 子路径须注册在前缀匹配安全区内）
                {"/api/v1/organizations/:organization_id/invite_code", organization_api_handler, #{
                    action => invite_code
                }},
                {"/api/v1/organizations/:organization_id/invite_code/join",
                    organization_api_handler, #{
                        action => invite_code_join
                    }},
                {"/api/v1/workspaces", workspace_handler, #{action => create}},
                {"/api/v1/workspaces/mine", workspace_handler, #{action => mine}},
                {"/api/v1/workspaces/join", workspace_handler, #{action => join}},
                {"/api/v1/workspaces/:workspace_id", workspace_handler, #{action => show}},
                {"/api/v1/workspaces/:workspace_id/update", workspace_handler, #{
                    action => update
                }},
                {"/api/v1/workspaces/:workspace_id/branding", workspace_handler, #{
                    action => branding
                }},
                {"/api/v1/workspaces/:workspace_id/overview", workspace_handler, #{
                    action => overview
                }},
                {"/api/v1/workspaces/:workspace_id/channels", workspace_handler, #{
                    action => channel_list
                }},
                {"/api/v1/workspaces/:workspace_id/groups", workspace_handler, #{
                    action => group_list
                }},
                {"/api/v1/workspaces/:workspace_id/members", workspace_handler, #{
                    action => member_list
                }},
                {"/api/v1/workspaces/:workspace_id/members/invite", workspace_handler, #{
                    action => member_invite
                }},
                {"/api/v1/workspaces/:workspace_id/members/remove", workspace_handler, #{
                    action => member_remove
                }},
                {"/api/v1/workspaces/:workspace_id/members/role", workspace_handler, #{
                    action => member_role
                }},
                {"/api/v1/workspaces/:workspace_id/members/transfer_owner", workspace_handler, #{
                    action => owner_transfer
                }},
                {"/api/v1/workspaces/:workspace_id/invite_code", workspace_handler, #{
                    action => invite_code
                }},
                {"/api/v1/workspaces/:workspace_id/invite_code/revoke", workspace_handler, #{
                    action => invite_code_revoke
                }},
                {"/api/v1/workspaces/:workspace_id/archive", workspace_handler, #{
                    action => archive
                }},
                {"/api/v1/workspaces/:workspace_id/restore", workspace_handler, #{
                    action => restore
                }},
                {"/api/v1/workspaces/:workspace_id/projects", project_handler, #{
                    action => projects
                }},
                {"/api/v1/projects/:project_id", project_handler, #{action => show}},
                {"/api/v1/projects/:project_id/update", project_handler, #{action => update}},
                {"/api/v1/projects/:project_id/status", project_handler, #{
                    action => update_status
                }},
                {"/api/v1/projects/:project_id/tasks", project_task_handler, #{
                    action => tasks
                }},
                {"/api/v1/tasks/:task_id", project_task_handler, #{action => show}},
                {"/api/v1/tasks/:task_id/update", project_task_handler, #{action => update}},
                {"/api/v1/tasks/:task_id/status", project_task_handler, #{
                    action => update_status
                }},

                %% ============================================================
                %% Channel-first-class W2（ZC-05 整合：按 project_member_handler /
                %% project_milestone_handler / project_channel_handler 三个文件
                %% 顶部路由片段清单合并，唯一一次 router 修改）；全部走
                %% /api/v1/* JWT 默认门。members/milestones/channels 集合路径
                %% 同路径双语义由 handler 按 method 分派。
                %% ============================================================
                {"/api/v1/projects/:project_id/members", project_member_handler, #{
                    action => members
                }},
                {"/api/v1/projects/:project_id/members/invite", project_member_handler, #{
                    action => invite
                }},
                {"/api/v1/projects/:project_id/members/remove", project_member_handler, #{
                    action => remove
                }},
                {"/api/v1/projects/:project_id/members/transfer_owner", project_member_handler, #{
                    action => transfer_owner
                }},
                {"/api/v1/projects/:project_id/milestones", project_milestone_handler, #{
                    action => milestones
                }},
                {"/api/v1/milestones/:milestone_id/update", project_milestone_handler, #{
                    action => update
                }},
                {"/api/v1/milestones/:milestone_id/reach", project_milestone_handler, #{
                    action => reach
                }},
                {"/api/v1/projects/:project_id/channels", project_channel_handler, #{
                    action => channels
                }},
                {"/api/v1/projects/:project_id/channels/:channel_id/unlink",
                    project_channel_handler, #{
                        action => unlink
                    }},
                {"/api/v1/projects/:project_id/links/update", project_channel_handler, #{
                    action => update_links
                }},
                {"/api/v1/projects/:project_id/aggregations/pinned", project_channel_handler, #{
                    action => pinned
                }},
                {"/api/v1/projects/:project_id/aggregations/resources", project_channel_handler, #{
                    action => resources
                }},
                {"/api/v1/projects/:project_id/aggregations/activity", project_channel_handler, #{
                    action => activity
                }},
                {"/api/v1/projects/:project_id/aggregations/related_posts", project_channel_handler,
                    #{
                        action => related_posts
                    }}
            ] ++
            %% EB-10：企业租户面 16 条（编译期物理裁剪；
            %% 未选中 enterprise_business 时 helper 体被预处理剔除，helper 见文件底部）
            enterprise_tenant_routes() ++
            %% CS-02/CSB-03：客服租户面 16 条 + widget 接入面 8 条（同款编译期
            %% 物理裁剪，helper 见文件底部；widget 动作由 cs_widget_handler 承接）
            customer_service_tenant_routes(),

    %% ---------------------------------------------------------------------------
    %% EB-10 / BUILD-00R：enterprise_business 路由段的**编译期物理裁剪** helper

    % Admin routes (原 imadm)
    AdmRoutes =
        [
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
            % Product Experience 安装级配置只读（双体验 v2.5.2 WP7/T11；无运行时写接口）
            {"/api/adm/admin/config/product-experience", adm_admin_handler, #{
                action => config_product_experience
            }},
            {"/api/adm/admin/config/policy/bootstrap", adm_admin_handler, #{
                action => config_policy_bootstrap
            }},
            {"/api/adm/admin/config/policy/meta", adm_admin_handler, #{
                action => config_policy_meta
            }},
            {"/api/adm/admin/config/policy/preview", adm_admin_handler, #{
                action => config_policy_preview
            }},
            {"/api/adm/admin/config/policy/saved", adm_admin_handler, #{
                action => config_policy_saved
            }},
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
            {"/api/adm/admin/muted_users/unmute", adm_admin_handler, #{
                action => muted_users_unmute
            }},
            {"/api/adm/admin/muted_users/unmute_batch", adm_admin_handler, #{
                action => muted_users_unmute_batch
            }},
            % 推送 Token 管理 API
            {"/api/adm/admin/push_token/list", adm_admin_handler, #{action => push_token_list}},
            % 合规密钥管理 API
            {"/api/adm/admin/compliance_key/list", adm_admin_handler, #{
                action => compliance_key_list
            }},
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
            {"/api/adm/mcp/clients/create", adm_mcp_handler, #{action => create}},
            {"/api/adm/mcp/clients/approve", adm_mcp_handler, #{action => approve}},
            {"/api/adm/mcp/clients/reject", adm_mcp_handler, #{action => reject}},
            {"/api/adm/mcp/clients/revoke", adm_mcp_handler, #{action => revoke}},
            {"/api/adm/mcp/clients/grants", adm_mcp_handler, #{action => grants}},
            {"/api/adm/mcp/clients/grants/set", adm_mcp_handler, #{action => set_grant}},
            {"/api/adm/mcp/audit", adm_mcp_handler, #{action => audit}},
            {"/api/adm/bot/deliveries/replay", adm_bot_delivery_handler, #{action => replay}},
            {"/api/adm/bot/deliveries", adm_bot_delivery_handler, #{action => list}},
            {"/api/adm/app_ddl/index", adm_app_ddl_handler, #{action => index}},
            {"/api/adm/app_ddl/save", adm_app_ddl_handler, #{action => save}},
            {"/api/adm/app_ddl/delete", adm_app_ddl_handler, #{action => delete}},
            {"/api/adm/app_version/index", adm_app_version_handler, #{action => index}},
            {"/api/adm/app_version/save", adm_app_version_handler, #{action => save}},
            {"/api/adm/app_version/delete", adm_app_version_handler, #{action => delete}},
            {"/api/adm/app_version/version_stats", adm_app_version_handler, #{
                action => version_stats
            }},
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
            % Workspace/Project 运营管理 API（双体验 v2.5.2 WP7/T11b）
            % 鉴权：adm_acl workspaces:read / workspaces:update（fail-closed 403）
            {"/api/adm/workspace/list", adm_workspace_handler, #{action => list}},
            {"/api/adm/workspace/detail", adm_workspace_handler, #{action => detail}},
            {"/api/adm/workspace/members", adm_workspace_handler, #{action => members}},
            {"/api/adm/workspace/archive", adm_workspace_handler, #{action => archive}},
            {"/api/adm/workspace/restore", adm_workspace_handler, #{action => restore}},
            % Platform Admin Organization 治理面（ORG-ADM-ORG-API；ORG-14 选 A）
            % 读=organizations:read；写=organizations:write（fail-closed 403）。
            % TSID 一律 string 传输；写端点全部审计 adm_operation_log。
            {"/api/adm/organizations", adm_organization_handler, #{action => list}},
            {"/api/adm/organizations/:organization_id", adm_organization_handler, #{
                action => show
            }},
            {"/api/adm/organizations/:organization_id/members", adm_organization_handler, #{
                action => members
            }},
            {"/api/adm/organizations/:organization_id/invitations", adm_organization_handler, #{
                action => invitations
            }},
            {"/api/adm/organizations/:organization_id/departments", adm_organization_handler, #{
                action => departments
            }},
            {"/api/adm/organizations/:organization_id/workspaces", adm_organization_handler, #{
                action => workspaces
            }},
            {"/api/adm/organizations/:organization_id/archive", adm_organization_handler, #{
                action => org_archive
            }},
            {"/api/adm/organizations/:organization_id/restore", adm_organization_handler, #{
                action => org_restore
            }},
            {"/api/adm/organizations/:organization_id/owner-transfer", adm_organization_handler, #{
                action => owner_transfer
            }},
            % 待激活 Owner 治理面（GZAPP-06 / D11-D13）
            % 读=organizations:read；写=organizations:write（fail-closed 403）。
            % 手机号出站一律脱敏；审计 adm_operation_log 只落 masked 形态。
            {"/api/adm/organizations/:organization_id/owner-activation", adm_owner_activation_handler, #{
                action => owner_activation_show
            }},
            {"/api/adm/organizations/:organization_id/owner-activation/resend", adm_owner_activation_handler, #{
                action => owner_activation_resend
            }},
            {"/api/adm/organizations/:organization_id/owner-activation/reactivate", adm_owner_activation_handler, #{
                action => owner_activation_reactivate
            }},
            {"/api/adm/organizations/:organization_id/owner-activation/consume", adm_owner_activation_handler, #{
                action => owner_activation_consume
            }},
            {"/api/adm/organizations/:organization_id/owner-transfer-by-phone", adm_owner_activation_handler, #{
                action => owner_transfer_by_phone
            }},
            {"/api/adm/organizations/:organization_id/members/:user_id/suspend",
                adm_organization_handler, #{action => member_suspend}},
            {"/api/adm/organizations/:organization_id/members/:user_id/restore",
                adm_organization_handler, #{action => member_restore}},
            {"/api/adm/organizations/:organization_id/members/:user_id/remove",
                adm_organization_handler, #{action => member_remove}},
            {"/api/adm/organizations/:organization_id/invitations/:invitation_id/cancel",
                adm_organization_handler, #{action => invitation_cancel}},
            {"/api/adm/organizations/:organization_id/departments/:department_id/rename",
                adm_organization_handler, #{action => department_rename}},
            {"/api/adm/organizations/:organization_id/departments/:department_id/move",
                adm_organization_handler, #{action => department_move}},
            {"/api/adm/organizations/:organization_id/departments/:department_id/archive",
                adm_organization_handler, #{action => department_archive}},
            {"/api/adm/project/list", adm_workspace_handler, #{action => project_list}},
            {"/api/adm/project/detail", adm_workspace_handler, #{action => project_detail}},
            %% Channel-first-class W2 治理只读面（ZC-05）
            {"/api/adm/project/members", adm_workspace_handler, #{action => project_members}},
            {"/api/adm/project/milestones", adm_workspace_handler, #{action => project_milestones}},
            {"/api/adm/project/channels", adm_workspace_handler, #{action => project_channels}},
            {"/api/adm/project/aggregations", adm_workspace_handler, #{
                action => project_aggregations
            }},
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
            {"/api/adm/group/category/delete", adm_group_content_handler, #{
                action => category_delete
            }},
            {"/api/adm/group/file/list", adm_group_content_handler, #{action => file_list}},
            {"/api/adm/group/file/detail", adm_group_content_handler, #{action => file_detail}},
            {"/api/adm/group/file/delete", adm_group_content_handler, #{action => file_delete}},
            {"/api/adm/group/album/list", adm_group_content_handler, #{action => album_list}},
            {"/api/adm/group/album/detail", adm_group_content_handler, #{action => album_detail}},
            {"/api/adm/group/album/delete", adm_group_content_handler, #{action => album_delete}},
            {"/api/adm/group/schedule/list", adm_group_schedule_handler, #{action => schedule_list}},
            {"/api/adm/group/schedule/detail", adm_group_schedule_handler, #{
                action => schedule_detail
            }},
            {"/api/adm/group/schedule/cancel", adm_group_schedule_handler, #{
                action => schedule_cancel
            }},
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
            % Bot 管理处置 API（平台侧，无属主校验）
            {"/api/adm/bot/list", adm_bot_handler, #{action => list}},
            {"/api/adm/bot/detail", adm_bot_handler, #{action => detail}},
            {"/api/adm/bot/disable", adm_bot_handler, #{action => disable}},
            {"/api/adm/bot/enable", adm_bot_handler, #{action => enable}},
            %% 固定段路由置于 :channel_id 通配之前，避免 order 被当作 channel_id
            {"/api/adm/channel/order/refund", adm_channel_handler, #{action => refund_order}},
            {"/api/adm/channel/detail/:channel_id", adm_channel_handler, #{action => detail}},
            {"/api/adm/channel/:channel_id/messages", adm_channel_handler, #{action => messages}},
            {"/api/adm/channel/:channel_id/subscribers", adm_channel_handler, #{
                action => subscribers
            }},
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
            {"/api/adm/channel/:channel_id/invitations", adm_channel_handler, #{
                action => invitations
            }},
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
            {"/api/adm/finance/billing/invoices", adm_finance_handler, #{
                action => billing_invoices
            }},
            {"/api/adm/finance/withdrawals", adm_finance_handler, #{action => withdrawals}},
            {"/api/adm/finance/withdrawals/complete", adm_finance_handler, #{
                action => withdrawal_complete
            }},
            {"/api/adm/finance/withdrawals/reject", adm_finance_handler, #{
                action => withdrawal_reject
            }}
            % Moment 与举报治理 API（BUILD-00R：编译期物理裁剪，helper 见文件底部）
        ] ++ moment_admin_routes() ++
            [
                {"/api/adm/report/create", adm_report_handler, #{action => create}},
                {"/api/adm/report/list", adm_report_handler, #{action => list}},
                {"/api/adm/report/detail", adm_report_handler, #{action => detail}},
                {"/api/adm/report/resolve", adm_report_handler, #{action => resolve}},
                {"/api/adm/report/batch_resolve", adm_report_handler, #{action => batch_resolve}},
                {"/api/adm/group/report/list", adm_report_handler, #{action => group_list}},
                {"/api/adm/group/report/resolve", adm_report_handler, #{action => group_resolve}},
                {"/api/adm/group/report/batch_resolve", adm_report_handler, #{
                    action => group_batch_resolve
                }},
                {"/api/adm/channel/report/list", adm_report_handler, #{action => channel_list}},
                {"/api/adm/channel/report/resolve", adm_report_handler, #{
                    action => channel_resolve
                }},
                {"/api/adm/channel/report/batch_resolve", adm_report_handler, #{
                    action => channel_batch_resolve
                }},
                {"/api/adm/user/report/list", adm_report_handler, #{action => user_list}},
                {"/api/adm/user/report/resolve", adm_report_handler, #{action => user_resolve}},
                {"/api/adm/user/report/batch_resolve", adm_report_handler, #{
                    action => user_batch_resolve
                }},
                %% R-02：处置动作（case = report_ticket 行；audit = moderation_action 表）
                {"/api/adm/report_action/execute", adm_report_action_handler, #{action => execute}},
                {"/api/adm/report_action/reverse", adm_report_action_handler, #{action => reverse}},
                {"/api/adm/report_action/list", adm_report_action_handler, #{action => list}},
                % 统计 API
                {"/api/adm/announcement/index", adm_announcement_handler, #{action => index}},
                {"/api/adm/announcement/create", adm_announcement_handler, #{action => create}},
                {"/api/adm/announcement/update", adm_announcement_handler, #{action => update}},
                {"/api/adm/announcement/delete", adm_announcement_handler, #{action => delete}},
                {"/api/adm/announcement/publish", adm_announcement_handler, #{action => publish}},
                {"/api/adm/announcement/unpublish", adm_announcement_handler, #{
                    action => unpublish
                }},
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
                {"/api/adm/moderation/review-queue", adm_moderation_handler, #{
                    action => review_queue
                }},
                % R-04 处置申诉复审（独立复审约束在 logic 层：reviewer≠原执行者）
                {"/api/adm/appeal/list", adm_appeal_handler, #{action => list}},
                {"/api/adm/appeal/review", adm_appeal_handler, #{action => review}},
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
                {"/api/adm/plugin/force_uninstall", adm_plugin_handler, #{
                    action => force_uninstall
                }},
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
            ] ++
            %% EB-10：企业平台运营面 10 条（同上：编译期物理裁剪，helper 见文件底部）
            enterprise_platform_routes() ++
            %% CS-02：客服平台运营面 6 条（同款编译期物理裁剪，helper 见文件底部）
            customer_service_platform_routes() ++
            %% FULL-08：Admin 企业应用治理面 A-01..A-14（/api/adm/enterprise/*）。
            %% 与 enterprise_platform_routes()（/api/adm/enterprise-business/*）是
            %% **两个前缀不相交的 surface**：本族只服务 Application/Credential/
            %% Grant/Webhook/审计治理，**不经 feature 门**——理由与
            %% enterprise_internal_routes() 一致（企业集成平台是核心交付面，
            %% 新增 feature key 会改 IMBOY_PRODUCT_FEATURE_MANIFEST_HASH）。
            %% 鉴权：/api/adm 前缀的 adm_auth_middleware（Admin Cookie 会话）+
            %% handler 内按方法强制 adm_acl 权限位；与 OA Credential 的
            %% /api/internal/v1/* 链路不可互换。
            enterprise_application_governance_routes(),
    %% ---------------------------------------------------------------------------
    %% EB-10 / BUILD-00R：enterprise_business 路由段的**编译期物理裁剪** helper

    CompiledApiRoutes = customer_service_wire(
        enterprise_wire(imboy_feature:compiled_routes(api, ApiV1Routes))
    ),
    CompiledAdmRoutes = customer_service_wire(
        enterprise_wire(imboy_feature:compiled_routes(admin, AdmRoutes))
    ),
    CompiledPluginRoutes = imboy_feature:compiled_routes(api, plugin_routes()),
    %% EPGZ-08 W4：企业 internal 面（/api/internal/v1/*，Application Credential
    %% 认证）挂进 CoreRoutes。**不经 feature 门**：它是 OA 集成面的冻结白名单
    %% （A0 control/internal-api-manifest.yaml，14 条），与 enterprise_business
    %% 的租户/运营面是两套 surface；新增 feature key 会改动
    %% IMBOY_PRODUCT_FEATURE_MANIFEST_HASH，超出 EPGZ-08「只做集成」范围。
    %% 匿名可达性由 auth_middleware 的前缀分支 + enterprise_internal_middleware
    %% 认证链保证（open()/option() 均不含该前缀）。
    CoreRoutes =
        MainRoutes ++ CompiledApiRoutes ++ CompiledAdmRoutes ++ enterprise_internal_routes(),
    %% 源路由已统一在 /api 命名空间下（双路过渡已撤，无存量老客户端）。
    %% 网站白名单（/、/help、/brand、/privacy-policy、/account-deletion、/metrics、/static/*）保留根路径。
    [{Host, CoreRoutes ++ CompiledPluginRoutes}].

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
        %% 墨芽习字：微信小程序登录——wx.login 一次性 code 换 token，握手前无
        %% sign/did 头；code 本身即凭证（AUTH-01：code 消费后重放必失败）
        <<"/api/v1/auth/wechat-mini/login">>,

        %% 墨芽习字：微信「消息推送」——微信侧不带任何 IMBoy 凭证，唯一凭证是
        %% URL 上的签名（GET 校验 signature / POST 校验 msg_signature，均由
        %% moya_wechat_msg_logic 逐条 fail-closed 裁决）。同理不带 sign/did 头，
        %% 必须免 902 签名门，否则微信保存配置时恒 403、后台报「Token 验证失败」。
        <<"/api/v1/wechat/mini/events">>,

        %% Bot 发消息：Bot 服务器无用户 JWT，凭证是 api_token
        %% （Authorization: Bearer <api_token>，校验在 bot_handler:authenticate/1，
        %% 并有 agent_rate_limiter + has_exchange 防骚扰闸门）
        <<"/api/v1/bot/send_message">>,

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

%% ===================================================================
%% BUILD-00R：moment 编译期物理裁剪 helper
%% 未选中 moment 时预处理剔除整个函数体，路由路径字符串不进 beam；
%% 与 imboy_feature:compiled_routes 的运行时过滤构成双保险。
%% ===================================================================
-ifdef(IMBOY_FEATURE_MOMENT).
-spec moment_api_routes() -> list().
moment_api_routes() ->
    [
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
        {"/api/v1/moment/:moment_id/report", moment_handler, #{action => report}}
    ].

-spec moment_admin_routes() -> list().
moment_admin_routes() ->
    [
        {"/api/adm/moment/list", adm_moment_handler, #{action => list}},
        {"/api/adm/moment/detail/:moment_id", adm_moment_handler, #{action => detail}},
        {"/api/adm/moment/delete", adm_moment_handler, #{action => delete}},
        {"/api/adm/moment/report/list", adm_moment_handler, #{action => report_list}},
        {"/api/adm/moment/report/resolve", adm_moment_handler, #{action => report_resolve}},
        {"/api/adm/moment/report/batch_resolve", adm_moment_handler, #{
            action => report_batch_resolve
        }}
    ].
-else.
-spec moment_api_routes() -> list().
moment_api_routes() ->
    [].

-spec moment_admin_routes() -> list().
moment_admin_routes() ->
    [].
-endif.

%% ===================================================================
%% EB-09：企业面 route 装配
%% ===================================================================

%% @doc 给企业面的路由注入三个**面级**不变量键。
%%
%% 为什么集中注入而不是逐路由手写：`surface`（由 handler 决定）、`feature`
%% （恒为 enterprise_business）与 `auth_facts`（本面的只读事实装配：租户面
%% `eb_pg_auth_facts`、平台面 `eb_platform_auth_facts`）在同一个面上**恒等**，
%% 30 条路由逐条手写只会制造漂移面。逐条**可变**的键（path / handler / action /
%% auth_context / required_*）仍全部字面登记在路由表里，可直接机械核对
%% （EB-09-A01 的核对套件读的是 `imboy_router:get_routes/0` 的运行时结果）。
-spec enterprise_wire(list()) -> list().
enterprise_wire(Routes) ->
    [enterprise_wire_route(Route) || Route <- Routes].

enterprise_wire_route({Path, eb_tenant_handler, Opts}) when is_map(Opts) ->
    {Path, eb_tenant_handler, Opts#{
        surface => tenant,
        feature => enterprise_business,
        auth_facts => eb_pg_auth_facts
    }};
enterprise_wire_route({Path, eb_platform_handler, Opts}) when is_map(Opts) ->
    {Path, eb_platform_handler, Opts#{
        surface => platform,
        feature => enterprise_business,
        auth_facts => eb_platform_auth_facts
    }};
enterprise_wire_route(Route) ->
    Route.

%% @doc 给客服面的路由注入三个**面级**不变量键（CS-02，模式照 enterprise_wire/1）。
%%
%% `surface`（由 handler 决定）、`feature`（恒为 customer_service）与 `auth_facts`
%% （本面的只读事实装配：租户成员事实 `eb_pg_auth_facts`、平台事实
%% `eb_platform_auth_facts`；访客/门店凭证类不用事实，注入无害）在同一个面上
%% 恒等，21 条路由逐条手写只会制造漂移面。逐条**可变**的键（path / handler /
%% action / auth_context / required_*）仍全部字面登记在路由表里。
-spec customer_service_wire(list()) -> list().
customer_service_wire(Routes) ->
    [customer_service_wire_route(Route) || Route <- Routes].

customer_service_wire_route({Path, cs_tenant_handler, Opts}) when is_map(Opts) ->
    {Path, cs_tenant_handler, Opts#{
        surface => tenant,
        feature => customer_service,
        auth_facts => eb_pg_auth_facts
    }};
customer_service_wire_route({Path, cs_widget_handler, Opts}) when is_map(Opts) ->
    {Path, cs_widget_handler, Opts#{
        surface => widget,
        feature => customer_service,
        %% widget 面的凭证校验在 application 用例内（bootstrap 令牌 digest），
        %% 不用事实装配；注入与租户面同键保持面级不变量一致（无害）。
        auth_facts => eb_pg_auth_facts
    }};
%% BE-W01 A05：动态 frame HTML 与 cs_widget_handler 同属 widget 面——面级
%% 不变量键（surface/feature/auth_facts）逐字同款，防面间漂移。
customer_service_wire_route({Path, cs_widget_frame_handler, Opts}) when is_map(Opts) ->
    {Path, cs_widget_frame_handler, Opts#{
        surface => widget,
        feature => customer_service,
        auth_facts => eb_pg_auth_facts
    }};
customer_service_wire_route({Path, cs_platform_handler, Opts}) when is_map(Opts) ->
    {Path, cs_platform_handler, Opts#{
        surface => platform,
        feature => customer_service,
        auth_facts => eb_platform_auth_facts
    }};
customer_service_wire_route(Route) ->
    Route.

%% ===================================================================
%% EB-10 / BUILD-00R：enterprise_business 路由段的**编译期物理裁剪** helper
%% ===================================================================
%% 为什么另开 helper 而不是把 16+10 条直接写在 ApiV1Routes/AdmRoutes 里：
%% 未选中 enterprise_business 时，`-ifdef(IMBOY_FEATURE_ENTERPRISE_BUSINESS)`
%% 让整个函数体在预处理阶段被剔除 —— 企业路由的**路径字符串不进
%% imboy_router.beam**（物理裁剪，不是运行时开关），与
%% imboy_feature:route_feature/3 + compiled_routes/2 的运行时过滤构成双保险。
%% 依据：plan §8 EB-10、docs/adr/0007-feature-slice-architecture.md、
%% docs/architecture/feature-slice-rules.md；与 moment_api_routes/0 同款。
%% 注意：路由条目本身（path/handler/action/auth_context/required_*）与 EB-09
%% 逐字一致，仅位置与缩进变化（契约核对仍读 imboy_router:get_routes/0）。
%% 契约侧：`scripts/contract_gate.py:extract_routes/1` 用 SCOPE_MARKERS 做文本
%% 窗口切片，窗口不含本段 —— 故同批加了 2 条**精确 enterprise 行**把本段并入
%% tenant/platform 两面，否则 `.contract/api_contract.json` 会丢掉这 26 条路由。
%% ===================================================================
-ifdef(IMBOY_FEATURE_ENTERPRISE_BUSINESS).

-spec enterprise_tenant_routes() -> list().
enterprise_tenant_routes() ->
    [
        %% FND-1（RULING-2026-09-15 §五）：业务身份的创建/列举/绑定是**治理动作**，
        %% 走 governance auth（active owner/admin），不得要求调用者预先持有 sales
        %% assignment —— 旧配置（member+sales+org.manage）把「建身份」的资格挂在
        %% 「已有身份」上，空 Org 无法自举（owner 也过不了 identity_assignment_missing）。
        {"/api/v1/enterprise/organizations/:org_id/business-identities", eb_tenant_handler, #{
            action => business_identities,
            auth_context => enterprise_owner_admin,
            required_governance => [<<"owner">>, <<"admin">>]
        }},
        {"/api/v1/enterprise/organizations/:org_id/business-identities/:id/assign",
            eb_tenant_handler, #{
                action => assign_identity,
                auth_context => enterprise_owner_admin,
                required_governance => [<<"owner">>, <<"admin">>]
            }},
        {"/api/v1/enterprise/organizations/:org_id/contacts", eb_tenant_handler, #{
            action => contacts,
            auth_context => enterprise_member,
            required_function => <<"sales">>,
            required_permission => <<"contact.read">>
        }},
        {"/api/v1/enterprise/organizations/:org_id/contacts/:id", eb_tenant_handler, #{
            action => contact_detail,
            auth_context => enterprise_member,
            required_function => <<"sales">>,
            required_permission => <<"contact.read">>
        }},
        {"/api/v1/enterprise/organizations/:org_id/contacts/:id/notes", eb_tenant_handler, #{
            action => append_note,
            auth_context => enterprise_member,
            required_function => <<"sales">>,
            required_permission => <<"note.write">>
        }},
        {"/api/v1/enterprise/organizations/:org_id/conversations", eb_tenant_handler, #{
            action => open_conversation,
            auth_context => enterprise_member,
            required_function => <<"sales">>,
            required_permission => <<"conversation.write">>
        }},
        {"/api/v1/enterprise/organizations/:org_id/conversations/:id/messages", eb_tenant_handler,
            #{
                action => conversation_messages,
                auth_context => enterprise_member,
                %% CSX-01：消息真源面职能白名单（与 eb_enterprise_actions 同步）——
                %% 客服坐席（customer_service assignment）经此面回复会话消息。
                required_function => [<<"sales">>, <<"customer_service">>],
                required_permission => <<"conversation.read">>
            }},
        % ACK 是 delivery-only 动作（eb_enterprise_actions:ack_delivery 的
        % delivery_only=true）：响应不含删除/归档语义（EB-09-A06）。
        % BE-S01a（勘察缺口 J05/J07）：ACK/presign/confirm/content 四动作的
        % 职能白名单扩为 sales|customer_service——此前 sales-only 把坐席挡死
        % （403）；坐席的会话经办 ACL 在 application 层裁决（附件三动作
        % eb_asset_scope 全员门 + ACK 的 seat 门），sales 行为不变。
        {"/api/v1/enterprise/organizations/:org_id/conversations/:id/messages/:message_id/ack",
            eb_tenant_handler, #{
                action => ack_delivery,
                auth_context => enterprise_member,
                required_function => [<<"sales">>, <<"customer_service">>],
                required_permission => <<"message.write">>
            }},
        {"/api/v1/enterprise/organizations/:org_id/assets/presign", eb_tenant_handler, #{
            action => presign,
            auth_context => enterprise_member,
            required_function => [<<"sales">>, <<"customer_service">>],
            required_permission => <<"asset.write">>
        }},
        {"/api/v1/enterprise/organizations/:org_id/assets/confirm", eb_tenant_handler, #{
            action => confirm_asset,
            auth_context => enterprise_member,
            required_function => [<<"sales">>, <<"customer_service">>],
            required_permission => <<"asset.write">>
        }},
        % content 经 facade 取流并流式返回，不签发任何 URL（EB-09-A05）
        {"/api/v1/enterprise/organizations/:org_id/assets/:id/content", eb_tenant_handler, #{
            action => asset_content,
            auth_context => enterprise_member,
            required_function => [<<"sales">>, <<"customer_service">>],
            required_permission => <<"asset.read">>
        }},
        {"/api/v1/enterprise/organizations/:org_id/members/:uid/suspend", eb_tenant_handler, #{
            action => suspend_member,
            auth_context => enterprise_owner_admin,
            required_governance => [<<"owner">>, <<"admin">>]
        }},
        {"/api/v1/enterprise/organizations/:org_id/offboarding", eb_tenant_handler, #{
            action => offboarding_open,
            auth_context => enterprise_owner_admin,
            required_governance => [<<"owner">>, <<"admin">>]
        }},
        {"/api/v1/enterprise/organizations/:org_id/offboarding/:id/execute", eb_tenant_handler, #{
            action => offboarding_execute,
            auth_context => enterprise_owner_admin,
            required_governance => [<<"owner">>, <<"admin">>]
        }},
        {"/api/v1/enterprise/organizations/:org_id/offboarding/:id/verify", eb_tenant_handler, #{
            action => offboarding_verify,
            auth_context => enterprise_owner_admin,
            required_governance => [<<"owner">>, <<"admin">>]
        }},
        {"/api/v1/enterprise/organizations/:org_id/offboarding/:id/finalize", eb_tenant_handler, #{
            action => offboarding_finalize,
            auth_context => enterprise_owner_admin,
            required_governance => [<<"owner">>, <<"admin">>]
        }},
        %% offboarding 读取面（closure §8：查询交接 case）。固定段 `cases` 与既有
        %% `:id` 动作路径互斥，cowboy 顺序匹配无遮蔽：`/offboarding/cases/123` 只命中
        %% detail（execute/verify/finalize 是字面段）；`/offboarding/123/execute`
        %% 不命中 detail（字面段 cases ≠ 123）。
        {"/api/v1/enterprise/organizations/:org_id/offboarding/cases", eb_tenant_handler, #{
            action => offboarding_list,
            auth_context => enterprise_owner_admin,
            required_governance => [<<"owner">>, <<"admin">>]
        }},
        {"/api/v1/enterprise/organizations/:org_id/offboarding/cases/:id", eb_tenant_handler, #{
            action => offboarding_detail,
            auth_context => enterprise_owner_admin,
            required_governance => [<<"owner">>, <<"admin">>]
        }}
    ].

-spec enterprise_platform_routes() -> list().
enterprise_platform_routes() ->
    [
        {"/api/adm/enterprise-business/organizations/:org_id/identities", eb_platform_handler, #{
            action => p_identities,
            auth_context => platform_admin,
            required_permission => <<"enterprise_business:read">>
        }},
        {"/api/adm/enterprise-business/organizations/:org_id/contacts", eb_platform_handler, #{
            action => p_contacts,
            auth_context => platform_admin,
            required_permission => <<"enterprise_business:read">>
        }},
        {"/api/adm/enterprise-business/organizations/:org_id/contacts/:id", eb_platform_handler, #{
            action => p_contact_detail,
            auth_context => platform_admin,
            required_permission => <<"enterprise_business:read">>
        }},
        {"/api/adm/enterprise-business/organizations/:org_id/conversations/:id/messages",
            eb_platform_handler, #{
                action => p_conversation_messages,
                auth_context => platform_admin,
                required_permission => <<"enterprise_business:read">>
            }},
        {"/api/adm/enterprise-business/organizations/:org_id/messages/:message_id",
            eb_platform_handler, #{
                action => p_message_detail,
                auth_context => platform_admin,
                required_permission => <<"enterprise_business:read">>
            }},
        {"/api/adm/enterprise-business/organizations/:org_id/assets/:id/content",
            eb_platform_handler, #{
                action => p_asset_content,
                auth_context => platform_admin,
                required_permission => <<"enterprise_business:read">>
            }},
        {"/api/adm/enterprise-business/organizations/:org_id/members/:uid/suspend",
            eb_platform_handler, #{
                action => p_suspend_member,
                auth_context => platform_admin,
                required_permission => <<"enterprise_business:write">>
            }},
        {"/api/adm/enterprise-business/organizations/:org_id/offboarding/:id/execute",
            eb_platform_handler, #{
                action => p_offboarding_execute,
                auth_context => platform_admin,
                required_permission => <<"enterprise_business:write">>
            }},
        {"/api/adm/enterprise-business/organizations/:org_id/offboarding/:id/verify",
            eb_platform_handler, #{
                action => p_offboarding_verify,
                auth_context => platform_admin,
                required_permission => <<"enterprise_business:write">>
            }},
        {"/api/adm/enterprise-business/organizations/:org_id/offboarding/:id/finalize",
            eb_platform_handler, #{
                action => p_offboarding_finalize,
                auth_context => platform_admin,
                required_permission => <<"enterprise_business:write">>
            }},
        %% offboarding 读取面（closure §8）：与租户面同款 `cases` 固定段设计；
        %% 平台读是只读端点（enterprise_business:read），跨 Org 必须显式带 :org_id。
        {"/api/adm/enterprise-business/organizations/:org_id/offboarding/cases",
            eb_platform_handler, #{
                action => p_offboarding_list,
                auth_context => platform_admin,
                required_permission => <<"enterprise_business:read">>
            }},
        {"/api/adm/enterprise-business/organizations/:org_id/offboarding/cases/:id",
            eb_platform_handler, #{
                action => p_offboarding_detail,
                auth_context => platform_admin,
                required_permission => <<"enterprise_business:read">>
            }}
    ].

-else.
%% 未选中 enterprise_business：两张面都不注册，路径字符串不进 beam。
-spec enterprise_tenant_routes() -> list().
enterprise_tenant_routes() ->
    [].

-spec enterprise_platform_routes() -> list().
enterprise_platform_routes() ->
    [].
-endif.

%% ===================================================================
%% FULL-08：Admin 企业应用治理面（A-01..A-14）
%% ===================================================================
%% 路径集合与 imboyadmin（分支 run/full-candidate-admin-20260921T101806Z）
%% src/modules/enterprise_apps/api/contracts.ts:ENDPOINTS **逐字对应**；前端
%% 字符串被单测钉死，后端不得漂移。前缀 `/api/adm/enterprise/` 与
%% - `/api/adm/enterprise-business/*`（enterprise_business 租户/运营面）
%% - `/api/internal/v1/*`（OA Application Credential 面）
%% 三者互不相交 —— 这是产品硬边界 §2 的机械保证（OA 凭据不能换 Admin 权限）。
%%
%% 权限：**按方法**在 handler 内强制（adm_acl:ensure_permission/3）——
%%   * GET（A-01/A-02/A-05/A-09/A-12/A-13/A-14）→ `enterprise_business:read`
%%   * POST/PUT/PATCH/DELETE（A-03/A-04/A-06/A-07/A-08/A-10/A-11）→
%%     `enterprise_business:write`
%% A-05/A-06、A-09/A-10 **同路径不同方法**，因此权限不能挂在路由条目上（挂在
%% 路由上只会得到「读权限可签发 credential」这类放宽）；逐方法判定放在
%% handler 的 with_read/with_write 里，与其余 `/api/adm/*` 路由的形态一致
%% （本仓 adm 面统一由 `/api/adm` 前缀的 adm_auth_middleware 做会话鉴权）。
%% 本模块**不注册、不接受**任何 Application Credential 请求头。
-spec enterprise_application_governance_routes() -> list().
enterprise_application_governance_routes() ->
    [
        %% A-01 列表（GET）
        {"/api/adm/enterprise/organizations/:org_id/applications",
            adm_enterprise_application_handler, #{action => applications}},
        %% A-02 详情（GET）
        {"/api/adm/enterprise/organizations/:org_id/applications/:application_id",
            adm_enterprise_application_handler, #{action => application_detail}},
        %% A-03 生命周期迁移（POST，CAS）
        {"/api/adm/enterprise/organizations/:org_id/applications/:application_id/status",
            adm_enterprise_application_handler, #{action => application_status}},
        %% A-04 scope 授予/降级（PUT，CAS）
        {"/api/adm/enterprise/organizations/:org_id/applications/:application_id/scopes",
            adm_enterprise_application_handler, #{action => application_scopes}},
        %% A-05 列表（GET）/ A-06 签发（POST，唯一回显 secret 的响应）
        {"/api/adm/enterprise/organizations/:org_id/applications/:application_id/credentials",
            adm_enterprise_application_handler, #{action => credentials}},
        %% A-07 轮换（POST，唯一回显 secret 的响应）
        {"/api/adm/enterprise/organizations/:org_id/applications/:application_id/credentials/:credential_id/rotate",
            adm_enterprise_application_handler, #{action => credential_rotate}},
        %% A-08 撤销（DELETE）
        {"/api/adm/enterprise/organizations/:org_id/applications/:application_id/credentials/:credential_id",
            adm_enterprise_application_handler, #{action => credential_revoke}},
        %% A-09 列表（GET）/ A-10 新增（POST）
        {"/api/adm/enterprise/organizations/:org_id/applications/:application_id/grants",
            adm_enterprise_application_handler, #{action => grants}},
        %% A-11 Grant CAS 增删（PATCH）
        {"/api/adm/enterprise/organizations/:org_id/applications/:application_id/grants/:grant_id",
            adm_enterprise_application_handler, #{action => grant}},
        %% A-12 投递统计（GET，无 payload）
        {"/api/adm/enterprise/organizations/:org_id/applications/:application_id/delivery-stats",
            adm_enterprise_application_handler, #{action => delivery_stats}},
        %% A-13 投递列表（GET，无 payload / 无 secret）
        {"/api/adm/enterprise/organizations/:org_id/applications/:application_id/deliveries",
            adm_enterprise_application_handler, #{action => deliveries}},
        %% A-14 审计（GET，before/after diff）
        {"/api/adm/enterprise/organizations/:org_id/applications/:application_id/audit-logs",
            adm_enterprise_application_handler, #{action => audit_logs}}
    ].

%% ===================================================================
%% CS-02：customer_service 路由段的**编译期物理裁剪** helper
%% ===================================================================
%% 模式照 enterprise_tenant_routes()/enterprise_platform_routes()：未选中
%% customer_service 时，`-ifdef(IMBOY_FEATURE_CUSTOMER_SERVICE)` 让整个函数体在
%% 预处理阶段被剔除 —— 客服路由的**路径字符串不进 imboy_router.beam**（物理裁剪，
%% 不是运行时开关），与 imboy_feature:route_feature/3 + compiled_routes/2 的运行时
%% 过滤构成双保险。运行时 feature 归属见 src/lib/imboy_feature.erl 的
%% route_feature(api, cs_tenant_handler, _) / route_feature(admin, cs_platform_handler, _)。
%% 契约侧：`scripts/contract_gate.py` 同批加了 2 条**精确 customer_service 行**把本段
%% 并入 api_v1/adm 两面（否则 `.contract/api_contract.json` 会丢这 21 条路由）。
%% ===================================================================
-ifdef(IMBOY_FEATURE_CUSTOMER_SERVICE).

-spec customer_service_tenant_routes() -> list().
customer_service_tenant_routes() ->
    [
        %% —— BE-S01a（T-2 裁定）：坐席上下文清单。主体自身作用域（无 Org 键），
        %% 一次返回当前用户全部可用 Organization[]/Workspace[]/active
        %% customer_service identity/seat enabled/capabilities。——
        {"/api/v1/cs/me/seat-contexts", cs_tenant_handler, #{
            action => seat_contexts,
            auth_context => cs_seat
        }},
        %% 门店开会话（T-2 后 org 显式在路径）：shop key 主体（cs_auth 用 digest
        %% 同语句证明 path org 与 key 绑定 org 逐字一致）。
        {"/api/v1/cs/organizations/:org_id/sessions/queue", cs_tenant_handler, #{
            action => session_queue,
            auth_context => cs_shop_key
        }},
        %% 坐席会话生命周期（claim/transfer/close）：cs_seat 主体；T-2 裁定——
        %% 服务端逐字校验 path org_id == session.org_id == 坐席 active member org。
        {"/api/v1/cs/organizations/:org_id/sessions/:id/claim", cs_tenant_handler, #{
            action => session_claim,
            auth_context => cs_seat,
            required_function => <<"customer_service">>,
            required_permission => <<"conversation.write">>
        }},
        {"/api/v1/cs/organizations/:org_id/sessions/:id/transfer", cs_tenant_handler, #{
            action => session_transfer,
            auth_context => cs_seat,
            required_function => <<"customer_service">>,
            required_permission => <<"conversation.write">>
        }},
        {"/api/v1/cs/organizations/:org_id/sessions/:id/close", cs_tenant_handler, #{
            action => session_close,
            auth_context => cs_seat,
            required_function => <<"customer_service">>,
            required_permission => <<"conversation.write">>
        }},
        %% 访客面：visit token 主体（列自己的会话 / 入站消息 / 评分）。
        {"/api/v1/cs/sessions", cs_tenant_handler, #{
            action => visitor_sessions,
            auth_context => cs_visit
        }},
        {"/api/v1/cs/sessions/:id/messages", cs_tenant_handler, #{
            action => session_messages,
            auth_context => cs_visit
        }},
        {"/api/v1/cs/sessions/:id/rating", cs_tenant_handler, #{
            action => session_rating,
            auth_context => cs_visit
        }},
        %% A0 客户端契约基准：客服端企业消息列表（游标 after_id，TSID string）。
        {"/api/v1/enterprise/conversations/:conversation_id/messages", cs_tenant_handler, #{
            action => conversation_messages,
            auth_context => cs_seat,
            required_function => <<"customer_service">>,
            required_permission => <<"conversation.read">>
        }},
        %% —— 租户治理面（owner/admin）：seat / shop key / visit token 管理 ——
        %% C4（contracts-w2）：seats 列表 GET 支持 after_id/limit 键集分页。
        {"/api/v1/cs/organizations/:org_id/seats", cs_tenant_handler, #{
            action => seats,
            auth_context => enterprise_owner_admin,
            required_governance => [<<"owner">>, <<"admin">>]
        }},
        {"/api/v1/cs/organizations/:org_id/seats/:id/suspend", cs_tenant_handler, #{
            action => seat_suspend,
            auth_context => enterprise_owner_admin,
            required_governance => [<<"owner">>, <<"admin">>]
        }},
        {"/api/v1/cs/organizations/:org_id/seats/:id/resume", cs_tenant_handler, #{
            action => seat_resume,
            auth_context => enterprise_owner_admin,
            required_governance => [<<"owner">>, <<"admin">>]
        }},
        %% C2（contracts-w2）：shop key 治理面（列表 GET + 创建 POST 同路径动作，
        %% 动作键冻结为 shop_key_list；cowboy 只按 path 匹配——seats 同款先例）。
        {"/api/v1/cs/organizations/:org_id/shop-keys", cs_tenant_handler, #{
            action => shop_key_list,
            auth_context => enterprise_owner_admin,
            required_governance => [<<"owner">>, <<"admin">>]
        }},
        {"/api/v1/cs/organizations/:org_id/shop-keys/:id/revoke", cs_tenant_handler, #{
            action => shop_key_revoke,
            auth_context => enterprise_owner_admin,
            required_governance => [<<"owner">>, <<"admin">>]
        }},
        %% C3（contracts-w2）：visit token 治理面（列表 GET + 签发 POST 同路径动作）。
        {"/api/v1/cs/organizations/:org_id/visit-tokens", cs_tenant_handler, #{
            action => visit_token_list,
            auth_context => enterprise_owner_admin,
            required_governance => [<<"owner">>, <<"admin">>]
        }},
        {"/api/v1/cs/organizations/:org_id/visit-tokens/:id/revoke", cs_tenant_handler, #{
            action => visit_token_revoke,
            auth_context => enterprise_owner_admin,
            required_governance => [<<"owner">>, <<"admin">>]
        }},
        %% CSB-03：坐席会话详情（GET；T-2 后路径显式 org_id；登记在全部字面
        %% 路径之后——cowboy 按注册序匹配，:id 不得抢在 queue 等字面段之前）。
        {"/api/v1/cs/organizations/:org_id/sessions/:id", cs_tenant_handler, #{
            action => session_detail,
            auth_context => cs_seat,
            required_function => <<"customer_service">>,
            required_permission => <<"conversation.read">>
        }},
        %% CSB-02R：坐席工作台 active/closed 两视图（T-2 后 org 显式在路径）。
        %% 独立路径而非 /sessions?scope=seat 的理由：GET /api/v1/cs/sessions 已
        %% 冻结为访客面，route metadata 是 principal 的唯一分流依据。
        {"/api/v1/cs/organizations/:org_id/seats/sessions", cs_tenant_handler, #{
            action => seat_session_list,
            auth_context => cs_seat,
            required_function => <<"customer_service">>,
            required_permission => <<"conversation.read">>
        }},
        %% —— BE-S01a：坐席 transfer 目标最小投影（同 Org 其他可用坐席；
        %% api-surface-freeze：无 owner/admin 权限要求，不复用治理 identity 列表）——
        {"/api/v1/cs/organizations/:org_id/transfer-targets", cs_tenant_handler, #{
            action => transfer_targets,
            auth_context => cs_seat,
            required_function => <<"customer_service">>,
            required_permission => <<"conversation.read">>
        }},
        %% —— BE-S01a：坐席 SSE 事件流占位（sse-event-contract 端点；流式实现
        %% 在 BE-S01b——facade 返回 not_implemented → 501。字面段 me 先于
        %% seats/:id/suspend 的 :id 捕获注册）。——
        {"/api/v1/cs/organizations/:org_id/seats/me/events", cs_tenant_handler, #{
            action => seat_events,
            auth_context => cs_seat,
            required_function => <<"customer_service">>,
            required_permission => <<"conversation.read">>
        }},
        %% —— CSB-03：widget 接入面（浏览器访客；凭证 = bootstrap 令牌专用头
        %% x-cs-visit-token，查询串携带即 400；中间件免签/免 JWT 直通面由
        %% cs_http:is_credential_surface_path/1 声明，handler 侧 fail-closed；
        %% bootstrap 的 Origin 头经 cs_widget:normalize_origin/1 归一后进
        %% application 与 installation allowlist 精确匹配——Origin 不是唯一
        %% 认证，令牌/限流/租户 scope 照常生效；SSE 见 cs_widget_handler）——
        {"/api/v1/cs/widget/bootstrap", cs_widget_handler, #{
            action => widget_bootstrap,
            auth_context => cs_visit
        }},
        {"/api/v1/cs/widget/identity/exchange", cs_widget_handler, #{
            action => widget_identity_exchange,
            auth_context => cs_visit
        }},
        %% BE-W01：动态 frame HTML（iframe src 落点，零凭证面；嵌入策略由
        %% handler 按 installation allowed_origins 出精确 frame-ancestors CSP，
        %% security_headers_middleware / cors_middleware 对该路径豁免 XFO）。
        {"/api/v1/cs/widget/frame/:installation_id", cs_widget_frame_handler, #{
            action => widget_frame_html,
            auth_context => cs_visit
        }},
        %% CSD-BE-01（hosted-widget-contract S2/S4）：动态 frame HTML 的新落点
        %% `GET /w/:public_widget_id`（snippet 零 org/workspace 申报，iframe src
        %% 只带全局 public ID）。零凭证导航面：租户归属由 public_widget_id
        %% 全局反查**派生**；HTML 壳零 installation_id/org/workspace/secret；
        %% 错误统一 404 installation_unavailable。handler 复用 cs_widget_handler
        %% （surface=widget 注入走 customer_service_wire_route 同款分支）。
        %% 根路径不与 /api/* 路由树冲突（段首 w 唯一）；auth_middleware 对
        %% /w/* 直通、throttle 归 cs_widget 桶、XFO 豁免经
        %% imboy_route_shape:is_cs_widget_frame_path/1 单一真源登记。
        {"/w/:public_widget_id", cs_widget_handler, #{
            action => widget_public_frame_html,
            auth_context => cs_visit
        }},
        {"/api/v1/cs/widget/sessions", cs_widget_handler, #{
            action => widget_sessions,
            auth_context => cs_visit
        }},
        {"/api/v1/cs/widget/sessions/:id/messages", cs_widget_handler, #{
            action => widget_session_messages,
            auth_context => cs_visit
        }},
        {"/api/v1/cs/widget/sessions/:id/events", cs_widget_handler, #{
            action => widget_session_events,
            auth_context => cs_visit
        }},
        {"/api/v1/cs/widget/sessions/:id/assets/presign", cs_widget_handler, #{
            action => widget_asset_upload,
            auth_context => cs_visit
        }},
        {"/api/v1/cs/widget/sessions/:id/assets/confirm", cs_widget_handler, #{
            action => widget_asset_confirm,
            auth_context => cs_visit
        }},
        %% BE-PATCH-01：访客附件字节上传代理（payload=请求体字节；upload_ref 是
        %% 唯一凭证——FE 裸 PUT 合同，无凭证头/无 Cookie。服务端经
        %% eb_asset_app:put_object 写私有桶，响应永无对象 URL）。
        {"/api/v1/cs/widget/sessions/:id/assets/upload", cs_widget_handler, #{
            action => widget_asset_put,
            auth_context => cs_visit
        }},
        %% BE-S01b：访客附件内容代理（GET；响应是对象字节本体，不走 JSON 面——
        %% 线格式分支在 cs_widget_handler；绑定门：asset 必须绑在本会话的消息上）。
        {"/api/v1/cs/widget/sessions/:id/assets/:asset/content", cs_widget_handler, #{
            action => widget_asset_content,
            auth_context => cs_visit
        }},
        {"/api/v1/cs/widget/sessions/:id/rating", cs_widget_handler, #{
            action => widget_session_rating,
            auth_context => cs_visit
        }}
    ].

-spec customer_service_platform_routes() -> list().
customer_service_platform_routes() ->
    [
        {"/api/adm/customer-service/organizations/:org_id/seats", cs_platform_handler, #{
            action => p_seats,
            auth_context => platform_admin,
            required_permission => <<"customer_service:read">>
        }},
        %% BE-S01b（api-surface-freeze admin_provisioning）：事务化开通/修复
        %% identity/assignment/seat + actor/target/before/after 不可抵赖审计；
        %% customer_service:write 平台权限；幂等（重复调用返回既有事实 200）。
        {"/api/adm/customer-service/organizations/:org_id/provisioning", cs_platform_handler, #{
            action => p_seat_provision,
            auth_context => platform_admin,
            required_permission => <<"customer_service:write">>
        }},
        %% C1（contracts-w2）：平台 session 列表（只读；workspace_id 为 face 级
        %% 必填参数——不存在「不带 workspace 的全局列举」）。
        {"/api/adm/customer-service/organizations/:org_id/sessions", cs_platform_handler, #{
            action => p_session_list,
            auth_context => platform_admin,
            required_permission => <<"customer_service:read">>
        }},
        {"/api/adm/customer-service/organizations/:org_id/sessions/:id", cs_platform_handler, #{
            action => p_session,
            auth_context => platform_admin,
            required_permission => <<"customer_service:read">>
        }},
        {"/api/adm/customer-service/organizations/:org_id/seats/:id/suspend", cs_platform_handler,
            #{
                action => p_seat_suspend,
                auth_context => platform_admin,
                required_permission => <<"customer_service:write">>
            }},
        {"/api/adm/customer-service/organizations/:org_id/seats/:id/resume", cs_platform_handler, #{
                action => p_seat_resume,
                auth_context => platform_admin,
                required_permission => <<"customer_service:write">>
            }},
        {"/api/adm/customer-service/organizations/:org_id/sessions/:id/transfer",
            cs_platform_handler, #{
                action => p_session_transfer,
                auth_context => platform_admin,
                required_permission => <<"customer_service:write">>
            }},
        {"/api/adm/customer-service/organizations/:org_id/sessions/:id/close", cs_platform_handler,
            #{
                action => p_session_close,
                auth_context => platform_admin,
                required_permission => <<"customer_service:write">>
            }},
        {"/api/adm/customer-service/widget-installations", cs_platform_handler, #{
            action => p_widget_installations,
            auth_context => platform_admin,
            required_permission => <<"customer_service:read">>
        }},
        {"/api/adm/customer-service/widget-installations/:id/revoke", cs_platform_handler, #{
            action => p_widget_installation_revoke,
            auth_context => platform_admin,
            required_permission => <<"customer_service:write">>
        }}
    ].

-else.
%% 未选中 customer_service：两张面都不注册，路径字符串不进 beam。
-spec customer_service_tenant_routes() -> list().
customer_service_tenant_routes() ->
    [].

-spec customer_service_platform_routes() -> list().
customer_service_platform_routes() ->
    [].
-endif.

%% ===========================================================================
%% EPGZ-08 W4：企业 internal 面路由（/api/internal/v1/*）
%%
%% 冻结真源 = src/api/enterprise_internal_routes.erl（method+path+scope+
%% rate_bucket+idempotency+sender_mode，逐条对应 A0 control/
%% internal-api-manifest.yaml 的 INT-01..INT-14）。本函数只把该表映射为
%% cowboy 三元组：**路径与方法真源仍只有一处**，这里不重抄 scope/rate。
%%
%% 三条硬约束（plan §3 硬边界，均由 control/assert_manifest.py 机械断言）：
%%   1. 前缀恒为 /api/internal/v1/，**零 Open Platform 生产面**（硬边界 1）；
%%   2. 不进 imboy_router:open()/option()：匿名不可达（认证由
%%      enterprise_internal_middleware 的 credential 链负责）；
%%   3. 认证中间件方向：auth_middleware 前缀分支委托，**不落 verify_sign**
%%      客户端签名门（internal 面无设备/JWT/签名）。
%%
%% 路径参数：cowboy 段名 :group_id / :delivery_id 由 cowboy_router 注入
%% bindings，enterprise_internal_middleware 合并（并把这两个 TSID 段名收敛
%% 为整数）进 handler_opts。
%%
%% 无 feature 门：见 get_routes/0 内注释（新增 feature key 会改动产品 feature
%% manifest hash，超出 EPGZ-08 集成范围）。
%% ===========================================================================

-spec enterprise_internal_routes() -> list().
enterprise_internal_routes() ->
    [
        %% INT-01 凭证自检
        {"/api/internal/v1/application", enterprise_application_handler, #{action => self_info}},
        %% INT-02 绑定 external_user_id <-> active member（PUT；与 INT-15 的
        %% DELETE 共用本 path，方法分派在 handler 的 mappings action 内）
        %% INT-03 批量解析（无全量导出形态）
        {"/api/internal/v1/identity-mappings/resolve", enterprise_identity_handler, #{
            action => resolve
        }},
        %% INT-04 创建 Workspace 企业群
        {"/api/internal/v1/groups", enterprise_group_handler, #{action => create}},
        %% INT-05/06 同一 path 幂等加/删成员（方法分派在 handler 内，非表外组合）
        {"/api/internal/v1/groups/:group_id/members", enterprise_group_handler, #{
            action => members
        }},
        %% INT-07/08 企业附件 presign/confirm
        {"/api/internal/v1/files/presign", enterprise_asset_handler, #{action => presign}},
        {"/api/internal/v1/files/confirm", enterprise_asset_handler, #{action => confirm}},
        %% INT-09/10 OA 代发（application / 指定 sender_user_id），固定非 E2EE
        {"/api/internal/v1/messages/direct", enterprise_message_handler, #{action => direct}},
        {"/api/internal/v1/groups/:group_id/messages", enterprise_message_handler, #{
            action => group
        }},
        %% INT-11 代 Human 发起好友申请（只发起，无自动接受/确认）
        {"/api/internal/v1/friend-requests", enterprise_friend_request_handler, #{
            action => create
        }},
        %% INT-12/13 Webhook 配置与仅本 Application 的 replay
        {"/api/internal/v1/webhook", enterprise_webhook_handler, #{action => configure}},
        {"/api/internal/v1/webhook/deliveries/:delivery_id/replay", enterprise_webhook_handler, #{
            action => replay
        }},
        %% INT-14 OA SSO 一次性 code 原子交换
        {"/api/internal/v1/oa/sso/exchange", enterprise_oa_sso_exchange_handler, #{
            action => exchange
        }},
        %% ---- FULL-02 新增（A0 接线）。边界规格 enterprise_internal_boundary:
        %%      spec/1 与冻结表 enterprise_internal_routes:routes/0 两侧必须
        %%      逐条一致，有机械比对测试。----
        %% INT-02(PUT 绑定)/INT-15(DELETE 撤销) 共用同一 path，方法分派在
        %% handler 内（cowboy 不允许同 path 重复登记；与 INT-05/06 同款口径）
        {"/api/internal/v1/identity-mappings", enterprise_identity_handler, #{action => mappings}},
        %% INT-16 映射受限游标目录（无全量导出形态）
        {"/api/internal/v1/identity-mappings/directory", enterprise_directory_handler, #{
            action => mappings
        }},
        %% INT-17 成员受限游标目录
        {"/api/internal/v1/directory/users", enterprise_directory_handler, #{
            action => users
        }},
        %% INT-18/19/21 群详情(GET) / 更新(PATCH) / 归档(DELETE)：同一 path，
        %% 方法分派在 handler 内（与 INT-05/06 同款口径）
        {"/api/internal/v1/groups/:group_id", enterprise_group_handler, #{action => group}},
        %% INT-20 成员角色
        {"/api/internal/v1/groups/:group_id/members/roles", enterprise_group_handler, #{
            action => member_roles
        }},
        %% INT-22 附件留存/hold/purge 治理
        {"/api/internal/v1/files/governance", enterprise_asset_handler, #{action => governance}},
        %% INT-23 投递列表 + 健康度摘要（FULL-03；只读，无 payload）
        {"/api/internal/v1/webhook/deliveries", enterprise_webhook_handler, #{
            action => deliveries
        }}
    ].
