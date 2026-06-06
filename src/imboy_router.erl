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
    MainRoutes =
        [
            {"/", index_handler, #{action => help}},
            {"/help", index_handler, #{action => help}},

            % v0 兼容路由 — 计划在 1.1.0 移除，届时通过 410 Gone 响应引导客户端升级
            {"/init", index_handler, #{action => init}},
            {"/refreshtoken", passport_handler, #{action => refreshtoken}},

            {"/app_version/check", app_version_handler, #{action => check}},

            % 【新增】Prometheus 指标端点
            {"/metrics", metrics_handler, #{}},

            {"/passport/quick_login", passport_handler, #{action => quick_login}},
            {"/passport/login", passport_handler, #{action => login}},
            {"/passport/signup", passport_handler, #{action => signup}},
            {"/passport/getcode", passport_handler, #{action => getcode}},
            {"/passport/findpassword", passport_handler, #{action => find_password}},
            {"/passport/bind_mail", passport_handler, #{action => bind_mail}},

            {"/ws", websocket_handler, #{}},
            {"/auth/assets", auth_handler, #{action => assets}}
        ] ++
            test_routes() ++
            [
                {"/conversation/online", conversation_handler, #{action => online}},
                {"/conversation/mine", conversation_handler, #{action => mine}},
                {"/conversation/pin", conversation_handler, #{action => pin_conversation}},
                {"/conversation/unpin", conversation_handler, #{action => unpin_conversation}},
                {"/conversation/pinned", conversation_handler, #{action => pinned_list}},
                {"/conversation/delete", conversation_handler, #{action => delete_conversation}},
                {"/conversation/restore", conversation_handler, #{action => restore_conversation}},
                {"/msg/offline", msg_handler, #{action => offline}},
                {"/msg/offline_ack", msg_handler, #{action => offline_ack}},

                {"/user/qrcode", user_handler, #{action => qrcode}},
                {"/user/update", user_handler, #{action => update}},
                {"/user/show", user_handler, #{action => show}},
                {"/user/change_state", user_handler, #{action => change_state}},
                {"/user/setting", user_handler, #{action => setting}},
                {"/user/credential", user_handler, #{action => credential}},
                {"/user/change_password", user_handler, #{action => change_password}},
                {"/user/set_password", user_handler, #{action => set_password}},
                {"/user/apply_logout", user_handler, #{action => apply_logout}},
                {"/user/cancel_logout", user_handler, #{action => cancel_logout}},
                {"/user/search", user_handler, #{action => search}},

                {"/user_device/page", user_device_handler, #{action => page}},
                {"/user_device/change_name", user_device_handler, #{action => change_name}},
                {"/user_device/delete", user_device_handler, #{action => delete}},

                {"/user_collect/page", user_collect_handler, #{action => page}},
                {"/user_collect/add", user_collect_handler, #{action => add}},
                {"/user_collect/remove", user_collect_handler, #{action => remove}},
                {"/user_collect/change", user_collect_handler, #{action => change}},

                {"/feedback/page", feedback_handler, #{action => page}},
                {"/feedback/add", feedback_handler, #{action => add}},
                {"/feedback/change", feedback_handler, #{action => change}},
                {"/feedback/remove", feedback_handler, #{action => remove}},
                {"/feedback/reply", feedback_handler, #{action => reply}},
                {"/feedback/page_reply", feedback_handler, #{action => page_reply}},

                {"/user_tag/page", user_tag_handler, #{action => page}},
                {"/user_tag/add", user_tag_handler, #{action => add}},
                {"/user_tag/change_name", user_tag_handler, #{action => change_name}},
                {"/user_tag/delete", user_tag_handler, #{action => delete}},

                {"/user_tag_relation/collect_page", user_tag_relation_handler, #{
                    action => collect_page
                }},
                {"/user_tag_relation/friend_page", user_tag_relation_handler, #{
                    action => friend_page
                }},
                {"/user_tag_relation/add", user_tag_relation_handler, #{action => add}},
                {"/user_tag_relation/set", user_tag_relation_handler, #{action => set}},
                {"/user_tag_relation/remove", user_tag_relation_handler, #{action => remove}},

                {"/location/makeMyselfVisible", location_handler, #{action => make_myself_visible}},
                {"/location/makeMyselfUnvisible", location_handler, #{
                    action => make_myself_unvisible
                }},
                {"/location/peopleNearby", location_handler, #{action => people_nearby}},

                {"/friend/add", friend_handler, #{action => add_friend}},
                {"/friend/confirm", friend_handler, #{action => confirm}},
                {"/friend/reject", friend_handler, #{action => reject}},
                {"/friend/delete", friend_handler, #{action => delete_friend}},
                {"/friend/list", friend_handler, #{action => list}},
                {"/friend/information", friend_handler, #{action => information}},
                {"/friend/change_remark", friend_handler, #{action => change_remark}},

                {"/friend/denylist/add", user_denylist_handler, #{action => add}},
                {"/friend/denylist/remove", user_denylist_handler, #{action => remove}},
                {"/friend/denylist/page", user_denylist_handler, #{action => page}},

                {"/friend/move", friend_handler, #{action => move}},
                {"/friend/category/add", friend_category_handler, #{action => add}},
                {"/friend/category/delete", friend_category_handler, #{action => delete}},
                {"/friend/category/rename", friend_category_handler, #{action => rename}},

                % 搜索"用户允许被搜索"的用户
                {"/fts/user_search", fts_handler, #{action => user_search}},
                % 最近新注册的并且允许被搜索到的朋友
                {"/fts/recently_user", fts_handler, #{action => recently_user}},

                {"/group/remark", group_handler, #{action => remark}},
                {"/group/qrcode", group_handler, #{action => qrcode}},
                {"/group/face2face", group_handler, #{action => face2face}},
                {"/group/face2face_save", group_handler, #{action => face2face_save}},
                {"/group/add", group_handler, #{action => add}},
                {"/group/edit", group_handler, #{action => edit}},
                {"/group/dissolve", group_handler, #{action => dissolve}},
                {"/group/detail", group_handler, #{action => detail}},
                {"/group/page", group_handler, #{action => page}},
                {"/group/msg_page", group_handler, #{action => msg_page}},

                {"/group_member/join", group_member_handler, #{action => join}},
                {"/group_member/leave", group_member_handler, #{action => leave}},
                {"/group_member/page", group_member_handler, #{action => page}},
                {"/group_member/alias", group_member_handler, #{action => alias}},
                {"/group_member/same_group", group_member_handler, #{action => same_group}},
                % 群组公告
                {"/group_notice/add", group_notice_handler, #{action => add}},
                {"/group_notice/edit", group_notice_handler, #{action => edit}},
                {"/group_notice/delete", group_notice_handler, #{action => delete}},
                {"/group_notice/page", group_notice_handler, #{action => page}},
                {"/group_notice/publish", group_notice_handler, #{action => publish}},
                {"/group_notice/latest", group_notice_handler, #{action => latest}},
                % @提及功能
                {"/mention/list", mention_handler, #{action => list}},
                {"/mention/unread", mention_handler, #{action => unread}},
                {"/mention/mark_read", mention_handler, #{action => mark_read}},
                {"/mention/suggest", mention_handler, #{action => suggest}},
                % 群相册
                {"/group_album/create", group_album_handler, #{action => create_album}},
                {"/group_album/list", group_album_handler, #{action => list_albums}},
                {"/group_album/rename", group_album_handler, #{action => rename_album}},
                {"/group_album/delete", group_album_handler, #{action => delete_album}},
                {"/group_album/photo/upload", group_album_handler, #{action => upload_photo}},
                {"/group_album/photo/batch", group_album_handler, #{action => batch_upload}},
                {"/group_album/photo/list", group_album_handler, #{action => list_photos}},
                {"/group_album/photo/detail", group_album_handler, #{action => photo_detail}},
                {"/group_album/photo/delete", group_album_handler, #{action => delete_photo}},
                {"/group_album/photo/like", group_album_handler, #{action => like_photo}},
                {"/group_album/photo/unlike", group_album_handler, #{action => unlike_photo}},
                {"/group_album/photo/comment", group_album_handler, #{action => add_comment}},
                {"/group_album/photo/comments", group_album_handler, #{action => list_comments}},
                {"/group_album/cover/update", group_album_handler, #{action => update_cover}},
                % end v0 兼容路由 — 计划在 1.1.0 移除

                %%%%%%% 上面写API路由，下面写静态资源 %%%%%%%%

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
            {"/v1/init", index_handler, #{action => init}},
            {"/v1/refreshtoken", passport_handler, #{action => refreshtoken}},
            {"/v1/app/features", app_feature_handler, #{action => features}},
            {"/v1/app/manifest", app_manifest_handler, #{action => manifest}},
            {"/v1/app/policy", app_feature_handler, #{action => policy}},
            {"/v1/app/ice_servers", app_feature_handler, #{action => ice_servers}},
            {"/v1/app_version/check", app_version_handler, #{action => check}},
            {"/v1/app_upgrade/report", app_upgrade_log_handler, #{action => report}},

            % 【新增】Prometheus 指标端点
            {"/v1/metrics", metrics_handler, #{}},

            {"/v1/passport/quick_login", passport_handler, #{action => quick_login}},
            {"/v1/passport/login", passport_handler, #{action => login}},
            {"/v1/passport/signup", passport_handler, #{action => signup}},
            {"/v1/passport/getcode", passport_handler, #{action => getcode}},
            {"/v1/passport/findpassword", passport_handler, #{action => find_password}},
            {"/v1/passport/bind_mail", passport_handler, #{action => bind_mail}},

            % QR 码登录（WhatsApp Web 风格）
            {"/v1/passport/qr_login/create", qr_login_handler, #{action => create}},
            {"/v1/passport/qr_login/status", qr_login_handler, #{action => status}},
            {"/v1/passport/qr_login/scan", qr_login_handler, #{action => scan}},
            {"/v1/passport/qr_login/confirm", qr_login_handler, #{action => confirm}},
            {"/v1/passport/qr_login/cancel", qr_login_handler, #{action => cancel}},
            %% PR-3β: SSE 推送端点（cowboy_loop），替代轮询 /status
            %% Web 端 EventSource 长连接，scan/confirm 后实时推送状态
            {"/v1/passport/qr_login/subscribe", qr_login_sse_handler, #{}},

            {"/v1/ws", websocket_handler, #{}},
            {"/v1/auth/assets", auth_handler, #{action => assets}}
        ] ++
            test_routes_v1() ++
            [
                {"/v1/conversation/online", conversation_handler, #{action => online}},
                {"/v1/conversation/mine", conversation_handler, #{action => mine}},
                {"/v1/conversation/pin", conversation_handler, #{action => pin_conversation}},
                {"/v1/conversation/unpin", conversation_handler, #{action => unpin_conversation}},
                {"/v1/conversation/pinned", conversation_handler, #{action => pinned_list}},
                {"/v1/conversation/delete", conversation_handler, #{action => delete_conversation}},
                {"/v1/conversation/restore", conversation_handler, #{
                    action => restore_conversation
                }},
                {"/v1/msg/offline", msg_handler, #{action => offline}},
                {"/v1/msg/offline_ack", msg_handler, #{action => offline_ack}},
                {"/v1/msg/read_stats", msg_handler, #{action => read_stats}},
                {"/v1/msg/pin", msg_handler, #{action => pin}},
                {"/v1/msg/forward", msg_handler, #{action => forward}},
                {"/v1/msg/reaction/add", msg_handler, #{action => reaction_add}},
                {"/v1/msg/reaction/remove", msg_handler, #{action => reaction_remove}},
                {"/v1/msg/reaction/list", msg_handler, #{action => reaction_list}},
                {"/v1/msg/history", msg_handler, #{action => history}},

                {"/v1/user/qrcode", user_handler, #{action => qrcode}},
                {"/v1/user/update", user_handler, #{action => update}},
                {"/v1/user/show", user_handler, #{action => show}},
                {"/v1/user/change_state", user_handler, #{action => change_state}},
                {"/v1/user/setting", user_handler, #{action => setting}},
                {"/v1/user/credential", user_handler, #{action => credential}},
                {"/v1/user/change_password", user_handler, #{action => change_password}},
                {"/v1/user/set_password", user_handler, #{action => set_password}},
                {"/v1/user/apply_logout", user_handler, #{action => apply_logout}},
                {"/v1/user/cancel_logout", user_handler, #{action => cancel_logout}},
                {"/v1/user/export_data", user_handler, #{action => export_data}},
                {"/v1/user/search", user_handler, #{action => search}},

                {"/v1/user_device/page", user_device_handler, #{action => page}},
                {"/v1/user_device/change_name", user_device_handler, #{action => change_name}},
                {"/v1/user_device/delete", user_device_handler, #{action => delete}},

                % 设备会话管理
                {"/v1/user_device/sessions", user_device_handler, #{action => sessions}},
                {"/v1/user_device/check_login", user_device_handler, #{action => check_login}},
                {"/v1/user_device/kick", user_device_handler, #{action => kick}},
                {"/v1/user_device/kick-others", user_device_handler, #{action => kick_others}},

                % 推送 Token 管理
                {"/v1/push/register", user_device_handler, #{action => push_register}},
                {"/v1/push/unregister", user_device_handler, #{action => push_unregister}},

                {"/v1/e2ee/user_keys", e2ee_handler, #{action => user_keys}},
                {"/v1/e2ee/group_member_keys", e2ee_handler, #{action => group_member_keys}},
                {"/v1/e2ee/report_device_key", e2ee_handler, #{action => report_device_key}},
                {"/v1/e2ee/key/status", e2ee_handler, #{action => key_status}},
                {"/v1/e2ee/notifications/pull", e2ee_handler, #{action => pull_notifications}},
                {"/v1/e2ee/recovery/start", e2ee_handler, #{action => start_recovery}},
                {"/v1/e2ee/backup/list", e2ee_handler, #{action => backup_list}},
                {"/v1/e2ee/backup/delete", e2ee_handler, #{action => backup_delete}},

                {"/v1/e2ee/transfer/create", e2ee_transfer_handler, #{action => create}},
                {"/v1/e2ee/transfer/accept", e2ee_transfer_handler, #{action => accept}},
                {"/v1/e2ee/transfer/confirm", e2ee_transfer_handler, #{action => confirm}},
                {"/v1/e2ee/transfer/cancel", e2ee_transfer_handler, #{action => cancel}},
                {"/v1/e2ee/transfer/info", e2ee_transfer_handler, #{action => info}},
                {"/v1/e2ee/transfer/pending", e2ee_transfer_handler, #{action => pending}},

                {"/v1/e2ee/social/contacts", e2ee_social_handler, #{action => contacts}},
                {"/v1/e2ee/social/contacts/add", e2ee_social_handler, #{action => add_contact}},
                {"/v1/e2ee/social/contacts/remove", e2ee_social_handler, #{
                    action => remove_contact
                }},
                {"/v1/e2ee/social/create_shards", e2ee_social_handler, #{action => create_shards}},
                {"/v1/e2ee/social/shards", e2ee_social_handler, #{action => get_shards}},
                {"/v1/e2ee/social/recover", e2ee_social_handler, #{action => recover_key}},
                {"/v1/e2ee/social/proxy_shards", e2ee_social_handler, #{action => get_proxy_shards}},
                {"/v1/e2ee/social/decrypt_shard", e2ee_social_handler, #{action => decrypt_shard}},

                % 合规密钥分发（三层加密架构）
                {"/v1/e2ee/compliance_key", e2ee_handler, #{action => compliance_key}},

                {"/v1/user_collect/page", user_collect_handler, #{action => page}},
                {"/v1/user_collect/add", user_collect_handler, #{action => add}},
                {"/v1/user_collect/remove", user_collect_handler, #{action => remove}},
                {"/v1/user_collect/change", user_collect_handler, #{action => change}},

                {"/v1/feedback/page", feedback_handler, #{action => page}},
                {"/v1/feedback/add", feedback_handler, #{action => add}},
                {"/v1/feedback/change", feedback_handler, #{action => change}},
                {"/v1/feedback/remove", feedback_handler, #{action => remove}},
                {"/v1/feedback/reply", feedback_handler, #{action => reply}},
                {"/v1/feedback/page_reply", feedback_handler, #{action => page_reply}},

                {"/v1/user_tag/page", user_tag_handler, #{action => page}},
                {"/v1/user_tag/add", user_tag_handler, #{action => add}},
                {"/v1/user_tag/change_name", user_tag_handler, #{action => change_name}},
                {"/v1/user_tag/delete", user_tag_handler, #{action => delete}},

                {"/v1/user_tag_relation/collect_page", user_tag_relation_handler, #{
                    action => collect_page
                }},
                {"/v1/user_tag_relation/friend_page", user_tag_relation_handler, #{
                    action => friend_page
                }},
                {"/v1/user_tag_relation/add", user_tag_relation_handler, #{action => add}},
                {"/v1/user_tag_relation/set", user_tag_relation_handler, #{action => set}},
                {"/v1/user_tag_relation/remove", user_tag_relation_handler, #{action => remove}},

                {"/v1/location/makeMyselfVisible", location_handler, #{
                    action => make_myself_visible
                }},
                {"/v1/location/makeMyselfUnvisible", location_handler, #{
                    action => make_myself_unvisible
                }},
                {"/v1/location/peopleNearby", location_handler, #{action => people_nearby}},

                {"/v1/friend/add", friend_handler, #{action => add_friend}},
                {"/v1/friend/confirm", friend_handler, #{action => confirm}},
                {"/v1/friend/reject", friend_handler, #{action => reject}},
                {"/v1/friend/delete", friend_handler, #{action => delete_friend}},
                {"/v1/friend/list", friend_handler, #{action => list}},
                {"/v1/friend/information", friend_handler, #{action => information}},
                {"/v1/friend/change_remark", friend_handler, #{action => change_remark}},

                {"/v1/friend/denylist/add", user_denylist_handler, #{action => add}},
                {"/v1/friend/denylist/remove", user_denylist_handler, #{action => remove}},
                {"/v1/friend/denylist/page", user_denylist_handler, #{action => page}},

                {"/v1/friend/move", friend_handler, #{action => move}},
                {"/v1/friend/category/add", friend_category_handler, #{action => add}},
                {"/v1/friend/category/delete", friend_category_handler, #{action => delete}},
                {"/v1/friend/category/rename", friend_category_handler, #{action => rename}},

                % 搜索"用户允许被搜索"的用户
                {"/v1/fts/user_search", fts_handler, #{action => user_search}},
                % 最近新注册的并且允许被搜索到的朋友
                {"/v1/fts/recently_user", fts_handler, #{action => recently_user}},
                % 消息全文搜索
                {"/v1/fts/msg", fts_handler, #{action => msg}},

                {"/v1/group/remark", group_handler, #{action => remark}},
                {"/v1/group/qrcode", group_handler, #{action => qrcode}},
                {"/v1/group/face2face", group_handler, #{action => face2face}},
                {"/v1/group/face2face_save", group_handler, #{action => face2face_save}},
                {"/v1/group/add", group_handler, #{action => add}},
                {"/v1/group/edit", group_handler, #{action => edit}},
                {"/v1/group/dissolve", group_handler, #{action => dissolve}},
                {"/v1/group/detail", group_handler, #{action => detail}},
                {"/v1/group/page", group_handler, #{action => page}},
                {"/v1/group/msg_page", group_handler, #{action => msg_page}},

                % 群组分类管理
                {"/v1/group/category/create", group_category_handler, #{action => create}},
                {"/v1/group/category/list", group_category_handler, #{action => list}},
                {"/v1/group/category/rename", group_category_handler, #{action => rename}},
                {"/v1/group/category/delete", group_category_handler, #{action => delete}},
                {"/v1/group/category/move_group", group_category_handler, #{action => move_group}},
                {"/v1/group/category/sort", group_category_handler, #{action => sort}},

                {"/v1/group_member/join", group_member_handler, #{action => join}},
                {"/v1/group_member/leave", group_member_handler, #{action => leave}},
                {"/v1/group_member/page", group_member_handler, #{action => page}},
                {"/v1/group_member/alias", group_member_handler, #{action => alias}},
                {"/v1/group_member/same_group", group_member_handler, #{action => same_group}},
                {"/v1/group_member/mute", group_member_handler, #{action => mute}},
                {"/v1/group_member/unmute", group_member_handler, #{action => unmute}},
                {"/v1/group_member/role", group_member_handler, #{action => role}},
                {"/v1/group/transfer", group_handler, #{action => transfer}},
                % 群组标签
                {"/v1/group/tag/add", group_tag_handler, #{action => add}},
                {"/v1/group/tag/remove", group_tag_handler, #{action => remove}},
                {"/v1/group/tag/list", group_tag_handler, #{action => list}},
                {"/v1/group/tag/search", group_tag_handler, #{action => search}},
                {"/v1/group/tag/hot", group_tag_handler, #{action => hot}},
                % 群组公告
                {"/v1/group_notice/add", group_notice_handler, #{action => add}},
                {"/v1/group_notice/edit", group_notice_handler, #{action => edit}},
                {"/v1/group_notice/delete", group_notice_handler, #{action => delete}},
                {"/v1/group_notice/page", group_notice_handler, #{action => page}},
                {"/v1/group_notice/publish", group_notice_handler, #{action => publish}},
                {"/v1/group_notice/latest", group_notice_handler, #{action => latest}},
                {"/v1/group/notice/list", group_notice_handler, #{action => list}},
                {"/v1/group/notice/detail", group_notice_handler, #{action => detail}},
                {"/v1/group/notice/pin", group_notice_handler, #{action => pin}},
                {"/v1/group/notice/unpin", group_notice_handler, #{action => unpin}},
                {"/v1/group/notice/mark_read", group_notice_handler, #{action => mark_read}},
                % 群投票 API
                {"/v1/group/vote/create", group_vote_handler, #{action => create}},
                {"/v1/group/vote/list", group_vote_handler, #{action => list}},
                {"/v1/group/vote/detail", group_vote_handler, #{action => detail}},
                {"/v1/group/vote/cast", group_vote_handler, #{action => cast}},
                {"/v1/group/vote/update", group_vote_handler, #{action => update}},
                {"/v1/group/vote/cancel", group_vote_handler, #{action => cancel}},
                {"/v1/group/vote/close", group_vote_handler, #{action => close}},
                {"/v1/group/vote/my_vote", group_vote_handler, #{action => my_vote}},
                % 群组日程 API
                {"/v1/group_schedule/create", group_schedule_handler, #{action => create}},
                {"/v1/group_schedule/update", group_schedule_handler, #{action => update}},
                {"/v1/group_schedule/cancel", group_schedule_handler, #{action => cancel}},
                {"/v1/group_schedule/detail", group_schedule_handler, #{action => detail}},
                {"/v1/group_schedule/list", group_schedule_handler, #{action => list}},
                {"/v1/group_schedule/my_list", group_schedule_handler, #{action => my_list}},
                {"/v1/group_schedule/confirm", group_schedule_handler, #{action => confirm}},
                % @提及功能 API
                {"/v1/mention/list", mention_handler, #{action => list}},
                {"/v1/mention/unread", mention_handler, #{action => unread}},
                {"/v1/mention/mark_read", mention_handler, #{action => mark_read}},
                {"/v1/mention/suggest", mention_handler, #{action => suggest}},
                % 群相册 API
                {"/v1/group_album/create", group_album_handler, #{action => create_album}},
                {"/v1/group_album/list", group_album_handler, #{action => list_albums}},
                {"/v1/group_album/rename", group_album_handler, #{action => rename_album}},
                {"/v1/group_album/delete", group_album_handler, #{action => delete_album}},
                {"/v1/group_album/photo/upload", group_album_handler, #{action => upload_photo}},
                {"/v1/group_album/photo/batch", group_album_handler, #{action => batch_upload}},
                {"/v1/group_album/photo/list", group_album_handler, #{action => list_photos}},
                {"/v1/group_album/photo/detail", group_album_handler, #{action => photo_detail}},
                {"/v1/group_album/photo/delete", group_album_handler, #{action => delete_photo}},
                {"/v1/group_album/photo/like", group_album_handler, #{action => like_photo}},
                {"/v1/group_album/photo/unlike", group_album_handler, #{action => unlike_photo}},
                {"/v1/group_album/photo/comment", group_album_handler, #{action => add_comment}},
                {"/v1/group_album/photo/comments", group_album_handler, #{action => list_comments}},
                {"/v1/group_album/cover/update", group_album_handler, #{action => update_cover}},

                % 频道功能 API
                {"/v1/channel/create", channel_handler, #{action => create}},
                {"/v1/channel/:channel_id", channel_handler, #{action => show}},
                {"/v1/channel/by_custom_id/:custom_id", channel_handler, #{action => by_custom_id}},
                {"/v1/channel/:channel_id/update", channel_handler, #{action => update}},
                {"/v1/channel/:channel_id/delete", channel_handler, #{action => delete}},
                {"/v1/channel/:channel_id/subscribe", channel_handler, #{action => subscribe}},
                {"/v1/channel/:channel_id/unsubscribe", channel_handler, #{action => unsubscribe}},
                {"/v1/channels/subscribed", channel_handler, #{action => subscribed}},
                {"/v1/channels/managed", channel_handler, #{action => managed}},
                {"/v1/channels/unread/summary", channel_handler, #{action => unread_summary}},
                {"/v1/channel/:channel_id/message", channel_handler, #{action => publish_message}},
                {"/v1/channel/:channel_id/messages", channel_handler, #{action => messages}},
                {"/v1/channel/:channel_id/read", channel_handler, #{action => mark_read}},
                {"/v1/channels/search", channel_handler, #{action => search}},
                {"/v1/channels/discover", channel_handler, #{action => discover}},
                {"/v1/channel/:channel_id/admin", channel_handler, #{action => add_admin}},
                {"/v1/channel/:channel_id/admins", channel_handler_admin, #{action => admins}},
                {"/v1/channel/:channel_id/admin/:user_id/role", channel_handler_admin, #{
                    action => update_admin_role
                }},
                {"/v1/channel/:channel_id/admin/:user_id", channel_handler_admin, #{
                    action => remove_admin
                }},
                % 频道统计 API
                {"/v1/channel/:channel_id/stats", channel_handler, #{action => stats}},
                {"/v1/channel/:channel_id/stats/daily", channel_handler, #{action => stats_daily}},
                {"/v1/channel/:channel_id/message/:message_id/view", channel_handler_message, #{
                    action => record_view
                }},
                {"/v1/channel/:channel_id/message/:message_id/reaction", channel_handler_message, #{
                    action => add_reaction
                }},
                {"/v1/channel/:channel_id/message/:message_id/reaction/:reaction_type",
                    channel_handler_message, #{action => remove_reaction}},
                % 消息管理 API
                {"/v1/channel/:channel_id/message/:message_id/pin", channel_handler_message, #{
                    action => pin_message
                }},
                {"/v1/channel/:channel_id/message/:message_id/delete", channel_handler_message, #{
                    action => delete_message
                }},
                {"/v1/channel/:channel_id/message/:message_id/revoke", channel_handler_message, #{
                    action => revoke_message
                }},
                % 订阅者管理 API
                {"/v1/channel/:channel_id/subscribers", channel_handler_message, #{action => subscribers}},
                {"/v1/channel/:channel_id/subscriber/:user_id", channel_handler_admin, #{
                    action => remove_subscriber
                }},
                % 邀请相关 API（私有频道）
                {"/v1/channel/:channel_id/invitation", channel_handler_admin, #{
                    action => create_invitation
                }},
                {"/v1/channel/invitation/accept", channel_handler_admin, #{action => accept_invitation}},
                {"/v1/channel/invitation/reject", channel_handler_admin, #{action => reject_invitation}},
                {"/v1/channel/invitations/my", channel_handler_admin, #{action => my_invitations}},
                {"/v1/channel/invitations/sent", channel_handler_admin, #{action => sent_invitations}},
                % 订单相关 API（付费频道）
                {"/v1/channel/:channel_id/order", channel_handler_order, #{action => create_order}},
                {"/v1/channel/order/pay", channel_handler_order, #{action => pay_order}},
                {"/v1/channel/orders/my", channel_handler_order, #{action => my_orders}},
                {"/v1/channel/order/:order_no", channel_handler_order, #{action => get_order}},
                {"/v1/channels/sync", channel_handler_admin, #{action => sync}},

                % 群文件管理 API
                {"/v1/group/file/upload", group_file_handler, #{action => upload}},
                {"/v1/group/file/download", group_file_handler, #{action => download}},
                {"/v1/group/file/list", group_file_handler, #{action => list}},
                {"/v1/group/file/delete", group_file_handler, #{action => delete}},
                {"/v1/group/file/search", group_file_handler, #{action => search}},
                {"/v1/group/file/categories", group_file_handler, #{action => categories}},

                % 群作业管理 API
                {"/v1/group/task/create", group_task_handler, #{action => create}},
                {"/v1/group/task/update", group_task_handler, #{action => update}},
                {"/v1/group/task/assign", group_task_handler, #{action => assign}},
                {"/v1/group/task/submit", group_task_handler, #{action => submit}},
                {"/v1/group/task/review", group_task_handler, #{action => review}},
                {"/v1/group/task/list", group_task_handler, #{action => list}},
                {"/v1/group/task/detail", group_task_handler, #{action => detail}},
                {"/v1/group/task/my", group_task_handler, #{action => my_tasks}},
                {"/v1/group/task/pending", group_task_handler, #{action => pending_review}},

                {"/v1/report/create", report_handler, #{action => create}},
                {"/v1/moment/report/create", report_handler, #{action => moment_create}},
                {"/v1/moment/create", moment_handler, #{action => create}},
                {"/v1/moment/:moment_id", moment_handler, #{action => show}},
                {"/v1/moment/:moment_id/delete", moment_handler, #{action => delete}},
                {"/v1/moments/feed", moment_handler, #{action => feed}},
                {"/v1/moments/user/:uid", moment_handler, #{action => user_posts}},
                {"/v1/moment/:moment_id/like", moment_handler, #{action => like}},
                {"/v1/moment/:moment_id/unlike", moment_handler, #{action => unlike}},
                {"/v1/moment/:moment_id/comment", moment_handler, #{action => add_comment}},
                {"/v1/moment/:moment_id/comments", moment_handler, #{action => comments}},
                {"/v1/moment/:moment_id/comment/:comment_id/delete", moment_handler, #{
                    action => delete_comment
                }},
                {"/v1/moment/:moment_id/report", moment_handler, #{action => report}},

                % 直播间 API
                {"/v1/live_room/list", live_room_handler, #{action => list}},
                {"/v1/live_room/my_list", live_room_handler, #{action => my_list}},
                {"/v1/live_room/create", live_room_handler, #{action => create}},
                {"/v1/live_room/start", live_room_handler, #{action => start}},
                {"/v1/live_room/stop", live_room_handler, #{action => stop}},
                {"/v1/live_room/detail", live_room_handler, #{action => detail}},

                % 钱包 API
                {"/v1/wallet/balance", wallet_handler, #{action => balance}},
                {"/v1/wallet/transactions", wallet_handler, #{action => transactions}},
                {"/v1/wallet/topup", wallet_handler, #{action => topup}},

                % 附件 presign API（需 JWT 认证）
                {"/v1/attachment/presign", attach_handler, #{action => presign}},
                % 附件直传成功回调落库（需 JWT 认证）
                {"/v1/attachment/confirm", attach_handler, #{action => confirm}},
                % 附件下载短时签发 URL，替代 bucket 公开读（需 JWT 认证）
                {"/v1/attachment/view_url", attach_handler, #{action => view_url}}
            ],

    % Admin routes (原 imadm)
    AdmRoutes = [
        {"/adm", adm_index_handler, #{action => index}},
        {"/adm/index", adm_index_handler, #{action => index}},
        % 首启初始化向导（P0-5）— 免鉴权，见 open/0
        {"/adm/setup/status", adm_setup_handler, #{action => status}},
        {"/adm/setup/init", adm_setup_handler, #{action => init_setup}},
        {"/adm/current", adm_index_handler, #{action => current}},
        {"/adm/rbac/me", adm_index_handler, #{action => rbac}},
        {"/adm/welcome", adm_index_handler, #{action => welcome}},
        {"/adm/feedback/index", adm_feedback_handler, #{action => index}},
        {"/adm/admin/config/features", adm_admin_handler, #{action => config_features}},
        {"/adm/admin/config/policy/bootstrap", adm_admin_handler, #{
            action => config_policy_bootstrap
        }},
        {"/adm/admin/config/policy/meta", adm_admin_handler, #{action => config_policy_meta}},
        {"/adm/admin/config/policy/preview", adm_admin_handler, #{action => config_policy_preview}},
        {"/adm/admin/config/policy/saved", adm_admin_handler, #{action => config_policy_saved}},
        {"/adm/admin/config/policy", adm_admin_handler, #{action => config_policy}},
        % 禁言用户管理 API
        {"/adm/admin/muted_users/list", adm_admin_handler, #{action => muted_users_list}},
        {"/adm/admin/muted_users/unmute", adm_admin_handler, #{action => muted_users_unmute}},
        {"/adm/admin/muted_users/unmute_batch", adm_admin_handler, #{
            action => muted_users_unmute_batch
        }},
        % 推送 Token 管理 API
        {"/adm/admin/push_token/list", adm_admin_handler, #{action => push_token_list}},
        % 合规密钥管理 API
        {"/adm/admin/compliance_key/list", adm_admin_handler, #{action => compliance_key_list}},
        {"/adm/admin/compliance_key/create", adm_admin_handler, #{action => compliance_key_create}},
        {"/adm/admin/compliance_key/revoke", adm_admin_handler, #{action => compliance_key_revoke}},
        {"/adm/admin/list", adm_admin_handler, #{action => list}},
        {"/adm/admin/create", adm_admin_handler, #{action => create}},
        {"/adm/admin/assign_role", adm_admin_handler, #{action => assign_role}},
        {"/adm/admin/disable", adm_admin_handler, #{action => disable}},
        {"/adm/feedback/reply", adm_feedback_handler, #{action => reply}},
        {"/adm/feedback/delete", adm_feedback_handler, #{action => delete}},
        {"/adm/role/list", adm_role_handler, #{action => list}},
        {"/adm/roles/list", adm_role_handler, #{action => list}},
        {"/adm/role/create", adm_role_handler, #{action => create}},
        {"/adm/roles/create", adm_role_handler, #{action => create}},
        {"/adm/role/permissions/save", adm_role_handler, #{action => permissions_save}},
        {"/adm/role/permission/update", adm_role_handler, #{action => permissions_save}},
        {"/adm/roles/permissions/save", adm_role_handler, #{action => permissions_save}},
        {"/adm/app_ddl/index", adm_app_ddl_handler, #{action => index}},
        {"/adm/app_ddl/save", adm_app_ddl_handler, #{action => save}},
        {"/adm/app_ddl/delete", adm_app_ddl_handler, #{action => delete}},
        {"/adm/app_version/index", adm_app_version_handler, #{action => index}},
        {"/adm/app_version/save", adm_app_version_handler, #{action => save}},
        {"/adm/app_version/delete", adm_app_version_handler, #{action => delete}},
        {"/adm/app_version/version_stats", adm_app_version_handler, #{action => version_stats}},
        {"/adm/attach/auth", adm_attach_handler, #{action => auth}},
        % 存储管理 API
        {"/adm/storage/stats", adm_attach_handler, #{action => stats}},
        {"/adm/storage/index", adm_attach_handler, #{action => index}},
        {"/adm/storage/disable", adm_attach_handler, #{action => disable}},
        {"/adm/storage/enable", adm_attach_handler, #{action => enable}},
        {"/adm/storage/delete", adm_attach_handler, #{action => delete}},
        {"/adm/storage/orphan", adm_attach_handler, #{action => orphan}},
        {"/adm/storage/orphan/cleanup", adm_attach_handler, #{action => orphan_cleanup}},
        {"/adm/passport/meta", adm_passport_handler, #{action => meta}},
        {"/adm/passport/login", adm_passport_handler, #{action => login}},
        {"/adm/passport/captcha", adm_passport_handler, #{action => captcha}},
        {"/adm/passport/do_login", adm_passport_handler, #{action => do_login}},
        {"/adm/passport/logout", adm_passport_handler, #{action => logout}},
        % 用户管理 API
        {"/adm/user/list", adm_user_handler, #{action => list}},
        {"/adm/user/detail", adm_user_handler, #{action => detail}},
        {"/adm/user/ban", adm_user_handler, #{action => ban}},
        {"/adm/user/unban", adm_user_handler, #{action => unban}},
        {"/adm/user/search", adm_user_handler, #{action => search}},
        {"/adm/user/tag/list", adm_user_handler, #{action => tag_list}},
        {"/adm/user/tag/delete", adm_user_handler, #{action => tag_delete}},
        {"/adm/user/collect/list", adm_user_handler, #{action => collect_list}},
        {"/adm/user/collect/remove", adm_user_handler, #{action => collect_remove}},
        {"/adm/user/logout_apply/list", adm_logout_apply_handler, #{action => list}},
        {"/adm/user/logout_apply/export", adm_logout_apply_handler, #{action => export}},
        {"/adm/user/logout_apply/reject", adm_logout_apply_handler, #{action => reject}},
        {"/adm/user/logout_apply/approve", adm_logout_apply_handler, #{action => approve}},
        % 群组管理 API
        {"/adm/group/list", adm_group_handler, #{action => list}},
        {"/adm/group/detail", adm_group_handler, #{action => detail}},
        {"/adm/group/dissolve", adm_group_handler, #{action => dissolve}},
        {"/adm/group/update", adm_group_handler, #{action => update}},
        {"/adm/group/search", adm_group_handler, #{action => search}},
        {"/adm/group/member/kick", adm_group_handler, #{action => kick_member}},
        {"/adm/group/members", adm_group_handler, #{action => members}},
        % 群组子功能 API
        {"/adm/group/vote/list", adm_group_vote_handler, #{action => vote_list}},
        {"/adm/group/vote/detail", adm_group_vote_handler, #{action => vote_detail}},
        {"/adm/group/vote/close", adm_group_vote_handler, #{action => vote_close}},
        {"/adm/group/notice/list", adm_group_notice_handler, #{action => notice_list}},
        {"/adm/group/notice/detail", adm_group_notice_handler, #{action => notice_detail}},
        {"/adm/group/notice/delete", adm_group_notice_handler, #{action => notice_delete}},
        {"/adm/group/tag/list", adm_group_content_handler, #{action => tag_list}},
        {"/adm/group/tag/delete", adm_group_content_handler, #{action => tag_delete}},
        {"/adm/group/category/list", adm_group_content_handler, #{action => category_list}},
        {"/adm/group/category/delete", adm_group_content_handler, #{action => category_delete}},
        {"/adm/group/file/list", adm_group_content_handler, #{action => file_list}},
        {"/adm/group/file/detail", adm_group_content_handler, #{action => file_detail}},
        {"/adm/group/file/delete", adm_group_content_handler, #{action => file_delete}},
        {"/adm/group/album/list", adm_group_content_handler, #{action => album_list}},
        {"/adm/group/album/detail", adm_group_content_handler, #{action => album_detail}},
        {"/adm/group/album/delete", adm_group_content_handler, #{action => album_delete}},
        {"/adm/group/schedule/list", adm_group_schedule_handler, #{action => schedule_list}},
        {"/adm/group/schedule/detail", adm_group_schedule_handler, #{action => schedule_detail}},
        {"/adm/group/schedule/cancel", adm_group_schedule_handler, #{action => schedule_cancel}},
        {"/adm/group/schedule/restore", adm_group_schedule_handler, #{action => schedule_restore}},
        {"/adm/group/governance_log/list", adm_group_schedule_handler, #{action => governance_log_list}},
        {"/adm/group/task/list", adm_group_task_handler, #{action => task_list}},
        {"/adm/group/task/detail", adm_group_task_handler, #{action => task_detail}},
        {"/adm/group/task/pending_review", adm_group_task_handler, #{action => task_pending_review}},
        {"/adm/group/task/review", adm_group_task_handler, #{action => task_review}},
        {"/adm/group/task/restore", adm_group_task_handler, #{action => task_restore}},
        {"/adm/group/task/close", adm_group_task_handler, #{action => task_close}},
        {"/adm/group/task/delete", adm_group_task_handler, #{action => task_delete}},
        % 消息管理 API
        {"/adm/message/list", adm_message_handler, #{action => list}},
        {"/adm/message/detail", adm_message_handler, #{action => detail}},
        {"/adm/message/export", adm_message_handler, #{action => export}},
        % 频道管理 API
        {"/adm/channel/list", adm_channel_handler, #{action => list}},
        {"/adm/channel/detail/:channel_id", adm_channel_handler, #{action => detail}},
        {"/adm/channel/:channel_id/messages", adm_channel_handler, #{action => messages}},
        {"/adm/channel/:channel_id/subscribers", adm_channel_handler, #{action => subscribers}},
        {"/adm/channel/:channel_id/subscriber/:user_id", adm_channel_handler, #{
            action => remove_subscriber
        }},
        {"/adm/channel/:channel_id/admins", adm_channel_handler, #{action => admins}},
        {"/adm/channel/:channel_id/admin/:user_id/role", adm_channel_handler, #{
            action => update_admin_role
        }},
        {"/adm/channel/:channel_id/admin/:user_id", adm_channel_handler, #{action => remove_admin}},
        {"/adm/channel/:channel_id/invitations", adm_channel_handler, #{action => invitations}},
        {"/adm/channel/:channel_id/orders", adm_channel_handler, #{action => orders}},
        {"/adm/channel/:channel_id/stats", adm_channel_handler, #{action => stats}},
        {"/adm/channel/:channel_id/message/:message_id/pin", adm_channel_handler, #{
            action => pin_message
        }},
        {"/adm/channel/:channel_id/message/:message_id/delete", adm_channel_handler, #{
            action => delete_message
        }},
        {"/adm/channel/search", adm_channel_handler, #{action => search}},
        {"/adm/channel/delete", adm_channel_handler, #{action => delete}},
        % Moment 与举报治理 API
        {"/adm/moment/list", adm_moment_handler, #{action => list}},
        {"/adm/moment/detail/:moment_id", adm_moment_handler, #{action => detail}},
        {"/adm/moment/delete", adm_moment_handler, #{action => delete}},
        {"/adm/moment/report/list", adm_moment_handler, #{action => report_list}},
        {"/adm/moment/report/resolve", adm_moment_handler, #{action => report_resolve}},
        {"/adm/moment/report/batch_resolve", adm_moment_handler, #{action => report_batch_resolve}},
        {"/adm/report/create", adm_report_handler, #{action => create}},
        {"/adm/report/list", adm_report_handler, #{action => list}},
        {"/adm/report/resolve", adm_report_handler, #{action => resolve}},
        {"/adm/report/batch_resolve", adm_report_handler, #{action => batch_resolve}},
        {"/adm/group/report/list", adm_report_handler, #{action => group_list}},
        {"/adm/group/report/resolve", adm_report_handler, #{action => group_resolve}},
        {"/adm/group/report/batch_resolve", adm_report_handler, #{action => group_batch_resolve}},
        {"/adm/channel/report/list", adm_report_handler, #{action => channel_list}},
        {"/adm/channel/report/resolve", adm_report_handler, #{action => channel_resolve}},
        {"/adm/channel/report/batch_resolve", adm_report_handler, #{
            action => channel_batch_resolve
        }},
        {"/adm/user/report/list", adm_report_handler, #{action => user_list}},
        {"/adm/user/report/resolve", adm_report_handler, #{action => user_resolve}},
        {"/adm/user/report/batch_resolve", adm_report_handler, #{action => user_batch_resolve}},
        % 统计 API
        {"/adm/announcement/index", adm_announcement_handler, #{action => index}},
        {"/adm/announcement/create", adm_announcement_handler, #{action => create}},
        {"/adm/announcement/update", adm_announcement_handler, #{action => update}},
        {"/adm/announcement/delete", adm_announcement_handler, #{action => delete}},
        {"/adm/announcement/publish", adm_announcement_handler, #{action => publish}},
        {"/adm/announcement/unpublish", adm_announcement_handler, #{action => unpublish}},
        % 插件生命周期管理 API (lifecycle.md §10)
        {"/adm/plugin/list", adm_plugin_handler, #{action => list}},
        {"/adm/plugin/detail", adm_plugin_handler, #{action => detail}},
        {"/adm/plugin/state", adm_plugin_handler, #{action => state_query}},
        {"/adm/plugin/health", adm_plugin_handler, #{action => health}},
        {"/adm/plugin/install", adm_plugin_handler, #{action => install}},
        {"/adm/plugin/enable", adm_plugin_handler, #{action => enable}},
        {"/adm/plugin/disable", adm_plugin_handler, #{action => disable}},
        {"/adm/plugin/upgrade", adm_plugin_handler, #{action => upgrade}},
        {"/adm/plugin/uninstall", adm_plugin_handler, #{action => uninstall}},
        {"/adm/plugin/reset", adm_plugin_handler, #{action => reset}},
        {"/adm/plugin/force_uninstall", adm_plugin_handler, #{action => force_uninstall}},
        {"/adm/plugin/logs", adm_plugin_handler, #{action => logs}},
        {"/adm/stats/overview", adm_stats_handler, #{action => overview}},
        {"/adm/stats/user", adm_stats_handler, #{action => user}},
        {"/adm/stats/message", adm_stats_handler, #{action => message}},
        {"/adm/stats/group", adm_stats_handler, #{action => group}},
        {"/adm/stats/ranking", adm_stats_handler, #{action => ranking}},
        {"/static/admin/[...]", cowboy_static,
            {priv_dir, imboy, "static/admin", [{mimetypes, cow_mimetypes, all}]}}
    ],
    [{Host, MainRoutes ++ ApiV1Routes ++ AdmRoutes ++ plugin_routes()}].

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
        <<"/feedback/add">>,
        <<"/app_version/check">>,

        % 没有登录也可以提交反馈建议
        <<"/v1/feedback/add">>,
        <<"/v1/app_version/check">>,
        <<"/v1/app_upgrade/report">>
    ].

%% @doc 不需要认证的API
%% 列表元素必须为binary
%% auth_middleware 去除了path 最后的斜杆，所以不用以 / 结尾了
-spec open() -> [binary()].
open() ->
    [
        <<"/help">>,
        <<"/privacy-policy">>,
        <<"/account-deletion">>,
        % /ws 有自己的auth
        <<"/ws">>,
        <<"/conversation/online">>,
        <<"/init">>,
        <<"/user/show">>,
        <<"/refreshtoken">>,
        <<"/passport/login">>,
        <<"/passport/quick_login">>,
        <<"/passport/signup">>,
        <<"/passport/getcode">>,
        <<"/passport/findpassword">>,
        <<"/passport/bind_mail">>,
        <<"/auth/assets">>,

        <<"/v1/ws">>,
        <<"/v1/conversation/online">>,
        <<"/v1/init">>,
        <<"/v1/app/features">>,
        <<"/v1/app/manifest">>,
        <<"/v1/app/policy">>,
        <<"/v1/user/show">>,
        <<"/v1/refreshtoken">>,
        <<"/v1/passport/login">>,
        <<"/v1/passport/quick_login">>,
        <<"/v1/passport/signup">>,
        <<"/v1/passport/getcode">>,
        <<"/v1/passport/findpassword">>,
        <<"/v1/passport/bind_mail">>,
        <<"/v1/passport/qr_login/create">>,
        <<"/v1/passport/qr_login/status">>,
        <<"/v1/passport/qr_login/scan">>,
        <<"/v1/passport/qr_login/confirm">>,
        <<"/v1/passport/qr_login/cancel">>,
        %% PR-3β: SSE 端点免登录（EventSource 在握手完成前没有 token）
        <<"/v1/passport/qr_login/subscribe">>,
        <<"/v1/auth/assets">>,

        <<"/metrics">>,
        <<"/v1/metrics">>,

        %% 首启初始化向导（P0-5）— 部署后首次访问必须免鉴权
        <<"/adm/setup/status">>,
        <<"/adm/setup/init">>,

        <<"/">>
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

%% @doc 测试路由（v0）- 仅非生产环境
-spec test_routes() -> list().
test_routes() ->
    case is_dev_env() of
        true ->
            [
                {"/test/req_get", test_handler, #{action => req_get}},
                {"/test/req_post", test_handler, #{action => req_post}}
            ];
        false ->
            []
    end.

%% @doc 测试路由（v1）- 仅非生产环境
-spec test_routes_v1() -> list().
test_routes_v1() ->
    case is_dev_env() of
        true ->
            [
                {"/v1/test/req_get", test_handler, #{action => req_get}},
                {"/v1/test/req_post", test_handler, #{action => req_post}}
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
                <<"/test/req_get">>,
                <<"/test/req_post">>,
                <<"/v1/test/req_get">>,
                <<"/v1/test/req_post">>
            ];
        false ->
            []
    end.
