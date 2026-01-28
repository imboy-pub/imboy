-module(imboy_router).

-export([get_routes/0]).
-export([open/0]).
-export([option/0]).


%% @doc 获取所有路由定义
-spec get_routes() -> list().
get_routes() ->
    Host = config_ds:env(host, '_'),
    MainRoutes = [
        {"/", index_handler, #{action => help}},
        {"/help", index_handler, #{action => help}},

        % begin v0 为了兼容版本，在下一个版本下面的路由可以删除 2026-01-12
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
        {"/auth/assets", auth_handler, #{action => assets}},
        {"/test/req_get", test_handler, #{action => req_get}},
        {"/test/req_post", test_handler, #{action => req_post}},

        {"/conversation/online", conversation_handler, #{action => online}},
        {"/conversation/mine", conversation_handler, #{action => mine}},
        {"/msg/offline", msg_handler, #{action => offline}},
        {"/msg/offline_ack", msg_handler, #{action => offline_ack}},

        {"/uqrcode", user_handler, #{action => qrcode}},% 2024-05-10 过两个版本可以清除该路由
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

        {"/user_tag_relation/collect_page", user_tag_relation_handler, #{action => collect_page}},
        {"/user_tag_relation/friend_page", user_tag_relation_handler, #{action => friend_page}},
        {"/user_tag_relation/add", user_tag_relation_handler, #{action => add}},
        {"/user_tag_relation/set", user_tag_relation_handler, #{action => set}},
        {"/user_tag_relation/remove", user_tag_relation_handler, #{action => remove}},

        {"/location/makeMyselfVisible", location_handler, #{action => make_myself_visible}},
        {"/location/makeMyselfUnvisible", location_handler, #{action => make_myself_unvisible}},
        {"/location/peopleNearby", location_handler, #{action => people_nearby}},

        {"/friend/add", friend_handler, #{action => add_friend}},
        {"/friend/confirm", friend_handler, #{action => confirm}},
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
        % end v0 为了兼容版本，在下一个版本下面的路由可以删除 2026-01-12

        %%%%%%% 上面写API路由，下面写静态资源 %%%%%%%%

        {"/static/[...]", cowboy_static, {priv_dir, imboy, "static", [{mimetypes, cow_mimetypes, all}]}}
    ],

    % Api v1 routes
    ApiV1Routes = [
        {"/v1/init", index_handler, #{action => init}},
        {"/v1/refreshtoken", passport_handler, #{action => refreshtoken}},
        {"/v1/app_version/check", app_version_handler, #{action => check}},

        % 【新增】Prometheus 指标端点
        {"/v1/metrics", metrics_handler, #{}},

        {"/v1/passport/quick_login", passport_handler, #{action => quick_login}},
        {"/v1/passport/login", passport_handler, #{action => login}},
        {"/v1/passport/signup", passport_handler, #{action => signup}},
        {"/v1/passport/getcode", passport_handler, #{action => getcode}},
        {"/v1/passport/findpassword", passport_handler, #{action => find_password}},
        {"/v1/passport/bind_mail", passport_handler, #{action => bind_mail}},

        {"/v1/ws", websocket_handler, #{}},
        {"/v1/auth/assets", auth_handler, #{action => assets}},
        {"/v1/test/req_get", test_handler, #{action => req_get}},
        {"/v1/test/req_post", test_handler, #{action => req_post}},

        {"/v1/conversation/online", conversation_handler, #{action => online}},
        {"/v1/conversation/mine", conversation_handler, #{action => mine}},
        {"/v1/msg/offline", msg_handler, #{action => offline}},
        {"/v1/msg/offline_ack", msg_handler, #{action => offline_ack}},

        {"/v1/uqrcode", user_handler, #{action => qrcode}},% 2024-05-10 过两个版本可以清除该路由
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
        {"/v1/user/search", user_handler, #{action => search}},

        {"/v1/user_device/page", user_device_handler, #{action => page}},
        {"/v1/user_device/change_name", user_device_handler, #{action => change_name}},
        {"/v1/user_device/delete", user_device_handler, #{action => delete}},

        {"/v1/e2ee/user_keys", e2ee_handler, #{action => user_keys}},
        {"/v1/e2ee/group_member_keys", e2ee_handler, #{action => group_member_keys}},

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

        {"/v1/user_tag_relation/collect_page", user_tag_relation_handler, #{action => collect_page}},
        {"/v1/user_tag_relation/friend_page", user_tag_relation_handler, #{action => friend_page}},
        {"/v1/user_tag_relation/add", user_tag_relation_handler, #{action => add}},
        {"/v1/user_tag_relation/set", user_tag_relation_handler, #{action => set}},
        {"/v1/user_tag_relation/remove", user_tag_relation_handler, #{action => remove}},

        {"/v1/location/makeMyselfVisible", location_handler, #{action => make_myself_visible}},
        {"/v1/location/makeMyselfUnvisible", location_handler, #{action => make_myself_unvisible}},
        {"/v1/location/peopleNearby", location_handler, #{action => people_nearby}},

        {"/v1/friend/add", friend_handler, #{action => add_friend}},
        {"/v1/friend/confirm", friend_handler, #{action => confirm}},
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

        {"/v1/group_member/join", group_member_handler, #{action => join}},
        {"/v1/group_member/leave", group_member_handler, #{action => leave}},
        {"/v1/group_member/page", group_member_handler, #{action => page}},
        {"/v1/group_member/alias", group_member_handler, #{action => alias}},
        {"/v1/group_member/same_group", group_member_handler, #{action => same_group}},
        % 群组公告
        {"/v1/group_notice/add", group_notice_handler, #{action => add}},
        {"/v1/group_notice/edit", group_notice_handler, #{action => edit}},
        {"/v1/group_notice/delete", group_notice_handler, #{action => delete}},
        {"/v1/group_notice/page", group_notice_handler, #{action => page}},
        {"/v1/group_notice/publish", group_notice_handler, #{action => publish}},
        {"/v1/group_notice/latest", group_notice_handler, #{action => latest}}
    ],

    % Admin routes (原 imadm)
    AdmRoutes = [
        {"/adm", adm_index_handler, #{action => index}},
        {"/adm/index", adm_index_handler, #{action => index}},
        {"/adm/welcome", adm_index_handler, #{action => welcome}},
        {"/adm/feedback/index", adm_feedback_handler, #{action => index}},
        {"/adm/feedback/reply", adm_feedback_handler, #{action => reply}},
        {"/adm/app_ddl/index", adm_app_ddl_handler, #{action => index}},
        {"/adm/app_ddl/save", adm_app_ddl_handler, #{action => save}},
        {"/adm/app_ddl/delete", adm_app_ddl_handler, #{action => delete}},
        {"/adm/app_version/index", adm_app_version_handler, #{action => index}},
        {"/adm/app_version/save", adm_app_version_handler, #{action => save}},
        {"/adm/app_version/delete", adm_app_version_handler, #{action => delete}},
        {"/adm/attach/auth", adm_attach_handler, #{action => auth}},
        {"/adm/passport/login", adm_passport_handler, #{action => login}},
        {"/adm/passport/captcha", adm_passport_handler, #{action => captcha}},
        {"/adm/passport/do_login", adm_passport_handler, #{action => do_login}},
        {"/static/admin/[...]", cowboy_static, {priv_dir, imboy, "static/admin", [{mimetypes, cow_mimetypes, all}]}}
    ],
    [{Host, MainRoutes ++ ApiV1Routes ++ AdmRoutes}].


%% 因为 除去 option 和 open 的路由，就是必须要 auth 的路由了
%% 所以 这里不需要定义 auth/0 方法

%% @doc 如果请求头里面有 authorization 字段，就需要认证的API
%% 列表元素必须为binary
%% auth_middleware 去除了path 最后的斜杆，所以不用以 / 结尾了
-spec option() -> [binary()].
option() ->
    [
        <<"/uqrcode">>
        , <<"/feedback/add">> % 没有登录也可以提交反馈建议
        , <<"/app_version/check">>
        , <<"/app_ddl/get">>

        , <<"/v1/uqrcode">>
        , <<"/v1/feedback/add">> % 没有登录也可以提交反馈建议
        , <<"/v1/app_version/check">>
        , <<"/v1/app_ddl/get">>
    ].


%% @doc 不需要认证的API
%% 列表元素必须为binary
%% auth_middleware 去除了path 最后的斜杆，所以不用以 / 结尾了
-spec open() -> [binary()].
open() ->
    [
     <<"/help">>,
     % /ws 有自己的auth
     <<"/ws">>,
     <<"/test/req_get">>,
     <<"/test/req_post">>,
     <<"/conversation/online">>,
     <<"/init">>,
     <<"/user/show">>,
     <<"/refreshtoken">>,
     <<"/stress_testing">>,
     <<"/passport/login">>,
     <<"/passport/quick_login">>,
     <<"/passport/signup">>,
     <<"/passport/getcode">>,
     <<"/passport/findpassword">>,
     <<"/passport/bind_mail">>,
     <<"/auth/assets">>,

     <<"/v1/ws">>,
     <<"/v1/test/req_get">>,
     <<"/v1/test/req_post">>,
     <<"/v1/conversation/online">>,
     <<"/v1/init">>,
     <<"/v1/user/show">>,
     <<"/v1/refreshtoken">>,
     <<"/v1/stress_testing">>,
     <<"/v1/passport/login">>,
     <<"/v1/passport/quick_login">>,
     <<"/v1/passport/signup">>,
     <<"/v1/passport/getcode">>,
     <<"/v1/passport/findpassword">>,
     <<"/v1/passport/bind_mail">>,
     <<"/v1/auth/assets">>,

     <<"/">>].
