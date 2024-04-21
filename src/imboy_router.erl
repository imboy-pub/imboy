-module(imboy_router).

-export([get_routes/0]).
-export([open/0]).
-export([option/0]).


%% 服务端所有路由定义再这里(包含 option/0 open/0 里面的定义)
get_routes() ->
    [{config_ds:env(host),
      [{"/", index_handler, #{action => help}},
       {"/help", index_handler, #{action => help}},
       {"/init", index_handler, #{action => init}},
       {"/refreshtoken", passport_handler, #{action => refreshtoken}},

       {"/app_version/check", app_version_handler, #{action => check}},

       {"/app_ddl/get", app_ddl_handler, #{action => get_ddl}},

       {"/passport/login", passport_handler, #{action => login}},
       {"/passport/signup", passport_handler, #{action => signup}},
       {"/passport/getcode", passport_handler, #{action => getcode}},
       {"/passport/findpassword", passport_handler, #{action => find_password}},

       {"/ws", websocket_handler, #{}},
       {"/stress_testing", stress_testing_ws_handler, #{}},
       {"/auth/assets", auth_handler, #{action => assets}},
       {"/test/req_get", test_handler, #{action => req_get}},
       {"/test/req_post", test_handler, #{action => req_post}},

       {"/conversation/online", conversation_handler, #{action => online}},
       {"/conversation/mine", conversation_handler, #{action => mine}},
       {"/conversation/msgbox", conversation_handler, #{action => msgbox}},

       {"/uqrcode", user_handler, #{action => uqrcode}},
       {"/user/update", user_handler, #{action => update}},
       {"/user/show", user_handler, #{action => show}},
       {"/user/change_state", user_handler, #{action => change_state}},
       {"/user/setting", user_handler, #{action => setting}},
       {"/user/credential", user_handler, #{action => credential}},
       {"/user/cancel", user_handler, #{action => cancel}},

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

       % 搜索“用户允许被搜索”的用户
       {"/fts/user_search", fts_handler, #{action => user_search}},
       % 最近新注册的并且允许被搜索到的朋友
       {"/fts/recently_user", fts_handler, #{action => recently_user}},

       {"/group/face2face", group_handler, #{action => face2face}},
       {"/group/face2face_save", group_handler, #{action => face2face_save}},
       {"/group/add", group_handler, #{action => add}},
       {"/group/edit", group_handler, #{action => edit}},
       {"/group/dissolve", group_handler, #{action => dissolve}},
       {"/group/page", group_handler, #{action => page}},
       {"/group/msg_page", group_handler, #{action => msg_page}},

       {"/group_member/join", group_member_handler, #{action => join}},
       {"/group_member/leave", group_member_handler, #{action => leave}},
       {"/group_member/page", group_member_handler, #{action => page}},
       {"/group_member/alias", group_member_handler, #{action => alias}},
       % 群组公告
       {"/group_notice/add", group_notice_handler, #{action => add}},
       {"/group_notice/edit", group_notice_handler, #{action => edit}},
       {"/group_notice/delete", group_notice_handler, #{action => delete}},
       {"/group_notice/page", group_notice_handler, #{action => page}},
       {"/group_notice/publish", group_notice_handler, #{action => publish}},
       {"/group_notice/latest", group_notice_handler, #{action => latest}},

       %
       {"/live_room/[:stream_id]", live_room_stream_handler, #{}},
       {"/whip/publish/[:room_id]/[:stream_id]", whip_handler, #{action => publish}},
       {"/whip/[:room_id]/[:stream_id]", whip_handler, #{action => check}},

       {"/whip/unpublish/[:room_id]/[:stream_id]", whip_handler, #{action => unpublish}},
       {"/whip/subscribe/[:room_id]/[:stream_id]", whip_handler, #{action => subscribe}},
       {"/whip/unsubscribe/[:room_id]/[:stream_id]", whip_handler, #{action => unsubscribe}},

       %%%%%%% 上面写API路由，下面写静态资源 %%%%%%%%

       {"/static/[...]", cowboy_static, {priv_dir, imboy, "static", [{mimetypes, cow_mimetypes, all}]}}]}].


%% 因为 除去 option 和 open 的路由，就是必须要 auth 的路由了
%% 所以 这里不需要定义 auth/0 方法

%% 如果请求头里面有 authorization 字段，就需要认证的API
%% 列表元素必须为binary
%% auth_middleware 去除了path 最后的斜杆，所以不用以 / 结尾了
option() ->
    [
        <<"/uqrcode">>
        , <<"/feedback/add">> % 没有登录也可以提交反馈建议
        , <<"/app_version/check">>
        , <<"/app_ddl/get">>
    ].


%% 不需要认证的API
%% 列表元素必须为binary
%% auth_middleware 去除了path 最后的斜杆，所以不用以 / 结尾了
open() ->
    [
     % /ws 有自己的auth
     <<"/ws">>,
     <<"/help">>,
     <<"/test/req_get">>,
     <<"/test/req_post">>,
     <<"/conversation/online">>,
     <<"/init">>,
     <<"/user/show">>,
     <<"/refreshtoken">>,
     <<"/stress_testing">>,
     <<"/passport/login">>,
     <<"/passport/signup">>,
     <<"/passport/getcode">>,
     <<"/passport/findpassword">>,
     <<"/auth/assets">>,
     <<"/">>].
