-module(route_helper).

-export([get_routes/0]).
-export([not_need_auth_paths/0]).


get_routes() ->
    {ok, HostImboy} = application:get_env(imboy, host),
    [{HostImboy,
      [{"/", handler_init, [{action, help}]},
       {"/help", handler_init, [{action, help}]},
       {"/init", handler_init, [{action, init}]},
       {"/refreshtoken", handler_passport, [{action, refreshtoken}]},
       {"/passport/login", handler_passport, [{action, do_login}]},
       {"/passport/signup", handler_passport, [{action, do_signup}]},
       {"/passport/getcode", handler_passport, [{action, send_code}]},
       {"/passport/findpassword", handler_passport,
        [{action, find_password}]},
       {"/stress_testing", handler_stress_testing_ws, []},
       {"/ws", handler_websocket, []},
       {"/auth/assets", handler_auth, [{action, assets}]},
       {"/conversation/online",
        handler_conversation,
        [{action, online}]},
       {"/conversation/mine", handler_conversation, [{action, mine}]},
       {"/conversation/msgbox",
        handler_conversation,
        [{action, msgbox}]},
       {"/user/change_state", handler_user, [{action, change_state}]},
       {"/user/change_sign", handler_user, [{action, change_sign}]},
       {"/user/open_info", handler_user, [{action, open_info}]},
       {"/friend/list", handler_friend, [{action, friend_list}]},
       {"/friend/myfriend", handler_friend, [{action, myfriend}]},
       {"/friend/move", handler_friend, [{action, move}]},
       {"/friend/information", handler_friend, [{action, information}]},
       {"/friend/find", handler_friend, [{action, find}]},
       {"/friend/change_remark",
        handler_friend,
        [{action, change_remark}]},
       {"/friend/category/add",
        handler_friend_category,
        [{action, add}]},
       {"/friend/category/delete",
        handler_friend_category,
        [{action, delete}]},
       {"/friend/category/rename",
        handler_friend_category,
        [{action, rename}]},
       {"/group/member", handler_group, [{action, member}]}

       %%%%%%%% 上面写API路由，下面写静态资源 %%%%%%%%
       ,
       {"/friend/find.html",
        cowboy_static,
        {priv_file, imboy, "templates/web-chat/find.html"}}
       % 好友群资料页面
       ,
       {"/friend/information.html",
        cowboy_static,
        {priv_file, imboy,
                    "templates/web-chat/friend_information.html"}},
       {"/chat",
        cowboy_static,
        {priv_file, imboy, "templates/web-chat/index.html"}},
       {"/chat.html",
        cowboy_static,
        {priv_file, imboy, "templates/web-chat/index.html"}},
       {"/passport/login.html",
        cowboy_static,
        {priv_file, imboy, "templates/web-chat/login.html"}},
       {"/assets/images/def_avatar.png",
        cowboy_static,
        {priv_file, imboy, "static/image/def_avatar.png"}},
       {"/favicon.ico",
        cowboy_static,
        {priv_file, imboy, "static/favicon.ico"}},
       {"/static/[...]",
        cowboy_static,
        {priv_dir, imboy,
                   "static",
                   [{mimetypes, cow_mimetypes, all}]}}]}].


%% 不需要认证的API，列表元素必须为binary
not_need_auth_paths() ->
    [<<"/">>
     % /ws 有自己的auth
     ,
     <<"/ws">>,
     <<"/ws/">>,
     <<"/help">>,
     <<"/conversation/online">>,
     <<"/init">>,
     <<"/refreshtoken">>,
     <<"/stress_testing">>,
     <<"/passport/login">>,
     <<"/passport/signup">>,
     <<"/passport/getcode">>,
     <<"/passport/findpassword">>,
     <<"/auth/assets">>].
