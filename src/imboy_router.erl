-module(imboy_router).

-export([get_routes/0]).
-export([not_need_auth_paths/0]).


get_routes() ->
    {ok, HostImboy} = application:get_env(imboy, host),
    [{HostImboy, [
        {"/", index_handler, [{action, help}]},
        {"/help", index_handler, [{action, help}]},
        {"/init", index_handler, [{action, init}]},
        {"/refreshtoken", passport_handler, [{action, refreshtoken}]},
        {"/passport/login", passport_handler, [{action, do_login}]},
        {"/passport/signup", passport_handler, [{action, do_signup}]},
        {"/passport/getcode", passport_handler, [{action, send_code}]},
        {"/passport/findpassword", passport_handler, [{action, find_password}]},
        {"/stress_testing", stress_testing_ws_handler, []},
        {"/ws", websocket_handler, []},
        {"/auth/assets", auth_handler, [{action, assets}]},
        {"/conversation/online", conversation_handler, [{action, online}]},
        {"/conversation/mine", conversation_handler, [{action, mine}]},
        {"/conversation/msgbox", conversation_handler, [{action, msgbox}]},

        {"/user/change_state", user_handler, [{action, change_state}]},
        {"/user/update", user_handler, [{action, update}]},
        {"/user/show", user_handler, [{action, open_info}]},

        {"/friend/list", friend_handler, [{action, friend_list}]},
        {"/friend/myfriend", friend_handler, [{action, myfriend}]},
        {"/friend/move", friend_handler, [{action, move}]},
        {"/friend/information", friend_handler, [{action, information}]},
        {"/friend/find", friend_handler, [{action, find}]},
        {"/friend/change_remark", friend_handler, [{action, change_remark}]},
        {"/friend/category/add", friend_category_handler, [{action, add}]},
        {"/friend/category/delete", friend_category_handler, [{action, delete}]},
        {"/friend/category/rename", friend_category_handler, [{action, rename}]},

        {"/group/member", group_handler, [{action, member}]},

        %%%%%%% 上面写API路由，下面写静态资源 %%%%%%%%

        {"/static/[...]", cowboy_static,
            {priv_dir, imboy, "static", [{mimetypes, cow_mimetypes, all}]}
        }
    ]}].


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
     <<"/user/show">>,
     <<"/refreshtoken">>,
     <<"/stress_testing">>,
     <<"/passport/login">>,
     <<"/passport/signup">>,
     <<"/passport/getcode">>,
     <<"/passport/findpassword">>,
     <<"/auth/assets">>].
