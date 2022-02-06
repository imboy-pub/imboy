-module(route_helper).

-export([get_routes/0]).
-export([need_auth_paths/0]).

get_routes() ->
    % {ok, HostImboy} = application:get_env(imboy, host),
    [
        % {HostImboy, [
        {'_', [
            {"/", init_handler, [{action, help}]}

            , {"/help", init_handler, [{action, help}]}
            , {"/init", init_handler, [{action, init}]}
            , {"/refreshtoken", passport_handler, [{action, refreshtoken}]}
            , {"/passport/login", passport_handler, [{action, do_login}]}

            , {"/stress_testing", stress_testing_ws_handler, []}
            % 专门为浏览器提供的websocket API
            , {"/websocket", websocket_handler, []}
            % 专门为APP提供的websocket API
            , {"/ws", websocket_handler, []}

            , {"/conversation/online", conversation_handler, [{action, online}]}
            , {"/conversation/mine", conversation_handler, [{action, mine}]}
            , {"/conversation/msgbox", conversation_handler, [{action, msgbox}]}

            , {"/user/change_state", user_handler, [{action, change_state}]}
            , {"/user/change_sign", user_handler, [{action, change_sign}]}
            , {"/user/open_info", user_handler, [{action, open_info}]}

            , {"/friend/list", friend_handler, [{action, friend_list}]}
            , {"/friend/myfriend", friend_handler, [{action, myfriend}]}
            , {"/friend/move", friend_handler, [{action, move}]}
            , {"/friend/information", friend_handler, [{action, information}]}
            , {"/friend/find", friend_handler, [{action, find}]}
            , {"/friend/change_remark", friend_handler, [{action, change_remark}]}

            , {"/friend/category/add", friend_category_handler, [{action, add}]}
            , {"/friend/category/delete", friend_category_handler, [{action, delete}]}
            , {"/friend/category/rename", friend_category_handler, [{action, rename}]}

            , {"/group/member", group_handler, [{action, member}]}

            %%%%%%%% 上面写API路由，下面写静态资源 %%%%%%%%
            , {"/friend/find.html", cowboy_static, {priv_file, imboy, "templates/web-chat/find.html"}}
            % 好友群资料页面
            , {"/friend/information.html", cowboy_static, {priv_file, imboy, "templates/web-chat/friend_information.html"}}
            , {"/chat", cowboy_static, {priv_file, imboy, "templates/web-chat/index.html"}}
            , {"/chat.html", cowboy_static, {priv_file, imboy, "templates/web-chat/index.html"}}
            , {"/passport/login.html", cowboy_static, {priv_file, imboy, "templates/web-chat/login.html"}}

            , {"/assets/images/def_avatar.png", cowboy_static, {priv_file, imboy, "static/image/def_avatar.png"}}

            , {"/favicon.ico", cowboy_static, {priv_file, imboy, "static/favicon.ico"}}
            , {"/static/[...]", cowboy_static, {priv_dir, imboy, "static", [{mimetypes, cow_mimetypes, all}]}}
        ]}
    ].

%% 需要认证的API，列表元素必须为binary
%% <<"/refreshtoken">> 请不要加入 auth
need_auth_paths() ->
    [
        <<"/friend/myfriend">>
        , <<"/friend/list">>
        , <<"/friend/find">>
        , <<"/friend/move">>
        , <<"/friend/information">>
        , <<"/friend/change_remark">>

        , <<"/friend/category/add">>
        , <<"/friend/category/delete">>
        , <<"/friend/category/rename">>

        % 我的会话列表
        , <<"/conversation/mine">>

        , <<"/group/member">>

        , <<"/user/change_state">>
        , <<"/user/change_sign">>
    ].
