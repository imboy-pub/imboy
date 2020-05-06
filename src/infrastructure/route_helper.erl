-module(route_helper).

-export([get_routes/0, need_auth_paths/0]).

get_routes() ->
    [
        %% {URIHost, list({URIPath, Handler, Opts})}
        %% {'_', [{'_', my_handler, []}]}
        {'_', [
            {"/", init_handler, [{action, help}]}
            % , {"/passport/login.html", dtl_handler, [login]}

            , {"/help", init_handler, [{action, help}]}
            , {"/init", init_handler, [{action, init}]}
            , {"/refreshtoken", init_handler, [{action, refreshtoken}]}
            , {"/passport/login", passport_handler, [{action, do_login}]}

            , {"/chat/websocket", websocket_handler, []}

            , {"/chat/myfriend", chat_handler, [{action, myfriend}]}
            , {"/chat/msgbox", chat_handler, [{action, chat_msgbox}]}
            , {"/friend/find", friend_handler, [{action, find}]}

            , {"/friend/find.html", cowboy_static, {priv_file, imboy, "templates/web-chat/find.html"}}
            , {"/chat", cowboy_static, {priv_file, imboy, "templates/web-chat/index.html"}}
            , {"/chat.html", cowboy_static, {priv_file, imboy, "templates/web-chat/index.html"}}
            , {"/passport/login.html", cowboy_static, {priv_file, imboy, "templates/web-chat/login.html"}}

            , {"/favicon.png", cowboy_static, {priv_file, imboy, "static/favicon.png"}}
            , {"/static/[...]", cowboy_static, {priv_dir, imboy, "static", [{mimetypes, cow_mimetypes, all}]}}
        ]}
    ].

%% 需要认证的API，列表元素必须为binary
%% <<"/refreshtoken">> 请不要加入 auth
need_auth_paths() ->
    [
        <<"/chat/myfriend">>
        , <<"/friend/find">>
    ].
