-module(route_helper).

-export([get_routes/0, need_auth_paths/0]).

get_routes() ->
    [
        %% {URIHost, list({URIPath, Handler, Opts})}
        %% {'_', [{'_', my_handler, []}]}
        {'_', [
            {"/", init_handler, [help]}
            , {"/passport/login.html", dtl_handler, [login]}
            , {"/help", init_handler, [help]}
            , {"/init", init_handler, [init]}
            , {"/refreshtoken", init_handler, [refreshtoken]}
            , {"/passport/login", passport_handler, [do_login]}
            , {"/chat", cowboy_static, {priv_file, imboy, "static/index.html"}}
            , {"/static/[...]", cowboy_static, {priv_dir, imboy, "static", [{mimetypes, cow_mimetypes, all}]}}
        ]}
    ].

%% 需要认证的API，列表元素必须为binary
%% <<"/refreshtoken">> 请不要加入 auth
need_auth_paths() ->
    [
        <<"/chat">>
    ].
