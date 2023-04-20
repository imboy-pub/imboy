-module(imboy_router).

-export([get_routes/0]).
-export([open/0]).
-export([option/0]).

%% 服务端所有路由定义再这里(包含 option/0 open/0 里面的定义)
get_routes() ->
    {ok, HostImboy} = application:get_env(imboy, host),
    [{HostImboy, [
        {"/", index_handler, #{action => help}},
        {"/help", index_handler, #{action => help}},
        {"/init", index_handler, #{action => init}},
        {"/refreshtoken", passport_handler, #{action => refreshtoken}},

        {"/passport/login", passport_handler, #{action => do_login}},
        {"/passport/signup", passport_handler, #{action => do_signup}},
        {"/passport/getcode", passport_handler, #{action => send_code}},
        {"/passport/findpassword", passport_handler, #{action => find_password}},

        {"/ws", websocket_handler, #{}},
        {"/stress_testing", stress_testing_ws_handler, #{}},
        {"/auth/assets", auth_handler, #{action => assets}},

        {"/conversation/online", conversation_handler, #{action => online}},
        {"/conversation/mine", conversation_handler, #{action => mine}},
        {"/conversation/msgbox", conversation_handler, #{action => msgbox}},

        {"/uqrcode", user_handler, #{action => uqrcode}},
        {"/user/update", user_handler, #{action => update}},
        {"/user/show", user_handler, #{action => open_info}},
        {"/user/change_state", user_handler, #{action => change_state}},
        {"/user/setting", user_handler, #{action => setting}},
        {"/user/credential", user_handler, #{action => credential}},

        {"/location/makeMyselfVisible", location_handler, #{action => make_myself_visible}},
        {"/location/makeMyselfUnvisible", location_handler, #{action => make_myself_unvisible}},
        {"/location/peopleNearby", location_handler, #{action => people_nearby}},

        {"/friend/add", friend_handler, #{action => add_friend}},
        {"/friend/confirm", friend_handler, #{action => confirm_friend}},
        {"/friend/delete", friend_handler, #{action => delete_friend}},
        {"/friend/list", friend_handler, #{action => friend_list}},
        {"/friend/information", friend_handler, #{action => information}},
        {"/friend/find", friend_handler, #{action => find}},
        {"/friend/change_remark", friend_handler, #{action => change_remark}},

        {"/friend/denylist/add", user_denylist_handler, #{action => add}},
        {"/friend/denylist/remove", user_denylist_handler, #{action => remove}},
        {"/friend/denylist/page", user_denylist_handler, #{action => page}},

        {"/friend/move", friend_handler, #{action => move}},
        {"/friend/category/add", friend_category_handler, #{action => add}},
        {"/friend/category/delete", friend_category_handler, #{action => delete}},
        {"/friend/category/rename", friend_category_handler, #{action => rename}},

        {"/group/member", group_handler, #{action => member}},

        %%%%%%% 上面写API路由，下面写静态资源 %%%%%%%%

        {"/static/[...]", cowboy_static,
            {priv_dir, imboy, "static", [{mimetypes, cow_mimetypes, all}]}
        }
    ]}].

%% 因为 除去 option 和 open 的路由，就是必须要 auth 的路由了
%% 所以 这里不需要订阅 auth/0 方法

%% 如果请求头里面有 authorization 字段，就需要认证的API
%% 列表元素必须为binary
%% auth_middleware 去除了path 最后的斜杆，所以不用以 / 结尾了
option() ->
    [
        <<"/uqrcode">>
    ].

%% 不需要认证的API
%% 列表元素必须为binary
%% auth_middleware 去除了path 最后的斜杆，所以不用以 / 结尾了
open() ->
    [
        % /ws 有自己的auth
        <<"/ws">>,
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
        <<"/auth/assets">>,
        <<"/">>
    ].
