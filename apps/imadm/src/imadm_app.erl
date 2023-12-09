-module(imadm_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).


start(_Type, _Args) ->
    simple_captcha_ets:init(),
    Routes = [{config_ds:env(host),
               [{"/adm/passport/login", adm_passport_handler, #{action => login}},
               {"/adm/passport/captcha", adm_passport_handler, #{action => captcha}},
                {"/adm/passport/do_login", adm_passport_handler, #{action => do_login}},

                %%%%%%% 上面写API路由，下面写静态资源 %%%%%%%%
                {"/static/[...]", cowboy_static, {priv_dir, imadm, "static", [{mimetypes, cow_mimetypes, all}]}}]}],
    Dispatch = cowboy_router:compile(Routes),

    ProtoOpts = #{
                  % middlewares => [cowboy_router, cowboy_handler],
                  % metrics_callback => do_metrics_callback(),
                  env => #{dispatch => Dispatch}
                 },

    Port = config_ds:env(http_port_adm, 9806),
    start_clear(ProtoOpts, Port),
    imadm_sup:start_link().


stop(_State) ->
    cowboy:stop_listener(imadm_listener),
    ok.


-spec start_clear(map(), integer()) -> {ok, pid()} | {error, any()}.
start_clear(ProtoOpts, Port) ->
    cowboy:start_clear(imadm_listener, [{port, Port}], ProtoOpts).
