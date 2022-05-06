-module(imboy_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

-include("common.hrl").


start(_Type, _Args) ->
    %%启动存储pid的树据 可以采用 ets 表格处理 但是为了方便集群处理 我采用的mnesia
    repo_chat_store:init(),
    % begin handler
    Routes = route_helper:get_routes(),
    ?LOG(Routes),
    Dispatch = cowboy_router:compile(Routes),
    {ok, Port} = application:get_env(imboy, http_port),
    {ok, _} = cowboy:start_clear(imboy_http_listener,
                                 [{port, Port}],
                                 #{middlewares => [cowboy_router,
                                                   % middleware_verify,
                                                   middleware_auth,
                                                   cowboy_handler],
                                   env => #{dispatch => Dispatch}}),
    % end handler
    imboy_sup:start_link().


stop(_State) ->
    ok = cowboy:stop_listener(imboy_http_listener).
