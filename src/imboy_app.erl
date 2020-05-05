-module(imboy_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    %
    % begin handler
    Routes = route_helper:get_routes(),
    Dispatch = cowboy_router:compile(Routes),
    {ok, Port} = application:get_env(imboy, http_port),
    {ok, _} = cowboy:start_clear(imboy_http_listener,
        [{port, Port}],
        #{
            middlewares => [
                cowboy_router,
                auth_middleware,
                cowboy_handler
            ],
            env => #{dispatch => Dispatch}
        }
    ),
    % end handler
    imboy_sup:start_link().

stop(_State) ->
    ok.
