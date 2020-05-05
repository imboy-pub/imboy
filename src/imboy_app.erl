-module(imboy_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
        {
            '_',
            [
                {"/", handler_hello, []}
            ]
        }
    ]),
    {ok, _} = cowboy:start_clear(my_http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    imboy_sup:start_link().

stop(_State) ->
    ok.
