-module(imboy_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

-include_lib("imboy/include/log.hrl").


start(_Type, _Args) ->
    imboy_session:init(),

    % begin handler
    Routes = imboy_router:get_routes(),

    % ?LOG(Routes),
    Dispatch = cowboy_router:compile(Routes),

    {ok, _} = cowboy:start_clear(imboy_listener,
        [
            {port, imboy_func:env(http_port)}
        ]

        % PrivDir = code:priv_dir(imboy),
        % {ok, _} = cowboy:start_tls(imboy_listener,
        %     [
        %         {port, Port}
        %         , {cacertfile, PrivDir ++ "/ssl/cowboy-ca.crt"}
        %         , {certfile, PrivDir ++ "/ssl/server.crt"}
        %         , {keyfile, PrivDir ++ "/ssl/server.key"}
        %     ],

        , #{
            middlewares => [
                cowboy_router
                % , verify_middleware
                , auth_middleware
                , cowboy_handler
            ],
            % metrics_callback => do_metrics_callback(),
            stream_handlers => [
                cowboy_compress_h
                , cowboy_stream_h
                % , cowboy_metrics_h
            ],
            env => #{dispatch => Dispatch}
        }
    ),
    % end handler
    imboy_sup:start_link().

% do_metrics_callback() ->
%    fun(Metrics) ->
%       error_logger:error_msg("@@ metrics~n~p~n", [Metrics]),
%       ok
%    end.

stop(_State) ->
    ok = cowboy:stop_listener(imboy_listener).
