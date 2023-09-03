-module(imboy_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

-include_lib("imboy/include/log.hrl").


start(_Type, _Args) ->
    inets:start(),
    imboy_session:init(),
    % begin handler
    Routes = imboy_router:get_routes(),
    % cowboy_router:dispatch_rules()
    Dispatch = cowboy_router:compile(Routes),
    StartMode = config_ds:env(start_mode),
    if
        StartMode == quic ->
            start_quic(Dispatch);
        true ->
            ProtoOpts = #{
                middlewares => [
                    cowboy_router
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
            },
            case StartMode of
                tls ->
                    start_tls(ProtoOpts);
                _ ->
                    start_clear(ProtoOpts)
            end
    end,
    imboy_sup:start_link().

% do_metrics_callback() ->
%    fun(Metrics) ->
%       error_logger:error_msg("@@ metrics~n~p~n", [Metrics]),
%       ok
%    end.

stop(_State) ->
    ok = cowboy:stop_listener(imboy_listener).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================
-spec start_quic(cowboy_router:dispatch_rules())
    -> {ok, pid()} | {error, any()}.
start_quic(Dispatch) ->
    PrivDir = code:priv_dir(imboy),
    cowboy:start_quic(#{
        socket_opts => [
            % {cert, "deps/quicer/test/quicer_SUITE_data/cert.pem"},
            % {key, "deps/quicer/test/quicer_SUITE_data/key.pem"}
            {cert, PrivDir ++ config_ds:env(certfile)}
            , {key, PrivDir ++ config_ds:env(keyfile)}
        ]
    }, #{
        env => #{dispatch => Dispatch}
    }).

-spec start_tls(map())
    -> {ok, pid()} | {error, any()}.
start_tls(ProtoOpts) ->
    Port = config_ds:env(http_port),
    PrivDir = code:priv_dir(imboy),
    cowboy:start_tls(imboy_listener
        , [
            {port, Port}
            , {cacertfile, PrivDir ++ config_ds:env(cacertfile)}
            , {certfile, PrivDir ++ config_ds:env(certfile)}
            , {keyfile, PrivDir ++ config_ds:env(keyfile)}
        ]
        , ProtoOpts
    ).

-spec start_clear(map())
    -> {ok, pid()} | {error, any()}.
start_clear(ProtoOpts) ->
    Port = config_ds:env(http_port),
    cowboy:start_clear(imboy_listener
        , [
            {port, Port}
        ]
        , ProtoOpts
    ).
