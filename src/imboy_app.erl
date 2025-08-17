-module(imboy_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

% -include_lib("imlib/include/log.hrl").


start(_Type, _Args) ->
    inets:start(),
    imboy_syn:init(),
    % 初始化集群管理
    imboy_cluster:init(),
    % khepri:start(),
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
                    cowboy_router % 必须是第一个元素
                    , auth_middleware % 必须是第二个元素
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
            Port = case os:getenv("HTTP_PORT") of
                P when is_list(P) ->
                    list_to_integer(P);
                false ->
                    config_ds:env(http_port)
            end,
            case StartMode of
                tls ->
                    start_tls(ProtoOpts, Port);
                _ ->
                    start_clear(ProtoOpts, Port)
            end
    end,
    imboy_sup:start_link().


% do_metrics_callback() ->
%    fun(Metrics) ->
%       error_logger:error_msg("@@ metrics~n~p~n", [Metrics]),
%       ok
%    end.

stop(_State) ->
    StartMode = config_ds:env(start_mode),
    case StartMode of
        http_tls ->
            cowboy:stop_listener(imboy_listener),
            cowboy:stop_listener(imboy_listener_tls);
        tls ->
            cowboy:stop_listener(imboy_listener_tls);
        _ ->
            cowboy:stop_listener(imboy_listener)
    end.


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================
-spec start_quic(cowboy_router:dispatch_rules()) -> {ok, pid()} | {error, any()}.
start_quic(_Dispatch) ->
    {error, "调整中的功能"}.
    % PrivDir = code:priv_dir(imboy),
    % cowboy:start_quic(#{socket_opts => [
    %                                     % {cert, "deps/quicer/test/quicer_SUITE_data/cert.pem"},
    %                                     % {key, "deps/quicer/test/quicer_SUITE_data/key.pem"}
    %                                     {cert, PrivDir ++ config_ds:env(certfile)},
    %                                     {key, PrivDir ++ config_ds:env(keyfile)}]},
    %                   #{env => #{dispatch => Dispatch}}).


-spec start_tls(map(), integer()) -> {ok, pid()} | {error, any()}.
start_tls(ProtoOpts, Port) ->
    % Port = config_ds:env(http_port),
    PrivDir = code:priv_dir(imboy),
    cowboy:start_tls(imboy_listener_tls,
                     [{port, Port},
                      {cacertfile, PrivDir ++ config_ds:env(cacertfile)},
                      {certfile, PrivDir ++ config_ds:env(certfile)},
                      {keyfile, PrivDir ++ config_ds:env(keyfile)}],
                     ProtoOpts).


-spec start_clear(map(), integer()) -> {ok, pid()} | {error, any()}.
start_clear(ProtoOpts, Port) ->
    cowboy:start_clear(imboy_listener, [{port, Port}], ProtoOpts).
