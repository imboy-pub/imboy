-module(imboy_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

% -include("log.hrl").

%% @doc 启动 application 回调
-spec start(term(), term()) -> {ok, pid()} | {ok, pid(), term()} | {error, term()}.
start(_Type, _Args) ->
    _ = inets:start(),
    _ = imboy_syn:init(),
    % 初始化集群管理
    _ = imboy_cluster:init(),
    % 初始化验证码 ETS 表
    _ = simple_captcha_ets:init(),
    % khepri:start(),
    % begin handler
    Routes = imboy_router:get_routes(),
    % cowboy_router:dispatch_rules()
    Dispatch = cowboy_router:compile(Routes),
    StartMode = config_ds:env(start_mode, http),
    _ = if
        StartMode == quic ->
            start_quic(Dispatch);
        true ->
            ProtoOpts = #{
                env => #{dispatch => Dispatch},
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
                tcp_opts => [
                    % 【关键修复】禁用 Nagle 算法，消除小消息延迟
                    {nodelay, true}
                ]
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

%% @doc 停止 application 回调
-spec stop(term()) -> ok.
stop(_State) ->
    StartMode = config_ds:env(start_mode, http),
    case StartMode of
        http_tls ->
            _ = cowboy:stop_listener(imboy_listener),
            _ = cowboy:stop_listener(imboy_listener_tls);
        tls ->
            _ = cowboy:stop_listener(imboy_listener_tls);
        _ ->
            _ = cowboy:stop_listener(imboy_listener)
    end.


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================
-spec start_quic(cowboy_router:dispatch_rules()) -> {ok, pid()} | {error, any()}.
start_quic(_Dispatch) ->
    {error, <<"调整中的功能"/utf8>>}.
    % PrivDir = code:priv_dir(imboy),
    % cowboy:start_quic(#{socket_opts => [
    %                                     % {cert, "deps/quicer/test/quicer_SUITE_data/cert.pem"},
    %                                     % {key, "deps/quicer/test/quicer_SUITE_data/key.pem"}
    %                                     {cert, PrivDir ++ config_ds:env(certfile)},
    %                                     {key, PrivDir ++ config_ds:env(keyfile)}]},
    %                   #{env => #{dispatch => Dispatch}}).


-spec start_tls(map(), integer()) -> {ok, pid()} | {error, any()}.
start_tls(ProtoOpts, Port) ->
    PrivDir = code:priv_dir(imboy),
    cowboy:start_tls(imboy_listener_tls,
                     [{port, Port},
                      {cacertfile, PrivDir ++ config_ds:env(cacertfile)},
                      {certfile, PrivDir ++ config_ds:env(certfile)},
                      {keyfile, PrivDir ++ config_ds:env(keyfile)}],
                     ProtoOpts).


-spec start_clear(map(), integer()) -> {ok, pid()} | {error, any()}.
start_clear(ProtoOpts, Port) ->
    cowboy:start_clear(imboy_listener,
                       [{port, Port}],
                       ProtoOpts).
