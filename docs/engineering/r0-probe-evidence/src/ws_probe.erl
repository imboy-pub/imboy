-module(ws_probe).
-behaviour(cowboy_websocket).

-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

%% Same shape as imboy websocket_ds:auth/4 (src/ds/websocket_ds.erl:240):
%% the options map rides in the 4th element of the
%% {cowboy_websocket, Req, State, Opts} tuple, i.e. STREAM options.
init(Req, State) ->
    bump(handler_init),
    {cowboy_websocket, Req, State, stream_opts()}.

%% Verbatim keys of imboy websocket_handler:websocket_options/0 (:116-127).
stream_opts() ->
    #{
        num_acceptors => infinity,
        max_connections => infinity,
        enable_connect_protocol => true,
        max_frame_size => 2097152,
        idle_timeout => 180000
    }.

websocket_init(State) ->
    bump(ws_init),
    {ok, State, hibernate}.

websocket_handle({text, <<"ping">>}, State) ->
    bump(ws_ping),
    {reply, {text, <<"pong">>}, State, hibernate};
websocket_handle(_Frame, State) ->
    {ok, State, hibernate}.

websocket_info(_Msg, State) ->
    {ok, State}.

terminate(_Reason, _Req, _State) ->
    bump(ws_terminate),
    ok.

bump(Key) ->
    try
        ets:update_counter(probe_counters, Key, {2, 1}, {Key, 0})
    catch
        _:_ -> 0
    end,
    ok.
