-module(ws_perf_socket).
-export([init/2,websocket_init/1,websocket_handle/2,websocket_info/2]).
init(Req, State) -> {cowboy_websocket, Req, State,
    #{idle_timeout => 180000, max_frame_size => 2097152}}.
websocket_init(#{parent := Parent, uid := Uid, did := Did} = State) ->
    ok = imboy_syn:join(Uid, <<"synthetic">>, self(), Did),
    Parent ! {registered, self()},
    {ok, State}.
%% This fixture deliberately does not implement production login or CLIENT_ACK.
%% Production inbound ACK persistence is verified separately by its real tests.
websocket_handle(ping, State) -> websocket_handler:websocket_handle(ping, State);
websocket_handle({ping, _} = Frame, State) -> websocket_handler:websocket_handle(Frame, State);
websocket_handle(_, State) -> {ok, State}.
websocket_info(Info, State) -> websocket_handler:websocket_info(Info, State).
