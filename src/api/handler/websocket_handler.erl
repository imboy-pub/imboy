-module(websocket_handler).
-behavior(cowboy_websocket).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).


init(Req, State) ->
    {cowboy_websocket, Req, State}.

websocket_init(State) ->
    {[], State}.

websocket_handle({text, Data}, State) ->
    {[{text, Data}], State};
websocket_handle({binary, Data}, State) ->
    {[{binary, Data}], State};
websocket_handle(_Frame, State) ->
    {[], State}.

websocket_info({timeout, _Ref, Msg}, State) ->
    erlang:start_timer(1000, self(), <<"How' you doin'?">>),
    {[{text, Msg}], State};
websocket_info(_Info, State) ->
    {[], State}.
