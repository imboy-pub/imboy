-module(textguard_grpcbox_transport).

-export([start/3, stop/1, call/4, raw/3]).

start(Name, Host, Port) ->
    grpcbox_channel_sup:start_child(Name, [{http, binary_to_list(Host), Port, []}], #{
        sync_start => true
    }).

stop(Name) ->
    try
        grpcbox_channel:stop(Name)
    catch
        _:_ -> ok
    end,
    ok.

call(Endpoint, Method, Request, Budget) ->
    moderation_detector_grpcbox:call(Endpoint, Method, Request, Budget).

raw(Method, Request, Budget) ->
    moderation_detector_grpcbox:request(Method, Request, Budget).
