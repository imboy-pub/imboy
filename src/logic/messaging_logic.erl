-module(messaging_logic).

-export([handle_rest_action/3, route_ws/5]).

-spec handle_rest_action(atom() | false, cowboy_req:req(), map()) -> cowboy_req:req().
handle_rest_action(offline, Req0, State) ->
    msg_handler:offline(Req0, State);
handle_rest_action(offline_ack, Req0, State) ->
    msg_handler:offline_ack(Req0, State);
handle_rest_action(read_stats, Req0, State) ->
    msg_handler:read_stats(Req0, State);
handle_rest_action(pin, Req0, State) ->
    msg_handler:pin(Req0, State);
handle_rest_action(forward, Req0, State) ->
    msg_handler:forward(Req0, State);
handle_rest_action(reaction_add, Req0, State) ->
    msg_handler:reaction_add(Req0, State);
handle_rest_action(reaction_remove, Req0, State) ->
    msg_handler:reaction_remove(Req0, State);
handle_rest_action(reaction_list, Req0, State) ->
    msg_handler:reaction_list(Req0, State);
handle_rest_action(false, Req0, _State) ->
    Req0.

-spec route_ws(binary(), integer(), map(), binary(), binary()) -> ok | {reply, map()}.
route_ws(MsgId, CurrentUid, Data, Type, OriginalMsg) ->
    message_router_logic:route(MsgId, CurrentUid, Data, Type, OriginalMsg).
