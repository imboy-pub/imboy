-module (websocket_as).
%%%
% websocket_as 是 user application service 缩写
%%%
% -export ([subprotocol/1]).
-export ([dialog/2]).

-include("imboy.hrl").

dialog(CurrentUid, Data) ->
    {<<"to_id">>, ToId} = lists:keyfind(<<"to_id">>, 1, Data),
    ToPid = get_pid(ToId),
    FromPid = get_pid(CurrentUid),
    {<<"content">>, Content} = lists:keyfind(<<"content">>, 1, Data),
   Msg = [
        {<<"type">>,<<"dialog">>},
        {<<"from_id">>, CurrentUid},
        {<<"to_id">>, ToId},
        {<<"content">>, Content},
        {<<"timestamp">>, imboy_func:milliseconds()}
    ],
    ?LOG(["CurrentUid", CurrentUid, "FromPid", FromPid, "ToId", ToId, "ToPid:", ToPid, jsx:encode(Msg)]),
    % jsx:encode(Msg).
    erlang:start_timer(1, ToPid, jsx:encode(Msg)),
    ok.

get_pid(Uid) ->
    L1 = websocket_store_repo:lookup(Uid),
    {Uid, Pid, _Type} = lists:keyfind(Uid, 1, L1),
    ?LOG(["get_pid ",Uid, Pid, L1]),
    Pid.
