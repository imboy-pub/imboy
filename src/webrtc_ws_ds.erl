-module(webrtc_ws_ds).

-export([json_encode/1,
         json_decode/1,
         text_event/1,
         text_event/2]).

-export([
    room_name/2,
    peer_id/0,
    join_room/3,
    run_callback/4
]).

-include_lib("imboy/include/chat.hrl").

%% ------------------------------------------------------------------
%% api
%% ------------------------------------------------------------------


room_name(From, To) when From > To ->
    <<To/binary, "_", From/binary>>;
room_name(From, To) ->
    <<From/binary, "_", To/binary>>.

join_room(Room, From, PeerId) ->
    OtherMembers = syn:members(?ROOM_SCOPE, Room),
    syn:register(?ROOM_SCOPE, PeerId, self(), {From, PeerId, Room}),
    syn:join(?ROOM_SCOPE, Room, self(), {From, PeerId}),

    %% broadcast peer joined to the rest of the peers in the room
    Message = webrtc_ws_ds:text_event(joined, #{
        peer_id => PeerId,
        from => From
    }),
    lists:foreach(fun({Pid, _}) -> Pid ! Message end, OtherMembers),

    OtherNames = [Name || {_, {Name, _Peer}} <- OtherMembers],
    run_callback(join_callback, Room, From, OtherNames).

run_callback(Type, Room, From, CurrentUsers) ->
    case webrtc_ws_logic:callback(Type) of
        {Module, Function} ->
            try
                Module:Function(Room, From, CurrentUsers)
            catch
                ErrorType:Error:Stacktrace ->
                    lager:error(
                        "~nError running ~p ~p ~p:~s",
                        [Type, Room, From, lager:pr_stacktrace(Stacktrace,
                            {ErrorType, Error})])
            end;
        undefined ->
            ok
    end.


peer_id() ->
    base64:encode(crypto:strong_rand_bytes(10)).

json_decode(Data) ->
    jsone:decode(Data, [{object_format, map},{keys, atom}]).

json_encode(Data) ->
    jsone:encode(Data, [native_utf8]).

text_event(Event) ->
    {text, json_encode(#{type => Event})}.

text_event(Event, Data) ->
    {text, json_encode(#{type => Event, payload => Data})}.


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% -------------------------------------------------------------------
