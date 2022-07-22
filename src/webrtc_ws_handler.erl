-module(webrtc_ws_handler).

-export([init/2,
         websocket_init/1,
         websocket_handle/2,
         websocket_info/2,
         terminate/3
        ]).


-include_lib("imboy/include/chat.hrl").

init(Req, _) ->
  Room = cowboy_req:binding(?ROOM_SCOPE, Req),
  IdleTimeout = application:get_env(webrtc_server, idle_timeout, 60000),
  {cowboy_websocket, Req,
   #{room => Room, authenticated => false},
   % #{max_frame_size => 1048576},  % 1MB
   #{idle_timeout => IdleTimeout, max_frame_size => 1048576}
  }.

websocket_init(State) ->
  Time = application:get_env(webrtc_server, ws_auth_delay, 300),

  %% give the ws some time to authenticate before disconnecting it
  timer:send_after(Time, check_auth),
  {ok, State}.

%% not all ws clients can send a ping frame (namely, browsers can't)
%% so we handle a ping text frame.
websocket_handle({text, <<"ping">>}, State) ->
  {reply, {text, <<"pong">>}, State};

%% Before authentication, just expect the socket to send user/pass
websocket_handle({text, Text}, State = #{authenticated := false, room := Room}) ->
   case authenticate(Text) of
     {success, Username} ->
       lager:debug("socket authenticated"),

       PeerId = peer_id(),
       join_room(Room, Username, PeerId),

       State2 = State#{authenticated => true,
                       username => Username,
                       peer_id => PeerId},

       {reply, webrtc_ws_ds:text_event(authenticated, #{peer_id => PeerId}), State2};
     Reason ->
       lager:debug("bad authentication: ~p ~p", [Reason, Text]),
       {reply, webrtc_ws_ds:text_event(unauthorized), State}
   end;

%% After authentication, any message should be targeted to a specific peer
websocket_handle({text, Text}, State = #{authenticated := true,
                                         room := Room,
                                         peer_id := ThisPeer}) ->
  case webrtc_ws_ds:json_decode(Text) of
    #{to := OtherPeer} = Message ->
      %% crash if room doesn't match
      {Pid, {Username, OtherPeer, Room}} = syn:lookup(?ROOM_SCOPE, OtherPeer),

      lager:debug("Username: ~p; OtherPeer:~p, ThisPeer:~p~n", [Username, OtherPeer, ThisPeer]),
      %% extend message with this peer id before sending
      Message2 = Message#{from => ThisPeer},
      Message3 = webrtc_ws_ds:json_encode(Message2),
      % lager:debug("Message3: ~s~n", [Message3]),
      Pid ! {text, Message3},
      {ok, State};
    _ ->
      {reply, webrtc_ws_ds:text_event(invalid_message), State}
  end;

websocket_handle(Frame, State) ->
  lager:warning("Received non text frame ~p~p", [Frame, State]),
  {ok, State}.

%% If user/password not sent before ws_auth_delay, disconnect
websocket_info(check_auth, State = #{authenticated := false}) ->
  lager:debug("disconnecting unauthenticated socket"),
  {stop, State};

websocket_info(check_auth, State) ->
  %% already authenticated, do nothing
  {ok, State};

%% incoming text frame, send to the client socket
websocket_info({text, Text}, State = #{authenticated := true}) ->
  lager:debug("Sending to client ~p", [Text]),
  {reply, {text, Text}, State};

websocket_info(Info, State) ->
  lager:warning("Received unexpected info ~p~p", [Info, State]),
  {ok, State}.

terminate(_Reason, _Req, #{room := Room, username := Username, peer_id := PeerId}) ->
  OtherUsers = [Name || {Pid, {Name, _PeerId}} <- syn:members(?ROOM_SCOPE, Room), Pid /= self()],
  syn:publish(?ROOM_SCOPE, Room, webrtc_ws_ds:text_event(left, #{username => Username,
                                                    peer_id => PeerId})),
  run_callback(leave_callback, Room, Username, OtherUsers),
  ok;
terminate(_Reason, _Req, _State) ->
  ok.

%%% internal
authenticate(Data) ->
  try webrtc_ws_ds:json_decode(Data) of
    #{event := <<"authenticate">>, data := #{username := User, password := Password}} ->
      % {success, User};
      case webrtc_ws_logic:authenticate(User, Password) of
        Password -> {success, User};
        _ -> wrong_credentials
      end;
    _ -> invalid_format
  catch
    Type:Error ->
      lager:debug("invalid json ~p ~p", [Type, Error]),
      invalid_json
  end.

join_room(Room, Username, PeerId) ->
  OtherMembers = syn:members(?ROOM_SCOPE, Room),
  syn:register(?ROOM_SCOPE, PeerId, self(), {Username, PeerId, Room}),
  syn:join(?ROOM_SCOPE, Room, self(), {Username, PeerId}),

  %% broadcast peer joined to the rest of the peers in the room
  Message = webrtc_ws_ds:text_event(joined, #{peer_id => PeerId,
                                              username => Username}),
  lists:foreach(fun({Pid, _}) -> Pid ! Message end, OtherMembers),

  OtherNames = [Name || {_, {Name, _Peer}} <- OtherMembers],
  run_callback(join_callback, Room, Username, OtherNames).

run_callback(Type, Room, Username, CurrentUsers) ->
  case webrtc_ws_logic:callback(Type) of
    {Module, Function} ->
      try
        Module:Function(Room, Username, CurrentUsers)
      catch
        ErrorType:Error:Stacktrace ->
          lager:error(
            "~nError running ~p ~p ~p:~s",
            [Type, Room, Username, lager:pr_stacktrace(Stacktrace,
                                                       {ErrorType, Error})])
      end;
    undefined ->
      ok
  end.


peer_id() ->
  base64:encode(crypto:strong_rand_bytes(10)).
