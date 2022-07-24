-module(webrtc_ws_logic).
%%%
% webrtc_ws 业务逻辑模块
% webrtc_ws business logic module
%%%

-export([
  callback/1,
  authenticate/2,
  create/3,
  join/3,
  leave/3
]).

-export([
  peer_id/0,
  authenticate/1,
  join_room/3,
  run_callback/4
]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("imboy/include/chat.hrl").
-include_lib("imboy/include/common.hrl").
-include_lib("kernel/include/logger.hrl").

%% ------------------------------------------------------------------
%% api
%% ------------------------------------------------------------------

callback(create_callback) ->
  {webrtc_ws_logic, create};
callback(join_callback) ->
  {webrtc_ws_logic, join};
callback(leave_callback) ->
  {webrtc_ws_logic, leave}.

authenticate(_Username, Password) ->
  %% in a real scenario this may lookup the password in the db, request an external service, etc.
  % {ok, Password} = application:get_env(example, example_password),
  % Password.
  Password.

create(Room, Username, _OtherUsers) ->
  lager:info("~s created ~s", [Username, Room]).

join(Room, Username, _OtherUsers) ->
  lager:info("~s joined ~s", [Username, Room]).

leave(Room, Username, _OtherUsers) ->
  lager:info("~s left ~s", [Username, Room]).


% for webrtc
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


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% -------------------------------------------------------------------

%

%% ------------------------------------------------------------------
%% EUnit tests.
%% ------------------------------------------------------------------

-ifdef(EUNIT).
%addr_test_() ->
%    [?_assert(is_public_addr(?PUBLIC_IPV4ADDR)),
%     ?_assert(is_public_addr(?PUBLIC_IPV6ADDR)),
%     ?_test(my_if_addr(inet)),
%     ?_test(my_if_addr(inet6))].
-endif.
