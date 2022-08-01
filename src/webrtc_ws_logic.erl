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
    event/3
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


% for webrtc
-spec event(Event::binary(), Data::map(), State::map()) -> ok.
event(<<"bye">>, Data, State) ->
    To = maps:get(to, Data),
    ToId = imboy_hashids:uid_decode(To),
    lager:info("data ~s, State ~p ~n", [Data, State]),
    _TimerRefList = message_ds:send(ToId,
        jsone:encode(Data, [native_utf8]),
        1),
    ok;
event(<<"leave">>, Data, State) ->
    To = maps:get(to, Data),
    ToId = imboy_hashids:uid_decode(To),
    lager:info("data ~s, State ~p ~n", [Data, State]),
    _TimerRefList = message_ds:send(ToId,
        jsone:encode(Data, [native_utf8]),
        1),
    ok;
event(<<"offer">>, Data, State) ->
    To = maps:get(to, Data),
    ToId = imboy_hashids:uid_decode(To),
    Msg = jsone:encode(Data, [native_utf8]),
    lager:info("Msg: ~s; data ~s, State ~p ~n", [Msg, Data, State]),
    _TimerRefList = message_ds:send(ToId, Msg, 1),
    ok;
event(<<"answer">>, Data, State) ->
    To = maps:get(to, Data),
    ToId = imboy_hashids:uid_decode(To),
    % Data2 = Data#{from => To},
    Msg = jsone:encode(Data, [native_utf8]),
    lager:info("Msg: ~s; data ~s, State ~p ~n", [Msg, Data, State]),
    _TimerRefList = message_ds:send(ToId, Msg, 1),
    ok;
event(<<"candidate">>, Data, State) ->
    To = maps:get(to, Data),
    ToId = imboy_hashids:uid_decode(To),
    % Data2 = Data#{from => To},
    Msg = jsone:encode(Data, [native_utf8]),
    lager:info("Msg: ~s; data ~s, State ~p ~n", [Msg, Data, State]),
    _TimerRefList = message_ds:send(ToId, Msg, 1),
    ok;
event(Event, Data, State) ->
    lager:info("event ~s, data ~s, State ~p ~n", [Event, Data, State]),
    ok.
% event_new(Data, State) ->
%     lager:info("event_new ~s ~n", [Data]),
%     try webrtc_ws_ds:json_decode(Data) of
%         #{type := <<"new">>, to := To} ->
%             CurrentUid = maps:get(current_uid, State),
%             From = imboy_hashids:uid_encode(CurrentUid),
%             Room = webrtc_ws_ds:room_name(From, To),
%             % lager:debug("socket authenticated room %p ", [Room]),
%             PeerId = webrtc_ws_ds:peer_id(),
%             webrtc_ws_ds:join_room(Room, From, PeerId),
%             {reply, webrtc_ws_ds:text_event(authenticated, #{peer_id => PeerId})};
%         _ ->
%             invalid_format
%     catch
%         Type:Error ->
%             lager:debug("invalid json ~p ~p", [Type, Error]),
%             invalid_json
%     end.

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
