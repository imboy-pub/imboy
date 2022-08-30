-module(webrtc_ws_logic).
%%%
% webrtc_ws 业务逻辑模块
% webrtc_ws business logic module
%%%

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
-spec event(ToId::integer(), Msg::binary(), State::map()) -> ok.
event(ToId, Msg, State) ->
    lager:info("data ~s, State ~p ~n", [Msg, State]),
    _TimerRefList = message_ds:send(ToId,
        Msg,
        1),
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
