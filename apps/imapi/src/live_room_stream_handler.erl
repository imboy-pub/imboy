-module(live_room_stream_handler).
%%%
% 直播间数据流处理模块
% room_stream controller module
%%%

-export([init/2]).
-export([info/3]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("kernel/include/logger.hrl").
-include_lib("imlib/include/common.hrl").
-include_lib("imlib/include/log.hrl").

%% ===================================================================
%% API
%% ===================================================================


init(Req0, Opts) ->
    StreamId = cowboy_req:binding(stream_id, Req0),
    imboy_log:info("StreamId ~p~n", [StreamId]),
    check_role(StreamId, Opts),
    Req = cowboy_req:stream_reply(200, #{<<"content-type">> => <<"text/event-stream">>}, Req0),
    erlang:send_after(1000, self(), {message, "Tick"}),
    {cowboy_loop, Req, Opts}.


info({message, Msg}, Req, State) ->
    imboy_log:info("info_Msg ~p, State ~p~n", [Msg, State]),
    cowboy_req:stream_events(#{id => id(), data => Msg}, nofin, Req),
    % erlang:send_after(10, self(), {message, "Tick"}),
    {ok, Req, State};
info(Msg, Req, State) ->
    imboy_log:info("info_Msg2 ~p, State ~p~n", [Msg, State]),
    cowboy_req:stream_events(#{id => id(), data => Msg}, nofin, Req),
    {ok, Req, State}.


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================


check_role(StreamId, State) ->
    imboy_log:info("StreamId ~p, State ~p~n", [StreamId, State]),
    ok.


id() ->
    integer_to_list(erlang:unique_integer([positive, monotonic]), 16).


%% ===================================================================
%% EUnit tests.
%% ===================================================================

-ifdef(EUNIT).
%addr_test_() ->
%    [?_assert(is_public_addr(?PUBLIC_IPV4ADDR)),
%     ?_assert(is_public_addr(?PUBLIC_IPV6ADDR)),
%     ?_test(my_if_addr(inet)),
%     ?_test(my_if_addr(inet6))].
-endif.
