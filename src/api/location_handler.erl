-module(location_handler).

%%%
% location 控制器模块
% location controller module
%%%
-behavior(cowboy_rest).

-export([init/2]).

-ifdef(EUNIT).

-include_lib("eunit/include/eunit.hrl").

-endif.

-include_lib("kernel/include/logger.hrl").

-include("common.hrl").
-include("log.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    % ?DEBUG_LOG(State),
    Action = maps:get(action, State0),
    % ?DEBUG_LOG([people_nearby, handler, Action]),
    State = maps:remove(action, State0),
    Req1 =
        case Action of
            make_myself_visible ->
                make_myself_visible(Req0, State);
            make_myself_unvisible ->
                make_myself_unvisible(Req0, State);
            people_nearby ->
                people_nearby(Req0, State);
            false ->
                Req0
        end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================
% 让自己可见
make_myself_visible(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    PostVals = imboy_param:post(Req0),
    Lat = maps:get(<<"latitude">>, PostVals, <<>>),
    Lng = maps:get(<<"longitude">>, PostVals, <<>>),
    % ?DEBUG_LOG([CurrentUid, Lat, Lng]),
    case location_logic:make_myself_visible(CurrentUid, Lat, Lng) of
        ok ->
            imboy_response:success(Req0, #{}, "success.");
        {error, Msg} ->
            imboy_response:error(Req0, Msg)
    end.

% 让自己不可见
make_myself_unvisible(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    location_logic:make_myself_unvisible(CurrentUid),
    imboy_response:success(Req0, #{}, "success.").

% 附近的人
-spec people_nearby(cowboy_req:req(), map()) -> cowboy_req:req().
people_nearby(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    #{longitude := Lng} = cowboy_req:match_qs([{longitude, [], undefined}], Req0),
    #{latitude := Lat} = cowboy_req:match_qs([{latitude, [], undefined}], Req0),
    % #{radius := Radius} = cowboy_req:match_qs([{radius, [], <<"500">>}], Req0),
    #{unit := Unit} = cowboy_req:match_qs([{unit, [], <<"m">>}], Req0),
    % #{limit := Limit} = cowboy_req:match_qs([{limit, [], <<"100">>}], Req0),
    {ok, Radius} = imboy_param:int(radius, Req0, 500),
    {ok, Limit} = imboy_param:int(limit, Req0, 100),
    % ?DEBUG_LOG([people_nearby, handler, Lng, Lat, Radius, Unit, Limit]),
    % 直接传递 integer，location_logic:people_nearby 的签名支持 binary() | integer()
    List = location_logic:people_nearby(CurrentUid, Lng, Lat, Radius, Unit, Limit),
    Payload = #{
        <<"radius">> => Radius,
        <<"size">> => length(List),
        <<"unit">> => <<"m">>,
        <<"list">> => List
    },
    imboy_response:success(Req0, Payload).

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
