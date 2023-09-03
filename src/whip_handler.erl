-module(whip_handler).
%%%
% whip 控制器模块
% whip controller module
% 参考 https://blog.csdn.net/sweibd/article/details/124552793
%%%
-behavior(cowboy_rest).

-export([init/2]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("imboy/include/log.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("imboy/include/common.hrl").

%% ===================================================================
%% API
%% ===================================================================

init(Req0, State0) ->
    % ?LOG(State),
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 = case Action of
        publish ->
            publish(Req0, State);
        unpublish ->
            unpublish(Req0, State);
        publish ->
            publish(Req0, State);
        unsubscribe ->
            unsubscribe(Req0, State);
        % candidate ->
        %     candidate(Req0, State);
        false ->
            Req0
    end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

% webrtc推流接口
publish(Req0, State) ->
    % CurrentUid = maps:get(current_uid, State),
    % Uid = imboy_hashids:uid_encode(CurrentUid),
    {ok, OfferSdp, _Req} = cowboy_req:read_body(Req0),
    {ok, OfferSdp1} = ersip:parse(OfferSdp),
    new_sdp(),
    AsswerSdp = [],
    {ok, NewReq} = cowboy_req:reply(200
        , #{
            <<"Content-Type">> => "application/sdp"
            , <<"server">> => "imboy"
        }
        , iolist_to_binary(AsswerSdp)
        , Req0),
   {ok, NewReq, State}.

% webrtc unpublish
% 也可以暴力的关闭可以直接在客户端进行PeerConnection.Close(), 或者暴力关闭网页；
unpublish(Req0, _State) ->
    % CurrentUid = maps:get(current_uid, State),
    % Uid = imboy_hashids:uid_encode(CurrentUid),,
    imboy_response:success(Req0).


% webrtc拉流接口
subscribe(Req0, State) ->
    % CurrentUid = maps:get(current_uid, State),
    % Uid = imboy_hashids:uid_encode(CurrentUid),
    {ok, OfferSdp, _Req} = cowboy_req:read_body(Req0),
    AsswerSdp = [],
    {ok, NewReq} = cowboy_req:reply(200
        , #{
            <<"Content-Type">> => "application/sdp"
            , <<"server">> => "imboy"
        }
        , iolist_to_binary(AsswerSdp)
        , Req0),
   {ok, NewReq, State}.

% webrtc unsubscribe
% 也可以暴力的关闭可以直接在客户端进行PeerConnection.Close(), 或者暴力关闭网页；
unsubscribe(Req0, _State) ->
    % CurrentUid = maps:get(current_uid, State),
    % Uid = imboy_hashids:uid_encode(CurrentUid),,
    imboy_response:success(Req0).


% candidate(Req0, _State) ->
%     % CurrentUid = maps:get(current_uid, State),
%     % Uid = imboy_hashids:uid_encode(CurrentUid),
%     {ok, Candidate, _Req} = cowboy_req:read_body(Req0),
%     ok.

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

new_sdp() =>
    IP4FQDN = make_in_ip4(<<"pc33.atlanta.com">>),
    {od, Addr} = ersip_sdp_addr:from_raw(ersip_sdp_addr:raw(IP4FQDN)),
    ersip_sdp_conn:new(Addr).
