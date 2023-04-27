-module(user_device_handler).
%%%
% user_device 控制器模块
% user_device controller module
%%%
-behavior(cowboy_rest).

-export([init/2]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("imboy/include/log.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("imboy/include/common.hrl").

%% ------------------------------------------------------------------
%% api
%% ------------------------------------------------------------------

init(Req0, State0) ->
    % ?LOG(State),
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 = case Action of
        page ->
            page(Req0, State);
        change_name ->
            change_name(Req0, State);
        delete ->
            delete(Req0, State);
        false ->
            Req0
    end,
    {ok, Req1, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

page(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    {Page, Size} = imboy_req:page_size(Req0),
    Payload = user_device_logic:page(CurrentUid, Page, Size),
    imboy_response:success(Req0, Payload).

change_name(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    PostVals = imboy_req:post_params(Req0),
    % ?LOG(PostVals),
    DID = proplists:get_value(<<"did">>, PostVals, <<"">>),
    Name = proplists:get_value(<<"name">>, PostVals, <<"">>),
    user_device_logic:change_name(CurrentUid, DID, Name),
    imboy_response:success(Req0).

delete(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    PostVals = imboy_req:post_params(Req0),
    % ?LOG(PostVals),
    DID = proplists:get_value(<<"did">>, PostVals, <<"">>),
    user_device_logic:delete(CurrentUid, DID),
    imboy_response:success(Req0).

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
