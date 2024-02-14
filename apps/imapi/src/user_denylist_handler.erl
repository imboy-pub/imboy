-module(user_denylist_handler).
%%%
% user_denylist 控制器模块
% user_denylist controller module
%%%
-behavior(cowboy_rest).

-export([init/2]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("kernel/include/logger.hrl").
-include_lib("imlib/include/common.hrl").

%% ===================================================================
%% API
%% ===================================================================


init(Req0, State0) ->
    % ?LOG(State),
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 =
        case Action of
            add ->
                add(Req0, State);
            remove ->
                remove(Req0, State);
            page ->
                page(Req0, State);
            false ->
                Req0
        end,
    {ok, Req1, State}.


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================


page(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    {Page, Size} = imboy_req:page_size(Req0),
    Payload = user_denylist_logic:page(CurrentUid, Page, Size),
    imboy_response:success(Req0, Payload).


add(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),

    PostVals = imboy_req:post_params(Req0),
    DeniedUserId = proplists:get_value(<<"denied_user_id">>, PostVals, ""),

    DeniedUserId2 = imboy_hashids:decode(DeniedUserId),
    CreatedAt = user_denylist_logic:add(CurrentUid, DeniedUserId2),
    imboy_response:success(Req0,
                           [{<<"user_id">>, imboy_hashids:encode(CurrentUid)},
                            {<<"denied_user_id">>, DeniedUserId},
                            {<<"created_at">>, CreatedAt}]).


remove(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),

    PostVals = imboy_req:post_params(Req0),
    DeniedUserId = proplists:get_value(<<"denied_user_id">>, PostVals, ""),
    DeniedUserId2 = imboy_hashids:decode(DeniedUserId),

    user_denylist_logic:remove(CurrentUid, DeniedUserId2),
    imboy_response:success(Req0).


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
