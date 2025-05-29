-module(fts_handler).
%%%
% fts 控制器模块
% fts controller module
%%%
-behavior(cowboy_rest).

-export([init/2]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("imlib/include/log.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("imlib/include/common.hrl").

%% ===================================================================
%% API
%% ===================================================================


init(Req0, State0) ->
    % ?DEBUG_LOG(State),
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 =
        case Action of
            user_search ->
                user_search(Req0, State);
            recently_user ->
                recently_user(Req0, State);
            false ->
                Req0
        end,
    {ok, Req1, State}.


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================


% 搜索“用户允许被搜索”的用户
user_search(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    {Page, Size} = imboy_req:page_size(Req0),
    #{keyword := Keyword} = cowboy_req:match_qs([{keyword, [], <<"">>}], Req0),
    Payload = fts_logic:user_search_page(CurrentUid, Page, Size, Keyword),
    imboy_response:success(Req0, Payload).


% 最近新注册的并且允许被搜索到的朋友
recently_user(Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    {Page, Size} = imboy_req:page_size(Req0),
    #{keyword := Keyword} = cowboy_req:match_qs([{keyword, [], <<"">>}], Req0),
    Payload = fts_logic:recently_user_page(CurrentUid, Page, Size, Keyword),
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
