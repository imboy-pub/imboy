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

-include("log.hrl").

-include_lib("kernel/include/logger.hrl").

-include("common.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
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

%% @doc 搜索用户
%% 搜索允许被搜索的用户
%%
%% @param Req0 Cowboy请求对象，包含搜索关键词和分页参数
%% @param State 状态映射，包含 current_uid
%% @return 返回包含搜索结果的响应
%% @end
-spec user_search(cowboy_req:req(), map()) -> cowboy_req:req().
user_search(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    {Page, Size} = elib_param:page(Req0),
    #{keyword := Keyword} = cowboy_req:match_qs([{keyword, [], <<"">>}], Req0),
    Payload = fts_logic:user_search_page(CurrentUid, Page, Size, Keyword),
    elib_response:success(Req0, Payload).

%% @doc 最近新注册的用户
%% 获取最近新注册的并且允许被搜索到的用户
%%
%% @param Req0 Cowboy请求对象，包含搜索关键词和分页参数
%% @param State 状态映射，包含 current_uid
%% @return 返回包含搜索结果的响应
%% @end
-spec recently_user(cowboy_req:req(), map()) -> cowboy_req:req().
recently_user(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    {Page, Size} = elib_param:page(Req0),
    #{keyword := Keyword} = cowboy_req:match_qs([{keyword, [], <<"">>}], Req0),
    Payload = fts_logic:recently_user_page(CurrentUid, Page, Size, Keyword),
    elib_response:success(Req0, Payload).

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
