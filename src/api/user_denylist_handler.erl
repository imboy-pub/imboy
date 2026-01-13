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

%% @doc 黑名单分页列表
%% 获取用户的黑名单列表（分页）
%%
%% @param Req0 Cowboy请求对象，包含分页参数
%% @param State 状态映射，包含 current_uid
%% @return 返回包含黑名单列表的响应
%% @end
-spec page(cowboy_req:req(), map()) -> cowboy_req:req().
page(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    {Page, Size} = elib_param:page(Req0),
    Payload = user_denylist_logic:page(CurrentUid, Page, Size),
    elib_response:success(Req0, Payload).

%% @doc 添加黑名单
%% 将用户添加到黑名单
%%
%% @param Req0 Cowboy请求对象，包含目标用户ID
%% @param State 状态映射，包含 current_uid
%% @return 返回包含黑名单信息的响应
%% @end
-spec add(cowboy_req:req(), map()) -> cowboy_req:req().
add(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),

    PostVals = elib_param:post(Req0),
    DeniedUserId = maps:get(<<"denied_user_id">>, PostVals, ""),

    DeniedUserId2 = elib_hashids:decode(DeniedUserId),
    CreatedAt = user_denylist_logic:add(CurrentUid, DeniedUserId2),
    elib_response:success(Req0,
                           #{<<"user_id">> => elib_hashids:encode(CurrentUid),
                             <<"denied_user_id">> => DeniedUserId,
                             <<"created_at">> => CreatedAt}).

%% @doc 移除黑名单
%% 将用户从黑名单中移除
%%
%% @param Req0 Cowboy请求对象，包含目标用户ID
%% @param State 状态映射，包含 current_uid
%% @return 返回成功响应
%% @end
-spec remove(cowboy_req:req(), map()) -> cowboy_req:req().
remove(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),

    PostVals = elib_param:post(Req0),
    DeniedUserId = maps:get(<<"denied_user_id">>, PostVals, ""),
    DeniedUserId2 = elib_hashids:decode(DeniedUserId),

    user_denylist_logic:remove(CurrentUid, DeniedUserId2),
    elib_response:success(Req0).

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
