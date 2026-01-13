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

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 设备分页列表
%% 获取用户的设备列表（分页）
%%
%% @param Req0 Cowboy请求对象，包含分页参数
%% @param State 状态映射，包含 current_uid
%% @return 返回包含设备列表的响应
%% @end
-spec page(cowboy_req:req(), map()) -> cowboy_req:req().
page(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    {Page, Size} = elib_param:page(Req0),
    Payload = user_device_logic:page(CurrentUid, Page, Size),
    elib_response:success(Req0, Payload).

%% @doc 修改设备名称
%% 修改设备的显示名称
%%
%% @param Req0 Cowboy请求对象，包含设备ID和新名称
%% @param State 状态映射，包含 current_uid
%% @return 返回成功响应
%% @end
-spec change_name(cowboy_req:req(), map()) -> cowboy_req:req().
change_name(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    % ?DEBUG_LOG(PostVals),
    DID = maps:get(<<"did">>, PostVals, <<"">>),
    Name = maps:get(<<"name">>, PostVals, <<"">>),
    user_device_logic:change_name(CurrentUid, DID, Name),
    elib_response:success(Req0).

%% @doc 删除设备
%% 删除指定的设备
%%
%% @param Req0 Cowboy请求对象，包含设备ID
%% @param State 状态映射，包含 current_uid
%% @return 返回成功响应
%% @end
-spec delete(cowboy_req:req(), map()) -> cowboy_req:req().
delete(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    % ?DEBUG_LOG(PostVals),
    DID = maps:get(<<"did">>, PostVals, <<"">>),
    user_device_logic:delete(CurrentUid, DID),
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
