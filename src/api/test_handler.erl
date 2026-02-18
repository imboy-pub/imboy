-module(test_handler).

%%%
% test 控制器模块
% test controller module
%%%
-behavior(cowboy_rest).

-export([init/2]).

-include_lib("eunit/include/eunit.hrl").

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
            req_get ->
                req_get(Req0, State);
            req_post ->
                req_post(Req0, State);
            false ->
                Req0
        end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 测试GET请求
%% 用于测试API的GET请求端点
%%
%% @param Req0 Cowboy请求对象，包含查询参数
%% @param _State 状态映射
%% @return 返回包含测试数据的响应
%% @end
-spec req_get(cowboy_req:req(), map()) -> cowboy_req:req().
req_get(Req0, _State) ->
    % CurrentUid = auth_ds:current_uid(State),
    % Uid = elib_hashids:encode(CurrentUid),
    Qs3 = cowboy_req:parse_qs(Req0),
    Type = proplists:get_value(<<"type">>, Qs3, undefined),
    A = proplists:get_value(<<"a">>, Qs3, 0),

    % test_logic:demo(CurrentUid, Val1, Val2),
    elib_response:success(Req0,
                           #{<<"a">> => A,
                             <<"config">> => elib_cnv:implode("", [config_ds:env(test)]),
                             <<"type">> => Type,
                             <<"host">> => cowboy_req:header(<<"host">>, Req0),
                             <<"client">> => cowboy_req:header(<<"client">>, Req0),
                             <<"content-type">> => cowboy_req:header(<<"content-type">>, Req0)},
                           "success.").

%% @doc 测试POST请求
%% 用于测试API的POST请求端点
%%
%% @param Req0 Cowboy请求对象，包含POST数据
%% @param _State 状态映射
%% @return 返回包含POST数据的响应
%% @end
-spec req_post(cowboy_req:req(), map()) -> cowboy_req:req().
req_post(Req0, _State) ->
    % CurrentUid = auth_ds:current_uid(State),
    % Uid = elib_hashids:encode(CurrentUid),
    PostVals = elib_param:post(Req0),
    % Val1 = proplists:get_value(<<"val1">>, PostVals, ""),
    % Val2 = proplists:get_value(<<"val2">>, PostVals, ""),
    elib_response:success(Req0, PostVals, "success.").

%% ===================================================================
%% EUnit tests.
%% ===================================================================

