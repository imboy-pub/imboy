-module(app_version_handler).

%%%
% app_version 控制器模块
% app_version controller module
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

%% @doc 初始化版本检查处理器
%% 根据请求中的 action 参数调用相应的处理函数
%%
%% @param Req0 Cowboy请求对象
%% @param State0 状态映射，包含 action 参数
%% @return {ok, Req1, State} 处理后的请求对象和状态
%% @end
-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    % ?DEBUG_LOG(State0),
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Method = cowboy_req:method(Req0),
    Req1 =
        case Action of
            check ->
                check(Method, Req0, State);
            false ->
                Req0
        end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 检查应用版本
%% 检查客户端版本是否需要更新
%%
%% @param Method HTTP方法（GET）
%% @param Req0 Cowboy请求对象，包含版本和地区信息
%% @param _State 状态映射
%% @return 返回包含版本信息和更新状态的响应
%% @end
-spec check(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
check(<<"GET">>, Req0, _State) ->
    Cos = cowboy_req:header(<<"cos">>, Req0, <<"web">>),
    % elib_log:info(Cos),
    #{vsn := Vsn} = cowboy_req:match_qs([{vsn, [], <<>>}], Req0),
    #{region_code := RegionCode} = cowboy_req:match_qs([{region_code, [], <<>>}], Req0),

    Res = app_version_repo:find(Cos, RegionCode),
    % ?DEBUG_LOG([Res]),
    LastVsn = maps:get(<<"vsn">>, Res, <<"0.0.0">>),
    % ?DEBUG_LOG([LastVsn, Res, WhereMap]),
    %  updatable = [true | false]
    elib_response:success(Req0, Res#{<<"updatable">> => ec_semver:lt(Vsn, LastVsn)}).

%% ===================================================================
%% EUnit tests.
%% ===================================================================

