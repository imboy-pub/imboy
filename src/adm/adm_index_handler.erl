-module(adm_index_handler).
%%%
% adm_index 控制器模块
% adm_index controller module
%%%
-behavior(cowboy_rest).

-export([init/2]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include("include/log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("include/common.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 初始化管理后台首页处理器
%% 根据请求中的 action 参数分发到不同的处理函数
%% @param Req0 Cowboy 请求对象
%% @param State0 状态映射，包含 action 等信息
%% @return {ok, Req, State} 更新后的请求和状态
-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    % ?DEBUG_LOG(State),
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Method = cowboy_req:method(Req0),
    Req1 = case Action of
        index ->
            index(Method, Req0, State);
        welcome ->
            welcome(Method, Req0, State);
        false ->
            Req0
    end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 处理管理后台首页请求
%% 返回包含在线用户和设备统计信息的 HTML 页面
%% @param Method HTTP 方法
%% @param Req0 Cowboy 请求对象
%% @param State 状态映射
%% @return cowboy_req:req() 更新后的请求对象
-spec index(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
index(<<"GET">>, Req0, State) ->
    % AdmUserId = maps:get(adm_user_id, State, []),
    {ok, Body} = imboy_dtl:template(adm_index_dtl, [
         {"coversation_online_user", imboy_syn:count_user()}
         , {"coversation_online_device", imboy_syn:count()}
    ] ++ imboy_dtl:imadm_param(State), imboy),

    % {ok, Body} = file:read_file(iolist_to_binary([code:priv_dir(imadm), "/template/adm_index_dtl.html"])),
    cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/html; charset=utf-8">>
        , <<"Access-Control-Allow-Origin">> => <<"*">>
    }, Body, Req0).


%% @doc 处理欢迎页面请求
%% 返回包含在线用户和设备统计信息的欢迎页面
%% @param Method HTTP 方法
%% @param Req0 Cowboy 请求对象
%% @param State 状态映射
%% @return cowboy_req:req() 更新后的请求对象
-spec welcome(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
welcome(<<"GET">>, Req0, State) ->
    % AdmUserId = maps:get(adm_user_id, State, []),
    {ok, Body} = imboy_dtl:template(adm_welcome_dtl, [
         {"coversation_online_user", imboy_syn:count_user()}
         , {"coversation_online_device", imboy_syn:count()}
    ] ++ imboy_dtl:imadm_param(State), imboy),

    % {ok, Body} = file:read_file(iolist_to_binary([code:priv_dir(imadm), "/template/adm_welcome_dtl.html"])),
    cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/html; charset=utf-8">>
        , <<"Access-Control-Allow-Origin">> => <<"*">>
    }, Body, Req0).

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
