-module(adm_attach_handler).

%%%
% adm_attach 控制器模块
% adm_attach controller module
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

%% @doc 初始化附件鉴权处理器
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
    Req1 =
        case Action of
            auth ->
                auth(Method, Req0, State);
            stats ->
                stats(Method, Req0, State);
            index ->
                index(Method, Req0, State);
            false ->
                Req0
        end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 处理附件鉴权请求
%% 验证附件 URI 的访问权限，检查是否需要认证
%% @param Method HTTP 方法
%% @param Req0 Cowboy 请求对象
%% @param State 状态映射
%% @return cowboy_req:req() 更新后的请求对象
-spec auth(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
% 获取特定附近的反馈token参数
auth(<<"POST">>, Req0, _State) ->
    PostVals = elib_param:post(Req0),
    Uri = maps:get(<<"uri">>, PostVals, ""),
    Result = [elib_uri:check_auth(I) || I <- binary:split(Uri, <<",">>)],
    elib_response:success(Req0, #{<<"uri">> => Result}, "success.");
auth(_, Req0, _State) ->
    Req0.

%% @doc 附件统计信息（管理后台）
%% 返回总文件数、总大小、各类型文件计数、今日上传统计
-spec stats(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
stats(<<"GET">>, Req0, _State) ->
    Result = attachment_repo:stats(),
    elib_response:success(Req0, Result, "success.");
stats(_, Req0, _State) ->
    Req0.

%% @doc 附件列表（管理后台，支持分页 + 筛选）
%% 查询参数: page, size, mime_type（前缀匹配）, keyword（名称/md5模糊匹配）
-spec index(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
index(<<"GET">>, Req0, _State) ->
    {Page, Size} = elib_param:page(Req0),
    Qs = cowboy_req:parse_qs(Req0),
    MimeType = proplists:get_value(<<"mime_type">>, Qs, undefined),
    Keyword = proplists:get_value(<<"keyword">>, Qs, undefined),
    Opts = #{mime_type => MimeType, keyword => Keyword},
    case attachment_repo:page(Page, Size, Opts) of
        {ok, Result} ->
            elib_response:success(Req0, Result, "success.");
        {error, _Reason} ->
            elib_response:error(Req0, "查询失败")
    end;
index(_, Req0, _State) ->
    Req0.

%% ===================================================================
%% EUnit tests.
%% ===================================================================
