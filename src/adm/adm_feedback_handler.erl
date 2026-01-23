-module(adm_feedback_handler).

%%%
% adm_feedback 控制器模块
% adm_feedback controller module
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

%% @doc 初始化反馈管理处理器
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
            index ->
                {ok, Ajax} = elib_param:int(ajax, Req0, -2),
                % elib_log:info(["AjaxAjaxAjaxAjaxAjax: ", Ajax, ";  Ajax"]),
                index(Method, Ajax, Req0, State);
            reply ->
                reply(Method, Req0, State);
            false ->
                Req0
        end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 处理反馈列表页面
-spec index(binary(), integer(), cowboy_req:req(), map()) -> cowboy_req:req().
index(<<"GET">>, 1, Req0, _State) ->
    {Page, Size} = elib_param:page(Req0),
    % Where2 = <<"status > 0 AND ", Where/binary>>,
    Where = #{status => {op, <<">">>, -2}},
    Column =
        <<"id as feedback_id, user_id, device_id, client_operating_system, "
          "client_operating_system_vsn, type, rating, contact_detail, "
          "body, attach, reply_count, status, updated_at, created_at, "
          "app_vsn">>,
    Tb = feedback_repo:tablename(),
    {ok, P} = elib_pg:page_with_total(Tb, Column, Where, <<"id desc">>, Page, Size),
    elib_response:success(Req0, P);
index(<<"GET">>, _, Req0, State) ->
    {ok, Body} =
        imboy_dtl:template(feedback_index_dtl,
                           [{attach_token, ""}] ++ imboy_dtl:imadm_param(State),
                           imboy),

    % {ok, Body} = file:read_file(iolist_to_binary([code:priv_dir(imadm), "/template/adm_index_dtl.html"])),
    cowboy_req:reply(200,
                     #{<<"content-type">> => <<"text/html; charset=utf-8">>,
                       <<"Access-Control-Allow-Origin">> => <<"*">>},
                     Body,
                     Req0).

%% @doc 处理反馈回复
%% 管理员回复用户反馈，记录回复内容和回复者信息
%% @param Method HTTP 方法
%% @param Req0 Cowboy 请求对象
%% @param State 状态映射，包含管理员 ID 等信息
%% @return cowboy_req:req() 更新后的请求对象
-spec reply(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
reply(<<"POST">>, Req0, State) ->
    % Uid = elib_hashids:encode(CurrentUid),
    AdmUserId = maps:get(adm_user_id, State),
    Key = {adm_user_sample, AdmUserId},
    U = adm_user_logic:find(AdmUserId, <<"id,nickname">>, Key),
    Nickname = maps:get(<<"nickname">>, U),
    % replier_user_id
    PostVals = elib_param:post(Req0),
    % FeedbackId = proplists:get_value(<<"feedback_id">>, PostVals),
    % FeedbackId = elib_param:int(<<"FeedbackId">>, Req0, 0),
    {ok, FeedbackId} =
        case string:to_integer(
                 ec_cnv:to_list(
                     maps:get(<<"feedback_id">>, PostVals, <<"0">>)))
        of
            {error, _} ->
                {ok, 0};
            {Val2, _} ->
                {ok, Val2}
        end,
    % ?DEBUG_LOG(["FeedbackId", FeedbackId, PostVals]),
    if is_integer(FeedbackId), FeedbackId > 0 ->
           feedback_ds:add_reply(#{<<"feedback_id">> => FeedbackId,
                                   <<"feedback_reply_pid">> =>
                                       maps:get(<<"feedback_reply_pid">>, PostVals, 0),
                                   <<"replier_user_id">> => AdmUserId,
                                   <<"replier_name">> => Nickname,
                                   <<"body">> => maps:get(<<"body">>, PostVals, ""),
                                   <<"created_at">> => elib_dt:now()}),
           elib_response:success(Req0, PostVals, "success.");
       true ->
           elib_response:error(Req0)
    end.

%% ===================================================================
%% EUnit tests.
%% ===================================================================

