-module(adm_app_version_handler).

%%%
% adm_app_version 控制器模块
% adm_app_version controller module
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

%% @doc 初始化应用版本管理 REST 处理器
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
            save ->
                save(Method, Req0, State);
            delete ->
                delete(Method, Req0, State);
            false ->
                Req0
        end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 处理应用版本索引请求（纯 JSON）
%% @param Method HTTP 方法
%% @param Ajax Ajax 标志，1 表示返回 JSON 数据
%% @param Req0 Cowboy 请求对象
%% @param State 状态映射
%% @return cowboy_req:req() 更新后的请求对象
-spec index(binary(), integer(), cowboy_req:req(), map()) -> cowboy_req:req().
index(<<"GET">>, _Ajax, Req0, _State) ->
    {Page, Size} = elib_param:page(Req0),
    Where = #{},
    Column = <<"*">>,
    Tb = app_version_repo:tablename(),
    OrderBy = <<"sort desc, updated_at desc">>,
    {ok, P} = elib_pg:page_with_total(Tb, Column, Where, OrderBy, Page, Size),
    elib_response:success(Req0, P);
index(_Method, _Ajax, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

%% @doc 保存应用版本信息
%% 处理 POST 请求，保存或更新应用版本配置
-spec save(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
save(<<"POST">>, Req0, _State) ->
    PostVals = elib_param:post(Req0),

    RCode = maps:get(<<"region_code">>, PostVals, <<"cn">>),
    Type = maps:get(<<"type">>, PostVals, <<>>),
    PkgName = maps:get(<<"package_name">>, PostVals, <<>>),
    AppName = maps:get(<<"app_name">>, PostVals, <<>>),
    Vsn = maps:get(<<"vsn">>, PostVals, <<>>),
    SKey = maps:get(<<"sign_key">>, PostVals, <<>>),
    DUrl = maps:get(<<"download_url">>, PostVals, <<>>),
    Desc = maps:get(<<"description">>, PostVals, <<>>),
    ForceUpdate = maps:get(<<"force_update">>, PostVals, 2),
    Status = maps:get(<<"status">>, PostVals, 0),
    Id = maps:get(<<"id">>, PostVals, 0),
    Data =
        #{id => Id,
          region_code => RCode,
          type => Type,
          package_name => PkgName,
          app_name => AppName,
          vsn => Vsn,
          sort => adm_app_version_logic:vsn_sort(Vsn),
          sign_key => SKey,
          download_url => DUrl,
          description => Desc,
          force_update => ec_cnv:to_integer(ForceUpdate),
          status => ec_cnv:to_integer(Status)},
    _ = adm_app_version_logic:save(Data),
    elib_response:success(Req0, PostVals, <<"success."/utf8>>);
save(_, Req0, _State) ->
    Req0.

%% @doc 删除应用版本信息
%% 处理 DELETE 请求，删除指定的应用版本配置
-spec delete(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
delete(<<"DELETE">>, Req0, _State) ->
    PostVals = elib_param:post(Req0),
    Id = maps:get(<<"id">>, PostVals, ""),

    Where = <<"status = 0 AND id = ", (ec_cnv:to_binary(Id))/binary>>,
    adm_app_version_logic:delete(Where),
    elib_response:success(Req0, PostVals, <<"success."/utf8>>);
delete(_, Req0, _State) ->
    Req0.

%% ===================================================================
%% EUnit tests.
%% ===================================================================
