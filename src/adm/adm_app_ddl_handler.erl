-module(adm_app_ddl_handler).
%%%
% adm_app_ddl 控制器模块
% adm_app_ddl controller module
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

%% @doc 初始化 REST 处理器
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
            {ok, Ajax} = imboy_param:int(ajax, Req0, -2),
            % imboy_log:info(["AjaxAjaxAjaxAjaxAjax: ", Ajax, ";  Ajax"]),
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

%% @doc 处理索引请求
%% 当 ajax=1 时返回 JSON 数据，否则返回 HTML 页面
%% @param Method HTTP 方法
%% @param Ajax Ajax 标志，1 表示返回 JSON 数据
%% @param Req0 Cowboy 请求对象
%% @param State 状态映射
%% @return cowboy_req:req() 更新后的请求对象
-spec index(binary(), integer(), cowboy_req:req(), map()) -> cowboy_req:req().
index(<<"GET">>, 1, Req0, _State) ->
    {Page, Size} = imboy_param:page(Req0),
    Where = #{},
    Column = <<"id, ddl, down_ddl,old_vsn,new_vsn,status,updated_at,created_at">>,
    Tb = app_ddl_repo:tablename(),
    {ok, Payload} = imboy_pg:page_with_total(Tb, Column, Where, <<"id desc">>, Page, Size),
    imboy_response:success(Req0, Payload);
index(<<"GET">>, _, Req0, State) ->
    {ok, Body} = imboy_dtl:template(app_ddl_index_dtl, [
        {attach_token, ""}
    ] ++ imboy_dtl:imadm_param(State), imboy),

    % {ok, Body} = file:read_file(iolist_to_binary([code:priv_dir(imadm), "/template/adm_index_dtl.html"])),
    cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/html; charset=utf-8">>
        , <<"Access-Control-Allow-Origin">> => <<"*">>
    }, Body, Req0).


%% @doc 保存应用 DDL 配置
%% 处理 POST 请求，保存新的 DDL 配置信息
%% @param Req0 Cowboy 请求对象
%% @param State 状态映射，包含管理员用户 ID
%% @return cowboy_req:req() 更新后的请求对象
-spec save(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
save(<<"POST">>, Req0, State) ->
    % CurrentUid = maps:get(current_uid, State),
    % Uid = imboy_hashids:encode(CurrentUid),
    AdmUserId = maps:get(adm_user_id, State),

    PostVals = imboy_req:post_params(Req0),
    NewVsn = proplists:get_value(<<"new_vsn">>, PostVals, 0),
    OldVsn = proplists:get_value(<<"old_vsn">>, PostVals, 0),
    Status = proplists:get_value(<<"status">>, PostVals, 0),
    Ddl = proplists:get_value(<<"ddl">>, PostVals, <<>>),
    DownDdl = proplists:get_value(<<"down_ddl">>, PostVals, <<>>),
    app_ddl_ds:save(AdmUserId, NewVsn, OldVsn, Status, Ddl, DownDdl),
    imboy_response:success(Req0, PostVals, "success.").

%% @doc 删除应用 DDL 配置
%% 处理 DELETE 请求，删除指定的 DDL 配置
%% @param Req0 Cowboy 请求对象
%% @param State 状态映射
%% @return cowboy_req:req() 更新后的请求对象
-spec delete(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
delete(<<"DELETE">>, Req0, _State) ->
    % CurrentUid = maps:get(current_uid, State),
    % Uid = imboy_hashids:encode(CurrentUid),

    PostVals = imboy_req:post_params(Req0),
    Id = proplists:get_value(<<"id">>, PostVals, ""),

    Where = <<"status = 0 AND id = ", (ec_cnv:to_binary(Id))/binary>>,
    app_ddl_ds:delete(Where),
    imboy_response:success(Req0, PostVals, "success.").

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
