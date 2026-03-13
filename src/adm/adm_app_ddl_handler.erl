-module(adm_app_ddl_handler).

%%%
% adm_app_ddl 控制器模块
% adm_app_ddl controller module
%%%
-behavior(cowboy_rest).

-export([init/2]).

-include_lib("eunit/include/eunit.hrl").

-include("log.hrl").


-include("common.hrl").

-include("error_code.hrl").

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
    Req1 =
        case Action of
            index ->
                {ok, Ajax} = elib_param:int(ajax, Req0, -2),
                % elib_log:info(["AjaxAjaxAjaxAjaxAjax: ", Ajax, ";  Ajax"]),
                index(Method, Ajax, Req0, State);
            save ->
                save(Method, Req0, State);
            delete ->
                delete_action(Method, Req0);
            false ->
                Req0
        end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 处理索引请求（纯 JSON）
%% @param Method HTTP 方法
%% @param Ajax Ajax 标志，1 表示返回 JSON 数据
%% @param Req0 Cowboy 请求对象
%% @param State 状态映射
%% @return cowboy_req:req() 更新后的请求对象
-spec index(binary(), integer(), cowboy_req:req(), map()) -> cowboy_req:req().
index(<<"GET">>, _Ajax, Req0, _State) ->
    {Page, Size} = elib_param:page(Req0),
    Where = #{},
    Column = <<"id, ddl, down_ddl,old_vsn,new_vsn,status,updated_at,created_at">>,
    Tb = app_ddl_repo:tablename(),
    {ok, Payload} = elib_pg:page_with_total(Tb, Column, Where, <<"id desc">>, Page, Size),
    elib_response:success(Req0, Payload);
index(_Method, _Ajax, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

%% @doc 保存应用 DDL 配置
%% 处理 POST 请求，保存新的 DDL 配置信息
-spec save(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
save(<<"POST">>, Req0, State) ->
    % CurrentUid = auth_ds:current_uid(State),
    % Uid = elib_hashids:encode(CurrentUid),
    AdmUserId = maps:get(adm_user_id, State),

    PostVals = elib_param:post(Req0),
    NewVsn = maps:get(<<"new_vsn">>, PostVals, 0),
    OldVsn = maps:get(<<"old_vsn">>, PostVals, 0),
    Status = maps:get(<<"status">>, PostVals, 0),
    Ddl = maps:get(<<"ddl">>, PostVals, <<>>),
    DownDdl = maps:get(<<"down_ddl">>, PostVals, <<>>),
    _ = app_ddl_ds:save(AdmUserId, NewVsn, OldVsn, Status, Ddl, DownDdl),
    elib_response:success(Req0, PostVals, <<"success."/utf8>>);
save(_, Req0, _State) ->
    Req0.

%% @doc 删除应用 DDL 配置
%% 处理 DELETE 请求，删除指定的 DDL 配置
%% @param Method HTTP 方法
%% @param Req0 Cowboy 请求对象
%% @return cowboy_req:req() 更新后的请求对象
-spec delete_action(binary(), cowboy_req:req()) -> cowboy_req:req().
delete_action(<<"DELETE">>, Req0) ->
    PostVals = elib_param:post(Req0),
    Id = maps:get(<<"id">>, PostVals, ""),

    % 使用安全的参数化查询，避免 SQL 注入
    case app_ddl_ds:delete(Id) of
        {ok, _Count} ->
            elib_response:success(Req0, PostVals, <<"success."/utf8>>);
        {error, Reason} ->
            ?DEBUG_LOG("删除 DDL 失败: ~p", [Reason]),
            elib_response:error(Req0, <<"删除失败"/utf8>>, ?ERR_INTERNAL_SERVER_ERROR)
    end;
delete_action(_Method, Req0) ->
    Req0.
