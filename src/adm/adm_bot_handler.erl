-module(adm_bot_handler).

%%%
% adm_bot 控制器模块
% Bot 管理后台 API——平台对开发者 Bot 的处置能力：
% 浏览（list/detail）+ 启停（disable/enable，平台处置权，无属主校验）。
% 属主自身的启停走 /api/v1/bot/disable|enable（bot_logic:set_status/3）。
%%%

-behavior(cowboy_rest).

-export([init/2]).

-include("log.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 初始化 REST 处理器
-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Method = cowboy_req:method(Req0),
    Req1 =
        case imboy_plugin_registry:required_feature(admin, adm_bot_handler, Action) of
            undefined ->
                dispatch(Action, Method, Req0, State);
            Feature ->
                case imboy_feature:ensure_enabled(Req0, Feature) of
                    ok ->
                        dispatch(Action, Method, Req0, State);
                    {error, RespReq} ->
                        RespReq
                end
        end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

-spec dispatch(atom() | false, binary(), cowboy_req:req(), map()) -> cowboy_req:req().
dispatch(list, Method, Req0, State) ->
    guard(<<"bots:read">>, Method, Req0, State, fun list/2);
dispatch(detail, Method, Req0, State) ->
    guard(<<"bots:read">>, Method, Req0, State, fun detail/2);
dispatch(disable, Method, Req0, State) ->
    guard(<<"bots:update">>, Method, Req0, State, fun(M, R) -> change_status(M, R, 0) end);
dispatch(enable, Method, Req0, State) ->
    guard(<<"bots:update">>, Method, Req0, State, fun(M, R) -> change_status(M, R, 1) end);
dispatch(_, _, Req0, _State) ->
    Req0.

%% @doc RBAC 权限门（对齐 adm_group/adm_channel 的 adm_acl:ensure_permission 范式）
-spec guard(binary(), binary(), cowboy_req:req(), map(), fun()) -> cowboy_req:req().
guard(Permission, Method, Req0, State, Action) ->
    case adm_acl:ensure_permission(State, Permission, Req0) of
        ok -> Action(Method, Req0);
        {error, RespReq} -> RespReq
    end.

%% @doc 分页列出全部 Bot（含属主昵称/头像，供后台检索）
-spec list(binary(), cowboy_req:req()) -> cowboy_req:req().
list(<<"GET">>, Req0) ->
    {Page, Size} = elib_param:page(Req0),
    case bot_repo:page(Page, Size) of
        {ok, Result} ->
            elib_response:success(Req0, Result);
        {error, Reason} ->
            ?ERROR_LOG("adm_bot list error ~p~n", [Reason]),
            elib_response:error(Req0, <<"查询失败"/utf8>>)
    end;
list(_, Req0) ->
    Req0.

%% @doc Bot 详情（复用 bot_logic:get/1 的敏感字段过滤）
-spec detail(binary(), cowboy_req:req()) -> cowboy_req:req().
detail(<<"GET">>, Req0) ->
    Qs = cowboy_req:parse_qs(Req0),
    case proplists:get_value(<<"bot_id">>, Qs) of
        undefined ->
            elib_response:error(Req0, <<"Bot ID 不能为空"/utf8>>);
        BotIdBin ->
            case bot_logic:get(ec_cnv:to_integer(BotIdBin)) of
                {ok, Bot} ->
                    elib_response:success(Req0, Bot);
                {error, Reason} ->
                    elib_response:error(Req0, Reason)
            end
    end;
detail(_, Req0) ->
    Req0.

%% @doc 管理端启停（平台处置权，无属主校验）
-spec change_status(binary(), cowboy_req:req(), -1 | 0 | 1) -> cowboy_req:req().
change_status(<<"POST">>, Req0, Status) ->
    case elib_req:body(Req0, []) of
        {ok, Body, _Req1} ->
            case ec_cnv:to_integer(maps:get(<<"bot_id">>, Body, 0)) of
                BotId when BotId > 0 ->
                    case bot_logic:admin_set_status(BotId, Status) of
                        {ok, Result} ->
                            elib_response:success(Req0, Result);
                        {error, Reason} ->
                            elib_response:error(Req0, Reason)
                    end;
                _ ->
                    elib_response:error(Req0, <<"Bot ID 不能为空"/utf8>>)
            end;
        {error, _} ->
            elib_response:error(Req0, <<"参数错误"/utf8>>)
    end;
change_status(_, Req0, _Status) ->
    Req0.
