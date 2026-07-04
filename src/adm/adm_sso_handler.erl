-module(adm_sso_handler).

%%%
% SSO 外部认证配置管理控制器模块
% SSO external authentication config management controller
%
% 路由（前端 baseURL=/api/adm）：
%   GET  /adm/sso/config -> {ldap?, saml?, oauth2?}
%   POST /adm/sso/config -> 按 provider upsert（body 即单个 provider 配置对象）
%   POST /adm/sso/test   -> {success, message}（连通性 + 字段校验，MVP）
%
% 权限：仅超管（settings:view，role 1 持有）。
%%%

-behavior(cowboy_rest).

-export([init/2]).

-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").
-include("error_code.hrl").

-define(PERMISSION, <<"settings:view">>).

%% ===================================================================
%% API
%% ===================================================================

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Method = cowboy_req:method(Req0),
    Req1 =
        case Action of
            config ->
                config(Method, Req0, State);
            test ->
                test(Method, Req0, State);
            false ->
                Req0
        end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc GET 读取 / POST 保存 SSO 配置
-spec config(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
config(<<"GET">>, Req0, State) ->
    case adm_acl:ensure_permission(State, ?PERMISSION, Req0) of
        ok ->
            case sso_logic:get_config() of
                {ok, Payload} ->
                    elib_response:success(Req0, Payload);
                {error, Reason} ->
                    ?LOG_ERROR("adm_sso_handler:config get error ~p", [Reason]),
                    elib_response:error(Req0, <<"读取 SSO 配置失败"/utf8>>, ?ERR_BAD_REQUEST)
            end;
        {error, Req1} ->
            Req1
    end;
config(<<"POST">>, Req0, State) ->
    case adm_acl:ensure_permission(State, ?PERMISSION, Req0) of
        ok ->
            PostVals = elib_param:post(Req0),
            case sso_logic:save_config(PostVals) of
                {ok, Result} ->
                    elib_response:success(Req0, Result, <<"保存成功"/utf8>>);
                {error, Reason} ->
                    elib_response:error(Req0, Reason, ?ERR_BAD_REQUEST)
            end;
        {error, Req1} ->
            Req1
    end;
config(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

%% @doc POST 连通性/字段校验测试
-spec test(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
test(<<"POST">>, Req0, State) ->
    case adm_acl:ensure_permission(State, ?PERMISSION, Req0) of
        ok ->
            PostVals = elib_param:post(Req0),
            Provider = maps:get(<<"provider">>, PostVals, <<>>),
            Config = maps:get(<<"config">>, PostVals, #{}),
            {Success, Message} = sso_logic:test_connection(Provider, Config),
            elib_response:success(Req0, #{
                <<"success">> => Success,
                <<"message">> => Message
            });
        {error, Req1} ->
            Req1
    end;
test(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).
