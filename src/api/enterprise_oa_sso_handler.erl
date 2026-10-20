-module(enterprise_oa_sso_handler).

-moduledoc "OA 一次性 SSO code 签发端点壳（EPGZ-05 HUMAN-SSO-01）。".

-behavior(cowboy_rest).

-export([init/2]).

-include("error_code.hrl").
-include("log.hrl").

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
            code -> code(Method, Req0, State);
            entries -> entries(Method, Req0, State);
            _ -> Req0
        end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal
%% ===================================================================

entries(<<"GET">>, Req0, State) ->
    %% organization_id 查询参数容忍并忽略（定位修订：发现不依赖 org；
    %% 旧客户端仍会携带）。判定只要求 Human JWT 已认证。
    Uid = auth_ds:current_uid(State),
    case enterprise_oa_sso_logic:entries(Uid) of
        {ok, Result} -> elib_response:success(Req0, Result);
        {error, {401, Msg}} -> elib_response:error_with_status(Req0, 401, Msg, 401);
        {error, {Code, Msg}} -> elib_response:error(Req0, Msg, Code)
    end;
entries(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

%% @doc HUMAN-SSO-01：Human JWT 身份只取 auth_ds:current_uid/1（认证中间件
%% 注入），绝不读 body 里的用户标识。
-spec code(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
code(<<"POST">>, Req0, State) ->
    Uid = auth_ds:current_uid(State),
    Params = elib_param:post(Req0),
    case enterprise_oa_sso_logic:issue_code(Uid, Params) of
        {ok, Result} ->
            elib_response:success(Req0, Result);
        {error, {?ERR_UNAUTHORIZED = Code, Msg}} ->
            %% 认证边界：真实 HTTP 401（合同 §3.5；elib 同款先例）
            elib_response:error_with_status(Req0, Code, Msg, Code);
        {error, {Code, Msg}} ->
            elib_response:error(Req0, Msg, Code)
    end;
code(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).
