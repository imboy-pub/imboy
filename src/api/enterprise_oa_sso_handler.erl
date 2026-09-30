-module(enterprise_oa_sso_handler).

%%%
% HUMAN-SSO-01 OA 一次性 SSO code 签发端点壳（EPGZ-05，合同 §3）。
%
% 路由（A0 W4 经 Router lease 串行登记，本模块不接线 Router）：
%   POST /api/v1/oa/sso/code -> {"/api/v1/oa/sso/code",
%                                enterprise_oa_sso_handler, #{action => code}}
%   —— 必须挂在 /api/v1 认证块（Human JWT：auth_middleware_api_v1），
%      不得进入 imboy_router:open()/0（NEG-X01：Application Credential
%      只携带 Bearer ib_int_*，不会是人类 JWT，认证链 401 拒绝）。
%
% 壳职责（仓内惯例）：参数解析 + 调 logic + 信封；全部判定在
% enterprise_oa_sso_logic。错误映射（合同 §3.5，整数信封）：
%   401 真实 HTTP 401 | 404/403/400/500 HTTP 200 + envelope code。
%%%

-behavior(cowboy_rest).

-export([init/2]).

-include("error_code.hrl").

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
    Uid = auth_ds:current_uid(State),
    {ok, RawId} = elib_param:binary(organization_id, Req0, <<>>),
    OrgId =
        case re:run(RawId, <<"^[1-9][0-9]{0,18}$">>, [{capture, none}]) of
            match -> binary_to_integer(RawId);
            nomatch -> 0
        end,
    case enterprise_oa_sso_logic:entries(Uid, OrgId) of
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
