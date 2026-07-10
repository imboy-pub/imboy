-module(agent_mandate_handler).

%%%
% Agent 受控支付授权 API / Agent payment mandate API（Phase 4 T4.3 收尾）
%
% 路由（JWT 认证，见 imboy_router ApiV1Routes 认证块）：
%   POST /api/v1/agent/mandate/authorize  -> owner 授权 agent 代付（额度/有效期）
%   POST /api/v1/agent/mandate/revoke     -> 撤销授权
%   GET  /api/v1/agent/mandate/active     -> ?agent_uid 查当前有效授权
%
% ⚠️ 金钱红线：owner_uid **只取 auth_ds:current_uid(State)**（JWT 身份），
%    绝不读 body 里的 owner_uid——用户只能授权从自己钱包扣款。授权/归属判定全在 logic 层。
%%%

-behavior(cowboy_rest).

-export([init/2]).

-include("log.hrl").
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
            authorize -> authorize(Method, Req0, State);
            revoke -> revoke(Method, Req0, State);
            active -> active(Method, Req0, State);
            _ -> Req0
        end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal
%% ===================================================================

-spec authorize(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
authorize(<<"POST">>, Req0, State) ->
    OwnerUid = auth_ds:current_uid(State),
    Params = elib_param:post(Req0),
    case agent_payment_mandate_logic:authorize(OwnerUid, Params) of
        {ok, R} -> elib_response:success(Req0, R, <<"授权成功"/utf8>>);
        {error, Reason} -> elib_response:error(Req0, reason_msg(Reason), ?ERR_BAD_REQUEST)
    end;
authorize(_, Req0, _State) ->
    method_not_allowed(Req0).

-spec revoke(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
revoke(<<"POST">>, Req0, State) ->
    OwnerUid = auth_ds:current_uid(State),
    Params = elib_param:post(Req0),
    MandateId = ec_cnv:to_integer(maps:get(<<"mandate_id">>, Params, 0)),
    case agent_payment_mandate_logic:revoke(OwnerUid, MandateId) of
        {ok, _} -> elib_response:success(Req0, #{}, <<"撤销成功"/utf8>>);
        {error, Reason} -> elib_response:error(Req0, reason_msg(Reason), ?ERR_BAD_REQUEST)
    end;
revoke(_, Req0, _State) ->
    method_not_allowed(Req0).

-spec active(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
active(<<"GET">>, Req0, State) ->
    OwnerUid = auth_ds:current_uid(State),
    {ok, AgentUid} = elib_param:int(agent_uid, Req0, 0),
    case agent_payment_mandate_logic:get_active(OwnerUid, AgentUid) of
        {ok, M} -> elib_response:success(Req0, M, <<"success."/utf8>>);
        {error, notfound} -> elib_response:success(Req0, #{}, <<"无有效授权"/utf8>>);
        {error, Reason} -> elib_response:error(Req0, reason_msg(Reason), ?ERR_BAD_REQUEST)
    end;
active(_, Req0, _State) ->
    method_not_allowed(Req0).

%% 错误原子 → 用户可读消息（不泄露内部结构）
-spec reason_msg(atom()) -> binary().
reason_msg(invalid_params) -> <<"参数不合法"/utf8>>;
reason_msg(not_agent_owner) -> <<"无权授权该 Agent"/utf8>>;
reason_msg(not_authorized) -> <<"无权操作"/utf8>>;
reason_msg(create_failed) -> <<"授权失败"/utf8>>;
reason_msg(_) -> <<"操作失败"/utf8>>.

-spec method_not_allowed(cowboy_req:req()) -> cowboy_req:req().
method_not_allowed(Req0) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).
