-module(enterprise_oa_sso_exchange_handler).

-moduledoc "OA 一次性 SSO code 交换端点壳（EPGZ-05 INT-14）。".
%%%
% INT-14 OA 一次性 SSO code 交换端点壳（EPGZ-05，合同 §4）。
%
% 路由（A0 W4 经 Router lease 串行登记，本模块不接线 Router）：
%   POST /api/internal/v1/oa/sso/exchange ->
%       {Path, enterprise_oa_sso_exchange_handler, #{action => exchange}}
%   —— 必须经 enterprise_internal_middleware（A2 认证链：credential →
%      active/expiry → application active → organization active →
%      scope sso:exchange → rate internal_sso fail-closed → INT-14 幂等豁免），
%      不得进入人类 JWT/签名门（NEG-X02）。ctx 由中间件注入
%      handler_opts.enterprise_internal（atom 键 map）。
%
% 壳职责：参数解析 + 取认证产物 ctx + 调 logic + stable 错误信封
% （enterprise_internal_error）。Idempotency-Key 不要求（manifest INT-14
% 行 single_use_code：code 即幂等键，重放=拒绝）。
%%%

-behavior(cowboy_rest).

-export([init/2]).

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
            exchange -> exchange(Method, Req0, State);
            _ -> Req0
        end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal
%% ===================================================================

%% @doc INT-14：成功回裸 JSON payload（合同 §4.3 五字段，无 IMBoy 凭证）；
%% 失败一律 stable 错误信封（enterprise_internal_error:reply/2，真实 HTTP
%% 状态码映射以 A2 冻结表为准）。
-spec exchange(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
exchange(<<"POST">>, Req0, State) ->
    Ctx = maps:get(enterprise_internal, State, #{}),
    Params = elib_param:post(Req0),
    case enterprise_oa_sso_logic:exchange(Ctx, Params) of
        {ok, Payload} ->
            Body = jsone:encode(Payload),
            cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Body, Req0);
        {error, Code} when is_binary(Code) ->
            enterprise_internal_error:reply(Req0, Code);
        {error, _Other} ->
            enterprise_internal_error:reply(Req0, <<"internal_error">>)
    end;
exchange(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).
