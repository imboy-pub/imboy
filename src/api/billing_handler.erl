-module(billing_handler).

%%%
% SaaS 计费 API 处理器 / SaaS billing API handler
%
% 管理端：套餐 CRUD（plan_create/plan_update/plan_list）。
% 租户端：订阅（subscribe/renew/cancel）、用量上报与配额查询、账单查询与支付。
%
% 路由 action 由 imboy_router 注入；认证区接口经 JWT。
% tenant_id 为逻辑字段：单租户场景默认取 0（无多租户实体表）；
%   如前端显式传 tenant_id 则采用之。
%%%

-behavior(cowboy_rest).

-export([init/2]).

-include("log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 =
        case Action of
            %% 管理端套餐管理
            plan_create -> plan_create(Req0, State);
            plan_update -> plan_update(Req0, State);
            plan_list -> plan_list(Req0, State);
            %% 租户端订阅
            subscribe -> subscribe(Req0, State);
            renew -> renew(Req0, State);
            cancel -> cancel(Req0, State);
            subscription -> subscription(Req0, State);
            %% 用量与配额
            report_usage -> report_usage(Req0, State);
            check_quota -> check_quota(Req0, State);
            %% 账单
            invoice_generate -> invoice_generate(Req0, State);
            invoice_pay -> invoice_pay(Req0, State);
            invoice_list -> invoice_list(Req0, State);
            false -> Req0
        end,
    {ok, Req1, State}.

%% ===================================================================
%% 管理端：套餐
%% ===================================================================

%% @doc 创建套餐
%% POST /v1/billing/plan
%% 参数: code, name, price(分), billing_period(month/year), quota_config(jsonb), description
-spec plan_create(cowboy_req:req(), map()) -> cowboy_req:req().
plan_create(Req0, _State) ->
    PostVals = elib_param:post(Req0),
    case billing_logic:create_plan(PostVals) of
        {ok, Id} ->
            elib_response:success(Req0, #{<<"id">> => Id}, "success.");
        {error, Msg} ->
            elib_response:error(Req0, Msg)
    end.

%% @doc 更新套餐
%% POST /v1/billing/plan/update
%% 参数: id（必填）+ 任意可改字段
-spec plan_update(cowboy_req:req(), map()) -> cowboy_req:req().
plan_update(Req0, _State) ->
    PostVals = elib_param:post(Req0),
    case maps:get(<<"id">>, PostVals, 0) of
        Id when is_integer(Id), Id > 0 ->
            Change = maps:remove(<<"id">>, PostVals),
            case billing_logic:update_plan(Id, Change) of
                {ok, _} -> elib_response:success(Req0, #{<<"id">> => Id}, "success.");
                {error, Msg} -> elib_response:error(Req0, Msg)
            end;
        _ ->
            elib_response:error(Req0, <<"套餐 id 不合法"/utf8>>)
    end.

%% @doc 套餐列表（上架）
%% GET /v1/billing/plan/list
-spec plan_list(cowboy_req:req(), map()) -> cowboy_req:req().
plan_list(Req0, _State) ->
    Plans = billing_logic:list_plans(),
    elib_response:success(Req0, #{<<"list">> => Plans}, "success.").

%% ===================================================================
%% 租户端：订阅
%% ===================================================================

%% @doc 订阅套餐
%% POST /v1/billing/subscribe
%% 参数: plan_id（必填）, tenant_id（可选，默认0）, trial（可选bool）
-spec subscribe(cowboy_req:req(), map()) -> cowboy_req:req().
subscribe(Req0, _State) ->
    PostVals = elib_param:post(Req0),
    PlanId = maps:get(<<"plan_id">>, PostVals, 0),
    TenantId = tenant_id(PostVals),
    case is_integer(PlanId) andalso PlanId > 0 of
        false ->
            elib_response:error(Req0, <<"套餐 id 不合法"/utf8>>);
        true ->
            Trial = maps:get(<<"trial">>, PostVals, false) =:= true,
            case billing_logic:subscribe(TenantId, PlanId, #{trial => Trial}) of
                {ok, Id} ->
                    elib_response:success(Req0, #{<<"subscription_id">> => Id}, "success.");
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

%% @doc 续费
%% POST /v1/billing/renew
%% 参数: subscription_id
-spec renew(cowboy_req:req(), map()) -> cowboy_req:req().
renew(Req0, _State) ->
    PostVals = elib_param:post(Req0),
    SubId = maps:get(<<"subscription_id">>, PostVals, 0),
    case is_integer(SubId) andalso SubId > 0 of
        false ->
            elib_response:error(Req0, <<"订阅 id 不合法"/utf8>>);
        true ->
            case billing_logic:renew(SubId) of
                {ok, EndMs} ->
                    elib_response:success(
                        Req0,
                        #{
                            <<"subscription_id">> => SubId,
                            <<"current_period_end_ms">> => EndMs
                        },
                        "success."
                    );
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

%% @doc 取消订阅
%% POST /v1/billing/cancel
%% 参数: subscription_id
-spec cancel(cowboy_req:req(), map()) -> cowboy_req:req().
cancel(Req0, _State) ->
    PostVals = elib_param:post(Req0),
    SubId = maps:get(<<"subscription_id">>, PostVals, 0),
    case is_integer(SubId) andalso SubId > 0 of
        false ->
            elib_response:error(Req0, <<"订阅 id 不合法"/utf8>>);
        true ->
            case billing_logic:cancel(SubId) of
                ok -> elib_response:success(Req0, #{<<"subscription_id">> => SubId}, "success.");
                {error, Msg} -> elib_response:error(Req0, Msg)
            end
    end.

%% @doc 查询当前租户订阅
%% GET /v1/billing/subscription?tenant_id=0
-spec subscription(cowboy_req:req(), map()) -> cowboy_req:req().
subscription(Req0, _State) ->
    GetVals = elib_param:get(<<"tenant_id">>, Req0, <<"0">>),
    TenantId = to_int(GetVals, 0),
    Sub = billing_logic:current_subscription(TenantId),
    elib_response:success(Req0, Sub, "success.").

%% ===================================================================
%% 用量与配额
%% ===================================================================

%% @doc 上报用量
%% POST /v1/billing/usage
%% 参数: subscription_id, metric, used(增量), period(可选)
-spec report_usage(cowboy_req:req(), map()) -> cowboy_req:req().
report_usage(Req0, _State) ->
    PostVals = elib_param:post(Req0),
    SubId = maps:get(<<"subscription_id">>, PostVals, 0),
    Metric = maps:get(<<"metric">>, PostVals, <<>>),
    Delta = maps:get(<<"used">>, PostVals, 0),
    Period = maps:get(<<"period">>, PostVals, undefined),
    case validate_usage(SubId, Metric, Delta) of
        {error, Msg} ->
            elib_response:error(Req0, Msg);
        ok ->
            case billing_logic:report_usage(SubId, Metric, Delta, Period) of
                {ok, Used} ->
                    elib_response:success(
                        Req0,
                        #{<<"metric">> => Metric, <<"used">> => Used},
                        "success."
                    );
                {error, quota_exceeded} ->
                    elib_response:error(Req0, <<"用量已超出套餐配额"/utf8>>);
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

%% @doc 配额查询
%% GET /v1/billing/quota?subscription_id=&metric=&period=
-spec check_quota(cowboy_req:req(), map()) -> cowboy_req:req().
check_quota(Req0, _State) ->
    SubId = to_int(elib_param:get(<<"subscription_id">>, Req0, <<"0">>), 0),
    Metric = elib_param:get(<<"metric">>, Req0, <<>>),
    Period0 = elib_param:get(<<"period">>, Req0, <<>>),
    Period =
        case Period0 of
            <<>> -> undefined;
            P -> P
        end,
    case SubId > 0 andalso byte_size(Metric) > 0 of
        false ->
            elib_response:error(Req0, <<"参数不合法"/utf8>>);
        true ->
            case billing_logic:check_quota(SubId, Metric, Period) of
                {ok, Payload} -> elib_response:success(Req0, Payload, "success.");
                {error, Msg} -> elib_response:error(Req0, Msg)
            end
    end.

%% ===================================================================
%% 账单
%% ===================================================================

%% @doc 生成账单
%% POST /v1/billing/invoice/generate
%% 参数: subscription_id
-spec invoice_generate(cowboy_req:req(), map()) -> cowboy_req:req().
invoice_generate(Req0, _State) ->
    PostVals = elib_param:post(Req0),
    SubId = maps:get(<<"subscription_id">>, PostVals, 0),
    case is_integer(SubId) andalso SubId > 0 of
        false ->
            elib_response:error(Req0, <<"订阅 id 不合法"/utf8>>);
        true ->
            case billing_logic:generate_invoice(SubId) of
                {ok, already_generated} ->
                    elib_response:success(
                        Req0,
                        #{<<"already_generated">> => true},
                        "success."
                    );
                {ok, Id} ->
                    elib_response:success(Req0, #{<<"invoice_id">> => Id}, "success.");
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end.

%% @doc 支付账单
%% POST /v1/billing/invoice/pay
%% 参数: invoice_no, payment_method
-spec invoice_pay(cowboy_req:req(), map()) -> cowboy_req:req().
invoice_pay(Req0, _State) ->
    PostVals = elib_param:post(Req0),
    InvoiceNo = maps:get(<<"invoice_no">>, PostVals, <<>>),
    Method = maps:get(<<"payment_method">>, PostVals, <<>>),
    case
        is_binary(InvoiceNo) andalso byte_size(InvoiceNo) > 0 andalso
            is_binary(Method) andalso byte_size(Method) > 0
    of
        false ->
            elib_response:error(Req0, <<"账单号或支付方式不能为空"/utf8>>);
        true ->
            case billing_logic:pay_invoice(InvoiceNo, Method) of
                {ok, Payload} -> elib_response:success(Req0, Payload, "success.");
                {error, Msg} -> elib_response:error(Req0, Msg)
            end
    end.

%% @doc 账单列表
%% GET /v1/billing/invoice/list?subscription_id=
-spec invoice_list(cowboy_req:req(), map()) -> cowboy_req:req().
invoice_list(Req0, _State) ->
    SubId = to_int(elib_param:get(<<"subscription_id">>, Req0, <<"0">>), 0),
    case SubId > 0 of
        false ->
            elib_response:error(Req0, <<"订阅 id 不合法"/utf8>>);
        true ->
            List = billing_logic:list_invoices(SubId),
            elib_response:success(Req0, #{<<"list">> => List}, "success.")
    end.

%% ===================================================================
%% Internal
%% ===================================================================

%% @doc 从入参取 tenant_id，缺省 0（单租户逻辑字段）
-spec tenant_id(map()) -> integer().
tenant_id(PostVals) ->
    case maps:get(<<"tenant_id">>, PostVals, 0) of
        T when is_integer(T), T >= 0 -> T;
        T when is_binary(T) -> to_int(T, 0);
        _ -> 0
    end.

%% @doc 用量上报参数校验
-spec validate_usage(term(), term(), term()) -> ok | {error, binary()}.
validate_usage(SubId, Metric, Delta) ->
    if
        not is_integer(SubId) orelse SubId =< 0 ->
            {error, <<"订阅 id 不合法"/utf8>>};
        not is_binary(Metric) orelse byte_size(Metric) =:= 0 ->
            {error, <<"指标键不能为空"/utf8>>};
        not is_integer(Delta) orelse Delta < 0 ->
            {error, <<"用量增量不合法"/utf8>>};
        true ->
            ok
    end.

%% @doc binary/整数 安全转整数
-spec to_int(term(), integer()) -> integer().
to_int(V, _Def) when is_integer(V) -> V;
to_int(V, Def) when is_binary(V) ->
    try
        binary_to_integer(V)
    catch
        _:_ -> Def
    end;
to_int(_, Def) ->
    Def.
