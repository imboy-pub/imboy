-module(billing_handler).

%%%
% SaaS 计费 API 处理器 / SaaS billing API handler
%
% 套餐列表（plan_list，登录可读）；管理端套餐 CRUD 已迁至
%   /adm/finance/billing/*（adm_finance_handler，RBAC）。
% 租户端：订阅（subscribe/renew/cancel）、用量上报与配额查询、账单查询与支付。
%
% 路由 action 由 imboy_router 注入；认证区接口经 JWT。
% 授权红线（W0-SEC-01 / C0-BILL-01）：
%   tenant_id 由客户端传入，**不能作为授权依据**。所有涉及具体订阅的端点
%   一律先取 auth 中间件注入的 current_uid，再经 billing_logic:assert_owner/2
%   校验订阅归属；账单支付走 assert_invoice_owner/2 由 invoice 反查订阅归属。
%   owner_uid=0 的历史无主订阅在用户端一律拒绝，只允许管理端处理。
% tenant_id 保留为逻辑分组字段：单租户场景默认取 0（无多租户实体表）。
%%%

-behavior(cowboy_rest).

-export([init/2]).

-include("log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").
-include("error_code.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 =
        case Action of
            %% 套餐列表（管理端 CRUD 已迁至 /adm/finance/billing/*，见 adm_finance_handler）
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
subscribe(Req0, State) ->
    with_uid(Req0, State, fun(Uid) ->
        PostVals = elib_param:post(Req0),
        PlanId = maps:get(<<"plan_id">>, PostVals, 0),
        TenantId = tenant_id(PostVals),
        case is_integer(PlanId) andalso PlanId > 0 of
            false ->
                elib_response:error(Req0, <<"套餐 id 不合法"/utf8>>);
            true ->
                Trial = maps:get(<<"trial">>, PostVals, false) =:= true,
                %% owner_uid 取当前登录用户，不取客户端传入的 tenant_id
                Opts = #{trial => Trial, owner_uid => Uid},
                case billing_logic:subscribe(TenantId, PlanId, Opts) of
                    {ok, Id} ->
                        elib_response:success(Req0, #{<<"subscription_id">> => Id}, "success.");
                    {error, Msg} ->
                        elib_response:error(Req0, Msg)
                end
        end
    end).

%% @doc 续费
%% POST /v1/billing/renew
%% 参数: subscription_id
-spec renew(cowboy_req:req(), map()) -> cowboy_req:req().
renew(Req0, State) ->
    with_owned_sub(Req0, State, elib_param:post(Req0), fun(SubId) ->
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
    end).

%% @doc 取消订阅
%% POST /v1/billing/cancel
%% 参数: subscription_id
-spec cancel(cowboy_req:req(), map()) -> cowboy_req:req().
cancel(Req0, State) ->
    with_owned_sub(Req0, State, elib_param:post(Req0), fun(SubId) ->
        case billing_logic:cancel(SubId) of
            ok -> elib_response:success(Req0, #{<<"subscription_id">> => SubId}, "success.");
            {error, Msg} -> elib_response:error(Req0, Msg)
        end
    end).

%% @doc 查询当前租户订阅
%% GET /v1/billing/subscription?tenant_id=0
-spec subscription(cowboy_req:req(), map()) -> cowboy_req:req().
subscription(Req0, State) ->
    with_uid(Req0, State, fun(Uid) ->
        %% tenant_id 仍作为逻辑分组读取，但只有归属本人的订阅才返回；
        %% 否则任意用户传别人的 tenant_id 就能看到他人订阅详情。
        TenantId = to_int(elib_param:get(<<"tenant_id">>, Req0, <<"0">>), 0),
        Sub = billing_logic:current_subscription(TenantId),
        case sub_owned_by(Sub, Uid) of
            true -> elib_response:success(Req0, Sub, "success.");
            false -> elib_response:success(Req0, #{}, "success.")
        end
    end).

%% ===================================================================
%% 用量与配额
%% ===================================================================

%% @doc 上报用量
%% POST /v1/billing/usage
%% 参数: subscription_id, metric, used(增量), period(可选)
-spec report_usage(cowboy_req:req(), map()) -> cowboy_req:req().
report_usage(Req0, State) ->
    PostVals = elib_param:post(Req0),
    with_owned_sub(Req0, State, PostVals, fun(SubId) ->
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
        end
    end).

%% @doc 配额查询
%% GET /v1/billing/quota?subscription_id=&metric=&period=
-spec check_quota(cowboy_req:req(), map()) -> cowboy_req:req().
check_quota(Req0, State) ->
    with_owned_sub(
        Req0,
        State,
        #{
            <<"subscription_id">> =>
                to_int(elib_param:get(<<"subscription_id">>, Req0, <<"0">>), 0)
        },
        fun(SubId) ->
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
            end
        end
    ).

%% ===================================================================
%% 账单
%% ===================================================================

%% @doc 生成账单
%% POST /v1/billing/invoice/generate
%% 参数: subscription_id
-spec invoice_generate(cowboy_req:req(), map()) -> cowboy_req:req().
invoice_generate(Req0, State) ->
    with_owned_sub(Req0, State, elib_param:post(Req0), fun(SubId) ->
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
    end).

%% @doc 支付账单
%% POST /v1/billing/invoice/pay
%% 参数: invoice_no, payment_method
-spec invoice_pay(cowboy_req:req(), map()) -> cowboy_req:req().
invoice_pay(Req0, State) ->
    with_uid(Req0, State, fun(Uid) ->
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
                case billing_logic:assert_invoice_owner(InvoiceNo, Uid) of
                    {error, Deny} ->
                        elib_response:error(Req0, Deny, ?ERR_FORBIDDEN);
                    ok ->
                        case billing_logic:pay_invoice(InvoiceNo, Method) of
                            {ok, Payload} -> elib_response:success(Req0, Payload, "success.");
                            {error, Msg} -> elib_response:error(Req0, Msg)
                        end
                end
        end
    end).

%% @doc 账单列表
%% GET /v1/billing/invoice/list?subscription_id=
-spec invoice_list(cowboy_req:req(), map()) -> cowboy_req:req().
invoice_list(Req0, State) ->
    with_owned_sub(
        Req0,
        State,
        #{
            <<"subscription_id">> =>
                to_int(elib_param:get(<<"subscription_id">>, Req0, <<"0">>), 0)
        },
        fun(SubId) ->
            List = billing_logic:list_invoices(SubId),
            elib_response:success(Req0, #{<<"list">> => List}, "success.")
        end
    ).

%% ===================================================================
%% Internal
%% ===================================================================

%% @doc 取当前登录用户；未登录直接 403。
%% current_uid 由 auth 中间件注入 State，绝不从请求参数取。
-spec with_uid(cowboy_req:req(), map(), fun((integer()) -> cowboy_req:req())) ->
    cowboy_req:req().
with_uid(Req0, State, Fun) ->
    case auth_ds:current_uid(State) of
        Uid when is_integer(Uid), Uid > 0 -> Fun(Uid);
        _ -> elib_response:error(Req0, <<"未登录"/utf8>>, ?ERR_FORBIDDEN)
    end.

%% @doc 取当前用户 + 校验 subscription_id 归属本人，两关都过才执行 Fun。
%% 参数 map 既可来自 POST body，也可由调用方用 GET 参数构造。
-spec with_owned_sub(cowboy_req:req(), map(), map(), fun((integer()) -> cowboy_req:req())) ->
    cowboy_req:req().
with_owned_sub(Req0, State, Params, Fun) ->
    with_uid(Req0, State, fun(Uid) ->
        SubId = to_int(maps:get(<<"subscription_id">>, Params, 0), 0),
        case SubId > 0 of
            false ->
                elib_response:error(Req0, <<"订阅 id 不合法"/utf8>>);
            true ->
                case billing_logic:assert_owner(SubId, Uid) of
                    ok -> Fun(SubId);
                    {error, Deny} -> elib_response:error(Req0, Deny, ?ERR_FORBIDDEN)
                end
        end
    end).

%% @doc 订阅是否归属该用户（查询类端点用，过滤而非报错）
-spec sub_owned_by(map(), integer()) -> boolean().
sub_owned_by(Sub, Uid) when is_map(Sub), map_size(Sub) > 0 ->
    to_int(maps:get(<<"owner_uid">>, Sub, 0), 0) =:= Uid;
sub_owned_by(_Sub, _Uid) ->
    false.

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
