-module(adm_finance_handler).

%%%
% adm_finance 控制器模块 / Admin finance operational API
%
% 运营财务端点 /adm/finance/*：供 React 管理后台（管理员鉴权）做跨用户运营查询。
%   - 钱包列表 / 某用户钱包流水
%   - 充值订单列表 / 支付流水列表（对账）
%   - billing 套餐 CRUD / 订阅列表 / 账单列表
%
% 职责边界：仅做参数解析 + 鉴权 + 调 logic + 响应封装，不写业务/SQL。
%   wallet/recharge/payment 运营查询 -> finance_adm_logic
%   billing 套餐/订阅/账单         -> billing_logic
%
% 金额单位「分」(bigint) 透传不改，前端负责分→元显示。
% 统一响应信封 {code,msg,sv_ts,payload}（经 elib_response）。
%
% 权限：沿用 adm 管理员 Cookie 鉴权（adm_auth_middleware 注入 adm_user_id），
%   并用 adm_acl 细粒度权限守卫：
%     finance:read  — 查询类（GET）
%     finance:write — 变更类（套餐创建/更新 POST）
%   这两个权限新增于 adm_index_handler:role_acl/1（super_admin/ops_admin）。
%%%

-behavior(cowboy_rest).

-export([init/2]).

%% fail/3 是查询失败的运行时兜底响应。底层分页查询的 dialyzer 成功类型多为 ok-only，
%% 导致 dialyzer 判定 fail/3 的 {error,_} 调用分支不可达（no local return）。
%% 该 error 分支为防御性运行时保留（底层 SQL 实际可能返回 {error,_}），故抑制此告警。
-dialyzer({nowarn_function, [fail/3]}).

-include("error_code.hrl").
-include("log.hrl").

-define(PERM_READ, <<"finance:read">>).
-define(PERM_WRITE, <<"finance:write">>).

%% ===================================================================
%% API
%% ===================================================================

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0, false),
    State = maps:remove(action, State0),
    Method = cowboy_req:method(Req0),
    Req1 = dispatch(Action, Method, Req0, State),
    {ok, Req1, State}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

-spec dispatch(atom(), binary(), cowboy_req:req(), map()) -> cowboy_req:req().
dispatch(wallets, Method, Req0, State) ->
    wallets(Method, Req0, State);
dispatch(wallet_transactions, Method, Req0, State) ->
    wallet_transactions(Method, Req0, State);
dispatch(recharge_orders, Method, Req0, State) ->
    recharge_orders(Method, Req0, State);
dispatch(payment_transactions, Method, Req0, State) ->
    payment_transactions(Method, Req0, State);
dispatch(recharge_order_refund, Method, Req0, State) ->
    recharge_order_refund(Method, Req0, State);
dispatch(payment_transaction_refund, Method, Req0, State) ->
    payment_transaction_refund(Method, Req0, State);
dispatch(wallet_freeze, Method, Req0, State) ->
    wallet_freeze(Method, Req0, State);
dispatch(wallet_unfreeze, Method, Req0, State) ->
    wallet_unfreeze(Method, Req0, State);
dispatch(billing_plans, Method, Req0, State) ->
    billing_plans(Method, Req0, State);
dispatch(billing_plan_create, Method, Req0, State) ->
    billing_plan_create(Method, Req0, State);
dispatch(billing_plan_update, Method, Req0, State) ->
    billing_plan_update(Method, Req0, State);
dispatch(billing_subscriptions, Method, Req0, State) ->
    billing_subscriptions(Method, Req0, State);
dispatch(billing_invoices, Method, Req0, State) ->
    billing_invoices(Method, Req0, State);
dispatch(withdrawals, Method, Req0, State) ->
    withdrawals(Method, Req0, State);
dispatch(withdrawal_complete, Method, Req0, State) ->
    withdrawal_complete(Method, Req0, State);
dispatch(withdrawal_reject, Method, Req0, State) ->
    withdrawal_reject(Method, Req0, State);
dispatch(_, _Method, Req0, _State) ->
    Req0.

%% -------------------------------------------------------------------
%% 钱包列表 GET /adm/finance/wallets
%% 筛选 user_id / status
%% -------------------------------------------------------------------
-spec wallets(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
wallets(<<"GET">>, Req0, State) ->
    with_read_perm(State, Req0, fun() ->
        {Page, Size} = elib_param:page(Req0),
        Qs = cowboy_req:parse_qs(Req0),
        Filter = collect_filters(Qs, [
            {<<"user_id">>, user_id, int},
            {<<"status">>, status, int}
        ]),
        respond_list(Req0, finance_adm_logic:list_wallets(Filter, Page, Size))
    end);
wallets(_, Req0, _State) ->
    Req0.

%% -------------------------------------------------------------------
%% 某用户钱包流水 GET /adm/finance/wallet/:user_id/transactions
%% -------------------------------------------------------------------
-spec wallet_transactions(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
wallet_transactions(<<"GET">>, Req0, State) ->
    with_read_perm(State, Req0, fun() ->
        case parse_uid_binding(Req0) of
            {error, Msg} ->
                elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
            {ok, Uid} ->
                {Page, Size} = elib_param:page(Req0),
                respond_list(Req0, finance_adm_logic:list_wallet_transactions(Uid, Page, Size))
        end
    end);
wallet_transactions(_, Req0, _State) ->
    Req0.

%% -------------------------------------------------------------------
%% 充值订单列表 GET /adm/finance/recharge-orders
%% 筛选 status[0-4] / payment_method / user_id / order_no
%% -------------------------------------------------------------------
-spec recharge_orders(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
recharge_orders(<<"GET">>, Req0, State) ->
    with_read_perm(State, Req0, fun() ->
        {Page, Size} = elib_param:page(Req0),
        Qs = cowboy_req:parse_qs(Req0),
        Filter = collect_filters(Qs, [
            {<<"status">>, status, int},
            {<<"payment_method">>, payment_method, binary},
            {<<"user_id">>, user_id, int},
            {<<"order_no">>, order_no, binary}
        ]),
        respond_list(Req0, finance_adm_logic:list_recharge_orders(Filter, Page, Size))
    end);
recharge_orders(_, Req0, _State) ->
    Req0.

%% -------------------------------------------------------------------
%% 支付流水列表（对账）GET /adm/finance/payment-transactions
%% 筛选 gateway / biz_type[1-3] / status[0-4] / user_id
%% -------------------------------------------------------------------
-spec payment_transactions(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
payment_transactions(<<"GET">>, Req0, State) ->
    with_read_perm(State, Req0, fun() ->
        {Page, Size} = elib_param:page(Req0),
        Qs = cowboy_req:parse_qs(Req0),
        Filter = collect_filters(Qs, [
            {<<"gateway">>, gateway, binary},
            {<<"biz_type">>, biz_type, int},
            {<<"status">>, status, int},
            {<<"user_id">>, user_id, int},
            {<<"trade_no">>, trade_no, binary}
        ]),
        respond_list(Req0, finance_adm_logic:list_payment_transactions(Filter, Page, Size))
    end);
payment_transactions(_, Req0, _State) ->
    Req0.

%% -------------------------------------------------------------------
%% 充值订单退款 POST /adm/finance/recharge-orders/refund
%% body: {"order_no": <bin>, "refund_reason": <bin?>}
%% 单事务：钱包扣回可用余额 + 订单置退款态 + 退款流水；已退款幂等拒绝。
%% -------------------------------------------------------------------
-spec recharge_order_refund(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
recharge_order_refund(<<"POST">>, Req0, State) ->
    with_write_perm(State, Req0, fun() ->
        PostVals = elib_param:post(Req0),
        case parse_required_bin(PostVals, <<"order_no">>) of
            {error, Msg} ->
                elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
            {ok, OrderNo} ->
                Reason = maps:get(<<"refund_reason">>, PostVals, <<>>),
                case finance_adm_logic:refund_recharge_order(OrderNo) of
                    {ok, NewBalance} ->
                        _ = adm_operation_log_ds:insert(
                            maps:get(adm_user_id, State, 0),
                            <<"recharge_order_refund">>,
                            0,
                            <<"recharge_order">>,
                            #{
                                <<"order_no">> => OrderNo,
                                <<"reason">> => Reason,
                                <<"balance_after">> => NewBalance
                            },
                            elib_req:peer_ip(Req0)
                        ),
                        elib_response:success(
                            Req0,
                            #{<<"order_no">> => OrderNo, <<"balance">> => NewBalance},
                            <<"退款成功"/utf8>>
                        );
                    {error, Msg} ->
                        elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST)
                end
        end
    end);
recharge_order_refund(_, Req0, _State) ->
    Req0.

%% -------------------------------------------------------------------
%% 支付流水退款（对账原路退回）POST /adm/finance/payment-transactions/refund
%% body: {"trade_no": <bin>, "refund_reason": <bin?>}
%% 保守：充值/频道订单流水拒绝（走各自专用退款入口，防重复退款）。
%% -------------------------------------------------------------------
-spec payment_transaction_refund(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
payment_transaction_refund(<<"POST">>, Req0, State) ->
    with_write_perm(State, Req0, fun() ->
        PostVals = elib_param:post(Req0),
        case parse_required_bin(PostVals, <<"trade_no">>) of
            {error, Msg} ->
                elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
            {ok, TradeNo} ->
                Reason = maps:get(<<"refund_reason">>, PostVals, <<>>),
                case finance_adm_logic:refund_payment_transaction(TradeNo) of
                    {ok, refunded} ->
                        _ = adm_operation_log_ds:insert(
                            maps:get(adm_user_id, State, 0),
                            <<"payment_transaction_refund">>,
                            0,
                            <<"payment_transaction">>,
                            #{<<"trade_no">> => TradeNo, <<"reason">> => Reason},
                            elib_req:peer_ip(Req0)
                        ),
                        elib_response:success(
                            Req0, #{<<"trade_no">> => TradeNo}, <<"退款成功"/utf8>>
                        );
                    {error, Msg} ->
                        elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST)
                end
        end
    end);
payment_transaction_refund(_, Req0, _State) ->
    Req0.

%% -------------------------------------------------------------------
%% 冻结钱包 POST /adm/finance/wallets/freeze
%% body: {"user_id": <int>, "amount": <int 分>}
%% -------------------------------------------------------------------
-spec wallet_freeze(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
wallet_freeze(<<"POST">>, Req0, State) ->
    with_write_perm(State, Req0, fun() ->
        do_freeze_action(Req0, State, freeze)
    end);
wallet_freeze(_, Req0, _State) ->
    Req0.

%% -------------------------------------------------------------------
%% 解冻钱包 POST /adm/finance/wallets/unfreeze
%% body: {"user_id": <int>, "amount": <int 分>}
%% -------------------------------------------------------------------
-spec wallet_unfreeze(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
wallet_unfreeze(<<"POST">>, Req0, State) ->
    with_write_perm(State, Req0, fun() ->
        do_freeze_action(Req0, State, unfreeze)
    end);
wallet_unfreeze(_, Req0, _State) ->
    Req0.

%% @doc 冻结/解冻共用执行体（参数解析 + logic 调用 + 审计 + 响应）
-spec do_freeze_action(cowboy_req:req(), map(), freeze | unfreeze) -> cowboy_req:req().
do_freeze_action(Req0, State, Op) ->
    PostVals = elib_param:post(Req0),
    case parse_freeze_params(PostVals) of
        {error, Msg} ->
            elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
        {ok, Uid, Amount} ->
            {LogicRes, Action, OkMsg} =
                case Op of
                    freeze ->
                        {
                            finance_adm_logic:freeze_wallet(Uid, Amount),
                            <<"wallet_freeze">>,
                            <<"冻结成功"/utf8>>
                        };
                    unfreeze ->
                        {
                            finance_adm_logic:unfreeze_wallet(Uid, Amount),
                            <<"wallet_unfreeze">>,
                            <<"解冻成功"/utf8>>
                        }
                end,
            case LogicRes of
                ok ->
                    _ = adm_operation_log_ds:insert(
                        maps:get(adm_user_id, State, 0),
                        Action,
                        Uid,
                        <<"wallet">>,
                        #{<<"amount">> => Amount},
                        elib_req:peer_ip(Req0)
                    ),
                    elib_response:success(Req0, #{}, OkMsg);
                {error, Msg} ->
                    elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST)
            end
    end.

%% -------------------------------------------------------------------
%% billing 套餐列表 GET /adm/finance/billing/plans
%% 筛选 status[0-1]
%% -------------------------------------------------------------------
-spec billing_plans(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
billing_plans(<<"GET">>, Req0, State) ->
    with_read_perm(State, Req0, fun() ->
        {Page, Size} = elib_param:page(Req0),
        Qs = cowboy_req:parse_qs(Req0),
        Filter = collect_filters(Qs, [
            {<<"status">>, status, int},
            {<<"billing_period">>, billing_period, binary}
        ]),
        case billing_logic:list_plans_page(Filter, Page, Size) of
            {ok, Payload} ->
                elib_response:success(Req0, normalize_billing_payload(Payload, [<<"id">>]));
            {error, Reason} ->
                fail(Req0, <<"套餐列表查询失败"/utf8>>, Reason)
        end
    end);
billing_plans(_, Req0, _State) ->
    Req0.

%% -------------------------------------------------------------------
%% billing 套餐创建 POST /adm/finance/billing/plan
%% -------------------------------------------------------------------
-spec billing_plan_create(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
billing_plan_create(<<"POST">>, Req0, State) ->
    with_write_perm(State, Req0, fun() ->
        PostVals = elib_param:post(Req0),
        case billing_logic:create_plan(PostVals) of
            {ok, PlanId} ->
                elib_response:success(
                    Req0,
                    #{<<"id">> => elib_id:tsid_to_bin(PlanId)},
                    <<"套餐已创建"/utf8>>
                );
            {error, Msg} ->
                elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST)
        end
    end);
billing_plan_create(_, Req0, _State) ->
    Req0.

%% -------------------------------------------------------------------
%% billing 套餐更新 POST /adm/finance/billing/plan/update
%% body 须含 id；其余字段为待更新列
%% -------------------------------------------------------------------
-spec billing_plan_update(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
billing_plan_update(<<"POST">>, Req0, State) ->
    with_write_perm(State, Req0, fun() ->
        PostVals = elib_param:post(Req0),
        case parse_required_id(PostVals, <<"id">>) of
            {error, Msg} ->
                elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
            {ok, PlanId} ->
                ChangeMap = maps:remove(<<"id">>, PostVals),
                case map_size(ChangeMap) > 0 of
                    false ->
                        elib_response:error(
                            Req0, <<"至少提供一个可更新字段"/utf8>>, ?ERR_BAD_REQUEST
                        );
                    true ->
                        case billing_logic:update_plan(PlanId, ChangeMap) of
                            {ok, _} ->
                                elib_response:success(Req0, #{}, <<"套餐已更新"/utf8>>);
                            {error, Msg} ->
                                elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST)
                        end
                end
        end
    end);
billing_plan_update(_, Req0, _State) ->
    Req0.

%% -------------------------------------------------------------------
%% billing 订阅列表 GET /adm/finance/billing/subscriptions
%% 筛选 status[0-3] / plan_id / tenant_id
%% -------------------------------------------------------------------
-spec billing_subscriptions(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
billing_subscriptions(<<"GET">>, Req0, State) ->
    with_read_perm(State, Req0, fun() ->
        {Page, Size} = elib_param:page(Req0),
        Qs = cowboy_req:parse_qs(Req0),
        Filter = collect_filters(Qs, [
            {<<"status">>, status, int},
            {<<"plan_id">>, plan_id, int},
            {<<"tenant_id">>, tenant_id, int}
        ]),
        case billing_logic:list_subscriptions_page(Filter, Page, Size) of
            {ok, Payload} ->
                elib_response:success(
                    Req0, normalize_billing_payload(Payload, [<<"id">>, <<"plan_id">>])
                );
            {error, Reason} ->
                fail(Req0, <<"订阅列表查询失败"/utf8>>, Reason)
        end
    end);
billing_subscriptions(_, Req0, _State) ->
    Req0.

%% -------------------------------------------------------------------
%% billing 账单列表 GET /adm/finance/billing/invoices
%% 筛选 status[0-2] / subscription_id
%% -------------------------------------------------------------------
-spec billing_invoices(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
billing_invoices(<<"GET">>, Req0, State) ->
    with_read_perm(State, Req0, fun() ->
        {Page, Size} = elib_param:page(Req0),
        Qs = cowboy_req:parse_qs(Req0),
        Filter = collect_filters(Qs, [
            {<<"status">>, status, int},
            {<<"subscription_id">>, subscription_id, int},
            {<<"invoice_no">>, invoice_no, binary}
        ]),
        case billing_logic:list_invoices_page(Filter, Page, Size) of
            {ok, Payload} ->
                elib_response:success(
                    Req0, normalize_billing_payload(Payload, [<<"id">>, <<"subscription_id">>])
                );
            {error, Reason} ->
                fail(Req0, <<"账单列表查询失败"/utf8>>, Reason)
        end
    end);
billing_invoices(_, Req0, _State) ->
    Req0.

%% -------------------------------------------------------------------
%% 提现列表 GET /adm/finance/withdrawals
%% 筛选 user_id / status[0=待处理,1=已完成,2=已拒绝]
%% -------------------------------------------------------------------
-spec withdrawals(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
withdrawals(<<"GET">>, Req0, State) ->
    with_read_perm(State, Req0, fun() ->
        {Page, Size} = elib_param:page(Req0),
        Qs = cowboy_req:parse_qs(Req0),
        Filter = collect_filters(Qs, [
            {<<"user_id">>, user_id, int},
            {<<"status">>, status, int}
        ]),
        respond_list(Req0, finance_adm_logic:list_withdrawals(Filter, Page, Size))
    end);
withdrawals(_, Req0, _State) ->
    Req0.

%% -------------------------------------------------------------------
%% 标记提现完成 POST /adm/finance/withdrawals/complete
%% body: {"id": <tx_id>}
%% -------------------------------------------------------------------
-spec withdrawal_complete(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
withdrawal_complete(<<"POST">>, Req0, State) ->
    with_write_perm(State, Req0, fun() ->
        PostVals = elib_param:post(Req0),
        case parse_required_id(PostVals, <<"id">>) of
            {error, Msg} ->
                elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
            {ok, TxId} ->
                case finance_adm_logic:complete_withdrawal(TxId) of
                    {ok, 1} ->
                        _ = adm_operation_log_ds:insert(
                            maps:get(adm_user_id, State, 0),
                            <<"withdrawal_complete">>,
                            TxId,
                            <<"payment_transaction">>,
                            #{
                                <<"before">> => #{<<"status">> => 0},
                                <<"after">> => #{<<"status">> => 1}
                            },
                            elib_req:peer_ip(Req0)
                        ),
                        elib_response:success(Req0, #{}, <<"提现已标记完成"/utf8>>);
                    {ok, 0} ->
                        elib_response:error(
                            Req0, <<"提现记录不存在或已处理"/utf8>>, ?ERR_BAD_REQUEST
                        );
                    {error, Reason} ->
                        fail(Req0, <<"标记完成失败"/utf8>>, Reason)
                end
        end
    end);
withdrawal_complete(_, Req0, _State) ->
    Req0.

%% -------------------------------------------------------------------
%% 拒绝提现 POST /adm/finance/withdrawals/reject
%% body: {"id": <tx_id>}
%% 原子操作：status=2 + 退还余额 + 写退款流水（tx_type=11）
%% -------------------------------------------------------------------
-spec withdrawal_reject(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
withdrawal_reject(<<"POST">>, Req0, State) ->
    with_write_perm(State, Req0, fun() ->
        PostVals = elib_param:post(Req0),
        case parse_required_id(PostVals, <<"id">>) of
            {error, Msg} ->
                elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
            {ok, TxId} ->
                case finance_adm_logic:reject_withdrawal(TxId) of
                    {ok, 1} ->
                        _ = adm_operation_log_ds:insert(
                            maps:get(adm_user_id, State, 0),
                            <<"withdrawal_reject">>,
                            TxId,
                            <<"payment_transaction">>,
                            #{
                                <<"before">> => #{<<"status">> => 0},
                                <<"after">> => #{<<"status">> => 2},
                                <<"note">> => <<"refund tx_type=11">>
                            },
                            elib_req:peer_ip(Req0)
                        ),
                        elib_response:success(Req0, #{}, <<"提现已拒绝"/utf8>>);
                    {ok, 0} ->
                        elib_response:error(
                            Req0, <<"提现记录不存在或已处理"/utf8>>, ?ERR_BAD_REQUEST
                        );
                    {error, Reason} ->
                        fail(Req0, <<"拒绝操作失败"/utf8>>, Reason)
                end
        end
    end);
withdrawal_reject(_, Req0, _State) ->
    Req0.

%% ===================================================================
%% 鉴权守卫
%% ===================================================================

%% @doc 读权限守卫：通过后执行 Fun（返回 cowboy_req），否则返回 403。
-spec with_read_perm(map(), cowboy_req:req(), fun(() -> cowboy_req:req())) -> cowboy_req:req().
with_read_perm(State, Req0, Fun) ->
    guard(State, ?PERM_READ, Req0, Fun).

%% @doc 写权限守卫。
-spec with_write_perm(map(), cowboy_req:req(), fun(() -> cowboy_req:req())) -> cowboy_req:req().
with_write_perm(State, Req0, Fun) ->
    guard(State, ?PERM_WRITE, Req0, Fun).

-spec guard(map(), binary(), cowboy_req:req(), fun(() -> cowboy_req:req())) -> cowboy_req:req().
guard(State, Permission, Req0, Fun) ->
    case adm_acl:ensure_permission(State, Permission, Req0) of
        ok ->
            Fun();
        {error, Req1} ->
            Req1
    end.

%% ===================================================================
%% 参数解析与响应
%% ===================================================================

%% @doc 从 query-string proplist 中按规格提取筛选项，组装 Filter map。
%% Spec: [{QsKey :: binary(), MapKey :: atom(), int | binary}]。
%% 缺省/空值的键不进 Filter（由 logic 层 build_where 再次过白名单）。
-spec collect_filters([{binary(), binary()}], [{binary(), atom(), int | binary}]) -> map().
collect_filters(Qs, Spec) ->
    lists:foldl(
        fun({QsKey, MapKey, Type}, Acc) ->
            case proplists:get_value(QsKey, Qs) of
                undefined -> Acc;
                <<>> -> Acc;
                Raw -> put_filter(Acc, MapKey, Type, Raw)
            end
        end,
        #{},
        Spec
    ).

-spec put_filter(map(), atom(), int | binary, binary()) -> map().
put_filter(Acc, MapKey, int, Raw) ->
    case parse_int(Raw) of
        {ok, Int} -> Acc#{MapKey => Int};
        error -> Acc
    end;
put_filter(Acc, MapKey, binary, Raw) ->
    Acc#{MapKey => Raw}.

-spec parse_int(binary()) -> {ok, integer()} | error.
parse_int(Bin) ->
    case string:to_integer(binary_to_list(Bin)) of
        {Int, []} when is_integer(Int) -> {ok, Int};
        _ -> error
    end.

%% @doc 解析路由 binding 中的 user_id（须为正整数）
-spec parse_uid_binding(cowboy_req:req()) -> {ok, integer()} | {error, binary()}.
parse_uid_binding(Req0) ->
    case cowboy_req:binding(user_id, Req0) of
        undefined ->
            {error, <<"用户ID不能为空"/utf8>>};
        UidBin ->
            case parse_int(UidBin) of
                {ok, Uid} when Uid > 0 -> {ok, Uid};
                _ -> {error, <<"用户ID格式错误"/utf8>>}
            end
    end.

%% @doc 从 body 解析必填的整数 id 字段
-spec parse_required_id(map(), binary()) -> {ok, integer()} | {error, binary()}.
parse_required_id(PostVals, Key) ->
    case maps:get(Key, PostVals, undefined) of
        undefined ->
            {error, <<"ID不能为空"/utf8>>};
        Val ->
            case to_positive_int(Val) of
                {ok, Id} -> {ok, Id};
                error -> {error, <<"ID格式错误"/utf8>>}
            end
    end.

%% @doc 从 body 解析必填的非空 binary 字段
-spec parse_required_bin(map(), binary()) -> {ok, binary()} | {error, binary()}.
parse_required_bin(PostVals, Key) ->
    case maps:get(Key, PostVals, undefined) of
        undefined -> {error, <<"缺少必填参数"/utf8>>};
        <<>> -> {error, <<"必填参数不能为空"/utf8>>};
        Val when is_binary(Val) -> {ok, Val};
        _ -> {error, <<"参数格式错误"/utf8>>}
    end.

%% @doc 解析冻结/解冻参数：user_id(正整数) + amount(正整数，单位分)
-spec parse_freeze_params(map()) -> {ok, integer(), integer()} | {error, binary()}.
parse_freeze_params(PostVals) ->
    case parse_required_id(PostVals, <<"user_id">>) of
        {error, _} = E ->
            E;
        {ok, Uid} ->
            case maps:get(<<"amount">>, PostVals, undefined) of
                undefined ->
                    {error, <<"金额不能为空"/utf8>>};
                AmtRaw ->
                    case to_positive_int(AmtRaw) of
                        {ok, Amount} -> {ok, Uid, Amount};
                        error -> {error, <<"金额必须为正整数（单位：分）"/utf8>>}
                    end
            end
    end.

-spec to_positive_int(term()) -> {ok, integer()} | error.
to_positive_int(Val) when is_integer(Val), Val > 0 -> {ok, Val};
to_positive_int(Val) when is_binary(Val) ->
    case parse_int(Val) of
        {ok, Id} when Id > 0 -> {ok, Id};
        _ -> error
    end;
to_positive_int(_) ->
    error.

%% @doc 统一封装 logic 返回的 {ok, Payload} | {error, Reason}（payload 已含 list/total/page/size）
%% 注：不加 -spec，与本仓其他 adm handler 的响应辅助函数一致；底层查询成功类型多为
%% ok-only，error 分支为运行时兜底，加严格 spec 会触发 dialyzer 死分支告警。
respond_list(Req0, {ok, Payload}) ->
    elib_response:success(Req0, Payload);
respond_list(Req0, {error, Reason}) ->
    fail(Req0, <<"查询失败"/utf8>>, Reason).

%% @doc 规范化 billing 分页 payload：list 中各条目指定 TSID 键转字符串。
-spec normalize_billing_payload(map(), [binary()]) -> map().
normalize_billing_payload(Payload, TsidKeys) ->
    List = maps:get(list, Payload, []),
    List2 = [elib_id:tsid_keys_to_bin(Item, TsidKeys) || Item <- List],
    Payload#{list => List2}.

%% @doc 查询失败统一响应（500 + 信封）。Reason 仅记日志，不外泄。
%% 注：不加 -spec，与本仓其他 adm handler 响应辅助函数一致；底层查询成功类型多为
%% ok-only，本函数仅在运行时 {error,_} 兜底分支触达。
fail(Req0, Msg, Reason) ->
    ?DEBUG_LOG("adm_finance 查询失败: ~p", [Reason]),
    elib_response:error(Req0, Msg, ?ERR_INTERNAL_SERVER_ERROR).
