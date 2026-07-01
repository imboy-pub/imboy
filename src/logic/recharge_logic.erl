-module(recharge_logic).

%%%===================================================================
%%% @doc 钱包充值业务逻辑 / Wallet recharge business logic
%%%
%%% 充值真实闭环：下单 -> 拉起第三方支付 -> 回调入账 -> 查询
%%%
%%%   create_order/3        创建充值订单（status=0 待支付）
%%%   pay/2                 校验后拉起第三方支付，回填 payment_no；
%%%                         沙箱(mock)即时入账便于联调
%%%   mark_paid_and_credit/2 供「线B 支付回调」调用的入账核心（幂等）
%%%   query/2               查询充值订单状态
%%%
%%% 金额单位全程「分」，与钱包统一；前端传入分。
%%% 入账幂等：reference_no UNIQUE（RCH_<OrderNo>）+ 先查订单状态双保险。
%%% @end
%%%===================================================================

-export([create_order/3]).
-export([pay/2]).
-export([mark_paid_and_credit/2]).
-export([query/2]).

-include("log.hrl").

%% 充值订单状态
-define(STATUS_PENDING, 0).
-define(STATUS_PAID, 1).

%% 充值限额默认值（单位：分）
-define(DEFAULT_MIN_AMOUNT, 100).
-define(DEFAULT_MAX_AMOUNT, 1000000).

%% 生产环境允许的支付方式白名单（不含 mock）
-define(ALLOWED_PAYMENT_METHODS, [<<"alipay">>, <<"wechat">>, <<"stripe">>]).
%% 非生产环境额外允许 mock（用于测试/开发/联调）
-define(DEV_ALLOWED_PAYMENT_METHODS, [<<"mock">> | ?ALLOWED_PAYMENT_METHODS]).

%%===================================================================
%%% API
%%===================================================================

%% @doc 创建充值订单
%% @param Uid 用户ID
%% @param Amount 充值金额（分）
%% @param PaymentMethod 支付方式 alipay/wechat/stripe/mock
%% @returns {ok, Order :: map()} | {error, binary()}
-spec create_order(integer(), integer(), binary()) -> {ok, map()} | {error, binary()}.
create_order(Uid, Amount, PaymentMethod) ->
    case validate_amount(Amount) of
        {error, _} = AmtErr ->
            AmtErr;
        ok ->
            case is_payment_method_allowed(PaymentMethod) of
                false ->
                    {error, <<"不支持的支付方式"/utf8>>};
                true ->
                    Data = #{
                        user_id => Uid,
                        amount => Amount,
                        currency => <<"CNY">>,
                        payment_method => PaymentMethod
                    },
                    case recharge_order_ds:create(Data) of
                        {ok, OrderNo} ->
                            load_order(OrderNo);
                        {error, Reason} when is_binary(Reason) ->
                            {error, Reason};
                        {error, Reason} ->
                            {error, elib_cnv:safe_to_binary(Reason)}
                    end
            end
    end.

%% @doc 拉起第三方支付
%% 校验：订单归属当前用户、未过期、待支付 → 调 payment_gateway:pay 拿支付单号 →
%% 回填 payment_no。沙箱网关(mock)即时入账，便于联调。
%% @returns {ok, PayResult :: map()} | {error, binary()}
-spec pay(integer(), binary()) -> {ok, map()} | {error, binary()}.
pay(Uid, OrderNo) ->
    case load_order_raw(OrderNo) of
        {error, _} = Err ->
            Err;
        {ok, Order} ->
            OrderUid = maps:get(<<"user_id">>, Order, 0),
            Status = maps:get(<<"status">>, Order, 0),
            Method = maps:get(<<"payment_method">>, Order, <<>>),
            Amount = maps:get(<<"amount">>, Order, 0),
            if
                OrderUid =/= Uid ->
                    {error, <<"无权操作此订单"/utf8>>};
                Status =/= ?STATUS_PENDING ->
                    {error, <<"订单状态不允许支付"/utf8>>};
                true ->
                    do_pay(Uid, OrderNo, Method, Amount)
            end
    end.

%% @doc 入账核心（供线B支付回调调用）—— 幂等
%% 幂等地把 recharge_order 标记已支付 + 钱包加余额。
%% 双保险：① 先查订单状态，已支付则直接返回 ok（不重复入账）；
%%         ② atomic_balance_change 使用 RefNo=<<"RCH_", OrderNo>>，
%%            依赖 wallet_transaction.reference_no UNIQUE 兜底防重复入账。
%% @param OrderNo 充值订单号
%% @param GatewayPaymentNo 第三方支付单号
%% @returns {ok, NewBalance :: integer()} | {ok, already_credited} | {error, binary()}
-spec mark_paid_and_credit(binary(), binary()) ->
    {ok, integer()} | {ok, already_credited} | {error, binary()}.
mark_paid_and_credit(OrderNo, GatewayPaymentNo) ->
    case load_order_raw(OrderNo) of
        {error, _} = Err ->
            Err;
        {ok, Order} ->
            Status = maps:get(<<"status">>, Order, 0),
            case Status of
                ?STATUS_PAID ->
                    %% 双保险①：订单已支付，幂等返回，不重复入账
                    {ok, already_credited};
                ?STATUS_PENDING ->
                    credit_order(Order, OrderNo, GatewayPaymentNo);
                _ ->
                    {error, <<"订单状态不允许入账"/utf8>>}
            end
    end.

%% @doc 查询充值订单状态（仅本人可查）
-spec query(integer(), binary()) -> {ok, map()} | {error, binary()}.
query(Uid, OrderNo) ->
    case load_order_raw(OrderNo) of
        {error, _} = Err ->
            Err;
        {ok, Order} ->
            OrderUid = maps:get(<<"user_id">>, Order, 0),
            case OrderUid =:= Uid of
                false -> {error, <<"无权查看此订单"/utf8>>};
                true -> {ok, order_transfer(Order)}
            end
    end.

%%===================================================================
%%% Internal Functions
%%===================================================================

%% @doc 执行支付：调网关 → 回填 payment_no → 沙箱即时入账
-spec do_pay(integer(), binary(), binary(), integer()) -> {ok, map()} | {error, binary()}.
do_pay(Uid, OrderNo, Method, Amount) ->
    PayOpts = #{uid => Uid, amount => Amount, currency => order_currency(OrderNo)},
    case normalize_pay_result(payment_gateway:pay(Method, OrderNo, PayOpts)) of
        {ok, PaymentNo, Extra} ->
            %% payment_no 由回调/入账(credit_in_tx)回填；下单阶段不改订单状态。
            %% 统一信封契约：payment_method / payment_no / pay_params（binary key，
            %% 前端 dart 收 string key）。pay_params 透传网关支付参数：
            %%   alipay #{<<"order_str">>}，wechat #{<<"prepay_id">>|<<"code_url">>}，
            %%   wallet/mock #{}（即时入账无需拉起第三方）。
            %% 另保留 recharge 业务字段 order_no/amount/status 供前端轮询。
            Result0 = #{
                <<"payment_method">> => Method,
                <<"payment_no">> => PaymentNo,
                <<"pay_params">> => Extra,
                <<"order_no">> => OrderNo,
                <<"amount">> => Amount
            },
            %% 沙箱(mock)网关即时入账，便于本地联调
            case Method of
                <<"mock">> ->
                    case mark_paid_and_credit(OrderNo, PaymentNo) of
                        {ok, NewBalance} when is_integer(NewBalance) ->
                            {ok, Result0#{
                                <<"status">> => ?STATUS_PAID,
                                <<"balance">> => NewBalance
                            }};
                        {ok, already_credited} ->
                            {ok, Result0#{<<"status">> => ?STATUS_PAID}};
                        {error, Reason} ->
                            {error, Reason}
                    end;
                _ ->
                    %% 真实网关：返回支付参数，等待第三方回调入账
                    {ok, Result0#{<<"status">> => ?STATUS_PENDING}}
            end;
        {error, PayReason} when is_binary(PayReason) ->
            {error, PayReason}
    end.

%% @doc 归一网关返回：兼容 {ok, PaymentNo} 与 {ok, PaymentNo, Extra}，
%% 统一成 {ok, PaymentNo, Extra::map()}，错误原样透传。
-spec normalize_pay_result(term()) ->
    {ok, binary(), map()} | {error, term()}.
normalize_pay_result({ok, PaymentNo, Extra}) when is_map(Extra) ->
    {ok, PaymentNo, Extra};
normalize_pay_result({ok, PaymentNo}) ->
    {ok, PaymentNo, #{}};
normalize_pay_result({error, _} = Err) ->
    Err.

%% @doc 取订单币种（透传给网关用于换算与下单），缺省 CNY。
-spec order_currency(binary()) -> binary().
order_currency(OrderNo) ->
    case load_order_raw(OrderNo) of
        {ok, Order} -> maps:get(<<"currency">>, Order, <<"CNY">>);
        _ -> <<"CNY">>
    end.

%% @doc 入账：单事务(订单状态翻转+钱包加余额+流水)原子完成，幂等。
%% 取代此前 mark_paid + do_credit 两步非原子写法（曾有"订单已支付但钱没进"风险）。
-spec credit_order(map(), binary(), binary()) ->
    {ok, integer()} | {ok, already_credited} | {error, binary()}.
credit_order(Order, OrderNo, GatewayPaymentNo) ->
    Uid = maps:get(<<"user_id">>, Order, 0),
    Amount = maps:get(<<"amount">>, Order, 0),
    %% 确保钱包存在（事务内只加余额，不建钱包）
    _ = wallet_ds:ensure_wallet(Uid),
    case recharge_order_ds:credit_in_tx(OrderNo, GatewayPaymentNo, Uid, Amount) of
        {ok, NewBalance} ->
            {ok, NewBalance};
        {rollback, already_credited} ->
            {ok, already_credited};
        {rollback, order_not_payable} ->
            %% 并发竞态/已过期：回查最终态
            recheck_after_race(OrderNo);
        {rollback, wallet_not_found} ->
            {error, <<"钱包不可用"/utf8>>};
        {error, Reason} ->
            {error, elib_cnv:safe_to_binary(Reason)}
    end.

%% @doc 并发竞态后回查最终态
-spec recheck_after_race(binary()) -> {ok, already_credited} | {error, binary()}.
recheck_after_race(OrderNo) ->
    case load_order_raw(OrderNo) of
        {ok, Order} ->
            case maps:get(<<"status">>, Order, 0) of
                ?STATUS_PAID -> {ok, already_credited};
                _ -> {error, <<"订单不存在或已过期"/utf8>>}
            end;
        {error, _} ->
            {error, <<"订单不存在或已过期"/utf8>>}
    end.

%% @doc 校验充值金额在 [min, max] 限额内
-spec validate_amount(term()) -> ok | {error, binary()}.
validate_amount(Amount) when is_integer(Amount) ->
    Min = application:get_env(imboy, recharge_min_amount, ?DEFAULT_MIN_AMOUNT),
    Max = application:get_env(imboy, recharge_max_amount, ?DEFAULT_MAX_AMOUNT),
    case Amount >= Min andalso Amount =< Max of
        true ->
            ok;
        false ->
            MinB = integer_to_binary(Min),
            MaxB = integer_to_binary(Max),
            {error, <<"充值金额不合法，请输入"/utf8, MinB/binary, "分到"/utf8, MaxB/binary, "分之间的整数"/utf8>>}
    end;
validate_amount(_) ->
    {error, <<"充值金额必须为整数（单位：分）"/utf8>>}.

%% @doc 支付方式白名单校验（生产禁用 mock）
-spec is_payment_method_allowed(binary()) -> boolean().
is_payment_method_allowed(Method) when is_binary(Method) ->
    Env = config_ds:env(env, <<"local">>),
    EnvBin = elib_cnv:safe_to_binary(Env),
    Allowed =
        case EnvBin of
            <<"pro">> -> ?ALLOWED_PAYMENT_METHODS;
            <<"prod">> -> ?ALLOWED_PAYMENT_METHODS;
            <<"production">> -> ?ALLOWED_PAYMENT_METHODS;
            _ -> ?DEV_ALLOWED_PAYMENT_METHODS
        end,
    lists:member(Method, Allowed);
is_payment_method_allowed(_) ->
    false.

%% @doc 加载订单并做面向前端的字段整形
-spec load_order(binary()) -> {ok, map()} | {error, binary()}.
load_order(OrderNo) ->
    case load_order_raw(OrderNo) of
        {ok, Order} -> {ok, order_transfer(Order)};
        {error, _} = Err -> Err
    end.

%% @doc 加载原始订单 map
-spec load_order_raw(binary()) -> {ok, map()} | {error, binary()}.
load_order_raw(OrderNo) ->
    case recharge_order_ds:find_by_order_no(OrderNo) of
        {ok, Order} when is_map(Order) ->
            {ok, Order};
        {error, not_found} ->
            {error, <<"订单不存在"/utf8>>};
        {error, Reason} when is_binary(Reason) ->
            {error, Reason};
        {error, Reason} ->
            {error, elib_cnv:safe_to_binary(Reason)}
    end.

%% @doc 面向前端的订单字段整形
-spec order_transfer(map()) -> map().
order_transfer(Order) ->
    Order.
