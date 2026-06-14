-module(payment_gateway).

%%%===================================================================
%%% @doc 支付网关 Behaviour 定义 + 配置驱动的网关注册表
%%%
%%% 所有实现者须导出 pay/3 和 refund/2。
%%%
%%% 网关注册表 = 内置默认表（mock/wallet）⊕ application env 覆盖
%%% （sys.config 的 {payment_gateways, #{<<"alipay">> => Mod, ...}}）。
%%%
%%% 新增网关只需：
%%%   ① 新建 payment_xxx_gateway.erl 实现 behaviour
%%%   ② 在 sys.config 的 payment_gateways 注册映射
%%% 无需修改本模块的 dispatch 逻辑 —— 解除多任务并行编辑冲突。
%%% @end
%%%===================================================================

-callback pay(OrderNo :: binary(), Amount :: term(), Opts :: map()) ->
    {ok, binary()} | {error, binary()}.
%% 返回 {ok, PaymentNo} 或 {error, Reason}

-callback refund(PaymentNo :: binary(), Amount :: term()) ->
    ok | {error, binary()}.

-export([pay/3, refund/3, method_module/1, registry/0]).

%% @doc 发起支付
-spec pay(binary(), binary(), map()) -> {ok, binary()} | {error, binary()}.
pay(Method, OrderNo, Opts) ->
    case method_module(Method) of
        {ok, Module} ->
            Amount = maps:get(amount, Opts, 0),
            Module:pay(OrderNo, Amount, Opts);
        {error, _} = Err ->
            Err
    end.

%% @doc 发起退款
-spec refund(binary(), binary(), term()) -> ok | {error, binary()}.
refund(Method, PaymentNo, Amount) ->
    case method_module(Method) of
        {ok, Module} ->
            Module:refund(PaymentNo, Amount);
        {error, _} = Err ->
            Err
    end.

%% @doc 支付方式 → 实现模块（配置驱动 + 内置默认回退）
-spec method_module(binary()) -> {ok, module()} | {error, binary()}.
method_module(Method) when is_binary(Method) ->
    case maps:find(Method, registry()) of
        {ok, Module} -> {ok, Module};
        error -> {error, <<"不支持的支付方式"/utf8>>}
    end;
method_module(_) ->
    {error, <<"不支持的支付方式"/utf8>>}.

%% @doc 网关注册表：内置默认 ⊕ sys.config 覆盖
-spec registry() -> #{binary() => module()}.
registry() ->
    Default = #{
        <<"mock">> => payment_mock_gateway,
        <<"wallet">> => payment_wallet_gateway
    },
    case application:get_env(imboy, payment_gateways, #{}) of
        Custom when is_map(Custom) -> maps:merge(Default, Custom);
        _ -> Default
    end.
