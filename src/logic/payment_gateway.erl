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
    {ok, binary()} | {ok, binary(), map()} | {error, binary()}.
%% 返回 {ok, PaymentNo} 或 {ok, PaymentNo, Extra} 或 {error, Reason}。
%% Extra 携带网关向客户端透传的支付参数（如 Stripe client_secret、
%% 微信 prepay_id/paySign、支付宝 orderStr），由上层并入下单响应。

-callback refund(PaymentNo :: binary(), Amount :: term()) ->
    ok | {error, binary()}.

-export([pay/3, refund/3, method_module/1, registry/0]).
-export([enabled/0]).

%% @doc 外部支付网关总开关（默认关闭）。
%%
%% 为什么需要它：strict 环境（IMBOYENV != dev/local/test）下 imboy_app 会强制
%% `payment_mode=live` **且**至少一个网关凭据完整，否则节点 fail-fast；而
%% `sandbox` 在 strict 环境同样 fail-fast（sandbox_verify/3 完全跳过验签，
%% 回调端点又免 JWT，等于任何人可伪造入账）。两条路都堵死的结果是：
%% **没有真实支付商户凭据的部署方根本装不起来**。
%%
%% 本开关只关**外部网关**这一层（充值下单/支付/查单、网关回调、提现）。
%% 站内钱包账务（余额、流水、红包、转账）不受影响，仍然可用。
%%
%% 关闭 ≠ 放行：关闭时上述端点一律返回 ?ERR_FEATURE_DISABLED，
%% 而不是跳过校验后继续执行 —— 后者会把 fail-fast 变成静默的零验签通道。
-spec enabled() -> boolean().
enabled() ->
    application:get_env(imboy, payment_gateway_enabled, false) =:= true.

%% @doc 发起支付
-spec pay(binary(), binary(), map()) ->
    {ok, binary()} | {ok, binary(), map()} | {error, binary()}.
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
