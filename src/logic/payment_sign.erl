-module(payment_sign).
%%%===================================================================
%%% @doc 支付回调验签 / Payment webhook signature verification
%%%
%%% 两种运行模式（由 sys.config {payment_mode, _} 决定）：
%%%   - sandbox（当前默认）：直通放行，用于 mock 网关联调跑通回调入账链路。
%%%   - live：全部复用 erlang_pay:verify_notify 做真实验签/解密
%%%     （支付宝 RSA2 / 微信平台公钥+AES-GCM / Stripe HMAC，全在 erlang_pay）；
%%%     凭据为空时返回 {error, no_credential}。
%%%
%%% 凭据读取（IMBOY_* 注入，application:get_env/3）：
%%%   stripe_webhook_secret / wechat_api_v3_key / alipay_public_key
%%%
%%% verify/3:
%%%   Gateway        :: binary()  网关标识（alipay/wechat/stripe/mock/...）
%%%   RawBody        :: binary()  回调原始报文（验签必须用原始字节，勿用解析后 map）
%%%   Headers        :: map()     回调 HTTP 头（Stripe 签名在 Stripe-Signature 头）
%%% @end
%%%===================================================================

-export([verify/3]).

-include("log.hrl").

%% @doc 验签入口。sandbox 直通；live 按网关校验。
%% 返回 {ok, Notify}：live 透出 erlang_pay 解密/验签后的明文 map（微信回调
%% 入账依赖此明文）；sandbox 返回 {ok, #{}}（调用方继续用 handler 解析的 Notify）。
-spec verify(binary(), binary(), map()) -> {ok, map()} | {error, atom()}.
verify(Gateway, RawBody, Headers) ->
    case payment_mode() of
        sandbox ->
            sandbox_verify(Gateway, RawBody, Headers);
        _ ->
            live_verify(Gateway, RawBody, Headers)
    end.

%%%===================================================================
%%% sandbox 模式 —— 直通（最朴素的存在性校验，不做密码学验签）
%%%===================================================================

-spec sandbox_verify(binary(), binary(), map()) -> {ok, map()}.
sandbox_verify(_Gateway, _RawBody, _Headers) ->
    %% sandbox 直通，返回空 map：调用方继续用 handler 解析的 Notify
    {ok, #{}}.

%%%===================================================================
%%% live 模式 —— 真实验签骨架（凭据为空即拒绝）
%%%===================================================================

%% live 验签全部复用 erlang_pay:verify_notify（真实密码学验签/解密在 erlang_pay）。
%% imboy 侧只读凭据、组 Cfg/Ctx、归一返回，不再自实现验签（剔除重复）。
-spec live_verify(binary(), binary(), map()) -> ok | {error, atom()}.
live_verify(<<"alipay">>, RawBody, _Headers) ->
    PubKey = cfg(alipay_public_key),
    case is_blank(PubKey) of
        true ->
            {error, no_credential};
        false ->
            %% 支付宝异步通知为 form 串（明文），解析为 map 交 erlang_pay 验签。
            Ctx = #{form => parse_form(RawBody)},
            normalize_verify(erlang_pay:verify_notify(alipay, #{public_key => PubKey}, Ctx))
    end;
live_verify(<<"wechat">>, RawBody, Headers) ->
    ApiV3Key = cfg(wechat_api_v3_key),
    PlatPub = cfg(wechat_platform_public_key),
    case is_blank(ApiV3Key) orelse is_blank(PlatPub) of
        true ->
            {error, no_credential};
        false ->
            %% 经 erlang_pay 平台公钥验签 + AES-256-GCM 解密。
            %% ⚠️ 微信回调体加密，入账所需明文字段在 verify_notify 返回值中；
            %%   当前 verify/3 仅返回 ok/error，明文未透出 —— 微信回调入账完整
            %%   需把 verify_notify 解密结果传给 payment_callback（callback 流程待重构）。
            Cfg = #{api_v3 => ApiV3Key, platform_public_key => PlatPub},
            Ctx = #{headers => Headers, body => RawBody},
            normalize_verify(erlang_pay:verify_notify(wechat, Cfg, Ctx))
    end;
live_verify(<<"stripe">>, RawBody, Headers) ->
    Secret = cfg(stripe_webhook_secret),
    case is_blank(Secret) of
        true ->
            {error, no_credential};
        false ->
            %% HMAC-SHA256 + 时间戳容差防重放，全在 erlang_pay。
            Ctx = #{headers => Headers, body => RawBody},
            normalize_verify(erlang_pay:verify_notify(stripe, #{webhook_secret => Secret}, Ctx))
    end;
live_verify(_Gateway, _RawBody, _Headers) ->
    %% 未知网关 live 下一律拒绝
    {error, unsupported_gateway}.

%% erlang_pay:verify_notify 返回归一：透出解密/验签后的明文 map；失败提取错误码
-spec normalize_verify(term()) -> {ok, map()} | {error, atom()}.
normalize_verify({ok, Data}) when is_map(Data) -> {ok, Data};
normalize_verify({ok, _}) -> {ok, #{}};
normalize_verify({error, {Code, _Msg}}) when is_atom(Code) -> {error, Code};
normalize_verify({error, Code}) when is_atom(Code) -> {error, Code};
normalize_verify({error, _}) -> {error, bad_signature}.

%% 支付宝 form 串(a=1&b=2&sign=...) → map，交 erlang_pay 按字典序验签
-spec parse_form(binary()) -> map().
parse_form(RawBody) when is_binary(RawBody) ->
    maps:from_list(cow_qs:parse_qs(RawBody));
parse_form(_) ->
    #{}.

%%%===================================================================
%%% 配置读取
%%%===================================================================

-spec payment_mode() -> sandbox | live | term().
payment_mode() ->
    application:get_env(imboy, payment_mode, sandbox).

-spec cfg(atom()) -> binary().
cfg(Key) ->
    application:get_env(imboy, Key, <<>>).

-spec is_blank(term()) -> boolean().
is_blank(<<>>) -> true;
is_blank("") -> true;
is_blank(undefined) -> true;
is_blank(_) -> false.
