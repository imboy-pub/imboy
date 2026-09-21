-module(imboy_sms_provider).

%% Owner 激活短信 provider 合同（GZAPP-06 / D11-D13）。
%%
%% 定位：只定义「发一条激活短信」的统一合同与平台选择；本卡只交付
%% fake/local 实现——真实短信（yjsms/jsms/aliyun 等）严禁在本卡实现或触发
%% （需要真实发送 = BLOCKED_EXTERNAL_CONFIRMATION，走后续卡）。
%%
%% 平台选择（config_ds，与 imboy_sms:send/3 同源配置）：
%%   config_ds:env([sms, platform]) == <<"fake">> → imboy_sms_fake
%%     （本地/测试：返回 ok 并把「待发」记录进内存 outbox，绝不外发）；
%%   其余取值（yjsms/jsms/aliyun/未配置）→ 本模块留子句返回
%%     {error, not_configured}——真实 provider 未实现，fail-closed 不外发。
%%
%% 调用方语义（D12）：send_activation 的成败不回滚任何企业/转移事务；
%% 失败仅驱动 invite.status → sms_failed，可重发。
%%
%% PII 边界：Mobile 只传给实现方与出站脱敏日志（imboy_mobile:mask/1），
%% 严禁出现在任何 ?INFO_LOG/?ERROR_LOG 的原始形态里。

-include("log.hrl").

-export([send_activation/3, provider/0]).

%% Owner 激活短信 provider 合同。
%% 实现方返回 ok | {error, Reason}；Reason 为 stable binary（可入日志，不含手机号）。
-callback send_activation(
    Mobile :: binary(),
    Token :: binary(),
    OrgName :: binary()
) -> ok | {error, binary()}.

%% @doc 派发一次激活短信发送尝试。永不抛异常——实现方异常收敛为
%% {error, provider_crashed}（D12：发送链路故障不冒泡打断业务调用方）。
-spec send_activation(binary(), binary(), binary()) -> ok | {error, binary()}.
send_activation(Mobile, Token, OrgName) when
    is_binary(Mobile), is_binary(Token), is_binary(OrgName)
->
    case provider() of
        fake ->
            try
                imboy_sms_fake:send_activation(Mobile, Token, OrgName)
            catch
                Class:Reason ->
                    _ = log_send_failure(Class, Reason, Mobile),
                    {error, provider_crashed}
            end;
        not_configured ->
            %% 真实 provider 未实现（本卡交付边界）：fail-closed，不外发。
            _ = log_not_configured(Mobile),
            {error, not_configured}
    end;
send_activation(_, _, _) ->
    {error, invalid_args}.

%% @doc 当前生效的 provider（config_ds [sms, platform]；fake | not_configured）。
-spec provider() -> fake | not_configured.
provider() ->
    case config_ds:env([sms, platform]) of
        <<"fake">> ->
            fake;
        _ ->
            not_configured
    end.

%% ------------------------------------------------------------------
%% Internal（日志全部走脱敏形态）
%% ------------------------------------------------------------------

log_send_failure(Class, Reason, Mobile) ->
    ?ERROR_LOG([
        owner_activation_sms_crashed,
        {class, Class},
        {reason, Reason},
        {mobile_masked, imboy_mobile:mask(Mobile)}
    ]).

log_not_configured(Mobile) ->
    ?ERROR_LOG([
        owner_activation_sms_not_configured,
        {mobile_masked, imboy_mobile:mask(Mobile)}
    ]).
