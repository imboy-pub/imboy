-module(push_provider_jpush).
%%%
% push_provider_jpush 极光推送（JPush）provider adapter
%
% EPGZ-07：JPush REST API v3 的最小接入层，供
% push_notification_ds:do_send_push 按 platform=jpush 分派调用。
%
% 隐私不变量（fail-closed，与 push_notification_logic 常量哲学一致）：
%   - notification 仅携带 title/alert 调用方传入的常量文案，
%     不含消息正文/密文片段/发送者身份/extras；
%   - 错误返回值只含语义原子，不含 AppKey/MasterSecret 明文。
%
% 配置（{imboy, push} proplist，凭据走 sys.local/环境注入，严禁入库）：
%   {jpush_app_key,     "your-jpush-appkey"}
%   {jpush_master_secret, "your-jpush-secret"}
%   {jpush_push_url,    "https://api.jpush.cn/v3/push"}  %% 可选，默认同左
%
% 合同文档：docs/reference/push-provider-jpush-research-2026-09-21.md §3
%%%

-include("log.hrl").

-export([provider/0]).
-export([device_type/0]).
-export([send/3]).
-export([classify/2]).

%% JPush REST API v3 默认端点（测试/代理经 {jpush_push_url, ...} 覆盖）
-define(DEFAULT_PUSH_URL, <<"https://api.jpush.cn/v3/push">>).

%% JPush 业务错误码（响应 body 的 error.code）
-define(JPUSH_CODE_INVALID_TOKEN, 1003).
-define(JPUSH_CODE_UNAUTHORIZED, 1004).
-define(JPUSH_CODE_RATE_LIMITED, 1011).

%% ===================================================================
%% API
%% ===================================================================

%% @doc provider 标识（push_token.platform 存储值）
-spec provider() -> binary().
provider() ->
    <<"jpush">>.

%% @doc 目标设备 OS（一期 JPush 仅 Android）
-spec device_type() -> binary().
device_type() ->
    <<"android">>.

%% @doc 向单个 RegistrationID 发送 Android 通知
%%
%% 返回值：
%%   ok                            HTTP 200，推送受理
%%   {error, not_configured}       缺 jpush_app_key/master_secret（fail-closed，不发请求）
%%   {error, {jpush_error, invalid_token}}   400+code 1003，调用方应 deactivate_by_token
%%   {error, {jpush_error, unauthorized}}    401/code 1004，配置类错误，不重试不下线
%%   {error, {jpush_error, rate_limited}}    429/code 1011，可重试
%%   {error, {jpush_error, {status, N}}}     其余非 200，可重试
%%   {error, term()}                网络层错误原样透传，可重试
-spec send(binary(), binary(), binary()) ->
    ok | {error, not_configured} | {error, {jpush_error, term()}} | {error, term()}.
send(Token, Title, Body) when is_binary(Token), byte_size(Token) > 0 ->
    case get_config() of
        {ok, AppKey, MasterSecret, Url} ->
            Headers = [
                {<<"authorization">>, basic_auth(AppKey, MasterSecret)},
                {<<"content-type">>, <<"application/json">>}
            ],
            Payload = build_payload(Token, Title, Body),
            case push_provider_jpush_http:post(Url, Headers, Payload) of
                {ok, Status, RespBody} ->
                    case classify(Status, RespBody) of
                        ok -> ok;
                        {jpush_error, Reason} -> {error, {jpush_error, Reason}}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, not_configured} = Err ->
            ?DEBUG_LOG(["jpush not configured, skip push"]),
            Err
    end.

%% @doc 响应分类（纯函数，供测试直测）
%%
%% 200 → ok；401 → unauthorized；429 → rate_limited；
%% 其余状态码结合 body error.code 映射 JPush 业务错误码
%% （1003 invalid_token / 1004 unauthorized / 1011 rate_limited），
%% 无可识别错误码时落 {status, N}（可重试）。
-spec classify(integer(), binary()) -> ok | {jpush_error, term()}.
classify(200, _RespBody) ->
    ok;
classify(401, _RespBody) ->
    {jpush_error, unauthorized};
classify(429, _RespBody) ->
    {jpush_error, rate_limited};
classify(Status, RespBody) when is_integer(Status) ->
    case parse_error_code(RespBody) of
        ?JPUSH_CODE_INVALID_TOKEN -> {jpush_error, invalid_token};
        ?JPUSH_CODE_UNAUTHORIZED -> {jpush_error, unauthorized};
        ?JPUSH_CODE_RATE_LIMITED -> {jpush_error, rate_limited};
        _ -> {jpush_error, {status, Status}}
    end.

%% ===================================================================
%% Internal Functions
%% ===================================================================

%% @doc 读取 JPush 配置；AppKey/MasterSecret 缺失或为空即视为未配置
get_config() ->
    case application:get_env(imboy, push) of
        {ok, PushConfig} ->
            AppKey = to_binary(proplists:get_value(jpush_app_key, PushConfig)),
            MasterSecret = to_binary(proplists:get_value(jpush_master_secret, PushConfig)),
            Url = to_binary(
                proplists:get_value(jpush_push_url, PushConfig, ?DEFAULT_PUSH_URL)
            ),
            case is_non_empty(AppKey) andalso is_non_empty(MasterSecret) of
                true -> {ok, AppKey, MasterSecret, Url};
                false -> {error, not_configured}
            end;
        undefined ->
            {error, not_configured}
    end.

to_binary(undefined) -> <<>>;
to_binary(Value) -> elib_cnv:safe_to_binary(Value).

is_non_empty(<<>>) -> false;
is_non_empty(Bin) when is_binary(Bin) -> true;
is_non_empty(_) -> false.

%% @doc Basic base64(AppKey:MasterSecret) 鉴权头
basic_auth(AppKey, MasterSecret) ->
    Credentials = <<AppKey/binary, ":", MasterSecret/binary>>,
    <<"Basic ", (base64:encode(Credentials))/binary>>.

%% @doc 构造最小 Android 通知 body（fail-closed）
%%
%% 仅 platform/audience/notification 三键；notification.android 仅
%% title/alert，绝不携带 extras/消息正文/密文/发送者身份。
build_payload(Token, Title, Body) ->
    jsone:encode(
        #{
            <<"platform">> => [device_type()],
            <<"audience">> => #{
                <<"registration_id">> => [Token]
            },
            <<"notification">> => #{
                device_type() => #{
                    <<"title">> => Title,
                    <<"alert">> => Body
                }
            }
        },
        [native_utf8]
    ).

%% @doc 提取响应 body 的 error.code；非 JSON/无 error.code 返回 undefined
parse_error_code(RespBody) when is_binary(RespBody) ->
    try
        case jsone:try_decode(RespBody) of
            {ok, #{<<"error">> := #{<<"code">> := Code}}, _Rest} when is_integer(Code) ->
                Code;
            _ ->
                undefined
        end
    catch
        _:_ -> undefined
    end;
parse_error_code(_) ->
    undefined.
