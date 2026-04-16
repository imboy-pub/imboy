-module(imboy_sms).

%%% @doc 短信服务模块
%%% 支持极光推送、云极短信等多种短信服务商

-include("log.hrl").


-export([jverification/1]).
-export([filter_mobile/1]).
-export([send/3]).


%% @doc 过滤手机号码前缀
%% 移除 +86 国际区号前缀
%% @param Mobile 手机号码
%% @returns 去除 +86 前缀的手机号
-spec filter_mobile(binary()) -> binary().
filter_mobile(<<"+86", Tail/binary>>) ->
    Tail;
filter_mobile(Mobile) ->
    Mobile.


%% @doc 发送短信
%% @param Mobile 手机号码
%% @param Content 短信内容或验证码
%% @param ServiceProvider 服务提供商（<<"yjsms">> 或 <<"jsms">>）
%% @returns {ok, success} | {error, Reason} | map()
%% @example
%% imboy_sms:send(<<"13800138000">>, <<"123456">>, <<"jsms">>).
-spec send(binary(), binary(), binary()) -> {ok, binary()} | {error, binary()} | map().
send(Mobile, Content, <<"yjsms">>) ->
    Username = config_ds:get(<<"yjsms_account">>),
    Password = config_ds:get(<<"yjsms_secret">>),
    URL = config_ds:get(<<"yjsms_url">>),
    Ts = elib_dt:millisecond(),
    Headers = [
        {"Content-Type","application/json"}
    ],
    % MD5(userName + timestamp + MD5(password))
    Sign = elib_hasher:md5(<<Username/binary, (integer_to_binary(Ts))/binary, (elib_hasher:md5(Password))/binary>>),
    Data = #{
        <<"userName">> => Username
        , <<"messageList">> => [
            #{
                <<"phone">> => filter_mobile(Mobile)
                , <<"content">> => Content
            }
        ]
        , <<"timestamp">> => Ts
        , <<"sign">> => Sign
    },
    % ?DEBUG_LOG([Data]),
    {ok, RespMap} = elib_req:post(URL, Data, Headers),
    % RespMap = elib_req:post(URL, Data, Headers),
    ok = ?DEBUG_LOG([RespMap]),
    Code = maps:get(<<"code">>, RespMap),
    case Code of
        0 ->
            {ok, <<"success">>};
        % 1 ->  帐号名为空
        % 1 ->  帐号名为空
        % 1 ->  帐号名为空
        % 1 ->  帐号名为空
        % 1 ->  帐号名为空
        % 1 ->  帐号名为空
        _ ->
            {error, maps:get(<<"message">>, RespMap)}
    end;


% https://docs.jiguang.cn/jsms/server/rest_api_jsms
% 发送单条模板短信 API
% imboy_sms:send(<<"13692177080">>, <<"123456">>).
send(Mobile, Code, <<"jsms">>) ->
    Username = config_ds:get(<<"jpush_app_key">>),
    Password = config_ds:get(<<"jpush_master_secret">>),
    Base64Credentials = base64:encode(<<Username/binary, ":", Password/binary>>),
    URL = <<"https://api.sms.jpush.cn/v1/messages">>,
    Headers = [
        {"Content-Type","application/json"}
        , {"Authorization", "Basic " ++ Base64Credentials}
    ],
    % 您的手机验证码：{{code}}，有效期5分钟，请勿泄露。如非本人操作，请忽略此短信。谢谢！
    Data = #{
        <<"temp_id">> => <<"1">>
        , <<"temp_para">> => #{
            <<"code">> => Code
        }
        , <<"mobile">> => Mobile
        , <<"sign_id">> => <<"28010">> % IMBoy
    },
    % ?DEBUG_LOG([Data]),
    % {ok, RespMap} = elib_req:post(URL, Data, Headers),
    RespMap = elib_req:post(URL, Data, Headers),
    ok = ?DEBUG_LOG([RespMap]),
    RespMap.


%% @doc 极光验证登录 Token
%% 提交 loginToken，验证后返回加密的手机号码
%% @param Tk 登录 Token
%% @returns {ok, Mobile} | {error, Reason}
%% @example
%% imboy_sms:jverification(LoginToken).
%% @see https://docs.jiguang.cn/jverification/server/rest_api/loginTokenVerify_api
-spec jverification(binary()) -> {ok, binary()} | {error, binary()}.
jverification(Tk) ->
    Username = config_ds:get(<<"jpush_app_key">>),
    Password = config_ds:get(<<"jpush_master_secret">>),
    Base64Credentials = base64:encode(<<Username/binary, ":", Password/binary>>),
    URL = <<"https://api.verification.jpush.cn/v1/web/loginTokenVerify">>,
    Headers = [
        {"Content-Type","application/json"}
        , {"Authorization", "Basic " ++ Base64Credentials}
    ],
    Data = #{
        <<"loginToken">> => Tk
    },
    {ok, RespMap} = elib_req:post(URL, Data, Headers),
    % RespMap = elib_req:post(URL, Data, Headers),
    ok = ?DEBUG_LOG([RespMap]),
    case maps:get(<<"code">>, RespMap, undefined) of
        8000 ->
            Phone = maps:get(<<"phone">>, RespMap),
            PemBin = config_ds:env(jverification_rsa_priv_key),
            Mobile = elib_cipher:rsa_decrypt(Phone, PemBin),
            {ok, Mobile};
        _ ->
            {error, maps:get(<<"content">>, RespMap, <<"unknown">>)}
    end.
