-module(imboy_sms).

-include_lib("imlib/include/log.hrl").


-export([jverification/1]).
-export([filter_mobile/1]).
-export([send/3]).

filter_mobile(<<"+86", Tail/binary>>) ->
    Tail;
filter_mobile(Mobile) ->
    Mobile.

send(Mobile, Content, <<"yjsms">>) ->
    Username = config_ds:get(<<"yjsms_account">>),
    Password = config_ds:get(<<"yjsms_secret">>),
    Ts = imboy_dt:millisecond(),
    URL = <<"http://39.108.93.159:8001/sms/api/sendMessageOne">>,
    Headers = [
        {"Content-Type","application/json"}
    ],
    % MD5(userName + timestamp + MD5(password))
    Sign = imboy_hasher:md5(<<Username/binary, (integer_to_binary(Ts))/binary, (imboy_hasher:md5(Password))/binary>>),
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
    % ?LOG([Data]),
    {ok, RespMap} = imboy_req:post(URL, Data, Headers),
    % RespMap = imboy_req:post(URL, Data, Headers),
    ?LOG([RespMap]),
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
    % ?LOG([Data]),
    % {ok, RespMap} = imboy_req:post(URL, Data, Headers),
    RespMap = imboy_req:post(URL, Data, Headers),
    ?LOG([RespMap]),
    RespMap.


% imboy_sms:jverification(LoginToken).
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
    {ok, RespMap} = imboy_req:post(URL, Data, Headers),
    % RespMap = imboy_req:post(URL, Data, Headers),
    % ?LOG([RespMap]),
    case maps:get(<<"code">>, RespMap, undefined) of
        8000 ->
            Phone = maps:get(<<"phone">>, RespMap),
            PemBin = config_ds:get(<<"jverification_rsa_priv_key">>),
            {ok, imboy_cipher:rsa_decrypt(Phone, PemBin)};
        _ ->
            {error, maps:get(<<"content">>, RespMap, <<"unknown">>)}
    end.
