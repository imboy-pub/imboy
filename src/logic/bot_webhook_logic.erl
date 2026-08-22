-module(bot_webhook_logic).

%%%
% Bot Webhook 推送模块
%
% 职责：Bot 收到用户消息后，异步推送到 Bot 注册的 webhook_url
% 边界：仅 C2C 私聊触发；群内 @Bot 不在本期
% 范式：Slack 同款，只发签名不发 token
%%%

-export([push/2]).
-export([push_message/3]).
-export([sign_payload/2]).

-include("log.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 异步推送事件到 Bot 的 webhook URL
%% 必须经 elib_async 异步执行，HTTP 超时 5s，不阻塞主路径
-spec push(integer(), map()) -> ok.
push(BotId, Event) ->
    _ = elib_async:async(fun() ->
        do_push(BotId, Event)
    end),
    ok.

%% @doc 封装消息为 webhook 推送格式并推送
-spec push_message(integer(), map(), map()) -> ok.
push_message(BotId, FromUser, Msg) ->
    Payload = #{
        <<"event">> => <<"message">>,
        <<"from">> => #{
            <<"user_id">> => maps:get(<<"user_id">>, FromUser, 0),
            <<"nickname">> => maps:get(<<"nickname">>, FromUser, <<>>)
        },
        <<"chat">> => #{
            <<"type">> => <<"c2c">>,
            <<"chat_id">> => maps:get(<<"chat_id">>, Msg, <<>>)
        },
        <<"message">> => #{
            <<"msg_id">> => maps:get(<<"msg_id">>, Msg, <<>>),
            <<"msg_type">> => maps:get(<<"msg_type">>, Msg, <<>>),
            <<"text">> => maps:get(<<"text">>, Msg, <<>>)
        }
    },
    push(BotId, Payload).

%% @doc HMAC-SHA256 签名 payload
%% 用于 webhook 请求头 X-IMBoy-Signature
-spec sign_payload(binary(), binary()) -> binary().
sign_payload(Secret, Payload) ->
    Mac = crypto:mac(hmac, sha256, Secret, Payload),
    <<"sha256=", (binary:encode_hex(Mac))/binary>>.

%% ===================================================================
%% Internal
%% ===================================================================

-spec do_push(integer(), map()) -> ok.
do_push(BotId, Event) ->
    case bot_repo:find(BotId) of
        {ok, Bot} ->
            WebhookUrl = maps:get(<<"webhook_url">>, Bot, <<>>),
            VerifyToken = maps:get(<<"verify_token">>, Bot, <<>>),
            case WebhookUrl of
                <<>> ->
                    ?WARN_LOG("bot_webhook_logic: bot_id=~p has no webhook_url, skip~n", [BotId]),
                    ok;
                _ ->
                    PayloadBin = jsone:encode(Event, [native_utf8]),
                    Signature = sign_payload(VerifyToken, PayloadBin),
                    Headers = [
                        {"content-type", "application/json"},
                        {"X-IMBoy-Signature", binary_to_list(Signature)}
                    ],
                    %% 5s 整体超时（post/4）：挂死的 Bot 端点最多占用异步 worker 5 秒
                    case elib_req:post(binary_to_list(WebhookUrl), Event, Headers, 5000) of
                        {ok, _Resp} ->
                            ?DEBUG_LOG([bot_webhook_push_ok, BotId, Event]),
                            ok;
                        {error, Reason} ->
                            ?ERROR_LOG("bot_webhook_logic: push to bot_id=~p failed: ~p~n", [
                                BotId, Reason
                            ]),
                            ok
                    end
            end;
        {error, Reason} ->
            ?ERROR_LOG("bot_webhook_logic: find bot_id=~p failed: ~p~n", [BotId, Reason]),
            ok
    end.
