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
-export([make_reply_context/4, verify_reply_context/2]).
-export([dispatch_group_mention/5]).

-include("log.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 异步推送事件到 Bot 的 webhook URL。
%% WH-01：主路径只写 outbox（幂等：idempotency_key 唯一约束），不依赖外部
%% HTTP 成功；实际投递由 bot_webhook_delivery_worker 拉取执行（有界重试+死信）。
-spec push(integer(), map()) -> ok.
push(BotId, Event) ->
    %% L-01：overseas_baseline 预设默认关闭 Bot webhook 外呼
    case imboy_feature:enabled(bot_webhook) of
        true ->
            _ = elib_async:async(fun() ->
                do_push(BotId, Event)
            end),
            ok;
        false ->
            ok
    end.

%% @doc 封装消息为 webhook 推送格式并推送
-spec push_message(integer(), map(), map()) -> ok.
push_message(BotId, FromUser, Msg) ->
    %% L-01：overseas_baseline 预设默认关闭 Bot webhook 外呼
    case imboy_feature:enabled(bot_webhook) of
        false ->
            ok;
        true ->
            push_message_payload(BotId, FromUser, Msg)
    end.

push_message_payload(BotId, FromUser, Msg) ->
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
            case WebhookUrl of
                <<>> ->
                    ?WARN_LOG(
                        "bot_webhook_logic: bot_id=~p has no webhook_url, skip~n",
                        [BotId]
                    ),
                    ok;
                _ ->
                    enqueue(BotId, WebhookUrl, Event)
            end;
        {error, Reason} ->
            ?ERROR_LOG("bot_webhook_logic: find bot_id=~p failed: ~p~n", [BotId, Reason]),
            ok
    end.

%% 入队：构造契约 payload（delivery_id/correlation_id 一次定死）→ outbox 幂等写。
enqueue(BotId, WebhookUrl, Event) ->
    DeliveryId = iolist_to_binary(
        [
            "dlv-",
            integer_to_binary(erlang:unique_integer([positive])),
            "-",
            integer_to_binary(erlang:phash2({BotId, Event}))
        ]
    ),
    Corr = correlation_of(Event),
    EventType = event_type(Event),
    Envelope = #{
        <<"event">> => EventType,
        <<"delivery_id">> => DeliveryId,
        <<"correlation_id">> => Corr,
        <<"bot_id">> => BotId,
        <<"occurred_at">> => occurred_at(Event)
    },
    Body = jsone:encode(Envelope, [native_utf8]),
    Res = bot_webhook_delivery_repo:insert(#{
        delivery_id => DeliveryId,
        bot_id => BotId,
        event_type => EventType,
        payload => Body,
        correlation_id => Corr,
        idempotency_key => idem_key(BotId, Event),
        webhook_host => host_of(WebhookUrl)
    }),
    case Res of
        {ok, inserted} ->
            bot_webhook_delivery_worker:poll_now(),
            ok;
        {ok, duplicate} ->
            %% 重复事件：outbox 已有同幂等键，不重复投递（A03）
            ok;
        {error, Reason} ->
            ?ERROR_LOG(
                "bot_webhook_logic: enqueue bot_id=~p error ~p~n",
                [BotId, Reason]
            ),
            ok
    end.

event_type(Event) ->
    maps:get(<<"event">>, Event, <<"message">>).

occurred_at(Event) ->
    maps:get(<<"occurred_at">>, Event, <<>>).

correlation_id_of(Event) ->
    maps:get(<<"correlation_id">>, Event, undefined).

%% correlation_id：事件自带（上游受信入口生成）优先；否则本层生成（TRACE-00）
correlation_of(Event) ->
    case correlation_id_of(Event) of
        <<>> -> new_corr();
        undefined -> new_corr();
        C when is_binary(C), byte_size(C) >= 16 -> C;
        _ -> new_corr()
    end.

new_corr() ->
    Bin = binary:encode_hex(crypto:strong_rand_bytes(16), lowercase),
    <<"corr-", Bin/binary>>.

%% 幂等键：bot + 事件语义键（msg_id 或事件去重键），由调用方保证语义幂等
idem_key(BotId, Event) ->
    CaseId = maps:get(
        <<"idempotency_key">>,
        Event,
        maps:get(<<"msg_id">>, Event, <<>>)
    ),
    CaseIdBin =
        case CaseId of
            B when is_binary(B) -> B;
            _ -> iolist_to_binary(io_lib:format("~p", [CaseId]))
        end,
    iolist_to_binary(["bwd:", integer_to_binary(BotId), ":", CaseIdBin]).

host_of(Url) ->
    case uri_string:parse(Url) of
        #{host := H} -> ec_cnv:to_binary(H);
        _ -> <<>>
    end.

%% ===================================================================
%% BOT-01：reply context 签名令牌 + 群 mention 分派
%% ===================================================================

-define(REPLY_CTX_TTL_SECS, 900).
-define(REPLY_JTI_TAB, bot_reply_ctx_jti).

%% @doc 生成签名 reply context 令牌（不可伪造）：
%%   base64(json{group_id,trigger_msg_id,bot_id,correlation_id,jti,exp}) "." base64(HMAC)
%% Secret 取 Bot verify_token（AEAD 解密，fail-closed）。exp = now + 900s。
-spec make_reply_context(integer(), integer(), binary(), binary()) ->
    {ok, binary(), binary()} | {error, term()}.
make_reply_context(BotId, GroupId, TriggerMsgId, Corr) ->
    case bot_repo:get_verify_token(BotId) of
        {ok, Secret} ->
            Jti = iolist_to_binary(
                [
                    integer_to_binary(BotId),
                    "-",
                    integer_to_binary(erlang:unique_integer([positive]))
                ]
            ),
            Exp = os:system_time(second) + ?REPLY_CTX_TTL_SECS,
            PayloadBin = jsone:encode(#{
                <<"group_id">> => GroupId,
                <<"trigger_msg_id">> => TriggerMsgId,
                <<"bot_id">> => BotId,
                <<"correlation_id">> => Corr,
                <<"jti">> => Jti,
                <<"exp">> => Exp
            }),
            Sig = crypto:mac(hmac, sha256, Secret, PayloadBin),
            Token = iolist_to_binary(
                [base64:encode(PayloadBin), $., base64:encode(Sig)]
            ),
            {ok, Token, Jti};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 校验 reply context：签名（常量时间）+ 到期 + jti 一次性。
%% Secret 为认证后的 Bot verify secret 明文。
-spec verify_reply_context(binary(), binary()) ->
    {ok, map()} | {error, invalid | expired | reused}.
verify_reply_context(Token, Secret) when is_binary(Token), is_binary(Secret) ->
    case binary:split(Token, <<".">>) of
        [PayloadB64, SigB64] ->
            %% 签名基准 = payload 原文字节（非 base64 文本），与 make 一致
            PayloadBin = base64:decode(PayloadB64),
            Expect = crypto:mac(hmac, sha256, Secret, PayloadBin),
            Given = base64:decode(SigB64),
            %% 长度不等（篡改/截断）时 hash_equals 会 badarg，先守卫
            SigOk =
                byte_size(Given) =:= byte_size(Expect) andalso
                    crypto:hash_equals(Expect, Given),
            case SigOk of
                true ->
                    case jsone:decode(base64:decode(PayloadB64)) of
                        #{<<"exp">> := Exp} = Ctx when is_integer(Exp) ->
                            Now = os:system_time(second),
                            case Now =< Exp of
                                false ->
                                    {error, expired};
                                true ->
                                    case claim_jti(maps:get(<<"jti">>, Ctx, <<>>)) of
                                        true -> {ok, Ctx};
                                        false -> {error, reused}
                                    end
                            end;
                        _ ->
                            {error, invalid}
                    end;
                false ->
                    {error, invalid}
            end;
        _ ->
            {error, invalid}
    end;
verify_reply_context(_, _) ->
    {error, invalid}.

%% jti 一次性占位（ETS insert_new 原子；进程重启清空——窗口受 exp≤15min 约束）
claim_jti(Jti) when Jti =/= <<>> ->
    ok = ensure_jti_tab(),
    case ets:insert_new(?REPLY_JTI_TAB, {Jti, os:system_time(second)}) of
        true -> true;
        false -> false
    end;
claim_jti(_) ->
    false.

ensure_jti_tab() ->
    case ets:whereis(?REPLY_JTI_TAB) of
        undefined ->
            try ets:new(?REPLY_JTI_TAB, [set, public, named_table]) of
                _ -> ok
            catch
                error:badarg -> ok
            end;
        _ ->
            ok
    end.

%% @doc 群 mention 分派（msg_c2g_logic 成功发送后旁路调用，恒容错）。
%% 防自环：发送者是 bot（bot 表 user_id 命中）→ 跳过（断 Bot-to-Bot 环）。
%% E2EE fail-closed：Data.e2ee 非 null → 跳过。
-spec dispatch_group_mention(integer(), integer(), binary(), map(), [integer()]) -> ok.
dispatch_group_mention(FromUid, ToGID, Data, Payload, _MemberUids) ->
    try
        case is_bot_sender(FromUid) of
            true -> ok;
            false -> do_dispatch(FromUid, ToGID, Data, Payload)
        end
    catch
        C:R:ST ->
            ?ERROR_LOG("[BOT01] dispatch ~p:~p ~p~n", [C, R, ST]),
            ok
    end.

do_dispatch(FromUid, ToGID, Data, Payload) ->
    %% E2EE fail-closed：e2ee 消息不触发任何 bot 外呼
    case maps:get(<<"e2ee">>, Data, null) of
        null ->
            MsgId = maps:get(<<"id">>, Data, <<>>),
            Mentions = mentions_of(Payload),
            lists:foreach(
                fun(M) ->
                    Uid = to_int(M),
                    maybe_dispatch_one(Uid, ToGID, FromUid, MsgId)
                end,
                Mentions
            ),
            ok;
        _ ->
            ok
    end.

maybe_dispatch_one(BotUid, _ToGID, _FromUid, _MsgId) when BotUid =< 0 ->
    ok;
maybe_dispatch_one(BotUid, ToGID, FromUid, MsgId) ->
    case bot_repo:find(BotUid) of
        {ok, Bot} ->
            case
                maps:get(<<"status">>, Bot, 0) =:= 1 andalso
                    subscribed(Bot, <<"message.c2g_mention">>) andalso
                    maps:get(<<"webhook_url">>, Bot, <<>>) =/= <<>>
            of
                true ->
                    enqueue_mention(BotUid, Bot, ToGID, FromUid, MsgId);
                false ->
                    ok
            end;
        {error, _} ->
            ok
    end.

%% 订阅白名单判定（jsonb 文本包含目标 event 名；解析失败=未订阅）
subscribed(Bot, Event) ->
    EventsBin = maps:get(<<"events">>, Bot, <<"[]">>),
    try
        lists:member(Event, jsone:decode(EventsBin))
    catch
        _:_ -> false
    end.

%% 入队（幂等：bot+msg_id 唯一），payload 携带签名 reply_context
enqueue_mention(BotUid, Bot, ToGID, FromUid, MsgId) ->
    DeliveryId = iolist_to_binary(
        [
            "dlv-",
            integer_to_binary(erlang:unique_integer([positive])),
            "-",
            integer_to_binary(erlang:phash2({BotUid, MsgId}))
        ]
    ),
    Corr = new_corr(),
    case bot_webhook_logic:make_reply_context(BotUid, ToGID, MsgId, Corr) of
        {ok, ReplyCtx, _Jti} ->
            Envelope = #{
                <<"event">> => <<"message.c2g_mention">>,
                <<"delivery_id">> => DeliveryId,
                <<"correlation_id">> => Corr,
                <<"bot_id">> => BotUid,
                <<"occurred_at">> => elib_dt:to_rfc3339(os:system_time(second)),
                <<"data">> => #{
                    <<"group_id">> => ToGID,
                    <<"trigger_msg_id">> => MsgId,
                    <<"from_uid">> => FromUid,
                    <<"reply_context">> => ReplyCtx
                }
            },
            Body = jsone:encode(Envelope, [native_utf8]),
            case
                bot_webhook_delivery_repo:insert(#{
                    delivery_id => DeliveryId,
                    bot_id => BotUid,
                    event_type => <<"message.c2g_mention">>,
                    payload => Body,
                    correlation_id => Corr,
                    idempotency_key =>
                        iolist_to_binary(["bwd-mention:", BotUid, ":", MsgId]),
                    webhook_host => host_of_url(maps:get(<<"webhook_url">>, Bot, <<>>))
                })
            of
                {ok, inserted} ->
                    bot_webhook_delivery_worker:poll_now(),
                    ok;
                {ok, duplicate} ->
                    ok;
                {error, _} ->
                    ok
            end;
        {error, _} ->
            %% verify secret 解密失败 = fail-closed，不投递（无 reply 能力）
            ok
    end.

%% 防自环：发送者本身是 bot（bot 表命中）→ 跳过全部分派
is_bot_sender(FromUid) ->
    case bot_repo:find(FromUid) of
        {ok, _} -> true;
        _ -> false
    end.

host_of_url(Url) ->
    case uri_string:parse(Url) of
        #{host := H} -> ec_cnv:to_binary(H);
        _ -> <<>>
    end.

mentions_of(Payload) when is_map(Payload) ->
    maps:get(<<"mentions">>, Payload, []);
mentions_of(_) ->
    [].

to_int(V) when is_integer(V) -> V;
to_int(V) when is_binary(V) ->
    try binary_to_integer(V) of
        I -> I
    catch
        _:_ -> 0
    end;
to_int(_) ->
    0.
