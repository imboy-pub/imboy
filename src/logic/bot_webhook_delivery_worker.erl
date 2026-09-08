-module(bot_webhook_delivery_worker).
-behaviour(gen_server).

%%%
% WH-01 出站 Webhook 交付 worker：拉取 outbox 到期行 → guard 校验/pin →
% 签名发送 → 分类结果（2xx 成功 / 4xx 不重试 / 5xx-超时 有界重试 → dead）。
%
% 有界重试（PDT-01 webhook 契约 §2.6）：初始投递失败后按 [5, 30, 300] 秒退避
% 重试 3 次，耗尽进入 dead；410/4xx 不重试；410 直接死信。
% 每次尝试写 bot_delivery_attempt 审计（class/status/latency/截断错误；
% 不存响应正文与 secret）。
%%%

-export([start_link/0, poll_now/0]).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-include("log.hrl").

-define(POLL_INTERVAL_MS, 1000).
-define(BATCH_LIMIT, 10).
%% 有界重试：初始投递后最多 3 次重试（退避秒数）
-define(RETRY_SCHEDULE, [5, 30, 300]).
%% sender 可注入（测试 stub；生产用 bot_webhook_delivery_sender）
-define(SENDER_MOD,
    application:get_env(
        imboy,
        bot_webhook_sender_mod,
        bot_webhook_delivery_sender
    )
).

-spec start_link() -> gen_server:start_ret().
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc 立即触发一轮投递（供测试/运维）。
poll_now() ->
    gen_server:cast(?MODULE, poll).

init([]) ->
    schedule(0),
    {ok, #{}}.

handle_call(_R, _From, S) -> {reply, ok, S}.

handle_cast(poll, S) ->
    ok = run_batch(),
    schedule(?POLL_INTERVAL_MS),
    {noreply, S};
handle_cast(_R, S) ->
    {noreply, S}.

handle_info(poll, S) ->
    ok = run_batch(),
    schedule(?POLL_INTERVAL_MS),
    {noreply, S};
handle_info(_I, S) ->
    {noreply, S}.

terminate(_R, _S) -> ok.
code_change(_Old, S, _Extra) -> {ok, S}.

schedule(DelayMs) ->
    erlang:send_after(DelayMs, self(), poll).

sender_mod() ->
    case application:get_env(imboy, bot_webhook_sender_mod) of
        {ok, M} -> M;
        undefined -> bot_webhook_delivery_sender
    end.

%% ===================================================================
%% 单轮批量
%% ===================================================================

run_batch() ->
    try
        case bot_webhook_delivery_repo:claim_due(?BATCH_LIMIT) of
            {ok, Rows} ->
                lists:foreach(fun execute/1, Rows),
                ok;
            {error, Reason} ->
                ?WARN_LOG("[WH01] claim_due error ~p~n", [Reason]),
                ok
        end
    catch
        C:R:ST ->
            ?ERROR_LOG("[WH01] batch crash ~p:~p~n~p~n", [C, R, ST]),
            ok
    end.

%% @doc 执行单条交付：guard → 凭证解密 → 签名发送 → 分类落账。
execute(Delivery) ->
    #{<<"delivery_id">> := Did} = Delivery,
    AttemptNo = maps:get(<<"attempt_count">>, Delivery, 0) + 1,
    try
        do_execute(Delivery, AttemptNo)
    catch
        C:R:ST ->
            ?ERROR_LOG("[WH01] delivery ~ts crash ~p:~p~n~p~n", [Did, C, R, ST]),
            ok = audit(Did, AttemptNo, <<"internal_error">>, null, 0, R),
            finish_attempt(Did, AttemptNo, {error, internal_error})
    end.

do_execute(Delivery, AttemptNo) ->
    BotId = to_int(maps:get(<<"bot_id">>, Delivery)),
    Url =
        case bot_repo:find(to_int_or_bin(BotId)) of
            {ok, Bot} -> maps:get(<<"webhook_url">>, Bot, <<>>);
            {error, _} -> <<>>
        end,
    case bot_webhook_guard:validate_and_pin(Url) of
        {error, invalid_scheme} ->
            dead(Delivery, AttemptNo, <<"invalid_scheme">>, null),
            ok;
        {error, forbidden_host} ->
            dead(Delivery, AttemptNo, <<"forbidden_host">>, null),
            ok;
        {error, dns_failure} ->
            %% DNS 瞬时故障 → 有界重试
            retry(Delivery, AttemptNo, <<"dns_failure">>, null),
            ok;
        {ok, Pin} ->
            Host = maps:get(host, Pin),
            Port = maps:get(port, Pin),
            IP = maps:get(ip, Pin),
            PathQS = maps:get(path, Pin),
            IsTls = maps:get(tls, Pin, true),
            case bot_repo:get_verify_token(to_int_or_bin(BotId)) of
                {ok, VerifySecret} ->
                    Body = maps:get(<<"payload">>, Delivery, <<"{}">>),
                    Ts = integer_to_binary(os:system_time(second)),
                    %% 签名原文 = <timestamp>\n<raw body>（PDT-01 webhook 契约 §2.2）
                    SigBase = <<Ts/binary, "\n", Body/binary>>,
                    Sig = bot_webhook_logic:sign_payload(VerifySecret, SigBase),
                    Headers = [
                        {<<"x-imboy-delivery">>, maps:get(<<"delivery_id">>, Delivery)},
                        {<<"x-imboy-event">>, maps:get(<<"event_type">>, Delivery, <<"message">>)},
                        {<<"x-imboy-timestamp">>, Ts},
                        {<<"x-imboy-signature">>, Sig}
                    ],
                    T0 = erlang:monotonic_time(millisecond),
                    Res = (sender_mod()):post(IP, Port, IsTls, PathQS, Host, Headers, Body),
                    Lat = erlang:monotonic_time(millisecond) - T0,
                    settle(Delivery, AttemptNo, Res, Lat)
            end;
        {error, Reason} ->
            Did2 = maps:get(<<"delivery_id">>, Delivery),
            dead(Delivery, AttemptNo, <<"guard_error">>, null),
            ok = audit(Did2, AttemptNo, <<"guard_error">>, null, 0, Reason),
            ok
    end.

settle(Delivery, AttemptNo, {ok, Code} = Res, Lat) ->
    Did = maps:get(<<"delivery_id">>, Delivery),
    Class = class_of(Code),
    io:format(user, "~nDBG settle code=~p class=~p~n", [Code, Class]),
    ok = audit(Did, AttemptNo, Class, Code, Lat, <<>>),
    case Class of
        <<"2xx">> ->
            _ = bot_webhook_delivery_repo:mark_success(Did);
        <<"4xx">> when Code =:= 410 ->
            _ = bot_webhook_delivery_repo:mark_dead(Did, AttemptNo);
        <<"4xx">> ->
            %% 4xx 配置类错误：不重试，直接死信
            _ = bot_webhook_delivery_repo:mark_dead(Did, AttemptNo);
        _ ->
            %% 5xx/3xx/1xx：有界重试
            retry(Delivery, AttemptNo, Class, Code)
    end,
    Res;
settle(Delivery, AttemptNo, {error, Reason}, Lat) ->
    io:format(user, "~nDBG settle error=~p~n", [Reason]),
    Did = maps:get(<<"delivery_id">>, Delivery),
    ok = audit(Did, AttemptNo, <<"error">>, null, Lat, Reason),
    retry(Delivery, AttemptNo, <<"error">>, null),
    {error, Reason}.

retry(Delivery, AttemptNo, Class, Code) ->
    Did = maps:get(<<"delivery_id">>, Delivery),
    ok = audit(Did, AttemptNo, Class, Code, 0, <<>>),
    case retries_left(AttemptNo) of
        [] ->
            _ = bot_webhook_delivery_repo:mark_dead(Did, AttemptNo),
            ?WARN_LOG(
                "[WH01] delivery ~ts -> dead after ~p attempts~n",
                [Did, AttemptNo]
            ),
            ok;
        [After | _Rest] ->
            _ = bot_webhook_delivery_repo:mark_retry(Did, After, AttemptNo, <<>>),
            ok
    end.

retries_left(AttemptNo) ->
    %% AttemptNo=初始投递序号；剩余重试 = schedule 中第 AttemptNo 个之后
    Schedule = ?RETRY_SCHEDULE,
    case AttemptNo =< length(Schedule) of
        true -> [lists:nth(AttemptNo, Schedule)];
        false -> []
    end.

dead(Delivery, AttemptNo, Class, Code) ->
    Did = maps:get(<<"delivery_id">>, Delivery),
    ok = audit(Did, AttemptNo, Class, Code, 0, <<>>),
    _ = bot_webhook_delivery_repo:mark_dead(Did, AttemptNo).

audit(Did, AttemptNo, Class, Code, Lat, Reason) ->
    Id = iolist_to_binary([
        "bda-",
        integer_to_binary(erlang:unique_integer([positive])),
        "-",
        integer_to_binary(erlang:phash2({Did, AttemptNo}))
    ]),
    bot_webhook_delivery_repo:insert_attempt(Did, #{
        id => Id,
        attempt_no => AttemptNo,
        status_class => Class,
        http_status => Code,
        latency_ms => Lat,
        error_trunc => err_trunc(Reason)
    }),
    ok.

err_trunc(Reason) when is_binary(Reason) ->
    case byte_size(Reason) > 200 of
        true -> binary:part(Reason, 0, 200);
        false -> Reason
    end;
err_trunc(Reason) ->
    iolist_to_binary(io_lib:format("~p", [Reason])).

finish_attempt(_Did, _AttemptNo, _Res) ->
    ok.

class_of(Code) when Code >= 200, Code < 300 -> <<"2xx">>;
class_of(Code) when Code >= 300, Code < 400 -> <<"3xx">>;
class_of(Code) when Code >= 400, Code < 500 -> <<"4xx">>;
class_of(Code) when Code >= 500 -> <<"5xx">>;
class_of(_) -> <<"other">>.

to_int(V) when is_integer(V) -> V;
to_int(V) when is_binary(V) ->
    try binary_to_integer(V) of
        I -> I
    catch
        _:_ -> 0
    end;
to_int(_) ->
    0.

to_int_or_bin(V) when is_binary(V) ->
    try binary_to_integer(V) of
        I -> I
    catch
        _:_ -> V
    end;
to_int_or_bin(V) ->
    V.
