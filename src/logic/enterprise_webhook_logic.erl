-module(enterprise_webhook_logic).

%%%
% EPGZ-04 INT-12/13 企业 Webhook（配置/轮换/停用 + 事件入箱 + 投递执行）。
%
% 复用不建第二套（plan-gz §7.1）：
%   * durable outbox = bot_delivery / bot_delivery_attempt（迁移 92/104）；
%     worker = bot_webhook_delivery_worker（同一轮询/重试 [5,30,300]/死信/
%     attempt 审计）；企业行以 bot_id 命名空间 'eapp:<principal_uid>' 区分，
%     worker 在 execute/1 入口按前缀分派到本模块 execute_delivery/1
%     （A0 复核的最小 hook 点）。
%   * SSRF/DNS pin/redirect/timeout = bot_webhook_guard（HTTPS-only、私网/
%     loopback/link-local/保留段全拒、混合解析 fail-closed、V1 不跟随 3xx）；
%     sender = bot_webhook_delivery_sender（pin IP 连接 + 域名 SNI/Host）。
%   * 配置载体 = bot 行（user_id=principal_user_id；详见
%     enterprise_webhook_repo 模块头）。HMAC secret 以 AEAD 密文存储
%     （postgre_aes_key 派生，与 bot_repo 同源），明文只在配置/轮换响应
%     出现一次。
%
% 签名合同（plan-gz §7.1）：HMAC-SHA256(secret, timestamp "." raw_body)——
% 与 bot 域的 "\n" 分隔不同（两侧合同各自冻结），因此企业投递不走
% worker 的 bot 签名分支，而由本模块 execute_delivery 全程执行。
%%%

-export([
    events_whitelist/0,
    envelope/3,
    signature_base/2,
    sign/2,
    configure_tx/3,
    replay_tx/3,
    emit_event_tx/4,
    emit_event_failed/4,
    execute_delivery/1
]).

-include("log.hrl").

-define(EVENTS_WHITELIST, [
    <<"message.enterprise.accepted">>,
    <<"message.enterprise.failed">>,
    <<"group.member.changed">>,
    <<"file.confirmed">>
]).

-define(ENVELOPE_VERSION, 1).
-define(RETRY_SCHEDULE, [5, 30, 300]).

%%%===================================================================
%%% 纯合同（envelope / 签名）
%%%===================================================================

-spec events_whitelist() -> [binary()].
events_whitelist() ->
    ?EVENTS_WHITELIST.

%% @doc 事件信封：event_id/delivery_id/event_type/version/occurred_at/
%% org/app/resource——无 secret、无正文、无签名 URL（plan-gz §7.1）。
%% occurred_at 为 ISO-8601 UTC 毫秒 Z。
-spec envelope(map(), binary(), map()) -> map().
envelope(Ctx, EventType, Resource) ->
    #{
        <<"event_id">> => maps:get(event_id, Resource),
        <<"delivery_id">> => maps:get(delivery_id, Resource),
        <<"event_type">> => EventType,
        <<"version">> => ?ENVELOPE_VERSION,
        <<"occurred_at">> => occurred_at(),
        <<"organization_id">> => maps:get(organization_id, Ctx),
        <<"application_id">> => maps:get(application_id, Ctx),
        <<"resource">> => #{
            <<"type">> => maps:get(resource_type, Resource),
            <<"id">> => maps:get(resource_id, Resource)
        }
    }.

%% @doc 签名原文 = <timestamp> "." <raw body>（plan-gz §7.1 冻结）。
-spec signature_base(binary(), binary()) -> binary().
signature_base(Timestamp, RawBody) ->
    <<Timestamp/binary, ".", RawBody/binary>>.

%% @doc HMAC-SHA256 hex 签名（复用 bot_webhook_logic 同款 mac 原语）。
-spec sign(binary(), binary()) -> binary().
sign(Secret, SigBase) ->
    bot_webhook_logic:sign_payload(Secret, SigBase).

%%%===================================================================
%%% INT-12 配置 / 轮换 / 停用
%%%===================================================================

%% @doc 配置本 Application endpoint + 订阅（upsert）。
%% Input（atom 键）：
%%   url      必填 binary（HTTPS；SSRF guard 即时校验——DNS 解析私网/保留段拒）
%%   events   必填 [binary]（⊆ 事件白名单；空列表 = 只停用订阅）
%%   status   可填 enabled | disabled（缺省 enabled）
%%   rotate   可填 boolean（true = 轮换 secret，响应返回新明文一次）
%% 返回 {ok, #{url, events, status, rotated, secret?}}。
-spec configure_tx(any(), map(), map()) -> {ok, map()} | {error, {binary(), term()}}.
configure_tx(Conn, Ctx, Input) when is_map(Input) ->
    Url = maps:get(url, Input, undefined),
    Events = maps:get(events, Input, undefined),
    Status = status_of(maps:get(status, Input, enabled)),
    Rotate = maps:get(rotate, Input, false) =:= true,
    case Status =/= invalid andalso valid_url(Url) andalso valid_events(Events) of
        true ->
            %% SSRF 守卫即时跑（DNS pin 结果不在此持久化——入箱时重新 pin
            %% 快照；此处只做拒绝性校验）。
            case bot_webhook_guard:validate_and_pin(Url) of
                {ok, _Pin} ->
                    configure_principal(Conn, Ctx, Url, Events, Status, Rotate);
                {error, Reason} ->
                    {error, {<<"invalid_request">>, {ssrf_or_invalid_url, Reason}}}
            end;
        false ->
            {error, {<<"invalid_request">>, invalid_webhook_config}}
    end;
configure_tx(_Conn, _Ctx, _Input) ->
    {error, {<<"invalid_request">>, input_not_map}}.

configure_principal(Conn, Ctx, Url, Events, Status, Rotate) ->
    Principal = maps:get(principal_user_id, Ctx, undefined),
    case is_integer(Principal) andalso Principal > 0 of
        true ->
            case application_of(Conn, Ctx, Principal) of
                {ok, App} ->
                    Username = enterprise_webhook_repo:bot_username(
                        maps:get(<<"application_key">>, App)
                    ),
                    StatusInt =
                        case Status of
                            enabled -> 1;
                            disabled -> 0
                        end,
                    case
                        enterprise_webhook_repo:upsert_config_tx(
                            Conn,
                            Principal,
                            #{
                                name => maps:get(<<"name">>, App, <<"application">>),
                                username => Username,
                                webhook_url => Url
                            },
                            Events,
                            StatusInt
                        )
                    of
                        {ok, _} ->
                            maybe_rotate(Conn, Principal, Rotate, Url, Events, Status);
                        {error, Reason} ->
                            {error, {<<"internal_error">>, Reason}}
                    end;
                {error, not_found} ->
                    {error, {<<"invalid_request">>, application_principal_required}};
                {error, Reason} ->
                    {error, {<<"internal_error">>, Reason}}
            end;
        false ->
            {error, {<<"invalid_request">>, application_principal_required}}
    end.

maybe_rotate(Conn, Principal, Rotate, Url, Events, Status) ->
    NeedSecret = Rotate orelse first_time(Conn, Principal),
    case NeedSecret of
        true ->
            Secret = new_secret(),
            case enterprise_webhook_repo:set_secret_tx(Conn, Principal, Secret) of
                {ok, updated} ->
                    {ok, result(Url, Events, Status, true, #{<<"secret">> => Secret})};
                {error, no_key} ->
                    {error, {<<"security_gate_closed">>, webhook_secret_key_missing}};
                {error, Reason} ->
                    {error, {<<"internal_error">>, Reason}}
            end;
        false ->
            {ok, result(Url, Events, Status, false, #{})}
    end.

first_time(Conn, Principal) ->
    case enterprise_webhook_repo:find_bot_config_tx(Conn, Principal) of
        {ok, Row} ->
            maps:get(<<"verify_token_enc">>, Row, <<>>) =:= <<>>;
        {error, not_found} ->
            true
    end.

result(Url, Events, Status, Rotated, Extra) ->
    maps:merge(
        #{
            <<"url">> => Url,
            <<"events">> => Events,
            <<"status">> => Status,
            <<"rotated">> => Rotated
        },
        Extra
    ).

%%%===================================================================
%%% 事件入箱（durable outbox）
%%%===================================================================

%% @doc 事件入箱（与业务写入同事务调用）。
%% 订阅判定（events 白名单 ∩ 本 App 订阅）+ 当前端点 SSRF pin 快照；
%% guard 拒绝/未订阅 → {ok, skipped}（事件是旁路，不阻断业务主链）。
%% 幂等键 evt-<event_id>（同事件不重复入箱；replay 走新行新键）。
-spec emit_event_tx(any(), map(), binary(), map()) ->
    {ok, emitted | skipped} | {error, term()}.
emit_event_tx(Conn, Ctx, EventType, Resource0) when is_binary(EventType) ->
    case lists:member(EventType, ?EVENTS_WHITELIST) of
        false ->
            {ok, skipped};
        true ->
            Principal = maps:get(principal_user_id, Ctx, undefined),
            case is_integer(Principal) andalso Principal > 0 of
                false ->
                    {ok, skipped};
                true ->
                    case subscribed(Conn, Principal, EventType) of
                        true ->
                            do_emit(Conn, Ctx, EventType, Resource0);
                        false ->
                            {ok, skipped}
                    end
            end
    end.

do_emit(Conn, Ctx, EventType, Resource0) ->
    Principal = maps:get(principal_user_id, Ctx, undefined),
    case bot_webhook_guard:validate_and_pin(current_url(Conn, Principal)) of
        {ok, Pin} ->
            EventId = maps:get(event_id, Resource0, new_event_id()),
            DeliveryId = new_delivery_id(),
            Resource = Resource0#{
                event_id => EventId,
                delivery_id => DeliveryId
            },
            Env = envelope(Ctx, EventType, Resource),
            Delivery = #{
                delivery_id => DeliveryId,
                bot_id => enterprise_webhook_repo:delivery_bot_id(Principal),
                event_type => EventType,
                payload => jsone:encode(Env),
                correlation_id => new_correlation_id(),
                idempotency_key => <<"evt-", EventId/binary>>,
                webhook_url => current_url(Conn, Principal),
                webhook_host => maps:get(host, Pin),
                pinned_ip => ip_to_binary(maps:get(ip, Pin))
            },
            case enterprise_webhook_repo:insert_delivery_tx(Conn, Delivery) of
                {ok, inserted} -> {ok, emitted};
                {ok, duplicate} -> {ok, skipped};
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            %% SSRF/DNS 拒绝：跳过入箱并留痕（不阻断业务事务）。
            ?WARN_LOG("[EPGZ04] webhook emit guard rejected: ~p~n", [Reason]),
            {ok, skipped}
    end.

%% @doc message.enterprise.failed（业务 {error} 后由 handler 在 ROLLBACK 之后
%% 调用——独立小事务，不与已回滚的业务事务绑定）。
-spec emit_event_failed(map(), binary(), map(), binary()) -> ok.
emit_event_failed(Ctx, EventType, Resource, ReasonCode) ->
    Fun = fun(Conn) ->
        _ = emit_event_tx(
            Conn,
            Ctx,
            EventType,
            maps:merge(Resource, #{reason_code => ReasonCode})
        ),
        ok
    end,
    try
        {ok, ok} = elib_pg:with_tx(Fun),
        ok
    catch
        Class:Reason ->
            ?ERROR_LOG("[EPGZ04] emit_event_failed crash ~p:~p~n", [Class, Reason]),
            ok
    end.

%%%===================================================================
%%% INT-13 replay
%%%===================================================================

%% @doc 重放本 Application delivery：新 delivery id、保留原 event id。
%% 仅本 App 的行可重放（bot_id=eapp:<本 app principal> 反查同 Org active
%% Application；不属/不存在 → resource_not_found）；仅已终结态
%% （success/dead）可重放（在途 pending/retry → idempotency_conflict 语义
%% 的 invalid_request 拒绝，避免并发重复投递）。
-spec replay_tx(any(), map(), binary()) -> {ok, map()} | {error, {binary(), term()}}.
replay_tx(Conn, Ctx, DeliveryId) when is_binary(DeliveryId), DeliveryId =/= <<>> ->
    OrgId = maps:get(organization_id, Ctx),
    _ = OrgId,
    case enterprise_webhook_repo:find_delivery_tx(Conn, DeliveryId) of
        {ok, Delivery = #{<<"bot_id">> := BotId}} ->
            case replay_owner_ok(Conn, OrgId, BotId) of
                true ->
                    replay_finalize(Conn, Ctx, Delivery, BotId);
                false ->
                    {error, {<<"resource_not_found">>, delivery_not_found}}
            end;
        {ok, _} ->
            {error, {<<"resource_not_found">>, delivery_not_found}};
        {error, notfound} ->
            {error, {<<"resource_not_found">>, delivery_not_found}};
        {error, Reason} ->
            {error, {<<"internal_error">>, Reason}}
    end;
replay_tx(_Conn, _Ctx, _DeliveryId) ->
    {error, {<<"invalid_request">>, invalid_delivery_id}}.

replay_owner_ok(Conn, OrgId, BotId) ->
    case enterprise_webhook_repo:principal_of_delivery(BotId) of
        {ok, Principal} ->
            case enterprise_webhook_repo:find_application_by_principal_tx(Conn, OrgId, Principal) of
                {ok, _} -> true;
                _ -> false
            end;
        error ->
            false
    end.

replay_finalize(Conn, Ctx, Delivery, BotId) ->
    case maps:get(<<"status">>, Delivery, <<>>) of
        Status when Status =:= <<"dead">>; Status =:= <<"success">> ->
            replay_insert(Conn, Ctx, Delivery, BotId);
        <<"pending">> ->
            {error, {<<"invalid_request">>, delivery_in_flight}};
        <<"retry">> ->
            {error, {<<"invalid_request">>, delivery_in_flight}};
        _ ->
            {error, {<<"invalid_request">>, delivery_not_replayable}}
    end.

replay_insert(Conn, _Ctx, Delivery, BotId) ->
    Original = maps:get(<<"payload">>, Delivery, <<"{}">>),
    EventId =
        try jsone:decode(Original) of
            #{<<"event_id">> := Eid} -> Eid;
            _ -> new_event_id()
        catch
            _:_ -> new_event_id()
        end,
    {ok, Principal} = enterprise_webhook_repo:principal_of_delivery(BotId),
    case bot_webhook_guard:validate_and_pin(current_url(Conn, Principal)) of
        {ok, Pin} ->
            NewDeliveryId = new_delivery_id(),
            Env0 =
                try
                    jsone:decode(Original)
                catch
                    _:_ -> #{}
                end,
            Env =
                case is_map(Env0) of
                    true -> Env0#{<<"delivery_id">> => NewDeliveryId};
                    false -> #{<<"event_id">> => EventId, <<"delivery_id">> => NewDeliveryId}
                end,
            Row = #{
                delivery_id => NewDeliveryId,
                bot_id => BotId,
                event_type => maps:get(<<"event_type">>, Delivery, <<"message">>),
                payload => jsone:encode(Env),
                correlation_id => new_correlation_id(),
                idempotency_key => <<"replay-", NewDeliveryId/binary>>,
                webhook_url => current_url(Conn, Principal),
                webhook_host => maps:get(host, Pin),
                pinned_ip => ip_to_binary(maps:get(ip, Pin))
            },
            case enterprise_webhook_repo:insert_delivery_tx(Conn, Row) of
                {ok, inserted} ->
                    {ok, #{
                        <<"delivery_id">> => NewDeliveryId,
                        <<"event_id">> => EventId,
                        <<"original_delivery_id">> => maps:get(<<"delivery_id">>, Delivery)
                    }};
                {ok, duplicate} ->
                    {error, {<<"idempotency_conflict">>, replay_already_queued}};
                {error, Reason} ->
                    {error, {<<"internal_error">>, Reason}}
            end;
        {error, Reason} ->
            {error, {<<"invalid_request">>, {ssrf_or_invalid_url, Reason}}}
    end.

%%%===================================================================
%%% 投递执行（bot_webhook_delivery_worker 企业分派目标）
%%%===================================================================

%% @doc 执行单条企业交付（worker execute/1 分派入口；与 bot 路径同一
%% outbox 行/重试表/死信语义）：guard 快照校验 -> secret 解密 ->
%% HMAC-SHA256(ts "." body) 签名 -> sender post -> 2xx 成功 /
%% 4xx 死信 / 5xx-超时 有界重试（[5,30,300] 秒，耗尽 dead）。
%% 每次 attempt 写 bot_delivery_attempt（class/status/latency，无 secret）。
-spec execute_delivery(map()) -> ok.
execute_delivery(Delivery) ->
    Did = maps:get(<<"delivery_id">>, Delivery),
    AttemptNo = maps:get(<<"attempt_count">>, Delivery, 0) + 1,
    try
        do_execute_delivery(Delivery, AttemptNo)
    catch
        Class:Reason ->
            ?ERROR_LOG("[EPGZ04] delivery ~ts crash ~p:~p~n", [Did, Class, Reason]),
            retry(Delivery, AttemptNo, <<"internal_error">>, null, 0, Reason),
            ok
    end.

do_execute_delivery(Delivery, AttemptNo) ->
    Url = maps:get(<<"webhook_url">>, Delivery, <<>>),
    PinnedIP = maps:get(<<"pinned_ip">>, Delivery, <<>>),
    case bot_webhook_guard:validate_pinned(Url, PinnedIP) of
        {ok, Pin} ->
            Host = maps:get(host, Pin),
            Port = maps:get(port, Pin),
            IP = maps:get(ip, Pin),
            PathQS = maps:get(path, Pin),
            IsTls = maps:get(tls, Pin, true),
            {ok, Principal} = enterprise_webhook_repo:principal_of_delivery(
                maps:get(<<"bot_id">>, Delivery)
            ),
            case enterprise_webhook_repo:get_secret(Principal) of
                {ok, Secret} ->
                    Body = maps:get(<<"payload">>, Delivery, <<"{}">>),
                    Ts = integer_to_binary(os:system_time(second)),
                    %% plan-gz §7.1 冻结：timestamp "." raw_body（企业合同，
                    %% 与 bot 域 "\n" 分隔各自冻结，互不改写）。
                    SigBase = signature_base(Ts, Body),
                    Sig = sign(Secret, SigBase),
                    Headers = [
                        {<<"x-imboy-delivery">>, maps:get(<<"delivery_id">>, Delivery)},
                        {<<"x-imboy-event">>, maps:get(<<"event_type">>, Delivery, <<"message">>)},
                        {<<"x-imboy-timestamp">>, Ts},
                        {<<"x-imboy-signature">>, Sig}
                    ],
                    T0 = erlang:monotonic_time(millisecond),
                    Res = (sender_mod()):post(IP, Port, IsTls, PathQS, Host, Headers, Body),
                    Lat = erlang:monotonic_time(millisecond) - T0,
                    _ = settle(Delivery, AttemptNo, Res, Lat),
                    ok;
                {error, Reason} ->
                    retry(Delivery, AttemptNo, <<"credential_error">>, null, 0, Reason),
                    ok
            end;
        {error, Reason} ->
            dead(Delivery, AttemptNo, guard_class(Reason), null, 0, Reason),
            ok
    end.

settle(Delivery, AttemptNo, {ok, Code} = Res, Lat) ->
    Did = maps:get(<<"delivery_id">>, Delivery),
    Class = class_of(Code),
    case Class of
        <<"2xx">> ->
            ok = audit(Did, AttemptNo, Class, Code, Lat, <<>>),
            {ok, _} = bot_webhook_delivery_repo:mark_success(Did, AttemptNo);
        <<"4xx">> ->
            dead(Delivery, AttemptNo, Class, Code, Lat, <<>>);
        _ ->
            retry(Delivery, AttemptNo, Class, Code, Lat, <<>>)
    end,
    Res.

retry(Delivery, AttemptNo, Class, Code, Lat, Reason) ->
    Did = maps:get(<<"delivery_id">>, Delivery),
    ok = audit(Did, AttemptNo, Class, Code, Lat, Reason),
    case retries_left(AttemptNo) of
        [] ->
            {ok, _} = bot_webhook_delivery_repo:mark_dead(Did, AttemptNo),
            ?WARN_LOG("[EPGZ04] delivery ~ts -> dead after ~p attempts~n", [Did, AttemptNo]),
            ok;
        [After | _] ->
            {ok, _} = bot_webhook_delivery_repo:mark_retry(Did, After, AttemptNo, <<>>),
            ok
    end.

retries_left(AttemptNo) ->
    case AttemptNo =< length(?RETRY_SCHEDULE) of
        true -> [lists:nth(AttemptNo, ?RETRY_SCHEDULE)];
        false -> []
    end.

dead(Delivery, AttemptNo, Class, Code, Lat, Reason) ->
    Did = maps:get(<<"delivery_id">>, Delivery),
    ok = audit(Did, AttemptNo, Class, Code, Lat, Reason),
    {ok, _} = bot_webhook_delivery_repo:mark_dead(Did, AttemptNo).

audit(Did, AttemptNo, Class, Code, Lat, Reason) ->
    Id = iolist_to_binary([
        "ewda-",
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
    }).

err_trunc(Reason) when is_binary(Reason) ->
    case byte_size(Reason) > 200 of
        true -> binary:part(Reason, 0, 200);
        false -> Reason
    end;
err_trunc(Reason) ->
    iolist_to_binary(io_lib:format("~p", [Reason])).

class_of(Code) when Code >= 200, Code < 300 -> <<"2xx">>;
class_of(Code) when Code >= 300, Code < 400 -> <<"3xx">>;
class_of(Code) when Code >= 400, Code < 500 -> <<"4xx">>;
class_of(Code) when Code >= 500 -> <<"5xx">>;
class_of(_) -> <<"other">>.

guard_class(Reason) when is_atom(Reason) -> atom_to_binary(Reason);
guard_class(_) -> <<"guard_error">>.

sender_mod() ->
    case application:get_env(imboy, bot_webhook_sender_mod) of
        {ok, M} -> M;
        undefined -> bot_webhook_delivery_sender
    end.

%%%===================================================================
%%% Internal
%%%===================================================================

subscribed(Conn, Principal, EventType) ->
    case enterprise_webhook_repo:find_bot_config_tx(Conn, Principal) of
        {ok, #{<<"status">> := 1, <<"events">> := EventsJson}} ->
            Events =
                try
                    jsone:decode(EventsJson)
                catch
                    _:_ -> []
                end,
            is_list(Events) andalso lists:member(EventType, Events);
        _ ->
            false
    end.

current_url(Conn, Principal) ->
    case enterprise_webhook_repo:find_bot_config_tx(Conn, Principal) of
        {ok, #{<<"webhook_url">> := Url, <<"status">> := 1}} when Url =/= <<>> ->
            Url;
        _ ->
            <<>>
    end.

application_of(Conn, Ctx, Principal) ->
    enterprise_webhook_repo:find_application_by_principal_tx(
        Conn, maps:get(organization_id, Ctx), Principal
    ).

valid_url(<<"https://", _/binary>> = Url) ->
    byte_size(Url) =< 2048;
valid_url(_) ->
    false.

valid_events(Events) when is_list(Events) ->
    length(Events) =< length(?EVENTS_WHITELIST) andalso
        lists:all(fun(E) -> lists:member(E, ?EVENTS_WHITELIST) end, Events);
valid_events(_) ->
    false.

status_of(enabled) -> enabled;
status_of(disabled) -> disabled;
status_of(_) -> invalid.

new_secret() ->
    %% 32B CSPRNG base64url（无 padding）——credential 同款熵。
    Base64 = base64url(crypto:strong_rand_bytes(32)),
    <<"whsec_", Base64/binary>>.

base64_url_encode(Bin) ->
    base64:encode(Bin, #{padding => false}).

base64url(Bin) ->
    Url = base64_url_encode(Bin),
    binary:replace(Url, <<"+">>, <<"-">>, [global]).

new_event_id() ->
    iolist_to_binary([
        "evt-",
        integer_to_binary(erlang:unique_integer([positive])),
        "-",
        binary:encode_hex(crypto:strong_rand_bytes(8))
    ]).

new_delivery_id() ->
    iolist_to_binary([
        "ewd-",
        integer_to_binary(erlang:unique_integer([positive])),
        "-",
        binary:encode_hex(crypto:strong_rand_bytes(8))
    ]).

new_correlation_id() ->
    binary:encode_hex(crypto:strong_rand_bytes(16)).

ip_to_binary(IP) when is_tuple(IP) ->
    iolist_to_binary(inet:ntoa(IP));
ip_to_binary(IP) when is_binary(IP) ->
    IP;
ip_to_binary(_) ->
    <<>>.

occurred_at() ->
    elib_dt:to_rfc3339(erlang:system_time(millisecond), millisecond).
