-module(enterprise_webhook_logic).

%%%
% EPGZ-04 INT-12/13 企业 Webhook（配置/轮换/停用 + 事件入箱 + 投递执行）。
% FULL-03 扩展（Gate WEBHOOK_PASS，plan-full §3.1/§5/§7）：
%   * 投递账本：ownership（org/app 显式落行）、endpoint snapshot（url/host/pin
%     + 配置代际，DB 守卫强制不可变）、ledger_version（守卫独占写入的单调版本）、
%     claim（ewh_claimed_at + 租约）、terminal（success/dead 不可回退，重放走新行
%     + ewh_replay_of）。**不建第二套 outbox / worker**——全部是既有
%     bot_delivery + bot_delivery_worker 的扩展（plan-full §5）。
%   * 签名合同可验：verify/4（常量时间比较）与 verify_within/5（反重放时间窗），
%     头名由 signature_headers/0 单点声明（旋转后旧签名一律不通过）。
%   * 可观测：attempt/dead-letter/emit/replay 计数与投递延迟直方图全部进既有
%     elib_metric 通道（metrics 名见 metric_names/0）；只读统计/列表读面不含
%     secret、正文与签名 URL。
%   * 无正文/无 secret：envelope 键集封闭（envelope_keys/0）；投递行只存
%     信封（事件 id + 资源 id），不存消息正文；日志不落 secret/正文。
%
% 复用不建第二套（plan-gz §7.1）：
%   * durable outbox = bot_delivery / bot_delivery_attempt（迁移 92/104/141）；
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
    envelope_keys/0,
    signature_base/2,
    signature_headers/0,
    sign/2,
    verify/4,
    verify_within/5,
    configure_tx/3,
    replay_tx/3,
    emit_event_tx/4,
    emit_event_failed/4,
    execute_delivery/1,
    deliveries_tx/4,
    delivery_stats_tx/2,
    purgeable_tx/3,
    retention_days/0,
    metric_names/0
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

%% 信封键集封闭（plan-full §3.1「无正文/无 secret」）：envelope/3 只产出这些键，
%% 多一个键都是合同变更（套件按本清单逐键断言 == 集合相等）。
-define(ENVELOPE_KEYS, [
    <<"event_id">>,
    <<"delivery_id">>,
    <<"event_type">>,
    <<"version">>,
    <<"occurred_at">>,
    <<"organization_id">>,
    <<"application_id">>,
    <<"resource">>
]).
-define(ENVELOPE_RESOURCE_KEYS, [<<"type">>, <<"id">>]).

%% 签名头名（冻结合同；rotation/重放两侧共用同一组名字）
-define(HDR_DELIVERY, <<"x-imboy-delivery">>).
-define(HDR_EVENT, <<"x-imboy-event">>).
-define(HDR_TIMESTAMP, <<"x-imboy-timestamp">>).
-define(HDR_SIGNATURE, <<"x-imboy-signature">>).
%% 验签时间窗（秒）：默认 300（与常见 webhook 反重放窗口同量级）
-define(DEFAULT_VERIFY_WINDOW_S, 300).

%% 指标名（复用 elib_metric；dead-letter/成功率/重试计数全部经既有通道）
-define(METRIC_ATTEMPT, enterprise_webhook_attempt_total).
-define(METRIC_DEAD, enterprise_webhook_dead_letter_total).
-define(METRIC_LATENCY, enterprise_webhook_delivery_latency_seconds).
-define(METRIC_EMIT, enterprise_webhook_emit_total).
-define(METRIC_REPLAY, enterprise_webhook_replay_total).

-define(DEFAULT_RETENTION_DAYS, 30).

%%%===================================================================
%%% 纯合同（envelope / 签名）
%%%===================================================================

-spec events_whitelist() -> [binary()].
events_whitelist() ->
    ?EVENTS_WHITELIST.

%% @doc 信封键集（封闭；测试逐键断言）。
-spec envelope_keys() -> {[binary()], [binary()]}.
envelope_keys() ->
    {?ENVELOPE_KEYS, ?ENVELOPE_RESOURCE_KEYS}.

%% @doc 投递头名合同（timestamp/signature/delivery/event 四头，名字稳定）。
-spec signature_headers() -> map().
signature_headers() ->
    #{
        delivery => ?HDR_DELIVERY,
        event => ?HDR_EVENT,
        timestamp => ?HDR_TIMESTAMP,
        signature => ?HDR_SIGNATURE
    }.

%% @doc 指标名（复用既有 elib_metric 通道，不新建通道）。
-spec metric_names() -> map().
metric_names() ->
    #{
        attempt => ?METRIC_ATTEMPT,
        dead_letter => ?METRIC_DEAD,
        latency => ?METRIC_LATENCY,
        emit => ?METRIC_EMIT,
        replay => ?METRIC_REPLAY
    }.

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

%% @doc 验签（接收侧合同实现；常量时间比较，大小写不敏感 hex）。
%% secret 轮换后：用旧 secret 算出的签名在新 secret 下**必然为 false**
%% （无「双密钥并行窗口」——旋转即失效，见 FULL-03 checkpoint §能力对照）。
-spec verify(binary(), binary(), binary(), binary()) -> boolean().
verify(Secret, Timestamp, RawBody, Signature) when
    is_binary(Secret), is_binary(Timestamp), is_binary(RawBody), is_binary(Signature)
->
    Expected = sign(Secret, signature_base(Timestamp, RawBody)),
    constant_time_eq(Expected, Signature);
verify(_, _, _, _) ->
    false.

%% @doc 带时间窗的验签（反重放）：|now - timestamp| =< WindowSec 才可能为 true。
-spec verify_within(binary(), binary(), binary(), binary(), non_neg_integer()) -> boolean().
verify_within(Secret, Timestamp, RawBody, Signature, WindowSec) when
    is_integer(WindowSec), WindowSec >= 0
->
    case timestamp_seconds(Timestamp) of
        {ok, Ts} ->
            case abs(os:system_time(second) - Ts) =< WindowSec of
                true -> verify(Secret, Timestamp, RawBody, Signature);
                false -> false
            end;
        error ->
            false
    end;
verify_within(_, _, _, _, _) ->
    false.

timestamp_seconds(Timestamp) when byte_size(Timestamp) > 0, byte_size(Timestamp) =< 20 ->
    try binary_to_integer(Timestamp) of
        Ts when Ts >= 0 -> {ok, Ts};
        _ -> error
    catch
        _:_ -> error
    end;
timestamp_seconds(_) ->
    error.

%% 常量时间 hex 比较：先规范化大小写与长度，再走 crypto:hash_equals/2
%% （长度不同直接 false——长度本身不是秘密，签名是固定 64 hex）。
constant_time_eq(A, B) when byte_size(A) =:= byte_size(B) ->
    crypto:hash_equals(lower_hex(A), lower_hex(B));
constant_time_eq(_, _) ->
    false.

lower_hex(Bin) ->
    list_to_binary(string:lowercase(binary_to_list(Bin))).

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
                    configured_reply(
                        Conn,
                        Principal,
                        result(Url, Events, Status, true, #{
                            <<"secret">> => Secret
                        })
                    );
                {error, no_key} ->
                    {error, {<<"security_gate_closed">>, webhook_secret_key_missing}};
                {error, Reason} ->
                    {error, {<<"internal_error">>, Reason}}
            end;
        false ->
            configured_reply(Conn, Principal, result(Url, Events, Status, false, #{}))
    end.

%% @doc 配置成功统一出口：endpoint_generation 回填成功 → {ok, Result}；
%% 代际持久化失败 → {error, internal_error}（§15.1：不得 2xx 缺字段返回成功，
%% 让整个配置事务回滚）。
configured_reply(Conn, Principal, Result) ->
    case enterprise_webhook_repo:bump_generation_tx(Conn, Principal) of
        {ok, Gen} -> {ok, Result#{<<"endpoint_generation">> => Gen}};
        {error, Reason} -> {error, {<<"internal_error">>, {generation_bump_failed, Reason}}}
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
                pinned_ip => ip_to_binary(maps:get(ip, Pin)),
                %% FULL-03 账本：ownership + 端点配置代际（DB 守卫强制不可变）
                owner_organization_id => maps:get(organization_id, Ctx),
                owner_application_id => maps:get(application_id, Ctx),
                endpoint_generation => current_generation(Conn, Principal)
            },
            case enterprise_webhook_repo:insert_delivery_tx(Conn, Delivery) of
                {ok, inserted} ->
                    metric_emit(emitted),
                    {ok, emitted};
                {ok, duplicate} ->
                    metric_emit(duplicate),
                    {ok, skipped};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            %% SSRF/DNS 拒绝：跳过入箱并留痕（不阻断业务事务）。
            ?WARN_LOG("[EPGZ04] webhook emit guard rejected: ~p~n", [Reason]),
            metric_emit(skipped),
            {ok, skipped}
    end.

current_generation(Conn, Principal) ->
    case enterprise_webhook_repo:find_bot_config_tx(Conn, Principal) of
        {ok, Row} -> maps:get(<<"ewh_endpoint_generation">>, Row, 0);
        _ -> 0
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

%% @doc 重放本 Application delivery：新 delivery id、保留原 event id、
%% ewh_replay_of 指向原行。仅本 App 的行可重放（ownership 双证：行的
%% ewh_owner_* 列 + bot_id 前缀反查同 Org active Application；两者都过才放行）；
%% 仅已终结态（success/dead）可重放（在途 pending/retry → invalid_request 拒绝，
%% 避免并发重复投递）。DB 侧 uq_ewh_delivery_replay_inflight 保证同一原行
%% **同时最多一条在途重放**——并发重放第二次命中 23505 → idempotency_conflict。
-spec replay_tx(any(), map(), binary()) -> {ok, map()} | {error, {binary(), term()}}.
replay_tx(Conn, Ctx, DeliveryId) when is_binary(DeliveryId), DeliveryId =/= <<>> ->
    OrgId = maps:get(organization_id, Ctx),
    case enterprise_webhook_repo:find_delivery_tx(Conn, DeliveryId) of
        {ok, Delivery = #{<<"bot_id">> := BotId}} ->
            case replay_owner_ok(Conn, OrgId, Delivery) of
                true ->
                    replay_finalize(Conn, Ctx, Delivery, BotId);
                false ->
                    metric_replay(rejected),
                    {error, {<<"resource_not_found">>, delivery_not_found}}
            end;
        {ok, _} ->
            metric_replay(rejected),
            {error, {<<"resource_not_found">>, delivery_not_found}};
        {error, notfound} ->
            metric_replay(rejected),
            {error, {<<"resource_not_found">>, delivery_not_found}};
        {error, Reason} ->
            {error, {<<"internal_error">>, Reason}}
    end;
replay_tx(_Conn, _Ctx, _DeliveryId) ->
    {error, {<<"invalid_request">>, invalid_delivery_id}}.

%% ownership 双证：① 行的 ownership 列必须与本 ctx 的 org 一致（FULL-03 显式
%% 归属，fallback 到 bot_id 前缀反查以兼容迁移 141 之前写入的既有行——backfill
%% 已补齐，此处 fallback 只是防御）；② bot_id 前缀反查的 Application 必须属于
%% 本 Org 且 active。
replay_owner_ok(Conn, OrgId, Delivery) ->
    case maps:get(<<"ewh_owner_organization_id">>, Delivery, undefined) of
        RowOrg when is_integer(RowOrg), RowOrg =/= OrgId ->
            false;
        _ ->
            case
                enterprise_webhook_repo:principal_of_delivery(
                    maps:get(<<"bot_id">>, Delivery)
                )
            of
                {ok, Principal} ->
                    case
                        enterprise_webhook_repo:find_application_by_principal_tx(
                            Conn, OrgId, Principal
                        )
                    of
                        {ok, App} -> replay_owner_app_ok(Delivery, App);
                        _ -> false
                    end;
                error ->
                    false
            end
    end.

%% 行的 Application 归属必须与反查结果一致（防一行被写成「A 的 bot 前缀 + B 的
%% ownership」——DB 守卫也拦，这里是读侧的第二道）。
replay_owner_app_ok(Delivery, App) ->
    case maps:get(<<"ewh_owner_application_id">>, Delivery, undefined) of
        undefined -> true;
        null -> true;
        AppId -> AppId =:= maps:get(<<"id">>, App)
    end.

replay_finalize(Conn, Ctx, Delivery, BotId) ->
    case maps:get(<<"status">>, Delivery, <<>>) of
        Status when Status =:= <<"dead">>; Status =:= <<"success">> ->
            replay_insert(Conn, Ctx, Delivery, BotId);
        <<"pending">> ->
            metric_replay(rejected),
            {error, {<<"invalid_request">>, delivery_in_flight}};
        <<"retry">> ->
            metric_replay(rejected),
            {error, {<<"invalid_request">>, delivery_in_flight}};
        _ ->
            metric_replay(rejected),
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
            OriginalId = maps:get(<<"delivery_id">>, Delivery),
            Row = #{
                delivery_id => NewDeliveryId,
                bot_id => BotId,
                event_type => maps:get(<<"event_type">>, Delivery, <<"message">>),
                payload => jsone:encode(Env),
                correlation_id => new_correlation_id(),
                idempotency_key => <<"replay-", NewDeliveryId/binary>>,
                webhook_url => current_url(Conn, Principal),
                webhook_host => maps:get(host, Pin),
                pinned_ip => ip_to_binary(maps:get(ip, Pin)),
                %% 重放行的归属沿用原行（同 App），代际取**当前**配置代际
                %% （重放到当前端点，而不是已废弃的历史端点）
                owner_organization_id => maps:get(
                    <<"ewh_owner_organization_id">>, Delivery, undefined
                ),
                owner_application_id => maps:get(
                    <<"ewh_owner_application_id">>, Delivery, undefined
                ),
                replay_of => OriginalId,
                endpoint_generation => current_generation(Conn, Principal)
            },
            case enterprise_webhook_repo:insert_delivery_tx(Conn, Row) of
                {ok, inserted} ->
                    metric_replay(queued),
                    {ok, #{
                        <<"delivery_id">> => NewDeliveryId,
                        <<"event_id">> => EventId,
                        <<"original_delivery_id">> => OriginalId
                    }};
                {ok, duplicate} ->
                    %% 同一原行已有在途重放（唯一索引仲裁）
                    metric_replay(rejected),
                    {error, {<<"idempotency_conflict">>, replay_already_queued}};
                {error, Reason} ->
                    {error, {<<"internal_error">>, Reason}}
            end;
        {error, Reason} ->
            metric_replay(rejected),
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
                        {?HDR_DELIVERY, maps:get(<<"delivery_id">>, Delivery)},
                        {?HDR_EVENT, maps:get(<<"event_type">>, Delivery, <<"message">>)},
                        {?HDR_TIMESTAMP, Ts},
                        {?HDR_SIGNATURE, Sig}
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
    metric_latency(Lat),
    case Class of
        <<"2xx">> ->
            ok = audit(Did, AttemptNo, Class, Code, Lat, <<>>),
            settle_write(success, Did, fun() ->
                bot_webhook_delivery_repo:mark_success(Did, AttemptNo)
            end),
            metric_attempt(success, Class);
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
            settle_write(dead, Did, fun() ->
                bot_webhook_delivery_repo:mark_dead(Did, AttemptNo)
            end),
            metric_attempt(dead, Class),
            metric_dead(Class),
            ?WARN_LOG("[EPGZ04] delivery ~ts -> dead after ~p attempts~n", [Did, AttemptNo]),
            ok;
        [After | _] ->
            settle_write(retry, Did, fun() ->
                bot_webhook_delivery_repo:mark_retry(Did, After, AttemptNo, <<>>)
            end),
            metric_attempt(retry, Class),
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
    settle_write(dead, Did, fun() ->
        bot_webhook_delivery_repo:mark_dead(Did, AttemptNo)
    end),
    metric_attempt(dead, Class),
    metric_dead(Class).

%% 状态落账容错（FULL-03）：账本守卫把终态行冻结、并拒绝非法迁移——若本行已被
%% 另一个执行者终结（并发 worker / 重放竞争），mark_* 会拿到 {rollback, ...}
%% 或直接 raise。那不是投递失败，也不该炸掉 worker 批次：记 WARN 继续。
%% 任何情况下都不吞掉「成功」语义——成功路径的 mark_success 失败同样只留痕。
settle_write(Label, Did, Fun) ->
    try Fun() of
        {ok, _} ->
            ok;
        Other ->
            ?WARN_LOG("[FULL03] delivery ~ts settle ~p rejected: ~p~n", [Did, Label, Other]),
            ok
    catch
        Class:Reason ->
            ?WARN_LOG(
                "[FULL03] delivery ~ts settle ~p crash ~p:~p~n", [Did, Label, Class, Reason]
            ),
            ok
    end.

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
%%% FULL-03 可观测：指标（复用 elib_metric 通道）
%%%===================================================================

metric_attempt(Outcome, Class) ->
    _ = elib_metric:increment(?METRIC_ATTEMPT, 1, #{outcome => Outcome, class => Class}),
    ok.

metric_dead(Class) ->
    _ = elib_metric:increment(?METRIC_DEAD, 1, #{class => Class}),
    ok.

metric_latency(LatMs) when is_integer(LatMs), LatMs >= 0 ->
    elib_metric:record(?METRIC_LATENCY, LatMs / 1000);
metric_latency(_) ->
    ok.

metric_emit(Result) ->
    _ = elib_metric:increment(?METRIC_EMIT, 1, #{result => Result}),
    ok.

metric_replay(Result) ->
    _ = elib_metric:increment(?METRIC_REPLAY, 1, #{result => Result}),
    ok.

%%%===================================================================
%%% FULL-03 只读面：投递列表 / 统计 / 保留窗口
%%%===================================================================

%% @doc 本 Application 的投递列表 + 健康度摘要（org/app 归属过滤，页大小夹紧）。
%% 响应**不含 payload**（无正文/无 secret/无签名 URL），只有元数据 + 计数。
-spec deliveries_tx(any(), map(), map(), integer()) ->
    {ok, map()} | {error, {binary(), term()}}.
deliveries_tx(Conn, Ctx, Params, DefaultSize) when is_map(Params) ->
    OrgId = maps:get(organization_id, Ctx),
    AppId = maps:get(application_id, Ctx),
    Filters = #{
        size => maps:get(size, Params, DefaultSize),
        status => status_filter(maps:get(status, Params, undefined))
    },
    Page = int_or(maps:get(page, Params, 1), 1),
    case enterprise_webhook_repo:list_deliveries_tx(Conn, OrgId, AppId, Filters, Page) of
        {ok, Result} ->
            case delivery_stats_tx(Conn, Ctx) of
                {ok, Summary} -> {ok, Result#{summary => Summary}};
                {error, Reason} -> {error, {<<"internal_error">>, Reason}}
            end;
        {error, Reason} ->
            {error, {<<"internal_error">>, Reason}}
    end;
deliveries_tx(_Conn, _Ctx, _Params, _DefaultSize) ->
    {error, {<<"invalid_request">>, invalid_page_params}}.

%% @doc 投递健康度：状态计数 + 尝试/重试次数 + 死信数 + 成功率。
%% 成功率口径：success / (success + dead)——**在途（pending/retry）不计入分母**，
%% 因此「成功率」不会因为刚入箱还没投递而虚低。
-spec delivery_stats_tx(any(), map()) -> {ok, map()} | {error, {binary(), term()}}.
delivery_stats_tx(Conn, Ctx) ->
    OrgId = maps:get(organization_id, Ctx),
    AppId = maps:get(application_id, Ctx),
    case enterprise_webhook_repo:delivery_stats_tx(Conn, OrgId, AppId) of
        {ok, Stats} ->
            Success = maps:get(<<"success">>, maps:get(<<"status_counts">>, Stats), 0),
            Dead = maps:get(<<"dead_letter_count">>, Stats, 0),
            Settled = Success + Dead,
            Rate =
                case Settled of
                    0 -> null;
                    _ -> Success / Settled
                end,
            {ok, Stats#{
                <<"success_count">> => Success,
                <<"success_rate">> => Rate,
                <<"in_flight_count">> =>
                    maps:get(<<"pending">>, maps:get(<<"status_counts">>, Stats), 0) +
                    maps:get(<<"retry">>, maps:get(<<"status_counts">>, Stats), 0)
            }};
        {error, Reason} ->
            {error, {<<"internal_error">>, Reason}}
    end.

%% @doc 保留窗口（天）：配置缺省 30；只读集合，不做物理删除。
-spec retention_days() -> pos_integer().
retention_days() ->
    case config_ds:env(enterprise_webhook_retention_days, ?DEFAULT_RETENTION_DAYS) of
        D when is_integer(D), D > 0 -> D;
        _ -> ?DEFAULT_RETENTION_DAYS
    end.

%% @doc 保留窗口只读集合：已终结 + 早于 cutoff 的企业投递（在途永不入选）。
-spec purgeable_tx(any(), map(), pos_integer()) ->
    {ok, [map()]} | {error, {binary(), term()}}.
purgeable_tx(Conn, Ctx, Days) ->
    OrgId = maps:get(organization_id, Ctx),
    AppId = maps:get(application_id, Ctx),
    case enterprise_webhook_repo:purgeable_tx(Conn, OrgId, AppId, Days) of
        {ok, Rows} -> {ok, Rows};
        {error, Reason} -> {error, {<<"internal_error">>, Reason}}
    end.

status_filter(undefined) ->
    undefined;
status_filter(Status) when is_binary(Status) ->
    case lists:member(Status, [<<"pending">>, <<"retry">>, <<"success">>, <<"dead">>]) of
        true -> Status;
        false -> undefined
    end;
status_filter(_) ->
    undefined.

int_or(V, _Default) when is_integer(V) -> V;
int_or(V, Default) when is_binary(V) ->
    try binary_to_integer(V) of
        I -> I
    catch
        _:_ -> Default
    end;
int_or(_, Default) ->
    Default.

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
