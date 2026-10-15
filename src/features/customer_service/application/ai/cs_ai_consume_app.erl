%%% @doc A 组（v4 02-A）事件消费编排：访客事件 → 幂等派生 AI job + reconcile
%%% sweep + 启动校验。自 `cs_ai_app` 按域拆出（文件行数规范收口，2026-10-15）；
%%% 函数体与语义未改，数据面仍在 `cs_pg_ai`（经 `impl/1` 动态派发，注入面同构）。
-module(cs_ai_consume_app).

-moduledoc "智能客服 AI 事件消费编排（A 组：批量消费 / sweep / 启动校验）。".

-export([
    consume_batch/3,
    reconcile_sweep/0,
    reconcile_sweep/1,
    validate_consume_config/0,
    apply_event/6
]).

%% A 组默认（工程参数；与 cs_pg_ai 宏同值，此处供编排/校验）：
%% backscan 窗口 2 天 + 回扫 10000 条 ⇒ 隐含"平均 ≥ 5 秒 1 条事件"的
%% 流量上界假定；不满足时调小 cs_ai_backscan_event_count 或调大保留窗口。
%% （job 寿命上界宏已随 insert_job 下沉 cs_pg_ai。）
-define(DEFAULT_BACKSCAN_WINDOW_SECONDS, 172800).
-define(DEFAULT_BACKSCAN_EVENT_COUNT, 10000).
-define(DEFAULT_SEEN_RETENTION_SECONDS, 604800).
-define(DEFAULT_CONSUMER, <<"ai_job_derive">>).

%% ===================================================================
%% 批量消费
%% ===================================================================

%% @doc 批量消费事件（A.3：xmin-horizon 闸门 + seen 幂等 + 逐事件副作用）。
%%
%% （visitor 消息 → 幂等派生 AI job；幂等键 `uq_csaij_org_inbound`，
%% 与 B 组投递幂等键同源）。数据面在 `cs_pg_ai:consume_batch/5`
%% （非 cs_ai_port 冻结 callback；经 `impl/1` 动态派发，注入面同构）。
%%
%% Params：`consumer`（默认 `ai_job_derive`）/ `seen_retention_seconds` 可选。
-spec consume_batch(integer(), integer(), map()) -> {ok, map()} | {error, term()}.
consume_batch(OrgId, WsId, Params) when is_integer(OrgId), is_integer(WsId), is_map(Params) ->
    Impl = cs_ai_port:impl(Params),
    Consumer = opt_binary(Params, consumer, ?DEFAULT_CONSUMER),
    Apply = fun(Conn, Event) -> apply_event(Impl, Conn, OrgId, WsId, Params, Event) end,
    Impl:consume_batch(OrgId, WsId, Consumer, Apply, consume_opts(Params));
consume_batch(_OrgId, _WsId, _Params) ->
    {error, {invalid_argument, consume_batch}}.

%% @doc 全量 reconcile sweep（A.3 段 2，低频兜底）：遍历所有游标，
%% 按 id 回扫 seen 缺失的事件并补消费。`remaining` 非 0 的游标须告警。
-spec reconcile_sweep() -> {ok, [map()]} | {error, term()}.
reconcile_sweep() ->
    reconcile_sweep(#{}).

-spec reconcile_sweep(map()) -> {ok, [map()]} | {error, term()}.
reconcile_sweep(Params) when is_map(Params) ->
    Impl = cs_ai_port:impl(Params),
    Results = [
        begin
            Apply = fun(Conn, Event) ->
                apply_event(Impl, Conn, Org, Ws, Params, Event)
            end,
            #{
                org_id => Org,
                workspace_id => Ws,
                consumer => Consumer,
                result => Impl:consume_sweep(Org, Ws, Consumer, Apply, consume_opts(Params))
            }
        end
     || {Org, Ws, Consumer} <- Impl:list_consume_cursors()
    ],
    {ok, Results};
reconcile_sweep(_Params) ->
    {error, {invalid_argument, reconcile_sweep}}.

%% @doc 事件副作用（A.3 关键点 2）：只对「访客消息」事件幂等建 AI job；
%% 其余 action 仅记 seen（消费证明），零副作用。幂等键在 DB 层
%% （`uq_csaij_org_inbound`），重复消费/重复建单天然 0 行。
%%
%% 职责边界（铁律 5）：本层只做**事件面过滤**（action/actor_kind/
%% message_id 提取）；消息事实查询与 job INSERT 的 SQL 数据面在
%% `cs_pg_ai:derive_job_tx/5`（经 `impl/1` 动态派发，注入面同构）。
%% 建单**与配置无关**（配置裁决在投递 gate_1）：消费窗口内关停 AI 再开启
%% 不丢 job；`deadline_at` 用工程默认（配置化读取属 P0-3 claim 面）。
-spec apply_event(module(), term(), integer(), integer(), map(), map()) ->
    ok | {error, term()}.
apply_event(Impl, Conn, OrgId, WsId, _Params, #{action := <<"message.appended">>} = Event) ->
    case Event of
        #{actor_kind := <<"visitor">>} ->
            MessageId = maps:get(<<"message_id">>, maps:get(detail, Event, #{}), undefined),
            case is_integer(MessageId) of
                false ->
                    %% 事件缺 message_id（payload_limits 合同外的畸形行）：跳过，
                    %% 不建 job（fail-closed；seen 已记录，sweep 不会重复处理）。
                    ok;
                true ->
                    Impl:derive_job_tx(Conn, OrgId, WsId, Event, MessageId)
            end;
        _ ->
            %% 坐席/系统消息：不需要 AI 应答，仅记 seen。
            ok
    end;
apply_event(_Impl, _Conn, _OrgId, _WsId, _Params, _Event) ->
    %% 非消息事件（session.opened/claimed/...）：仅记 seen。
    ok.

%% @doc 启动校验（A.3 段 3，fail-fast）：seen 保留窗口必须 ≥ backscan
%% 窗口，否则 sweep 可能复查不到已清退的 seen 行（隐性漏检）。
%% 不满足 ⇒ `erlang:error`（拒绝启动，不是 skip）。
-spec validate_consume_config() -> ok.
validate_consume_config() ->
    Window = pos_env(cs_ai_backscan_window_seconds, ?DEFAULT_BACKSCAN_WINDOW_SECONDS),
    Retention = pos_env(cs_ai_seen_retention_seconds, ?DEFAULT_SEEN_RETENTION_SECONDS),
    Backscan = pos_env(cs_ai_backscan_event_count, ?DEFAULT_BACKSCAN_EVENT_COUNT),
    case Window =< Retention andalso Backscan > 0 of
        true ->
            ok;
        false ->
            erlang:error(
                {consume_window_mismatch, #{
                    backscan_window_seconds => Window,
                    seen_retention_seconds => Retention,
                    backscan_event_count => Backscan
                }}
            )
    end.

%% ===================================================================
%% 内部辅助
%% ===================================================================

consume_opts(Params) ->
    Opts0 = #{},
    Opts1 = int_from_env(
        Params,
        seen_retention_seconds,
        cs_ai_seen_retention_seconds,
        ?DEFAULT_SEEN_RETENTION_SECONDS,
        Opts0
    ),
    int_from_env(
        Params,
        backscan_event_count,
        cs_ai_backscan_event_count,
        ?DEFAULT_BACKSCAN_EVENT_COUNT,
        Opts1
    ).

int_from_env(Params, PKey, EKey, Default, Opts) ->
    Value =
        case maps:get(PKey, Params, Default) of
            V when is_integer(V), V > 0 -> V;
            _ -> pos_env(EKey, Default)
        end,
    Opts#{PKey => Value}.

pos_env(Key, Default) ->
    case config_ds:env(Key, Default) of
        V when is_integer(V), V > 0 -> V;
        _ -> Default
    end.

opt_binary(Map, Key, Default) ->
    case maps:get(Key, Map, Default) of
        B when is_binary(B), B =/= <<>> -> B;
        _ -> Default
    end.
