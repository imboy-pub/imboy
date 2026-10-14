%%% @doc 智能客服 Agent 的**运行时用例**：一次「访客说话 → 判定 → 回复/转人工」。
%%%
%%% 分层职责（本模块只做编排，判定全在 domain）：
%%%   * 取事实：会话（`cs_session_app`）、AI 配置 / 会话 AI 状态 / FAQ（`cs_ai_port`）；
%%%   * 调模型：意图判定 + 回复生成（`imboy_llm_registry` 的 Core 扩展点，可注入替换）；
%%%   * 判裁决：`cs_ai_decision:decide/1`（**唯一**语义真源，本模块不重复任何规则）；
%%%   * 落结果：消息经**既有唯一写路径** `cs_session_app:append_conversation_message/2`
%%%     进 enterprise 真源（铁律 5：不建消息副本）；决策经 `cs_ai_port:append_turn/4`
%%%     进 append-only 审计。
%%%
%%% 失败哲学：**访客路径绝不被 AI 拖垮**。模型不可用、上下文取不到、AI 坐席身份
%%% 未授权——每一种都退化为「记录事实 + 转人工」，而不是抛异常或静默不答。
%%% `respond/2` 只在**入参形状**非法时返回 `{error, _}`。
-module(cs_ai_app).

-moduledoc "智能客服 Agent 运行时用例（意图 → FAQ → 生成 → 转人工）。".

-export([respond/2, decide_only/1, reason_of_llm_failure/1]).
%% 注入面与测试直调
-export([default_llm/2, load_history/2]).
%% A 组（v4 02-A）：事件消费编排 + 启动校验
-export([
    consume_batch/3,
    reconcile_sweep/0,
    reconcile_sweep/1,
    validate_consume_config/0,
    apply_event/6
]).
%% B 组（v4 02-B）：投递协议编排（错误语义表 02-B.4 冻结）
-export([deliver/3]).

%% 注入 prompt 的 FAQ 条数上限（"受控证据"规模）。
-define(FAQ_TOP, 3).
%% 每次检索从 PG 取的候选数（融合前的召回池，比注入量大以给 RRF 留腾挪空间）。
-define(FAQ_POOL, 9).

%% A 组默认（工程参数；与 cs_pg_ai 宏同值，此处供编排/校验）：
%% backscan 窗口 2 天 + 回扫 10000 条 ⇒ 隐含"平均 ≥ 5 秒 1 条事件"的
%% 流量上界假定；不满足时调小 cs_ai_backscan_event_count 或调大保留窗口。
%% （job 寿命上界宏已随 insert_job 下沉 cs_pg_ai。）
-define(DEFAULT_BACKSCAN_WINDOW_SECONDS, 172800).
-define(DEFAULT_BACKSCAN_EVENT_COUNT, 10000).
-define(DEFAULT_SEEN_RETENTION_SECONDS, 604800).
-define(DEFAULT_CONSUMER, <<"ai_job_derive">>).

%% ===================================================================
%% 主用例
%% ===================================================================

%% @doc 跑一次 AI 应答。
%%
%% Params：`workspace_id` / `session_id` / `text` 必填；
%% `history` / `embedding` / `llm` / `ai_store` / `store` / `client_msg_id` 可选。
%%
%% 返回 `{ok, Result}`（`Result.action` ∈ `auto_reply | handoff | no_match`）：
%% `#{action, reason, intent, confidence, resolved, faq_ids, faq_score, reply,
%%    delivered, handoff, unresolved_turns, ai_turns, model, latency_ms, session_id}`
-spec respond(integer(), map()) -> {ok, map()} | {error, term()}.
respond(OrgId, Params) when is_integer(OrgId), OrgId > 0, is_map(Params) ->
    case shape(Params) of
        {error, _} = Err -> Err;
        {ok, Ws, SessionId, Text} -> run(OrgId, Ws, SessionId, Text, Params)
    end;
respond(_OrgId, _Params) ->
    {error, {invalid_argument, respond}}.

shape(Params) ->
    Ws = maps:get(workspace_id, Params, undefined),
    Sid = maps:get(session_id, Params, undefined),
    Text = maps:get(text, Params, undefined),
    case {Ws, Sid, Text} of
        {_, _, _} when
            is_integer(Ws) andalso is_integer(Sid) andalso is_binary(Text) andalso Text =/= <<>>
        ->
            {ok, Ws, Sid, Text};
        Other ->
            {error, {invalid_argument, {respond_params, Other}}}
    end.

%% 未配置 / 未启用 ⇒ 不做任何模型调用、不写任何状态，只如实回报 `ai_disabled`。
run(OrgId, Ws, SessionId, Text, Params) ->
    Impl = cs_ai_port:impl(Params),
    case Impl:fetch_config(OrgId) of
        {error, not_found} ->
            {ok, disabled(ai_disabled)};
        {error, Reason} ->
            {error, {ai_config_unavailable, Reason}};
        {ok, Config} ->
            case maps:get(auto_reply_enabled, Config, false) of
                false -> {ok, disabled(ai_disabled)};
                true -> run_enabled(OrgId, Ws, SessionId, Text, Params, Impl, Config)
            end
    end.

disabled(Reason) ->
    #{
        action => no_match,
        reason => Reason,
        intent => other,
        confidence => 0.0,
        resolved => false,
        faq_ids => [],
        faq_score => 0.0,
        reply => <<>>,
        delivered => false,
        handoff => false
    }.

run_enabled(OrgId, Ws, SessionId, Text, Params, Impl, Config) ->
    Started = now_ms(),
    case session_facts(OrgId, Ws, SessionId, Params) of
        {error, Reason} ->
            {error, {session_unavailable, Reason}};
        {ok, Session} ->
            Facts = #{
                contact_id => maps:get(contact_id, Session),
                conversation_id => maps:get(conversation_id, Session),
                status => maps:get(status, Session, undefined)
            },
            History = cs_ai_prompt:trim_history(
                load_history(OrgId, Params#{conversation_id => maps:get(conversation_id, Session)}),
                Params
            ),
            State = session_state(Impl, OrgId, Ws, SessionId),
            {Intent, Confidence, Model, Resolved} = classify(Params, Config, History, Text),
            {FaqIds, FaqDocs, FaqScore} = retrieve(Impl, OrgId, Params, Text),
            Decision = cs_ai_decision:decide(#{
                intent => Intent,
                confidence => Confidence,
                resolved => Resolved,
                faq_score => FaqScore,
                unresolved_turns => maps:get(unresolved_turns, State, 0),
                explicit_human => cs_ai_intent:contains_any(
                    Text, cs_ai_intent:human_request_keywords()
                ),
                negative => cs_ai_intent:contains_any(Text, cs_ai_intent:negative_keywords()),
                config => decision_config(Config)
            }),
            settle(#{
                org_id => OrgId,
                workspace_id => Ws,
                session_id => SessionId,
                text => Text,
                params => Params,
                impl => Impl,
                config => Config,
                facts => Facts,
                state => State,
                decision => Decision,
                intent => Intent,
                confidence => Confidence,
                resolved => Resolved,
                faq_ids => FaqIds,
                faq_docs => FaqDocs,
                faq_score => FaqScore,
                history => History,
                model => Model,
                started => Started
            })
    end.

%% 会话事实：**只经既有读路径**（本 feature 的 application 用例，同单元）。
session_facts(OrgId, Ws, SessionId, Params) ->
    Base = #{workspace_id => Ws, session_id => SessionId},
    cs_session_app:fetch_session(OrgId, maps:merge(Base, passthrough(Params, [store]))).

session_state(Impl, OrgId, Ws, SessionId) ->
    case Impl:fetch_session_state(OrgId, Ws, SessionId) of
        {ok, S} -> S;
        {error, _} -> #{unresolved_turns => 0, ai_turns => 0, handoff => false}
    end.

%% ===================================================================
%% 落地：回复 / 转人工 + 审计 + 状态
%% ===================================================================

settle(Ctx) ->
    Decision = maps:get(decision, Ctx),
    Handoff = cs_ai_decision:is_handoff(Decision),
    IsAuto = maps:get(action, Decision) =:= auto_reply,
    {Reply, Delivered} = maybe_reply(Ctx, IsAuto),
    PrevUnresolved = maps:get(unresolved_turns, maps:get(state, Ctx), 0),
    NextUnresolved = next_unresolved(Handoff, maps:get(resolved, Ctx), PrevUnresolved),
    StateOut = upsert_state(Ctx, Handoff, NextUnresolved),
    _ = append_turn(Ctx, Reply, Delivered, Handoff),
    ok = bump_hits(Ctx),
    {ok, #{
        action => maps:get(action, Decision),
        reason => maps:get(reason, Decision),
        intent => maps:get(intent, Ctx),
        confidence => maps:get(confidence, Ctx),
        resolved => maps:get(resolved, Ctx),
        faq_ids => maps:get(faq_ids, Ctx),
        faq_score => maps:get(faq_score, Ctx),
        reply => Reply,
        delivered => Delivered,
        handoff => Handoff,
        unresolved_turns => NextUnresolved,
        ai_turns => maps:get(ai_turns, StateOut, 0),
        model => maps:get(model, Ctx),
        latency_ms => now_ms() - maps:get(started, Ctx),
        session_id => maps:get(session_id, Ctx)
    }}.

%% 未解决轮数：转人工或本轮已解决 ⇒ 归零；否则 +1。
%% 归零很重要——人接手/问题解决后，AI 的历史"没答上来"不该继续顶客户。
next_unresolved(true, _Resolved, _Prev) -> 0;
next_unresolved(false, true, _Prev) -> 0;
next_unresolved(false, false, Prev) -> Prev + 1.

upsert_state(Ctx, Handoff, NextUnresolved) ->
    Impl = maps:get(impl, Ctx),
    Facts = maps:get(facts, Ctx),
    Decision = maps:get(decision, Ctx),
    case
        Impl:upsert_session_state(
            maps:get(org_id, Ctx), maps:get(workspace_id, Ctx), maps:get(session_id, Ctx), #{
                contact_id => maps:get(contact_id, Facts),
                conversation_id => maps:get(conversation_id, Facts),
                current_intent => atom_to_binary(maps:get(intent, Ctx), utf8),
                intent_confidence => maps:get(confidence, Ctx),
                ai_turns_delta => 1,
                unresolved_turns => NextUnresolved,
                handoff => Handoff,
                handoff_reason => maps:get(reason, Decision)
            }
        )
    of
        {ok, S} -> S;
        {error, _} -> #{}
    end.

append_turn(Ctx, Reply, Delivered, Handoff) ->
    Impl = maps:get(impl, Ctx),
    Decision = maps:get(decision, Ctx),
    Detail = #{
        <<"resolved">> => maps:get(resolved, Ctx),
        <<"faq_score">> => maps:get(faq_score, Ctx),
        <<"delivered">> => Delivered,
        <<"handoff">> => Handoff,
        <<"history_turns">> => length(maps:get(history, Ctx)),
        <<"model">> => maps:get(model, Ctx)
    },
    Impl:append_turn(
        maps:get(org_id, Ctx), maps:get(workspace_id, Ctx), maps:get(session_id, Ctx), #{
            direction => <<"inbound">>,
            intent => atom_to_binary(maps:get(intent, Ctx), utf8),
            confidence => maps:get(confidence, Ctx),
            action => atom_to_binary(maps:get(action, Decision), utf8),
            faq_ids => maps:get(faq_ids, Ctx),
            reply_text => Reply,
            model => maps:get(model, Ctx),
            latency_ms => now_ms() - maps:get(started, Ctx),
            detail => Detail
        }
    ).

bump_hits(Ctx) ->
    case maps:get(faq_ids, Ctx) of
        [] ->
            ok;
        Ids ->
            case (maps:get(impl, Ctx)):bump_faq_hits(maps:get(org_id, Ctx), Ids) of
                ok -> ok;
                {error, _} -> ok
            end
    end.

%% ===================================================================
%% 回复生成与投递
%% ===================================================================

maybe_reply(_Ctx, false) ->
    {<<>>, false};
maybe_reply(Ctx, true) ->
    Config = maps:get(config, Ctx),
    Messages = cs_ai_prompt:build_reply_messages(
        maps:get(system_prompt, Config, <<>>),
        maps:get(history, Ctx),
        maps:get(text, Ctx),
        maps:get(faq_docs, Ctx)
    ),
    case llm(Ctx, Messages) of
        {ok, Reply} when is_binary(Reply), Reply =/= <<>> ->
            {Reply, deliver(Ctx, Reply)};
        _ ->
            {<<>>, false}
    end.

%% AI 回复经**既有唯一写路径**落库：与坐席发言同一条 `append_conversation_message`。
%% 前提是会话状态为 active 且经办人就是配置里的 AI 坐席身份——不满足即 false，
%% 由调用方把本轮记为「已判定但仍需人工」（fail-safe，不伪造已送达）。
deliver(Ctx, Reply) ->
    Config = maps:get(config, Ctx),
    Params = maps:get(params, Ctx),
    Facts = maps:get(facts, Ctx),
    case maps:get(seat_identity_id, Config, undefined) of
        Identity when is_integer(Identity) ->
            Args = #{
                workspace_id => maps:get(workspace_id, Ctx),
                conversation_id => maps:get(conversation_id, Facts),
                sender_type => <<"business_identity">>,
                identity_id => Identity,
                client_msg_id => msg_id(Params, Ctx),
                body => Reply
            },
            case cs_session_app:append_conversation_message(maps:get(org_id, Ctx), Args) of
                {ok, _} -> true;
                {error, _} -> false
            end;
        _ ->
            false
    end.

msg_id(Params, Ctx) ->
    case maps:get(client_msg_id, Params, undefined) of
        Id when is_binary(Id), Id =/= <<>> ->
            Id;
        _ ->
            iolist_to_binary([
                <<"ai-cs-">>,
                integer_to_binary(maps:get(session_id, Ctx)),
                <<"-">>,
                integer_to_binary(erlang:unique_integer([positive]))
            ])
    end.

%% ===================================================================
%% 意图判定（关键词 + LLM 融合）
%% ===================================================================

classify(Params, Config, History, Text) ->
    Keyword = cs_ai_intent:classify_keywords(Text),
    Messages = cs_ai_prompt:build_classify_messages(
        maps:get(system_prompt, Config, <<>>), History, Text
    ),
    case invoke(Params, Config, Messages) of
        {ok, Raw} ->
            Llm = cs_ai_intent:parse_llm(Raw),
            {Intent, Confidence} = cs_ai_intent:merge(Keyword, Llm),
            {Intent, Confidence, model_of(Config), resolved_of(Llm)};
        {error, _} ->
            %% 模型不可用：关键词层照常工作（这就是它的存在理由），但
            %% `resolved` 恒 false ⇒ 无 FAQ 依据时走 no_match 转人工（不硬猜）。
            {KwIntent, KwConf} = cs_ai_intent:merge(Keyword, {error, llm_unavailable}),
            {KwIntent, KwConf, <<"keyword-only">>, false}
    end.

resolved_of({ok, #{resolved := R}}) -> R;
resolved_of(_) -> false.

model_of(Config) -> maps:get(model, Config, <<>>).

%% @doc LLM 失败的公开分类（写进审计 detail，便于后台区分配置问题与网络问题）。
-spec reason_of_llm_failure(term()) -> binary().
reason_of_llm_failure({provider_not_found, _}) -> <<"provider_not_found">>;
reason_of_llm_failure({unexpected_llm_result, _}) -> <<"bad_response">>;
reason_of_llm_failure(timeout) -> <<"timeout">>;
reason_of_llm_failure(_) -> <<"llm_error">>.

%% ===================================================================
%% 检索
%% ===================================================================

%% 返回 `{Ids, Docs, TopScore}`：TopScore 是**首条融合分**（0..1），
%% 它是 `faq_min_score` 阈值的唯一输入 —— 必须是真实分数，不能是"有没有命中"。
retrieve(Impl, OrgId, Params, Text) ->
    Channels0 = #{lexical => channel_lexical(Impl, OrgId, Text)},
    Channels =
        case embedding(Params, Text) of
            {ok, Vector} -> Channels0#{vector => channel_vector(Impl, OrgId, Vector)};
            none -> Channels0
        end,
    Ranked = cs_ai_retrieval:top(cs_ai_retrieval:fuse(Channels, #{}), ?FAQ_TOP),
    case Ranked of
        [] ->
            {[], [], 0.0};
        _ ->
            Ids = [Id || {Id, _} <- Ranked],
            Top = element(2, hd(Ranked)),
            Docs =
                case Impl:fetch_faq(OrgId, Ids) of
                    {ok, Rows} -> order_docs(Rows, Ids);
                    {error, _} -> []
                end,
            {Ids, Docs, Top}
    end.

channel_lexical(Impl, OrgId, Text) ->
    case Impl:search_faq_lexical(OrgId, Text, ?FAQ_POOL) of
        {ok, Pairs} -> Pairs;
        {error, _} -> []
    end.

channel_vector(Impl, OrgId, Vector) ->
    case Impl:search_faq_vector(OrgId, Vector, ?FAQ_POOL) of
        {ok, Pairs} -> Pairs;
        {error, _} -> []
    end.

%% 按检索名次重排正文（`fetch_faq` 按 id 升序返回，名次信息在融合结果里）。
order_docs(Rows, Ids) ->
    ById = maps:from_list([{maps:get(id, R), R} || R <- Rows]),
    [maps:get(Id, ById) || Id <- Ids, maps:is_key(Id, ById)].

%% 语义通道：未配置 embedding provider 时**不调用**向量检索（不造假数据）。
embedding(Params, Text) ->
    case maps:get(embedding, Params, undefined) of
        Fun when is_function(Fun, 1) ->
            case Fun(Text) of
                {ok, V} when is_list(V), V =/= [] -> {ok, V};
                _ -> none
            end;
        _ ->
            none
    end.

%% ===================================================================
%% LLM 调用（可注入）
%% ===================================================================

invoke(Ctx, Messages) when is_map(Ctx) ->
    invoke(maps:get(params, Ctx), maps:get(config, Ctx), Messages).
invoke(Params, Config, Messages) ->
    case maps:get(llm, Params, undefined) of
        Fun when is_function(Fun, 2) -> Fun(Messages, Config);
        _ -> default_llm(Messages, Config)
    end.

llm(Ctx, Messages) -> invoke(Ctx, Messages).

%% @doc 默认 LLM 调用：经 Core 的 `imboy_llm_registry` 取 provider 实现。
%% provider/model 来自组织级 AI 配置（数据行，不是代码常量）。
-spec default_llm([map()], map()) -> {ok, binary()} | {error, term()}.
default_llm(Messages, Config) ->
    Provider = maps:get(provider, Config, <<"bigmodel">>),
    case imboy_llm_registry:lookup(Provider) of
        {ok, #{module := Mod, opts := Opts0}} ->
            Opts = merge_model(Opts0, maps:get(model, Config, <<>>)),
            case Mod:chat(0, Messages, Opts) of
                {ok, Resp} when is_map(Resp) -> {ok, maps:get(<<"result">>, Resp, <<>>)};
                {ok, Other} -> {error, {unexpected_llm_result, Other}};
                {error, Reason} -> {error, Reason}
            end;
        undefined ->
            {error, {provider_not_found, Provider}}
    end.

merge_model(Opts, M) when is_binary(M), M =/= <<>> -> Opts#{model => M};
merge_model(Opts, _) -> Opts.

%% ===================================================================
%% A 组（v4 02-A）：事件消费编排
%% ===================================================================

%% @doc 增量消费一批事件（A.3 段 1）。副作用 = `apply_event/5`
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

%% ===================================================================
%% B 组（v4 02-B）：投递协议编排
%% ===================================================================

%% @doc 投递一条已生成的 AI 回复（02-B.4 伪代码的编排面）。
%%
%% 事务结构：canonical 唯一写路径承载整个投递事务——L1 由方案甲的
%% `fetch_conversation FOR NO KEY UPDATE`（conversation 行锁，cs 会话↔
%% conversation 一对一）取得；deadline / input_version 复核、L4 delivery
%% 幂等 INSERT、job → completed CAS 全部在 `ai_delivery_hook` 内同事务
%% 执行（`cs_session_app:compose_ai_hook/2` 组合），任一失败 ⇒ 消息与
%% delivery 原子回滚。
%%
%% 返回值语义（02-B.4 冻结，调用方**不得**对 `{skipped, _}` 重试）：
%% `{ok, delivered}` | `{skipped, expired | superseded}` |
%% `{error, lease_lost | conflict | term()}`。
%%
%% Params：`job_id` / `worker_id` / `reply` 必填；`seat_identity_id`
%% 可选（缺省从 agent 配置取）；`gate_fun`（P0-5 D 组注入位）可选。
-spec deliver(integer(), integer(), map()) ->
    {ok, delivered} | {skipped, expired | superseded} | {error, term()}.
deliver(OrgId, WsId, #{job_id := JobId, worker_id := WorkerId} = Params) when
    is_integer(OrgId), is_integer(WsId), is_binary(WorkerId)
->
    Impl = cs_ai_port:impl(Params),
    case Impl:fetch_job(OrgId, JobId) of
        {ok, #{status := running, lease_owner := WorkerId, version := V} = Job} ->
            case deliver_preflight(Impl, OrgId, Job, Params) of
                {skipped, _} = Skip ->
                    Skip;
                {ok, SeatIdentity, ActorUser} ->
                    deliver_message(
                        OrgId, WsId, Impl, Job, WorkerId, V, SeatIdentity, ActorUser, Params
                    )
            end;
        {ok, #{status := running}} ->
            %% running 但 owner/version 失配 = 陈旧租约（I4：无任何副作用）。
            {error, lease_lost};
        {ok, #{status := Status}} ->
            {error, {conflict, Status}};
        {ok, _} ->
            {error, lease_lost};
        {error, Reason} ->
            {error, Reason}
    end;
deliver(_OrgId, _WsId, _Params) ->
    {error, {invalid_argument, deliver}}.

%% 快速预检：deadline 过 ⇒ 终态 + {skipped, expired}；坐席身份缺失 ⇒ 拒。
deliver_preflight(Impl, OrgId, Job, Params) ->
    case deadline_passed(Job) of
        true ->
            _ = Impl:mark_job_skipped(OrgId, maps:get(id, Job), expired),
            {skipped, expired};
        false ->
            case maps:get(seat_identity_id, Params, undefined) of
                Seat when is_integer(Seat) ->
                    seat_actor(Impl, OrgId, Seat, Params);
                _ ->
                    case seat_identity_from_config(Impl, OrgId) of
                        {ok, Seat} -> seat_actor(Impl, OrgId, Seat, Params);
                        {error, _} = Err -> Err
                    end
            end
    end.

%% actor 解析：Params.actor_user_id 显式注入优先（测试面），否则取
%% seat 身份归属用户（canonical 出站 actor 非空不变量；见
%% cs_pg_ai:identity_actor/2 注释——C 组「actor NULL」字面与既有
%% eb_message domain 不变量冲突的实现性偏差，P0-4 state 详述）。
seat_actor(Impl, OrgId, Seat, Params) ->
    case maps:get(actor_user_id, Params, undefined) of
        Actor when is_integer(Actor) ->
            {ok, Seat, Actor};
        _ ->
            case Impl:identity_actor(OrgId, Seat) of
                {ok, Actor} -> {ok, Seat, Actor};
                {error, Reason} -> {error, {ai_seat_actor_unavailable, Reason}}
            end
    end.

deadline_passed(#{deadline_at := Dl}) when is_integer(Dl), Dl > 0 ->
    os:system_time(second) >= Dl;
deadline_passed(_) ->
    true.

seat_identity_from_config(Impl, OrgId) ->
    case Impl:fetch_config(OrgId) of
        {ok, #{seat_identity_id := Seat}} when is_integer(Seat) -> {ok, Seat};
        {ok, _} -> {error, ai_seat_unconfigured};
        {error, Reason} -> {error, {ai_config_unavailable, Reason}}
    end.

deliver_message(
    OrgId, WsId, Impl, Job, WorkerId, ExpectedVersion, SeatIdentity, ActorUser, Params
) ->
    JobId = maps:get(id, Job),
    Hook = Impl:delivery_hook(OrgId, JobId, WorkerId, ExpectedVersion, #{
        seat_identity_id => SeatIdentity,
        gate_fun => maps:get(gate_fun, Params, undefined)
    }),
    MsgParams = #{
        workspace_id => WsId,
        conversation_id => maps:get(conversation_id, Job),
        sender_type => <<"business_identity">>,
        identity_id => SeatIdentity,
        actor_user_id => ActorUser,
        client_msg_id => reply_key(JobId),
        body => maps:get(reply, Params, <<>>),
        ai_delivery_hook => Hook
    },
    case cs_session_app:append_conversation_message(OrgId, MsgParams) of
        {ok, _} ->
            {ok, delivered};
        {error, {skipped, Reason}} when Reason =:= expired; Reason =:= superseded ->
            %% 钩子事务内复核失败已回滚消息；此处落 job 终态
            %% （dead/cancelled + last_error_code 细分，02-B.4 返回值冻结形状）。
            _ = Impl:mark_job_skipped(OrgId, JobId, Reason),
            {skipped, Reason};
        {error, {skipped, Other}} ->
            {error, Other};
        {error, Reason} ->
            {error, Reason}
    end.

%% 投递幂等键（02-B.4 `reply_key(j)`）：跨重试稳定，一条逻辑 job 一条
%% 逻辑回复（与 uq_csaid_reply 同键集）。
reply_key(JobId) ->
    <<"ai-job-", (integer_to_binary(JobId))/binary>>.

%% ===================================================================
%% 历史（多轮上下文）
%% ===================================================================

%% @doc 加载会话历史（时间升序的 `#{role, content}`）。
%%
%% 注入面：`Params` 带 `history` 时直接采用（测试与调用方自备上下文）。
%% 缺省返回 `[]`（单轮）——**显式降级**，且 `append_turn` 的 detail 里带
%% `history_turns`，后台能看出这一轮是不是裸单轮，不做"假装有上下文"。
-spec load_history(integer(), map()) -> [map()].
load_history(_OrgId, Params) ->
    case maps:get(history, Params, undefined) of
        H when is_list(H) -> H;
        _ -> []
    end.

%% ===================================================================
%% 供后台"试算"规则（纯函数，不查库不调模型）
%% ===================================================================

%% @doc 把裁决规则直接暴露给管理面：给定事实 → 会怎么判。
-spec decide_only(map()) -> map().
decide_only(Facts) -> cs_ai_decision:decide(Facts).

%% ===================================================================
%% 内部辅助
%% ===================================================================

decision_config(Config) ->
    #{
        threshold => maps:get(auto_reply_threshold, Config, 0.6),
        max_unresolved_turns => maps:get(max_unresolved_turns, Config, 2),
        faq_min_score => maps:get(faq_min_score, Config, 0.2)
    }.

passthrough(Params, Keys) ->
    maps:filter(fun(K, V) -> lists:member(K, Keys) andalso V =/= undefined end, Params).

now_ms() -> erlang:system_time(millisecond).
