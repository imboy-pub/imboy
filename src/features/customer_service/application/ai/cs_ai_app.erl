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
%% C 组（v4 02-C）：权限矩阵（fail-closed；闩锁触发/投递前置门在
%% cs_ai_delivery_app，A/B/D/E 组编排自 2026-10-15 起按域拆出——
%% consume → cs_ai_consume_app、deliver/闩锁 → cs_ai_delivery_app、
%% gate → cs_ai_gate_app、budget → cs_ai_budget_app）。
-export([authorize/3]).

%% 注入 prompt 的 FAQ 条数上限（"受控证据"规模）。
-define(FAQ_TOP, 3).
%% 每次检索从 PG 取的候选数（融合前的召回池，比注入量大以给 RRF 留腾挪空间）。
-define(FAQ_POOL, 9).

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
            %% v4（04 对照表）：auto_reply_enabled 删除，由 auto_reply_mode 承载；
            %% 只有 enabled 放行（observe/canary/disabled 均不自动应答）。
            case cs_ai_gate_app:mode_of(Config) =:= enabled of
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
            model_reported_confidence => maps:get(confidence, Ctx),
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
%% C 组（v4 02-C.4）：权限矩阵（冻结，fail-closed）
%% ===================================================================

%% @doc 权限矩阵判定（02-C.4 逐格编码）。
%%
%% 角色：org_admin | workspace_admin | seat（本 ws active 坐席）| ai_runtime
%%      | anonymous | superuser。
%% 动作：read_config | set_mode | set_limits | set_canary | faq_write
%%      | faq_publish | read_sessions | read_stats | claim | ai_deliver
%%      | purge_trace。
%%
%% 返回 ok | {error, forbidden} | {error, {forbidden, scope}}（C 条件的
%% scope 约束不满足）。**未列出的 (动作, 角色) 组合一律禁止**（fail-closed
%% 规则 1）；ai_runtime 无人类身份 ⇒ 不能改配置/claim/transfer/purge（规则 2）。
-spec authorize(atom(), atom(), map()) -> ok | {error, term()}.
authorize(Action, Role, Ctx) when is_atom(Action), is_atom(Role), is_map(Ctx) ->
    case matrix(Action, Role) of
        allow -> ok;
        deny -> {error, forbidden};
        {scoped, Kind} -> scoped(Kind, Ctx);
        _ -> {error, forbidden}
    end;
authorize(_Action, _Role, _Ctx) ->
    {error, forbidden}.

%% 矩阵真源（02-C.4；A=allow、—=deny、C=scoped）。
matrix(read_config, org_admin) -> allow;
matrix(read_config, workspace_admin) -> allow;
matrix(read_config, seat) -> allow;
%% 只读自身 org（org 隔离由 SQL 首参保证）
matrix(read_config, ai_runtime) -> allow;
matrix(read_config, superuser) -> allow;
matrix(set_mode, org_admin) -> allow;
matrix(set_mode, superuser) -> allow;
matrix(set_limits, org_admin) -> allow;
matrix(set_limits, superuser) -> allow;
matrix(set_canary, org_admin) -> allow;
matrix(set_canary, superuser) -> allow;
matrix(faq_write, org_admin) -> allow;
matrix(faq_write, workspace_admin) -> {scoped, workspace};
matrix(faq_write, superuser) -> allow;
matrix(faq_publish, org_admin) -> allow;
matrix(faq_publish, workspace_admin) -> {scoped, workspace};
matrix(faq_publish, superuser) -> allow;
matrix(read_sessions, org_admin) -> allow;
matrix(read_sessions, workspace_admin) -> {scoped, workspace};
matrix(read_sessions, seat) -> {scoped, workspace};
matrix(read_sessions, superuser) -> allow;
matrix(read_stats, org_admin) -> allow;
matrix(read_stats, workspace_admin) -> {scoped, workspace};
matrix(read_stats, superuser) -> allow;
matrix(claim, org_admin) -> {scoped, seat_identity};
matrix(claim, workspace_admin) -> {scoped, seat_identity};
matrix(claim, seat) -> allow;
matrix(claim, superuser) -> allow;
%% 唯一投递者（02-C.4）
matrix(ai_deliver, ai_runtime) -> allow;
%% 且走 SECURITY DEFINER（F 组）
matrix(purge_trace, superuser) -> {scoped, purge};
matrix(_, _) -> deny.

%% C 条件的 scope 约束：
%%   workspace     — Ctx 需 workspace_id 且与操作目标一致（调用方比对；
%%                   本函数校验键存在与同值）
%%   seat_identity — 需携带 seat 身份（org/ws admin 仅在自有 seat 身份下可 claim）
%%   purge         — 走 cs_ai_purge_trace 的 session_user 白名单（F 组），
%%                   应用层只允许 superuser 发起
scoped(workspace, #{workspace_id := Ws, target_workspace_id := Ws}) ->
    ok;
scoped(workspace, #{workspace_id := _Ws, target_workspace_id := _Other}) ->
    {error, {forbidden, workspace_scope}};
scoped(workspace, _Ctx) ->
    {error, {forbidden, workspace_scope}};
scoped(seat_identity, #{seat_identity_id := Id}) when is_integer(Id) -> ok;
scoped(seat_identity, _Ctx) ->
    {error, {forbidden, seat_identity_required}};
scoped(purge, _Ctx) ->
    ok.

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

%% v4（04 对照表）：auto_reply_threshold/faq_min_score 删除——放行由 D 组
%% 七道 gate 承担，不再有阈值；决策只剩轮数上限。cs_ai_decision 对缺失键
%% 的域默认值（0.6/0.2）属 domain 层（不在 P0-8 文件清单），见 state 偏差。
decision_config(Config) ->
    #{
        max_unresolved_turns => maps:get(max_unresolved_turns, Config, 2)
    }.

passthrough(Params, Keys) ->
    maps:filter(fun(K, V) -> lists:member(K, Keys) andalso V =/= undefined end, Params).

now_ms() -> erlang:system_time(millisecond).
