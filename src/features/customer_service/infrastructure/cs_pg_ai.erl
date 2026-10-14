%%% @doc `cs_ai_port` 的 PG 实现（铁律 6：首参恒为 `OrganizationId`，SQL 同语句带
%%% `organization_id`；铁律 7：单调状态推进在 SQL 内完成，不做检查-使用序列）。
%%%
%%% 设计要点（都是踩过的坑）：
%%%
%%%   * **中文全文检索必须自己组 tsquery**：`plainto_tsquery('jiebacfg', '我想退款')`
%%%     是 AND 语义，而 FAQ 向量里只有「退款」这一个词元 ⇒ 恒不命中。故用
%%%     `tsvector_to_array(to_tsvector(...))` 先在 PG 侧分词，再用 `' | '` 组 OR。
%%%     空查询用 `CASE WHEN tsq='' THEN NULL` 兜底（`to_tsquery('')` 会抛错）。
%%%   * **`seq` 在 SQL 内生成**：`coalesce(max(seq),0)+1` 与 INSERT 同语句，避免
%%%     "先查后写"的竞态（多轮并发时会产生重复 seq）。
%%%   * **时间一律 `extract(epoch ...)::bigint` 出站**：避免 timestamptz 的文本
%%%     codec 在无时区串上退化为 `<<0:64>>`（历史事故，见项目记忆）。
%%%   * **`handoff` 粘性**：`upsert_session_state` 里用
%%%     `session.handoff OR EXCLUDED.handoff`，一旦转人工不得被后续轮次改回。
-module(cs_pg_ai).

-moduledoc "cs_ai_port 的 PG 实现（org 作用域 + 单调状态 + SQL 内 seq 生成）。".
-behaviour(cs_ai_port).

-export([
    fetch_config/1,
    upsert_config/2,
    list_faq/2,
    upsert_faq/2,
    delete_faq/2,
    search_faq_lexical/3,
    search_faq_vector/3,
    fetch_faq/2,
    bump_faq_hits/2,
    fetch_session_state/3,
    upsert_session_state/4,
    append_turn/4,
    list_turns/2,
    list_sessions/2,
    stats/2,
    %% A 组（v4 02-A）：事件消费三件套——数据面。非 cs_ai_port 冻结 callback
    %% （contracts() 面不动），cs_ai_app 经 impl/1 动态调用；测试注入面同构。
    consume_batch/5,
    consume_sweep/5,
    fetch_consume_cursor/3,
    list_consume_cursors/0,
    derive_job_tx/5,
    %% 冻结语句静态断言面（cs_pg_seat:sql_statements/0 先例；零 DB）。
    sql_statements/0
]).

-define(CONFIG_KEYS, [
    id,
    name,
    provider,
    model,
    system_prompt,
    greeting,
    auto_reply_enabled,
    auto_reply_threshold,
    max_unresolved_turns,
    faq_min_score,
    seat_identity_id,
    version,
    created_at,
    updated_at
]).

%% A 组默认窗口（工程参数，非业务阈值——G4 只约束 token/cost 数值）：
%% seen 保留 7 天（02-A.3 段 1 冻结值）；sweep 回扫 10000 条（段 2 冻结值）。
%% 两者经 Opts 可覆盖；启动校验见 cs_ai_app:validate_consume_config/0。
-define(SEEN_RETENTION_SECONDS, 604800).
-define(BACKSCAN_EVENT_COUNT, 10000).
%% job 寿命上界 5 分钟（迁移 180 的 max_job_age_ms 默认 300000 同口径；
%% claim 侧的配置化读取属 P0-3）。
-define(JOB_MAX_AGE_SECONDS, 300).

-define(FAQ_KEYS, [
    id,
    question,
    answer,
    intent,
    category,
    keywords,
    enabled,
    hit_count,
    version,
    created_at,
    updated_at
]).

-define(STATE_KEYS, [
    session_id,
    contact_id,
    conversation_id,
    current_intent,
    intent_confidence,
    ai_turns,
    unresolved_turns,
    handoff,
    handoff_reason,
    version,
    updated_at
]).

-define(TURN_KEYS, [
    seq,
    direction,
    intent,
    confidence,
    action,
    faq_ids,
    reply_text,
    model,
    latency_ms,
    detail,
    created_at
]).

%% ===================================================================
%% 配置
%% ===================================================================

fetch_config(OrgId) ->
    Sql = <<
        "SELECT id,name,provider,model,system_prompt,greeting,auto_reply_enabled,"
        "auto_reply_threshold,max_unresolved_turns,faq_min_score,seat_identity_id,version,"
        "extract(epoch from created_at)::bigint AS created_at,"
        "extract(epoch from updated_at)::bigint AS updated_at "
        "FROM customer_service_ai_agent WHERE organization_id=$1 ORDER BY id LIMIT 1"
    >>,
    cs_pg_common:fetch_one(Sql, [OrgId], ?CONFIG_KEYS).

upsert_config(OrgId, Params) ->
    Sql = <<
        "INSERT INTO customer_service_ai_agent "
        "(organization_id,id,name,provider,model,system_prompt,greeting,auto_reply_enabled,"
        "auto_reply_threshold,max_unresolved_turns,faq_min_score,seat_identity_id) "
        "VALUES ($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12) "
        "ON CONFLICT (organization_id,id) DO UPDATE SET "
        "name=EXCLUDED.name,provider=EXCLUDED.provider,model=EXCLUDED.model,"
        "system_prompt=EXCLUDED.system_prompt,greeting=EXCLUDED.greeting,"
        "auto_reply_enabled=EXCLUDED.auto_reply_enabled,"
        "auto_reply_threshold=EXCLUDED.auto_reply_threshold,"
        "max_unresolved_turns=EXCLUDED.max_unresolved_turns,"
        "faq_min_score=EXCLUDED.faq_min_score,seat_identity_id=EXCLUDED.seat_identity_id,"
        "updated_at=now(),version=customer_service_ai_agent.version+1 "
        "RETURNING id,name,provider,model,system_prompt,greeting,auto_reply_enabled,"
        "auto_reply_threshold,max_unresolved_turns,faq_min_score,seat_identity_id,version,"
        "extract(epoch from created_at)::bigint AS created_at,"
        "extract(epoch from updated_at)::bigint AS updated_at"
    >>,
    Args = [
        OrgId,
        maps:get(id, Params),
        opt(Params, name, <<"智能客服">>),
        opt(Params, provider, <<"bigmodel">>),
        opt(Params, model, <<>>),
        opt(Params, system_prompt, <<>>),
        opt(Params, greeting, <<>>),
        maps:get(auto_reply_enabled, Params, false),
        maps:get(auto_reply_threshold, Params, 0.6),
        maps:get(max_unresolved_turns, Params, 2),
        maps:get(faq_min_score, Params, 0.2),
        maps:get(seat_identity_id, Params, undefined)
    ],
    cs_pg_common:fetch_one(Sql, Args, ?CONFIG_KEYS).

%% ===================================================================
%% FAQ
%% ===================================================================

list_faq(OrgId, Params) ->
    Sql = <<
        "SELECT id,question,answer,intent,category,keywords,enabled,hit_count,version,"
        "(embedding IS NOT NULL) AS embedded,"
        "extract(epoch from created_at)::bigint AS created_at,"
        "extract(epoch from updated_at)::bigint AS updated_at "
        "FROM customer_service_faq WHERE organization_id=$1 "
        "AND ($2::text = '' OR intent = $2) "
        "ORDER BY id DESC LIMIT $3 OFFSET $4"
    >>,
    Args = [
        OrgId,
        opt(Params, intent, <<>>),
        pos(maps:get(limit, Params, 100), 100),
        max(0, int(maps:get(offset, Params, 0), 0))
    ],
    cs_pg_common:fetch_many(Sql, Args, ?FAQ_KEYS ++ [embedded]).

upsert_faq(OrgId, Params) ->
    Sql = <<
        "INSERT INTO customer_service_faq "
        "(organization_id,id,question,answer,intent,category,keywords,enabled) "
        "VALUES ($1,$2,$3,$4,$5,$6,$7::text[],$8) "
        "ON CONFLICT (organization_id,id) DO UPDATE SET "
        "question=EXCLUDED.question,answer=EXCLUDED.answer,intent=EXCLUDED.intent,"
        "category=EXCLUDED.category,keywords=EXCLUDED.keywords,enabled=EXCLUDED.enabled,"
        "updated_at=now(),version=customer_service_faq.version+1 "
        "RETURNING id,question,answer,intent,category,keywords,enabled,hit_count,version,"
        "(embedding IS NOT NULL) AS embedded,"
        "extract(epoch from created_at)::bigint AS created_at,"
        "extract(epoch from updated_at)::bigint AS updated_at"
    >>,
    Args = [
        OrgId,
        maps:get(id, Params),
        maps:get(question, Params),
        opt(Params, answer, <<>>),
        opt(Params, intent, <<"other">>),
        opt(Params, category, <<>>),
        maps:get(keywords, Params, []),
        maps:get(enabled, Params, true)
    ],
    cs_pg_common:fetch_one(Sql, Args, ?FAQ_KEYS).

delete_faq(OrgId, FaqId) when is_integer(FaqId) ->
    Sql = <<"DELETE FROM customer_service_faq WHERE organization_id=$1 AND id=$2 RETURNING id">>,
    case cs_pg_common:fetch_many(Sql, [OrgId, FaqId], [id]) of
        {ok, [_ | _]} -> ok;
        {ok, []} -> {error, not_found};
        {error, _} = Err -> Err
    end;
delete_faq(_OrgId, _FaqId) ->
    {error, {invalid_argument, faq_id}}.

%% 词法检索：PG 侧分词 → OR 组 tsquery（见模块头注释）。
search_faq_lexical(OrgId, Query, Limit) when is_binary(Query), Query =/= <<>> ->
    Sql = <<
        "WITH q AS MATERIALIZED ("
        "  SELECT array_to_string(tsvector_to_array(to_tsvector('jiebacfg',$2)), ' | ') AS tsq"
        ") "
        "SELECT f.id, "
        "  ts_rank(f.search_tsv, CASE WHEN q.tsq='' THEN NULL ELSE to_tsquery('jiebacfg',q.tsq) END)::float8 AS score "
        "FROM customer_service_faq f, q "
        "WHERE f.organization_id=$1 AND f.enabled = true "
        "  AND f.search_tsv @@ CASE WHEN q.tsq='' THEN NULL ELSE to_tsquery('jiebacfg',q.tsq) END "
        "ORDER BY score DESC, f.id ASC LIMIT $3"
    >>,
    case cs_pg_common:fetch_many(Sql, [OrgId, Query, pos(Limit, 10)], [id, score]) of
        {ok, Rows} -> {ok, [{maps:get(id, R), maps:get(score, R, 0.0)} || R <- Rows]};
        {error, _} = Err -> Err
    end;
search_faq_lexical(_OrgId, _Query, _Limit) ->
    {ok, []}.

%% 语义检索（pgvector）：embedding 为 NULL 的行天然被 `IS NOT NULL` 排除。
search_faq_vector(OrgId, Vector, Limit) when is_list(Vector), Vector =/= [] ->
    Sql = <<
        "SELECT id, (1 - (embedding <=> $2::text::vector))::float8 AS similarity "
        "FROM customer_service_faq "
        "WHERE organization_id=$1 AND enabled = true AND embedding IS NOT NULL "
        "ORDER BY embedding <=> $2::text::vector LIMIT $3"
    >>,
    case
        cs_pg_common:fetch_many(Sql, [OrgId, vector_literal(Vector), pos(Limit, 10)], [
            id, similarity
        ])
    of
        {ok, Rows} -> {ok, [{maps:get(id, R), maps:get(similarity, R, 0.0)} || R <- Rows]};
        {error, _} = Err -> Err
    end;
search_faq_vector(_OrgId, _Vector, _Limit) ->
    {ok, []}.

fetch_faq(OrgId, Ids) when is_list(Ids), Ids =/= [] ->
    Sql = <<
        "SELECT id,question,answer,intent,category,keywords,enabled,hit_count,version,"
        "extract(epoch from created_at)::bigint AS created_at,"
        "extract(epoch from updated_at)::bigint AS updated_at "
        "FROM customer_service_faq WHERE organization_id=$1 AND id = ANY($2::bigint[]) ORDER BY id"
    >>,
    cs_pg_common:fetch_many(Sql, [OrgId, lists:usort(Ids)], ?FAQ_KEYS);
fetch_faq(_OrgId, _Ids) ->
    {ok, []}.

%% 命中计数：**不动 updated_at**（那是"内容被编辑"的语义，不该被读操作污染）。
bump_faq_hits(OrgId, Ids) when is_list(Ids), Ids =/= [] ->
    Sql = <<
        "UPDATE customer_service_faq SET hit_count = hit_count + 1 "
        "WHERE organization_id=$1 AND id = ANY($2::bigint[]) RETURNING id"
    >>,
    case cs_pg_common:fetch_many(Sql, [OrgId, lists:usort(Ids)], [id]) of
        {ok, _} -> ok;
        {error, _} = Err -> Err
    end;
bump_faq_hits(_OrgId, _Ids) ->
    ok.

%% ===================================================================
%% 会话 AI 状态
%% ===================================================================

fetch_session_state(OrgId, WsId, SessionId) ->
    Sql = <<
        "SELECT session_id,contact_id,conversation_id,current_intent,intent_confidence,"
        "ai_turns,unresolved_turns,handoff,handoff_reason,version,"
        "extract(epoch from updated_at)::bigint AS updated_at "
        "FROM customer_service_ai_session "
        "WHERE organization_id=$1 AND workspace_id=$2 AND session_id=$3"
    >>,
    cs_pg_common:fetch_one(Sql, [OrgId, WsId, SessionId], ?STATE_KEYS).

%% 单调推进：ai_turns 累加、handoff 粘性、handoff_at 只写一次（见模块头）。
upsert_session_state(OrgId, WsId, SessionId, Params) ->
    Sql = <<
        "INSERT INTO customer_service_ai_session "
        "(organization_id,workspace_id,session_id,contact_id,conversation_id,"
        "current_intent,intent_confidence,ai_turns,unresolved_turns,handoff,handoff_reason,handoff_at,last_ai_at) "
        "VALUES ($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,"
        "CASE WHEN $10 THEN now() ELSE NULL END, now()) "
        "ON CONFLICT (organization_id,workspace_id,session_id) DO UPDATE SET "
        "current_intent=EXCLUDED.current_intent,"
        "intent_confidence=EXCLUDED.intent_confidence,"
        "ai_turns=customer_service_ai_session.ai_turns + $8,"
        "unresolved_turns=$9,"
        "handoff=customer_service_ai_session.handoff OR EXCLUDED.handoff,"
        "handoff_reason=CASE WHEN EXCLUDED.handoff THEN EXCLUDED.handoff_reason "
        "                   ELSE customer_service_ai_session.handoff_reason END,"
        "handoff_at=CASE WHEN EXCLUDED.handoff AND customer_service_ai_session.handoff_at IS NULL "
        "               THEN now() ELSE customer_service_ai_session.handoff_at END,"
        "last_ai_at=now(),updated_at=now(),version=customer_service_ai_session.version+1 "
        "RETURNING session_id,contact_id,conversation_id,current_intent,intent_confidence,"
        "ai_turns,unresolved_turns,handoff,handoff_reason,version,"
        "extract(epoch from updated_at)::bigint AS updated_at"
    >>,
    Args = [
        OrgId,
        WsId,
        SessionId,
        maps:get(contact_id, Params),
        maps:get(conversation_id, Params),
        opt(Params, current_intent, <<>>),
        maps:get(intent_confidence, Params, 0.0),
        int(maps:get(ai_turns_delta, Params, 1), 1),
        int(maps:get(unresolved_turns, Params, 0), 0),
        maps:get(handoff, Params, false) =:= true,
        opt(Params, handoff_reason, <<>>)
    ],
    cs_pg_common:fetch_one(Sql, Args, ?STATE_KEYS).

%% ===================================================================
%% 逐轮审计
%% ===================================================================

append_turn(OrgId, WsId, SessionId, Params) ->
    Sql = <<
        "INSERT INTO customer_service_ai_turn "
        "(organization_id,workspace_id,session_id,seq,direction,intent,confidence,action,"
        "faq_ids,reply_text,model,latency_ms,detail) "
        "SELECT $1,$2,$3,coalesce(max(seq),0)+1,$4,$5,$6,$7,$8::bigint[],$9,$10,$11,$12::jsonb "
        "FROM customer_service_ai_turn WHERE organization_id=$1 AND session_id=$3 "
        "RETURNING seq,direction,intent,confidence,action,faq_ids,reply_text,model,latency_ms,detail,"
        "extract(epoch from created_at)::bigint AS created_at"
    >>,
    Args = [
        OrgId,
        WsId,
        SessionId,
        opt(Params, direction, <<"inbound">>),
        opt(Params, intent, <<>>),
        maps:get(confidence, Params, 0.0),
        opt(Params, action, <<>>),
        maps:get(faq_ids, Params, []),
        opt(Params, reply_text, <<>>),
        opt(Params, model, <<>>),
        int(maps:get(latency_ms, Params, 0), 0),
        jsonb(opt(Params, detail, #{}))
    ],
    cs_pg_common:fetch_one(Sql, Args, ?TURN_KEYS).

list_turns(OrgId, #{session_id := SessionId} = Params) ->
    Sql = <<
        "SELECT seq,direction,intent,confidence,action,faq_ids,reply_text,model,latency_ms,detail,"
        "extract(epoch from created_at)::bigint AS created_at "
        "FROM customer_service_ai_turn WHERE organization_id=$1 AND session_id=$2 "
        "ORDER BY seq ASC LIMIT $3"
    >>,
    cs_pg_common:fetch_many(
        Sql, [OrgId, SessionId, pos(maps:get(limit, Params, 200), 200)], ?TURN_KEYS
    );
list_turns(_OrgId, _Params) ->
    {error, {invalid_argument, session_id}}.

%% ===================================================================
%% 管理面
%% ===================================================================

%% 对话记录（会话真源 LEFT JOIN AI 状态）：未触发 AI 的会话也必须出现，
%% 否则后台看到的"对话记录"会缺一批（LEFT JOIN 而非 INNER JOIN 的理由）。
list_sessions(OrgId, Params) ->
    WsFilter = int(maps:get(workspace_id, Params, 0), 0),
    Status = opt(Params, status, <<>>),
    Intent = opt(Params, intent, <<>>),
    Limit = pos(maps:get(limit, Params, 50), 50),
    Offset = max(0, int(maps:get(offset, Params, 0), 0)),
    Sql = <<
        "SELECT s.id,s.workspace_id,s.contact_id,s.status,s.rating,"
        "extract(epoch from s.queued_at)::bigint AS queued_at,"
        "extract(epoch from s.claimed_at)::bigint AS claimed_at,"
        "extract(epoch from s.closed_at)::bigint AS closed_at,"
        "coalesce(a.current_intent,'') AS current_intent,"
        "coalesce(a.intent_confidence,0) AS intent_confidence,"
        "coalesce(a.ai_turns,0) AS ai_turns,"
        "coalesce(a.unresolved_turns,0) AS unresolved_turns,"
        "coalesce(a.handoff,false) AS handoff,"
        "coalesce(a.handoff_reason,'') AS handoff_reason,"
        "(SELECT count(*) FROM customer_service_ai_turn t "
        " WHERE t.organization_id=s.organization_id AND t.session_id=s.id) AS turn_count "
        "FROM customer_service_session s "
        "LEFT JOIN customer_service_ai_session a "
        "  ON a.organization_id=s.organization_id AND a.workspace_id=s.workspace_id AND a.session_id=s.id "
        "WHERE s.organization_id=$1 "
        "  AND ($2 = 0 OR s.workspace_id = $2) "
        "  AND ($3 = '' OR s.status = $3) "
        "  AND ($4 = '' OR coalesce(a.current_intent,'') = $4) "
        "ORDER BY s.id DESC LIMIT $5 OFFSET $6"
    >>,
    Keys = [
        id,
        workspace_id,
        contact_id,
        status,
        rating,
        queued_at,
        claimed_at,
        closed_at,
        current_intent,
        intent_confidence,
        ai_turns,
        unresolved_turns,
        handoff,
        handoff_reason,
        turn_count
    ],
    CountSql = <<
        "SELECT count(*)::bigint AS total FROM customer_service_session s "
        "LEFT JOIN customer_service_ai_session a "
        "  ON a.organization_id=s.organization_id AND a.workspace_id=s.workspace_id AND a.session_id=s.id "
        "WHERE s.organization_id=$1 "
        "  AND ($2 = 0 OR s.workspace_id = $2) "
        "  AND ($3 = '' OR s.status = $3) "
        "  AND ($4 = '' OR coalesce(a.current_intent,'') = $4)"
    >>,
    Filters = [OrgId, WsFilter, Status, Intent],
    case cs_pg_common:fetch_many(Sql, Filters ++ [Limit, Offset], Keys) of
        {error, _} = Err ->
            Err;
        {ok, Rows} ->
            case cs_pg_common:fetch_one(CountSql, Filters, [total]) of
                {ok, #{total := Total}} -> {ok, #{sessions => Rows, total => Total}};
                {error, not_found} -> {ok, #{sessions => Rows, total => length(Rows)}};
                {error, _} = Err2 -> Err2
            end
    end.

%% 满意度统计：评级真源是 `customer_service_session.rating`（**不新建副本**）。
stats(OrgId, Params) ->
    WsFilter = int(maps:get(workspace_id, Params, 0), 0),
    SessionSql = <<
        "SELECT count(*)::bigint AS sessions, count(rating)::bigint AS rated,"
        "coalesce(avg(rating),0)::float8 AS avg_rating,"
        "count(*) FILTER (WHERE rating = 5)::bigint AS rating_5,"
        "count(*) FILTER (WHERE rating = 4)::bigint AS rating_4,"
        "count(*) FILTER (WHERE rating = 3)::bigint AS rating_3,"
        "count(*) FILTER (WHERE rating = 2)::bigint AS rating_2,"
        "count(*) FILTER (WHERE rating = 1)::bigint AS rating_1,"
        "count(*) FILTER (WHERE status = 'closed')::bigint AS closed "
        "FROM customer_service_session WHERE organization_id=$1 AND ($2 = 0 OR workspace_id = $2)"
    >>,
    IntentSql = <<
        "SELECT coalesce(current_intent,'') AS intent, count(*)::bigint AS n,"
        "count(*) FILTER (WHERE handoff)::bigint AS handoff_n "
        "FROM customer_service_ai_session WHERE organization_id=$1 AND ($2 = 0 OR workspace_id = $2) "
        "GROUP BY 1 ORDER BY n DESC, 1 ASC"
    >>,
    ActionSql = <<
        "SELECT action, count(*)::bigint AS n FROM customer_service_ai_turn "
        "WHERE organization_id=$1 AND ($2 = 0 OR workspace_id = $2) GROUP BY 1 ORDER BY n DESC, 1 ASC"
    >>,
    TurnSql = <<
        "SELECT count(*)::bigint AS turns,"
        "count(*) FILTER (WHERE action = 'handoff')::bigint AS handoff_turns,"
        "coalesce(avg(latency_ms),0)::float8 AS avg_latency_ms "
        "FROM customer_service_ai_turn WHERE organization_id=$1 AND ($2 = 0 OR workspace_id = $2)"
    >>,
    SessionKeys = [
        sessions,
        rated,
        avg_rating,
        rating_5,
        rating_4,
        rating_3,
        rating_2,
        rating_1,
        closed
    ],
    with_ok(
        cs_pg_common:fetch_one(SessionSql, [OrgId, WsFilter], SessionKeys),
        fun(Session) ->
            with_ok(
                cs_pg_common:fetch_many(IntentSql, [OrgId, WsFilter], [intent, n, handoff_n]),
                fun(Intents) ->
                    with_ok(
                        cs_pg_common:fetch_many(ActionSql, [OrgId, WsFilter], [action, n]),
                        fun(Actions) ->
                            with_ok(
                                cs_pg_common:fetch_one(TurnSql, [OrgId, WsFilter], [
                                    turns, handoff_turns, avg_latency_ms
                                ]),
                                fun(Turns) ->
                                    {ok, Session#{
                                        intents => Intents,
                                        actions => Actions,
                                        turns => Turns,
                                        satisfaction_rate => satisfaction_rate(Session)
                                    }}
                                end
                            )
                        end
                    )
                end
            )
        end
    ).

%% 有评分的会话中 4~5 星占比（无样本时为 `undefined`，**不显示 0%**——
%% 把"没有样本"渲染成"0% 满意"是统计口径事故）。
satisfaction_rate(#{rated := 0}) ->
    undefined;
satisfaction_rate(#{rated := Rated, rating_5 := R5, rating_4 := R4}) ->
    (R5 + R4) / Rated.

%% ===================================================================
%% A 组（v4 02-A）：事件消费——增量闸门 / seen 幂等 / reconcile sweep
%% ===================================================================

%% 冻结读取谓词（02-A.2.3 逐字，外加 gate.horizon 出站——horizon 与 rows
%% 必须同一语句同一快照，A.3 关键点 1；picked 为空时 LEFT JOIN ON true
%% 仍输出 horizon 行）。派发依据是 xmin-horizon 闸门而非 id 单调推进
%% （探针 A1：TSID 发号时刻 ≠ 提交顺序，id 游标会永久跳过迟提交事件）。
-define(SQL_CONSUME_PICK, <<
    "WITH gate AS (SELECT pg_snapshot_xmin(pg_current_snapshot()) AS horizon),"
    " picked AS ("
    "  SELECT e.id, e.session_id, e.business_identity_id, e.actor_user_id,"
    "         e.actor_kind, e.action, e.detail,"
    "         extract(epoch from e.created_at)::bigint AS created_at"
    "    FROM customer_service_event e, gate"
    "   WHERE e.organization_id = $1"
    "     AND e.workspace_id    = $2"
    "     AND e.xmin::text::bigint >= $3"
    "     AND e.xmin::text::bigint <  gate.horizon::text::bigint"
    "   ORDER BY e.id"
    " )"
    "SELECT gate.horizon::text::bigint, picked.id, picked.session_id, picked.business_identity_id,"
    "       picked.actor_user_id, picked.actor_kind, picked.action, picked.detail,"
    "       picked.created_at"
    "  FROM gate LEFT JOIN picked ON true"
>>).

-define(SQL_CONSUME_CURSOR_LOCK, <<
    "SELECT last_event_id, last_xmin_horizon"
    "  FROM customer_service_ai_consume_cursor"
    " WHERE organization_id=$1 AND workspace_id=$2 AND consumer=$3"
    " FOR UPDATE"
>>).

-define(SQL_CONSUME_SEEN_INSERT, <<
    "INSERT INTO customer_service_ai_consume_seen"
    " (organization_id, workspace_id, consumer, event_id)"
    " VALUES ($1,$2,$3,$4)"
    " ON CONFLICT DO NOTHING RETURNING event_id"
>>).

-define(SQL_CONSUME_CURSOR_UPSERT, <<
    "INSERT INTO customer_service_ai_consume_cursor"
    " (organization_id, workspace_id, consumer, last_event_id, last_xmin_horizon,"
    "  last_consumed_at, updated_at)"
    " VALUES ($1,$2,$3,$4,$5,now(),now())"
    " ON CONFLICT (organization_id, workspace_id, consumer) DO UPDATE SET"
    "  last_event_id = GREATEST(customer_service_ai_consume_cursor.last_event_id,"
    "                           EXCLUDED.last_event_id),"
    "  last_xmin_horizon = GREATEST(customer_service_ai_consume_cursor.last_xmin_horizon,"
    "                               EXCLUDED.last_xmin_horizon),"
    "  last_consumed_at = now(), updated_at = now()"
>>).

-define(SQL_CONSUME_SEEN_PRUNE, <<
    "DELETE FROM customer_service_ai_consume_seen"
    " WHERE organization_id=$1 AND workspace_id=$2 AND consumer=$3"
    "   AND consumed_at < now() - make_interval(secs => $4)"
>>).

-define(SQL_SWEEP_MISSING, <<
    "SELECT e.id, e.session_id, e.business_identity_id, e.actor_user_id,"
    "       e.actor_kind, e.action, e.detail,"
    "       extract(epoch from e.created_at)::bigint AS created_at"
    "  FROM customer_service_event e"
    " WHERE e.organization_id=$1 AND e.workspace_id=$2 AND e.id >= $3"
    "   AND NOT EXISTS ("
    "     SELECT 1 FROM customer_service_ai_consume_seen s"
    "      WHERE s.organization_id=$1 AND s.workspace_id=$2"
    "        AND s.consumer=$4 AND s.event_id=e.id)"
    " ORDER BY e.id"
>>).

%% @doc 增量消费一批（A.3 段 1，单事务）：
%% cursor 行锁 → 冻结谓词取 rows+horizon（同快照）→ seen 幂等 INSERT →
%% ApplyEvent 副作用 → 推进 cursor（GREATEST，horizon 倒退不改写，A.4）→
%% seen 清退。ApplyEvent 返回 `{error, _}` ⇒ 整批回滚（A.4 失败路径）。
%%
%% 返回 `{ok, #{seen => N, applied => N, skipped => N, horizon_now => H,
%% regressed => boolean(), last_event_id => Id}}`。
-spec consume_batch(
    integer(), integer(), binary(), fun((term(), map()) -> ok | {error, term()}), map()
) ->
    {ok, map()} | {error, term()}.
consume_batch(OrgId, WsId, Consumer, ApplyEvent, Opts) ->
    Retention = int_opt(Opts, seen_retention_seconds, ?SEEN_RETENTION_SECONDS),
    elib_pg:with_tx(
        fun(Conn) ->
            {LastEventId, HorizonLo} = consume_cursor_locked(Conn, OrgId, WsId, Consumer),
            {HorizonNow, Rows} = consume_pick(Conn, OrgId, WsId, HorizonLo),
            {Applied, Skipped} = consume_apply(Conn, OrgId, WsId, Consumer, Rows, ApplyEvent),
            MaxId = consume_max_id(Rows, LastEventId),
            {ok, _} =
                elib_pg:execute(
                    Conn, ?SQL_CONSUME_CURSOR_UPSERT, [OrgId, WsId, Consumer, MaxId, HorizonNow]
                ),
            %% DELETE 无 RETURNING：{ok, Count} 二元组（清退 0 行是常态）。
            {ok, _} =
                elib_pg:execute(
                    Conn, ?SQL_CONSUME_SEEN_PRUNE, [OrgId, WsId, Consumer, Retention]
                ),
            {ok, #{
                seen => length(Rows),
                applied => Applied,
                skipped => Skipped,
                horizon_now => HorizonNow,
                regressed => HorizonNow < HorizonLo,
                last_event_id => MaxId
            }}
        end,
        [{reraise, false}]
    ).

%% cursor 行锁：无行（首次消费）⇒ {0, 0}，处理完由 UPSERT 建行
%% （(org,ws,consumer) 单消费者串行是 A 组前提）。
consume_cursor_locked(Conn, OrgId, WsId, Consumer) ->
    case elib_pg:query(Conn, ?SQL_CONSUME_CURSOR_LOCK, [OrgId, WsId, Consumer]) of
        {ok, [#{<<"last_event_id">> := LastId, <<"last_xmin_horizon">> := Horizon}]} ->
            {LastId, Horizon};
        {ok, []} ->
            {0, 0};
        {error, Reason} ->
            throw({consume_cursor_failed, Reason})
    end.

consume_pick(Conn, OrgId, WsId, HorizonLo) ->
    case elib_pg:query(Conn, ?SQL_CONSUME_PICK, [OrgId, WsId, HorizonLo]) of
        {ok, Rows} ->
            Horizon = maps:get(<<"horizon">>, hd(Rows), 0),
            Events = [consume_event(R) || R <- Rows, maps:get(<<"id">>, R, null) =/= null],
            {Horizon, Events};
        {error, Reason} ->
            throw({consume_pick_failed, Reason})
    end.

consume_event(Row) ->
    Ev = #{
        id => maps:get(<<"id">>, Row),
        session_id => maps:get(<<"session_id">>, Row),
        business_identity_id => maps:get(<<"business_identity_id">>, Row),
        actor_user_id => maps:get(<<"actor_user_id">>, Row),
        actor_kind => maps:get(<<"actor_kind">>, Row),
        action => maps:get(<<"action">>, Row),
        created_at => maps:get(<<"created_at">>, Row)
    },
    case maps:get(<<"detail">>, Row, undefined) of
        undefined -> Ev;
        Detail -> Ev#{detail => cs_pg_common:jsonb_read(Detail)}
    end.

%% seen 幂等（I2）：INSERT ON CONFLICT DO NOTHING 影响行数 = 0 ⇒ 已消费，
%% 跳过副作用；> 0 ⇒ ApplyEvent 同事务执行（失败整批回滚）。
%% 计数形状容错：epgsql RETURNING 在不同版本/语句形状下返回 {ok,N} 或
%% {ok,N,Rows}，两者都按 N 判定（见 elib_pg:execute 注释的既有形态差异）。
consume_apply(Conn, OrgId, WsId, Consumer, Rows, ApplyEvent) ->
    lists:foldl(
        fun(Event, {Applied, Skipped}) ->
            N =
                affected(
                    elib_pg:execute(
                        Conn, ?SQL_CONSUME_SEEN_INSERT, [OrgId, WsId, Consumer, maps:get(id, Event)]
                    )
                ),
            case N > 0 of
                true -> consume_fired(Conn, Event, ApplyEvent, Applied, Skipped);
                false -> {Applied, Skipped + 1}
            end
        end,
        {0, 0},
        Rows
    ).

%% 影响行数提取：RETURNING 语句经 elib_pg:execute（parse+execute_batch）
%% 实测返回 {ok,N,Cols,Rows} 四元组，原生 equery 路径为 {ok,N}；无
%% RETURNING 为 {ok,N}。四种形状统一按 N 收敛（execute_count 的事务内版）。
affected({ok, N}) when is_integer(N) -> N;
affected({ok, N, _}) when is_integer(N) -> N;
affected({ok, N, _, _}) when is_integer(N) -> N;
affected(_) -> 0.

consume_fired(Conn, Event, ApplyEvent, Applied, Skipped) ->
    case ApplyEvent(Conn, Event) of
        ok -> {Applied + 1, Skipped};
        {error, Reason} -> throw({apply_event_failed, Reason});
        Other -> throw({apply_event_bad_return, Other})
    end.

consume_max_id([], LastId) ->
    LastId;
consume_max_id(Rows, LastId) ->
    Ids = [maps:get(id, E) || E <- Rows],
    max(lists:max(Ids), LastId).

%% @doc reconcile sweep（A.3 段 2 兜底）：按 id 回扫
%% `last_event_id - backscan_event_count` 起、seen 缺失的事件并补消费。
%% 修复后复扫一次：`remaining` 非 0 ⇒ 调用方告警（sweep 不变量：结束即空）。
-spec consume_sweep(
    integer(), integer(), binary(), fun((term(), map()) -> ok | {error, term()}), map()
) ->
    {ok, map()} | {error, term()}.
consume_sweep(OrgId, WsId, Consumer, ApplyEvent, Opts) ->
    Backscan = int_opt(Opts, backscan_event_count, ?BACKSCAN_EVENT_COUNT),
    elib_pg:with_tx(
        fun(Conn) ->
            {ok, #{last_event_id := LastId}} = fetch_consume_cursor(OrgId, WsId, Consumer),
            Lo = max(0, LastId - Backscan),
            Missing0 = consume_missing(Conn, OrgId, WsId, Consumer, Lo),
            {Applied, _} = consume_apply(Conn, OrgId, WsId, Consumer, Missing0, ApplyEvent),
            Remaining = length(consume_missing(Conn, OrgId, WsId, Consumer, Lo)),
            {ok, #{missing => length(Missing0), repaired => Applied, remaining => Remaining}}
        end,
        [{reraise, false}]
    ).

consume_missing(Conn, OrgId, WsId, Consumer, Lo) ->
    case elib_pg:query(Conn, ?SQL_SWEEP_MISSING, [OrgId, WsId, Lo, Consumer]) of
        {ok, Rows} ->
            [consume_event(R) || R <- Rows];
        {error, Reason} ->
            throw({sweep_scan_failed, Reason})
    end.

%% @doc 观测：当前游标（无行 ⇒ `{error, not_found}`）。
-spec fetch_consume_cursor(integer(), integer(), binary()) ->
    {ok, map()} | {error, not_found | term()}.
fetch_consume_cursor(OrgId, WsId, Consumer) ->
    Sql = <<
        "SELECT last_event_id, last_xmin_horizon,"
        " extract(epoch from last_consumed_at)::bigint AS last_consumed_at"
        "  FROM customer_service_ai_consume_cursor"
        " WHERE organization_id=$1 AND workspace_id=$2 AND consumer=$3"
    >>,
    case elib_pg:query(Sql, [OrgId, WsId, Consumer]) of
        {ok, [Row]} ->
            {ok, #{
                last_event_id => maps:get(<<"last_event_id">>, Row),
                last_xmin_horizon => maps:get(<<"last_xmin_horizon">>, Row),
                last_consumed_at => maps:get(<<"last_consumed_at">>, Row)
            }};
        {ok, []} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 全部游标（reconcile_sweep 遍历面）。
-spec list_consume_cursors() -> [{integer(), integer(), binary()}].
list_consume_cursors() ->
    Sql = <<
        "SELECT organization_id, workspace_id, consumer"
        "  FROM customer_service_ai_consume_cursor ORDER BY 1,2,3"
    >>,
    case elib_pg:query(Sql, []) of
        {ok, Rows} ->
            [
                {
                    maps:get(<<"organization_id">>, R),
                    maps:get(<<"workspace_id">>, R),
                    maps:get(<<"consumer">>, R)
                }
             || R <- Rows
            ];
        {error, Reason} ->
            throw({consume_cursor_list_failed, Reason})
    end.

%% @doc 事件副作用数据面（铁律 5：SQL 只在 infrastructure 层）：从消息事实
%% 幂等派生 AI job。调用方（cs_ai_app:apply_event/6）已完成事件面过滤
%% （action=message.appended ∧ actor_kind=visitor ∧ message_id 为整数）；
%% 本函数在消费事务的连接上完成：消息事实查询 → 非 contact 静默跳过 →
%% generation_epoch 快照 → 幂等 INSERT（`uq_csaij_org_inbound` 冲突 0 行）。
-spec derive_job_tx(term(), integer(), integer(), map(), integer()) ->
    ok | {error, term()}.
derive_job_tx(Conn, OrgId, WsId, Event, MessageId) ->
    SessionId = maps:get(session_id, Event),
    case message_facts(Conn, OrgId, MessageId) of
        {ok, #{sender_type := <<"contact">>, conversation_id := ConvId}} ->
            insert_job(Conn, OrgId, WsId, SessionId, ConvId, MessageId);
        {ok, _Other} ->
            %% 非 contact 发送（坐席代发等）：不建 AI job。
            ok;
        {error, Reason} ->
            {error, {message_facts_unavailable, Reason}}
    end.

message_facts(Conn, OrgId, MessageId) ->
    Sql = <<
        "SELECT sender_type, conversation_id FROM enterprise_message"
        " WHERE organization_id=$1 AND id=$2"
    >>,
    case elib_pg:query(Conn, Sql, [OrgId, MessageId]) of
        {ok, [Row]} ->
            %% epgsql 列名是 binary 键，这里归一为 atom 键供模式匹配。
            {ok, #{
                sender_type => maps:get(<<"sender_type">>, Row),
                conversation_id => maps:get(<<"conversation_id">>, Row)
            }};
        {ok, []} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.

insert_job(Conn, OrgId, WsId, SessionId, ConversationId, InboundMessageId) ->
    Epoch = session_epoch(Conn, OrgId, WsId, SessionId),
    %% TSID 用 default 域：cs_ai_job 未入 elib_tsid_inventory 的冻结清单
    %% （本任务不改 inventory）；default 域全局唯一且 job 表专属，无碰撞面。
    JobId = elib_tsid:generate(default),
    Sql = <<
        "INSERT INTO customer_service_ai_job"
        " (id, organization_id, workspace_id, session_id, conversation_id,"
        "  inbound_message_id, generation_epoch, context_watermark, status,"
        "  deadline_at)"
        " VALUES ($1,$2,$3,$4,$5,$6,$7,$6,'pending', now() + make_interval(secs => $8))"
        " ON CONFLICT (organization_id, workspace_id, inbound_message_id) DO NOTHING"
    >>,
    case
        elib_pg:execute(Conn, Sql, [
            JobId,
            OrgId,
            WsId,
            SessionId,
            ConversationId,
            InboundMessageId,
            Epoch,
            ?JOB_MAX_AGE_SECONDS
        ])
    of
        {ok, _} -> ok;
        {ok, _, _} -> ok;
        {ok, _, _, _} -> ok;
        {error, Reason} -> {error, {job_insert_failed, Reason}}
    end.

session_epoch(Conn, OrgId, WsId, SessionId) ->
    Sql = <<
        "SELECT generation_epoch FROM customer_service_ai_session"
        " WHERE organization_id=$1 AND workspace_id=$2 AND session_id=$3"
    >>,
    case elib_pg:query(Conn, Sql, [OrgId, WsId, SessionId]) of
        {ok, [#{<<"generation_epoch">> := Epoch}]} when is_integer(Epoch) -> Epoch;
        _ -> 1
    end.

int_opt(Opts, Key, Default) when is_map(Opts) ->
    int(maps:get(Key, Opts, Default), Default).

%% @doc 冻结语句清单（静态契约测试面）：A 组读取谓词 + 游标 UPSERT。
%% 语句漂移（horizon 与 rows 拆成两条 SQL / 幂等键缺失 / GREATEST 改写
%% 消失）即红。
-spec sql_statements() -> [binary()].
sql_statements() ->
    [?SQL_CONSUME_PICK, ?SQL_CONSUME_CURSOR_UPSERT, ?SQL_CONSUME_SEEN_INSERT].

%% ===================================================================
%% 内部机械辅助
%% ===================================================================

with_ok({ok, V}, Fun) -> Fun(V);
with_ok({error, _} = Err, _Fun) -> Err.

opt(Map, Key, Default) ->
    case maps:get(Key, Map, Default) of
        undefined -> Default;
        V -> V
    end.

pos(V, _Default) when is_integer(V), V > 0 -> V;
pos(_, Default) -> Default.

int(V, _Default) when is_integer(V) -> V;
int(_, Default) -> Default.

jsonb(Map) when is_map(Map) ->
    jsone:encode(Map, [native_utf8]);
jsonb(B) when is_binary(B) ->
    B;
jsonb(_) ->
    <<"{}">>.

%% float 列表 → pgvector 字面量 `[1,2,3]`（经 `$n::text::vector` 显式转换）。
vector_literal(Vector) ->
    Body = lists:join(<<",">>, [float_to_binary(float(V), [{decimals, 8}, compact]) || V <- Vector]),
    iolist_to_binary([<<"[">>, Body, <<"]">>]).
