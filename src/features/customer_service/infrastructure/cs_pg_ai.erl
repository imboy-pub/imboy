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
    %% B 组（v4 02-B）：租约四操作 + 投递数据面（02-B.4/B.5）。
    claim_job/4,
    renew_lease/5,
    takeover_job/5,
    fetch_job/2,
    mark_job_skipped/3,
    input_version/3,
    delivery_hook/5,
    identity_actor/2,
    %% C 组（v4 02-C）：接管闩锁（粘性 + generation_epoch 自增）。
    latch_handoff/4,
    latch_org_pending/2,
    %% D 组（v4 02-D）：gate 数据面（config 窄读取 / canary / 引用复核 /
    %% session_open）。非 cs_ai_port 冻结 callback，经 impl/1 动态调用。
    fetch_gate_config/1,
    canary_covers/3,
    citations_current/4,
    session_open_tx/3,
    session_open/2,
    search_faq_lexical/4,
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

%% @doc D 组 gate 专用窄读取：只取 gate 需要的 v4 列（02-D gate_1/gate_3）。
%% 与 v1 `fetch_config/1` 分离：字段对照表的逐项替换属 P0-8（04 合同 §P0-8.4），
%% 而 gate_1 语义只能读 v4 列（`auto_reply_mode`/`faq_require_hit`）；P0-8
%% 对齐后本函数可由对齐版 fetch_config 取代。
-spec fetch_gate_config(integer()) -> {ok, map()} | {error, term()}.
fetch_gate_config(OrgId) ->
    Sql = <<
        "SELECT auto_reply_mode, faq_require_hit, config_version, seat_identity_id "
        "FROM customer_service_ai_agent WHERE organization_id=$1 ORDER BY id LIMIT 1"
    >>,
    case elib_pg:query(Sql, [OrgId]) of
        {ok, [Row]} ->
            {ok, #{
                auto_reply_mode => maps:get(<<"auto_reply_mode">>, Row, <<"disabled">>),
                faq_require_hit => maps:get(<<"faq_require_hit">>, Row, true),
                config_version => maps:get(<<"config_version">>, Row, 1),
                seat_identity_id => maps:get(<<"seat_identity_id">>, Row, undefined)
            }};
        {ok, []} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.

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
        opt(Params, name, <<"智能客服"/utf8>>),
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

%% 词法检索（02-D.5 v4 语义）：PG 侧分词 → OR 组 tsquery（多词 AND 会
%% 零命中）；status='published' + 有效期窗 + workspace 适用范围。
%% 02-D.5 字面的 `f.workspace_id = $2` 在 v4 schema 中以 scope_workspace_ids
%% 数组语义等价落地（表无 workspace_id 列，P0-1 时已记录该张力）：
%% 空数组 = 本 org 全部 workspace。
search_faq_lexical(OrgId, Query, Limit) when is_binary(Query), Query =/= <<>> ->
    search_faq_lexical(OrgId, 0, Query, Limit);
search_faq_lexical(_OrgId, _Query, _Limit) ->
    {ok, []}.

%% @doc 带 workspace 约束的词法检索（WorkspaceId = 0 时不约束，管理面用；
%% 运行时检索必须传真实 workspace）。有效期窗与 status 双门：过期/停更
%% FAQ 不可被引用（D-3 的检索侧源头）。
search_faq_lexical(OrgId, WsId, Query, Limit) when is_binary(Query), Query =/= <<>> ->
    %% 02-D.5 的 OR 组构造有一个与本 pg_jieba 构建冲突的洞：jiebacfg 会把
    %% 词间空格本身产出 ' ' 词位，array_to_string 后得到 " | 发票 | 退款"
    %% 这类非法 tsquery（服务端 syntax error in tsquery）。此处过滤空白
    %% 词位——语义与合同一致（多词 OR），D-4 验收（多词命中>0）优先。
    Sql = <<
        "WITH q AS MATERIALIZED ("
        "  SELECT (SELECT string_agg(x, ' | ')"
        "            FROM unnest(tsvector_to_array(to_tsvector('jiebacfg',$3))) AS x"
        "           WHERE btrim(x) <> '') AS tsq"
        ") "
        "SELECT f.id, "
        "  ts_rank(f.search_tsv, CASE WHEN q.tsq='' THEN NULL ELSE to_tsquery('jiebacfg',q.tsq) END)::float8 AS score "
        "FROM customer_service_faq f, q "
        "WHERE f.organization_id=$1 AND f.status = 'published' "
        "  AND (f.valid_until IS NULL OR f.valid_until > now())"
        "  AND (f.valid_from IS NULL OR f.valid_from <= now()) "
        "  AND ($2::bigint = 0 OR $2 = ANY(f.scope_workspace_ids) OR f.scope_workspace_ids = '{}') "
        "  AND f.search_tsv @@ CASE WHEN q.tsq='' THEN NULL ELSE to_tsquery('jiebacfg',q.tsq) END "
        "ORDER BY score DESC, f.id ASC LIMIT $4"
    >>,
    Args = [OrgId, int(WsId, 0), Query, pos(Limit, 10)],
    case cs_pg_common:fetch_many(Sql, Args, [id, score]) of
        {ok, Rows} -> {ok, [{maps:get(id, R), maps:get(score, R, 0.0)} || R <- Rows]};
        {error, _} = Err -> Err
    end;
search_faq_lexical(_OrgId, _WsId, _Query, _Limit) ->
    {ok, []}.

%% 语义检索：v4 迁移**无 vector 列**（R17；向量/RRF 扩展不在 P0 范围，
%% 由后续独立迁移添加）。通道恒空 = fail-closed（应用层未配置 embedding
%% provider 时不调用为前提，双保险：误调用也不产生 SQL 错误或假数据）。
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
        "ai_turns,unresolved_turns,handoff,handoff_reason,generation_epoch,version,"
        "extract(epoch from updated_at)::bigint AS updated_at "
        "FROM customer_service_ai_session "
        "WHERE organization_id=$1 AND workspace_id=$2 AND session_id=$3"
    >>,
    cs_pg_common:fetch_one(Sql, [OrgId, WsId, SessionId], [
        session_id,
        contact_id,
        conversation_id,
        current_intent,
        intent_confidence,
        ai_turns,
        unresolved_turns,
        handoff,
        handoff_reason,
        generation_epoch,
        version,
        updated_at
    ]).

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
        "  AND ($2::bigint = 0 OR s.workspace_id = $2) "
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
        "  AND ($2::bigint = 0 OR s.workspace_id = $2) "
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
        "FROM customer_service_session WHERE organization_id=$1 AND ($2::bigint = 0 OR workspace_id = $2)"
    >>,
    IntentSql = <<
        "SELECT coalesce(current_intent,'') AS intent, count(*)::bigint AS n,"
        "count(*) FILTER (WHERE handoff)::bigint AS handoff_n "
        "FROM customer_service_ai_session WHERE organization_id=$1 AND ($2::bigint = 0 OR workspace_id = $2) "
        "GROUP BY 1 ORDER BY n DESC, 1 ASC"
    >>,
    ActionSql = <<
        "SELECT action, count(*)::bigint AS n FROM customer_service_ai_turn "
        "WHERE organization_id=$1 AND ($2::bigint = 0 OR workspace_id = $2) GROUP BY 1 ORDER BY n DESC, 1 ASC"
    >>,
    TurnSql = <<
        "SELECT count(*)::bigint AS turns,"
        "count(*) FILTER (WHERE action = 'handoff')::bigint AS handoff_turns,"
        "coalesce(avg(latency_ms),0)::float8 AS avg_latency_ms "
        "FROM customer_service_ai_turn WHERE organization_id=$1 AND ($2::bigint = 0 OR workspace_id = $2)"
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

%% ===================================================================
%% B 组（v4 02-B）：租约四操作 + 投递数据面
%% ===================================================================

%% 租约 TTL 默认 60s（工程参数；job_timeout_ms 配置化读取属 P0-5 后接线）。
-define(LEASE_TTL_SECONDS, 60).

%% @doc 认领（02-B.5）：pending → running，CAS 带 version。影响 0 行 ⇒
%% 已被认领或 version 失配（B-1 栅栏）。
-spec claim_job(integer(), integer(), binary(), pos_integer()) ->
    {ok, map()} | {error, already_claimed | not_found | term()}.
claim_job(OrgId, JobId, WorkerId, TtlSeconds) ->
    Sql = <<
        "UPDATE customer_service_ai_job"
        " SET status='running', lease_owner=$3, lease_until=now() + make_interval(secs => $4),"
        "     version=version+1"
        " WHERE organization_id=$1 AND id=$2 AND status='pending' AND attempt=attempt"
        " RETURNING id, status, attempt, version, lease_owner,"
        " extract(epoch from lease_until)::bigint AS lease_until,"
        " extract(epoch from deadline_at)::bigint AS deadline_at"
    >>,
    case elib_pg:query(Sql, [OrgId, JobId, WorkerId, TtlSeconds]) of
        {ok, [Row]} -> {ok, job_row(Row)};
        {ok, []} -> {error, already_claimed};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 续租：owner + version 双断言。
-spec renew_lease(integer(), integer(), binary(), integer(), pos_integer()) ->
    ok | {error, lease_lost}.
renew_lease(OrgId, JobId, WorkerId, ExpectedVersion, TtlSeconds) ->
    lease_cast(
        <<
            "UPDATE customer_service_ai_job"
            " SET lease_until=now() + make_interval(secs => $5), version=version+1"
            " WHERE organization_id=$1 AND id=$2 AND lease_owner=$3 AND version=$4"
            "   AND status='running'"
        >>,
        [OrgId, JobId, WorkerId, ExpectedVersion, TtlSeconds]
    ).

%% @doc 接管：**必须含 `lease_until < now()`**（02-B.5 冻结；缺此条件双活）。
-spec takeover_job(integer(), integer(), binary(), integer(), pos_integer()) ->
    ok | {error, lease_lost}.
takeover_job(OrgId, JobId, WorkerId, ExpectedVersion, TtlSeconds) ->
    lease_cast(
        <<
            "UPDATE customer_service_ai_job"
            " SET lease_owner=$3, lease_until=now() + make_interval(secs => $5),"
            "     version=version+1"
            " WHERE organization_id=$1 AND id=$2 AND status='running' AND version=$4"
            "   AND lease_until < now()"
        >>,
        [OrgId, JobId, WorkerId, ExpectedVersion, TtlSeconds]
    ).

lease_cast(Sql, Args) ->
    case elib_pg:execute(Sql, Args) of
        {ok, N} when N > 0 -> ok;
        {ok, N, _} when N > 0 -> ok;
        {ok, _, _, _} -> ok;
        _ -> {error, lease_lost}
    end.

%% @doc AI seat 身份的归属用户（organization_business_identity.created_by_user_id）。
%% 既有 canonical 不变量要求出站消息 actor_user_id 非空（eb_message domain）；
%% AI 消息的发言方语义由 sender_type=business_identity + delivery.origin_kind
%% 承载，actor 取身份归属账户（与坐席代发同构，不伪装人类对话方）。
-spec identity_actor(integer(), integer()) -> {ok, integer()} | {error, term()}.
identity_actor(OrgId, IdentityId) ->
    Sql = <<
        "SELECT created_by_user_id FROM organization_business_identity"
        " WHERE organization_id=$1 AND id=$2"
    >>,
    case elib_pg:query(Sql, [OrgId, IdentityId]) of
        {ok, [#{<<"created_by_user_id">> := Creator}]} when is_integer(Creator) ->
            {ok, Creator};
        {ok, _} ->
            {error, identity_creator_missing};
        {error, Reason} ->
            {error, Reason}
    end.

%% ===================================================================
%% D 组（v4 02-D）：证据 gate 数据面
%% ===================================================================

%% @doc gate_1（mode_ok）的 canary 覆盖检查：canary 白名单覆盖
%% workspace 或 session 粒度；空集 = canary 不投任何会话（迁移 180 注释）。
-spec canary_covers(integer(), integer(), integer()) -> {ok, boolean()} | {error, term()}.
canary_covers(OrgId, WsId, SessionId) ->
    Sql = <<
        "SELECT EXISTS ("
        "  SELECT 1 FROM customer_service_ai_canary"
        "   WHERE organization_id=$1 AND scope_kind='workspace' AND scope_id=$2"
        ") OR EXISTS ("
        "  SELECT 1 FROM customer_service_ai_canary"
        "   WHERE organization_id=$1 AND scope_kind='session' AND scope_id=$3"
        ") AS covers"
    >>,
    case elib_pg:query(Sql, [OrgId, WsId, SessionId]) of
        {ok, [#{<<"covers">> := V}]} when is_boolean(V) -> {ok, V};
        {ok, _} -> {ok, false};
        {error, Reason} -> {error, Reason}
    end.

%% @doc gate_4（citations_current）：每个 cited (id, version) 的当前行仍是
%% published、未过期、version 一致——任一失配即 stale（I10：引用版本一致）。
%% 无引用时返回 {ok, false}（gate_3 的 require_hit 另判）。
-spec citations_current(term(), integer(), [integer()], [integer()]) ->
    {ok, boolean()} | {error, term()}.
citations_current(_Conn, _OrgId, [], _Versions) ->
    {ok, false};
citations_current(Conn, OrgId, FaqIds, FaqVersions) when
    is_list(FaqIds), is_list(FaqVersions), length(FaqIds) =:= length(FaqVersions)
->
    Sql = <<
        "SELECT count(*)::bigint AS ok_count"
        "  FROM unnest($2::bigint[], $3::integer[]) AS c(fid, fver)"
        "  JOIN customer_service_faq f"
        "    ON f.organization_id = $1 AND f.id = c.fid AND f.version = c.fver"
        " WHERE f.status = 'published'"
        "   AND (f.valid_until IS NULL OR f.valid_until > now())"
        "   AND (f.valid_from IS NULL OR f.valid_from <= now())"
    >>,
    Expected = length(FaqIds),
    case elib_pg:query(Conn, Sql, [OrgId, FaqIds, FaqVersions]) of
        {ok, [#{<<"ok_count">> := N}]} when is_integer(N) -> {ok, N =:= Expected};
        {ok, _} -> {ok, false};
        {error, Reason} -> {error, Reason}
    end;
citations_current(_Conn, _OrgId, _Ids, _Versions) ->
    {error, {invalid_argument, citations}}.

%% @doc gate_7（session_open）：会话 status ∈ {queued, active}。
-spec session_open_tx(term(), integer(), integer()) -> {ok, boolean()} | {error, term()}.
session_open_tx(Conn, OrgId, SessionId) ->
    Sql = <<
        "SELECT status FROM customer_service_session"
        " WHERE organization_id=$1 AND id=$2"
    >>,
    case elib_pg:query(Conn, Sql, [OrgId, SessionId]) of
        {ok, [#{<<"status">> := <<"queued">>}]} -> {ok, true};
        {ok, [#{<<"status">> := <<"active">>}]} -> {ok, true};
        {ok, [_]} -> {ok, false};
        {ok, []} -> {ok, false};
        {error, Reason} -> {error, Reason}
    end.

%% @doc gate_7 的投递前置快检（池连接版）：会话在进入 canonical 事务**之前**
%% 已关闭 ⇒ 免组装整条消息链直接跳过。事务内复核（竞态窗口）仍由
%% delivery_hook 里的 gate_fun 承担——本函数只是同谓语的提前求值。
-spec session_open(integer(), integer()) -> {ok, boolean()} | {error, term()}.
session_open(OrgId, SessionId) ->
    Sql = <<
        "SELECT status FROM customer_service_session"
        " WHERE organization_id=$1 AND id=$2"
    >>,
    case elib_pg:query(Sql, [OrgId, SessionId]) of
        {ok, [#{<<"status">> := <<"queued">>}]} -> {ok, true};
        {ok, [#{<<"status">> := <<"active">>}]} -> {ok, true};
        {ok, [_]} -> {ok, false};
        {ok, []} -> {ok, false};
        {error, Reason} -> {error, Reason}
    end.

%% ===================================================================
%% C 组（v4 02-C）：人工接管闩锁
%% ===================================================================

%% @doc E2 置位：粘性（handoff 一旦 true 不回退）+ generation_epoch+1
%% （「人工永远优先」的投递栅栏载体；在途旧生成结果因 epoch 失配被拦）。
%% handoff_at/handoff_reason 只在首次置位时写入（粘性语义）。
-spec latch_handoff(integer(), integer(), integer(), binary()) ->
    {ok, map()} | {error, term()}.
latch_handoff(OrgId, WsId, SessionId, Reason) ->
    Sql = <<
        "INSERT INTO customer_service_ai_session"
        " (organization_id, workspace_id, session_id, handoff, handoff_reason,"
        "  handoff_at, generation_epoch)"
        " VALUES ($1,$2,$3,true,$4,now(),2)"
        " ON CONFLICT (organization_id, workspace_id, session_id) DO UPDATE SET"
        "  handoff = true,"
        "  handoff_reason = CASE WHEN customer_service_ai_session.handoff"
        "                        THEN customer_service_ai_session.handoff_reason"
        "                        ELSE EXCLUDED.handoff_reason END,"
        "  handoff_at = CASE WHEN customer_service_ai_session.handoff"
        "                    THEN customer_service_ai_session.handoff_at ELSE now() END,"
        "  generation_epoch = customer_service_ai_session.generation_epoch + 1,"
        "  version = customer_service_ai_session.version + 1, updated_at = now()"
        " RETURNING handoff, handoff_reason, generation_epoch"
    >>,
    case elib_pg:query(Sql, [OrgId, WsId, SessionId, Reason]) of
        {ok, [Row]} ->
            {ok, #{
                handoff => maps:get(<<"handoff">>, Row),
                handoff_reason => maps:get(<<"handoff_reason">>, Row),
                generation_epoch => maps:get(<<"generation_epoch">>, Row)
            }};
        {ok, []} ->
            {error, latch_failed};
        {error, Reason0} ->
            {error, Reason0}
    end.

%% @doc E2（配置撤销面）：把 org 内全部未闩锁的 AI 会话置位（auto_reply_mode
%% 撤销 / canary 撤销 => 在途会话一并停）。返回置位行数。
-spec latch_org_pending(integer(), binary()) -> {ok, non_neg_integer()} | {error, term()}.
latch_org_pending(OrgId, Reason) ->
    Sql = <<
        "UPDATE customer_service_ai_session"
        " SET handoff = true,"
        "     handoff_reason = $2,"
        "     handoff_at = now(),"
        "     generation_epoch = generation_epoch + 1,"
        "     version = version + 1, updated_at = now()"
        " WHERE organization_id=$1 AND handoff = false"
    >>,
    case elib_pg:execute(Sql, [OrgId, Reason]) of
        {ok, N} when is_integer(N) -> {ok, N};
        {ok, N, _} when is_integer(N) -> {ok, N};
        {error, Reason0} -> {error, Reason0}
    end.

%% job 七态（180 CHECK）binary → atom；未知值 fail-closed 原样返回。
job_status(<<"pending">>) -> pending;
job_status(<<"running">>) -> running;
job_status(<<"retry">>) -> retry;
job_status(<<"completed">>) -> completed;
job_status(<<"cancelled">>) -> cancelled;
job_status(<<"failed">>) -> failed;
job_status(<<"dead">>) -> dead;
job_status(Other) -> Other.

%% @doc job 快读（投递编排的预检面）。
-spec fetch_job(integer(), integer()) -> {ok, map()} | {error, not_found | term()}.
fetch_job(OrgId, JobId) ->
    Sql = <<
        "SELECT id, organization_id, workspace_id, session_id, conversation_id,"
        "       inbound_message_id, generation_epoch, context_watermark, status, attempt,"
        "       version, lease_owner,"
        "       extract(epoch from lease_until)::bigint AS lease_until,"
        "       extract(epoch from deadline_at)::bigint AS deadline_at"
        "  FROM customer_service_ai_job WHERE organization_id=$1 AND id=$2"
    >>,
    case
        cs_pg_common:fetch_one(Sql, [OrgId, JobId], [
            id,
            organization_id,
            workspace_id,
            session_id,
            conversation_id,
            inbound_message_id,
            generation_epoch,
            context_watermark,
            status,
            attempt,
            version,
            lease_owner,
            lease_until,
            deadline_at
        ])
    of
        {ok, Job} -> {ok, Job#{status => job_status(maps:get(status, Job))}};
        {error, _} = Err -> Err
    end.

job_row(Row) ->
    #{
        id => maps:get(<<"id">>, Row),
        status => job_status(maps:get(<<"status">>, Row)),
        attempt => maps:get(<<"attempt">>, Row),
        version => maps:get(<<"version">>, Row),
        lease_owner => maps:get(<<"lease_owner">>, Row),
        lease_until => maps:get(<<"lease_until">>, Row),
        deadline_at => maps:get(<<"deadline_at">>, Row)
    }.

%% @doc 不可投递终态转换（02-B.4 错误语义）。180 迁移的 status CHECK 是七态
%% （无 superseded/expired），映射：expired → dead、superseded → cancelled；
%% 细分原因写 `last_error_code`，调用方返回值保持 `{skipped, expired |
%% superseded}` 冻结形状。
%% 180 七态映射（B.4 冻结语义 × CHECK 值域）：expired→dead、
%% superseded/stale_citation→cancelled、no_grounding→failed（I9/I10 细分
%% 写 last_error_code）。
-spec mark_job_skipped(integer(), integer(), expired | superseded | stale_citation | no_grounding) ->
    {ok, skipped, expired | superseded | stale_citation | no_grounding} | {error, term()}.
mark_job_skipped(OrgId, JobId, Reason) ->
    {Status, Code} =
        case Reason of
            expired -> {<<"dead">>, <<"expired">>};
            superseded -> {<<"cancelled">>, <<"superseded">>};
            stale_citation -> {<<"cancelled">>, <<"stale_citation">>};
            no_grounding -> {<<"failed">>, <<"no_grounding">>}
        end,
    Sql = <<
        "UPDATE customer_service_ai_job"
        " SET status=$3, last_error_code=$4, version=version+1,"
        "     lease_owner=NULL, lease_until=NULL, completed_at=now()"
        " WHERE organization_id=$1 AND id=$2"
    >>,
    case elib_pg:execute(Sql, [OrgId, JobId, Status, Code]) of
        {ok, _} -> {ok, skipped, Reason};
        {ok, _, _} -> {ok, skipped, Reason};
        {error, R} -> {error, R}
    end.

%% @doc `input_version` 求值（02-B.3.2 逐字；在持 L1 的 canonical 事务内
%% 调用——方案甲的 conversation 行锁使入站写与本读互斥，B-5 稳定）。
-spec input_version(term(), integer(), integer()) -> {ok, non_neg_integer()} | {error, term()}.
input_version(Conn, OrgId, ConversationId) ->
    Sql = <<
        "SELECT COALESCE(max(m.id), 0) AS watermark"
        "  FROM enterprise_message m"
        " WHERE m.organization_id = $1"
        "   AND m.conversation_id = $2"
        "   AND m.sender_type     = 'contact'"
    >>,
    case elib_pg:query(Conn, Sql, [OrgId, ConversationId]) of
        {ok, [#{<<"watermark">> := V}]} when is_integer(V) -> {ok, V};
        {ok, _} -> {ok, 0};
        {error, Reason} -> {error, Reason}
    end.

%% @doc AI 投递的 persist_hook 数据面（02-B.4 投递事务的尾部三件事）：
%% deadline 复核 → input_version 复核（L1 内）→ L4 delivery 幂等 INSERT →
%% job → completed（owner+version CAS）。任一失败返回 `{error, _}` ⇒
%% canonical 事务整体回滚（消息与 delivery 原子，`{persist_hook_failed, _}`
%% 由 canonical 解包）。注意 02-B.4 的 D 组 gate（faq_require_hit/citations）
%% 与 E 组结算（L3）由 P0-5/P0-6 以 GateFun/after_delivery 注入，本版本
%% 预留参数位。
-spec delivery_hook(
    integer(), integer(), binary(), integer(), map()
) ->
    fun((term(), map()) -> ok | {error, term()}).
delivery_hook(OrgId, JobId, WorkerId, ExpectedVersion, Opts) ->
    fun(Conn, StoredMessage) ->
        Job0 = job_in_delivery_tx(Conn, OrgId, JobId),
        Job = Job0#{seat_identity_id => maps:get(seat_identity_id, Opts)},
        case maps:get(status, Job) of
            running ->
                deliver_check_deadline(
                    Conn,
                    OrgId,
                    JobId,
                    Job,
                    WorkerId,
                    ExpectedVersion,
                    StoredMessage,
                    Opts
                );
            _ ->
                {error, {skipped, job_not_running}}
        end
    end.

job_in_delivery_tx(Conn, OrgId, JobId) ->
    %% L2：job 行锁（FOR UPDATE）——投递事务内的栅栏。
    Sql = <<
        "SELECT workspace_id, session_id, status, lease_owner, version, attempt,"
        "       conversation_id, context_watermark, generation_epoch,"
        "       extract(epoch from deadline_at)::bigint AS deadline_at"
        "  FROM customer_service_ai_job WHERE organization_id=$1 AND id=$2 FOR UPDATE"
    >>,
    case elib_pg:query(Conn, Sql, [OrgId, JobId]) of
        {ok, [Row]} ->
            #{
                workspace_id => maps:get(<<"workspace_id">>, Row),
                session_id => maps:get(<<"session_id">>, Row),
                status => job_status(maps:get(<<"status">>, Row)),
                lease_owner => maps:get(<<"lease_owner">>, Row),
                version => maps:get(<<"version">>, Row),
                attempt => maps:get(<<"attempt">>, Row),
                conversation_id => maps:get(<<"conversation_id">>, Row),
                context_watermark => maps:get(<<"context_watermark">>, Row),
                generation_epoch => maps:get(<<"generation_epoch">>, Row),
                deadline_at => maps:get(<<"deadline_at">>, Row)
            };
        {ok, []} ->
            throw({delivery_failed, job_not_found});
        {error, Reason} ->
            throw({delivery_failed, Reason})
    end.

deliver_check_deadline(
    Conn, OrgId, JobId, Job, WorkerId, ExpectedVersion, StoredMessage, Opts
) ->
    case maps:get(deadline_at, Job) of
        Dl when is_integer(Dl), Dl > 0 ->
            Now = os:system_time(second),
            case Now >= Dl of
                true ->
                    %% 超时终态写在钩子外由调用方 mark_job_skipped 完成；
                    %% 钩子内只裁决"不投递"（回滚消息）。
                    {error, {skipped, expired}};
                false ->
                    deliver_check_lease_and_version(
                        Conn, OrgId, JobId, Job, WorkerId, ExpectedVersion, StoredMessage, Opts
                    )
            end;
        _ ->
            {error, {skipped, expired}}
    end.

deliver_check_lease_and_version(
    Conn, OrgId, JobId, Job, WorkerId, ExpectedVersion, StoredMessage, Opts
) ->
    case
        maps:get(lease_owner, Job) =:= WorkerId andalso
            maps:get(version, Job) =:= ExpectedVersion
    of
        false ->
            %% 陈旧租约/失配 version ⇒ I4：不产生任何副作用（回滚消息）。
            {error, {skipped, lease_lost}};
        true ->
            deliver_check_input(
                Conn, OrgId, JobId, Job, WorkerId, ExpectedVersion, StoredMessage, Opts
            )
    end.

deliver_check_input(
    Conn, OrgId, JobId, Job, WorkerId, ExpectedVersion, StoredMessage, Opts
) ->
    ConversationId = maps:get(conversation_id, Job),
    {ok, Current} = input_version(Conn, OrgId, ConversationId),
    case Current > maps:get(context_watermark, Job, 0) of
        true ->
            {error, {skipped, superseded}};
        false ->
            run_delivery_gates(
                Conn, OrgId, JobId, Job, WorkerId, ExpectedVersion, StoredMessage, Opts
            )
    end.

%% P0-5/P0-6 的注入位：Opts.gate_fun(Conn, Job) -> ok | {error, {skipped, X}}
%% （D 组证据 gate）；通过后写 L4 + 完成 job。
run_delivery_gates(Conn, OrgId, JobId, Job, WorkerId, ExpectedVersion, StoredMessage, Opts) ->
    case maps:get(gate_fun, Opts, undefined) of
        Gate when is_function(Gate, 2) ->
            case Gate(Conn, Job) of
                ok ->
                    write_delivery_row(
                        Conn, OrgId, JobId, Job, WorkerId, ExpectedVersion, StoredMessage
                    );
                {error, _} = Err ->
                    Err
            end;
        _ ->
            write_delivery_row(
                Conn, OrgId, JobId, Job, WorkerId, ExpectedVersion, StoredMessage
            )
    end.

write_delivery_row(Conn, OrgId, JobId, Job, WorkerId, ExpectedVersion, StoredMessage) ->
    MessageId = maps:get(id, StoredMessage),
    %% seat_identity_id 不在 job 表——由编排层从 agent 配置经 Opts 注入。
    SeatIdentity =
        case Job of
            #{seat_identity_id := Sid} -> Sid;
            _ -> erlang:error({delivery_failed, seat_identity_missing})
        end,
    Sql = <<
        "INSERT INTO customer_service_ai_delivery"
        " (organization_id, workspace_id, session_id, job_id, message_id, conversation_id,"
        "  origin_kind, seat_identity_id, generation_epoch)"
        " VALUES ($1,$2,$3,$4,$5,$6,'ai_product',$7,$8)"
        " ON CONFLICT (organization_id, session_id, job_id) DO NOTHING"
    >>,
    case
        elib_pg:execute(Conn, Sql, [
            OrgId,
            maps:get(workspace_id, Job),
            maps:get(session_id, Job),
            JobId,
            MessageId,
            maps:get(conversation_id, Job),
            SeatIdentity,
            maps:get(generation_epoch, Job)
        ])
    of
        {ok, N} when N > 0 -> complete_job_in_tx(Conn, OrgId, JobId, WorkerId, ExpectedVersion);
        {ok, N, _} when N > 0 -> complete_job_in_tx(Conn, OrgId, JobId, WorkerId, ExpectedVersion);
        _ ->
            %% uq_csaid_reply 冲突：另一 worker 已投递（B-3）⇒ 本侧让位。
            {error, {skipped, already_delivered}}
    end.

complete_job_in_tx(Conn, OrgId, JobId, WorkerId, ExpectedVersion) ->
    Sql = <<
        "UPDATE customer_service_ai_job"
        " SET status='completed', version=version+1, lease_owner=NULL, lease_until=NULL,"
        "     completed_at=now()"
        " WHERE organization_id=$1 AND id=$2 AND lease_owner=$3 AND version=$4"
        "   AND status='running'"
    >>,
    case elib_pg:execute(Conn, Sql, [OrgId, JobId, WorkerId, ExpectedVersion]) of
        {ok, N} when N > 0 -> ok;
        {ok, N, _} when N > 0 -> ok;
        _ -> {error, {skipped, lease_lost}}
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
