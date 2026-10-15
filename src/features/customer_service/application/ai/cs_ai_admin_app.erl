%%% @doc 智能客服 Agent 的**管理面用例**：AI 坐席配置、FAQ 知识库、对话记录、满意度统计。
%%%
%%% 与运行时（`cs_ai_app`）分离的理由：这两条链路的**调用者与信任边界不同**——
%%% 运行时被访客消息触发（外部输入驱动），管理面被平台/租户管理员触发（内部治理）。
%%% 混在一个模块里会让"哪些参数来自不可信输入"变得不可审计。
%%%
%%% 全部读写经 `cs_ai_port`（铁律 6：首参 OrgId），本模块只做参数收敛与投影裁剪——
%%% **不返回** embedding 原始向量（无意义且体积大），只返回 `embedded` 布尔。
-module(cs_ai_admin_app).

-moduledoc "智能客服 Agent 管理面用例（配置 / FAQ / 对话记录 / 满意度统计）。".

-export([
    config_set/2,
    config_view/2,
    faq_list/2,
    faq_upsert/2,
    faq_delete/2,
    sessions/2,
    session_turns/2,
    stats/2,
    preview/2
]).

%% FAQ 正文出站投影：**不含** embedding 列（只出 `embedded` 标志）。
-define(FAQ_PROJECTION, [
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

%% ===================================================================
%% 配置
%% ===================================================================

%% @doc 读组织级 AI 坐席配置。未配置时返回 `#{configured => false}` 而不是 404 ——
%% 后台首屏要能显示"还没配"，而不是把它当错误。
-spec config_view(integer(), map()) -> {ok, map()} | {error, term()}.
config_view(OrgId, Params) when is_integer(OrgId), OrgId > 0, is_map(Params) ->
    case (cs_ai_port:impl(Params)):fetch_config(OrgId) of
        {ok, Config} -> {ok, Config#{configured => true}};
        {error, not_found} -> {ok, #{configured => false}};
        {error, Reason} -> {error, Reason}
    end;
config_view(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

%% @doc 建/改配置（幂等）。`id` 缺省时生成 TSID（一 Org 一配置，重复调用即覆盖）。
-spec config_set(integer(), map()) -> {ok, map()} | {error, term()}.
config_set(OrgId, Params) when is_integer(OrgId), OrgId > 0, is_map(Params) ->
    case validate_thresholds(Params) of
        ok ->
            case
                with_config_id(OrgId, Params, fun(Id, P1) ->
                    (cs_ai_port:impl(P1)):upsert_config(OrgId, P1#{id => Id})
                end)
            of
                {ok, _} = Ok ->
                    %% E2 触发二（02-C.3）：auto_reply_mode 撤销为 disabled ⇒
                    %% org 内全部在途 AI 会话闩锁（粘性，closed 后新会话才清）。
                    %% 永不改变 config_set 自身的返回形状。
                    Revoked =
                        maps:get(auto_reply_mode, Params, undefined) =:= <<"disabled">> orelse
                            maps:get(auto_reply_mode, Params, undefined) =:= disabled,
                    case Revoked of
                        true -> cs_ai_delivery_app:on_config_revoked(OrgId, <<"config_revoked">>);
                        false -> ok
                    end,
                    Ok;
                {error, _} = Err ->
                    Err
            end;
        {error, _} = Err ->
            Err
    end;
config_set(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

%% v4（04 对照表）：阈值字段删除；校验 auto_reply_mode 值域 +
%% faq_require_hit 布尔 + 轮数非负。坏值会让门控语义静默失效，fail-fast。
validate_thresholds(Params) ->
    Mode = maps:get(auto_reply_mode, Params, undefined),
    RequireHit = maps:get(faq_require_hit, Params, undefined),
    MaxTurns = maps:get(max_unresolved_turns, Params, undefined),
    case valid_mode(Mode) andalso valid_bool(RequireHit) andalso valid_non_neg(MaxTurns) of
        true ->
            ok;
        false ->
            {error, {invalid_argument, config_fields}}
    end.

valid_mode(undefined) ->
    true;
valid_mode(M) when is_binary(M) ->
    lists:member(M, [<<"disabled">>, <<"observe">>, <<"canary">>, <<"enabled">>]);
valid_mode(_) ->
    false.

valid_bool(undefined) -> true;
valid_bool(B) when is_boolean(B) -> true;
valid_bool(_) -> false.

valid_non_neg(undefined) -> true;
valid_non_neg(V) when is_integer(V) -> V >= 0;
valid_non_neg(_) -> false.

%% 配置行 id：优先沿用已存在的那一行（一 Org 一配置），否则生成。
with_config_id(OrgId, Params, Fun) ->
    case maps:get(id, Params, undefined) of
        Id when is_integer(Id), Id > 0 ->
            Fun(Id, Params);
        _ ->
            Impl = cs_ai_port:impl(Params),
            case Impl:fetch_config(OrgId) of
                {ok, #{id := Existing}} ->
                    Fun(Existing, Params);
                _ ->
                    case cs_app_support:new_id(ai_cs_config, Params) of
                        {ok, NewId} -> Fun(NewId, Params);
                        {error, _} = Err -> Err
                    end
            end
    end.

%% ===================================================================
%% FAQ 知识库
%% ===================================================================

-spec faq_list(integer(), map()) -> {ok, [map()]} | {error, term()}.
faq_list(OrgId, Params) when is_integer(OrgId), OrgId > 0, is_map(Params) ->
    case (cs_ai_port:impl(Params)):list_faq(OrgId, Params) of
        {ok, Rows} -> {ok, [project_faq(R) || R <- Rows]};
        {error, _} = Err -> Err
    end;
faq_list(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

%% @doc FAQ 建/改。`question` 必填非空、`intent` 必须落在 `cs_ai_intent` 值域内 ——
%% 意图标签是检索与统计的联表键，允许自由文本会让统计里出现拼写变体。
-spec faq_upsert(integer(), map()) -> {ok, map()} | {error, term()}.
faq_upsert(OrgId, Params) when is_integer(OrgId), OrgId > 0, is_map(Params) ->
    case validate_faq(Params) of
        ok ->
            Impl = cs_ai_port:impl(Params),
            case with_faq_id(OrgId, Params, Impl) of
                {ok, Id} ->
                    case Impl:upsert_faq(OrgId, Params#{id => Id}) of
                        {ok, Row} -> {ok, project_faq(Row)};
                        {error, _} = Err -> Err
                    end;
                {error, _} = Err ->
                    Err
            end;
        {error, _} = Err ->
            Err
    end;
faq_upsert(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

validate_faq(Params) ->
    Question = maps:get(question, Params, undefined),
    Intent = maps:get(intent, Params, <<"other">>),
    case is_binary(Question) andalso Question =/= <<>> of
        false ->
            {error, {invalid_argument, question}};
        true ->
            case valid_intent_bin(Intent) of
                true -> ok;
                false -> {error, {invalid_argument, {intent, Intent}}}
            end
    end.

valid_intent_bin(B) when is_binary(B) ->
    case cs_ai_intent:parse_intent_bin(B) of
        {ok, _} -> true;
        error -> false
    end;
valid_intent_bin(_) ->
    false.

with_faq_id(_OrgId, Params, _Impl) ->
    case maps:get(id, Params, undefined) of
        Id when is_integer(Id), Id > 0 -> {ok, Id};
        _ -> cs_app_support:new_id(ai_cs_faq, Params)
    end.

-spec faq_delete(integer(), map()) -> ok | {error, term()}.
faq_delete(OrgId, Params) when is_integer(OrgId), OrgId > 0, is_map(Params) ->
    case maps:get(id, Params, undefined) of
        Id when is_integer(Id), Id > 0 -> (cs_ai_port:impl(Params)):delete_faq(OrgId, Id);
        Other -> {error, {invalid_argument, {faq_id, Other}}}
    end;
faq_delete(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

%% embedding 向量不进响应；只出"有没有"（后台 UI 要能显示语义检索是否生效）。
project_faq(Row) ->
    maps:put(embedded, maps:get(embedded, Row, false), maps:with(?FAQ_PROJECTION, Row)).

%% ===================================================================
%% 对话记录
%% ===================================================================

-spec sessions(integer(), map()) -> {ok, map()} | {error, term()}.
sessions(OrgId, Params) when is_integer(OrgId), OrgId > 0, is_map(Params) ->
    (cs_ai_port:impl(Params)):list_sessions(OrgId, Params);
sessions(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

-spec session_turns(integer(), map()) -> {ok, [map()]} | {error, term()}.
session_turns(OrgId, Params) when is_integer(OrgId), OrgId > 0, is_map(Params) ->
    (cs_ai_port:impl(Params)):list_turns(OrgId, Params);
session_turns(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

%% ===================================================================
%% 满意度统计
%% ===================================================================

-spec stats(integer(), map()) -> {ok, map()} | {error, term()}.
stats(OrgId, Params) when is_integer(OrgId), OrgId > 0, is_map(Params) ->
    (cs_ai_port:impl(Params)):stats(OrgId, Params);
stats(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.

%% ===================================================================
%% 规则试算（调参用：给定事实，看裁决结果）
%% ===================================================================

%% @doc 用**运行时同一条规则**试算，不查库、不调模型。后台调阈值时用它解释
%% "为什么这句话会被转人工"，避免管理员只能靠猜。
-spec preview(integer(), map()) -> {ok, map()} | {error, term()}.
preview(OrgId, Params) when is_integer(OrgId), OrgId > 0, is_map(Params) ->
    Text = maps:get(text, Params, <<>>),
    Facts = #{
        intent => maps:get(intent, Params, other),
        confidence => maps:get(confidence, Params, 0.0),
        resolved => maps:get(resolved, Params, false),
        faq_score => maps:get(faq_score, Params, 0.0),
        unresolved_turns => maps:get(unresolved_turns, Params, 0),
        explicit_human => cs_ai_intent:contains_any(Text, cs_ai_intent:human_request_keywords()),
        negative => cs_ai_intent:contains_any(Text, cs_ai_intent:negative_keywords()),
        config => maps:get(config, Params, #{})
    },
    {ok, cs_ai_app:decide_only(Facts)};
preview(OrgId, _Params) ->
    {error, {invalid_argument, {organization_id, OrgId}}}.
