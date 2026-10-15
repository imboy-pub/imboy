%%% @doc 智能客服持久化 + LLM 的扩展点契约（铁律 9：只声明 `-callback`，零实现）。
%%%
%%% 为什么是**新端口**而不是塞进 `cs_store_port`：
%%%   * `cs_store_port` 的 callback 清单是**冻结契约**，由 `cs_ports:contracts()`
%%%     与 `cs_closure` 测试双向锁定；AI 是新能力，不应把既有冻结面拉长；
%%%   * AI 的读写面有明确边界（配置 / FAQ / 会话 AI 状态 / 逐轮审计 / 统计），
%%%     独立端口让"能不能替换成别的存储"这个问题有单一答案。
%%%
%%% 铁律 6（租户作用域显式贯穿）：**每个** callback 的首个业务参数是
%%% `OrganizationId`，实现侧 SQL 必须同语句带 `organization_id` 约束。
%%%
%%% 注入面（测试/联调）：`Params` 里的 `ai_store` 键覆盖实现模块，见 `impl/1`。
-module(cs_ai_port).

-moduledoc "智能客服持久化 / LLM 扩展点契约（铁律 9）。".

-export([impl/1, default_impl/0, contracts/0]).
-export_type([faq_row/0, config_row/0, session_state/0, turn_row/0]).

-type faq_row() :: map().
-type config_row() :: map().
-type session_state() :: map().
-type turn_row() :: map().

%% ===================================================================
%% 配置
%% ===================================================================

%% @doc 取组织级 AI 坐席配置；未配置 → `{error, not_found}`（不是默认开）。
-callback fetch_config(OrganizationId :: integer()) ->
    {ok, config_row()} | {error, not_found | term()}.

%% @doc 建/改配置（幂等 upsert）。`Params` 至少含 `id`；一 Org 一配置。
-callback upsert_config(OrganizationId :: integer(), Params :: map()) ->
    {ok, config_row()} | {error, term()}.

%% ===================================================================
%% FAQ
%% ===================================================================

%% @doc FAQ 分页列表（管理面）。
-callback list_faq(OrganizationId :: integer(), Params :: map()) ->
    {ok, [faq_row()]} | {error, term()}.

%% @doc FAQ 建/改（幂等 upsert）。
-callback upsert_faq(OrganizationId :: integer(), Params :: map()) ->
    {ok, faq_row()} | {error, term()}.

%% @doc FAQ 真删。
-callback delete_faq(OrganizationId :: integer(), FaqId :: integer()) ->
    ok | {error, term()}.

%% @doc 词法检索（pg_jieba 全文）。实现必须**在 PG 侧分词**再组 tsquery：
%% 中文 `plainto_tsquery` 是 AND 语义，会漏掉「我想退款」这类多词查询。
-callback search_faq_lexical(
    OrganizationId :: integer(), Query :: binary(), Limit :: pos_integer()
) ->
    {ok, [{FaqId :: integer(), RawScore :: float()}]} | {error, term()}.

%% @doc 语义检索（pgvector 余弦）。未配置 embedding provider 时由 application
%% **不调用**本 callback；实现遇到全 NULL embedding 列应返回空集而不是报错。
-callback search_faq_vector(
    OrganizationId :: integer(), Vector :: [float()], Limit :: pos_integer()
) ->
    {ok, [{FaqId :: integer(), Similarity :: float()}]} | {error, term()}.

%% @doc 按 id 取 FAQ 正文档（检索返回的 id → 正文，供 prompt 注入）。
-callback fetch_faq(OrganizationId :: integer(), FaqIds :: [integer()]) ->
    {ok, [faq_row()]} | {error, term()}.

%% @doc 命中计数累加（检索质量的可观测性；失败不阻断主流程）。
-callback bump_faq_hits(OrganizationId :: integer(), FaqIds :: [integer()]) ->
    ok | {error, term()}.

%% ===================================================================
%% 会话 AI 状态
%% ===================================================================

%% @doc 取会话 AI 状态；无行 → `{error, not_found}`（首次交互尚未发生）。
-callback fetch_session_state(
    OrganizationId :: integer(), WorkspaceId :: integer(), SessionId :: integer()
) ->
    {ok, session_state()} | {error, not_found | term()}.

%% @doc 单调推进会话 AI 状态。实现必须保证：
%%   * `ai_turns` **只增**（累加，不覆盖）；
%%   * `handoff` **粘性**（一旦为 true 不得被后续调用改回 false）；
%%   * `unresolved_turns` 由调用方给绝对值（它需要能被"本轮已解决"清零）。
-callback upsert_session_state(
    OrganizationId :: integer(),
    WorkspaceId :: integer(),
    SessionId :: integer(),
    Params :: map()
) ->
    {ok, session_state()} | {error, term()}.

%% ===================================================================
%% 逐轮审计
%% ===================================================================

%% @doc 追加一条 AI 决策审计。`seq` 由实现**在 SQL 内**生成
%% （`coalesce(max(seq),0)+1`），避免 check-then-insert 竞态。
-callback append_turn(
    OrganizationId :: integer(),
    WorkspaceId :: integer(),
    SessionId :: integer(),
    Params :: map()
) ->
    {ok, turn_row()} | {error, term()}.

%% @doc 某会话的逐轮审计（管理面会话详情；时间升序）。
-callback list_turns(OrganizationId :: integer(), Params :: map()) ->
    {ok, [turn_row()]} | {error, term()}.

%% ===================================================================
%% 管理面
%% ===================================================================

%% @doc 对话记录（含 AI 视角：意图/轮数/是否转人工）。
-callback list_sessions(OrganizationId :: integer(), Params :: map()) ->
    {ok, #{sessions := [map()], total := non_neg_integer()}} | {error, term()}.

%% @doc 满意度与 AI 效能统计（复用 `customer_service_session.rating` 真源）。
-callback stats(OrganizationId :: integer(), Params :: map()) ->
    {ok, map()} | {error, term()}.

%% ===================================================================
%% 装配（`-callback` 之后才能出现函数定义）
%% ===================================================================

%% @doc 默认实现（随 customer_service feature 发布的内置实现，非 Plugin）。
-spec default_impl() -> module().
default_impl() -> cs_pg_ai.

%% @doc 取本次调用使用的实现：`Params` 的 `ai_store` 键优先（测试注入面）。
-spec impl(map()) -> module().
impl(Params) when is_map(Params) ->
    case maps:get(ai_store, Params, undefined) of
        Mod when is_atom(Mod), Mod =/= undefined -> Mod;
        _ -> default_impl()
    end;
impl(_) ->
    default_impl().

%% @doc 冻结的 callback 契约（供契约测试逐项核对 `behaviour_info(callbacks)`）。
-spec contracts() -> [{atom(), arity()}].
contracts() ->
    [
        {fetch_config, 1},
        {upsert_config, 2},
        {list_faq, 2},
        {upsert_faq, 2},
        {delete_faq, 2},
        {search_faq_lexical, 3},
        {search_faq_vector, 3},
        {fetch_faq, 2},
        {bump_faq_hits, 2},
        {fetch_session_state, 3},
        {upsert_session_state, 4},
        {append_turn, 4},
        {list_turns, 2},
        {list_sessions, 2},
        {stats, 2}
    ].
