%%% @doc E 组（v4 03-E）预算准入生命周期用例：三层准入 / 结算 / 释放 /
%%% uncertain / reconcile 的编排与参数形状校验。自 `cs_ai_app` 按域拆出
%%% （文件行数规范收口，2026-10-15）；函数体与语义未改。
%%%
%%% 与 A/B 组不同，本域数据面为**直接模块调用** `cs_pg_ai:budget_*`
%%% （预算账本无测试注入需求；provider 查询在事务外，慢 I/O 铁律）。
-module(cs_ai_budget_app).

-moduledoc "智能客服 AI 预算准入生命周期（E 组：admit/settle/release/uncertain/reconcile）。".

-export([admit/2, settle/3, release/2, to_uncertain/2, reconcile/0]).

%% @doc 三层准入（E.4）。Params：`session_id` / `job_id` / `attempt` /
%% `est_tokens` 必填。返回 `{ok, reserved | already_admitted}`；
%% `{error, {budget_exceeded, Layer}}` 不重试、按配置降级；
%% `{error, {attempt_state_conflict, Layer}}` 表示该 attempt 已终结。
-spec admit(integer(), map()) ->
    {ok, reserved | already_admitted}
    | {error, {budget_exceeded, session | org | global}}
    | {error, {attempt_state_conflict, session | org | global}}
    | {error, term()}.
admit(OrgId, Params) when is_integer(OrgId), OrgId > 0, is_map(Params) ->
    SessionId = maps:get(session_id, Params, undefined),
    JobId = maps:get(job_id, Params, undefined),
    Attempt = maps:get(attempt, Params, undefined),
    Est = maps:get(est_tokens, Params, undefined),
    case
        is_integer(SessionId) andalso is_integer(JobId) andalso
            is_integer(Attempt) andalso is_integer(Est) andalso Est > 0
    of
        true -> cs_pg_ai:budget_admit(OrgId, SessionId, JobId, Attempt, Est);
        false -> {error, {invalid_argument, admit_params}}
    end;
admit(_OrgId, _Params) ->
    {error, {invalid_argument, admit}}.

%% @doc E.5 结算（I8 幂等）：重复结算只生效一次。
-spec settle(integer(), binary(), non_neg_integer()) ->
    {ok, settled}
    | {skipped, already_settled | {not_reserved, binary()}}
    | {error, term()}.
settle(OrgId, AttemptId, ActualTokens) when
    is_integer(OrgId), OrgId > 0, is_binary(AttemptId), is_integer(ActualTokens), ActualTokens >= 0
->
    cs_pg_ai:budget_settle(OrgId, AttemptId, ActualTokens);
settle(_OrgId, _AttemptId, _ActualTokens) ->
    {error, {invalid_argument, settle}}.

%% @doc E.3 release（参数形状与 04 步骤的 release/1 差一个 OrgId 前参：
%% 铁律 6 租户首参贯穿；数据面 SQL 同时带 organization_id 守卫）。
-spec release(integer(), binary()) ->
    {ok, released} | {skipped, term()} | {error, term()}.
release(OrgId, AttemptId) when is_integer(OrgId), OrgId > 0, is_binary(AttemptId) ->
    cs_pg_ai:budget_release(OrgId, AttemptId);
release(_OrgId, _AttemptId) ->
    {error, {invalid_argument, release}}.

%% @doc E.3 to_uncertain（provider 无响应/超时路径；budget 不动，E-7）。
-spec to_uncertain(integer(), binary()) ->
    {ok, uncertain} | {skipped, term()} | {error, term()}.
to_uncertain(OrgId, AttemptId) when is_integer(OrgId), OrgId > 0, is_binary(AttemptId) ->
    cs_pg_ai:budget_to_uncertain(OrgId, AttemptId);
to_uncertain(_OrgId, _AttemptId) ->
    {error, {invalid_argument, to_uncertain}}.

%% @doc E.6 reconcile：provider 查询在事务外（慢 I/O 铁律）；无 provider
%% 计量集成时默认 unknown ⇒ uncertain 到期行一律 released（保守收口）。
-spec reconcile() -> {ok, [map()]} | {error, term()}.
reconcile() ->
    cs_pg_ai:budget_reconcile().
