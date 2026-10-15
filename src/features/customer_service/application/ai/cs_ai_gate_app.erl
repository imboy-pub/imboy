%%% @doc D 组（v4 02-D）证据 gate：七道放行 gate（合取式）工厂、mode 解析
%%% 与离线指标（false_confidence 分桶）。自 `cs_ai_app` 按域拆出
%%% （文件行数规范收口，2026-10-15）；函数体与语义未改。
-module(cs_ai_gate_app).

-moduledoc "智能客服 AI 证据 gate（D 组：七道放行合取 / mode 解析 / 离线指标）。".

-export([grounding_gate/2, false_confidence_buckets/2, mode_of/1]).

%% ===================================================================
%% 七道放行 gate（合取式）
%% ===================================================================

%% @doc 证据 gate 工厂（P0-3 delivery_hook 的 gate_fun 注入位）。
%%
%% 合取式（02-D.3；gate_2 handoff_clear 在 deliver preflight、gate_6
%% budget_admitted 属 P0-6，不在此处）：
%%   gate_1 mode_ok      — auto_reply_mode：enabled 直接过；canary 需白名单
%%                         覆盖（cs_pg_ai:canary_covers）；disabled/shadow 拒
%%   gate_3 faq_require_hit — 配置为真时 cited_faq_ids 非空，否则 no_grounding
%%   gate_4 citations_current — 引用版本/状态/有效期与投递时刻一致（I10）
%%   gate_5 answerable   — 无据答复不投递（I9 的 gate 侧）
%%   gate_7 session_open — 会话 status ∈ {queued, active}
%%
%% Evidence：`cited_faq_ids` / `cited_faq_versions` / `answerable`。
%% 任一 gate 不成立 ⇒ `{error, {skipped, Reason}}`（B.4 冻结语义，
%% **禁止静默丢弃**；自报置信度（D-5 观测字段）**不出现在任何 gate**——
%% D-5，仅作观测与影子校准）。
-spec grounding_gate(integer(), map()) ->
    fun((term(), map()) -> ok | {error, term()}).
grounding_gate(OrgId, Evidence) when is_integer(OrgId), is_map(Evidence) ->
    fun(Conn, Job) ->
        Impl = cs_ai_port:impl(#{}),
        %% gate 只消费 v4 列（auto_reply_mode/faq_require_hit），走 gate 窄读取；
        %% v1 fetch_config 的字段对齐属 P0-8，不在此处混改。
        case Impl:fetch_gate_config(OrgId) of
            {ok, Config} ->
                evaluate_gates(Conn, Impl, OrgId, Config, Job, Evidence);
            {error, not_found} ->
                {error, {skipped, superseded}};
            {error, Reason} ->
                {error, Reason}
        end
    end;
grounding_gate(_OrgId, _Evidence) ->
    fun(_Conn, _Job) -> {error, {skipped, superseded}} end.

evaluate_gates(Conn, Impl, OrgId, Config, Job, Evidence) ->
    %% gate_1 mode_ok
    case mode_gate(Impl, OrgId, Config, Job) of
        ok ->
            %% gate_3 faq_require_hit
            Cited = maps:get(cited_faq_ids, Evidence, []),
            CitedVersions = maps:get(cited_faq_versions, Evidence, []),
            case maps:get(faq_require_hit, Config, true) andalso Cited =:= [] of
                true ->
                    {error, {skipped, no_grounding}};
                false ->
                    %% gate_4 citations_current
                    case Impl:citations_current(Conn, OrgId, Cited, CitedVersions) of
                        {ok, true} ->
                            %% gate_5 answerable
                            case maps:get(answerable, Evidence, false) of
                                true ->
                                    %% gate_7 session_open
                                    case
                                        Impl:session_open_tx(
                                            Conn, OrgId, maps:get(session_id, Job)
                                        )
                                    of
                                        {ok, true} -> ok;
                                        {ok, false} -> {error, {skipped, superseded}};
                                        {error, Reason} -> {error, Reason}
                                    end;
                                false ->
                                    {error, {skipped, no_grounding}}
                            end;
                        {ok, false} ->
                            %% I10：引用过期/停更 ⇒ stale_citation（B.4 冻结
                            %% 语义；job 侧映射 cancelled，细分 last_error_code）。
                            {error, {skipped, stale_citation}};
                        {error, Reason} ->
                            {error, Reason}
                    end
            end;
        {error, _} = Err ->
            Err
    end.

mode_gate(Impl, OrgId, Config, Job) ->
    case mode_of(Config) of
        enabled ->
            ok;
        canary ->
            case
                Impl:canary_covers(OrgId, maps:get(workspace_id, Job), maps:get(session_id, Job))
            of
                {ok, true} -> ok;
                {ok, false} -> {error, {skipped, superseded}};
                {error, Reason} -> {error, Reason}
            end;
        _ ->
            {error, {skipped, superseded}}
    end.

%% @doc auto_reply_mode 归一（atom | binary → atom；未知/缺失 ⇒ disabled）。
%% 主用例（cs_ai_app:run）与 gate_1 共用本解析。
-spec mode_of(map()) -> enabled | canary | shadow | disabled.
mode_of(Config) ->
    case maps:get(auto_reply_mode, Config, undefined) of
        M when is_atom(M) -> M;
        M when is_binary(M) -> binary_to_atom(M, utf8);
        _ -> disabled
    end.

%% ===================================================================
%% 离线指标（D.4.2）
%% ===================================================================

%% @doc false_confidence_rate 分桶（D.4.2，离线指标；不进热路径）。
%%
%% Samples：`[#{intent => binary(), faq_hit => boolean(),
%%              confidence => float(), support => boolean()}]`——support 是
%% 人工复核判定（姐姐复审环节天然产生）。
%% 返回分桶表：`[#{intent, faq_hit, n, support_rate}]`（n < MinSamples 的桶
%% 不输出——MIN_SAMPLES 是假定值，G4 重估前不得用于放行）。
-spec false_confidence_buckets([map()], pos_integer()) -> [map()].
false_confidence_buckets(Samples, MinSamples) when is_list(Samples), is_integer(MinSamples) ->
    Grouped = lists:foldl(
        fun(S, Acc) ->
            Key = {maps:get(intent, S, <<>>), maps:get(faq_hit, S, false) =:= true},
            maps:update_with(Key, fun(L) -> [S | L] end, [S], Acc)
        end,
        #{},
        Samples
    ),
    [
        #{
            intent => Intent,
            faq_hit => Hit,
            n => length(Members),
            support_rate => support_rate_of(Members)
        }
     || {{Intent, Hit}, Members} <- maps:to_list(Grouped),
        length(Members) >= MinSamples
    ];
false_confidence_buckets(_Samples, _MinSamples) ->
    [].

support_rate_of(Members) ->
    Supported = length([ok || #{support := true} <- Members]),
    case length(Members) of
        0 -> 0.0;
        N -> Supported / N
    end.
