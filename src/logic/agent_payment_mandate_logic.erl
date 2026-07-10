-module(agent_payment_mandate_logic).

%%%
% Agent 受控支付授权业务逻辑 / Agent payment mandate authorization logic
% （Phase 4 T4.3 收尾：补上 mandate 的**创建/撤销入口**，消费链见 agent_payment_logic）。
%
% ⚠️ 金钱红线全部落在本层（handler 只透传身份，绝不判权）：
%   ① owner_uid **恒等于调用方 OwnerUid**（REST 取 JWT current_uid），调用方保证；
%      → 用户只能授权从**自己**钱包扣款，绝不能替他人授权。
%   ② 归属校验 owns_agent/2：目标必须是「启用中的 agent」且 ai_agent.owner_uid == OwnerUid，
%      → 只有 agent 的注册所有者能授权它花钱（防劫持他人 agent 的唯一 active mandate 槽位）。
%   ③ 参数边界 validate/6：正整数、单笔 =< 累计、有效期硬上限 90 天、周期下限 60 秒。
%   撤销 revoke/2：只有 mandate.owner_uid 本人能撤（存在性差异统一收敛为 not_authorized，
%      不构成"mandate 是否存在"的探测 oracle）。
%
% 单活约束：uniq_agent_payment_mandate_active(agent_uid) WHERE status=1（迁移 00000029）
%   保证一个 agent 同时最多一个有效 mandate。authorize/2 先撤旧再建，语义="替换为最新授权"。
%   ponytail: 撤旧+建新非单事务，极端并发下两请求可能都撤旧后各插一条 → 后者撞 uniq 索引报错
%     （invalid_params，无资损）。uniq 索引是最终防线；真需严格串行再下沉单条 INSERT ON CONFLICT。
%%%

-export([authorize/2]).
-export([revoke/2]).
-export([get_active/2]).

%% 有效期硬上限：90 天（防呆授权，杜绝"永久有效"或天文数字有效期）
-define(MAX_EXPIRES_SECS, 7776000).
%% 周期窗口下限：60 秒
-define(MIN_PERIOD_SECS, 60).

%% @doc owner 授权 agent 代付。OwnerUid 恒为调用方身份（REST=JWT current_uid）。
%% @returns {ok, #{<<"id">> => integer()}} | {error, atom()}
%%   Reason: invalid_params | not_agent_owner | create 的错误
-spec authorize(integer(), map()) -> {ok, map()} | {error, atom()}.
authorize(OwnerUid, Params) when is_integer(OwnerUid), is_map(Params) ->
    AgentUid = to_int(maps:get(<<"agent_uid">>, Params, 0)),
    MaxAmount = to_int(maps:get(<<"max_amount_fen">>, Params, 0)),
    MaxTotal = to_int(maps:get(<<"max_total_fen">>, Params, 0)),
    ExpiresIn = to_int(maps:get(<<"expires_in_secs">>, Params, 0)),
    PeriodSecs = to_int(maps:get(<<"period_secs">>, Params, 86400)),
    case validate(OwnerUid, AgentUid, MaxAmount, MaxTotal, ExpiresIn, PeriodSecs) of
        ok ->
            %% 红线②：目标必须是启用 agent 且归属 OwnerUid
            case owns_agent(OwnerUid, AgentUid) of
                true ->
                    %% 单活：先撤该 agent 现有有效 mandate（如有），规避 uniq 冲突
                    _ = revoke_active_if_any(AgentUid),
                    do_create(OwnerUid, AgentUid, MaxAmount, MaxTotal, ExpiresIn, PeriodSecs);
                false ->
                    {error, not_agent_owner}
            end;
        Err ->
            Err
    end;
authorize(_, _) ->
    {error, invalid_params}.

%% @doc 撤销授权。只有 mandate.owner_uid 本人能撤。
%% @returns {ok, non_neg_integer()} | {error, not_authorized}
-spec revoke(integer(), integer()) -> {ok, non_neg_integer()} | {error, not_authorized}.
revoke(OwnerUid, MandateId) when is_integer(OwnerUid), is_integer(MandateId) ->
    case agent_payment_mandate_ds:get(MandateId) of
        {ok, M} ->
            case to_int(maps:get(<<"owner_uid">>, M, 0)) =:= OwnerUid of
                true -> normalize_revoke(agent_payment_mandate_ds:revoke(MandateId));
                %% 非本人：与 notfound 统一收敛，不泄露 mandate 存在性
                false -> {error, not_authorized}
            end;
        {error, _} ->
            {error, not_authorized}
    end;
revoke(_, _) ->
    {error, not_authorized}.

%% @doc 查 agent 当前有效授权（须归属 OwnerUid）。用于 owner 查看已授权状态。
%% @returns {ok, map()} | {error, notfound | not_agent_owner}
-spec get_active(integer(), integer()) -> {ok, map()} | {error, notfound | not_agent_owner}.
get_active(OwnerUid, AgentUid) when is_integer(OwnerUid), is_integer(AgentUid) ->
    case owns_agent(OwnerUid, AgentUid) of
        true ->
            case agent_payment_mandate_ds:find_active(AgentUid) of
                {ok, M} -> {ok, M};
                {error, _} -> {error, notfound}
            end;
        false ->
            {error, not_agent_owner}
    end;
get_active(_, _) ->
    {error, not_agent_owner}.

%% ===================================================================
%% Internal
%% ===================================================================

%% 参数边界：正整数、单笔 =< 累计、有效期 (0, 90天]、周期 >= 60s、agent≠owner
-spec validate(integer(), integer(), integer(), integer(), integer(), integer()) ->
    ok | {error, invalid_params}.
validate(OwnerUid, AgentUid, MaxAmt, MaxTotal, ExpiresIn, Period) when
    is_integer(OwnerUid),
    OwnerUid > 0,
    is_integer(AgentUid),
    AgentUid > 0,
    AgentUid =/= OwnerUid,
    is_integer(MaxAmt),
    MaxAmt > 0,
    is_integer(MaxTotal),
    MaxTotal >= MaxAmt,
    is_integer(ExpiresIn),
    ExpiresIn > 0,
    ExpiresIn =< ?MAX_EXPIRES_SECS,
    is_integer(Period),
    Period >= ?MIN_PERIOD_SECS
->
    ok;
validate(_, _, _, _, _, _) ->
    {error, invalid_params}.

%% 归属校验：AgentUid 是启用中 agent 且其 ai_agent.owner_uid == OwnerUid
-spec owns_agent(integer(), integer()) -> boolean().
owns_agent(OwnerUid, AgentUid) ->
    case ai_agent_ds:is_agent(AgentUid) of
        {true, Meta} -> to_int(maps:get(<<"owner_uid">>, Meta, 0)) =:= OwnerUid;
        false -> false
    end.

%% 撤销该 agent 现有有效 mandate（幂等：无有效授权则 no-op）
-spec revoke_active_if_any(integer()) -> ok.
revoke_active_if_any(AgentUid) ->
    case agent_payment_mandate_ds:find_active(AgentUid) of
        {ok, #{<<"id">> := Id}} ->
            _ = agent_payment_mandate_ds:revoke(Id),
            ok;
        _ ->
            ok
    end.

-spec do_create(integer(), integer(), integer(), integer(), integer(), integer()) ->
    {ok, map()} | {error, atom()}.
do_create(OwnerUid, AgentUid, MaxAmount, MaxTotal, ExpiresIn, PeriodSecs) ->
    case
        agent_payment_mandate_ds:create(#{
            owner_uid => OwnerUid,
            agent_uid => AgentUid,
            max_amount_fen => MaxAmount,
            max_total_fen => MaxTotal,
            period_secs => PeriodSecs,
            expires_in_secs => ExpiresIn
        })
    of
        {ok, Id} -> {ok, #{<<"id">> => Id}};
        {error, _} -> {error, create_failed}
    end.

-spec normalize_revoke({ok, non_neg_integer()} | {error, term()}) ->
    {ok, non_neg_integer()} | {error, not_authorized}.
normalize_revoke({ok, N}) -> {ok, N};
normalize_revoke({error, _}) -> {error, not_authorized}.

-spec to_int(term()) -> integer().
to_int(V) when is_integer(V) -> V;
to_int(V) when is_binary(V) ->
    try
        binary_to_integer(V)
    catch
        _:_ -> 0
    end;
to_int(_) ->
    0.
