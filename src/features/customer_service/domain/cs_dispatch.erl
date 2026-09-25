%%% @doc 客服派单（dispatch）的领域纯函数。
%%%
%%% 依据：plan v4.1 §4.2、§5.2、CS-01-A02（claim 恰一个成功且不超 max_concurrent）、
%%% 附带能力「dispatch（least-active 选 seat）」「suspend seat（enabled=false 即拒）」、
%%% CS-BE-05 / CS-RUNTIME-02（自动派单只选 presence 派生 `online` 的坐席）。
%%%
%%% 纯净性（铁律 4）：无 I/O、无库读；坐席快照（含 `active_count`）由调用方
%%% 显式传入，本模块只做选择与容量判定。真正的 DB 级并发裁决在
%%% `cs_pg_session:claim/4`（seat 行锁内 CAS），本模块是同一判定的 domain 真源。
%%%
%%% presence 接入（CS-BE-05，渐进式契约）：seat 快照可选键 `derived_status`
%%% （由 `cs_presence:annotate/3` 注入的运行态派生值）。键**存在**时，仅
%%% `online` 的坐席参与自动派单（away/busy/offline 跳过——busy 另由容量
%%% 条件兜底，语义双保险）；键**缺失**时行为与历史版本逐字节一致
%%% （旧调用方 / 既有测试不传该键，零破坏）。
-module(cs_dispatch).

-export([
    select_seat/2,
    claim_allowed/2,
    capacity_left/1
]).

-type seat() :: #{
    organization_id := integer(),
    business_identity_id := integer(),
    enabled := boolean(),
    max_concurrent := pos_integer(),
    active_count := non_neg_integer(),
    %% CS-BE-05（可选）：cs_presence:derive/2 注入的运行态派生值。
    derived_status => online | away | busy | offline
}.

-export_type([seat/0]).

%% ===================================================================
%% least-active 选择
%% ===================================================================

%% @doc 在 Org 的坐席快照里选一个可接单坐席：
%%   * 只考虑本 Org（`organization_id =:= OrgId`，跨 Org 行直接忽略）；
%%   * 只考虑 `enabled = true`（suspend 即时不可派单）；
%%   * 快照带 `derived_status` 键时（CS-BE-05）：只考虑派生 `online` 的坐席
%%     （away/busy/offline 不参与自动派单；无键行为不变）；
%%   * 只考虑 `active_count < max_concurrent`（上限内）；
%%   * 在候选中取 active_count 最小者；平局取 business_identity_id 最小
%%     （确定性，两个 idle 坐席的选择不依赖遍历顺序）。
%% 无候选 → `{error, no_seat_available}`（上层保持会话 queued）。
-spec select_seat(integer(), [seat()]) -> {ok, integer()} | {error, no_seat_available}.
select_seat(OrgId, Seats) when is_integer(OrgId), is_list(Seats) ->
    Candidates = [
        S
     || S <- Seats,
        maps:get(organization_id, S, undefined) =:= OrgId,
        maps:get(enabled, S, false) =:= true,
        presence_online(S),
        maps:get(active_count, S, 0) < maps:get(max_concurrent, S, 0)
    ],
    case Candidates of
        [] ->
            {error, no_seat_available};
        _ ->
            Sorted = lists:sort(
                fun(A, B) ->
                    {maps:get(active_count, A), maps:get(business_identity_id, A)} =<
                        {maps:get(active_count, B), maps:get(business_identity_id, B)}
                end,
                Candidates
            ),
            {ok, maps:get(business_identity_id, hd(Sorted))}
    end;
select_seat(_OrgId, _Seats) ->
    {error, no_seat_available}.

%% CS-BE-05：presence 派生在线过滤——仅在快照带 `derived_status` 键时生效
%% （渐进式契约：无键 = 旧调用方，行为与历史版本一致）。
presence_online(S) ->
    case maps:get(derived_status, S, undefined) of
        undefined -> true;
        online -> true;
        _Other -> false
    end.

%% ===================================================================
%% claim 容量判定（A02）
%% ===================================================================

%% @doc 判定该坐席当前还能否接一个新会话：
%%   * `enabled = false` → `{error, seat_disabled}`（suspend 即时拒绝新 claim）；
%%   * `ActiveCount >= max_concurrent` → `{error, seat_at_capacity}`；
%%   * 否则 `ok`。
%%
%% 本判定在 DB 侧由 claim 事务在 seat 行锁内复核（检查与写入同锁，竞态关闭）。
-spec claim_allowed(seat(), non_neg_integer()) -> ok | {error, seat_disabled | seat_at_capacity}.
claim_allowed(Seat, ActiveCount) when is_map(Seat), is_integer(ActiveCount) ->
    case maps:get(enabled, Seat, false) of
        false ->
            {error, seat_disabled};
        true ->
            case ActiveCount >= maps:get(max_concurrent, Seat, 0) of
                true -> {error, seat_at_capacity};
                false -> ok
            end
    end;
claim_allowed(_Seat, _ActiveCount) ->
    {error, seat_disabled}.

%% @doc 剩余可接单数；配置下调导致超限时按 0 处理（容量永不为负）。
-spec capacity_left(seat()) -> non_neg_integer().
capacity_left(Seat) when is_map(Seat) ->
    Left = maps:get(max_concurrent, Seat, 0) - maps:get(active_count, Seat, 0),
    case Left > 0 of
        true -> Left;
        false -> 0
    end;
capacity_left(_Seat) ->
    0.
