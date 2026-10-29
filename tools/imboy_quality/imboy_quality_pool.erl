%% IMBOY_ERLANG_QUALITY P1-06：资源感知有界分片（纯 OTP）。
%% schedule/2 为确定性纯函数：LPT（预计耗时降序分配到最早可用 worker 槽），
%% 同名资源（DB/端口/全局角色/mock 模块）冲突的 root 不进同一槽
%% （SCHED01/02/03/04）。execute/3 在有界 peer worker 组中执行；失败 root
%% 不丢其他 root 的结果（SCHED05）；根为模块原子单位，根内 EUnit 语义
%% 不扁平化（SCHED07）。
-module(imboy_quality_pool).
-export([schedule/2, execute/3]).

%% Root :: #{id := term(), test_mod := module(),
%%           estimated_ms := non_neg_integer(), resources := [term()]}.

%% ---- 纯调度：同输入同输出（可重放） ----

-spec schedule([map()], pos_integer()) -> {ok, [[map()]]}.
schedule(Roots, WorkerCount) when
    is_list(Roots), is_integer(WorkerCount), WorkerCount >= 1
->
    Ids = [maps:get(id, R) || R <- Roots],
    true = length(Ids) =:= length(lists:usort(Ids)) orelse error(duplicate_root_id),
    %% SCHED01：慢任务优先（estimated 降序，同耗按 id 稳定序）；未知成本
    %% 由调用方给保守估计，调度不把无数据当零成本。
    Sorted = lists:sort(
        fun(A, B) -> sort_key(A) =< sort_key(B) end, Roots
    ),
    assign(Sorted, [{I, 0, []} || I <- lists:seq(1, WorkerCount)]).

sort_key(R) -> {-maps:get(estimated_ms, R, 5000), maps:get(id, R)}.

assign([], Slots) ->
    Ordered = lists:sort(Slots),
    {ok, [lists:reverse(Assigned) || {_, _, Assigned} <- Ordered]};
assign([Root | Rest], Slots) ->
    Resources = maps:get(resources, Root, []),
    Est = maps:get(estimated_ms, Root, 5000),
    case find_slot(Slots, Resources) of
        {ok, Idx} ->
            assign(Rest, bump(Slots, Idx, Root, Est));
        conflict ->
            %% 全部槽与独占资源冲突：追加专用槽（组内串行）
            assign(Rest, [{length(Slots) + 1, Est, [Root]} | Slots])
    end.

find_slot(Slots, Resources) ->
    Ordered = lists:sort(
        fun({A, La, _}, {B, Lb, _}) ->
            {La, A} =< {Lb, B}
        end,
        Slots
    ),
    find(Ordered, Resources).

find([], _Resources) ->
    conflict;
find([{Idx, _, Assigned} | Rest], Resources) ->
    Used = lists:append([maps:get(resources, R, []) || R <- Assigned]),
    case lists:any(fun(Res) -> lists:member(Res, Used) end, Resources) of
        false -> {ok, Idx};
        true -> find(Rest, Resources)
    end.

bump(Slots, Idx, Root, Est) ->
    [
        case I of
            Idx -> {I, Load + Est, [Root | Assigned]};
            _ -> {I, Load, Assigned}
        end
     || {I, Load, Assigned} <- Slots
    ].

%% ---- 有界执行 ----

%% Opts :: #{code_path := string(), worker_count := pos_integer(),
%%           schedulers := pos_integer(), deadline_ms := pos_integer(),
%%           group_timeout_ms := pos_integer()}.
%% Agg :: events 协议状态（imboy_quality_events:new(RootIds)）。
%% 返回 {AggFinal, FailedRoots, Results}；失败 root 不进入 finish 协议，
%% 单独登记（整体由调用方判失败），成功 root 推进事件协议。
execute(Roots, Opts, Agg0) ->
    WorkerCount = maps:get(worker_count, Opts, 2),
    {ok, Groups} = schedule(Roots, WorkerCount),
    Parent = self(),
    Ref = make_ref(),
    Pids = [
        spawn(fun() -> run_group(G, Opts, Parent, Ref) end)
     || G <- Groups, G =/= []
    ],
    Timeout = maps:get(group_timeout_ms, Opts, 3600000),
    {Agg, Failed, Results, _Left} =
        collect(Pids, Ref, Agg0, #{}, #{}, length(Pids), Timeout),
    {Agg, maps:keys(Failed), Results}.

run_group([], _Opts, Parent, Ref) ->
    Parent ! {group_done, Ref, self()};
run_group([Root | Rest], Opts, Parent, Ref) ->
    Mod = maps:get(test_mod, Root),
    T0 = erlang:monotonic_time(millisecond),
    Result = imboy_quality_runner:run_in_worker(
        Mod, Opts, maps:get(deadline_ms, Opts, 60000)
    ),
    RootMs = erlang:monotonic_time(millisecond) - T0,
    Parent ! {root_result, Ref, maps:get(id, Root), Result#{root_wall_ms => RootMs}},
    run_group(Rest, Opts, Parent, Ref).

%% 计数驱动：全部 group_done 收齐后返回；root_result 任意时刻到达。
collect(_Pids, Ref, Agg, Failed, Results, Remaining, Timeout) when
    Remaining =< 0
->
    _ = Ref,
    {Agg, Failed, Results, 0};
collect(Pids, Ref, Agg, Failed, Results, Remaining, Timeout) ->
    receive
        {root_result, Ref, RootId, #{exit := ok, counts := Counts} = Result} ->
            Started = imboy_quality_events:event({start, RootId}, Agg),
            Finished = imboy_quality_events:event(
                {finish, RootId, pass, maps:get(pass, Counts, 0), ok}, Started
            ),
            collect(
                Pids,
                Ref,
                Finished,
                Failed,
                Results#{RootId => Result},
                Remaining,
                Timeout
            );
        {root_result, Ref, RootId, Result} ->
            %% 失败 root：已开始但无成功终结——事件协议如实保留缺口，
            %% 单独登记 Failed（SCHED05：整体失败，成功结果不丢）。
            Started = imboy_quality_events:event({start, RootId}, Agg),
            collect(
                Pids,
                Ref,
                Started,
                Failed#{RootId => Result},
                Results#{RootId => Result},
                Remaining,
                Timeout
            );
        {group_done, Ref, _Pid} ->
            collect(Pids, Ref, Agg, Failed, Results, Remaining - 1, Timeout)
    after Timeout ->
        {Agg, Failed#{pool_group_timeout => Timeout}, Results, 0}
    end.
