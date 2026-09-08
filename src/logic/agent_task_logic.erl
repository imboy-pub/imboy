-module(agent_task_logic).

%%%
% Agent Task 业务编排（DATA-01）：FSM-00 九状态矩阵的迁移知识 + 持久化编排。
%
% 职责：
%   ensure_task/1   任务落库（幂等），生成并持有 correlation_id（TRACE-00）；
%   record_event/1  事件落库 + 按边表 CAS 迁移，返回投递指令（重复 durable 事件
%                   不重复投递——agent_task_event.idempotency_key 唯一约束去重）；
%   decide/3        审批：授权判据（agent_task_ds）+ 单事务仲裁（CAS+决定同事务，
%                   first-writer-wins 由 decision.task_id 唯一约束保证）；
%   lookup/1        读任务/决定状态（重启恢复口径：一切以 DB 为真源）。
%
% ⚠️ 状态迁移的合法性与矩阵（FSM-00 冻结）逐边对齐；本模块只编码"目标态→合法
%    源态集合"，动作语义与审批规则的解释权在 FSM-00 契约。
%%%

-export([ensure_task/1, record_event/1, decide/3, lookup/1]).
-export([expire_stale_tasks/1]).
%% 供单测/审计核对：目标态 → 合法源态集合
-export([legal_sources/1]).

-include("log.hrl").

-type status() :: binary().

%% 目标态 → 合法源态（FSM-00 15 条边按目标态归并；approved/rejected 仅审批边，
%% 由 decide/3 走专用事务，不在 record_event 边表内）。
-define(EDGE_MAP, #{
    <<"working">> => [<<"submitted">>, <<"approved">>, <<"working">>],
    <<"awaiting_approval">> => [<<"working">>],
    <<"completed">> => [<<"working">>],
    <<"failed">> => [<<"submitted">>, <<"working">>, <<"approved">>],
    <<"cancelled">> => [<<"submitted">>, <<"working">>, <<"awaiting_approval">>],
    <<"expired">> => [<<"submitted">>, <<"awaiting_approval">>]
}).

-define(DURABLE_STATUSES, [
    <<"awaiting_approval">>,
    <<"rejected">>,
    <<"expired">>,
    <<"completed">>,
    <<"failed">>,
    <<"cancelled">>
]).

%% @doc 任务幂等落库（不存在则建，初始态 submitted）。Event 键：
%%   #{task_id := binary(), agent_uid := integer(), group_id := integer(),
%%     tool => binary(), params_digest => binary()}
%% correlation_id 服务端生成（TRACE-00 §2：corr- 前缀+随机 hex）。
-spec ensure_task(map()) -> {ok, map(), boolean()} | {error, term()}.
ensure_task(Event) ->
    TaskId = task_id(Event),
    case valid_task_id(TaskId) of
        false ->
            {error, invalid_task_id};
        true ->
            Data = #{
                id => TaskId,
                group_id => maps:get(group_id, Event),
                agent_uid => maps:get(agent_uid, Event),
                tool => ec_cnv:to_binary(maps:get(tool, Event, <<>>)),
                params_digest => ec_cnv:to_binary(maps:get(params_digest, Event, <<>>)),
                correlation_id => new_correlation_id(),
                idempotency_key => <<"task:", TaskId/binary>>
            },
            agent_task_repo:ensure_task(Data)
    end.

%% @doc 事件记录 + CAS 迁移。返回投递指令：
%%   {deliver, StatusBin}                     可投递（ephemeral 或首次 durable）
%%   {deliver_with_meta, StatusBin, TaskRow}  durable 且需要任务元数据（卡片）
%%   skip                                     非法迁移/重复 durable/未知状态
%% 恒不抛异常（observe 通道容错，任何失败降级 skip 并记日志）。
-spec record_event(map()) -> skip | {deliver, status()} | {deliver_with_meta, status(), map()}.
record_event(Event) ->
    try do_record_event(Event) of
        R -> R
    catch
        Class:Reason:Stack ->
            ok = ?ERROR_LOG(
                "[AGENT_TASK_LOGIC] record_event ~p:~p~n~p~n",
                [Class, Reason, Stack]
            ),
            skip
    end.

do_record_event(Event) ->
    TaskId = task_id(Event),
    StatusBin = status_bin(maps:get(status, Event, <<>>)),
    case legal_sources(StatusBin) of
        {ok, FromStates} ->
            case agent_task_repo:ensure_task(ensure_data(Event)) of
                {ok, TaskRow, _Created} ->
                    apply_event(TaskId, TaskRow, StatusBin, FromStates, Event);
                {error, Reason} ->
                    ok = ?ERROR_LOG("[AGENT_TASK_LOGIC] ensure_task ~p~n", [Reason]),
                    skip
            end;
        error ->
            %% 未知/不可由 emit 触发的状态（submitted 初值、approved/rejected 审批边）
            skip
    end.

apply_event(TaskId, TaskRow, StatusBin, FromStates, Event) ->
    Corr = maps:get(<<"correlation_id">>, TaskRow),
    Cur = cur_status(TaskRow),
    IsRepeat = (Cur =:= StatusBin) andalso (StatusBin =/= <<"working">>),
    if
        IsRepeat ->
            skip;
        true ->
            R = agent_task_repo:cas_status(TaskId, {FromStates, StatusBin}),
            case R of
                {ok, updated} ->
                    after_transition(TaskId, TaskRow, StatusBin, Corr, Event);
                {ok, not_matched} ->
                    not_matched(Cur, StatusBin);
                {ok, _Other} ->
                    not_matched(Cur, StatusBin);
                {error, _Reason} ->
                    skip
            end
    end.

%% working 自环（progress/working 重发）：状态不变但仍属合法，ephemeral 可重复。
not_matched(_Cur, <<"working">>) -> {deliver, <<"working">>};
not_matched(_Cur, _Target) -> skip.

after_transition(TaskId, TaskRow, StatusBin, Corr, Event) ->
    Durable = lists:member(StatusBin, ?DURABLE_STATUSES),
    EventData = #{
        id => new_id(<<"agent_task_event">>, TaskId, StatusBin),
        task_id => TaskId,
        status => StatusBin,
        correlation_id => Corr,
        idempotency_key => event_idem(TaskId, StatusBin, Durable),
        seq => 0
    },
    %% with_tx 直接返回 fun 值；回滚归一 skip（事件落库失败不投递 durable）
    Ret = agent_task_repo:with_tx(fun(Conn) ->
        agent_task_repo:insert_event_tx(Conn, EventData)
    end),
    case Ret of
        {ok, inserted} when Durable ->
            {deliver_with_meta, StatusBin, enrich(TaskRow, Event)};
        {ok, inserted} ->
            {deliver, StatusBin};
        {ok, duplicate} ->
            %% 重复 durable 事件：事件已记过、消息已投过 → 不重复投递（A03）
            skip;
        {rollback, Reason} ->
            ok = ?ERROR_LOG("[AGENT_TASK_LOGIC] event rollback ~p~n", [Reason]),
            skip;
        {error, Reason} ->
            ok = ?ERROR_LOG("[AGENT_TASK_LOGIC] event ~p~n", [Reason]),
            skip
    end.

%% @doc 审批（approve/reject）。ApproverUid 必须由调用方从已认证会话派生。
%% 返回 {ok, Decision} | {error, not_authorized | already_decided | not_found | internal_error}。
-spec decide(binary(), integer(), approved | rejected) ->
    {ok, approved | rejected, map()}
    | {error, not_authorized | already_decided | not_found | internal_error}.
decide(TaskId, ApproverUid, Decision) when Decision =:= approved; Decision =:= rejected ->
    try
        do_decide(TaskId, ApproverUid, Decision)
    catch
        Class:Reason ->
            ok = ?ERROR_LOG("[AGENT_TASK_DECIDE] task=~p ~p:~p~n", [TaskId, Class, Reason]),
            {error, internal_error}
    end;
decide(_TaskId, _ApproverUid, _Other) ->
    {error, not_authorized}.

do_decide(TaskId, ApproverUid, Decision) ->
    case agent_task_repo:get_task(TaskId) of
        {ok, TaskRow} ->
            AgentUid = to_int(maps:get(<<"agent_uid">>, TaskRow)),
            GroupId = to_int(maps:get(<<"group_id">>, TaskRow)),
            Corr = maps:get(<<"correlation_id">>, TaskRow),
            case agent_task_ds:is_authorized_approver(ApproverUid, GroupId, AgentUid) of
                false ->
                    {error, not_authorized};
                true ->
                    arbitrate(TaskId, TaskRow, ApproverUid, Decision, Corr, AgentUid)
            end;
        {error, not_found} ->
            {error, not_found};
        {error, _} ->
            {error, internal_error}
    end.

%% 单事务仲裁：CAS(awaiting_approval → 决定态) 与决定插入同事务。
%% CAS 先行：抢到迁移权的决定者才有资格插入决定行；并发败者 CAS 0 行 →
%% already_decided / not_found，无任何副作用。DB 故障整体回滚（fail-closed）。
arbitrate(TaskId, TaskRow, ApproverUid, Decision, Corr, AgentUid) ->
    DecisionBin = atom_to_binary(Decision, utf8),
    Tx = fun(Conn) ->
        case
            agent_task_repo:cas_status_tx(
                Conn, TaskId, {[<<"awaiting_approval">>], DecisionBin}
            )
        of
            {ok, updated} ->
                Data = #{
                    id => new_id(<<"agent_task_decision">>, TaskId, DecisionBin),
                    task_id => TaskId,
                    decision => DecisionBin,
                    approver_uid => ApproverUid,
                    correlation_id => Corr
                },
                case agent_task_repo:insert_decision_tx(Conn, Data) of
                    {ok, inserted} ->
                        Meta = #{
                            group_id => to_int(maps:get(<<"group_id">>, TaskRow)),
                            agent_uid => AgentUid
                        },
                        {ok, Decision, Meta};
                    {ok, duplicate} ->
                        {error, already_decided};
                    {error, Reason} ->
                        {error, Reason}
                end;
            {ok, not_matched} ->
                %% 快照可能过期：重读最新状态判定（败者=已决定 or 任务不在待审）
                case agent_task_repo:get_task_tx(Conn, TaskId) of
                    {ok, Fresh} ->
                        case cur_status(Fresh) of
                            <<"approved">> -> {error, already_decided};
                            <<"rejected">> -> {error, already_decided};
                            _ -> {error, not_found}
                        end;
                    {error, not_found} ->
                        {error, not_found};
                    {error, _} = E2 ->
                        E2
                end;
            {error, Reason} ->
                {error, Reason}
        end
    end,
    %% ds:with_tx 直接返回 Tx 的值；{rollback, _} 归一为 internal_error（fail-closed）
    case agent_task_ds:with_tx(Tx) of
        {ok, Decision, Meta} ->
            {ok, Decision, Meta};
        {error, Reason} ->
            {error, Reason};
        {rollback, _Reason} ->
            {error, internal_error}
    end.

%% @doc 读状态（重启恢复口径：真源=DB）。
%% 返回 {pending, GroupId, AgentUid} | {Decision, ApproverUid} | undefined。
-spec lookup(binary()) ->
    {pending, integer(), integer()}
    | {approved | rejected, integer()}
    | {live_status, binary()}
    | undefined.
lookup(TaskId) ->
    case agent_task_repo:get_task(TaskId) of
        {ok, TaskRow} ->
            case cur_status(TaskRow) of
                <<"awaiting_approval">> ->
                    {pending, to_int(maps:get(<<"group_id">>, TaskRow)),
                        to_int(maps:get(<<"agent_uid">>, TaskRow))};
                <<"approved">> ->
                    decision_or(TaskId, undefined);
                <<"rejected">> ->
                    decision_or(TaskId, undefined);
                %% submitted/working 等活跃态是建任务后的常态（EXT-01 实测）：
                %% 无决定行 ≠ 任务不存在，必须回读真实状态，不得与 undefined 混同。
                StatusBin when is_binary(StatusBin) ->
                    decision_or(TaskId, {live_status, StatusBin})
            end;
        {error, _} ->
            undefined
    end.

decision_or(TaskId, Default) ->
    case agent_task_repo:get_decision(TaskId) of
        {ok, #{<<"decision">> := D, <<"approver_uid">> := Uid}} ->
            {binary_to_atom(D, utf8), to_int(Uid)};
        _ ->
            Default
    end.

%% @doc 批量到期：submitted/awaiting_approval 等待人工动作超时 → expired
%% （FSM-00 边 #4/#12，system 语义）。返回到期任务数。
%% GraceSecs 为「进入当前态后」的等待宽限，由 updated_at 判定（FAIL-CLOSED：
%% 只迁状态与写事件，无外部副作用）。
-spec expire_stale_tasks(non_neg_integer()) -> {ok, non_neg_integer()} | {error, term()}.
expire_stale_tasks(GraceSecs) ->
    Cases = [
        {<<"submitted">>, <<"submitted">>},
        {<<"awaiting_approval">>, <<"awaiting_approval">>}
    ],
    Total = lists:foldl(
        fun({From, _Target}, Acc) ->
            case expire_batch(From, GraceSecs) of
                {ok, N} -> Acc + N;
                {error, _} -> Acc
            end
        end,
        0,
        Cases
    ),
    {ok, Total}.

expire_batch(From, GraceSecs) ->
    Sql = iolist_to_binary(
        [
            <<"UPDATE ", (elib_pg_sql:public_tablename(<<"agent_task">>))/binary,
                " SET status = 'expired', updated_at = NOW()"
                " WHERE status = $1"
                " AND updated_at < NOW() - ($2 || ' seconds')::interval"
                " RETURNING id">>
        ]
    ),
    case elib_pg:query(Sql, [From, integer_to_binary(GraceSecs)]) of
        {ok, Rows} when is_list(Rows) -> {ok, length(Rows)};
        {ok, N} when is_integer(N) -> {ok, N};
        {error, Reason} ->
            ?ERROR_LOG("[AGENT_TASK] expire_batch ~p error ~p~n", [From, Reason]),
            {error, Reason}
    end.

%% ===================================================================
%% Internal
%% ===================================================================

%% 投递元数据：任务行核心字段 + 事件自带覆盖（text/e2ee 由 observer 消费）
enrich(TaskRow, Event) ->
    #{
        <<"task_id">> => ec_cnv:to_binary(maps:get(task_id, Event)),
        <<"group_id">> => maps:get(<<"group_id">>, TaskRow),
        <<"agent_uid">> => maps:get(<<"agent_uid">>, TaskRow),
        <<"correlation_id">> => maps:get(<<"correlation_id">>, TaskRow),
        <<"status">> => status_bin(maps:get(status, Event, <<>>)),
        <<"text">> => ec_cnv:to_binary(maps:get(text, Event, <<>>))
    }.

legal_sources(StatusBin) when is_binary(StatusBin) ->
    case maps:find(StatusBin, ?EDGE_MAP) of
        {ok, From} -> {ok, From};
        error -> error
    end;
legal_sources(_) ->
    error.

ensure_data(Event) ->
    %% update 路径可不带 group_id/agent_uid（任务已存在时 ensure 幂等返回）
    T = task_id(Event),
    #{
        id => T,
        group_id => to_int(maps:get(group_id, Event, 0)),
        agent_uid => to_int(maps:get(agent_uid, Event, 0)),
        tool => ec_cnv:to_binary(maps:get(tool, Event, <<>>)),
        params_digest => ec_cnv:to_binary(maps:get(params_digest, Event, <<>>)),
        correlation_id => new_correlation_id(),
        idempotency_key => <<"task:", T/binary>>
    }.

task_id(Event) -> ec_cnv:to_binary(maps:get(task_id, Event, <<>>)).

valid_task_id(TaskId) ->
    Re = <<"^[A-Za-z0-9_-]{16,64}$">>,
    TaskId =/= <<>> andalso
        re:run(TaskId, Re, [{capture, none}]) =:= match.

cur_status(TaskRow) -> ec_cnv:to_binary(maps:get(<<"status">>, TaskRow)).

status_bin(S) when is_atom(S) -> atom_to_binary(S, utf8);
status_bin(S) when is_binary(S) -> S;
status_bin(_) -> <<>>.

%% durable 事件幂等键=任务+状态（天然唯一）；ephemeral 事件用每调用唯一键。
event_idem(TaskId, StatusBin, true) ->
    <<"evt:", TaskId/binary, ":", StatusBin/binary>>;
event_idem(TaskId, StatusBin, false) ->
    N = erlang:unique_integer([positive, monotonic]),
    iolist_to_binary(["evtx:", TaskId, ":", StatusBin, ":", integer_to_binary(N)]).

new_id(Prefix, TaskId, Tag) ->
    N = erlang:unique_integer([positive, monotonic]),
    iolist_to_binary([
        Prefix,
        "-",
        integer_to_binary(N),
        "-",
        integer_to_binary(erlang:phash2({TaskId, Tag}))
    ]).

new_correlation_id() ->
    Bin = binary:encode_hex(crypto:strong_rand_bytes(16), lowercase),
    <<"corr-", Bin/binary>>.

to_int(V) when is_integer(V) -> V;
to_int(V) when is_binary(V) ->
    try binary_to_integer(V) of
        I -> I
    catch
        _:_ -> 0
    end;
to_int(_) ->
    0.
