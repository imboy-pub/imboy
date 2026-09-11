-module(teaching_task_ds).
%%%
% 墨芽教师教学作业事务编排层（MN-TASK-01 / MN-TASK-02，P0-3）
% Teaching task data service：group_task + group_task_assignment 的
% 同一事务创建（零半成品）与持久幂等重放。
%
% 设计：
%   - create/6 生产入口：elib_pg:with_tx 包装 create_in_tx/7；
%     任一 assignment 写失败 → {rollback, {db, _}} → 整单 ROLLBACK，
%     不留半成品（MN-TASK-02 原子性）。
%   - create_in_tx/7 导出给真库集成测试直调（与生产同代码路径）。
%   - 幂等重放命中（created=false）回读原 task 的 assignments 原样返回，
%     不重复写 assignment（replayed=true，行数不增）。
%%%

-export([create/6, create_in_tx/7]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc 教学作业发布（生产入口，MN-TASK-02 同事务 + 持久幂等）
%% Uid 操作者（manager/teacher，logic 已校验）；Learners = [{LearnerId, GuardianUid}]
%% 返回 {ok, #{task_id, assignments, replayed}} | {error, Reason}
-spec create(integer(), integer(), binary(), binary(), map(), [{integer(), integer()}]) ->
    {ok, map()} | {error, idempotency_conflict | idempotency_key_required | db_error}.
create(Uid, GroupId, IdemKey, Digest, TaskFields, Learners) ->
    Tx = fun(Conn) ->
        create_in_tx(Conn, Uid, GroupId, IdemKey, Digest, TaskFields, Learners)
    end,
    case elib_pg:with_tx(Tx, [{reraise, false}]) of
        {ok, _} = Ok ->
            Ok;
        {rollback, idempotency_conflict} ->
            {error, idempotency_conflict};
        {rollback, idempotency_key_required} ->
            {error, idempotency_key_required};
        {rollback, {db, _}} ->
            {error, db_error};
        {error, _Reason} ->
            {error, db_error}
    end.

%% @doc 事务体（集成测试直调；返回 {rollback, Reason} 由外层 with_tx 回滚）
-spec create_in_tx(
    any(), integer(), integer(), binary(), binary(), map(), [{integer(), integer()}]
) ->
    {ok, map()}
    | {rollback, idempotency_conflict | idempotency_key_required | {db, term()}}.
create_in_tx(Conn, Uid, GroupId, IdemKey, Digest, TaskFields, Learners) ->
    case
        teaching_task_repo:create_task_idempotent_tx(Conn, #{
            id => elib_tsid:generate(),
            group_id => GroupId,
            task_id => elib_id:gen(<<"task">>),
            title => maps:get(title, TaskFields),
            description => maps:get(description, TaskFields, <<>>),
            creator_id => Uid,
            deadline => maps:get(deadline, TaskFields, undefined),
            idempotency_key => IdemKey,
            request_digest => Digest
        })
    of
        {ok, #{<<"created">> := true, <<"id">> := Id, <<"task_id">> := TaskId}} ->
            %% v3 P0-1 修复：对外 task_id 统一为 bigint id（内部 varchar 链路保留）
            insert_assignments(Conn, Id, TaskId, Learners);
        {ok, #{<<"created">> := false, <<"id">> := Id, <<"task_id">> := TaskId}} ->
            %% 幂等重放：返回原集合，不重复写 assignment（行数不增）
            replay_assignments(Conn, Id, TaskId);
        {error, idempotency_conflict} ->
            {rollback, idempotency_conflict};
        {error, idempotency_key_required} ->
            {rollback, idempotency_key_required};
        {error, Reason} ->
            {rollback, {db, Reason}}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec insert_assignments(any(), integer(), binary(), [{integer(), integer()}]) ->
    {ok, map()} | {rollback, {db, term()}}.
insert_assignments(Conn, Id, TaskId, Learners) ->
    insert_assignments(Conn, Id, TaskId, Learners, []).

-spec insert_assignments(any(), integer(), binary(), [{integer(), integer()}], [map()]) ->
    {ok, map()} | {rollback, {db, term()}}.
insert_assignments(_Conn, Id, _TaskId, [], Acc) ->
    {ok, payload(Id, lists:reverse(Acc), false)};
insert_assignments(Conn, Id, TaskId, [{LearnerId, GuardianUid} | Rest], Acc) ->
    AssignmentId = elib_tsid:generate(),
    case
        teaching_task_repo:insert_assignment_tx(
            Conn, AssignmentId, TaskId, LearnerId, GuardianUid
        )
    of
        ok ->
            insert_assignments(Conn, Id, TaskId, Rest, [
                #{
                    <<"assignment_id">> => integer_to_binary(AssignmentId),
                    <<"learner_id">> => integer_to_binary(LearnerId)
                }
                | Acc
            ]);
        {error, Reason} ->
            %% 任一失败整单回滚（MN-TASK-02：零半成品）
            {rollback, {db, Reason}}
    end.

-spec replay_assignments(any(), integer(), binary()) ->
    {ok, map()} | {rollback, {db, term()}}.
replay_assignments(Conn, Id, TaskId) ->
    case teaching_task_repo:assignments_of_task_tx(Conn, TaskId) of
        {ok, Rows} ->
            Items = [
                #{
                    <<"assignment_id">> => integer_to_binary(maps:get(<<"id">>, R)),
                    <<"learner_id">> => integer_to_binary(maps:get(<<"learner_id">>, R))
                }
             || R <- Rows
            ],
            {ok, payload(Id, Items, true)};
        {error, Reason} ->
            {rollback, {db, Reason}}
    end.

%% v3 P0-1 修复：对外 task_id = group_task.id 十进制字符串（JSON string，防 64bit 精度丢失）
-spec payload(integer(), [map()], boolean()) -> map().
payload(Id, Assignments, Replayed) ->
    #{
        <<"task_id">> => integer_to_binary(Id),
        <<"assignments">> => Assignments,
        <<"replayed">> => Replayed
    }.
