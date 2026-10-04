-module(moya_task_repo).
-moduledoc "墨芽教师教学作业仓库层（MN-TASK-01 / MN-TASK-02，P0-3）。".
%%%
% 墨芽教师教学作业仓库层（MN-TASK-01 / MN-TASK-02，P0-3）
% Teaching task repository：group_task 幂等创建、教学 assignment 写入、
% 老师作业列表统计、learner assignment_ready 校验。
%
% 设计（P0-3）：
%   - 幂等真源 = 00000106 持久列 group_task.(idempotency_key, request_digest)
%     + 部分唯一索引 uk_group_task_idempotency(creator_id, group_id, idempotency_key)；
%     ON CONFLICT DO NOTHING + 回读 + digest 比对（配方同 00000098
%     homework_submission，同 key 不同 digest 由应用层判 5460，DB 不参与唯一性）。
%   - 列表只聚合"教学作业"（存在 learner_id IS NOT NULL 的 assignment），
%     0 提交的新作业必须可见（不得从 review queue 反派生）。
%   - SQL 全参数化；行 map 键为 binary（epgsql column.name）。
%%%

-export([
    create_task_idempotent_tx/2,
    insert_assignment_tx/5,
    assignments_of_task_tx/2,
    list_tasks/4,
    list_tasks_tx/5,
    count_tasks/2,
    count_tasks_tx/3,
    active_staff_group_ids/1,
    active_staff_group_ids_tx/2,
    learner_readiness/3,
    learner_readiness_tx/4
]).

-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

%%%===================================================================
%%% API
%%%===================================================================

-spec tablename(binary()) -> binary().
tablename(Tb) ->
    elib_pg_sql:public_tablename(Tb).

%% ------------------------------------------------------------------
%% MN-TASK-02 配方：group_task 幂等创建（插入或回读同一行；digest 不一致 → 5460）
%% 返回 {ok, #{id, task_id, request_digest, created}} —— created=本次新插入
%% 缺幂等键（空/非 binary）→ {error, idempotency_key_required}（不落库）
%% ------------------------------------------------------------------

-spec create_task_idempotent_tx(any(), map()) ->
    {ok, map()} | {error, idempotency_conflict | idempotency_key_required | term()}.
create_task_idempotent_tx(Conn, #{
    id := Id,
    group_id := GroupId,
    task_id := TaskId,
    title := Title,
    description := Description,
    creator_id := CreatorId,
    deadline := Deadline,
    idempotency_key := IdemKey,
    request_digest := Digest
}) when
    is_integer(Id),
    is_integer(GroupId),
    is_binary(TaskId),
    is_binary(Title),
    is_integer(CreatorId),
    is_binary(IdemKey),
    byte_size(IdemKey) > 0,
    is_binary(Digest)
->
    Now = elib_dt:now(),
    DeadlineParam =
        case Deadline of
            D when is_binary(D), D =/= <<>> -> D;
            _ -> null
        end,
    %% 两段式（v3：修复并发同 key 快照窗口）——原单语句 CTE 的
    %% UNION ALL 回读分支与 INSERT 共用语句级快照：并发另一事务持同 key
    %% 未提交时 ON CONFLICT 等锁提交后，回读分支仍看不到新行 → {ok, []}
    %% → insert_failed(500)。拆为两条语句：第二条回读拿新快照，必然可见
    %% 已提交的原行（同 key 三元组由部分唯一索引保证唯一）。
    Sql1 = <<
        "INSERT INTO ",
        (tb(group_task))/binary,
        " (id, group_id, task_id, title, description, creator_id, deadline,"
        "  status, created_at, updated_at, idempotency_key, request_digest) "
        "VALUES ($1, $2, $3, $4, $5, $6, $7, 1, $8, $8, $9, $10) "
        "ON CONFLICT (creator_id, group_id, idempotency_key) "
        "  WHERE idempotency_key IS NOT NULL "
        "DO NOTHING "
        "RETURNING id, task_id, request_digest, TRUE AS created "
    >>,
    case
        elib_pg:query(Conn, Sql1, [
            Id,
            GroupId,
            TaskId,
            Title,
            Description,
            CreatorId,
            DeadlineParam,
            Now,
            IdemKey,
            Digest
        ])
    of
        {ok, [Row | _]} ->
            digest_check(Row, Digest);
        {ok, []} ->
            replay_by_key(Conn, CreatorId, GroupId, IdemKey, Digest);
        {error, Reason} ->
            {error, Reason}
    end;
create_task_idempotent_tx(_Conn, #{idempotency_key := Key}) when
    Key =:= <<>>; Key =:= undefined; Key =:= null
->
    {error, idempotency_key_required};
create_task_idempotent_tx(_Conn, #{}) ->
    %% 非法键类型（非 binary）同样拒绝（fail closed，不落库）
    {error, idempotency_key_required}.

%% 同 key 重放回读（独立语句=新快照；0 行=插入失败，防御性保留）
-spec replay_by_key(any(), integer(), integer(), binary(), binary()) ->
    {ok, map()} | {error, idempotency_conflict | insert_failed | term()}.
replay_by_key(Conn, CreatorId, GroupId, IdemKey, Digest) ->
    Sql2 = <<
        "SELECT id, task_id, request_digest, FALSE AS created FROM ",
        (tb(group_task))/binary,
        " WHERE creator_id = $1 AND group_id = $2 AND idempotency_key = $3 "
        "LIMIT 1"
    >>,
    case elib_pg:query(Conn, Sql2, [CreatorId, GroupId, IdemKey]) of
        {ok, [Row | _]} ->
            digest_check(Row, Digest);
        {ok, []} ->
            {error, insert_failed};
        {error, Reason} ->
            {error, Reason}
    end.

%% ------------------------------------------------------------------
%% MN-TASK-02：教学 assignment 写入（同事务；learner_id + 兼容 user_id=唯一监护人）
%% ------------------------------------------------------------------

-spec insert_assignment_tx(any(), integer(), binary(), integer(), integer()) ->
    ok | {error, term()}.
insert_assignment_tx(Conn, Id, TaskId, LearnerId, GuardianUid) when
    is_integer(Id), is_binary(TaskId), is_integer(LearnerId), is_integer(GuardianUid)
->
    Sql = <<
        "INSERT INTO ",
        (tb(group_task_assignment))/binary,
        " (id, task_id, user_id, learner_id, status, content, attachment,"
        "  created_at, updated_at) "
        "VALUES ($1, $2, $3, $4, 0, '', '', $5, $5)"
    >>,
    case elib_pg:query(Conn, Sql, [Id, TaskId, GuardianUid, LearnerId, elib_dt:now()]) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% ------------------------------------------------------------------
%% MN-TASK-02：幂等重放时回读原 assignments（同一事务/连接）
%% ------------------------------------------------------------------

-spec assignments_of_task_tx(any(), binary()) -> {ok, [map()]} | {error, term()}.
assignments_of_task_tx(Conn, TaskId) when is_binary(TaskId), TaskId =/= <<>> ->
    Sql = <<
        "SELECT id, learner_id FROM ",
        (tb(group_task_assignment))/binary,
        " WHERE task_id = $1 AND learner_id IS NOT NULL ORDER BY id"
    >>,
    case elib_pg:query(Conn, Sql, [TaskId]) of
        {ok, Rows} -> {ok, Rows};
        {error, Reason} -> {error, Reason}
    end.

%% ------------------------------------------------------------------
%% MN-TASK-01：老师作业列表（仅本人 active staff 班级的教学作业 + 统计）
%% GroupIdOpt = undefined 汇总全部 active staff 班级 | integer 指定班
%% 教学作业 = 存在 learner_id IS NOT NULL 的 assignment（含 0 提交）
%% ------------------------------------------------------------------

-spec list_tasks(integer(), integer() | undefined, integer(), integer()) ->
    {ok, [map()]} | {error, term()}.
list_tasks(Uid, GroupIdOpt, Page, Size) ->
    list_tasks_run(pool_exec(), Uid, GroupIdOpt, Page, Size).

%% 事务/连接内版本（真库集成测试直连 scratch 用，与生产同 SQL 代码路径）
-spec list_tasks_tx(any(), integer(), integer() | undefined, integer(), integer()) ->
    {ok, [map()]} | {error, term()}.
list_tasks_tx(Conn, Uid, GroupIdOpt, Page, Size) ->
    list_tasks_run(conn_exec(Conn), Uid, GroupIdOpt, Page, Size).

-spec list_tasks_run(exec_fun(), integer(), integer() | undefined, integer(), integer()) ->
    {ok, [map()]} | {error, term()}.
list_tasks_run(Exec, Uid, GroupIdOpt, Page, Size) when
    is_integer(Uid), Uid > 0, is_integer(Page), Page > 0, is_integer(Size), Size > 0
->
    {GroupWhere, GroupParams} = group_filter(GroupIdOpt),
    Params = [Uid | GroupParams],
    LimitIdx = length(Params) + 1,
    OffsetIdx = LimitIdx + 1,
    Offset = (Page - 1) * Size,
    Sql = <<
        "SELECT t.id AS task_id, t.group_id, g.title AS group_name, t.title, t.description, "
        "t.deadline, t.created_at, "
        "COUNT(a.id) AS learner_count, "
        "COUNT(a.id) FILTER (WHERE EXISTS ("
        "  SELECT 1 FROM homework_submission s "
        "  WHERE s.assignment_id = a.id AND s.status = 'submitted')) AS submitted_count, "
        "COUNT(a.id) FILTER (WHERE EXISTS ("
        "  SELECT 1 FROM homework_submission s "
        "  WHERE s.assignment_id = a.id AND s.status = 'submitted') "
        "AND NOT EXISTS ("
        "  SELECT 1 FROM homework_submission s2 "
        "  JOIN teacher_review r ON r.submission_id = s2.id "
        "  WHERE s2.assignment_id = a.id AND s2.status = 'submitted' "
        "    AND r.status = 'published')) AS pending_review_count "
        "FROM ",
        (tb(group_task))/binary,
        " t "
        "JOIN \"group\" g ON g.id = t.group_id "
        "JOIN class_staff cs ON cs.group_id = t.group_id AND cs.user_id = $1 "
        " AND cs.status = 'active' "
        "LEFT JOIN ",
        (tb(group_task_assignment))/binary,
        " a ON a.task_id = t.task_id AND a.learner_id IS NOT NULL "
        "WHERE t.deleted_at IS NULL "
        "AND EXISTS (SELECT 1 FROM ",
        (tb(group_task_assignment))/binary,
        " ta WHERE ta.task_id = t.task_id AND ta.learner_id IS NOT NULL)",
        GroupWhere/binary,
        " GROUP BY t.id, t.task_id, t.group_id, g.title, t.title, t.description, "
        "t.deadline, t.created_at "
        "ORDER BY t.id DESC "
        "LIMIT $",
        (integer_to_binary(LimitIdx))/binary,
        " OFFSET $",
        (integer_to_binary(OffsetIdx))/binary
    >>,
    Exec(Sql, Params ++ [Size, Offset]);
list_tasks_run(_Exec, _Uid, _GroupIdOpt, _Page, _Size) ->
    {error, invalid_param}.

%% ------------------------------------------------------------------
%% MN-TASK-01：列表总数（与 list_tasks 同 WHERE）
%% ------------------------------------------------------------------

-spec count_tasks(integer(), integer() | undefined) -> {ok, integer()} | {error, term()}.
count_tasks(Uid, GroupIdOpt) ->
    count_tasks_run(pool_exec(), Uid, GroupIdOpt).

-spec count_tasks_tx(any(), integer(), integer() | undefined) ->
    {ok, integer()} | {error, term()}.
count_tasks_tx(Conn, Uid, GroupIdOpt) ->
    count_tasks_run(conn_exec(Conn), Uid, GroupIdOpt).

-spec count_tasks_run(exec_fun(), integer(), integer() | undefined) ->
    {ok, integer()} | {error, term()}.
count_tasks_run(Exec, Uid, GroupIdOpt) when is_integer(Uid), Uid > 0 ->
    {GroupWhere, GroupParams} = group_filter(GroupIdOpt),
    Sql = <<
        "SELECT COUNT(DISTINCT t.id) AS count FROM ",
        (tb(group_task))/binary,
        " t "
        "JOIN class_staff cs ON cs.group_id = t.group_id AND cs.user_id = $1 "
        " AND cs.status = 'active' "
        "WHERE t.deleted_at IS NULL "
        "AND EXISTS (SELECT 1 FROM ",
        (tb(group_task_assignment))/binary,
        " ta WHERE ta.task_id = t.task_id AND ta.learner_id IS NOT NULL)",
        GroupWhere/binary
    >>,
    case Exec(Sql, [Uid | GroupParams]) of
        {ok, [#{<<"count">> := Count}]} when is_integer(Count) ->
            {ok, Count};
        {ok, [#{<<"count">> := Count}]} when is_binary(Count) ->
            {ok, binary_to_integer(Count)};
        {error, Reason} ->
            {error, Reason};
        Other ->
            {error, {unexpected_count_result, Other}}
    end;
count_tasks_run(_Exec, _Uid, _GroupIdOpt) ->
    {error, invalid_param}.

%% ------------------------------------------------------------------
%% MN-TASK-01：操作者 active staff 班级（manager/teacher/assistant 均可读）
%% ------------------------------------------------------------------

-spec active_staff_group_ids(integer()) -> {ok, [integer()]} | {error, term()}.
active_staff_group_ids(Uid) ->
    active_staff_group_ids_run(pool_exec(), Uid).

-spec active_staff_group_ids_tx(any(), integer()) -> {ok, [integer()]} | {error, term()}.
active_staff_group_ids_tx(Conn, Uid) ->
    active_staff_group_ids_run(conn_exec(Conn), Uid).

-spec active_staff_group_ids_run(exec_fun(), integer()) ->
    {ok, [integer()]} | {error, term()}.
active_staff_group_ids_run(Exec, Uid) when is_integer(Uid), Uid > 0 ->
    Sql = <<
        "SELECT cs.group_id FROM class_staff cs "
        "WHERE cs.user_id = $1 AND cs.status = 'active' ORDER BY cs.group_id"
    >>,
    case Exec(Sql, [Uid]) of
        {ok, Rows} ->
            {ok, [maps:get(<<"group_id">>, R) || R <- Rows]};
        {error, Reason} ->
            {error, Reason}
    end;
active_staff_group_ids_run(_Exec, _Uid) ->
    {error, invalid_param}.

%% ------------------------------------------------------------------
%% MN-TASK-01：learner assignment_ready 批量校验（一条 SQL）
%% 返回 {ok, #{LearnerId => {ok, GuardianUid} | {error, Reason}}}
%%   Reason = learner_not_in_class（无行/enrollment removed/机构不一致）
%%          | guardian_setup_required（0 或多个 active can_submit 监护人）
%% 缺行的 learner 由调用方补 learner_not_in_class。
%% ------------------------------------------------------------------

-spec learner_readiness(integer(), integer(), [integer()]) ->
    {ok, #{integer() => {ok, integer()} | {error, atom()}}} | {error, term()}.
learner_readiness(GroupId, OrgId, LearnerIds) ->
    learner_readiness_run(pool_exec(), GroupId, OrgId, LearnerIds).

-spec learner_readiness_tx(any(), integer(), integer(), [integer()]) ->
    {ok, #{integer() => {ok, integer()} | {error, atom()}}} | {error, term()}.
learner_readiness_tx(Conn, GroupId, OrgId, LearnerIds) ->
    learner_readiness_run(conn_exec(Conn), GroupId, OrgId, LearnerIds).

-spec learner_readiness_run(exec_fun(), integer(), integer(), [integer()]) ->
    {ok, #{integer() => {ok, integer()} | {error, atom()}}} | {error, term()}.
learner_readiness_run(Exec, GroupId, OrgId, LearnerIds) when
    is_integer(GroupId),
    is_integer(OrgId),
    is_list(LearnerIds),
    LearnerIds =/= []
->
    {InList, Params0} = in_clause(LearnerIds, 1),
    Sql = <<
        "SELECT ce.learner_id, "
        "(ce.status = 'active' AND l.status = 'active') AS enrolled, "
        "l.organization_id AS learner_org, "
        "(SELECT count(*) FROM guardian_learner gl "
        " WHERE gl.learner_id = ce.learner_id AND gl.status = 'active' "
        "   AND gl.can_submit = true) AS submit_guardians, "
        "(SELECT min(gl.guardian_uid) FROM guardian_learner gl "
        " WHERE gl.learner_id = ce.learner_id AND gl.status = 'active' "
        "   AND gl.can_submit = true) AS the_guardian "
        "FROM class_enrollment ce "
        "JOIN learner l ON l.id = ce.learner_id "
        "WHERE ce.group_id = $1 AND ce.learner_id IN ",
        InList/binary
    >>,
    case Exec(Sql, [GroupId | Params0]) of
        {ok, Rows} ->
            Base = readiness_map(Rows, OrgId),
            %% 无 enrollment 行的 learner（跨班/不存在）补 learner_not_in_class
            Missing = maps:from_list([
                {L, {error, learner_not_in_class}}
             || L <- LearnerIds, not maps:is_key(L, Base)
            ]),
            {ok, maps:merge(Missing, Base)};
        {error, Reason} ->
            {error, Reason}
    end;
learner_readiness_run(_Exec, _GroupId, _OrgId, _LearnerIds) ->
    {error, invalid_param}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-type exec_fun() :: fun((binary(), [term()]) -> {ok, term()} | {error, term()}).

-spec pool_exec() -> exec_fun().
pool_exec() ->
    fun(Sql, Params) -> elib_pg:query(Sql, Params) end.

-spec conn_exec(any()) -> exec_fun().
conn_exec(Conn) ->
    fun(Sql, Params) -> elib_pg:query(Conn, Sql, Params) end.

-spec tb(atom()) -> binary().
tb(Tb) ->
    tablename(atom_to_binary(Tb, utf8)).

%% 同 key 不同 digest → 5460（不覆盖原结果；DB 只保证同 key 单行）
%% 历史 NULL digest 行（非本次插入）按冲突处理（fail closed）
-spec digest_check(map(), binary()) -> {ok, map()} | {error, idempotency_conflict}.
digest_check(#{<<"created">> := true} = Row, _Digest) ->
    {ok, Row};
digest_check(#{<<"created">> := false, <<"request_digest">> := Digest} = Row, Digest) ->
    {ok, Row};
digest_check(#{<<"created">> := false}, _Digest) ->
    {error, idempotency_conflict};
digest_check(Row, _Digest) ->
    %% epgsql text 模式兜底：created 以其他形态返回时保守视为重放冲突
    ?LOG_WARNING("create_task_idempotent_tx unexpected row ~p", [Row]),
    {error, idempotency_conflict}.

-spec group_filter(integer() | undefined) -> {binary(), [integer()]}.
group_filter(undefined) ->
    {<<>>, []};
group_filter(GroupId) when is_integer(GroupId) ->
    {<<" AND t.group_id = $2">>, [GroupId]}.

%% N 个占位符（从 From+1 开始编号），占位符顺序与去重后参数一一对应
-spec in_clause([integer()], integer()) -> {binary(), [integer()]}.
in_clause(Items, From) ->
    Ids = lists:usort(Items),
    Placeholders = [
        <<" $", (integer_to_binary(From + I))/binary>>
     || I <- lists:seq(1, length(Ids))
    ],
    {iolist_to_binary(["(", lists:join(<<",">>, Placeholders), ")"]), Ids}.

-spec readiness_map([map()], integer()) ->
    #{integer() => {ok, integer()} | {error, atom()}}.
readiness_map(Rows, OrgId) ->
    maps:from_list([readiness_entry(R, OrgId) || R <- Rows]).

-spec readiness_entry(map(), integer()) ->
    {integer(), {ok, integer()} | {error, atom()}}.
readiness_entry(#{<<"learner_id">> := LearnerId} = Row, OrgId) ->
    Reason =
        case maps:get(<<"enrolled">>, Row, false) of
            false ->
                learner_not_in_class;
            true ->
                case maps:get(<<"learner_org">>, Row, undefined) of
                    OrgId -> guardian_state(maps:get(<<"submit_guardians">>, Row, 0));
                    _ -> learner_not_in_class
                end
        end,
    case Reason of
        ok -> {LearnerId, {ok, maps:get(<<"the_guardian">>, Row)}};
        R -> {LearnerId, {error, R}}
    end.

%% 恰好一个 active can_submit 监护人才 assignment_ready（P0-3：不猜默认监护人）
-spec guardian_state(integer()) -> ok | guardian_setup_required.
guardian_state(1) -> ok;
guardian_state(_) -> guardian_setup_required.
