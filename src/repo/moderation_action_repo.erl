-module(moderation_action_repo).

%% R-02：处置动作审计行仓储。动作自身事实（executed/failed/reversed/
%% expired）全在本表；对 report_ticket 零结构回写（case=举报单行）。

-export([tablename/0]).
-export([insert_tx/2]).
-export([find_by_id/1]).
-export([find_by_case/1]).
-export([has_executed_same_action/3]).
-export([mark_reversed/3]).
-export([mark_failed_tx/3]).
-export([expire_due/0]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"moderation_action">>).

%% @doc 事务内插入动作行（INSERT ... RETURNING 全字段）。
%% 零行 RETURNING（理论不可达，vendored epgsql 双形态防御）走 {ok, []} →
%% 报错而非假装成功（fail-closed：动作行必须落库）。
-spec insert_tx(pid(), map()) -> {ok, map()} | {error, binary()}.
insert_tx(Conn, #{case_id := CaseId, action := Action, actor_id := ActorId} = A) ->
    Tb = tablename(),
    Id = elib_tsid:generate(moderation_action),
    Scope = jsone:encode(maps:get(scope, A, #{}), [native_utf8]),
    Result = jsone:encode(maps:get(result, A, #{}), [native_utf8]),
    TargetType = maps:get(target_type, A, <<>>),
    TargetId = maps:get(target_id, A, 0),
    TargetUid = maps:get(target_uid, A, 0),
    Reason = maps:get(reason, A, <<>>),
    FailReason = maps:get(fail_reason, A, <<>>),
    Status = maps:get(status, A, <<"executed">>),
    EndAt = maps:get(end_at, A, null),
    Sql =
        <<"INSERT INTO ", Tb/binary,
            " (id, case_id, action, target_type, target_id, target_uid,"
            " scope, reason, actor_id, status, result, fail_reason, end_at)"
            " VALUES ($1, $2, $3, $4, $5, $6, $7::jsonb, $8, $9, $10,"
            " $11::jsonb, $12, $13)"
            " RETURNING id, case_id, action, target_type, target_id,"
            " target_uid, scope, reason, actor_id, status, result,"
            " fail_reason, start_at, end_at">>,
    case
        elib_pg:query(Conn, Sql, [
            Id,
            CaseId,
            Action,
            TargetType,
            TargetId,
            TargetUid,
            Scope,
            Reason,
            ActorId,
            Status,
            Result,
            FailReason,
            EndAt
        ])
    of
        {ok, [Row]} ->
            {ok, Row};
        {ok, _} ->
            {error, <<"moderation_action insert returned no row">>};
        {error, Reason} ->
            {error, elib_cnv:safe_to_binary(Reason)}
    end.

%% @doc 事务内补记失败（先插 executed 行失败时不会走到这里；本函数用于
%% primitives 抛错后由 logic 决定补一行 failed 审计——保持 case truthful）。
-spec mark_failed_tx(pid(), integer(), binary()) -> {ok, non_neg_integer()} | {error, binary()}.
mark_failed_tx(Conn, Id, FailReason) ->
    Tb = tablename(),
    Sql =
        <<"UPDATE ", Tb/binary,
            " SET status = 'failed', fail_reason = $2, updated_at = NOW()"
            " WHERE id = $1 AND status = 'executed'">>,
    case elib_pg:execute(Conn, Sql, [Id, FailReason]) of
        {ok, Count} when is_integer(Count) ->
            {ok, Count};
        {ok, Count, _} when is_integer(Count) ->
            {ok, Count};
        {error, Reason} ->
            {error, elib_cnv:safe_to_binary(Reason)}
    end.

-spec find_by_id(integer()) -> {ok, map() | undefined} | {error, binary()}.
find_by_id(Id) ->
    Tb = tablename(),
    Sql =
        <<
            "SELECT id, case_id, action, target_type, target_id, target_uid,"
            " scope, reason, actor_id, status, result, fail_reason,"
            " start_at, end_at, reversed_at, reversed_by, reverse_reason,"
            " created_at, updated_at"
            " FROM ",
            Tb/binary,
            " WHERE id = $1"
        >>,
    case elib_pg:query(Sql, [Id]) of
        {ok, [Row]} ->
            {ok, Row};
        {ok, []} ->
            {ok, undefined};
        {error, Reason} ->
            {error, elib_cnv:safe_to_binary(Reason)}
    end.

-spec find_by_case(integer()) -> {ok, [map()]} | {error, binary()}.
find_by_case(CaseId) ->
    Tb = tablename(),
    Sql =
        <<
            "SELECT id, case_id, action, target_type, target_id, target_uid,"
            " scope, reason, actor_id, status, result, fail_reason,"
            " start_at, end_at, reversed_at, reversed_by, reverse_reason,"
            " created_at, updated_at"
            " FROM ",
            Tb/binary,
            " WHERE case_id = $1 ORDER BY id ASC"
        >>,
    case elib_pg:query(Sql, [CaseId]) of
        {ok, Rows} ->
            {ok, Rows};
        {error, Reason} ->
            {error, elib_cnv:safe_to_binary(Reason)}
    end.

%% @doc 幂等守卫：同 case 同 action 已有 executed（未撤销/未过期）行 → true。
%% 重复执行同一处置应显式拒绝而非双发通知/二次禁言。
-spec has_executed_same_action(integer(), binary(), integer()) ->
    {ok, boolean()} | {error, binary()}.
has_executed_same_action(CaseId, Action, TargetUid) ->
    Tb = tablename(),
    Sql =
        <<"SELECT COUNT(*)::bigint AS n FROM ", Tb/binary,
            " WHERE case_id = $1 AND action = $2 AND status = 'executed'"
            " AND (target_uid = $3 OR target_uid = 0)">>,
    case elib_pg:query(Sql, [CaseId, Action, TargetUid]) of
        {ok, [#{<<"n">> := N}]} when is_integer(N) ->
            {ok, N > 0};
        {ok, [Row]} ->
            {ok, ec_cnv:to_integer(maps:get(<<"n">>, Row, 0)) > 0};
        {error, Reason} ->
            {error, elib_cnv:safe_to_binary(Reason)}
    end.

%% @doc 撤销：仅 executed 态可撤销（failed 无从撤销、reversed 幂等拒绝）。
%% 返回更新行数（0 = 不可撤销）。
-spec mark_reversed(integer(), integer(), binary()) ->
    {ok, non_neg_integer()} | {error, binary()}.
mark_reversed(Id, AdmUid, Reason) ->
    Tb = tablename(),
    Sql =
        <<"UPDATE ", Tb/binary,
            " SET status = 'reversed', reversed_at = NOW(),"
            "     reversed_by = $2, reverse_reason = $3, updated_at = NOW()"
            " WHERE id = $1 AND status = 'executed'">>,
    case elib_pg:execute(Sql, [Id, AdmUid, Reason]) of
        {ok, Count} when is_integer(Count) ->
            {ok, Count};
        {ok, Count, _} when is_integer(Count) ->
            {ok, Count};
        {error, Reason} ->
            {error, elib_cnv:safe_to_binary(Reason)}
    end.

%% @doc 到期 sweep：把 end_at 已过期的 executed 动作翻转为 expired。
%% 业务失效由原语自带的 until 时间戳保证，这里只做审计状态闭环。
%% 返回翻转的行（含 target_uid/scope，供 account_restrict 恢复 prev_status）。
-spec expire_due() -> {ok, [map()]} | {error, binary()}.
expire_due() ->
    Tb = tablename(),
    Sql =
        <<"UPDATE ", Tb/binary,
            " SET status = 'expired', updated_at = NOW()"
            " WHERE action IN ('group_mute', 'account_restrict')"
            " AND status = 'executed'"
            " AND end_at IS NOT NULL AND end_at <= NOW()"
            " RETURNING id, action, target_uid, scope">>,
    case elib_pg:query(Sql, []) of
        {ok, Rows} ->
            {ok, Rows};
        {error, Reason} ->
            {error, elib_cnv:safe_to_binary(Reason)}
    end.
