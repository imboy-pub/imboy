-module(user_deletion_request_repo).
%%%
% user_deletion_request_repo 是 user_deletion_request repository 缩写
% 账号注销请求窄记录仓库层（迁移 00000085，Implementation Plan Task D-01）
%
% 表结构：user_deletion_request(id TSID PK, user_id UNIQUE FK CASCADE,
%   status requested|cancelled|approved, requested_at, cancelled_at,
%   approved_at, updated_at)
%
% 幂等契约：
%   * upsert_request_tx/2 —— 无请求则建；cancelled/approved 态原位复活为
%     新请求（requested_at 重置）；已是 requested 态则**保留首次
%     requested_at**（重复请求不重置宽限期时钟）。
%   * cancel_request_tx/2 / approve_request_tx/2 —— 仅对 requested 态生效，
%     条件更新返回行数为 0/1，并发二次操作安全（同 admin 审批守卫语义）。
%%%

-export([tablename/0]).
-export([upsert_request_tx/2]).
-export([cancel_request_tx/2]).
-export([approve_request_tx/2]).
-export([find_latest/1]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"user_deletion_request">>).

%% @doc 事务内幂等申请注销（一人一行，requested 态保留首次 requested_at）
%% @param Conn 数据库连接
%% @param Uid 用户ID
%% @return {ok, #{<<"status">> => _, <<"requested_at">> => _}} | {error, term()}
-spec upsert_request_tx(pid(), integer()) -> {ok, map()} | {error, term()}.
upsert_request_tx(Conn, Uid) ->
    Tb = tablename(),
    Id = elib_tsid:generate(user_deletion_request),
    %% ON CONFLICT 的 DO UPDATE 带 WHERE：已是 requested 态时跳过更新
    %% （requested_at 保持首次申请时间），cancelled/approved 态原位复活。
    Sql =
        <<"INSERT INTO ", Tb/binary,
            " (id, user_id, status)"
            " VALUES ($1, $2, 'requested')"
            " ON CONFLICT (user_id) DO UPDATE"
            " SET status = 'requested',"
            "     cancelled_at = NULL,"
            "     approved_at = NULL,"
            "     updated_at = CURRENT_TIMESTAMP"
            " WHERE ", Tb/binary,
            ".status <> 'requested'"
            " RETURNING status, requested_at">>,
    case elib_pg:query(Conn, Sql, [Id, Uid]) of
        {ok, [Row]} ->
            {ok, Row};
        %% INSERT 冲突且 DO UPDATE 的 WHERE 未命中（已是 requested 态）：
        %% vendored epgsql 对 0 行 RETURNING 返回 {ok, []}（列表形态）。
        %% 读现值返回，幂等语义=保留首次 requested_at。
        {ok, []} ->
            find_latest_tx(Conn, Uid);
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 事务内撤销注销申请（仅 requested 态生效，0 行=无可撤销）
%% @return {ok, non_neg_integer()} | {error, term()}
-spec cancel_request_tx(pid(), integer()) -> {ok, non_neg_integer()} | {error, term()}.
cancel_request_tx(Conn, Uid) ->
    Tb = tablename(),
    Sql =
        <<"UPDATE ", Tb/binary,
            " SET status = 'cancelled',"
            "     cancelled_at = CURRENT_TIMESTAMP,"
            "     updated_at = CURRENT_TIMESTAMP"
            " WHERE user_id = $1 AND status = 'requested'">>,
    %% execute/3 对 RETURNING 稳定返回计数（1 行={ok, N, Tuples}、
    %% 0 行={ok, 0, []}），dialyzer 规格内且无需解析行
    case elib_pg:execute(Conn, Sql, [Uid]) of
        {ok, Count} -> {ok, Count};
        {ok, Count, _Tuples} -> {ok, Count};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 事务内审批通过注销申请（仅 requested 态生效，0 行=无可批准）
%% @return {ok, non_neg_integer()} | {error, term()}
-spec approve_request_tx(pid(), integer()) -> {ok, non_neg_integer()} | {error, term()}.
approve_request_tx(Conn, Uid) ->
    Tb = tablename(),
    Sql =
        <<"UPDATE ", Tb/binary,
            " SET status = 'approved',"
            "     approved_at = CURRENT_TIMESTAMP,"
            "     updated_at = CURRENT_TIMESTAMP"
            " WHERE user_id = $1 AND status = 'requested'">>,
    %% execute/3 对 RETURNING 稳定返回计数（1 行={ok, N, Tuples}、
    %% 0 行={ok, 0, []}），dialyzer 规格内且无需解析行
    case elib_pg:execute(Conn, Sql, [Uid]) of
        {ok, Count} -> {ok, Count};
        {ok, Count, _Tuples} -> {ok, Count};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 读某用户最近一条请求记录（一人一行，取现值）
%% @return {ok, map()} | {ok, undefined} | {error, term()}
-spec find_latest(integer()) -> {ok, map() | undefined} | {error, term()}.
find_latest(Uid) ->
    Tb = tablename(),
    Sql =
        <<
            "SELECT status, requested_at, cancelled_at, approved_at"
            " FROM ",
            Tb/binary,
            " WHERE user_id = $1"
            " ORDER BY updated_at DESC LIMIT 1"
        >>,
    case elib_pg:query(Sql, [Uid]) of
        {ok, [Row]} -> {ok, Row};
        {ok, []} -> {ok, undefined};
        {error, Reason} -> {error, Reason}
    end.

%% ===================================================================
%% Internal functions
%% ===================================================================

-spec find_latest_tx(pid(), integer()) -> {ok, map()} | {error, term()}.
find_latest_tx(Conn, Uid) ->
    Tb = tablename(),
    Sql =
        <<"SELECT status, requested_at FROM ", Tb/binary, " WHERE user_id = $1 LIMIT 1">>,
    case elib_pg:query(Conn, Sql, [Uid]) of
        {ok, [Row]} -> {ok, Row};
        {ok, []} -> {error, request_row_missing};
        {error, Reason} -> {error, Reason}
    end.
