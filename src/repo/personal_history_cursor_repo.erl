-module(personal_history_cursor_repo).
%%%
%%% personal_history_cursor_repo —— 个人历史 change/ack 游标 slot（迁移 278）
%%%
%%% 自 personal_history_repo 拆出（elvis no_god_modules：主仓 37 函数超 30）。
%%% 本模块零业务策略：水位推进/ack 合法性裁决在 ds/logic 层；此处只提供
%%% slot 行级读写与事务内原子原语。游标语义（acked ≤ change 由 DB CHECK 兜底）。
%%%

-export([cursor_tablename/0]).
-export([find_cursor/4, find_cursor_tx/5, ensure_cursor_tx/5]).
-export([record_issued_tx/6, ack_cursor_tx/6, delete_cursors_by_account_tx/3]).

%% ===================================================================
%% Tablename
%% ===================================================================

-spec cursor_tablename() -> binary().
cursor_tablename() ->
    elib_pg_sql:public_tablename(<<"personal_history_cursor">>).

%% ===================================================================
%% Cursor slots（change 发放 / durably-ack 水位）
%% ===================================================================

%% @doc 读 slot 游标行（change 发放水位 / acked 确认水位）。
-spec find_cursor(integer(), binary(), integer(), binary()) ->
    {ok, map() | undefined} | {error, term()}.
find_cursor(AccountId, Domain, Generation, ClientInstance) ->
    Params = slot_params(AccountId, Domain, Generation, ClientInstance),
    elib_pg:query_judged(cursor_slot_sql(), Params, {ok, undefined}).

%% @doc 事务内读 slot 游标行（tx 内禁用池连接）。
-spec find_cursor_tx(pid(), integer(), binary(), integer(), binary()) ->
    {ok, map() | undefined} | {error, term()}.
find_cursor_tx(Conn, AccountId, Domain, Generation, ClientInstance) ->
    Params = slot_params(AccountId, Domain, Generation, ClientInstance),
    elib_pg:query_judged(Conn, cursor_slot_sql(), Params, {ok, undefined}).

slot_params(AccountId, Domain, Generation, ClientInstance) ->
    [AccountId, Domain, Generation, ClientInstance].

slot_params(AccountId, Domain, Generation, ClientInstance, Cursor) ->
    [AccountId, Domain, Generation, ClientInstance, Cursor].

cursor_slot_sql() ->
    <<"SELECT change_cursor, acked_cursor, updated_at FROM ", (cursor_tablename())/binary,
        " WHERE account_id = $1 AND domain = $2 AND generation = $3 AND client_instance = $4">>.

%% @doc 事务内确保 slot 存在（幂等 ON CONFLICT DO NOTHING）。
-spec ensure_cursor_tx(pid(), integer(), binary(), integer(), binary()) -> ok | {error, term()}.
ensure_cursor_tx(Conn, AccountId, Domain, Generation, ClientInstance) ->
    Sql =
        <<"INSERT INTO ", (cursor_tablename())/binary,
            " (account_id, domain, generation, client_instance)"
            " VALUES ($1, $2, $3, $4) ON CONFLICT DO NOTHING">>,
    Params = slot_params(AccountId, Domain, Generation, ClientInstance),
    elib_pg:execute_ok(Conn, Sql, Params).

%% @doc 记录本 slot 已发放的最大 event_seq（只前进不回退）。
-spec record_issued_tx(pid(), integer(), binary(), integer(), binary(), integer()) ->
    ok | {error, term()}.
record_issued_tx(Conn, AccountId, Domain, Generation, ClientInstance, EventSeq) ->
    Sql =
        <<"UPDATE ", (cursor_tablename())/binary,
            " SET change_cursor = GREATEST(change_cursor, $5), updated_at = CURRENT_TIMESTAMP"
            " WHERE account_id = $1 AND domain = $2 AND generation = $3 AND client_instance = $4">>,
    Params = slot_params(AccountId, Domain, Generation, ClientInstance, EventSeq),
    elib_pg:execute_ok(Conn, Sql, Params).

%% @doc durably-ack：仅推进 ≤ 已发放最大值（acked ≤ change 由 DB CHECK 兜底）。
%% 返回推进后的 {acked_cursor, change_cursor}；越界/未命中返回 none。
-spec ack_cursor_tx(pid(), integer(), binary(), integer(), binary(), integer()) ->
    {ok, map()} | {ok, none} | {error, term()}.
ack_cursor_tx(Conn, AccountId, Domain, Generation, ClientInstance, Cursor) ->
    Sql =
        <<"UPDATE ", (cursor_tablename())/binary,
            " SET acked_cursor = GREATEST(acked_cursor, $5), updated_at = CURRENT_TIMESTAMP"
            " WHERE account_id = $1 AND domain = $2 AND generation = $3 AND client_instance = $4"
            " AND $5 <= change_cursor"
            " RETURNING acked_cursor, change_cursor">>,
    Params = slot_params(AccountId, Domain, Generation, ClientInstance, Cursor),
    elib_pg:query_judged(Conn, Sql, Params, {ok, none}).

%% @doc purge：事务内清 slot 游标。
-spec delete_cursors_by_account_tx(pid(), integer(), binary()) ->
    {ok, non_neg_integer()} | {error, term()}.
delete_cursors_by_account_tx(Conn, AccountId, Domain) ->
    Sql = <<"DELETE FROM ", (cursor_tablename())/binary, " WHERE account_id = $1 AND domain = $2">>,
    elib_pg:execute_count(Conn, Sql, [AccountId, Domain]).
