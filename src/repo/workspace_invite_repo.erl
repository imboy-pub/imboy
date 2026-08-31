-module(workspace_invite_repo).
%%%
% workspace_invite_repo 是 workspace_invite repository 缩写
% 工作区团队码数据仓库层（迁移 00000082，工作区可复用团队码 T2）
%
% 表结构：workspace_invite(id TSID PK, workspace_id FK CASCADE, code UNIQUE,
%   created_by FK SET NULL, expires_at, status active|revoked, timestamps)
% 团队码：8 位，字符集 = 大写 A-Z + 数字 2-9（排除 0/O/1/I 混淆字符），
%   一码多人复用、7 天有效、Owner 可撤销（revoked 后作废不复用）。
%%%

-export([tablename/0]).
-export([generate_invite_code/0]).
-export([add_tx/5]).
-export([find_active_by_code_tx/2]).
-export([revoke_tx/3]).
-export([revoke_active_by_ws_tx/2]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

%% 团队码字符集：大写字母（排除 I/O）+ 数字 2-9（32 个；
%% 排除 0/O/1/I 手抄混淆字符）
-define(INVITE_CHARSET, <<"ABCDEFGHJKLMNPQRSTUVWXYZ23456789">>).
-define(INVITE_CODE_LEN, 8).

%% ===================================================================
%% API functions
%% ===================================================================

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"workspace_invite">>).

%% @doc 生成团队码（8 位，A-Z + 2-9；rand:uniform 逐位抽取）
%% 镜像 channel_invitation_repo:generate_invitation_code/0 的写法。
-spec generate_invite_code() -> binary().
generate_invite_code() ->
    generate_code_chars(?INVITE_CODE_LEN, <<>>).

%% @doc 事务内插入团队码行（INSERT ... RETURNING 新行）
%% code 撞全局唯一约束或部分唯一索引 uk_workspace_invite_ws_active
%% （一工作区至多一个 active 码）均归一 {error, code_conflict}，
%% 由调用方（logic 层）重新生成码后重试。
-spec add_tx(any(), integer(), binary(), integer() | null, binary()) ->
    {ok, map()} | {error, code_conflict | term()}.
add_tx(Conn, WsId, Code, CreatedBy, ExpiresAt) ->
    Tb = tablename(),
    Id = elib_tsid:generate(workspace_invite),
    Now = elib_dt:now(),
    Sql =
        <<"INSERT INTO ", Tb/binary,
            " (id, workspace_id, code, created_by, expires_at, status, created_at, updated_at)",
            " VALUES ($1, $2, $3, $4, $5, 'active', $6, $6)",
            " RETURNING id, workspace_id, code, created_by, expires_at, status">>,
    case elib_pg:query(Conn, Sql, [Id, WsId, Code, CreatedBy, ExpiresAt, Now]) of
        {ok, [Row | _]} ->
            {ok, Row};
        {ok, []} ->
            {error, insert_empty_result};
        {error, {pgsql_error, #{code := <<"23505">>}}} ->
            {error, code_conflict};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 事务内按码查有效团队码（status=active；过期与否由调用方判定）
%% 过期判定由 SQL 同行计算 expired 布尔（(expires_at < CURRENT_TIMESTAMP)），
%% 避免在 Erlang 侧解析时间格式。含已撤销码 → not_found（981 口径）。
-spec find_active_by_code_tx(any(), binary()) -> {ok, map()} | not_found | {error, term()}.
find_active_by_code_tx(Conn, Code) ->
    Tb = tablename(),
    Sql =
        <<"SELECT id, workspace_id, code, created_by, expires_at,",
            " (expires_at < CURRENT_TIMESTAMP) AS expired", " FROM ", Tb/binary,
            " WHERE code = $1 AND status = 'active' LIMIT 1">>,
    case elib_pg:query(Conn, Sql, [Code]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> not_found;
        {error, Reason} -> {error, Reason}
    end.

%% @doc 事务内按码撤销单个团队码（幂等：非 active 码返回 not_active）。
%% 当前端点走 revoke_active_by_ws_tx/2（按工作区撤全部）；本函数为将来
%% 「多码并存/按码撤销」预留，暂无生产调用方。
-spec revoke_tx(any(), integer(), binary()) -> ok | {error, not_active | term()}.
revoke_tx(Conn, WsId, Code) ->
    Tb = tablename(),
    Now = elib_dt:now(),
    Sql =
        <<"UPDATE ", Tb/binary, " SET status = 'revoked', updated_at = $1",
            " WHERE workspace_id = $2 AND code = $3 AND status = 'active'">>,
    case elib_pg:execute(Conn, Sql, [Now, WsId, Code]) of
        {ok, 1} -> ok;
        {ok, _} -> {error, not_active};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 事务内按工作区撤销全部有效团队码（一工作区至多一个 active 码的
%% 维护口径：generate 前置撤旧 + Owner 主动撤销端点共用）。幂等：无
%% active 码返回 {ok, 0}。
-spec revoke_active_by_ws_tx(any(), integer()) -> {ok, non_neg_integer()} | {error, term()}.
revoke_active_by_ws_tx(Conn, WsId) ->
    Tb = tablename(),
    Now = elib_dt:now(),
    Sql =
        <<"UPDATE ", Tb/binary, " SET status = 'revoked', updated_at = $1",
            " WHERE workspace_id = $2 AND status = 'active'">>,
    case elib_pg:execute(Conn, Sql, [Now, WsId]) of
        {ok, Count} when is_integer(Count) -> {ok, Count};
        {error, Reason} -> {error, Reason}
    end.

%% ===================================================================
%% Internal Functions
%% ===================================================================

%% @doc 逐位抽取团队码（rand:uniform/1 返回 1..Len，转 0 起下标）
-spec generate_code_chars(non_neg_integer(), binary()) -> binary().
generate_code_chars(0, Acc) ->
    Acc;
generate_code_chars(N, Acc) ->
    Chars = ?INVITE_CHARSET,
    Pos = rand:uniform(byte_size(Chars)) - 1,
    <<_:Pos/binary, Char:1/binary, _/binary>> = Chars,
    generate_code_chars(N - 1, <<Acc/binary, Char/binary>>).
