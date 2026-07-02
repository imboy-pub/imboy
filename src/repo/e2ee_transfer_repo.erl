-module(e2ee_transfer_repo).
%%%===================================================================
%%% @doc E2EE 设备间传输 Repo 层
%%%
%%% 管理 E2EE 设备间传输会话的数据库操作
%%%===================================================================

-include("log.hrl").

%%===================================================================
%%% API Functions
%%===================================================================

%% 导出函数
-export([create/1]).
-export([update_status/2]).
-export([update_status_and_device/4]).
-export([get_by_session_id/1]).
-export([get_pending_sessions/1]).
-export([is_valid_session/1]).
-export([cleanup_expired_sessions/0]).
-export([get_stalled_sessions/0]).
-export([cancel_session/2]).

%%===================================================================
%%% Query Functions
%%===================================================================

%% @doc 创建传输会话
-spec create(map()) -> {ok, integer()} | {error, term()}.
create(Params) ->
    SessionId = maps:get(<<"session_id">>, Params),
    FromUid = maps:get(<<"from_uid">>, Params),
    FromDeviceId = maps:get(<<"from_device_id">>, Params),
    ToUid = maps:get(<<"to_uid">>, Params),
    KeyBundle = maps:get(<<"encrypted_key_bundle">>, Params),
    ExpiresAt = maps:get(<<"expires_at">>, Params),

    Id = elib_tsid:generate(e2ee_transfer),
    Sql1 =
        <<"INSERT INTO e2ee_transfer_sessions ",
            "(id, session_id, from_uid, from_device_id, to_uid, to_device_id, ",
            "status, encrypted_key_bundle, expires_at) ",
            "VALUES ($1, $2, $3, $4, $5, $6, 'pending', $7, $8)">>,
    case
        elib_pg:execute(Sql1, [
            Id, SessionId, FromUid, FromDeviceId, ToUid, <<>>, KeyBundle, ExpiresAt
        ])
    of
        {ok, _Count} ->
            {ok, Id};
        {error, {pgsql_error, #{code := <<"23505">>}}} ->
            % 唯一约束违规（并发保护）
            {error, unique_violation};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 更新会话状态
-spec update_status(binary(), binary()) -> ok | {error, term()}.
update_status(SessionId, Status) ->
    Sql1 =
        <<"UPDATE e2ee_transfer_sessions ", "SET status = $1 ", "WHERE session_id = $2 ",
            "AND expires_at > NOW()">>,
    case elib_pg:execute(Sql1, [Status, SessionId]) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc 更新会话状态（同时设置 to_device_id，并延长过期时间）
%% 仅当当前状态仍为 pending 时才允许转移（CAS），防止并发 accept 请求互相覆盖 to_device_id。
%% ExtendSeconds：accept 后重置有效期窗口，修复"accept 后 300s 硬过期，
%% 用户犹豫超时导致 confirm 必失败 / accepted 会话被 cleanup 误清"。
-spec update_status_and_device(binary(), binary(), binary(), pos_integer()) ->
    ok | {error, term()}.
update_status_and_device(SessionId, Status, ToDeviceId, ExtendSeconds) ->
    Sql1 =
        <<"UPDATE e2ee_transfer_sessions ", "SET status = $1, to_device_id = $2, ",
            "expires_at = NOW() + make_interval(secs => $4) ", "WHERE session_id = $3 ",
            "AND status = 'pending' ", "AND expires_at > NOW()">>,
    case elib_pg:execute(Sql1, [Status, ToDeviceId, SessionId, ExtendSeconds]) of
        {ok, 0} -> {error, conflict};
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc 获取传输会话信息
-spec get_by_session_id(binary()) -> {ok, map()} | {error, not_found}.
get_by_session_id(SessionId) ->
    Sql1 =
        <<"SELECT id, session_id, from_uid, from_device_id, to_uid, to_device_id, ",
            "status, encrypted_key_bundle, expires_at, created_at ", "FROM e2ee_transfer_sessions ",
            "WHERE session_id = $1 ", "AND expires_at > NOW() ", "LIMIT 1">>,
    case elib_pg:query(Sql1, [SessionId]) of
        {ok, []} -> {error, not_found};
        {ok, [Row | _]} -> {ok, Row};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 检查会话是否存在且有效
-spec is_valid_session(binary()) -> boolean().
is_valid_session(SessionId) ->
    Sql1 =
        <<"SELECT 1 FROM e2ee_transfer_sessions ",
            "WHERE session_id = $1 AND expires_at > NOW() LIMIT 1">>,
    case elib_pg:query(Sql1, [SessionId]) of
        {ok, [_ | _]} -> true;
        _ -> false
    end.

%% @doc 获取用户的待处理会话列表
-spec get_pending_sessions(integer()) -> {ok, list(map())} | {error, term()}.
get_pending_sessions(Uid) ->
    Sql1 =
        <<"SELECT id, session_id, from_uid, from_device_id, to_uid, to_device_id, ",
            "status, expires_at, created_at ", "FROM e2ee_transfer_sessions ", "WHERE to_uid = $1 ",
            "AND status = 'pending' ", "AND expires_at > NOW() ", "ORDER BY created_at DESC ",
            "LIMIT 10">>,
    case elib_pg:query(Sql1, [Uid]) of
        {ok, Rows} -> {ok, Rows};
        {error, Reason} -> {error, Reason}
    end.

%%===================================================================
%%% Utility Functions
%%===================================================================

%% @doc 清理过期的传输会话
%% 删除所有已过期的 pending 和 accepted 状态的会话
%% @returns {ok, DeletedCount} | {error, Reason}
-spec cleanup_expired_sessions() -> {ok, non_neg_integer()} | {error, term()}.
cleanup_expired_sessions() ->
    Sql1 =
        <<"DELETE FROM e2ee_transfer_sessions ", "WHERE id IN (",
            "  SELECT id FROM e2ee_transfer_sessions ",
            "  WHERE expires_at < NOW() AND status IN ('pending', 'accepted') LIMIT 1000",
            ") RETURNING id">>,
    case elib_pg:execute(Sql1, []) of
        {ok, Count, _Rows} ->
            ok = ?INFO_LOG([e2ee_transfer_cleanup, deleted_sessions, Count]),
            {ok, Count};
        {error, Reason} ->
            ok = ?ERROR_LOG([e2ee_transfer_cleanup, failed, Reason]),
            {error, Reason}
    end.

%% @doc 获取停滞的会话（长时间处于 pending 状态）
%% 用于监控和告警
%% @returns {ok, [Session]} | {error, Reason}
-spec get_stalled_sessions() -> {ok, [map()]} | {error, term()}.
get_stalled_sessions() ->
    Sql1 =
        <<"SELECT id, session_id, from_uid, to_uid, status, created_at ",
            "FROM e2ee_transfer_sessions ", "WHERE status = 'pending' ",
            "AND created_at < NOW() - INTERVAL '10 minutes' ", "ORDER BY created_at ASC ",
            "LIMIT 100">>,
    case elib_pg:query(Sql1, []) of
        {ok, Rows} -> {ok, Rows};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 取消传输会话
%% @param SessionId 会话 ID
%% @param FromUid 发送方用户 ID（用于权限验证）
%% @returns ok | {error, Reason}
-spec cancel_session(binary(), integer()) -> ok | {error, term()}.
cancel_session(SessionId, FromUid) ->
    Sql1 =
        <<"UPDATE e2ee_transfer_sessions ", "SET status = 'cancelled' ",
            "WHERE session_id = $1 AND from_uid = $2 ", "AND status = 'pending' ",
            "AND expires_at > NOW()">>,
    case elib_pg:execute(Sql1, [SessionId, FromUid]) of
        {ok, 0} -> {error, not_found_or_not_pending};
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.
