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
-export([update_status_and_device/3]).
-export([get_by_session_id/1]).
-export([get_pending_sessions/1]).
-export([is_valid_session/1]).
-export([generate_session_id/0]).
-export([cleanup_expired_sessions/0]).
-export([get_stalled_sessions/0]).
-export([cancel_session/2]).

%%===================================================================
%%% Query Functions
%%===================================================================

%% @doc 根据 session_id 查询传输会话
-spec find_by_session_id(binary()) -> {ok, map()} | {error, term()}.
find_by_session_id(SessionId) ->
    Sql1 = <<"SELECT id, session_id, from_uid, from_device_id, to_uid, to_device_id, ",
             "status, encrypted_key_bundle, expires_at, created_at ",
             "FROM e2ee_transfer_sessions ",
             "WHERE session_id = $1 ",
             "AND expires_at > NOW() ",
             "AND status IN ('pending', 'accepted')">>,
    case elib_pg:query(Sql1, [SessionId]) of
        {ok, []} -> {error, not_found};
        {ok, [Row | _]} -> {ok, Row};
        {error, Reason} -> {error, Reason}
    end.

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
    Sql1 = <<"INSERT INTO e2ee_transfer_sessions ",
             "(id, session_id, from_uid, from_device_id, to_uid, to_device_id, ",
             "status, encrypted_key_bundle, expires_at) ",
             "VALUES ($1, $2, $3, $4, $5, $6, 'pending', $7, $8)">>,
    case elib_pg:execute(Sql1, [Id, SessionId, FromUid, FromDeviceId, ToUid, <<>>, KeyBundle, ExpiresAt]) of
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
    Sql1 = <<"UPDATE e2ee_transfer_sessions ",
             "SET status = $1 ",
             "WHERE session_id = $2 ",
             "AND expires_at > NOW()">>,
    case elib_pg:execute(Sql1, [Status, SessionId]) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc 更新会话状态（同时设置 to_device_id）
-spec update_status_and_device(binary(), binary(), binary()) -> ok | {error, term()}.
update_status_and_device(SessionId, Status, ToDeviceId) ->
    Sql1 = <<"UPDATE e2ee_transfer_sessions ",
             "SET status = $1, to_device_id = $2 ",
             "WHERE session_id = $3 ",
             "AND expires_at > NOW()">>,
    case elib_pg:execute(Sql1, [Status, ToDeviceId, SessionId]) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc 获取传输会话信息
-spec get_by_session_id(binary()) -> {ok, map()} | {error, not_found}.
get_by_session_id(SessionId) ->
    Sql1 = <<"SELECT id, session_id, from_uid, from_device_id, to_uid, to_device_id, ",
             "status, encrypted_key_bundle, expires_at, created_at ",
             "FROM e2ee_transfer_sessions ",
             "WHERE session_id = $1 ",
             "AND expires_at > NOW() ",
             "LIMIT 1">>,
    case elib_pg:query(Sql1, [SessionId]) of
        {ok, []} -> {error, not_found};
        {ok, [Row | _]} -> {ok, Row};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 检查会话是否存在且有效
-spec is_valid_session(binary()) -> boolean().
is_valid_session(SessionId) ->
    case get_by_session_id(SessionId) of
        {ok, _} -> true;
        _ -> false
    end.

%% @doc 获取用户的待处理会话列表
-spec get_pending_sessions(integer()) -> {ok, list(map())} | {error, term()}.
get_pending_sessions(Uid) ->
    Sql1 = <<"SELECT id, session_id, from_uid, from_device_id, to_uid, to_device_id, ",
             "status, expires_at, created_at ",
             "FROM e2ee_transfer_sessions ",
             "WHERE to_uid = $1 ",
             "AND status = 'pending' ",
             "AND expires_at > NOW() ",
             "ORDER BY created_at DESC ",
             "LIMIT 10">>,
    case elib_pg:query(Sql1, [Uid]) of
        {ok, Rows} -> {ok, Rows};
        {error, Reason} -> {error, Reason}
    end.

%%===================================================================
%%% Utility Functions
%%===================================================================

%% @doc 生成会话 ID（UUID v4）
-spec generate_session_id() -> binary().
generate_session_id() ->
    % 使用 crypto 生成 UUID v4
    <<A:32, B:16, C:16, D:16, E:48>> = crypto:strong_rand_bytes(16),
    % 设置版本位和变体位（UUID v4）
    C4 = (C band 16#0FFF) bor 16#4000,
    D4 = (D band 16#3FFF) bor 16#8000,
    % 格式化为标准 UUID 格式
    Str = lists:flatten(io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~4.16.0b-~12.16.0b",
        [A, B, C4, D4, E])),
    list_to_binary(Str).

%% @doc 检查会话是否过期
-spec is_expired(map()) -> boolean().
is_expired(Session) ->
    ExpiresAt = maps:get(<<"expires_at">>, Session),
    case catch(calendar:rfc3339_to_system_time(ExpiresAt)) of
        ExpTime when is_integer(ExpTime) ->
            Now = erlang:system_time(second),
            Now > ExpTime;
        _ ->
            true
    end.

%% @doc 清理过期的传输会话
%% 删除所有已过期的 pending 和 accepted 状态的会话
%% @returns {ok, DeletedCount} | {error, Reason}
-spec cleanup_expired_sessions() -> {ok, non_neg_integer()} | {error, term()}.
cleanup_expired_sessions() ->
    Sql1 = <<"DELETE FROM e2ee_transfer_sessions ",
             "WHERE expires_at < NOW() ",
             "AND status IN ('pending', 'accepted') ",
             "RETURNING id">>,
    case elib_pg:execute(Sql1, []) of
        {ok, Count, _Rows} ->
            ok = ?INFO_LOG([e2ee_transfer_cleanup, deleted_sessions, Count]),
            {ok, Count};
        {ok, 0} ->
            {ok, 0};
        {error, Reason} ->
            ok = ?ERROR_LOG([e2ee_transfer_cleanup, failed, Reason]),
            {error, Reason}
    end.

%% @doc 获取停滞的会话（长时间处于 pending 状态）
%% 用于监控和告警
%% @returns {ok, [Session]} | {error, Reason}
-spec get_stalled_sessions() -> {ok, [map()]} | {error, term()}.
get_stalled_sessions() ->
    Sql1 = <<"SELECT id, session_id, from_uid, to_uid, status, created_at ",
             "FROM e2ee_transfer_sessions ",
             "WHERE status = 'pending' ",
             "AND created_at < NOW() - INTERVAL '10 minutes' ",
             "ORDER BY created_at ASC ",
             "LIMIT 100">>,
    case elib_pg:query(Sql1, []) of
        {ok, Rows} -> {ok, Rows};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 检查用户是否有进行中的传输会话
%% 用于并发保护，防止同时创建多个传输
%% @param FromUid 发送方用户 ID
%% @param ToUid 接收方用户 ID
%% @returns true | false
-spec has_pending_session(integer(), integer()) -> boolean().
has_pending_session(FromUid, ToUid) ->
    Sql1 = <<"SELECT COUNT(*) as cnt FROM e2ee_transfer_sessions ",
             "WHERE from_uid = $1 AND to_uid = $2 ",
             "AND status IN ('pending', 'accepted') ",
             "AND expires_at > NOW()">>,
    case elib_pg:query(Sql1, [FromUid, ToUid]) of
        {ok, [#{<<"cnt">> := 0}]} -> false;
        {ok, [#{<<"cnt">> := Count}]} when Count > 0 -> true;
        _ -> false
    end.

%% @doc 取消传输会话
%% @param SessionId 会话 ID
%% @param FromUid 发送方用户 ID（用于权限验证）
%% @returns ok | {error, Reason}
-spec cancel_session(binary(), integer()) -> ok | {error, term()}.
cancel_session(SessionId, FromUid) ->
    Sql1 = <<"UPDATE e2ee_transfer_sessions ",
             "SET status = 'cancelled' ",
             "WHERE session_id = $1 AND from_uid = $2 ",
             "AND status = 'pending' ",
             "AND expires_at > NOW()">>,
    case elib_pg:execute(Sql1, [SessionId, FromUid]) of
        {ok, 0} -> {error, not_found_or_not_pending};
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.
