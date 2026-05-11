-module(e2ee_transfer_ds).
-dialyzer(no_return).
%%%===================================================================
%%% @doc E2EE 设备间传输 DS 层
%%%
%%% 数据服务层，封装设备间传输功能的数据库操作和缓存逻辑
%%% 调用 e2ee_transfer_repo 进行数据库操作
%%%===================================================================

%%===================================================================
%%% API Functions - 传输会话管理
%%===================================================================

%% 导出所有公开 API 函数
-export([create_session/4]).
-export([get_session/1]).
-export([accept_session/3]).
-export([confirm_session/2]).
-export([get_pending_sessions/1]).
-export([cancel_session/2]).
-export([cleanup_expired_sessions/0]).
-export([is_valid_session/1]).
%% G3 thin wrappers: e2ee_transfer_logic 不应直调 e2ee_transfer_repo
-export([generate_session_id/0]).
-export([create_raw/1]).
-export([get_by_session_id/1]).
-export([update_status_and_device/3]).
-export([update_status/2]).
-export([cancel_session_raw/2]).

%% @doc 创建传输会话
%% @param FromUid 发送方用户 ID
%% @param FromDeviceId 发送方设备 ID
%% @param ToUid 接收方用户 ID
%% @param EncryptedKeyBundle 加密的密钥包（使用接收方公钥加密）
-spec create_session(integer(), binary(), integer(), binary()) -> {ok, map()} | {error, term()}.
create_session(FromUid, FromDeviceId, ToUid, EncryptedKeyBundle) ->
    % 生成会话 ID（UUID v4）
    SessionId = e2ee_transfer_repo:generate_session_id(),

    % 计算过期时间（5 分钟后）
    Now = elib_dt:now(),
    case elib_dt:add(Now, {5, minute}) of
        {error, _Reason} ->
            {error, invalid_datetime};
        ExpiresAt ->
            ExpiresAtStr = elib_dt:to_rfc3339(ExpiresAt),

            SessionMap = #{
                <<"session_id">> => SessionId,
                <<"from_uid">> => FromUid,
                <<"from_device_id">> => FromDeviceId,
                <<"to_uid">> => ToUid,
                <<"encrypted_key_bundle">> => EncryptedKeyBundle,
                <<"expires_at">> => ExpiresAtStr
            },

            case e2ee_transfer_repo:create(SessionMap) of
                {ok, Id} ->
                    {ok, SessionMap#{<<"id">> => Id}};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% @doc 获取传输会话信息
%% @param SessionId 会话 ID
-spec get_session(binary()) -> {ok, map()} | {error, term()}.
get_session(SessionId) ->
    CacheKey = {e2ee_transfer_session, SessionId},
    case imboy_cache:get(CacheKey) of
        {ok, Session} ->
            {ok, Session};
        undefined ->
            case e2ee_transfer_repo:get_by_session_id(SessionId) of
                {ok, Session} ->
                    % 缓存 2 分钟
                    imboy_cache:set(CacheKey, Session, 120),
                    {ok, Session};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% @doc 接受传输会话
%% @param SessionId 会话 ID
%% @param ToUid 接收方用户 ID
%% @param ToDeviceId 接收方设备 ID
-spec accept_session(binary(), integer(), binary()) -> ok | {error, term()}.
accept_session(SessionId, ToUid, ToDeviceId) ->
    % 1. 检查会话是否存在且有效
    case get_session(SessionId) of
        {error, Reason} ->
            {error, Reason};
        {ok, Session} ->
            % 2. 验证接收方用户 ID
            SessionToUid = maps:get(<<"to_uid">>, Session),
            case SessionToUid =:= ToUid of
                false ->
                    {error, <<"会话不属于该用户"/utf8>>};
                true ->
                    % 3. 检查会话状态
                    Status = maps:get(<<"status">>, Session),
                    case Status of
                        <<"pending">> ->
                            % 4. 更新设备 ID 和状态
                            ok = e2ee_transfer_repo:update_status_and_device(
                                SessionId, <<"accepted">>, ToDeviceId
                            ),
                            % 清除缓存
                            clear_session_cache(SessionId),
                            ok;
                        <<"accepted">> ->
                            {error, <<"会话已被接受"/utf8>>};
                        _ ->
                            {error, <<"会话状态无效"/utf8>>}
                    end
            end
    end.

%% @doc 确认传输完成
%% @param SessionId 会话 ID
%% @param FromUid 发送方用户 ID（用于验证权限）
-spec confirm_session(binary(), integer()) -> ok | {error, term()}.
confirm_session(SessionId, FromUid) ->
    % 1. 获取会话信息
    case get_session(SessionId) of
        {error, Reason} ->
            {error, Reason};
        {ok, Session} ->
            % 2. 验证发送方用户 ID
            SessionFromUid = maps:get(<<"from_uid">>, Session),
            case SessionFromUid =:= FromUid of
                false ->
                    {error, <<"无权限确认此会话"/utf8>>};
                true ->
                    % 3. 检查会话状态
                    Status = maps:get(<<"status">>, Session),
                    case Status of
                        <<"accepted">> ->
                            % 4. 更新状态为 confirmed
                            ok = e2ee_transfer_repo:update_status(SessionId, <<"confirmed">>),
                            % 清除缓存
                            clear_session_cache(SessionId),
                            ok;
                        _ ->
                            {error, <<"会话状态不正确"/utf8>>}
                    end
            end
    end.

%% @doc 获取用户的待处理传输会话列表
%% @param ToUid 接收方用户 ID
-spec get_pending_sessions(integer()) -> {ok, list(map())} | {error, term()}.
get_pending_sessions(ToUid) ->
    CacheKey = {e2ee_pending_sessions, ToUid},
    case imboy_cache:get(CacheKey) of
        {ok, Sessions} ->
            {ok, Sessions};
        undefined ->
            case e2ee_transfer_repo:get_pending_sessions(ToUid) of
                {ok, Sessions} ->
                    % 缓存 1 分钟
                    imboy_cache:set(CacheKey, Sessions, 60),
                    {ok, Sessions};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% @doc 取消传输会话
%% @param SessionId 会话 ID
%% @param FromUid 发送方用户 ID（用于验证权限）
-spec cancel_session(binary(), integer()) -> ok | {error, term()}.
cancel_session(SessionId, FromUid) ->
    case get_session(SessionId) of
        {error, Reason} ->
            {error, Reason};
        {ok, Session} ->
            % 验证权限
            SessionFromUid = maps:get(<<"from_uid">>, Session),
            case SessionFromUid =:= FromUid of
                false ->
                    {error, <<"无权限取消此会话"/utf8>>};
                true ->
                    % 只能取消 pending 状态的会话
                    Status = maps:get(<<"status">>, Session),
                    case Status of
                        <<"pending">> ->
                            ok = e2ee_transfer_repo:update_status(SessionId, <<"cancelled">>),
                            clear_session_cache(SessionId),
                            ok;
                        _ ->
                            {error, <<"只能取消待处理状态的会话"/utf8>>}
                    end
            end
    end.

%% @doc 清理过期的传输会话（定时任务调用）
-spec cleanup_expired_sessions() -> {ok, integer()} | {error, term()}.
cleanup_expired_sessions() ->
    Sql = <<"DELETE FROM e2ee_transfer_sessions
              WHERE expires_at < CURRENT_TIMESTAMP
              AND status != 'confirmed'
              RETURNING id">>,
    case elib_pg:execute(Sql, []) of
        {ok, Count, _Rows} ->
            % 清除所有会话缓存
            clear_all_session_cache(),
            {ok, Count};
        {ok, 0} ->
            {ok, 0};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 检查会话是否有效（未过期且状态正确）
-spec is_valid_session(binary()) -> boolean().
is_valid_session(SessionId) ->
    e2ee_transfer_repo:is_valid_session(SessionId).

%%===================================================================
%%% Internal Functions
%%===================================================================

%% @doc 清除单个会话缓存
-spec clear_session_cache(binary()) -> ok.
clear_session_cache(SessionId) ->
    CacheKey = {e2ee_transfer_session, SessionId},
    imboy_cache:delete(CacheKey),
    ok.

%% @doc 清除所有 E2EE 传输会话缓存（intentional no-op）
%%
%% depcache 不支持按前缀枚举键，无法批量删除 {e2ee_transfer_session, _} 形式的键。
%% 全量 flush（imboy_cache:flush/0）会影响整个应用的缓存，代价过高。
%% 各 session 缓存条目会在 TTL 到期后自动失效，此函数保留为接口占位。
%% 如需精确清除，应维护一个 session ID 注册表并逐一调用 clear_session_cache/1。
-spec clear_all_session_cache() -> ok.
clear_all_session_cache() ->
    ok.

%% @doc 清除用户待处理会话缓存
-spec clear_pending_sessions_cache(integer()) -> ok.
clear_pending_sessions_cache(ToUid) ->
    CacheKey = {e2ee_pending_sessions, ToUid},
    imboy_cache:delete(CacheKey),
    ok.

%% G3: e2ee_cleanup_worker 不应直调 e2ee_transfer_repo
-spec get_stalled_sessions() -> {ok, [map()]} | {error, term()}.
get_stalled_sessions() -> e2ee_transfer_repo:get_stalled_sessions().

%% G3 thin wrappers: e2ee_transfer_logic 不应直调 e2ee_transfer_repo
-spec generate_session_id() -> binary().
generate_session_id() -> e2ee_transfer_repo:generate_session_id().

-spec create_raw(map()) -> {ok, integer()} | {error, term()}.
create_raw(SessionMap) -> e2ee_transfer_repo:create(SessionMap).

-spec get_by_session_id(binary()) -> {ok, map()} | {error, not_found | term()}.
get_by_session_id(SessionId) -> e2ee_transfer_repo:get_by_session_id(SessionId).

-spec update_status_and_device(binary(), binary(), binary()) -> ok | {error, term()}.
update_status_and_device(SessionId, Status, ToDeviceId) ->
    e2ee_transfer_repo:update_status_and_device(SessionId, Status, ToDeviceId).

-spec update_status(binary(), binary()) -> ok | {error, term()}.
update_status(SessionId, Status) -> e2ee_transfer_repo:update_status(SessionId, Status).

-spec cancel_session_raw(binary(), integer()) -> ok | {error, term()}.
cancel_session_raw(SessionId, FromUid) ->
    e2ee_transfer_repo:cancel_session(SessionId, FromUid).
