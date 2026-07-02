-module(e2ee_transfer_logic).
%%%===================================================================
%%% @doc E2EE 设备间传输 Logic 层
%%%
%%% 处理设备间传输的业务逻辑
%%%===================================================================

-include("error_code.hrl").

%%===================================================================
%%% API Functions Export
%%===================================================================
-export([create_transfer/4]).
-export([accept_transfer/3]).
-export([confirm_transfer/2]).
-export([cancel_transfer/2]).
-export([get_transfer_info/1]).
-export([get_pending_transfers/1]).
-export([validate_receiver/1]).
-export([get_receiver_public_key/1]).

%%===================================================================
%%% API Functions
%%===================================================================

%% @doc 创建传输会话
%% @param FromUid 发送方用户 ID
%% @param FromDeviceId 发送方设备 ID
%% @param ToUid 接收方用户 ID
%% @param PrivateKeyPem 私钥 PEM 格式
%% @param ToPublicKeyPem 接收方公钥 PEM 格式（用于加密）
%% @returns {ok, SessionMap} | {error, Reason}
-spec create_transfer(integer(), binary(), integer(), binary()) ->
    {ok, map()} | {error, term()}.
%% 零信任：EncryptedBundle 为发送方客户端用接收方公钥加密自身私钥后的密文，
%% 服务端只存储/中转，永不接触明文私钥。
create_transfer(FromUid, FromDeviceId, ToUid, EncryptedBundle) ->
    % 1. 验证接收方用户是否存在
    case user_ds:may_exist(ToUid) of
        false ->
            {error, {<<"接收方用户不存在"/utf8>>, ?ERR_USER_NOT_FOUND}};
        true ->
            % 2. 直接尝试创建会话，依赖数据库唯一约束防止并发重复
            try
                % 3. 生成会话 ID
                SessionId = e2ee_transfer_ds:generate_session_id(),

                % 4. 直接使用客户端预加密的密钥包（服务端不加密）

                % 5. 设置过期时间（5 分钟后）
                ExpiresAt = erlang:system_time(second) + 300,
                ExpiresAtStr = calendar:system_time_to_rfc3339(ExpiresAt),

                % 6. 创建会话记录
                SessionMap = #{
                    <<"session_id">> => SessionId,
                    <<"from_uid">> => FromUid,
                    <<"from_device_id">> => FromDeviceId,
                    <<"to_uid">> => ToUid,
                    <<"to_device_id">> => <<>>,
                    <<"encrypted_key_bundle">> => EncryptedBundle,
                    <<"expires_at">> => ExpiresAtStr,
                    <<"status">> => <<"pending">>
                },

                case e2ee_transfer_ds:create_raw(SessionMap) of
                    {ok, _SessionId} ->
                        {ok, SessionMap};
                    {error, unique_violation} ->
                        % 数据库唯一约束违反：已有进行中的会话
                        {error, {<<"已有进行中的传输会话，请先取消或等待完成"/utf8>>, ?ERR_E2EE_TRANSFER_CONCURRENT}};
                    {error, CreateReason} ->
                        {error, CreateReason}
                end
            catch
                error:Reason -> {error, Reason};
                exit:Reason -> {error, Reason};
                throw:Reason -> {error, Reason}
            end
    end.

%% @doc 接受传输
%% @param SessionId 会话 ID
%% @param ToUid 接收方用户 ID
%% @param ToDeviceId 接收方设备 ID
%% @returns {ok, SessionMap} | {error, Reason}
-spec accept_transfer(binary(), integer(), binary()) ->
    {ok, map()} | {error, term()}.
accept_transfer(SessionId, ToUid, ToDeviceId) ->
    % 1. 获取会话信息
    case e2ee_transfer_ds:get_by_session_id(SessionId) of
        {error, not_found} ->
            {error, {<<"会话不存在或已过期"/utf8>>, ?ERR_E2EE_TRANSFER_SESSION_NOT_FOUND}};
        {ok, Session} ->
            % 2. 验证接收方用户
            ToUidSession = maps:get(<<"to_uid">>, Session),
            case ToUidSession =:= ToUid of
                false ->
                    {error, {<<"会话不属于该用户"/utf8>>, ?ERR_E2EE_TRANSFER_TO_UID_NOT_MATCH}};
                true ->
                    % 3. 验证接收设备确实归属当前用户，防止伪造 device_id 冒领转移
                    case user_device_ds:device_name(ToUid, ToDeviceId) of
                        <<>> ->
                            {error, {<<"设备不存在或不属于当前用户"/utf8>>, ?ERR_E2EE_TRANSFER_INVALID_DEVICE}};
                        _DeviceName when
                            ToDeviceId =:= map_get(<<"from_device_id">>, Session)
                        ->
                            % 同账号换机允许（ToUid==FromUid），但源设备与目标设备相同的
                            % 转移无意义且会覆盖自身 bundle，拒绝
                            {error, {<<"不能转移到源设备本身"/utf8>>, ?ERR_E2EE_TRANSFER_INVALID_DEVICE}};
                        _DeviceName ->
                            % 4. 检查状态
                            Status = maps:get(<<"status">>, Session),
                            case Status of
                                <<"pending">> ->
                                    % 5. 更新状态为 accepted（CAS，防止并发覆盖 to_device_id）
                                    case
                                        e2ee_transfer_ds:update_status_and_device(
                                            SessionId, <<"accepted">>, ToDeviceId
                                        )
                                    of
                                        ok ->
                                            {ok, Session#{
                                                <<"status">> => <<"accepted">>,
                                                <<"to_device_id">> => ToDeviceId
                                            }};
                                        {error, conflict} ->
                                            {error,
                                                {<<"会话已被接受"/utf8>>,
                                                    ?ERR_E2EE_TRANSFER_ALREADY_ACCEPTED}};
                                        {error, UpdateReason} ->
                                            {error, UpdateReason}
                                    end;
                                <<"accepted">> ->
                                    {error,
                                        {<<"会话已被接受"/utf8>>, ?ERR_E2EE_TRANSFER_ALREADY_ACCEPTED}};
                                _ ->
                                    {error, {<<"会话状态无效"/utf8>>, ?ERR_E2EE_TRANSFER_STATUS_INVALID}}
                            end
                    end
            end
    end.

%% @doc 确认传输完成
%% @param SessionId 会话 ID
%% @param ToUid 接收方用户 ID
%% @returns ok | {error, Reason}
-spec confirm_transfer(binary(), integer()) -> ok | {error, term()}.
confirm_transfer(SessionId, ToUid) ->
    % 1. 获取会话信息
    case e2ee_transfer_ds:get_by_session_id(SessionId) of
        {error, not_found} ->
            {error, {<<"会话不存在或已过期"/utf8>>, ?ERR_E2EE_TRANSFER_SESSION_NOT_FOUND}};
        {ok, Session} ->
            % 2. 验证接收方用户
            ToUidSession = maps:get(<<"to_uid">>, Session),
            case ToUidSession =:= ToUid of
                false ->
                    {error, {<<"会话不属于该用户"/utf8>>, ?ERR_E2EE_TRANSFER_TO_UID_NOT_MATCH}};
                true ->
                    % 3. 验证状态
                    Status = maps:get(<<"status">>, Session),
                    case Status =:= <<"accepted">> of
                        false ->
                            {error, {<<"会话状态错误"/utf8>>, ?ERR_E2EE_TRANSFER_STATUS_INVALID}};
                        true ->
                            % 4. 更新状态为 confirmed
                            case e2ee_transfer_ds:update_status(SessionId, <<"confirmed">>) of
                                ok -> ok;
                                {error, UpdateReason} -> {error, UpdateReason}
                            end
                    end
            end
    end.

%% @doc 取消传输会话
%% 只有发送方可以取消 pending 状态的会话
%% @param SessionId 会话 ID
%% @param FromUid 发送方用户 ID（用于权限验证）
%% @returns ok | {error, Reason}
-spec cancel_transfer(binary(), integer()) -> ok | {error, term()}.
cancel_transfer(SessionId, FromUid) ->
    % 1. 获取会话信息验证权限
    case e2ee_transfer_ds:get_by_session_id(SessionId) of
        {error, not_found} ->
            {error, {<<"会话不存在或已过期"/utf8>>, ?ERR_E2EE_TRANSFER_SESSION_NOT_FOUND}};
        {ok, Session} ->
            % 2. 验证是否为发送方
            SessionFromUid = maps:get(<<"from_uid">>, Session),
            case SessionFromUid =:= FromUid of
                false ->
                    {error, {<<"无权限取消此会话"/utf8>>, ?ERR_FORBIDDEN}};
                true ->
                    % 3. 验证状态
                    Status = maps:get(<<"status">>, Session),
                    case Status of
                        <<"pending">> ->
                            % 4. 执行取消
                            case e2ee_transfer_ds:cancel_session_raw(SessionId, FromUid) of
                                ok ->
                                    ok;
                                {error, not_found_or_not_pending} ->
                                    {error,
                                        {<<"会话已不可取消"/utf8>>, ?ERR_E2EE_TRANSFER_ALREADY_CANCELLED}};
                                {error, Reason} ->
                                    {error, Reason}
                            end;
                        <<"cancelled">> ->
                            {error, {<<"会话已取消"/utf8>>, ?ERR_E2EE_TRANSFER_ALREADY_CANCELLED}};
                        <<"accepted">> ->
                            {error, {<<"会话已被接受，无法取消"/utf8>>, ?ERR_E2EE_TRANSFER_ALREADY_ACCEPTED}};
                        <<"confirmed">> ->
                            {error, {<<"会话已完成，无法取消"/utf8>>, ?ERR_E2EE_TRANSFER_CANNOT_CONFIRM}};
                        _ ->
                            {error, {<<"无效的会话状态"/utf8>>, ?ERR_E2EE_TRANSFER_STATUS_INVALID}}
                    end
            end
    end.

%% @doc 获取传输会话信息
%% @param SessionId 会话 ID
%% @returns {ok, SessionMap} | {error, not_found}
-spec get_transfer_info(binary()) -> {ok, map()} | {error, not_found}.
get_transfer_info(SessionId) ->
    e2ee_transfer_ds:get_by_session_id(SessionId).

%% @doc 获取用户的待处理传输列表
%% @param Uid 用户 ID
%% @returns {ok, [SessionMap]} | {error, Reason}
-spec get_pending_transfers(integer()) -> {ok, list(map())} | {error, term()}.
get_pending_transfers(Uid) ->
    e2ee_transfer_ds:get_pending_sessions(Uid).

%% @doc 验证接收方用户是否存在于系统中
-spec validate_receiver(integer()) -> boolean().
validate_receiver(ToUid) ->
    user_ds:may_exist(ToUid).

%% @doc 获取接收方用户的设备公钥列表
-spec get_receiver_public_key(integer()) -> {ok, [map()]} | {error, term()}.
get_receiver_public_key(ToUid) ->
    user_device_ds:get_public_by_uid(ToUid).
