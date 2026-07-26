-module(e2ee_logic).

%% Stable security_privacy e2ee domain boundary.
-dialyzer(no_return).

-export([user_keys/2]).
-export([group_member_keys/2]).
-export([report_device_key/6]).
-export([pull_key_notifications/3]).
-export([group_by_uid/1]).
-export([get_active_compliance_key/0]).

-include("log.hrl").

-spec user_keys(integer(), integer()) -> {ok, map()} | {error, binary(), integer()}.
user_keys(CurrentUid, TargetUid) when is_integer(CurrentUid), is_integer(TargetUid) ->
    % 任何登录用户都可以获取其他用户的公钥（用于端到端加密）
    % 公钥本身不包含敏感信息，可以公开获取
    user_keys_payload(TargetUid).

-spec group_member_keys(integer(), integer()) -> {ok, map()} | {error, binary(), integer()}.
group_member_keys(CurrentUid, Gid) when is_integer(CurrentUid), is_integer(Gid) ->
    case group_ds:is_member(CurrentUid, Gid) of
        true ->
            MemberUids = group_ds:member_uids(Gid),
            case user_device_ds:list_public_keys_by_uids(MemberUids) of
                {ok, Rows} ->
                    {ok, #{
                        <<"gid">> => Gid,
                        <<"members">> => group_by_uid(Rows)
                    }};
                {error, Reason} ->
                    _ = ?ERROR_LOG({e2ee_group_member_keys_db_error, Reason}),
                    {error, <<"internal_error">>, 500}
            end;
        false ->
            {error, <<"forbidden">>, 403}
    end.

%% @doc 上报设备的 E2EE 公钥并通知好友
%%
%% 当用户重新生成密钥对时（如重新安装应用），需要：
%% 1. 更新数据库中的公钥
%% 2. 通知所有好友清除对该用户的公钥缓存
%% 3. 好友下次发送消息时会自动获取新公钥
%%
%% @param Uid 用户ID
%% @param DeviceId 设备ID
%% @param DeviceType 设备类型 (android/ios/macos/web)
%% @param DeviceName 设备名称（可选）
%% @param PublicKey PEM格式的公钥
%% @param KeyId 密钥ID
%% @return {ok, OtherDeviceCount} | {error, Reason}
%%   OtherDeviceCount 为该用户除本设备外、已上报有效公钥的活跃设备数量；
%%   客户端据此判断是否为"换设备"场景（>0 才显示 E2EE 恢复横幅）。
-spec report_device_key(integer(), binary(), binary(), binary() | undefined, binary(), binary()) ->
    {ok, non_neg_integer()} | {error, term()}.
report_device_key(Uid, DeviceId, DeviceType, DeviceName, PublicKey, KeyId) when is_integer(Uid) ->
    Now = elib_dt:now(),

    % 1. 检查设备是否存在并更新公钥
    SaveResult =
        case user_device_ds:update_public_key(Uid, DeviceId, PublicKey, KeyId, Now) of
            {ok, 0} ->
                % 设备不存在，创建新设备记录
                PostVals = #{
                    <<"did">> => DeviceId,
                    <<"cos">> => DeviceType,
                    <<"dname">> => DeviceName,
                    <<"public_key">> => PublicKey,
                    <<"ip">> => <<>>
                },
                case user_device_ds:save(Now, Uid, DeviceId, PostVals) of
                    {ok, _} ->
                        _ = ?INFO_LOG([e2ee_report_device_key_created, Uid, DeviceId, DeviceType]),
                        ok;
                    {error, Reason} ->
                        _ = ?ERROR_LOG({e2ee_report_device_key_create_error, Reason}),
                        {error, <<"internal_error">>}
                end;
            {ok, _Count} ->
                % 设备已存在，更新公钥
                _ = ?INFO_LOG([e2ee_report_device_key_updated, Uid, DeviceId, DeviceType]),
                ok;
            {error, Reason} ->
                _ = ?ERROR_LOG({e2ee_report_device_key_error, Reason}),
                {error, <<"internal_error">>}
        end,

    % 如果保存/更新失败，直接返回错误
    case SaveResult of
        {error, _} = Error ->
            Error;
        ok ->
            % 2. 统计该用户除本设备外、已有有效公钥的活跃设备数量
            % 用于区分"换设备/重装"（>0）与"全新注册首次登录"（=0）
            OtherDeviceCount = user_device_ds:count_other_device_keys(Uid, DeviceId),
            % 3. 通知所有好友：该用户的设备密钥已变更
            % 好友收到通知后应清除对该用户的公钥缓存
            notify_friends_key_changed(Uid, DeviceId, DeviceType, KeyId),
            {ok, OtherDeviceCount}
    end.

%% @doc 通知好友密钥已变更
%% 发送 S2C 消息给所有好友，告知他们该用户的设备密钥已变更
-spec notify_friends_key_changed(integer(), binary(), binary(), binary()) -> ok.
notify_friends_key_changed(Uid, DeviceId, DeviceType, KeyId) ->
    % 获取用户的所有好友
    FriendUids = friend_ds:list_by_uid(Uid),

    % 构造通知负载
    Payload = #{
        <<"uid">> => Uid,
        <<"device_id">> => DeviceId,
        <<"device_type">> => DeviceType,
        <<"key_id">> => KeyId
    },

    % 发送通知给所有好友
    % 使用 save 确保离线好友也能收到密钥变更通知
    Action = <<"e2ee_device_key_changed">>,
    _ = msg_s2c_ds:send(Uid, FriendUids, Action, <<>>, null, Payload, save),

    ok.

-spec user_keys_payload(integer()) -> {ok, map()} | {error, binary(), integer()}.
user_keys_payload(TargetUid) ->
    case user_device_ds:list_public_keys(TargetUid) of
        {ok, Devices} ->
            {ok, #{
                <<"uid">> => TargetUid,
                <<"user_id">> => TargetUid,
                <<"devices">> => Devices
            }};
        {error, Reason} ->
            _ = ?ERROR_LOG({e2ee_user_keys_db_error, Reason}),
            {error, <<"internal_error">>, 500}
    end.

-spec group_by_uid([map()]) -> [map()].
group_by_uid(Rows) ->
    Map0 =
        lists:foldl(
            fun(Row, Acc) ->
                Uid = maps:get(<<"user_id">>, Row),
                Existing = maps:get(Uid, Acc, []),
                Row2 = maps:put(<<"uid">>, Uid, maps:remove(<<"user_id">>, Row)),
                maps:put(Uid, [Row2 | Existing], Acc)
            end,
            #{},
            Rows
        ),
    lists:map(
        fun({Uid, DevsRev}) ->
            #{<<"uid">> => Uid, <<"user_id">> => Uid, <<"devices">> => lists:reverse(DevsRev)}
        end,
        lists:sort(maps:to_list(Map0))
    ).

%% @doc 查询当前活跃的合规公钥（key_id 和 public_key），供客户端双密钥加密使用
-spec get_active_compliance_key() -> {ok, map()} | {error, term()}.
get_active_compliance_key() ->
    compliance_key_ds:find_active().

%% ===================================================================
%% 通知拉取 API
%% ===================================================================

%% @doc 拉取密钥变更通知
%% 作为 WebSocket 推送的备选方案，支持客户端主动拉取
%% @param Uid 当前用户ID
%% @param Since 起始时间戳（毫秒）
%% @param Limit 返回数量限制
%% @returns {ok, [Notification]}
-spec pull_key_notifications(integer(), binary() | integer(), binary() | integer()) ->
    {ok, [map()]} | {error, term()}.
pull_key_notifications(Uid, Since, Limit) when is_integer(Uid) ->
    %% since/limit 可能是 handler 透传的未校验 query（如 since=abc、limit=-1）。
    %% safe_to_integer 兜底非数字降级为 0；再夹取范围：
    %% - limit 夹到 [1, 1000]：负数/0 会经 SQL "LIMIT $3" 触发 PG
    %%   "LIMIT must not be negative" → 500；超大 limit 是无界查询 DoS。
    %% - since 夹到 >= 0，避免负时间戳。
    SinceTs = erlang:max(0, elib_cnv:safe_to_integer(Since)),
    LimitInt = min(max(elib_cnv:safe_to_integer(Limit), 1), 1000),

    % 获取好友列表
    FriendUids = friend_ds:list_by_uid(Uid),

    case FriendUids of
        [] ->
            {ok, []};
        _ ->
            % 从数据库查询好友的密钥变更记录
            pull_key_changes_from_db(FriendUids, SinceTs, LimitInt)
    end.

%% @doc 从数据库拉取密钥变更
-spec pull_key_changes_from_db([map()], integer(), integer()) ->
    {ok, [map()]} | {error, term()}.
pull_key_changes_from_db(FriendUids, SinceTs, Limit) ->
    % 查询好友中最近更新了密钥的用户
    SinceRfc = elib_dt:to_rfc3339(SinceTs),

    % 使用 user_device 表查询密钥变更
    %% user_device 表无 updated_at 列，密钥变更时间记录在 last_active_at
    %% （update_public_key 同步更新该列）。此前查询 ud.updated_at 会导致
    %% SQL 报错、pull_key_notifications 端点永久返回 {error}。
    Sql =
        <<"SELECT ud.user_id, ud.device_id, ud.device_type, ud.key_id, ud.last_active_at ",
            "FROM user_device ud ", "WHERE ud.user_id = ANY($1) ",
            "AND ud.key_id IS NOT NULL AND ud.key_id != '' ", "AND ud.last_active_at > $2 ",
            "ORDER BY ud.last_active_at DESC ", "LIMIT $3">>,

    %% query/2 返回二元组 {ok, [map()]}；输出字段名保留 updated_at（客户端契约），
    %% 取值来源为 last_active_at 列。
    case elib_pg:query(Sql, [FriendUids, SinceRfc, Limit]) of
        {ok, Rows} ->
            Notifications = lists:map(
                fun(Row) ->
                    #{
                        <<"uid">> => maps:get(<<"user_id">>, Row),
                        <<"device_id">> => maps:get(<<"device_id">>, Row),
                        <<"device_type">> => maps:get(<<"device_type">>, Row),
                        <<"key_id">> => maps:get(<<"key_id">>, Row),
                        <<"updated_at">> => maps:get(<<"last_active_at">>, Row)
                    }
                end,
                Rows
            ),
            {ok, Notifications};
        {error, Reason} ->
            _ = ?ERROR_LOG({pull_key_notifications_db_error, Reason}),
            {error, <<"internal_error">>}
    end.
