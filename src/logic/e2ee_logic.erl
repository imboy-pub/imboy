-module(e2ee_logic).

-export([user_keys/2]).
-export([group_member_keys/2]).
-export([report_device_key/6]).

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
                        <<"gid">> => elib_hashids:encode(Gid),
                        <<"members">> => group_by_uid(Rows)
                    }};
                {error, Reason} ->
                    ok = ?ERROR_LOG({e2ee_group_member_keys_db_error, Reason}),
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
%% @return ok | {error, Reason}
-spec report_device_key(integer(), binary(), binary(), binary() | undefined, binary(), binary()) -> ok | {error, term()}.
report_device_key(Uid, DeviceId, DeviceType, DeviceName, PublicKey, KeyId) when is_integer(Uid) ->
    Now = elib_dt:now(),

    % 1. 检查设备是否存在并更新公钥
    SaveResult = case user_device_ds:update_public_key(Uid, DeviceId, PublicKey, KeyId, Now) of
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
                ok ->
                    ok = ?INFO_LOG([e2ee_report_device_key_created, Uid, DeviceId, DeviceType]),
                    ok;
                {error, Reason} ->
                    ok = ?ERROR_LOG({e2ee_report_device_key_create_error, Reason}),
                    {error, Reason}
            end;
        {ok, _Count} ->
            % 设备已存在，更新公钥
            ok = ?INFO_LOG([e2ee_report_device_key_updated, Uid, DeviceId, DeviceType]),
            ok;
        {error, Reason} ->
            ok = ?ERROR_LOG({e2ee_report_device_key_error, Reason}),
            {error, Reason}
    end,

    % 如果保存/更新失败，直接返回错误
    case SaveResult of
        {error, _} = Error -> Error;
        ok ->
            % 2. 通知所有好友：该用户的设备密钥已变更
            % 好友收到通知后应清除对该用户的公钥缓存
            notify_friends_key_changed(Uid, DeviceId, DeviceType, KeyId),
            ok
    end.

%% @doc 通知好友密钥已变更
%% 发送 S2C 消息给所有好友，告知他们该用户的设备密钥已变更
-spec notify_friends_key_changed(integer(), binary(), binary(), binary()) -> ok.
notify_friends_key_changed(Uid, DeviceId, DeviceType, KeyId) ->
    % 获取用户的所有好友
    FriendUids = friend_ds:list_by_uid(Uid),

    % 构造通知负载
    Payload = #{
        <<"uid">> => elib_hashids:encode(Uid),
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
                <<"uid">> => elib_hashids:encode(TargetUid),
                <<"devices">> => Devices
            }};
        {error, Reason} ->
            ok = ?ERROR_LOG({e2ee_user_keys_db_error, Reason}),
            {error, <<"internal_error">>, 500}
    end.

-spec group_by_uid([map()]) -> [map()].
group_by_uid(Rows) ->
    Map0 =
        lists:foldl(
            fun(Row, Acc) ->
                Uid = maps:get(<<"user_id">>, Row),
                Existing = maps:get(Uid, Acc, []),
                Row2 = maps:put(<<"uid">>, elib_hashids:encode(Uid), maps:remove(<<"user_id">>, Row)),
                maps:put(Uid, [Row2 | Existing], Acc)
            end,
            #{},
            Rows
        ),
    lists:map(
        fun({Uid, DevsRev}) ->
            #{<<"uid">> => elib_hashids:encode(Uid), <<"devices">> => lists:reverse(DevsRev)}
        end,
        lists:sort(maps:to_list(Map0))
    ).
