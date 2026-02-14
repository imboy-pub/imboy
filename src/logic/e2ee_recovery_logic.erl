-module(e2ee_recovery_logic).
%%%===================================================================
%%% @doc E2EE 自动恢复决策逻辑
%%%
%%% 功能：
%%% - 检查密钥状态
%%% - 获取可用恢复方式
%%% - 推荐最优恢复路径
%%%
%%% 恢复方式优先级：
%%% 1. 设备间传输（最快，最安全）
%%% 2. 社交恢复（零信任，需要代理配合）
%%% 3. 本地备份（完全离线）
%%%===================================================================

-include("log.hrl").
-include("error_code.hrl").

%% API
-export([check_key_status/2]).
-export([get_recovery_options/1]).
-export([recommend_method/1]).
-export([start_auto_recovery/3]).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc 检查密钥状态并返回恢复建议
%% @param Uid 用户ID
%% @param DeviceId 设备ID
%% @returns {ok, KeyStatus}
-spec check_key_status(integer(), binary()) -> {ok, map()}.
check_key_status(Uid, DeviceId) when is_integer(Uid), is_binary(DeviceId) ->
    % 1. 检查设备是否有有效密钥
    HasValidKey = check_device_has_key(Uid, DeviceId),

    % 2. 获取可用恢复方式
    RecoveryOptions = get_recovery_options(Uid),

    % 3. 推荐最优恢复方式
    Recommended = recommend_method(RecoveryOptions),

    {ok, #{
        <<"has_valid_key">> => HasValidKey,
        <<"recovery_options">> => RecoveryOptions,
        <<"recommended_method">> => Recommended
    }}.

%% @doc 获取用户可用的恢复方式
%% @param Uid 用户ID
%% @returns [RecoveryOption]
-spec get_recovery_options(integer()) -> [map()].
get_recovery_options(Uid) when is_integer(Uid) ->
    Options = [],

    % 1. 检查设备间传输（最高优先级）
    Options1 = case check_device_transfer_available(Uid) of
        {ok, true, Details1} ->
            [#{<<"method">> => <<"device_transfer">>,
               <<"available">> => true,
               <<"priority">> => 1,
               <<"details">> => Details1} | Options];
        _ ->
            Options
    end,

    % 2. 检查社交恢复
    Options2 = case check_social_recovery_available(Uid) of
        {ok, true, Details2} ->
            [#{<<"method">> => <<"social_recovery">>,
               <<"available">> => true,
               <<"priority">> => 2,
               <<"details">> => Details2} | Options1];
        _ ->
            Options1
    end,

    % 3. 检查本地备份
    Options3 = case check_local_backup_available(Uid) of
        {ok, true, Details3} ->
            [#{<<"method">> => <<"local_backup">>,
               <<"available">> => true,
               <<"priority">> => 3,
               <<"details">> => Details3} | Options2];
        _ ->
            Options2
    end,

    % 按优先级排序
    lists:sort(fun(A, B) ->
        maps:get(<<"priority">>, A) < maps:get(<<"priority">>, B)
    end, Options3).

%% @doc 推荐恢复方式
%% @param Options 恢复选项列表
%% @returns 推荐的方法名称
-spec recommend_method([map()]) -> binary().
recommend_method([]) -> <<"none">>;
recommend_method([Best | _]) -> maps:get(<<"method">>, Best).

%% @doc 启动自动恢复
%% @param Uid 用户ID
%% @param DeviceId 目标设备ID
%% @param Method 恢复方式
%% @returns {ok, Result} | {error, Reason}
-spec start_auto_recovery(integer(), binary(), binary()) ->
    {ok, map()} | {error, term()}.
start_auto_recovery(Uid, DeviceId, Method) ->
    case Method of
        <<"device_transfer">> ->
            start_device_transfer_recovery(Uid, DeviceId);
        <<"social_recovery">> ->
            start_social_recovery(Uid, DeviceId);
        <<"local_backup">> ->
            {ok, #{<<"message">> => <<"请使用本地备份文件恢复"/utf8>>}};
        _ ->
            {error, {<<"不支持的恢复方式"/utf8>>, ?ERR_E2EE_OPERATION_NOT_SUPPORTED}}
    end.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @doc 检查设备是否有有效密钥
-spec check_device_has_key(integer(), binary()) -> boolean().
check_device_has_key(Uid, DeviceId) ->
    case user_device_ds:list_public_keys(Uid) of
        {ok, Devices} ->
            lists:any(fun(D) ->
                case maps:get(<<"device_id">>, D, <<>>) of
                    DeviceId ->
                        PublicKey = maps:get(<<"public_key">>, D, <<>>),
                        KeyId = maps:get(<<"key_id">>, D, <<>>),
                        PublicKey =/= <<>> andalso KeyId =/= <<>>;
                    _ -> false
                end
            end, Devices);
        _ -> false
    end.

%% @doc 检查设备间传输是否可用
-spec check_device_transfer_available(integer()) ->
    {ok, boolean(), map()} | {error, term()}.
check_device_transfer_available(Uid) ->
    case e2ee_transfer_ds:get_pending_sessions(Uid) of
        {ok, []} ->
            {ok, false, #{}};
        {ok, Sessions} ->
            % 有待处理的传输会话
            {ok, true, #{
                <<"pending_count">> => length(Sessions),
                <<"sessions">> => Sessions
            }};
        {error, Reason} ->
            ok = ?ERROR_LOG([check_device_transfer_available, Reason]),
            {ok, false, #{}}
    end.

%% @doc 检查社交恢复是否可用
-spec check_social_recovery_available(integer()) ->
    {ok, boolean(), map()} | {error, term()}.
check_social_recovery_available(Uid) ->
    try
        % 检查是否有可信联系人
        case e2ee_social_ds:list_contacts(Uid) of
            {ok, []} ->
                {ok, false, #{}};
            {ok, Contacts} ->
                % 检查是否有足够的活跃分片
                ActiveCount = length(lists:filter(fun(C) ->
                    maps:get(<<"status">>, C, <<"inactive">>) =:= <<"active">>
                end, Contacts)),

                CanRecover = ActiveCount >= 2,  % 至少需要 2 个活跃联系人
                {ok, CanRecover, #{
                    <<"contact_count">> => length(Contacts),
                    <<"active_count">> => ActiveCount
                }};
            {error, Reason} ->
                ok = ?ERROR_LOG([check_social_recovery_available, Reason]),
                {ok, false, #{}}
        end
    catch
        _:Error ->
            ok = ?ERROR_LOG([check_social_recovery_available, {'catch', Error}]),
            {ok, false, #{}}
    end.

%% @doc 检查本地备份是否可用
-spec check_local_backup_available(integer()) ->
    {ok, boolean(), map()} | {error, term()}.
check_local_backup_available(Uid) ->
    case e2ee_local_backup_repo:find_latest(Uid) of
        {ok, []} ->
            {ok, false, #{}};
        {ok, Backup} ->
            {ok, true, #{
                <<"backup_version">> => maps:get(<<"backup_version">>, Backup, 0),
                <<"created_at">> => maps:get(<<"created_at">>, Backup, <<>>)
            }};
        {error, Reason} ->
            ok = ?ERROR_LOG([check_local_backup_available, Reason]),
            {ok, false, #{}}
    end.

%% @doc 启动设备间传输恢复
-spec start_device_transfer_recovery(integer(), binary()) ->
    {ok, map()} | {error, term()}.
start_device_transfer_recovery(Uid, _DeviceId) ->
    case e2ee_transfer_ds:get_pending_sessions(Uid) of
        {ok, []} ->
            {error, {<<"无待处理的传输会话"/utf8>>, ?ERR_E2EE_TRANSFER_SESSION_NOT_FOUND}};
        {ok, [Session | _]} ->
            SessionId = maps:get(<<"session_id">>, Session),
            {ok, #{
                <<"action">> => <<"accept_transfer">>,
                <<"session_id">> => SessionId,
                <<"message">> => <<"请接受传输会话以恢复密钥"/utf8>>
            }};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 启动社交恢复
-spec start_social_recovery(integer(), binary()) ->
    {ok, map()} | {error, term()}.
start_social_recovery(Uid, _DeviceId) ->
    case e2ee_social_ds:list_contacts(Uid) of
        {ok, []} ->
            {error, {<<"无可信联系人"/utf8>>, ?ERR_E2EE_SOCIAL_CONTACT_NOT_FOUND}};
        {ok, Contacts} ->
            ActiveContacts = lists:filter(fun(C) ->
                maps:get(<<"status">>, C, <<"inactive">>) =:= <<"active">>
            end, Contacts),

            case length(ActiveContacts) >= 2 of
                true ->
                    {ok, #{
                        <<"action">> => <<"request_shards">>,
                        <<"contacts">> => ActiveContacts,
                        <<"message">> => <<"请向可信联系人请求密钥分片"/utf8>>
                    }};
                false ->
                    {error, {<<"活跃联系人不足"/utf8>>, ?ERR_E2EE_SOCIAL_NOT_ENOUGH_SHARES}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.
