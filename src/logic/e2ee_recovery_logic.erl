-module(e2ee_recovery_logic).
%%%===================================================================
%%% @doc E2EE 自动恢复决策逻辑
%%%
%%% 功能：
%%% - 检查密钥状态
%%% - 获取可用恢复方式
%%% - 推荐最优恢复路径
%%%
%%% 恢复方式：服务端加密备份（4S 模式，需恢复口令/恢复密钥，见 e2ee_backup_logic）。
%%%
%%% 自研的设备间传输、社交恢复（Shamir 分片）已下线：统一收敛到口令/恢复密钥
%%% 加密的云备份（与 Matrix 4S 等价），服务端全程零密码学，不接触明文私钥。
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
-spec check_key_status(integer(), binary()) -> {ok, map()} | {error, term()}.
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
    % 唯一恢复方式：服务端加密备份（4S 模式，客户端凭恢复口令/恢复密钥本地解密）
    case check_server_backup_available(Uid) of
        {ok, true, Details} ->
            [
                #{
                    <<"method">> => <<"server_backup">>,
                    <<"available">> => true,
                    <<"priority">> => 1,
                    <<"details">> => Details
                }
            ];
        _ ->
            []
    end.

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
        <<"server_backup">> ->
            start_server_backup_recovery(Uid, DeviceId);
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
            lists:any(
                fun(D) ->
                    case maps:get(<<"device_id">>, D, <<>>) of
                        DeviceId ->
                            PublicKey = maps:get(<<"public_key">>, D, <<>>),
                            KeyId = maps:get(<<"key_id">>, D, <<>>),
                            PublicKey =/= <<>> andalso KeyId =/= <<>>;
                        _ ->
                            false
                    end
                end,
                Devices
            );
        _ ->
            false
    end.

%% @doc 检查服务端加密备份是否可用（只探测有无，不触密文内容）
-spec check_server_backup_available(integer()) ->
    {ok, boolean(), map()} | {error, term()}.
check_server_backup_available(Uid) ->
    case e2ee_backup_ds:latest(Uid) of
        {ok, Row} ->
            {ok, true, #{
                <<"backup_version">> => maps:get(<<"backup_version">>, Row),
                <<"created_at">> => maps:get(<<"created_at">>, Row, null)
            }};
        {error, not_found} ->
            {ok, false, #{}};
        {error, Reason} ->
            ok = ?ERROR_LOG([check_server_backup_available, Reason]),
            {ok, false, #{}}
    end.

%% @doc 启动服务端备份恢复：指引客户端拉密文包并本地口令解密（服务端零密码学）
-spec start_server_backup_recovery(integer(), binary()) ->
    {ok, map()} | {error, term()}.
start_server_backup_recovery(Uid, _DeviceId) ->
    case e2ee_backup_ds:latest(Uid) of
        {ok, Row} ->
            {ok, #{
                <<"action">> => <<"fetch_backup">>,
                <<"backup_version">> => maps:get(<<"backup_version">>, Row),
                <<"message">> => <<"请输入恢复口令以解密云端备份"/utf8>>
            }};
        {error, not_found} ->
            {error, {<<"无云端备份"/utf8>>, ?ERR_NOT_FOUND}};
        {error, Reason} ->
            _ = ?ERROR_LOG({start_server_backup_recovery_db_error, Uid, Reason}),
            {error, <<"internal_error">>}
    end.
