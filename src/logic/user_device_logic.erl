-module(user_device_logic).
%%%
% user_device 业务逻辑模块
% user_device business logic module
%
% 职责：
% - 设备档案管理（数据库）
% - 设备会话管理（imboy_syn 内存）
% - 登录冲突检测
%%%

-export([device_name/2,
         change_name/3,
         delete/2]).
-export([page/3]).

% 设备类型验证
-export([validate_device_type/1]).
-export([device_type_enum/0]).

% 登录前检查
-export([check_login_conflict/2]).
-export([check_device_can_login/2]).

% 设备在线状态查询
-export([is_online/2]).
-export([online_dids/1]).

% 会话管理
-export([register_device_session/4]).
-export([kick_device/3]).
-export([kick_device_by_type/2]).
-export([kick_all_other_devices/2]).
-export([get_active_devices/1]).

% 辅助函数
-export([format_device_info/1]).

-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").
-include("chat.hrl").
-include("error_code.hrl").

%% ===================================================================
%% API
%% ===================================================================


%% @doc 获取设备名称
%% 获取设备的显示名称，支持缓存
%% @param Uid 用户ID
%% @param DID 设备ID
%% @return binary() 设备名称
-spec device_name(integer(), binary()) -> binary().
% DID = <<"3f039a2b4724a5b7">>.
% Key = {user_device_name, Uid, DID}.
% user_device_logic:device_name(1, <<"HUAWEIMRD-AL00">>).
% user_device_ds:device_name(1, <<"HUAWEIMRD-AL00">>).
%  imboy_cache:get(Key).
device_name(Uid, DID) ->
    Key = {user_device_name, 2, Uid, DID},
    Fun = fun() -> user_device_ds:device_name(Uid, DID) end,
    % 缓存10天
    imboy_cache:memo(Fun, Key, 864000).


%% @doc 修改设备名称
%% 修改设备的显示名称
%% @param Uid 用户ID
%% @param DID 设备ID
%% @param Name 新的设备名称
%% @return ok
-spec change_name(integer(), binary(), binary()) -> ok.
change_name(Uid, DID, Name) ->
    Set = <<"device_name = $1">>,
    SetArgs = [Name],
    _ = user_device_ds:update_by_did(Uid, DID, Set, SetArgs),

    Key = {user_device_name, 2, Uid, DID},
    imboy_cache:flush(Key),
    ok.


%% @doc 删除设备
%% 删除指定设备记录
%% @param Uid 用户ID
%% @param DID 设备ID
%% @return ok
-spec delete(integer(), binary()) -> ok.
delete(Uid, DID) ->
    user_device_ds:delete(Uid, DID),
    Key = {user_device_name, 2, Uid, DID},
    imboy_cache:flush(Key),
    ok.


%% @doc 获取用户设备列表（分页）
%% 获取用户的所有设备，包含在线状态
%% @param Uid 用户ID
%% @param Page 页码
%% @param Size 每页大小
%% @return map() 包含 total、page、size、list 的分页结果
-spec page(integer(), integer(), integer()) -> map().
page(Uid, Page, Size) when Page > 0 ->
    Offset = (Page - 1) * Size,
    Total = user_device_ds:count_by_uid(Uid),
    case user_device_ds:page(Uid, Size, Offset) of
        {ok, []} ->
            #{total => Total, page => Page, size => Size, list => []};
        {ok, Items0} ->
            OnlineDids = online_dids(Uid),
            Items2 = [elib_response:json_decode_field(
                         #{<<"online">> => lists:member(maps:get(<<"device_id">>, Row), OnlineDids),
                           <<"device_id">> => maps:get(<<"device_id">>, Row),
                           <<"device_name">> => maps:get(<<"device_name">>, Row, <<>>),
                           <<"device_type">> => maps:get(<<"device_type">>, Row, <<>>),
                           <<"last_active_at">> => maps:get(<<"last_active_at">>, Row, <<>>),
                           <<"device_vsn">> => maps:get(<<"device_vsn">>, Row, <<>>)},
                         <<"device_vsn">>)
                      || Row <- Items0],
            #{total => Total, page => Page, size => Size, list => Items2};
        _ ->
            #{total => Total, page => Page, size => Size, list => []}
    end.


%% ===================================================================
%% Device Session Management (基于 imboy_syn)
%% ===================================================================

%% @doc 获取支持的设备类型枚举
-spec device_type_enum() -> [binary()].
device_type_enum() ->
    [<<"ios">>, <<"android">>, <<"macos">>, <<"windows">>, <<"linux">>, <<"web">>].

%% @doc 检查设备类型是否有效
-spec validate_device_type(binary()) -> boolean().
validate_device_type(DType) when is_binary(DType) ->
    lists:member(DType, device_type_enum());
validate_device_type(_) ->
    false.

%% @doc 检查登录冲突（返回冲突设备信息）
-spec check_login_conflict(integer(), binary()) ->
    {ok, no_conflict} | {ok, conflict, map()} | {error, binary()}.
check_login_conflict(Uid, DType) when is_integer(Uid), is_binary(DType) ->
    case validate_device_type(DType) of
        false ->
            {error, <<"无效的设备类型"/utf8>>};
        true ->
            case imboy_syn:list_by_uid(Uid) of
                [] ->
                    {ok, no_conflict};
                Devices ->
                    case find_device_by_type(Devices, DType) of
                        {ok, ConflictPid, ConflictDID} ->
                            ConflictInfo = #{
                                <<"device_type">> => DType,
                                <<"device_id">> => ConflictDID,
                                <<"pid">> => pid_to_list(ConflictPid)
                            },
                            {ok, conflict, ConflictInfo};
                        not_found ->
                            {ok, no_conflict}
                    end
            end
    end;
check_login_conflict(_Uid, _DType) ->
    {error, <<"参数类型错误"/utf8>>}.

%% @doc 检查设备是否可以登录（返回所有活跃设备）
-spec check_device_can_login(integer(), binary()) -> {ok, [map()]} | {error, binary()}.
check_device_can_login(Uid, _DType) when is_integer(Uid) ->
    Devices = imboy_syn:list_by_uid(Uid),
    ActiveDevices = [format_device_info({Pid, Meta}) || {Pid, Meta} <- Devices],
    {ok, ActiveDevices};
check_device_can_login(_Uid, _DType) ->
    {error, <<"用户ID必须是整数"/utf8>>}.

%% @doc 注册设备会话
-spec register_device_session(integer(), binary(), pid(), binary()) -> ok | {error, binary()}.
register_device_session(Uid, DType, Pid, DID) when is_integer(Uid), is_binary(DType), is_pid(Pid), is_binary(DID) ->
    case validate_device_type(DType) of
        false ->
            {error, <<"无效的设备类型"/utf8>>};
        true ->
            case imboy_syn:join(Uid, DType, Pid, DID) of
                ok ->
                    ok;
                {error, Reason} ->
                    ?ERROR_LOG([device_session_register_failed, Uid, DType, DID, Reason]),
                    {error, Reason}
            end
    end;
register_device_session(_Uid, _DType, _Pid, _DID) ->
    {error, <<"参数类型错误"/utf8>>}.

%% @doc 踢出指定设备
-spec kick_device(integer(), binary(), binary()) -> ok | {error, binary()}.
kick_device(Uid, DType, DID) when is_integer(Uid), is_binary(DType), is_binary(DID) ->
    Devices = imboy_syn:list_by_uid(Uid),
    case find_device_by_did(Devices, DID) of
        {ok, Pid, _DType} ->
            send_kick_message(Pid, #{<<"reason">> => <<"在其他设备登录"/utf8>>}),
            imboy_syn:leave(Uid, Pid),
            ok;
        not_found ->
            {error, <<"设备不存在"/utf8>>}
    end;
kick_device(_Uid, _DType, _DID) ->
    {error, <<"参数类型错误"/utf8>>}.

%% @doc 踢出指定类型的所有设备
-spec kick_device_by_type(integer(), binary()) -> ok | {error, binary()}.
kick_device_by_type(Uid, DType) when is_integer(Uid), is_binary(DType) ->
    Devices = imboy_syn:list_by_uid(Uid),
    MatchedDevices = [{Pid, DID} || {Pid, {DT, DID}} <- Devices, DT =:= DType],
    lists:foreach(fun({Pid, _DID}) ->
        send_kick_message(Pid, #{<<"reason">> => <<"同类型设备在其他地方登录"/utf8>>}),
        imboy_syn:leave(Uid, Pid)
    end, MatchedDevices),
    ok;
kick_device_by_type(_Uid, _DType) ->
    {error, <<"参数类型错误"/utf8>>}.

%% @doc 踢出所有其他设备（保留当前设备）
-spec kick_all_other_devices(integer(), {binary(), binary()}) -> ok | {error, binary()}.
kick_all_other_devices(Uid, {CurrentDType, CurrentDID}) when is_integer(Uid) ->
    Devices = imboy_syn:list_by_uid(Uid),
    lists:foreach(fun({Pid, {DType, DID}}) ->
        case {DType, DID} of
            {CurrentDType, CurrentDID} ->
                ok;
            _ ->
                send_kick_message(Pid, #{<<"reason">> => <<"账号在其他设备登录"/utf8>>}),
                imboy_syn:leave(Uid, Pid)
        end
    end, Devices),
    ok;
kick_all_other_devices(_Uid, _CurrentDevice) ->
    {error, <<"参数类型错误"/utf8>>}.

%% @doc 获取活跃设备列表
-spec get_active_devices(integer()) -> {ok, [map()]} | {error, binary()}.
get_active_devices(Uid) when is_integer(Uid) ->
    Devices = imboy_syn:list_by_uid(Uid),
    DeviceInfos = [format_device_info({Pid, Meta}) || {Pid, Meta} <- Devices],
    {ok, DeviceInfos};
get_active_devices(_Uid) ->
    {error, <<"用户ID必须是整数"/utf8>>}.

%% @doc 检查指定设备类型或设备ID是否在线
-spec is_online(integer(), {dtype, binary()} | {did, binary()}) -> boolean().
is_online(Uid, {dtype, DType}) when is_integer(Uid), is_binary(DType) ->
    case validate_device_type(DType) of
        false ->
            false;
        true ->
            Devices = imboy_syn:list_by_uid(Uid),
            lists:any(fun({_P, {DT, _DID}}) -> DT =:= DType end, Devices)
    end;
is_online(Uid, {did, DID}) when is_integer(Uid), is_binary(DID) ->
    Devices = imboy_syn:list_by_uid(Uid),
    lists:any(fun({_P, {_DType, D}}) -> D =:= DID end, Devices);
is_online(_Uid, _Query) ->
    false.

%% @doc 获取用户在线设备ID列表
-spec online_dids(integer()) -> [binary()].
online_dids(Uid) when is_integer(Uid) ->
    Devices = imboy_syn:list_by_uid(Uid),
    [DID || {_P, {_DType, DID}} <- Devices];
online_dids(_Uid) ->
    [].

%% @doc 格式化设备信息
-spec format_device_info({pid(), {binary(), binary()}}) -> map().
format_device_info({Pid, {DType, DID}}) when is_pid(Pid), is_binary(DType), is_binary(DID) ->
    #{
        <<"device_type">> => DType,
        <<"device_id">> => DID,
        <<"pid">> => pid_to_list(Pid)
    }.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 在设备列表中查找指定类型的设备
-spec find_device_by_type([{pid(), {binary(), binary()}}], binary()) ->
    {ok, pid(), binary()} | not_found.
find_device_by_type([], _DType) ->
    not_found;
find_device_by_type([{Pid, {DType, DID}} | _Rest], DType) ->
    {ok, Pid, DID};
find_device_by_type([{_Pid, {_OtherDType, _DID}} | Rest], DType) ->
    find_device_by_type(Rest, DType).

%% @doc 在设备列表中查找指定设备ID的设备
-spec find_device_by_did([{pid(), {binary(), binary()}}], binary()) ->
    {ok, pid(), binary()} | not_found.
find_device_by_did([], _DID) ->
    not_found;
find_device_by_did([{Pid, {DType, DID}} | _Rest], DID) ->
    {ok, Pid, DType};
find_device_by_did([{_Pid, {_DType, _OtherDID}} | Rest], DID) ->
    find_device_by_did(Rest, DID).

%% @doc 发送踢出消息到设备进程
-spec send_kick_message(pid(), map()) -> ok.
send_kick_message(Pid, ReasonMap) when is_pid(Pid), is_map(ReasonMap) ->
    Message = {kick_device, ReasonMap},
    try
        Pid ! Message,
        ok
    catch
        _:_ ->
            ok
    end.

%

%% ===================================================================
%% EUnit tests.
%% ===================================================================

