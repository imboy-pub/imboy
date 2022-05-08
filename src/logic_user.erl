-module(logic_user).
%%%
% user 业务逻辑模块
%%%
-export([online/4]).
-export([offline/3]).
-export([idle_timeout/1]).

-include("common.hrl").


-spec online(UID::any(), Pid::pid(), DType::binary(), DID :: binary()) -> ok.
online(UID, Pid, DType, DID) ->
    ?LOG(["user_logic/online/4", UID, Pid, DType, DID]),
    % 在其他设备登录了
    Msg = ds_message:s2c(786, "Logged in on another device"),
    % 在“把UID标记为online”之前，给UID同类型设备发送下线通知(s2c 786 消息)
    ds_message:send(UID, DType, jsone:encode(Msg, [native_utf8]), 1),
    % 把UID标记为online
    ds_user:online(UID, Pid, DType, DID),
    % 检查消息 用异步队列实现
    server_user:cast_online(UID, Pid, DID),
    ok.


-spec offline(UID :: any(), Pid :: pid(), DID :: binary()) -> ok.
offline(UID, Pid, DID) ->
    ds_user:offline(Pid),
    % 检查离线消息 用异步队列实现
    server_user:cast_offline(UID, Pid, DID).


% 设置用户websocket超时时间，默认60秒
idle_timeout(_UId) ->
    60000.
