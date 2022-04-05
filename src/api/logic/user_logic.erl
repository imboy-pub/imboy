-module (user_logic).
%%%
% user 业务逻辑模块
%%%
-export ([online/3]).
-export ([offline/2]).
-export ([idle_timeout/1]).

-include("common.hrl").

-spec online(any(), pid(), any()) -> ok.
online(Uid, Pid, DeviceType) ->
    ?LOG(["user_logic/online/3", Uid, Pid, DeviceType]),
    case user_ds:is_offline(Uid, DeviceType) of
        {ToPid, _Uid, DeviceType} ->
            Msg = message_ds:s2c(786, "Already logged in on another device"),
            ?LOG([ToPid, DeviceType]),
            erlang:start_timer(10, ToPid, jsone:encode(Msg));
        true ->
            ?LOG(["user_ds:is_offline/2", true, Uid, DeviceType]),
            ok
    end,
    % 把Uid标记为online
    user_ds:online(Uid, Pid, DeviceType),
    % 检查消息 用异步队列实现
    gen_server:cast(offline_server, {online, Uid, Pid}),
    ok.

-spec offline(any(), pid()) -> ok.
offline(Uid, Pid) ->
    user_ds:offline(Pid),
    % 检查离线消息 用异步队列实现
    gen_server:cast(offline_server, {offline, Uid, Pid}).

% 设置用户websocket超时时间，默认60秒
idle_timeout(_UId) ->
    60000.
