-module(logic_user).
%%%
% user 业务逻辑模块
%%%
-export([online/3]).
-export([offline/3]).
-export([idle_timeout/1]).

-include("common.hrl").


-spec online(any(), pid(), DID :: binary()) -> ok.
online(UID, Pid, DID) ->
    ?LOG(["user_logic/online/3", UID, Pid, DID]),
    case ds_user:is_offline(UID, DID) of
        {ToPid, _UID, DID} ->
            Msg = ds_message:s2c(786,
                                 "Already logged in on another device"),
            ?LOG([ToPid, DID]),
            erlang:start_timer(10, ToPid, jsone:encode(Msg));
        true ->
            ?LOG(["ds_user:is_offline/2", true, UID, DID]),
            ok
    end,
    % 把UID标记为online
    ds_user:online(UID, Pid, DID),
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
