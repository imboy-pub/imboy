-module(auth_logic).
%% Stable identity auth domain boundary.
%% API adapters should call this module instead of reaching auth data internals directly.

%%%
% auth 业务逻辑模块
% auth business logic module
%%%

-export([logout/2]).

-include_lib("eunit/include/eunit.hrl").

-include("log.hrl").

-include_lib("kernel/include/logger.hrl").

-include("common.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 兼容旧测试入口：按设备登出
-spec logout(integer(), binary()) -> {ok, binary()}.
logout(Uid, DID) when is_integer(Uid), is_binary(DID) ->
    Devices = imboy_syn:list_by_uid(Uid),
    lists:foreach(
        fun
            ({Pid, {_DType, DeviceId}}) when DeviceId =:= DID ->
                imboy_syn:leave(Uid, Pid);
            (_) ->
                ok
        end,
        Devices
    ),
    ok = user_device_ds:delete(Uid, DID),
    {ok, <<"success">>}.

%% ===================================================================
%% EUnit tests.
%% ===================================================================
