%%%-------------------------------------------------------------------
%%% @doc QR 码登录业务逻辑层
%%% 封装 qr_login_handler 的业务逻辑，避免 Handler 直接调用 DS 层。
%%% @end
%%%-------------------------------------------------------------------
-module(qr_login_logic).

-include("log.hrl").
-include("common.hrl").

-export([log_device_login/4]).

%% @doc 记录设备 QR 扫码登录
%% @param Uid    用户ID
%% @param DeviceId 设备ID
%% @param DeviceName 设备名称
%% @param Platform 平台
%% @return ok
-spec log_device_login(integer(), binary(), binary(), binary()) -> ok.
log_device_login(Uid, DeviceId, DeviceName, Platform) ->
    Now = elib_dt:now(),
    PostVals = #{
        <<"device_name">> => DeviceName,
        <<"platform">> => Platform,
        <<"login_type">> => <<"qr_scan">>
    },
    _ = user_device_ds:save(Now, Uid, DeviceId, PostVals),
    ?INFO_LOG([qr_login_device, Uid, DeviceId, DeviceName]),
    ok.
