%%%-------------------------------------------------------------------
%%% @doc QR 码登录业务逻辑层
%%% 封装 qr_login_handler 的业务逻辑，避免 Handler 直接调用 DS 层。
%%% @end
%%%-------------------------------------------------------------------
-module(qr_login_logic).

-moduledoc "QR 码登录业务逻辑层 / QR login logic。".
-include("log.hrl").
-include("common.hrl").

-export([log_device_login/4, web_session/1]).

%% @doc 记录设备 QR 扫码登录
%% @param Uid    用户ID
%% @param DeviceId 设备ID
%% @param DeviceName 设备名称
%% @param Platform 平台
%% @return ok
-spec log_device_login(integer(), binary(), binary(), binary()) ->
    ok | {ok, pos_integer()} | {error, term()}.
log_device_login(Uid, DeviceId, DeviceName, Platform) ->
    Now = elib_dt:now(),
    PostVals = #{
        <<"dname">> => DeviceName,
        <<"cos">> => Platform,
        <<"login_type">> => <<"qr_scan">>
    },
    try
        case Platform of
            <<"web">> ->
                case user_ds:find_by_id(Uid, <<"status,account_type">>) of
                    #{<<"status">> := Status, <<"account_type">> := 0} when
                        Status =:= 1; Status =:= 2
                    ->
                        user_device_ds:register_web(Now, Uid, DeviceId, PostVals);
                    _ ->
                        {error, invalid_account}
                end;
            _ ->
                case user_device_ds:save(Now, Uid, DeviceId, PostVals) of
                    {ok, _} -> ok;
                    {error, _} = Error -> Error
                end
        end
    catch
        _:_ -> {error, unavailable}
    end.

%% Web aliases may consume only the matching server-owned Human Web session.
-spec web_session(binary()) -> {ok, map()} | {error, term()}.
web_session(SessionToken) ->
    try
        Session = qr_login_session_ds:find_by_session_token(SessionToken),
        case Session of
            #{<<"purpose">> := <<"human">>, <<"platform">> := <<"web">>} -> {ok, Session};
            {error, _} -> {error, unavailable};
            undefined -> {error, not_found};
            _ -> {error, forbidden}
        end
    catch
        _:_ -> {error, unavailable}
    end.
