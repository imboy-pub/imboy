-module(e2ee_handler).

-behavior(cowboy_rest).

-export([init/2]).

-include("common.hrl").

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 =
        case Action of
            user_keys ->
                user_keys(Req0, State);
            group_member_keys ->
                group_member_keys(Req0, State);
            report_device_key ->
                report_device_key(Req0, State);
            _ ->
                elib_response:error(Req0, <<"not_found">>, 404)
        end,
    {ok, Req1, State}.

-spec user_keys(cowboy_req:req(), map()) -> cowboy_req:req().
user_keys(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    TargetUidEnc = elib_param:get(<<"uid">>, Req0, <<"">>),
    TargetUid = elib_hashids:decode(TargetUidEnc),
    case is_integer(TargetUid) andalso TargetUid > 0 of
        false ->
            elib_response:error(Req0, <<"bad_request">>, 400);
        true ->
            case e2ee_logic:user_keys(CurrentUid, TargetUid) of
                {ok, Payload} ->
                    elib_response:success(Req0, Payload);
                {error, Msg, Code} ->
                    elib_response:error(Req0, Msg, Code)
            end
    end.

-spec group_member_keys(cowboy_req:req(), map()) -> cowboy_req:req().
group_member_keys(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    GidEnc = elib_param:get(<<"gid">>, Req0, <<"">>),
    Gid = elib_hashids:decode(GidEnc),
    case is_integer(Gid) andalso Gid > 0 of
        false ->
            elib_response:error(Req0, <<"bad_request">>, 400);
        true ->
            case e2ee_logic:group_member_keys(CurrentUid, Gid) of
                {ok, Payload} ->
                    elib_response:success(Req0, Payload);
                {error, Msg, Code} ->
                    elib_response:error(Req0, Msg, Code)
            end
    end.

%% @doc 上报设备的 E2EE 公钥
%% 当用户的设备密钥发生变化时（如首次安装、重新安装），客户端调用此接口
%% 服务端更新密钥后，会通知该用户的所有好友清除其公钥缓存
-spec report_device_key(cowboy_req:req(), map()) -> cowboy_req:req().
report_device_key(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    case read_report_params(Req0) of
        {error, Reason} ->
            elib_response:error(Req0, Reason, 400);
        {ok, DeviceId, DeviceType, DeviceName, PublicKey, KeyId} ->
            case validate_params(DeviceId, DeviceType, PublicKey, KeyId) of
                {error, Reason} ->
                    elib_response:error(Req0, Reason, 400);
                ok ->
                    case e2ee_logic:report_device_key(CurrentUid, DeviceId, DeviceType, DeviceName, PublicKey, KeyId) of
                        ok ->
                            elib_response:success(Req0, #{<<"success">> => true});
                        {error, Reason} ->
                            elib_response:error(Req0, Reason, 500)
                    end
            end
    end.

%% @doc 读取上报参数
-spec read_report_params(cowboy_req:req()) -> {error, binary()} | {ok, binary(), binary(), binary() | undefined, binary(), binary()}.
read_report_params(Req) ->
    PostVals = elib_param:post(Req),
    DeviceId = maps:get(<<"device_id">>, PostVals, <<>>),
    DeviceType = maps:get(<<"device_type">>, PostVals, <<>>),
    DeviceName = maps:get(<<"device_name">>, PostVals, <<>>),
    PublicKey = maps:get(<<"public_key">>, PostVals, <<>>),
    KeyId = maps:get(<<"key_id">>, PostVals, <<>>),
    case DeviceId of
        <<>> -> {error, <<"device_id_required">>};
        _ -> {ok, DeviceId, DeviceType, DeviceName, PublicKey, KeyId}
    end.

%% @doc 验证参数
-spec validate_params(binary(), binary(), binary(), binary()) -> ok | {error, binary()}.
validate_params(DeviceId, DeviceType, PublicKey, KeyId) ->
    ValidDeviceTypes = [<<"ios">>, <<"android">>, <<"macos">>, <<"windows">>, <<"linux">>, <<"web">>],
    Condition1 = byte_size(DeviceId) > 0,
    Condition2 = lists:member(DeviceType, ValidDeviceTypes),
    Condition3 = byte_size(PublicKey) > 0,
    Condition4 = byte_size(KeyId) > 0,
    case {Condition1, Condition2, Condition3, Condition4} of
        {false, _, _, _} -> {error, <<"device_id_required">>};
        {_, false, _, _} -> {error, <<"invalid_device_type">>};
        {_, _, false, _} -> {error, <<"public_key_required">>};
        {_, _, _, false} -> {error, <<"key_id_required">>};
        {true, true, true, true} -> ok
    end.
