-module(e2ee_handler).
-dialyzer({nowarn_function, [do_key_status/2]}).

%% Thin HTTP adapter for the security_privacy e2ee boundary.
-dialyzer(
    {nowarn_function, [key_status/2, pull_notifications/2, start_recovery/2, compliance_key/2]}
).

-behavior(cowboy_rest).

-export([init/2]).

-include("common.hrl").
-include("error_code.hrl").

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
            key_status ->
                key_status(Req0, State);
            pull_notifications ->
                pull_notifications(Req0, State);
            start_recovery ->
                start_recovery(Req0, State);
            compliance_key ->
                compliance_key(Req0, State);
            _ ->
                elib_response:error(Req0, <<"not_found">>, 404)
        end,
    {ok, Req1, State}.

%% ===================================================================
%% Capability Gate
%% ===================================================================

%% @doc 检查 E2EE 功能是否启用
%% 根据 e2ee_mode capability 决定是否允许访问 E2EE 相关接口
-spec ensure_e2ee_enabled(cowboy_req:req()) -> ok | {error, cowboy_req:req()}.
ensure_e2ee_enabled(Req0) ->
    case imboy_policy:e2ee_enabled() of
        true ->
            ok;
        false ->
            {error,
                elib_response:error(
                    Req0,
                    imboy_error:error_msg(?ERR_FEATURE_DISABLED),
                    ?ERR_FEATURE_DISABLED
                )}
    end.

%% ===================================================================
%% API Handlers
%% ===================================================================

-spec user_keys(cowboy_req:req(), map()) -> cowboy_req:req().
user_keys(Req0, State) ->
    case ensure_e2ee_enabled(Req0) of
        ok ->
            do_user_keys(Req0, State);
        {error, Req1} ->
            Req1
    end.

-spec do_user_keys(cowboy_req:req(), map()) -> cowboy_req:req().
do_user_keys(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    TargetUidEnc = elib_param:get(<<"uid">>, Req0, <<"">>),
    TargetUid = elib_cnv:safe_to_integer(TargetUidEnc),
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
    case ensure_e2ee_enabled(Req0) of
        ok ->
            do_group_member_keys(Req0, State);
        {error, Req1} ->
            Req1
    end.

-spec do_group_member_keys(cowboy_req:req(), map()) -> cowboy_req:req().
do_group_member_keys(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    GidEnc = elib_param:get(<<"gid">>, Req0, <<"">>),
    Gid = elib_cnv:safe_to_integer(GidEnc),
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
    case ensure_e2ee_enabled(Req0) of
        ok ->
            do_report_device_key(Req0, State);
        {error, Req1} ->
            Req1
    end.

-spec do_report_device_key(cowboy_req:req(), map()) -> cowboy_req:req().
do_report_device_key(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    case throttle:check(e2ee_report_key, CurrentUid) of
        {limit_exceeded, _, _} ->
            elib_response:error(Req0, <<"操作过于频繁，请稍后再试"/utf8>>, 429);
        _ ->
            case read_report_params(Req0) of
                {error, Reason} ->
                    elib_response:error(Req0, Reason, 400);
                {ok, DeviceId, DeviceType, DeviceName, PublicKey, KeyId} ->
                    case validate_params(DeviceId, DeviceType, PublicKey, KeyId) of
                        {error, Reason} ->
                            elib_response:error(Req0, Reason, 400);
                        ok ->
                            %% S1.3 / DT-03：body device_id 必须等于 token 绑定 DID，
                            %% 防止同账号设备 A 的 token 为设备 B 上报公钥。
                            CurrentDid = auth_ds:current_did(State),
                            case olm_handler:device_write_decision(CurrentDid, DeviceId) of
                                ok ->
                                    do_report_device_key1(
                                        Req0,
                                        CurrentUid,
                                        DeviceId,
                                        DeviceType,
                                        DeviceName,
                                        PublicKey,
                                        KeyId
                                    );
                                device_binding_required ->
                                    elib_response:error(Req0, <<"device_binding_required">>, 403);
                                device_mismatch ->
                                    elib_response:error(Req0, <<"device_mismatch">>, 403)
                            end
                    end
            end
    end.

%% @doc 设备所有权校验通过后，执行实际上报逻辑。
-spec do_report_device_key1(
    cowboy_req:req(), integer(), binary(), binary(), binary() | undefined, binary(), binary()
) ->
    cowboy_req:req().
do_report_device_key1(Req0, CurrentUid, DeviceId, DeviceType, DeviceName, PublicKey, KeyId) ->
    case
        e2ee_logic:report_device_key(
            CurrentUid, DeviceId, DeviceType, DeviceName, PublicKey, KeyId
        )
    of
        {ok, OtherDeviceCount} ->
            elib_response:success(Req0, #{
                <<"success">> => true,
                <<"other_device_count">> => OtherDeviceCount,
                <<"has_other_device">> => OtherDeviceCount > 0
            });
        {error, Reason} ->
            elib_response:error(Req0, Reason, 500)
    end.

%% @doc 读取上报参数
-spec read_report_params(cowboy_req:req()) ->
    {error, binary()} | {ok, binary(), binary(), binary() | undefined, binary(), binary()}.
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
    ValidDeviceTypes = [
        <<"ios">>, <<"android">>, <<"macos">>, <<"windows">>, <<"linux">>, <<"web">>
    ],
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

%% ===================================================================
%% 自动恢复相关 API
%% ===================================================================

%% @doc 检查密钥状态
%% GET /v1/e2ee/key/status?device_id=xxx
%% 返回当前设备的密钥状态和可用的恢复方式
-spec key_status(cowboy_req:req(), map()) -> cowboy_req:req().
key_status(Req0, State) ->
    case ensure_e2ee_enabled(Req0) of
        ok ->
            do_key_status(Req0, State);
        {error, Req1} ->
            Req1
    end.

-spec do_key_status(cowboy_req:req(), map()) -> cowboy_req:req().
do_key_status(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    DeviceId = elib_param:get(<<"device_id">>, Req0, <<>>),

    case DeviceId of
        <<>> ->
            elib_response:error(Req0, <<"缺少 device_id 参数"/utf8>>, ?ERR_BAD_REQUEST);
        _ ->
            case e2ee_recovery_logic:check_key_status(CurrentUid, DeviceId) of
                {ok, Status} ->
                    elib_response:success(Req0, Status);
                {error, Reason} ->
                    elib_response:error(Req0, Reason, ?ERR_INTERNAL_SERVER_ERROR)
            end
    end.

%% @doc 拉取密钥变更通知
%% GET /v1/e2ee/notifications/pull?since=timestamp&limit=50
%% 支持增量拉取，返回好友的密钥变更记录
-spec pull_notifications(cowboy_req:req(), map()) -> cowboy_req:req().
pull_notifications(Req0, State) ->
    case ensure_e2ee_enabled(Req0) of
        ok ->
            do_pull_notifications(Req0, State);
        {error, Req1} ->
            Req1
    end.

-spec do_pull_notifications(cowboy_req:req(), map()) -> cowboy_req:req().
do_pull_notifications(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    Since = elib_param:get(<<"since">>, Req0, <<"0">>),
    {ok, Limit} = elib_param:int(<<"limit">>, Req0, 50),

    case e2ee_logic:pull_key_notifications(CurrentUid, Since, Limit) of
        {ok, Notifications} ->
            elib_response:success(Req0, #{
                <<"notifications">> => Notifications,
                <<"count">> => length(Notifications)
            });
        {error, Reason} ->
            elib_response:error(Req0, Reason, ?ERR_INTERNAL_SERVER_ERROR)
    end.

%% @doc 启动自动恢复
%% POST /v1/e2ee/recovery/start
%% Body: {"device_id": "xxx", "method": "device_transfer"}
%% 根据推荐方式自动启动恢复流程
-spec start_recovery(cowboy_req:req(), map()) -> cowboy_req:req().
start_recovery(Req0, State) ->
    case ensure_e2ee_enabled(Req0) of
        ok ->
            do_start_recovery_entry(Req0, State);
        {error, Req1} ->
            Req1
    end.

-spec do_start_recovery_entry(cowboy_req:req(), map()) -> cowboy_req:req().
do_start_recovery_entry(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    {ok, Body, _} = cowboy_req:read_body(Req0),
    Data =
        try jsx:decode(Body, [return_maps]) of
            D when is_map(D) -> D
        catch
            _:_ -> #{}
        end,

    DeviceId = maps:get(<<"device_id">>, Data, <<>>),
    Method = maps:get(<<"method">>, Data, <<>>),

    case {DeviceId, Method} of
        {<<>>, _} ->
            elib_response:error(Req0, <<"缺少 device_id 参数"/utf8>>, ?ERR_BAD_REQUEST);
        {_, <<>>} ->
            % 如果没有指定方法，自动选择最优方式
            case e2ee_recovery_logic:get_recovery_options(CurrentUid) of
                [] ->
                    elib_response:error(Req0, <<"无可用恢复方式"/utf8>>, ?ERR_E2EE_RECOVERY_NO_OPTIONS);
                Options ->
                    BestMethod = e2ee_recovery_logic:recommend_method(Options),
                    do_start_recovery(Req0, CurrentUid, DeviceId, BestMethod)
            end;
        {_, _} ->
            do_start_recovery(Req0, CurrentUid, DeviceId, Method)
    end.

%% @doc 执行恢复
-spec do_start_recovery(cowboy_req:req(), integer(), binary(), binary()) -> cowboy_req:req().
do_start_recovery(Req0, Uid, DeviceId, Method) ->
    case e2ee_recovery_logic:start_auto_recovery(Uid, DeviceId, Method) of
        {ok, Result} ->
            elib_response:success(Req0, Result);
        {error, {Msg, Code}} ->
            elib_response:error(Req0, Msg, Code);
        {error, Reason} ->
            elib_response:error(Req0, Reason, ?ERR_E2EE_RECOVERY_FAILED)
    end.

%% @doc 获取当前活跃的合规公钥
%% GET /v1/e2ee/compliance_key
%% 返回合规密钥的 key_id 和 public_key，客户端用于 compliance_e2ee 模式的双密钥加密
-spec compliance_key(cowboy_req:req(), map()) -> cowboy_req:req().
compliance_key(Req0, _State) ->
    %% 与其余 E2EE 端点一致，先过能力闸门
    case ensure_e2ee_enabled(Req0) of
        ok ->
            do_compliance_key(Req0);
        {error, Req1} ->
            Req1
    end.

-spec do_compliance_key(cowboy_req:req()) -> cowboy_req:req().
do_compliance_key(Req0) ->
    %% get_active_compliance_key/0 返回二元组 {ok, Map}（compliance_key_repo:find_active），
    %% 此前用三元组 {ok, KeyId, PublicKey} 匹配会触发 case_clause 崩溃（500）。
    case e2ee_logic:get_active_compliance_key() of
        {ok, #{<<"key_id">> := KeyId, <<"public_key">> := PublicKey}} ->
            elib_response:success(Req0, #{
                <<"key_id">> => KeyId,
                <<"public_key">> => PublicKey
            });
        {error, not_found} ->
            elib_response:error(Req0, <<"无活跃合规密钥"/utf8>>, ?ERR_NOT_FOUND);
        {error, _Reason} ->
            elib_response:error(Req0, <<"合规密钥查询失败"/utf8>>, ?ERR_INTERNAL_SERVER_ERROR)
    end.
