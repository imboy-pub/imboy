-module(olm_handler).
%%%
%%% olm_handler — Olm (X3DH + Double Ratchet) 设备密钥 HTTP 适配层。
%%%
%%% 与 e2ee_handler 同模式：thin adapter，capability gate (e2ee_enabled) + action 分发。
%%% 所有端点复用 ensure_e2ee_enabled/1 守卫（Olm 是 E2EE 的子系统，e2ee 关闭时一并禁用）。
%%%

-behavior(cowboy_rest).

-export([init/2]).

-include("common.hrl").
-include("error_code.hrl").

%% one-time keys 单次上报上限（与 logic 层 ?MAX_OTK_PER_REPORT 对齐）
-define(MAX_OTK_PER_REPORT, 100).

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 =
        case Action of
            report_identity ->
                report_identity(Req0, State);
            report_prekeys ->
                report_prekeys(Req0, State);
            report_fallback ->
                report_fallback(Req0, State);
            get_identity ->
                get_identity(Req0, State);
            claim_key ->
                claim_key(Req0, State);
            _ ->
                elib_response:error(Req0, <<"not_found">>, 404)
        end,
    {ok, Req1, State}.

%% ===================================================================
%% Capability Gate（复用 e2ee_policy）
%% ===================================================================

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
%% POST /api/v1/e2ee/olm/identity — 上报设备 Olm 身份键
%% ===================================================================

-spec report_identity(cowboy_req:req(), map()) -> cowboy_req:req().
report_identity(Req0, State) ->
    case ensure_e2ee_enabled(Req0) of
        ok ->
            do_report_identity(Req0, State);
        {error, Req1} ->
            Req1
    end.

-spec do_report_identity(cowboy_req:req(), map()) -> cowboy_req:req().
do_report_identity(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    DeviceId = maps:get(<<"device_id">>, PostVals, <<>>),
    DeviceType = maps:get(<<"device_type">>, PostVals, <<>>),
    Ed25519Key = maps:get(<<"ed25519_key">>, PostVals, <<>>),
    Curve25519Key = maps:get(<<"curve25519_key">>, PostVals, <<>>),
    Signature = maps:get(<<"signature">>, PostVals, <<>>),
    case
        olm_identity_logic:report_identity(
            CurrentUid, DeviceId, Ed25519Key, Curve25519Key, Signature, DeviceType
        )
    of
        ok ->
            elib_response:success(Req0, #{<<"success">> => true});
        {error, Msg} ->
            elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST)
    end.

%% ===================================================================
%% POST /api/v1/e2ee/olm/prekeys — 批量上报 one-time keys
%% Body: {device_id, keys: [{key_id, key_base64}, ...]}
%% ===================================================================

-spec report_prekeys(cowboy_req:req(), map()) -> cowboy_req:req().
report_prekeys(Req0, State) ->
    case ensure_e2ee_enabled(Req0) of
        ok ->
            do_report_prekeys(Req0, State);
        {error, Req1} ->
            Req1
    end.

-spec do_report_prekeys(cowboy_req:req(), map()) -> cowboy_req:req().
do_report_prekeys(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    DeviceId = maps:get(<<"device_id">>, PostVals, <<>>),
    KeysRaw = maps:get(<<"keys">>, PostVals, []),
    Keys = normalize_key_pairs(KeysRaw),
    case
        olm_identity_logic:report_one_time_keys(
            CurrentUid, DeviceId, Keys, ?MAX_OTK_PER_REPORT
        )
    of
        {ok, Count} ->
            elib_response:success(Req0, #{<<"count">> => Count});
        {error, Msg} ->
            elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST)
    end.

%% ===================================================================
%% POST /api/v1/e2ee/olm/fallback_key — 上报 fallback key
%% Body: {device_id, key_id, key_base64}
%% ===================================================================

-spec report_fallback(cowboy_req:req(), map()) -> cowboy_req:req().
report_fallback(Req0, State) ->
    case ensure_e2ee_enabled(Req0) of
        ok ->
            do_report_fallback(Req0, State);
        {error, Req1} ->
            Req1
    end.

-spec do_report_fallback(cowboy_req:req(), map()) -> cowboy_req:req().
do_report_fallback(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    DeviceId = maps:get(<<"device_id">>, PostVals, <<>>),
    KeyId = maps:get(<<"key_id">>, PostVals, <<>>),
    KeyB64 = maps:get(<<"key_base64">>, PostVals, <<>>),
    case olm_identity_logic:report_fallback_key(CurrentUid, DeviceId, KeyId, KeyB64) of
        ok ->
            elib_response:success(Req0, #{<<"success">> => true});
        {error, Msg} ->
            elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST)
    end.

%% ===================================================================
%% GET /api/v1/e2ee/olm/identity?uid=&device_id= — 查询对端身份键
%% ===================================================================

-spec get_identity(cowboy_req:req(), map()) -> cowboy_req:req().
get_identity(Req0, State) ->
    case ensure_e2ee_enabled(Req0) of
        ok ->
            do_get_identity(Req0, State);
        {error, Req1} ->
            Req1
    end.

-spec do_get_identity(cowboy_req:req(), map()) -> cowboy_req:req().
do_get_identity(Req0, _State) ->
    TargetUidEnc = elib_param:get(<<"uid">>, Req0, <<"">>),
    DeviceId = elib_param:get(<<"device_id">>, Req0, <<>>),
    TargetUid = elib_cnv:safe_to_integer(TargetUidEnc),
    case is_integer(TargetUid) andalso TargetUid > 0 andalso byte_size(DeviceId) > 0 of
        false ->
            elib_response:error(Req0, <<"bad_request">>, 400);
        true ->
            case olm_identity_logic:get_identity(TargetUid, DeviceId) of
                {ok, Payload} ->
                    elib_response:success(Req0, Payload);
                {error, not_found} ->
                    elib_response:error(Req0, <<"not_found">>, ?ERR_NOT_FOUND);
                {error, Msg} ->
                    elib_response:error(Req0, Msg, ?ERR_INTERNAL_SERVER_ERROR)
            end
    end.

%% ===================================================================
%% POST /api/v1/e2ee/olm/claim — 领取对端一个 prekey（X3DH）
%% Body: {target_uid, device_id}
%% 返回: {type: one_time|fallback, key_id, key_base64, identity: {...}}
%% ===================================================================

-spec claim_key(cowboy_req:req(), map()) -> cowboy_req:req().
claim_key(Req0, State) ->
    case ensure_e2ee_enabled(Req0) of
        ok ->
            do_claim_key(Req0, State);
        {error, Req1} ->
            Req1
    end.

-spec do_claim_key(cowboy_req:req(), map()) -> cowboy_req:req().
do_claim_key(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    TargetUidEnc = maps:get(<<"target_uid">>, PostVals, <<"">>),
    TargetUid = elib_cnv:safe_to_integer(TargetUidEnc),
    DeviceId = maps:get(<<"device_id">>, PostVals, <<>>),
    case is_integer(TargetUid) andalso TargetUid > 0 andalso byte_size(DeviceId) > 0 of
        false ->
            elib_response:error(Req0, <<"bad_request">>, 400);
        true ->
            case olm_identity_logic:claim_keys(CurrentUid, TargetUid, DeviceId) of
                {ok, Payload} ->
                    elib_response:success(Req0, Payload);
                {error, Msg} ->
                    elib_response:error(Req0, Msg, ?ERR_NOT_FOUND)
            end
    end.

%% ===================================================================
%% Internal: 归一化 keys 列表（接受 [{key_id, key_base64}] 或 #{key_id => key_base64}）
%% ===================================================================

-spec normalize_key_pairs(term()) -> [{binary(), binary()}].
normalize_key_pairs(List) when is_list(List) ->
    [
        {to_bin(maps:get(<<"key_id">>, M, <<>>)), to_bin(maps:get(<<"key_base64">>, M, <<>>))}
     || #{<<"key_id">> := _, <<"key_base64">> := _} = M <- List,
        is_map(M)
    ];
normalize_key_pairs(Map) when is_map(Map) ->
    [{to_bin(K), to_bin(V)} || {K, V} <- maps:to_list(Map), is_binary(V) orelse is_list(V)];
normalize_key_pairs(_) ->
    [].

-spec to_bin(term()) -> binary().
to_bin(B) when is_binary(B) -> B;
to_bin(L) when is_list(L) -> list_to_binary(L);
to_bin(_) -> <<>>.
