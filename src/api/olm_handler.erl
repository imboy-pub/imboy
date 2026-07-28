-module(olm_handler).
%%%
%%% olm_handler — Olm (X3DH + Double Ratchet) 设备密钥 HTTP 适配层。
%%%
%%% 与 e2ee_handler 同模式：thin adapter，capability gate (e2ee_enabled) + action 分发。
%%% 所有端点复用 ensure_e2ee_enabled/1 守卫（Olm 是 E2EE 的子系统，e2ee 关闭时一并禁用）。
%%%

-behavior(cowboy_rest).

-export([init/2]).
%% 导出纯谓词供 EUnit 直测设备所有权判定（DT-01/02）。
-export([device_write_decision/2]).

-include("common.hrl").
-include("log.hrl").
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
            list_devices ->
                list_devices(Req0, State);
            claim_key ->
                claim_key(Req0, State);
            batch_claim ->
                batch_claim(Req0, State);
            prekey_count ->
                prekey_count(Req0, State);
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
    case ensure_device_owner(Req0, State, DeviceId) of
        {error, Req1} ->
            Req1;
        ok ->
            do_report_identity1(Req0, CurrentUid, DeviceId, PostVals)
    end.

-spec do_report_identity1(cowboy_req:req(), integer(), binary(), map()) ->
    cowboy_req:req().
do_report_identity1(Req0, CurrentUid, DeviceId, PostVals) ->
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
    case ensure_device_owner(Req0, State, DeviceId) of
        {error, Req1} ->
            Req1;
        ok ->
            do_report_prekeys1(Req0, CurrentUid, DeviceId, PostVals)
    end.

-spec do_report_prekeys1(cowboy_req:req(), integer(), binary(), map()) ->
    cowboy_req:req().
do_report_prekeys1(Req0, CurrentUid, DeviceId, PostVals) ->
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
    case ensure_device_owner(Req0, State, DeviceId) of
        {error, Req1} ->
            Req1;
        ok ->
            do_report_fallback1(Req0, CurrentUid, DeviceId, PostVals)
    end.

-spec do_report_fallback1(cowboy_req:req(), integer(), binary(), map()) ->
    cowboy_req:req().
do_report_fallback1(Req0, CurrentUid, DeviceId, PostVals) ->
    KeyId = maps:get(<<"key_id">>, PostVals, <<>>),
    KeyB64 = maps:get(<<"key_base64">>, PostVals, <<>>),
    %% E2EE-062：可选签名（由设备已注册的 ed25519 身份键签发）。
    %% 统一走 /5：签名为空时 /5 计一次 olm_fallback_unsigned_total 再委托 /4，
    %% 计数因此**恰好每个未签名请求一次**，且验签通过的上传不会被误计
    %% （/5 成功后才内部复用 /4）。已 grep 核实全仓无任何测试按 arity mock
    %% report_fallback_key，故此处统一调 /5 不会造成静默穿透。
    Signature = to_bin(maps:get(<<"signature">>, PostVals, <<>>)),
    case olm_identity_logic:report_fallback_key(CurrentUid, DeviceId, KeyId, KeyB64, Signature) of
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
%% GET /api/v1/e2ee/devices?uid= — 列出对端全部活跃 olm 设备（ADR 03 §8.1）
%% ===================================================================

-spec list_devices(cowboy_req:req(), map()) -> cowboy_req:req().
list_devices(Req0, State) ->
    case ensure_e2ee_enabled(Req0) of
        ok ->
            do_list_devices(Req0, State);
        {error, Req1} ->
            Req1
    end.

-spec do_list_devices(cowboy_req:req(), map()) -> cowboy_req:req().
do_list_devices(Req0, _State) ->
    TargetUidEnc = elib_param:get(<<"uid">>, Req0, <<"">>),
    TargetUid = elib_cnv:safe_to_integer(TargetUidEnc),
    case is_integer(TargetUid) andalso TargetUid > 0 of
        false ->
            elib_response:error(Req0, <<"bad_request">>, 400);
        true ->
            case olm_identity_logic:list_devices(TargetUid) of
                {ok, Payload} ->
                    elib_response:success(Req0, Payload);
                {error, Msg} ->
                    elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST)
            end
    end.

%% ===================================================================
%% POST /api/v1/e2ee/devices/batch_claim — 批量领取多设备 prekey（ADR 03 §8.2）
%% Body: {target_uid, device_ids: ["dev-a", "dev-b", ...]}
%% 返回: {claimed: {device_id => {type, key_id, key_base64, identity}}, failed: {...}}
%% ===================================================================

-spec batch_claim(cowboy_req:req(), map()) -> cowboy_req:req().
batch_claim(Req0, State) ->
    case ensure_e2ee_enabled(Req0) of
        ok ->
            do_batch_claim(Req0, State);
        {error, Req1} ->
            Req1
    end.

-spec do_batch_claim(cowboy_req:req(), map()) -> cowboy_req:req().
do_batch_claim(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    %% S1.3 / P1：per-claimant 限流，防止高频 claim 耗尽目标 OTK 池
    case scope_limited(olm_claim, CurrentUid) of
        true ->
            elib_response:error(Req0, <<"rate_limited">>, 429);
        false ->
            do_batch_claim1(Req0, CurrentUid)
    end.

-spec do_batch_claim1(cowboy_req:req(), integer()) -> cowboy_req:req().
do_batch_claim1(Req0, CurrentUid) ->
    PostVals = elib_param:post(Req0),
    TargetUidEnc = maps:get(<<"target_uid">>, PostVals, <<"">>),
    TargetUid = elib_cnv:safe_to_integer(TargetUidEnc),
    DeviceIds = normalize_device_ids(maps:get(<<"device_ids">>, PostVals, [])),
    %% E2EE-062 续：同一条抽干路径（一次请求多设备），目标级限流不得漏
    TargetLimited = target_rate_limited(TargetUid),
    case is_integer(TargetUid) andalso TargetUid > 0 andalso DeviceIds =/= [] of
        false ->
            elib_response:error(Req0, <<"bad_request">>, 400);
        true when TargetLimited ->
            elib_response:error(Req0, <<"rate_limited">>, 429);
        true ->
            %% E2EE-062 第三刀：多设备 fan-out 同样要吃幂等租约——一次重试
            %% 消费 N 条 OTK，抽干速度是单设备路径的 N 倍。
            RequestId = normalize_request_id(maps:get(<<"request_id">>, PostVals, <<>>)),
            %% 保留对 batch_claim_keys/3 的原调用形状（既有测试按 arity 挂 meck 期望）
            Claimed =
                case RequestId of
                    <<>> ->
                        olm_identity_logic:batch_claim_keys(CurrentUid, TargetUid, DeviceIds);
                    _ ->
                        olm_identity_logic:batch_claim_keys(
                            CurrentUid, TargetUid, DeviceIds, RequestId
                        )
                end,
            case Claimed of
                {ok, Payload} ->
                    elib_response:success(Req0, Payload);
                {error, Msg} ->
                    elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST)
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
    %% S1.3 / P1：per-claimant 限流，防止高频 claim 耗尽目标 OTK 池
    case scope_limited(olm_claim, CurrentUid) of
        true ->
            elib_response:error(Req0, <<"rate_limited">>, 429);
        false ->
            do_claim_key1(Req0, CurrentUid)
    end.

-spec do_claim_key1(cowboy_req:req(), integer()) -> cowboy_req:req().
do_claim_key1(Req0, CurrentUid) ->
    PostVals = elib_param:post(Req0),
    TargetUidEnc = maps:get(<<"target_uid">>, PostVals, <<"">>),
    TargetUid = elib_cnv:safe_to_integer(TargetUidEnc),
    DeviceId = maps:get(<<"device_id">>, PostVals, <<>>),
    %% E2EE-062 续：目标级抗耗尽限流。在参数校验**之后**才计入配额——
    %% 非法 target_uid 不得消耗目标的预算，否则畸形请求即可把合法用户挡在门外。
    TargetLimited = target_rate_limited(TargetUid),
    case is_integer(TargetUid) andalso TargetUid > 0 andalso byte_size(DeviceId) > 0 of
        false ->
            elib_response:error(Req0, <<"bad_request">>, 400);
        true when TargetLimited ->
            elib_response:error(Req0, <<"rate_limited">>, 429);
        true ->
            %% E2EE-062：幂等租约键（可选）。同一领取方重放同一 request_id
            %% 只消费一条 OTK；缺省 <<>> 时保持旧的逐次消费语义（旧客户端零破坏）。
            RequestId = normalize_request_id(maps:get(<<"request_id">>, PostVals, <<>>)),
            %% 保留对 claim_keys/3 的原调用形状——既有测试按 arity 挂 meck 期望，
            %% 无脑改走 /4 会让它们静默穿透到真实实现（A2-a 已实证过同款回归）
            Claimed =
                case RequestId of
                    <<>> -> olm_identity_logic:claim_keys(CurrentUid, TargetUid, DeviceId);
                    _ -> olm_identity_logic:claim_keys(CurrentUid, TargetUid, DeviceId, RequestId)
                end,
            case Claimed of
                {ok, Payload} ->
                    elib_response:success(Req0, Payload);
                {error, Msg} ->
                    elib_response:error(Req0, Msg, ?ERR_NOT_FOUND)
            end
    end.

%% ===================================================================
%% GET /api/v1/e2ee/olm/prekey_count — 本设备 OTK 余量（E2EE-062 低水位补传）
%% 返回: {count: N}
%% ===================================================================

-spec prekey_count(cowboy_req:req(), map()) -> cowboy_req:req().
prekey_count(Req0, State) ->
    case ensure_e2ee_enabled(Req0) of
        ok ->
            do_prekey_count(Req0, State);
        {error, Req1} ->
            Req1
    end.

%% 查询对象（uid + device_id）**只取自 token，不接受任何入参**。
%% 接受入参就等于对外开放「探测某用户某设备的 prekey 池还剩多少」——
%% 攻击者据此可以精确判断耗尽攻击何时奏效（见 E2EE-062 目标级限流的威胁模型）。
%% 余量本身不是秘密，但「谁的池快空了」是。
-spec do_prekey_count(cowboy_req:req(), map()) -> cowboy_req:req().
do_prekey_count(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    case auth_ds:current_did(State) of
        <<>> ->
            %% legacy token 未绑定设备：与其余 crypto 端点同一 fail-closed 语义
            %% （E2EE-013），要求重新登录换取绑定 token。
            elib_response:error(Req0, <<"device_binding_required">>, ?ERR_FORBIDDEN);
        Did ->
            case olm_identity_logic:count_one_time_keys(CurrentUid, Did) of
                {ok, Count} ->
                    elib_response:success(Req0, #{<<"count">> => Count});
                {error, Msg} ->
                    elib_response:error(Req0, Msg, ?ERR_INTERNAL_SERVER_ERROR)
            end
    end.

%% ===================================================================
%% E2EE-013：Crypto 写端点设备所有权守卫。
%% - token 未绑定 DID（legacy）→ fail-closed（要求重新登录换取绑定 token）。
%% - body device_id 必须等于 token 绑定的 DID，否则同账号越权覆盖别设备密钥 → 403。
%% 完整可撤销 device-bound session（校验 active/撤销）见 E2EE-030。
%% ===================================================================
-spec ensure_device_owner(cowboy_req:req(), map(), binary()) ->
    ok | {error, cowboy_req:req()}.
ensure_device_owner(Req0, State, BodyDeviceId) ->
    CurrentDid = auth_ds:current_did(State),
    case device_write_decision(CurrentDid, BodyDeviceId) of
        ok ->
            ok;
        device_binding_required ->
            {error,
                elib_response:error(
                    Req0, <<"device_binding_required">>, ?ERR_FORBIDDEN
                )};
        device_mismatch ->
            {error,
                elib_response:error(
                    Req0, <<"device_mismatch">>, ?ERR_FORBIDDEN
                )}
    end.

%% @doc 纯谓词：crypto 写端点设备所有权判定（E2EE-013 / DT-01/02）。
%% - CurrentDid=<<>>（legacy token 未绑定）→ device_binding_required（fail-closed）。
%% - body device_id 为空 → device_mismatch（不允许空设备写）。
%% - body device_id 必须等于 token 绑定 DID，否则 device_mismatch（同账号越权）。
-spec device_write_decision(binary(), binary()) ->
    ok | device_binding_required | device_mismatch.
device_write_decision(<<>>, _BodyDeviceId) ->
    device_binding_required;
device_write_decision(_CurrentDid, <<>>) ->
    device_mismatch;
device_write_decision(Did, Did) ->
    ok;
device_write_decision(_CurrentDid, _BodyDeviceId) ->
    device_mismatch.

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

%% @doc 归一化 device_ids 列表（接受 binary / string 元素，剔除非法）。
-spec normalize_device_ids(term()) -> [binary()].
normalize_device_ids(List) when is_list(List) ->
    [to_bin(D) || D <- List, is_binary(D) orelse is_list(D)];
normalize_device_ids(_) ->
    [].

-spec to_bin(term()) -> binary().
to_bin(B) when is_binary(B) -> B;
to_bin(L) when is_list(L) -> list_to_binary(L);
to_bin(_) -> <<>>.

%% @private E2EE-062：request_id 白名单归一化。
%%  只接受 base64url / hex 安全字符集，长度 <= 64（对齐
%%  priv/migrations/00000049 的 varchar(64)）；不合规一律降级为 <<>>
%%  （= 无幂等，逐次消费），**不报错**——幂等键是可选优化，
%%  不是安全边界；用它来拒绝请求反而给了攻击者一个新的拒绝服务面。
-spec normalize_request_id(term()) -> binary().
normalize_request_id(Bin) when is_binary(Bin), byte_size(Bin) > 0, byte_size(Bin) =< 64 ->
    case re:run(Bin, <<"^[A-Za-z0-9_.-]+$">>, [{capture, none}]) of
        match -> Bin;
        nomatch -> <<>>
    end;
normalize_request_id(_) ->
    <<>>.

%% @private E2EE-062 续：**目标级**抗耗尽限流。
%%
%%  per-claimant 那一层（`throttle:check(olm_claim, CurrentUid)`）的键是
%%  **领取方**，与被抽干的**目标**无关——换个账号就绕过。幂等租约也只挡
%%  「同一个 request_id 重放」，挡不住「每次换新 request_id」。
%%  因此必须再加一层以**目标 uid** 为键的限流，否则：
%%    - N 个协同账号，每个都在自己的 30/min 之内；
%%    - 或单账号每次换新 request_id；
%%  都能把同一个用户的 OTK 池抽干，把其所有新会话逼到复用同一条
%%  fallback prekey（前向保密显著下降）。
%%
%%  ponytail: 只做单目标一层；playbook 还要求单租户 / 全局两层。
%%    单机部署下「单租户」与「全局」等价，且全局面更应由网关承担；
%%    等真有多租户部署或压测暴露出全局面时再加，届时在本函数内扩展。
%%  ⚠️ 已实证：`throttle:check/2` 遇到**未注册的 scope** 返回原子
%%  `rate_not_set`（不崩），会被 `_ -> false` 当成「未超限」静默放行——
%%  也就是说配置漂移（sys.config 少了这条 scope）会让整道限流**无声消失**。
%%  这里显式识别该原子并打 ERROR 日志，让配置错误可见。
%%  不改成 fail-closed：scope 缺失是配置错误而非攻击，拒掉全部 claim 会让
%%  整个 E2EE 建会话不可用，代价远大于「限流暂时失效」。
-spec target_rate_limited(term()) -> boolean().
target_rate_limited(TargetUid) when is_integer(TargetUid), TargetUid > 0 ->
    scope_limited(olm_claim_target, TargetUid);
target_rate_limited(_) ->
    false.

%% @private E2EE-062：**所有** OTK 限流门的统一判定。
%%
%%  两道门（per-claimant `olm_claim` 与 per-target `olm_claim_target`）此前各写
%%  一份 case，目标层修过 `rate_not_set` 而领取方层没修——正是这种复制粘贴让
%%  同一个缺陷在一处修好、在另一处继续存在。此处收敛成唯一判定点。
%%
%%  ⚠️ 已实证：`throttle:check/2` 遇到**未注册的 scope** 返回原子 `rate_not_set`
%%  （不崩）。朴素的 `_ -> false` 会把它当成「未超限」静默放行——也就是说
%%  `sys.config` 少写一条 scope，整道限流**无声消失**且没有任何信号。
%%  这里显式识别该原子并打 ERROR 日志，让配置漂移可见。
%%
%%  **不**改成 fail-closed：scope 缺失是配置错误而非攻击，拒掉全部 claim 会让
%%  整个 E2EE 建会话不可用，代价远大于「限流暂时失效」。
-spec scope_limited(atom(), term()) -> boolean().
scope_limited(Scope, Key) ->
    case throttle:check(Scope, Key) of
        {limit_exceeded, _, _} ->
            true;
        rate_not_set ->
            _ = ?ERROR_LOG({olm_throttle_scope_missing, Scope}),
            false;
        _ ->
            false
    end.
