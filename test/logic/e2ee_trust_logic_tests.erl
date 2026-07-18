-module(e2ee_trust_logic_tests).
%%%
%% ADR 06 §9.4 Device Trust 服务端守护测试（T-06-11..13 + 转换白名单 + canonical）。
%%%

-include_lib("eunit/include/eunit.hrl").

-define(WITH_MECKS(Modules, Fun),
    (fun() ->
        ok = meck:new(Modules, [passthrough, no_link]),
        try
            Fun()
        after
            meck:unload(Modules)
        end
    end)()
).

%% 生成一个 actor Ed25519 密钥对，对给定 canonical 负载签名，返回 {PubB64, SigB64}
sign_event(TargetUid, TargetDeviceId, TargetEd25519, FromState, ToState, TsBin) ->
    {Pub, Priv} = crypto:generate_key(eddsa, ed25519),
    Canonical = e2ee_trust_logic:canonical_payload(
        TargetUid, TargetDeviceId, TargetEd25519, FromState, ToState, TsBin
    ),
    Sig = crypto:sign(eddsa, none, Canonical, [Priv, ed25519]),
    {base64:encode(Pub), base64:encode(Sig)}.

%% ===================================================================
%% T-06-11 append-only：repo 无 update/delete API（代码级不可变审计）
%% ===================================================================

appendonly_repo_has_no_mutation_api_test() ->
    Names = [N || {N, _} <- trust_audit_repo:module_info(exports)],
    ?assert(lists:member(insert_event, Names)),
    ?assertNot(lists:member(update, Names)),
    ?assertNot(lists:member(delete, Names)),
    ?assertNot(lists:member(update_event, Names)),
    ?assertNot(lists:member(delete_event, Names)).

%% ===================================================================
%% T-06-13 有效带签事件：验签通过 → 写审计 + 广播 e2ee_trust_changed
%% ===================================================================

valid_event_writes_and_broadcasts_test() ->
    ?WITH_MECKS([olm_identity_ds, trust_audit_ds, msg_s2c_ds], fun() ->
        {PubB64, SigB64} = sign_event(
            200, <<"phone-b">>, <<"ed-b">>, <<"unverified">>, <<"verified">>, <<"1721300000">>
        ),
        meck:expect(olm_identity_ds, find_identity, 2, fun(100, <<"phone-a">>) ->
            {ok, #{<<"ed25519_key">> => PubB64}}
        end),
        meck:expect(trust_audit_ds, insert_event, 8, fun(_, _, _, _, _, _, _, _) -> {ok, 1} end),
        meck:expect(msg_s2c_ds, send, 7, fun(_, _, _, _, _, _, _) -> ok end),
        Result = e2ee_trust_logic:record_trust_event(
            100,
            <<"phone-a">>,
            200,
            <<"phone-b">>,
            <<"ed-b">>,
            <<"unverified">>,
            <<"verified">>,
            <<"qr_scan">>,
            <<"1721300000">>,
            SigB64
        ),
        ?assertEqual(ok, Result),
        ?assertEqual(1, meck:num_calls(trust_audit_ds, insert_event, '_')),
        %% 广播 action = e2ee_trust_changed，非 revoked 只发给 actor 自己（多设备同步）
        [{_Pid, {_M, _F, Args}, _Res}] = meck:history(msg_s2c_ds),
        [_From, ToUids, Action | _] = Args,
        ?assertEqual(<<"e2ee_trust_changed">>, Action),
        ?assertEqual([100], ToUids)
    end).

%% revoked：广播额外发给对端用户（§8.3）
revoked_event_broadcasts_to_target_test() ->
    ?WITH_MECKS([olm_identity_ds, trust_audit_ds, msg_s2c_ds], fun() ->
        {PubB64, SigB64} = sign_event(
            200, <<"phone-b">>, <<"ed-b">>, <<"verified">>, <<"revoked">>, <<"1721300001">>
        ),
        meck:expect(olm_identity_ds, find_identity, 2, fun(_, _) ->
            {ok, #{<<"ed25519_key">> => PubB64}}
        end),
        meck:expect(trust_audit_ds, insert_event, 8, fun(_, _, _, _, _, _, _, _) -> {ok, 1} end),
        meck:expect(msg_s2c_ds, send, 7, fun(_, _, _, _, _, _, _) -> ok end),
        ok = e2ee_trust_logic:record_trust_event(
            100,
            <<"phone-a">>,
            200,
            <<"phone-b">>,
            <<"ed-b">>,
            <<"verified">>,
            <<"revoked">>,
            <<"revoke">>,
            <<"1721300001">>,
            SigB64
        ),
        [{_Pid, {_M, _F, Args}, _Res}] = meck:history(msg_s2c_ds),
        [_From, ToUids | _] = Args,
        ?assertEqual([100, 200], ToUids)
    end).

%% ===================================================================
%% T-06-12 验签失败：拒写 + 拒广播（防 T7 伪造）
%% ===================================================================

bad_signature_rejects_write_and_broadcast_test() ->
    ?WITH_MECKS([olm_identity_ds, trust_audit_ds, msg_s2c_ds], fun() ->
        %% actor 公钥来自另一对密钥，与传入签名不匹配 → 验签失败
        {WrongPubB64, _} = sign_event(200, <<"x">>, <<"y">>, <<"a">>, <<"b">>, <<"1">>),
        {_, RealSigB64} = sign_event(
            200, <<"phone-b">>, <<"ed-b">>, <<"unverified">>, <<"verified">>, <<"1721300000">>
        ),
        meck:expect(olm_identity_ds, find_identity, 2, fun(_, _) ->
            {ok, #{<<"ed25519_key">> => WrongPubB64}}
        end),
        meck:expect(trust_audit_ds, insert_event, 8, fun(_, _, _, _, _, _, _, _) -> {ok, 1} end),
        meck:expect(msg_s2c_ds, send, 7, fun(_, _, _, _, _, _, _) -> ok end),
        Result = e2ee_trust_logic:record_trust_event(
            100,
            <<"phone-a">>,
            200,
            <<"phone-b">>,
            <<"ed-b">>,
            <<"unverified">>,
            <<"verified">>,
            <<"qr_scan">>,
            <<"1721300000">>,
            RealSigB64
        ),
        ?assertEqual({error, <<"invalid_signature">>}, Result),
        ?assertEqual(0, meck:num_calls(trust_audit_ds, insert_event, '_')),
        ?assertEqual(0, meck:num_calls(msg_s2c_ds, send, '_'))
    end).

%% actor 设备未注册 olm 身份 → 无验签公钥
actor_device_not_registered_test() ->
    ?WITH_MECKS([olm_identity_ds], fun() ->
        meck:expect(olm_identity_ds, find_identity, 2, fun(_, _) -> {ok, not_found} end),
        ?assertEqual(
            {error, <<"actor_device_not_registered">>},
            e2ee_trust_logic:record_trust_event(
                100,
                <<"phone-a">>,
                200,
                <<"phone-b">>,
                <<"ed-b">>,
                <<"unverified">>,
                <<"verified">>,
                <<"qr_scan">>,
                <<"1">>,
                <<"sig">>
            )
        )
    end).

%% ===================================================================
%% 状态转换白名单（ADR 06 §3.2）
%% ===================================================================

%% 非法转换 revoked→verified（绕过 unverified）在验签前即拒
illegal_transition_rejected_before_verify_test() ->
    ?WITH_MECKS([olm_identity_ds], fun() ->
        meck:expect(olm_identity_ds, find_identity, 2, fun(_, _) ->
            {ok, #{<<"ed25519_key">> => <<"x">>}}
        end),
        ?assertEqual(
            {error, <<"invalid_transition">>},
            e2ee_trust_logic:record_trust_event(
                100,
                <<"phone-a">>,
                200,
                <<"phone-b">>,
                <<"ed-b">>,
                <<"revoked">>,
                <<"verified">>,
                <<"qr_scan">>,
                <<"1">>,
                <<"sig">>
            )
        ),
        %% 未查身份（转换校验在验签前）
        ?assertEqual(0, meck:num_calls(olm_identity_ds, find_identity, '_'))
    end).

same_state_transition_rejected_test() ->
    ?assertEqual(
        {error, <<"invalid_transition">>},
        e2ee_trust_logic:record_trust_event(
            100,
            <<"d">>,
            200,
            <<"e">>,
            <<"ed">>,
            <<"verified">>,
            <<"verified">>,
            <<"qr_scan">>,
            <<"1">>,
            <<"sig">>
        )
    ).

%% 非法 method 拒收
invalid_method_rejected_test() ->
    ?assertEqual(
        {error, <<"bad_request">>},
        e2ee_trust_logic:record_trust_event(
            100,
            <<"d">>,
            200,
            <<"e">>,
            <<"ed">>,
            <<"unverified">>,
            <<"verified">>,
            <<"hacked">>,
            <<"1">>,
            <<"sig">>
        )
    ).

%% 空必填字段拒收
empty_field_rejected_test() ->
    ?assertEqual(
        {error, <<"bad_request">>},
        e2ee_trust_logic:record_trust_event(
            100,
            <<>>,
            200,
            <<"e">>,
            <<"ed">>,
            <<"unverified">>,
            <<"verified">>,
            <<"qr_scan">>,
            <<"1">>,
            <<"sig">>
        )
    ).

%% ===================================================================
%% canonical 负载确定性（客户端须用同格式签名）
%% ===================================================================

canonical_payload_deterministic_test() ->
    A = e2ee_trust_logic:canonical_payload(
        200, <<"d">>, <<"ed">>, <<"unverified">>, <<"verified">>, <<"9">>
    ),
    B = e2ee_trust_logic:canonical_payload(
        200, <<"d">>, <<"ed">>, <<"unverified">>, <<"verified">>, <<"9">>
    ),
    ?assertEqual(A, B),
    %% 任一字段变化 → 负载变化（雪崩前提）
    C = e2ee_trust_logic:canonical_payload(
        201, <<"d">>, <<"ed">>, <<"unverified">>, <<"verified">>, <<"9">>
    ),
    ?assertNotEqual(A, C).
