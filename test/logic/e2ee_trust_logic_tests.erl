-module(e2ee_trust_logic_tests).
%%%
%% ADR 06 §9.4 + ADR 16 §3.3.1（E2EE-014）Device Trust 服务端守护测试。
%% 覆盖：append-only、验签、状态机白名单、canonical 确定性、freshness 时间窗（-1/0/+1ms）、
%% event_id 幂等（重放不重播）、target_identity_version 单调、撤销 actor / 旧设备代数拒绝。
%%%

-include_lib("eunit/include/eunit.hrl").

%% 测试固定 "now"（ms）；freshness 用它做确定性边界
-define(NOW, 1721300000000).

-define(MECKS, [olm_identity_ds, trust_audit_ds, msg_s2c_ds, elib_dt]).

-define(WITH_MECKS(Fun),
    (fun() ->
        ok = meck:new(?MECKS, [passthrough, no_link]),
        try
            Fun()
        after
            meck:unload(?MECKS)
        end
    end)()
).

%% ===================================================================
%% 测试辅助
%% ===================================================================

%% 完整合法 Fields（issued_at=NOW，60s TTL，gen/ver=1）
base_fields() ->
    #{
        <<"actor_device_id">> => <<"phone-a">>,
        <<"target_uid">> => 200,
        <<"target_device_id">> => <<"phone-b">>,
        <<"target_ed25519">> => <<"ed-b">>,
        <<"from_state">> => <<"unverified">>,
        <<"to_state">> => <<"verified">>,
        <<"method">> => <<"qr_scan">>,
        <<"event_id">> => <<"evt-1">>,
        <<"issued_at">> => ?NOW,
        <<"expires_at">> => ?NOW + 60000,
        <<"actor_device_generation">> => 1,
        <<"target_identity_version">> => 1,
        <<"actor_signature">> => <<>>
    }.

%% 用新密钥对 Fields 的 canonical 签名，返回 {PubB64, SignedFields}
sign(ActorUid, F) ->
    {Pub, Priv} = crypto:generate_key(eddsa, ed25519),
    Canonical = e2ee_trust_logic:canonical_payload(#{
        <<"actor_device_generation">> => maps:get(<<"actor_device_generation">>, F),
        <<"actor_uid">> => ActorUid,
        <<"event_id">> => maps:get(<<"event_id">>, F),
        <<"expires_at">> => maps:get(<<"expires_at">>, F),
        <<"from_state">> => maps:get(<<"from_state">>, F),
        <<"issued_at">> => maps:get(<<"issued_at">>, F),
        <<"target_device_id">> => maps:get(<<"target_device_id">>, F),
        <<"target_ed25519">> => maps:get(<<"target_ed25519">>, F),
        <<"target_identity_version">> => maps:get(<<"target_identity_version">>, F),
        <<"target_uid">> => maps:get(<<"target_uid">>, F),
        <<"to_state">> => maps:get(<<"to_state">>, F)
    }),
    Sig = crypto:sign(eddsa, none, Canonical, [Priv, ed25519]),
    {base64:encode(Pub), F#{<<"actor_signature">> => base64:encode(Sig)}}.

%% 安装 happy-path 默认 mock（actor active gen=1、无历史版本、insert=inserted、now=NOW）
setup_ok(PubB64) ->
    meck:expect(olm_identity_ds, find_identity, 2, fun(_, _) ->
        {ok, #{<<"ed25519_key">> => PubB64}}
    end),
    meck:expect(trust_audit_ds, actor_device_state, 2, fun(_, _) ->
        {ok, #{<<"status">> => 1, <<"device_generation">> => 1}}
    end),
    meck:expect(trust_audit_ds, insert_event, 1, fun(_) -> {ok, inserted} end),
    meck:expect(msg_s2c_ds, send, 7, fun(_, _, _, _, _, _, _) -> ok end),
    meck:expect(elib_dt, millisecond, 0, fun() -> ?NOW end).

record(F) ->
    e2ee_trust_logic:record_trust_event(100, F).

%% ===================================================================
%% T-06-11 append-only：repo 无 update/delete API
%% ===================================================================

appendonly_repo_has_no_mutation_api_test() ->
    Names = [N || {N, _} <- trust_audit_repo:module_info(exports)],
    ?assert(lists:member(insert_event, Names)),
    ?assertNot(lists:member(update, Names)),
    ?assertNot(lists:member(delete, Names)),
    ?assertNot(lists:member(update_event, Names)),
    ?assertNot(lists:member(delete_event, Names)).

%% ===================================================================
%% T-06-13 有效带签事件：写审计 + 广播（非 revoked 仅发 actor 自己）
%% ===================================================================

valid_event_writes_and_broadcasts_test() ->
    ?WITH_MECKS(fun() ->
        {Pub, F} = sign(100, base_fields()),
        setup_ok(Pub),
        ?assertEqual(ok, record(F)),
        ?assertEqual(1, meck:num_calls(trust_audit_ds, insert_event, '_')),
        [{_, {_, _, Args}, _}] = meck:history(msg_s2c_ds),
        [_From, ToUids, Action | _] = Args,
        ?assertEqual(<<"e2ee_trust_changed">>, Action),
        ?assertEqual([100], ToUids)
    end).

revoked_event_broadcasts_to_target_test() ->
    ?WITH_MECKS(fun() ->
        F0 = (base_fields())#{<<"from_state">> => <<"verified">>, <<"to_state">> => <<"revoked">>},
        {Pub, F} = sign(100, F0),
        setup_ok(Pub),
        ?assertEqual(ok, record(F)),
        [{_, {_, _, Args}, _}] = meck:history(msg_s2c_ds),
        [_From, ToUids | _] = Args,
        ?assertEqual([100, 200], ToUids)
    end).

%% ===================================================================
%% T-06-12 验签失败：拒写 + 拒广播（防 T7 伪造）
%% ===================================================================

bad_signature_rejects_test() ->
    ?WITH_MECKS(fun() ->
        {_RealPub, F} = sign(100, base_fields()),
        %% actor 公钥换成另一对 → 验签失败
        {WrongPub, _} = sign(100, (base_fields())#{<<"event_id">> => <<"other">>}),
        setup_ok(WrongPub),
        ?assertEqual({error, <<"invalid_signature">>}, record(F)),
        ?assertEqual(0, meck:num_calls(trust_audit_ds, insert_event, '_')),
        ?assertEqual(0, meck:num_calls(msg_s2c_ds, send, '_'))
    end).

actor_device_not_registered_test() ->
    ?WITH_MECKS(fun() ->
        {_Pub, F} = sign(100, base_fields()),
        meck:expect(elib_dt, millisecond, 0, fun() -> ?NOW end),
        meck:expect(olm_identity_ds, find_identity, 2, fun(_, _) -> {ok, not_found} end),
        ?assertEqual({error, <<"actor_device_not_registered">>}, record(F))
    end).

%% ===================================================================
%% E2EE-014 撤销 actor / 旧设备代数拒绝
%% ===================================================================

revoked_actor_rejected_test() ->
    ?WITH_MECKS(fun() ->
        {Pub, F} = sign(100, base_fields()),
        setup_ok(Pub),
        meck:expect(trust_audit_ds, actor_device_state, 2, fun(_, _) ->
            {ok, #{<<"status">> => 0, <<"device_generation">> => 1}}
        end),
        ?assertEqual({error, <<"actor_device_revoked">>}, record(F)),
        ?assertEqual(0, meck:num_calls(trust_audit_ds, insert_event, '_'))
    end).

device_generation_mismatch_rejected_test() ->
    ?WITH_MECKS(fun() ->
        {Pub, F} = sign(100, base_fields()),
        setup_ok(Pub),
        %% 存储代数=2，签名声明=1（旧设备重放）→ 拒
        meck:expect(trust_audit_ds, actor_device_state, 2, fun(_, _) ->
            {ok, #{<<"status">> => 1, <<"device_generation">> => 2}}
        end),
        ?assertEqual({error, <<"actor_device_revoked">>}, record(F))
    end).

%% ===================================================================
%% E2EE-014 freshness 时间窗（-1/0/+1ms 边界）
%% ===================================================================

fresh_past_boundary_accept_test() ->
    ?WITH_MECKS(fun() ->
        %% issued 在过去窗口边界（NOW-300000），expires 恰达 now（TTL=满 300000）→ 未过期，接受
        F0 = (base_fields())#{
            <<"issued_at">> => ?NOW - 300000, <<"expires_at">> => ?NOW
        },
        {Pub, F} = sign(100, F0),
        setup_ok(Pub),
        ?assertEqual(ok, record(F))
    end).

fresh_past_boundary_reject_test() ->
    ?WITH_MECKS(fun() ->
        F0 = (base_fields())#{
            <<"issued_at">> => ?NOW - 300001, <<"expires_at">> => ?NOW - 300001 + 60000
        },
        {Pub, F} = sign(100, F0),
        setup_ok(Pub),
        ?assertEqual({error, <<"stale_event">>}, record(F)),
        ?assertEqual(0, meck:num_calls(trust_audit_ds, insert_event, '_'))
    end).

fresh_future_boundary_reject_test() ->
    ?WITH_MECKS(fun() ->
        F0 = (base_fields())#{
            <<"issued_at">> => ?NOW + 120001, <<"expires_at">> => ?NOW + 120001 + 60000
        },
        {Pub, F} = sign(100, F0),
        setup_ok(Pub),
        ?assertEqual({error, <<"stale_event">>}, record(F))
    end).

expired_event_reject_test() ->
    ?WITH_MECKS(fun() ->
        %% issued 合法但 expires 已过（now > expires）
        F0 = (base_fields())#{<<"issued_at">> => ?NOW - 100000, <<"expires_at">> => ?NOW - 1},
        {Pub, F} = sign(100, F0),
        setup_ok(Pub),
        ?assertEqual({error, <<"stale_event">>}, record(F))
    end).

%% ===================================================================
%% E2EE-014 event_id 幂等：重放不重播、不重复审计
%% ===================================================================

duplicate_event_id_idempotent_test() ->
    ?WITH_MECKS(fun() ->
        {Pub, F} = sign(100, base_fields()),
        setup_ok(Pub),
        meck:expect(trust_audit_ds, insert_event, 1, fun(_) -> {ok, duplicate} end),
        ?assertEqual(ok, record(F)),
        %% 幂等命中：不广播
        ?assertEqual(0, meck:num_calls(msg_s2c_ds, send, '_'))
    end).

%% ===================================================================
%% E2EE-014 target_identity_version 单调（回退拒绝，不靠幂等绕过状态机）
%% ===================================================================

identity_version_rollback_rejected_test() ->
    ?WITH_MECKS(fun() ->
        {Pub, F} = sign(100, base_fields()),
        setup_ok(Pub),
        %% 版本单调校验已下沉进 insert_event 单事务（锁内读 MAX 后决定），
        %% 回退作为 insert_event 的语义错误返回，logic 透传（不再是插入前拦截）。
        meck:expect(trust_audit_ds, insert_event, 1, fun(_) ->
            {error, <<"identity_version_rollback">>}
        end),
        ?assertEqual({error, <<"identity_version_rollback">>}, record(F)),
        ?assertEqual(1, meck:num_calls(trust_audit_ds, insert_event, '_'))
    end).

%% event_id 被他人抢占（归属不符）→ 拒绝，不静默吞掉合法事件
event_id_conflict_rejected_test() ->
    ?WITH_MECKS(fun() ->
        {Pub, F} = sign(100, base_fields()),
        setup_ok(Pub),
        meck:expect(trust_audit_ds, insert_event, 1, fun(_) ->
            {error, <<"event_id_conflict">>}
        end),
        ?assertEqual({error, <<"event_id_conflict">>}, record(F))
    end).

%% ===================================================================
%% 状态机白名单（ADR 06 §3.2）—— 幂等不得绕过
%% ===================================================================

illegal_transition_rejected_before_verify_test() ->
    ?WITH_MECKS(fun() ->
        F0 = (base_fields())#{<<"from_state">> => <<"revoked">>, <<"to_state">> => <<"verified">>},
        {Pub, F} = sign(100, F0),
        setup_ok(Pub),
        ?assertEqual({error, <<"invalid_transition">>}, record(F)),
        %% 转换校验在验签/DB 前
        ?assertEqual(0, meck:num_calls(olm_identity_ds, find_identity, '_'))
    end).

same_state_transition_rejected_test() ->
    ?WITH_MECKS(fun() ->
        F0 = (base_fields())#{<<"from_state">> => <<"verified">>, <<"to_state">> => <<"verified">>},
        {Pub, F} = sign(100, F0),
        setup_ok(Pub),
        ?assertEqual({error, <<"invalid_transition">>}, record(F))
    end).

invalid_method_rejected_test() ->
    F = (base_fields())#{<<"method">> => <<"hacked">>},
    ?assertEqual({error, <<"bad_request">>}, record(F)).

empty_event_id_rejected_test() ->
    F = (base_fields())#{<<"event_id">> => <<>>},
    ?assertEqual({error, <<"bad_request">>}, record(F)).

empty_device_field_rejected_test() ->
    F = (base_fields())#{<<"actor_device_id">> => <<>>},
    ?assertEqual({error, <<"bad_request">>}, record(F)).

non_integer_issued_at_rejected_test() ->
    F = (base_fields())#{<<"issued_at">> => <<"not-a-number">>},
    ?assertEqual({error, <<"bad_request">>}, record(F)).

%% ===================================================================
%% canonical 负载确定性（客户端须用同格式签名）
%% ===================================================================

canonical_payload_deterministic_test() ->
    M = #{
        <<"actor_device_generation">> => 1,
        <<"actor_uid">> => 100,
        <<"event_id">> => <<"evt-1">>,
        <<"expires_at">> => 9,
        <<"from_state">> => <<"unverified">>,
        <<"issued_at">> => 8,
        <<"target_device_id">> => <<"d">>,
        <<"target_ed25519">> => <<"ed">>,
        <<"target_identity_version">> => 1,
        <<"target_uid">> => 200,
        <<"to_state">> => <<"verified">>
    },
    A = e2ee_trust_logic:canonical_payload(M),
    ?assertEqual(A, e2ee_trust_logic:canonical_payload(M)),
    %% 任一字段变化 → 负载变化（雪崩前提）
    ?assertNotEqual(A, e2ee_trust_logic:canonical_payload(M#{<<"target_uid">> => 201})),
    ?assertNotEqual(A, e2ee_trust_logic:canonical_payload(M#{<<"event_id">> => <<"evt-2">>})).
