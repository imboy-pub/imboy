-module(olm_identity_logic_tests).

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

%% ===================================================================
%% report_identity 参数校验
%% ===================================================================

report_identity_rejects_empty_keys_test() ->
    ?WITH_MECKS([olm_identity_ds], fun() ->
        ?assertEqual(
            {error, <<"invalid_identity_keys">>},
            olm_identity_logic:report_identity(100, <<"dev-A">>, <<>>, <<>>, <<>>, <<"ios">>)
        )
    end).

report_identity_rejects_bad_args_test() ->
    ?assertEqual(
        {error, <<"bad_request">>},
        olm_identity_logic:report_identity(
            <<"not_int">>, <<"dev-A">>, <<"e">>, <<"c">>, <<"s">>, <<"ios">>
        )
    ).

report_identity_ok_test() ->
    %% 使用有效 base64（P1-2 添加了 verify_ed25519 需 decode_base64 成功）
    %% C01 起首次注册路径需查旧身份判分流（not_found → PoP 即授权）
    ?WITH_MECKS([olm_identity_ds, crypto], fun() ->
        meck:expect(olm_identity_ds, find_identity, 2, fun(_, _) -> {ok, not_found} end),
        meck:expect(olm_identity_ds, upsert_identity, 6, fun(_, _, _, _, _, _) -> {ok, 1} end),
        meck:expect(crypto, verify, 5, fun(eddsa, none, _, _, _) -> true end),
        ?assertEqual(
            ok,
            olm_identity_logic:report_identity(
                100,
                <<"dev-A">>,
                <<"ZQ==">>,
                <<"Yw==">>,
                <<"cw==">>,
                <<"ios">>
            )
        )
    end).

%% ===================================================================
%% report_one_time_keys 参数校验
%% ===================================================================

report_one_time_keys_rejects_empty_test() ->
    ?assertEqual(
        {error, <<"invalid_key_count">>},
        olm_identity_logic:report_one_time_keys(100, <<"dev-A">>, [], 100)
    ).

report_one_time_keys_rejects_invalid_format_test() ->
    %% 含空 key_base64 的条目应被拒
    ?assertEqual(
        {error, <<"invalid_key_format">>},
        olm_identity_logic:report_one_time_keys(100, <<"dev-A">>, [{<<"k1">>, <<>>}], 100)
    ).

report_one_time_keys_ok_test() ->
    ?WITH_MECKS([olm_identity_ds], fun() ->
        meck:expect(olm_identity_ds, upsert_one_time_keys, 4, fun(_, _, Keys, _) ->
            {ok, length(Keys)}
        end),
        ?assertEqual(
            {ok, 2},
            olm_identity_logic:report_one_time_keys(
                100, <<"dev-A">>, [{<<"k1">>, <<"v1">>}, {<<"k2">>, <<"v2">>}], 100
            )
        )
    end).

%% ===================================================================
%% claim_keys 优先级：OTK 命中 → type=one_time
%% ===================================================================

claim_keys_prefers_one_time_test() ->
    ?WITH_MECKS([olm_identity_ds, friend_ds], fun() ->
        meck:expect(friend_ds, is_friend, 2, fun(_, _) -> true end),
        Identity = #{<<"device_id">> => <<"dev-B">>, <<"ed25519_key">> => <<"e">>},
        meck:expect(olm_identity_ds, find_identity, 2, fun(_Uid, _Did) -> {ok, Identity} end),
        meck:expect(
            olm_identity_ds,
            claim_one_time_key,
            3,
            fun(_Uid, _Did, _By) ->
                {ok, #{<<"key_id">> => <<"otk-1">>, <<"key_base64">> => <<"A">>}}
            end
        ),
        {ok, Result} = olm_identity_logic:claim_keys(100, 200, <<"dev-B">>),
        ?assertEqual(<<"one_time">>, maps:get(<<"type">>, Result)),
        ?assertEqual(<<"otk-1">>, maps:get(<<"key_id">>, Result)),
        ?assertEqual(Identity, maps:get(<<"identity">>, Result))
    end).

%% claim_keys 优先级：OTK 耗尽 → fallback 兜底
claim_keys_falls_back_when_otk_exhausted_test() ->
    ?WITH_MECKS([olm_identity_ds, friend_ds], fun() ->
        meck:expect(friend_ds, is_friend, 2, fun(_, _) -> true end),
        Identity = #{<<"device_id">> => <<"dev-B">>, <<"ed25519_key">> => <<"e">>},
        meck:expect(olm_identity_ds, find_identity, 2, fun(_Uid, _Did) -> {ok, Identity} end),
        meck:expect(
            olm_identity_ds,
            claim_one_time_key,
            3,
            fun(_Uid, _Did, _By) -> {error, exhausted} end
        ),
        meck:expect(
            olm_identity_ds,
            claim_fallback_key,
            2,
            fun(_Uid, _Did) -> {ok, #{<<"key_id">> => <<"fb-1">>, <<"key_base64">> => <<"B">>}} end
        ),
        {ok, Result} = olm_identity_logic:claim_keys(100, 200, <<"dev-B">>),
        ?assertEqual(<<"fallback">>, maps:get(<<"type">>, Result)),
        ?assertEqual(<<"fb-1">>, maps:get(<<"key_id">>, Result))
    end).

%% claim_keys：OTK + fallback 都耗尽 → no_prekey_available
claim_keys_returns_no_prekey_available_test() ->
    ?WITH_MECKS([olm_identity_ds, friend_ds], fun() ->
        meck:expect(friend_ds, is_friend, 2, fun(_, _) -> true end),
        Identity = #{<<"device_id">> => <<"dev-B">>},
        meck:expect(olm_identity_ds, find_identity, 2, fun(_Uid, _Did) -> {ok, Identity} end),
        meck:expect(
            olm_identity_ds,
            claim_one_time_key,
            3,
            fun(_Uid, _Did, _By) -> {error, exhausted} end
        ),
        meck:expect(olm_identity_ds, claim_fallback_key, 2, fun(_Uid, _Did) ->
            {error, exhausted}
        end),
        ?assertEqual(
            {error, <<"no_prekey_available">>},
            olm_identity_logic:claim_keys(100, 200, <<"dev-B">>)
        )
    end).

%% claim_keys：对端未注册身份键 → device_not_registered
claim_keys_unknown_device_test() ->
    ?WITH_MECKS([olm_identity_ds, friend_ds], fun() ->
        meck:expect(friend_ds, is_friend, 2, fun(_, _) -> true end),
        meck:expect(olm_identity_ds, find_identity, 2, fun(_Uid, _Did) -> {ok, not_found} end),
        ?assertEqual(
            {error, <<"device_not_registered">>},
            olm_identity_logic:claim_keys(100, 200, <<"dev-B">>)
        )
    end).

%% ===================================================================
%% get_identity
%% ===================================================================

get_identity_ok_test() ->
    ?WITH_MECKS([olm_identity_ds], fun() ->
        Row = #{<<"device_id">> => <<"dev-B">>, <<"curve25519_key">> => <<"c">>},
        meck:expect(olm_identity_ds, find_identity, 2, fun(_Uid, _Did) -> {ok, Row} end),
        ?assertEqual({ok, Row}, olm_identity_logic:get_identity(200, <<"dev-B">>))
    end).

get_identity_not_found_test() ->
    ?WITH_MECKS([olm_identity_ds], fun() ->
        meck:expect(olm_identity_ds, find_identity, 2, fun(_Uid, _Did) -> {ok, not_found} end),
        ?assertEqual({error, <<"not_found">>}, olm_identity_logic:get_identity(200, <<"dev-B">>))
    end).

%% ===================================================================
%% list_devices（ADR 03 §8.1 统一设备列表）
%% ===================================================================

list_devices_ok_test() ->
    ?WITH_MECKS([olm_identity_ds], fun() ->
        Devices = [
            #{<<"device_id">> => <<"phone-a">>, <<"capabilities">> => [<<"olm">>]},
            #{<<"device_id">> => <<"ipad-b">>, <<"capabilities">> => [<<"olm">>, <<"megolm">>]}
        ],
        meck:expect(olm_identity_ds, list_devices_with_identity, 1, fun(_) -> {ok, Devices} end),
        {ok, Payload} = olm_identity_logic:list_devices(200),
        ?assertEqual(200, maps:get(<<"user_id">>, Payload)),
        ?assertEqual(Devices, maps:get(<<"devices">>, Payload))
    end).

list_devices_empty_ok_test() ->
    ?WITH_MECKS([olm_identity_ds], fun() ->
        meck:expect(olm_identity_ds, list_devices_with_identity, 1, fun(_) -> {ok, []} end),
        {ok, Payload} = olm_identity_logic:list_devices(200),
        ?assertEqual([], maps:get(<<"devices">>, Payload))
    end).

list_devices_rejects_bad_uid_test() ->
    ?assertEqual({error, <<"bad_request">>}, olm_identity_logic:list_devices(0)),
    ?assertEqual({error, <<"bad_request">>}, olm_identity_logic:list_devices(<<"x">>)).

list_devices_maps_ds_error_test() ->
    ?WITH_MECKS([olm_identity_ds], fun() ->
        meck:expect(olm_identity_ds, list_devices_with_identity, 1, fun(_) -> {error, db_down} end),
        ?assertEqual({error, <<"internal_error">>}, olm_identity_logic:list_devices(200))
    end).

%% ===================================================================
%% batch_claim_keys（ADR 03 §8.2 多设备 fan-out）
%% ===================================================================

%% 多设备各自 claim 成功，聚合到 claimed，failed 为空
batch_claim_all_ok_test() ->
    ?WITH_MECKS([olm_identity_ds, friend_ds], fun() ->
        meck:expect(friend_ds, is_friend, 2, fun(_, _) -> true end),
        Identity = #{<<"device_id">> => <<"d">>},
        meck:expect(olm_identity_ds, find_identity, 2, fun(_, _) -> {ok, Identity} end),
        meck:expect(olm_identity_ds, claim_one_time_key, 3, fun(_, Did, _) ->
            {ok, #{<<"key_id">> => <<"otk-", Did/binary>>, <<"key_base64">> => <<"A">>}}
        end),
        {ok, Payload} = olm_identity_logic:batch_claim_keys(100, 200, [<<"a">>, <<"b">>]),
        Claimed = maps:get(<<"claimed">>, Payload),
        ?assertEqual(2, maps:size(Claimed)),
        ?assertEqual(#{}, maps:get(<<"failed">>, Payload)),
        ?assertEqual(<<"one_time">>, maps:get(<<"type">>, maps:get(<<"a">>, Claimed)))
    end).

%% 部分设备未注册 → 该设备落 failed，不中断其他设备
batch_claim_partial_failure_test() ->
    ?WITH_MECKS([olm_identity_ds, friend_ds], fun() ->
        meck:expect(friend_ds, is_friend, 2, fun(_, _) -> true end),
        meck:expect(olm_identity_ds, find_identity, 2, fun
            (_, <<"good">>) -> {ok, #{<<"device_id">> => <<"good">>}};
            (_, <<"bad">>) -> {ok, not_found}
        end),
        meck:expect(olm_identity_ds, claim_one_time_key, 3, fun(_, _, _) ->
            {ok, #{<<"key_id">> => <<"otk-1">>, <<"key_base64">> => <<"A">>}}
        end),
        {ok, Payload} = olm_identity_logic:batch_claim_keys(100, 200, [<<"good">>, <<"bad">>]),
        ?assertEqual(1, maps:size(maps:get(<<"claimed">>, Payload))),
        Failed = maps:get(<<"failed">>, Payload),
        ?assertEqual(<<"device_not_registered">>, maps:get(<<"bad">>, Failed))
    end).

%% 去重：重复 device_id 只 claim 一次
batch_claim_dedups_device_ids_test() ->
    ?WITH_MECKS([olm_identity_ds, friend_ds], fun() ->
        meck:expect(friend_ds, is_friend, 2, fun(_, _) -> true end),
        meck:expect(olm_identity_ds, find_identity, 2, fun(_, _) ->
            {ok, #{<<"device_id">> => <<"a">>}}
        end),
        meck:expect(olm_identity_ds, claim_one_time_key, 3, fun(_, _, _) ->
            {ok, #{<<"key_id">> => <<"otk-1">>, <<"key_base64">> => <<"A">>}}
        end),
        {ok, Payload} = olm_identity_logic:batch_claim_keys(100, 200, [<<"a">>, <<"a">>, <<"a">>]),
        ?assertEqual(1, maps:size(maps:get(<<"claimed">>, Payload))),
        ?assertEqual(1, meck:num_calls(olm_identity_ds, claim_one_time_key, '_'))
    end).

batch_claim_rejects_empty_test() ->
    ?assertEqual(
        {error, <<"no_device_ids">>},
        olm_identity_logic:batch_claim_keys(100, 200, [])
    ),
    %% 全为非法元素过滤后为空
    ?assertEqual(
        {error, <<"no_device_ids">>},
        olm_identity_logic:batch_claim_keys(100, 200, [<<>>, 123])
    ).

batch_claim_rejects_too_many_test() ->
    Ids = [integer_to_binary(N) || N <- lists:seq(1, 21)],
    ?assertEqual(
        {error, <<"too_many_devices">>},
        olm_identity_logic:batch_claim_keys(100, 200, Ids)
    ).

batch_claim_rejects_bad_args_test() ->
    ?assertEqual(
        {error, <<"bad_request">>},
        olm_identity_logic:batch_claim_keys(<<"x">>, 200, [<<"a">>])
    ).

%% ===================================================================
%% Device API 契约冻结（ADR 03 §8.1/§8.2）
%% logic 返回的 map 即 handler 透传给客户端的响应体（elib_response:success/2），
%% 故断言 map 顶层键集 = 冻结 wire 契约。PR 改动契约时本测试立即可见。
%% ===================================================================

%% 契约冻结：list_devices 响应恰含 {user_id, devices}
contract_list_devices_shape_test() ->
    ?WITH_MECKS([olm_identity_ds], fun() ->
        Dev = #{<<"device_id">> => <<"d">>, <<"capabilities">> => [<<"olm">>]},
        meck:expect(olm_identity_ds, list_devices_with_identity, 1, fun(_) -> {ok, [Dev]} end),
        {ok, Payload} = olm_identity_logic:list_devices(200),
        ?assertEqual(
            [<<"devices">>, <<"user_id">>],
            lists:sort(maps:keys(Payload))
        )
    end).

%% 契约冻结：batch_claim 响应恰含 {claimed, failed}；claimed 项恰含
%% {type, key_id, key_base64, identity}（X3DH 客户端 createOutboundSession 依赖）
contract_batch_claim_shape_test() ->
    ?WITH_MECKS([olm_identity_ds, friend_ds], fun() ->
        meck:expect(friend_ds, is_friend, 2, fun(_, _) -> true end),
        Identity = #{<<"device_id">> => <<"a">>, <<"ed25519_key">> => <<"e">>},
        meck:expect(olm_identity_ds, find_identity, 2, fun(_, _) -> {ok, Identity} end),
        meck:expect(olm_identity_ds, claim_one_time_key, 3, fun(_, _, _) ->
            {ok, #{<<"key_id">> => <<"otk-1">>, <<"key_base64">> => <<"A">>}}
        end),
        {ok, Payload} = olm_identity_logic:batch_claim_keys(100, 200, [<<"a">>]),
        ?assertEqual([<<"claimed">>, <<"failed">>], lists:sort(maps:keys(Payload))),
        Entry = maps:get(<<"a">>, maps:get(<<"claimed">>, Payload)),
        ?assertEqual(
            [<<"identity">>, <<"key_base64">>, <<"key_id">>, <<"type">>],
            lists:sort(maps:keys(Entry))
        )
    end).

%% ===================================================================
%% C01（E2EE 计划 run-20261003-094804）：同 DID 换钥治理 + 撤销联合门
%%
%% 换钥威胁：E2EE-013 PoP 用「本次上传的新 ed25519 公钥」验「本次上传的签名」，
%% 只证明持有新私钥——盗 token 者自生成密钥对即可通过 PoP 并静默覆盖设备
%% 身份根键（ON CONFLICT DO UPDATE），对端 TOFU 无版本变化可感知。
%% 修复契约：
%%   - 根键（ed25519）替换：signature 必须由「已注册旧 ed25519 私钥」签署
%%     （旧钥过渡证明），否则拒绝 key_rotation_requires_old_key_proof；
%%   - 子键（curve25519）轮换：同根 PoP 即授权（根钥签了新子键）；
%%   - 任何有变化的身份写：user_device.identity_version 单调 +1，并写
%%     trust_audit 事件（method=identity_rotated, 版本/代数快照）；
%%   - 撤销设备（user_device 无活跃行）的身份写入 → device_revoked；
%%   - claim_keys：目标设备不活跃 → device_revoked（cleanup 失败残留材料领不走）。
%% ===================================================================

-define(C01_UID, 100).
-define(C01_DID, <<"dev-A">>).

%% 真实 Ed25519 密钥对（pub 为 base64，与 wire 契约一致；不 mock crypto）
ed25519_keypair() ->
    Seed = crypto:strong_rand_bytes(32),
    {Pub, Priv} = crypto:generate_key(eddsa, ed25519, Seed),
    {base64:encode(Pub), Priv}.

%% 用私钥对 curve25519 公钥的 base64 字符串签名（= report_identity 的 PoP 载荷）
sign_curve(Priv, CurveB64) ->
    base64:encode(crypto:sign(eddsa, none, CurveB64, [Priv, ed25519])).

old_identity_mock(OldEdB64, OldCurveB64) ->
    meck:expect(olm_identity_ds, find_identity, 2, fun(?C01_UID, ?C01_DID) ->
        {ok, #{
            <<"device_id">> => ?C01_DID,
            <<"ed25519_key">> => OldEdB64,
            <<"curve25519_key">> => OldCurveB64
        }}
    end).

%% RED-1：盗 token 者用全新密钥对自签换根 → 必须被拒，不得静默覆盖
report_identity_rejects_root_key_swap_without_old_key_proof_test() ->
    ?WITH_MECKS([olm_identity_ds, user_device_ds, trust_audit_ds], fun() ->
        {OldEdB64, _OldPriv} = ed25519_keypair(),
        {NewEdB64, NewPriv} = ed25519_keypair(),
        NewCurveB64 = base64:encode(<<"c01-curve-new">>),
        AttackerSig = sign_curve(NewPriv, NewCurveB64),
        old_identity_mock(OldEdB64, base64:encode(<<"c01-curve-old">>)),
        meck:expect(olm_identity_ds, upsert_identity, 6, fun(_, _, _, _, _, _) ->
            {ok, 1}
        end),
        ?assertEqual(
            {error, <<"key_rotation_requires_old_key_proof">>},
            olm_identity_logic:report_identity(
                ?C01_UID, ?C01_DID, NewEdB64, NewCurveB64, AttackerSig, <<"ios">>
            )
        ),
        %% 拒绝路径不得触碰存储
        ?assertEqual(0, meck:num_calls(olm_identity_ds, upsert_identity, '_'))
    end).

%% RED-2：持旧私钥者换根（旧钥签新 curve）→ 接受 + 版本事件
report_identity_root_key_rotation_with_old_key_signature_test() ->
    ?WITH_MECKS([olm_identity_ds, user_device_ds, trust_audit_ds, elib_tsid], fun() ->
        meck:expect(elib_tsid, generate, 1, fun(trust_audit) -> 990001 end),
        {OldEdB64, OldPriv} = ed25519_keypair(),
        {NewEdB64, _NewPriv} = ed25519_keypair(),
        NewCurveB64 = base64:encode(<<"c01-curve-new">>),
        TransitionSig = sign_curve(OldPriv, NewCurveB64),
        old_identity_mock(OldEdB64, base64:encode(<<"c01-curve-old">>)),
        meck:expect(user_device_ds, bump_identity_version, 2, fun(?C01_UID, ?C01_DID) ->
            {ok, 2, 1}
        end),
        meck:expect(
            olm_identity_ds,
            upsert_identity,
            6,
            fun(?C01_UID, ?C01_DID, Ed, Cv, _Sig, _Dt) when
                Ed =:= NewEdB64, Cv =:= NewCurveB64
            ->
                {ok, 1}
            end
        ),
        meck:expect(trust_audit_ds, insert_event, 1, fun(Event) ->
            erlang:put(c01_captured_event, Event),
            {ok, inserted}
        end),
        ?assertEqual(
            ok,
            olm_identity_logic:report_identity(
                ?C01_UID, ?C01_DID, NewEdB64, NewCurveB64, TransitionSig, <<"ios">>
            )
        ),
        Event = erlang:get(c01_captured_event),
        ?assertEqual(<<"identity_rotated">>, maps:get(method, Event)),
        ?assertEqual(?C01_UID, maps:get(actor_uid, Event)),
        ?assertEqual(?C01_UID, maps:get(target_uid, Event)),
        ?assertEqual(?C01_DID, maps:get(target_device_id, Event)),
        ?assertEqual(2, maps:get(target_identity_version, Event)),
        ?assertEqual(1, maps:get(actor_device_generation, Event)),
        ?assert(is_binary(maps:get(event_id, Event))),
        ?assert(is_integer(maps:get(issued_at, Event)))
    end).

%% RED-3：子键轮换（同根换 curve）→ PoP 即授权 + bump + 版本事件
report_identity_subkey_rotation_bumps_version_and_emits_event_test() ->
    ?WITH_MECKS([olm_identity_ds, user_device_ds, trust_audit_ds, elib_tsid], fun() ->
        meck:expect(elib_tsid, generate, 1, fun(trust_audit) -> 990002 end),
        {EdB64, Priv} = ed25519_keypair(),
        OldCurveB64 = base64:encode(<<"c01-curve-old">>),
        NewCurveB64 = base64:encode(<<"c01-curve-new">>),
        Sig = sign_curve(Priv, NewCurveB64),
        old_identity_mock(EdB64, OldCurveB64),
        BumpCalls = atomics:new(1, [{signed, true}]),
        meck:expect(user_device_ds, bump_identity_version, 2, fun(?C01_UID, ?C01_DID) ->
            atomics:add(BumpCalls, 1, 1),
            {ok, 5, 2}
        end),
        meck:expect(olm_identity_ds, upsert_identity, 6, fun(_, _, _, _, _, _) ->
            {ok, 1}
        end),
        meck:expect(trust_audit_ds, insert_event, 1, fun(_Event) -> {ok, inserted} end),
        ?assertEqual(
            ok,
            olm_identity_logic:report_identity(
                ?C01_UID, ?C01_DID, EdB64, NewCurveB64, Sig, <<"ios">>
            )
        ),
        ?assertEqual(1, atomics:get(BumpCalls, 1)),
        ?assertEqual(1, meck:num_calls(trust_audit_ds, insert_event, '_'))
    end).

%% 幂等重报（键完全一致）：不 bump、不写事件（版本历史只记真实变化）
report_identity_idempotent_rereport_no_bump_no_event_test() ->
    ?WITH_MECKS([olm_identity_ds, user_device_ds, trust_audit_ds], fun() ->
        {EdB64, Priv} = ed25519_keypair(),
        CurveB64 = base64:encode(<<"c01-curve-same">>),
        Sig = sign_curve(Priv, CurveB64),
        old_identity_mock(EdB64, CurveB64),
        meck:expect(user_device_ds, bump_identity_version, 2, fun(_, _) ->
            erlang:error(bump_must_not_be_called_on_idempotent_rereport)
        end),
        meck:expect(trust_audit_ds, insert_event, 1, fun(_) ->
            erlang:error(event_must_not_be_written_on_idempotent_rereport)
        end),
        meck:expect(olm_identity_ds, upsert_identity, 6, fun(_, _, _, _, _, _) ->
            {ok, 1}
        end),
        ?assertEqual(
            ok,
            olm_identity_logic:report_identity(?C01_UID, ?C01_DID, EdB64, CurveB64, Sig, <<"ios">>)
        )
    end).

%% RED-4：撤销复活——残留 olm 行 + user_device 无活跃行（bump 0 行）→ 拒绝。
%% 用同根子键轮换路径驱动（PoP 可通过），才能到达 bump 的撤销门。
report_identity_revived_device_rejected_test() ->
    ?WITH_MECKS([olm_identity_ds, user_device_ds, trust_audit_ds], fun() ->
        {EdB64, Priv} = ed25519_keypair(),
        NewCurveB64 = base64:encode(<<"c01-curve-new">>),
        Sig = sign_curve(Priv, NewCurveB64),
        old_identity_mock(EdB64, base64:encode(<<"c01-curve-old">>)),
        meck:expect(user_device_ds, bump_identity_version, 2, fun(?C01_UID, ?C01_DID) ->
            {ok, 0}
        end),
        meck:expect(olm_identity_ds, upsert_identity, 6, fun(_, _, _, _, _, _) ->
            {ok, 1}
        end),
        ?assertEqual(
            {error, <<"device_revoked">>},
            olm_identity_logic:report_identity(
                ?C01_UID, ?C01_DID, EdB64, NewCurveB64, Sig, <<"ios">>
            )
        ),
        ?assertEqual(0, meck:num_calls(olm_identity_ds, upsert_identity, '_'))
    end).

%% RED-5：find_identity 查询失败 → fail-closed，不得放行写（防「查不到旧身份即绕过」）
report_identity_find_identity_error_fails_closed_test() ->
    ?WITH_MECKS([olm_identity_ds, user_device_ds, trust_audit_ds], fun() ->
        {EdB64, Priv} = ed25519_keypair(),
        CurveB64 = base64:encode(<<"c01-curve">>),
        Sig = sign_curve(Priv, CurveB64),
        meck:expect(olm_identity_ds, find_identity, 2, fun(_, _) -> {error, db_down} end),
        meck:expect(olm_identity_ds, upsert_identity, 6, fun(_, _, _, _, _, _) ->
            {ok, 1}
        end),
        ?assertEqual(
            {error, <<"internal_error">>},
            olm_identity_logic:report_identity(?C01_UID, ?C01_DID, EdB64, CurveB64, Sig, <<"ios">>)
        ),
        ?assertEqual(0, meck:num_calls(olm_identity_ds, upsert_identity, '_'))
    end).

%% 版本事件写入失败不阻断换钥（换钥本身已验证授权），但必须有指标痕迹
report_identity_rotation_event_failure_does_not_block_test() ->
    ?WITH_MECKS([olm_identity_ds, user_device_ds, trust_audit_ds, elib_metric, elib_tsid], fun() ->
        meck:expect(elib_tsid, generate, 1, fun(trust_audit) -> 990003 end),
        {OldEdB64, OldPriv} = ed25519_keypair(),
        {NewEdB64, _} = ed25519_keypair(),
        NewCurveB64 = base64:encode(<<"c01-curve-new">>),
        TransitionSig = sign_curve(OldPriv, NewCurveB64),
        old_identity_mock(OldEdB64, base64:encode(<<"c01-curve-old">>)),
        meck:expect(user_device_ds, bump_identity_version, 2, fun(_, _) -> {ok, 2, 1} end),
        meck:expect(olm_identity_ds, upsert_identity, 6, fun(_, _, _, _, _, _) -> {ok, 1} end),
        meck:expect(trust_audit_ds, insert_event, 1, fun(_) -> {error, audit_db_down} end),
        meck:expect(elib_metric, increment, 1, fun(_) -> ok end),
        ?assertEqual(
            ok,
            olm_identity_logic:report_identity(
                ?C01_UID, ?C01_DID, NewEdB64, NewCurveB64, TransitionSig, <<"ios">>
            )
        ),
        ?assertEqual(1, meck:num_calls(trust_audit_ds, insert_event, '_')),
        ?assertEqual(1, meck:num_calls(elib_metric, increment, '_'))
    end).

%% RED-6：claim_keys——目标设备已撤销（ds 层联合门拒绝）→ 拒绝且不消费 OTK、
%% 不落入 fallback 兜底（撤销设备的 fallback 同样被 ds 层拦截）
claim_keys_rejects_revoked_target_device_test() ->
    ?WITH_MECKS([olm_identity_ds, friend_ds], fun() ->
        meck:expect(friend_ds, is_friend, 2, fun(_, _) -> true end),
        meck:expect(olm_identity_ds, find_identity, 2, fun(_, _) ->
            {ok, #{<<"device_id">> => <<"dev-B">>, <<"ed25519_key">> => <<"e">>}}
        end),
        meck:expect(olm_identity_ds, claim_one_time_key, 3, fun(_, _, _) ->
            {error, device_revoked}
        end),
        meck:expect(olm_identity_ds, claim_fallback_key, 2, fun(_, _) ->
            erlang:error(fallback_must_not_be_tried_for_revoked_device)
        end),
        ?assertEqual(
            {error, <<"device_revoked">>},
            olm_identity_logic:claim_keys(100, 200, <<"dev-B">>)
        )
    end).

%% RED-7：claim_keys/4（幂等租约路径）同样冒泡撤销拒绝
claim_keys_with_request_id_rejects_revoked_target_device_test() ->
    ?WITH_MECKS([olm_identity_ds, friend_ds], fun() ->
        meck:expect(friend_ds, is_friend, 2, fun(_, _) -> true end),
        meck:expect(olm_identity_ds, find_identity, 2, fun(_, _) ->
            {ok, #{<<"device_id">> => <<"dev-B">>, <<"ed25519_key">> => <<"e">>}}
        end),
        meck:expect(olm_identity_ds, claim_one_time_key, 4, fun(_, _, _, _) ->
            {error, device_revoked}
        end),
        meck:expect(olm_identity_ds, claim_fallback_key, 2, fun(_, _) ->
            erlang:error(fallback_must_not_be_tried_for_revoked_device)
        end),
        ?assertEqual(
            {error, <<"device_revoked">>},
            olm_identity_logic:claim_keys(100, 200, <<"dev-B">>, <<"req-1">>)
        )
    end).

%% 撤销门同样覆盖 self-claim（多设备同步自己领自己的已撤销设备）
claim_keys_self_claim_revoked_device_rejected_test() ->
    ?WITH_MECKS([olm_identity_ds], fun() ->
        meck:expect(olm_identity_ds, find_identity, 2, fun(200, <<"dev-B">>) ->
            {ok, #{<<"device_id">> => <<"dev-B">>, <<"ed25519_key">> => <<"e">>}}
        end),
        meck:expect(olm_identity_ds, claim_one_time_key, 3, fun(_, _, _) ->
            {error, device_revoked}
        end),
        meck:expect(olm_identity_ds, claim_fallback_key, 2, fun(_, _) ->
            erlang:error(fallback_must_not_be_tried_for_revoked_device)
        end),
        ?assertEqual(
            {error, <<"device_revoked">>},
            olm_identity_logic:claim_keys(200, 200, <<"dev-B">>)
        )
    end).

%% ===================================================================
%% cleanup_consumed_one_time_keys：retention 守卫 + days→seconds 换算 + 透传
%% ===================================================================

%% retention<=0：拒绝下探 DS（防删光审计行）
cleanup_rejects_zero_retention_test() ->
    ?WITH_MECKS([olm_identity_ds], fun() ->
        meck:expect(olm_identity_ds, cleanup_consumed_one_time_keys, 1, fun(_) -> {ok, 999} end),
        ?assertEqual(
            {error, <<"invalid_retention">>},
            olm_identity_logic:cleanup_consumed_one_time_keys(0)
        ),
        ?assertEqual(0, meck:num_calls(olm_identity_ds, cleanup_consumed_one_time_keys, '_'))
    end).

cleanup_rejects_negative_retention_test() ->
    ?WITH_MECKS([olm_identity_ds], fun() ->
        meck:expect(olm_identity_ds, cleanup_consumed_one_time_keys, 1, fun(_) -> {ok, 999} end),
        ?assertEqual(
            {error, <<"invalid_retention">>},
            olm_identity_logic:cleanup_consumed_one_time_keys(-1)
        ),
        ?assertEqual(0, meck:num_calls(olm_identity_ds, cleanup_consumed_one_time_keys, '_'))
    end).

%% retention>0：换算 days*86400 传 DS（参数透传验证）+ 成功返回条数
cleanup_converts_days_to_seconds_and_passes_through_test() ->
    ?WITH_MECKS([olm_identity_ds], fun() ->
        Captured = atomics:new(1, [{signed, true}]),
        meck:expect(olm_identity_ds, cleanup_consumed_one_time_keys, 1, fun(Seconds) ->
            atomics:put(Captured, 1, Seconds),
            {ok, 3}
        end),
        ?assertEqual({ok, 3}, olm_identity_logic:cleanup_consumed_one_time_keys(7)),
        %% 7 天 = 604800 秒
        ?assertEqual(604800, atomics:get(Captured, 1))
    end).

%% DS 报错：归一为 internal_error
cleanup_maps_ds_error_test() ->
    ?WITH_MECKS([olm_identity_ds], fun() ->
        meck:expect(olm_identity_ds, cleanup_consumed_one_time_keys, 1, fun(_) ->
            {error, db_down}
        end),
        ?assertEqual(
            {error, <<"internal_error">>},
            olm_identity_logic:cleanup_consumed_one_time_keys(7)
        )
    end).
