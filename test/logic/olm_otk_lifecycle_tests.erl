%%%-------------------------------------------------------------------
%%% @doc
%%% OTK 生命周期守护测试（S17）
%%%
%%% 验证 Olm One-Time Key 完整生命周期：
%%% upload → sequential claim (FIFO) → exhaust → fallback → replenish
%%%
%%% 使用状态化 mock（ets 表模拟 OTK 池），非静态 meck 返回值。
%%% 不依赖真实数据库。
%%% @end
%%%-------------------------------------------------------------------
-module(olm_otk_lifecycle_tests).

-include_lib("eunit/include/eunit.hrl").

-define(MAX_OTK, 100).

%%%===================================================================
%%% Test generators
%%%===================================================================

lifecycle_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"Sequential claim consumes OTK in FIFO order then falls back",
            fun sequential_claim_exhaust_then_fallback/0},
        {"No prekey available when both OTK and fallback exhausted",
            fun no_prekey_when_all_exhausted/0},
        {"Replenish replaces remaining pool (delete-available + insert)",
            fun replenish_replaces_pool/0},
        {"Fallback key is repeatable (non-destructive claim)", fun fallback_repeatable/0},
        {"Batch claim mixed: OTK / fallback / exhausted per device",
            fun batch_claim_mixed_devices/0}
    ]}.

%%%===================================================================
%%% Setup / Cleanup — 状态化 OTK 池（ets 表）
%%%===================================================================

setup() ->
    %% OTK 池：{TargetUid, DeviceId} → [KeyId]（FIFO 队列）
    Tab = ets:new(otk_pool, [set, public]),
    %% Fallback 池：{TargetUid, DeviceId} → KeyId | undefined
    FbTab = ets:new(fallback_pool, [set, public]),
    meck:new(olm_identity_ds, [passthrough, no_link]),
    %% claim_one_time_key(TargetUid, DeviceId, CurrentUid)
    meck:expect(olm_identity_ds, claim_one_time_key, fun(Uid, Did, _Claimer) ->
        case ets:lookup(Tab, {Uid, Did}) of
            [{_, [Key | Rest]}] ->
                ets:insert(Tab, {{Uid, Did}, Rest}),
                {ok, #{
                    <<"key_id">> => Key,
                    <<"key_base64">> => <<"key_", Key/binary>>
                }};
            _ ->
                {error, exhausted}
        end
    end),
    %% claim_fallback_key(TargetUid, DeviceId) — 非破坏性
    meck:expect(olm_identity_ds, claim_fallback_key, fun(Uid, Did) ->
        case ets:lookup(FbTab, {Uid, Did}) of
            [{_, KeyId}] when KeyId =/= undefined ->
                {ok, #{
                    <<"key_id">> => KeyId,
                    <<"key_base64">> => <<"fb_", KeyId/binary>>
                }};
            _ ->
                {error, exhausted}
        end
    end),
    %% find_identity(TargetUid, DeviceId) — 始终已注册
    meck:expect(olm_identity_ds, find_identity, fun(_Uid, _Did) ->
        {ok, #{<<"identity_key">> => <<"ik_test">>}}
    end),
    %% upsert_one_time_keys(Uid, Did, Keys, AuditInfo) — 全量替换
    meck:expect(olm_identity_ds, upsert_one_time_keys, fun(Uid, Did, Keys, _Audit) ->
        KeyIds = [KeyId || {KeyId, _B64} <- Keys],
        ets:insert(Tab, {{Uid, Did}, KeyIds}),
        {ok, length(KeyIds)}
    end),
    %% upsert_fallback_key(Uid, Did, KeyId, KeyB64)
    meck:expect(olm_identity_ds, upsert_fallback_key, fun(Uid, Did, KeyId, _B64) ->
        ets:insert(FbTab, {{Uid, Did}, KeyId}),
        {ok, 1}
    end),
    {Tab, FbTab}.

cleanup({Tab, FbTab}) ->
    meck:unload(olm_identity_ds),
    ets:delete(Tab),
    ets:delete(FbTab).

%%%===================================================================
%%% Tests
%%%===================================================================

%% @doc 顺序 claim：N 个 OTK 按 FIFO 消费，第 N+1 次自动转 fallback
sequential_claim_exhaust_then_fallback() ->
    Uid = 1001,
    Did = <<"dev-A">>,
    Claimer = 9001,
    %% 上传 3 个 OTK
    {ok, 3} = olm_identity_logic:report_one_time_keys(
        Uid,
        Did,
        [
            {<<"k1">>, <<"b64_1">>}, {<<"k2">>, <<"b64_2">>}, {<<"k3">>, <<"b64_3">>}
        ],
        ?MAX_OTK
    ),
    %% 上传 fallback
    ok = olm_identity_logic:report_fallback_key(Uid, Did, <<"fb-001">>, <<"fb_b64">>),

    %% Claim 1: one_time, key=k1 (FIFO)
    {ok, R1} = olm_identity_logic:claim_keys(Claimer, Uid, Did),
    ?assertEqual(<<"one_time">>, maps:get(<<"type">>, R1)),
    ?assertEqual(<<"k1">>, maps:get(<<"key_id">>, R1)),

    %% Claim 2: one_time, key=k2
    {ok, R2} = olm_identity_logic:claim_keys(Claimer, Uid, Did),
    ?assertEqual(<<"one_time">>, maps:get(<<"type">>, R2)),
    ?assertEqual(<<"k2">>, maps:get(<<"key_id">>, R2)),

    %% Claim 3: one_time, key=k3 (最后一个)
    {ok, R3} = olm_identity_logic:claim_keys(Claimer, Uid, Did),
    ?assertEqual(<<"one_time">>, maps:get(<<"type">>, R3)),
    ?assertEqual(<<"k3">>, maps:get(<<"key_id">>, R3)),

    %% Claim 4: OTK 耗尽 → 自动 fallback
    {ok, R4} = olm_identity_logic:claim_keys(Claimer, Uid, Did),
    ?assertEqual(<<"fallback">>, maps:get(<<"type">>, R4)),
    ?assertEqual(<<"fb-001">>, maps:get(<<"key_id">>, R4)).

%% @doc OTK + fallback 均耗尽 → no_prekey_available
no_prekey_when_all_exhausted() ->
    Uid = 2001,
    Did = <<"dev-B">>,
    Claimer = 9002,
    %% 上传 1 个 OTK，不上传 fallback
    {ok, 1} = olm_identity_logic:report_one_time_keys(
        Uid,
        Did,
        [
            {<<"only-key">>, <<"b64">>}
        ],
        ?MAX_OTK
    ),

    %% Claim 1: 消费唯一 OTK
    {ok, R1} = olm_identity_logic:claim_keys(Claimer, Uid, Did),
    ?assertEqual(<<"one_time">>, maps:get(<<"type">>, R1)),

    %% Claim 2: OTK 耗尽 + 无 fallback → no_prekey_available
    Result = olm_identity_logic:claim_keys(Claimer, Uid, Did),
    ?assertEqual({error, <<"no_prekey_available">>}, Result).

%% @doc 补充（re-upload）替换剩余池：旧剩余被删除，仅新 key 可用
replenish_replaces_pool() ->
    Uid = 3001,
    Did = <<"dev-C">>,
    Claimer = 9003,
    %% 上传 5 个 OTK
    {ok, 5} = olm_identity_logic:report_one_time_keys(
        Uid,
        Did,
        [
            {<<"a1">>, <<"b1">>},
            {<<"a2">>, <<"b2">>},
            {<<"a3">>, <<"b3">>},
            {<<"a4">>, <<"b4">>},
            {<<"a5">>, <<"b5">>}
        ],
        ?MAX_OTK
    ),
    %% 消费 2 个
    {ok, _} = olm_identity_logic:claim_keys(Claimer, Uid, Did),
    {ok, _} = olm_identity_logic:claim_keys(Claimer, Uid, Did),

    %% 补充：上传 2 个新 key（全量替换：旧 a3/a4/a5 被删除）
    {ok, 2} = olm_identity_logic:report_one_time_keys(
        Uid,
        Did,
        [
            {<<"n1">>, <<"nb1">>}, {<<"n2">>, <<"nb2">>}
        ],
        ?MAX_OTK
    ),

    %% 下一次 claim 应该是 n1（不是 a3）
    {ok, R} = olm_identity_logic:claim_keys(Claimer, Uid, Did),
    ?assertEqual(<<"one_time">>, maps:get(<<"type">>, R)),
    ?assertEqual(<<"n1">>, maps:get(<<"key_id">>, R)),

    %% 再 claim → n2
    {ok, R2} = olm_identity_logic:claim_keys(Claimer, Uid, Did),
    ?assertEqual(<<"n2">>, maps:get(<<"key_id">>, R2)).

%% @doc Fallback key 可重复 claim（非破坏性）
fallback_repeatable() ->
    Uid = 4001,
    Did = <<"dev-D">>,
    Claimer = 9004,
    %% 不上传 OTK，只上传 fallback
    ok = olm_identity_logic:report_fallback_key(Uid, Did, <<"fb-repeat">>, <<"b64">>),

    %% 连续 claim 3 次，每次都得到同一个 fallback
    {ok, R1} = olm_identity_logic:claim_keys(Claimer, Uid, Did),
    {ok, R2} = olm_identity_logic:claim_keys(Claimer, Uid, Did),
    {ok, R3} = olm_identity_logic:claim_keys(Claimer, Uid, Did),
    ?assertEqual(<<"fallback">>, maps:get(<<"type">>, R1)),
    ?assertEqual(<<"fallback">>, maps:get(<<"type">>, R2)),
    ?assertEqual(<<"fallback">>, maps:get(<<"type">>, R3)),
    ?assertEqual(maps:get(<<"key_id">>, R1), maps:get(<<"key_id">>, R2)),
    ?assertEqual(maps:get(<<"key_id">>, R2), maps:get(<<"key_id">>, R3)).

%% @doc batch_claim 混合场景：设备 A 有 OTK，B 仅 fallback，C 全耗尽
batch_claim_mixed_devices() ->
    TargetUid = 5001,
    Claimer = 9005,
    %% 设备 A：有 OTK + fallback
    {ok, _} = olm_identity_logic:report_one_time_keys(
        TargetUid,
        <<"dev-A">>,
        [
            {<<"ak1">>, <<"ab1">>}
        ],
        ?MAX_OTK
    ),
    ok = olm_identity_logic:report_fallback_key(TargetUid, <<"dev-A">>, <<"fb-A">>, <<"b64">>),
    %% 设备 B：仅 fallback（无 OTK）
    ok = olm_identity_logic:report_fallback_key(TargetUid, <<"dev-B">>, <<"fb-B">>, <<"b64">>),
    %% 设备 C：全耗尽（find_identity 返回已注册但无 key）

    %% batch_claim 三个设备
    {ok, Result} = olm_identity_logic:batch_claim_keys(
        Claimer, TargetUid, [<<"dev-A">>, <<"dev-B">>, <<"dev-C">>]
    ),

    Claimed = maps:get(<<"claimed">>, Result),
    Failed = maps:get(<<"failed">>, Result),

    %% A: one_time
    ?assert(maps:is_key(<<"dev-A">>, Claimed)),
    ?assertEqual(<<"one_time">>, maps:get(<<"type">>, maps:get(<<"dev-A">>, Claimed))),

    %% B: fallback
    ?assert(maps:is_key(<<"dev-B">>, Claimed)),
    ?assertEqual(<<"fallback">>, maps:get(<<"type">>, maps:get(<<"dev-B">>, Claimed))),

    %% C: failed with no_prekey_available
    ?assert(maps:is_key(<<"dev-C">>, Failed)),
    ?assertEqual(<<"no_prekey_available">>, maps:get(<<"dev-C">>, Failed)).
