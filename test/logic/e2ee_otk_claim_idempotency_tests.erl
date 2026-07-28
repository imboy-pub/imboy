%%%-------------------------------------------------------------------
%%% @doc E2EE-062：OTK claim 的**幂等租约**与目标级抗耗尽。
%%%
%%% == 缺口 ==
%%%
%%% X3DH 的 one-time prekey 是一次性资源：每次 claim 消费池里一条。
%%% 现状下 claim 没有任何幂等键——客户端一次网络超时后的重试、或 app 重启
%%% 后的重发，都会**再消费一条 OTK**。正常使用即可把对端 OTK 池打空；
%%% 恶意方只要重放同一个请求就能定向耗尽某个用户的池，把所有新会话逼到
%%% 复用同一条 fallback prekey（前向保密显著下降）。
%%%
%%% 21-playbook E2EE-025 验收要求：
%%%   「同 request id 重放 100 次只消费一次；不同攻击 request 触发目标级限流」。
%%%
%%% == 本文件守护 ==
%%%
%%% 1. 同 `request_id` 重放 N 次 → **只消费一条 OTK**，且每次返回同一条 key。
%%% 2. 【正向可用性】不同 `request_id` → **各自消费一条**（不得过度去重：
%%%    一个「永远返回同一条 key」的实现在幂等指标上恒得满分，必须被这条否掉）。
%%% 3. 无 `request_id`（旧客户端）→ 保持既有逐次消费语义，零破坏。
%%% 4. 幂等命中不得越权：request_id 只在**同一领取方**内生效，
%%%    换一个领取方拿同样的 request_id 不得读到别人的 claim 结果。
%%%
%%% 采用与 `olm_otk_lifecycle_tests` 同款的**状态化 ets mock**（非静态返回值），
%%% 因此「消费了几条」是可观测的真实状态，而不是调用次数的间接推断。
%%% 真 PostgreSQL 的原子性/并发由
%%% `test/integration/olm_otk_claim_idempotency_integration_tests.erl` 闭合。
%%% @end
%%%-------------------------------------------------------------------
-module(e2ee_otk_claim_idempotency_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TARGET, 2001).
-define(DID, <<"dev-target-A">>).
-define(CLAIMER, 9001).
-define(OTHER_CLAIMER, 9002).

idempotency_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"同 request_id 重放 100 次只消费一条 OTK，且每次返回同一条 key", fun replay_consumes_once/0},
        {"正向可用性：不同 request_id 各自消费一条（不得过度去重）", fun distinct_requests_consume_each/0},
        {"旧客户端（无 request_id）保持逐次消费语义", fun legacy_no_request_id/0},
        {"幂等租约按领取方隔离：换领取方不得命中别人的 claim", fun lease_is_scoped_to_claimant/0},
        {"OTK 耗尽后同 request_id 重放仍幂等（fallback 非破坏性）", fun replay_after_exhaustion/0}
    ]}.

%%%===================================================================
%%% 状态化 mock：ets 既存池，也存已发放的租约
%%%===================================================================

setup() ->
    Pool = ets:new(otk_pool, [set, public]),
    Lease = ets:new(otk_lease, [set, public]),
    Fb = ets:new(fallback_pool, [set, public]),
    meck:new(olm_identity_ds, [passthrough, no_link]),

    %% claim_one_time_key/4：带 request_id 的幂等租约语义。
    %% mock 忠实复刻**服务端应有的行为**：
    %%   - RequestId 非空且该 (Claimer, Uid, Did, RequestId) 已发放过 → 原样返回旧 key，池不变；
    %%   - 否则消费一条并登记租约。
    %% 生产实现在 olm_identity_repo:claim_one_time_key/4（单条 SQL + 部分唯一索引）。
    meck:expect(olm_identity_ds, claim_one_time_key, fun(Uid, Did, Claimer, ReqId) ->
        LeaseKey = {Claimer, Uid, Did, ReqId},
        case ReqId =/= <<>> andalso ets:lookup(Lease, LeaseKey) of
            [{_, Row}] ->
                {ok, Row};
            _ ->
                case ets:lookup(Pool, {Uid, Did}) of
                    [{_, [Key | Rest]}] ->
                        ets:insert(Pool, {{Uid, Did}, Rest}),
                        Row = #{
                            <<"key_id">> => Key,
                            <<"key_base64">> => <<"key_", Key/binary>>
                        },
                        case ReqId of
                            <<>> -> ok;
                            _ -> ets:insert(Lease, {LeaseKey, Row})
                        end,
                        {ok, Row};
                    _ ->
                        {error, exhausted}
                end
        end
    end),
    %% 旧 arity 保留：不得因扩参而让既有调用方静默穿透（A2-a 实证过的坑）
    meck:expect(olm_identity_ds, claim_one_time_key, fun(Uid, Did, Claimer) ->
        olm_identity_ds:claim_one_time_key(Uid, Did, Claimer, <<>>)
    end),
    meck:expect(olm_identity_ds, claim_fallback_key, fun(Uid, Did) ->
        case ets:lookup(Fb, {Uid, Did}) of
            [{_, KeyId}] when KeyId =/= undefined ->
                {ok, #{
                    <<"key_id">> => KeyId,
                    <<"key_base64">> => <<"fb_", KeyId/binary>>
                }};
            _ ->
                {error, exhausted}
        end
    end),
    meck:expect(olm_identity_ds, find_identity, fun(_Uid, _Did) ->
        {ok, #{<<"identity_key">> => <<"ik_test">>}}
    end),
    Tabs = {Pool, Lease, Fb},
    persistent_term:put({?MODULE, tabs}, Tabs),
    Tabs.

cleanup({Pool, Lease, Fb}) ->
    _ = (catch meck:unload(olm_identity_ds)),
    persistent_term:erase({?MODULE, tabs}),
    ets:delete(Pool),
    ets:delete(Lease),
    ets:delete(Fb),
    ok.

%%%===================================================================
%%% 用例
%%%===================================================================

replay_consumes_once() ->
    Tabs = fresh_pool([<<"k1">>, <<"k2">>, <<"k3">>], undefined),
    ReqId = <<"req-abc-001">>,
    Results = [
        olm_identity_logic:claim_keys(?CLAIMER, ?TARGET, ?DID, ReqId)
     || _ <- lists:seq(1, 100)
    ],
    %% 每次都成功且返回同一条 key
    KeyIds = lists:usort([maps:get(<<"key_id">>, P) || {ok, P} <- Results]),
    ?assertEqual(100, length(Results)),
    ?assertEqual([<<"k1">>], KeyIds),
    %% 池里只少了一条
    ?assertEqual([<<"k2">>, <<"k3">>], remaining(Tabs)).

distinct_requests_consume_each() ->
    Tabs = fresh_pool([<<"k1">>, <<"k2">>, <<"k3">>], undefined),
    {ok, P1} = olm_identity_logic:claim_keys(?CLAIMER, ?TARGET, ?DID, <<"req-1">>),
    {ok, P2} = olm_identity_logic:claim_keys(?CLAIMER, ?TARGET, ?DID, <<"req-2">>),
    ?assertNotEqual(maps:get(<<"key_id">>, P1), maps:get(<<"key_id">>, P2)),
    ?assertEqual([<<"k3">>], remaining(Tabs)).

legacy_no_request_id() ->
    Tabs = fresh_pool([<<"k1">>, <<"k2">>, <<"k3">>], undefined),
    {ok, _} = olm_identity_logic:claim_keys(?CLAIMER, ?TARGET, ?DID),
    {ok, _} = olm_identity_logic:claim_keys(?CLAIMER, ?TARGET, ?DID),
    ?assertEqual([<<"k3">>], remaining(Tabs)).

lease_is_scoped_to_claimant() ->
    Tabs = fresh_pool([<<"k1">>, <<"k2">>, <<"k3">>], undefined),
    ReqId = <<"req-shared">>,
    {ok, P1} = olm_identity_logic:claim_keys(?CLAIMER, ?TARGET, ?DID, ReqId),
    {ok, P2} = olm_identity_logic:claim_keys(?OTHER_CLAIMER, ?TARGET, ?DID, ReqId),
    ?assertNotEqual(maps:get(<<"key_id">>, P1), maps:get(<<"key_id">>, P2)),
    ?assertEqual([<<"k3">>], remaining(Tabs)).

replay_after_exhaustion() ->
    Tabs = fresh_pool([], <<"fb1">>),
    ReqId = <<"req-exhausted">>,
    R1 = olm_identity_logic:claim_keys(?CLAIMER, ?TARGET, ?DID, ReqId),
    R2 = olm_identity_logic:claim_keys(?CLAIMER, ?TARGET, ?DID, ReqId),
    %% 耗尽后走 fallback（非破坏性，重复领取同一条是协议允许的）
    ?assertMatch({ok, #{<<"type">> := <<"fallback">>}}, R1),
    ?assertEqual(R1, R2),
    ?assertEqual([], remaining(Tabs)).

%%%===================================================================
%%% 池操作（每个用例独立重置，避免顺序依赖）
%%%===================================================================

fresh_pool(Keys, FallbackKeyId) ->
    Tabs = {Pool, Lease, Fb} = current_tabs(),
    ets:delete_all_objects(Pool),
    ets:delete_all_objects(Lease),
    ets:delete_all_objects(Fb),
    ets:insert(Pool, {{?TARGET, ?DID}, Keys}),
    case FallbackKeyId of
        undefined -> ok;
        _ -> ets:insert(Fb, {{?TARGET, ?DID}, FallbackKeyId})
    end,
    Tabs.

remaining({Pool, _, _}) ->
    case ets:lookup(Pool, {?TARGET, ?DID}) of
        [{_, Keys}] -> Keys;
        _ -> []
    end.

current_tabs() ->
    persistent_term:get({?MODULE, tabs}).
