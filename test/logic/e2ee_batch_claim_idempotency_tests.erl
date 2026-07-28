%%%-------------------------------------------------------------------
%%% @doc E2EE-062 第三刀：**batch_claim 的幂等租约**。
%%%
%%% == 缺口 ==
%%%
%%% 第一刀给单设备 claim 加了幂等租约（`claim_keys/4` + 迁移 49 的部分唯一
%%% 索引，见 evidence/E2EE-062-otk-claim-idempotent-lease.md）。但**多设备
%%% fan-out 走的是另一条路**：`batch_claim_keys/3` 逐设备调 `claim_keys/3`
%%% ——**没有 request_id**。
%%%
%%% 后果：客户端给一个 N 设备的对端建会话时，一次网络超时后的重试会**再消费
%%% N 条 OTK**。这正是幂等租约要挡的那条路，却是多设备场景下的主路径：
%%% 单设备重试消费 1 条，batch 重试消费 N 条，抽干速度是前者的 N 倍。
%%%
%%% 21-playbook E2EE-025 验收要求「同 request id 重放 100 次只消费一次」，
%%% 未把 batch 排除在外。
%%%
%%% == 本文件守护 ==
%%%
%%% 1. batch 同 `request_id` 重放 → 每设备**只消费一条**（N 设备共 N 条，
%%%    不是 2N 条），且每次返回同一批 key。
%%% 2. 【正向可用性】不同 `request_id` → 各自正常消费（不得过度去重：
%%%    一个「永远返回第一批」的实现在幂等指标上恒得满分，必须被这条否掉）。
%%% 3. 【正向可用性】同一 `request_id` 下的**不同设备互不串键**——
%%%    每个设备必须拿到自己的 key，不能被去重成同一条。
%%% 4. 旧客户端（`batch_claim_keys/3`，无 request_id）保持逐次消费语义，
%%%    零破坏。**本条是对照组**：改前改后都必须绿。
%%% 5. 生产入口 `olm_handler` 必须把 body 里的 `request_id` 透传下去；
%%%    缺省时必须保留对 `batch_claim_keys/3` 的**原调用形状**。
%%%
%%% mock 说明：状态化 ets（非静态返回值），因此「消费了几条」是可观测的真实
%%% 状态。租约键 `{Claimer, Uid, Did, ReqId}` 逐字段对齐生产的部分唯一索引
%%% `uk_olm_otk_claim_request (claimed_by, user_id, device_id, claim_request_id)`
%%% （priv/migrations/00000049_olm_otk_claim_request.up.sql）——**device_id 本身
%%% 就在索引键里**，所以同一 request_id 在不同设备上不会互相命中。
%%% 真 PostgreSQL 的原子性/并发由
%%% `test/integration/e2ee_otk_claim_idempotency_integration_tests.erl` 闭合。
%%% @end
%%%-------------------------------------------------------------------
-module(e2ee_batch_claim_idempotency_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

-define(TARGET, 3001).
-define(DID_A, <<"dev-batch-A">>).
-define(DID_B, <<"dev-batch-B">>).
-define(CLAIMER, 7001).

batch_idempotency_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"对照组：无 request_id 的 batch 保持逐次消费语义", fun legacy_batch_consumes_each_time/0},
        {"同 request_id 重放 batch → 每设备只消费一条，返回同一批 key", fun batch_replay_consumes_once_per_device/0},
        {"正向可用性：不同 request_id 的 batch 各自正常消费", fun distinct_batch_requests_consume_each/0},
        {"正向可用性：同一 request_id 下不同设备互不串键", fun devices_do_not_collide_under_one_request/0}
    ]}.

%%%===================================================================
%%% 状态化 mock
%%%===================================================================

setup() ->
    Pool = ets:new(otk_pool, [set, public]),
    Lease = ets:new(otk_lease, [set, public]),
    meck:new(olm_identity_ds, [passthrough, no_link]),

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
    %% 旧 arity 保留原语义：不得因扩参而让既有调用方静默穿透
    meck:expect(olm_identity_ds, claim_one_time_key, fun(Uid, Did, Claimer) ->
        olm_identity_ds:claim_one_time_key(Uid, Did, Claimer, <<>>)
    end),
    meck:expect(olm_identity_ds, claim_fallback_key, fun(_Uid, _Did) ->
        {error, exhausted}
    end),
    meck:expect(olm_identity_ds, find_identity, fun(_Uid, _Did) ->
        {ok, #{<<"identity_key">> => <<"ik_test">>}}
    end),
    Tabs = {Pool, Lease},
    persistent_term:put({?MODULE, tabs}, Tabs),
    Tabs.

cleanup({Pool, Lease}) ->
    _ = (catch meck:unload(olm_identity_ds)),
    persistent_term:erase({?MODULE, tabs}),
    ets:delete(Pool),
    ets:delete(Lease),
    ok.

%%%===================================================================
%%% 用例
%%%===================================================================

%% 对照组：这条在改前改后都必须绿。红了说明 harness 坏了，不是缺口。
legacy_batch_consumes_each_time() ->
    fresh_pools(),
    Devices = [?DID_A, ?DID_B],
    {ok, _} = olm_identity_logic:batch_claim_keys(?CLAIMER, ?TARGET, Devices),
    {ok, _} = olm_identity_logic:batch_claim_keys(?CLAIMER, ?TARGET, Devices),
    ?assertEqual([<<"a3">>], remaining(?DID_A)),
    ?assertEqual([<<"b3">>], remaining(?DID_B)).

batch_replay_consumes_once_per_device() ->
    fresh_pools(),
    Devices = [?DID_A, ?DID_B],
    ReqId = <<"batch-req-001">>,
    Results = [
        olm_identity_logic:batch_claim_keys(?CLAIMER, ?TARGET, Devices, ReqId)
     || _ <- lists:seq(1, 10)
    ],
    %% 10 次重放的结果必须**逐字节相同**
    ?assertEqual(1, length(lists:usort(Results))),
    [{ok, Payload} | _] = Results,
    ?assertEqual(
        [<<"a1">>, <<"b1">>],
        lists:sort(claimed_key_ids(Payload))
    ),
    %% 每设备只少一条（2 设备共消费 2 条，而不是 20 条）
    ?assertEqual(
        [<<"a2">>, <<"a3">>],
        remaining(?DID_A),
        "batch 重放必须命中幂等租约，否则多设备 fan-out 的抽干速度是单设备的 N 倍"
    ),
    ?assertEqual([<<"b2">>, <<"b3">>], remaining(?DID_B)).

distinct_batch_requests_consume_each() ->
    fresh_pools(),
    Devices = [?DID_A, ?DID_B],
    {ok, P1} = olm_identity_logic:batch_claim_keys(?CLAIMER, ?TARGET, Devices, <<"batch-1">>),
    {ok, P2} = olm_identity_logic:batch_claim_keys(?CLAIMER, ?TARGET, Devices, <<"batch-2">>),
    ?assertEqual([<<"a1">>, <<"b1">>], lists:sort(claimed_key_ids(P1))),
    ?assertEqual(
        [<<"a2">>, <<"b2">>],
        lists:sort(claimed_key_ids(P2)),
        "不同 request_id 必须各自消费；永远返回第一批的实现在幂等指标上恒满分"
    ),
    ?assertEqual([<<"a3">>], remaining(?DID_A)),
    ?assertEqual([<<"b3">>], remaining(?DID_B)).

devices_do_not_collide_under_one_request() ->
    fresh_pools(),
    {ok, Payload} = olm_identity_logic:batch_claim_keys(
        ?CLAIMER, ?TARGET, [?DID_A, ?DID_B], <<"batch-shared">>
    ),
    Claimed = maps:get(<<"claimed">>, Payload),
    KeyA = maps:get(<<"key_id">>, maps:get(?DID_A, Claimed)),
    KeyB = maps:get(<<"key_id">>, maps:get(?DID_B, Claimed)),
    ?assertNotEqual(
        KeyA,
        KeyB,
        "同一 request_id 下每个设备必须拿到自己的 key；串键 = 用别人设备的 prekey 建会话"
    ).

%%%===================================================================
%%% 5. 生产入口：handler 必须透传 request_id
%%%===================================================================

handler_passes_request_id_test_() ->
    ?WITH_MECKS(
        [
            {imboy_policy, [{'e2ee_enabled', 0, fun() -> true end}]},
            {auth_ds, [{'current_uid', 1, fun(_State) -> ?CLAIMER end}]},
            {throttle, [{'check', 2, fun(_S, _K) -> ok end}]},
            {elib_param, [
                {'post', 1, fun(_Req) ->
                    #{
                        <<"target_uid">> => <<"3001">>,
                        <<"device_ids">> => [?DID_A, ?DID_B],
                        <<"request_id">> => <<"batch-from-body">>
                    }
                end}
            ]},
            {olm_identity_logic, [
                {'batch_claim_keys', 3, fun(_C, _T, _D) ->
                    erlang:error(must_not_drop_request_id)
                end},
                {'batch_claim_keys', 4, fun(_C, _T, _D, ReqId) ->
                    {ok, #{<<"echo">> => ReqId}}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, P) -> {responded, P} end},
                {'error', 3, fun(_Req, Msg, Code) -> {responded, error, Msg, Code} end}
            ]}
        ],
        fun() ->
            {ok, Result, _} = olm_handler:init(cowboy_req_ok, #{action => batch_claim}),
            ?assertEqual({responded, #{<<"echo">> => <<"batch-from-body">>}}, Result)
        end
    ).

%% 缺省 request_id 时必须保留对 /3 的**原调用形状**（旧客户端零破坏）
handler_without_request_id_keeps_arity3_test_() ->
    ?WITH_MECKS(
        [
            {imboy_policy, [{'e2ee_enabled', 0, fun() -> true end}]},
            {auth_ds, [{'current_uid', 1, fun(_State) -> ?CLAIMER end}]},
            {throttle, [{'check', 2, fun(_S, _K) -> ok end}]},
            {elib_param, [
                {'post', 1, fun(_Req) ->
                    #{
                        <<"target_uid">> => <<"3001">>,
                        <<"device_ids">> => [?DID_A]
                    }
                end}
            ]},
            {olm_identity_logic, [
                {'batch_claim_keys', 3, fun(_C, _T, _D) ->
                    {ok, #{<<"via">> => <<"arity3">>}}
                end},
                {'batch_claim_keys', 4, fun(_C, _T, _D, _R) ->
                    erlang:error(must_keep_legacy_call_shape)
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, P) -> {responded, P} end},
                {'error', 3, fun(_Req, Msg, Code) -> {responded, error, Msg, Code} end}
            ]}
        ],
        fun() ->
            {ok, Result, _} = olm_handler:init(cowboy_req_ok, #{action => batch_claim}),
            ?assertEqual({responded, #{<<"via">> => <<"arity3">>}}, Result)
        end
    ).

%%%===================================================================
%%% 池操作（每个用例独立重置，避免顺序依赖）
%%%===================================================================

fresh_pools() ->
    {Pool, Lease} = current_tabs(),
    ets:delete_all_objects(Pool),
    ets:delete_all_objects(Lease),
    ets:insert(Pool, {{?TARGET, ?DID_A}, [<<"a1">>, <<"a2">>, <<"a3">>]}),
    ets:insert(Pool, {{?TARGET, ?DID_B}, [<<"b1">>, <<"b2">>, <<"b3">>]}),
    ok.

remaining(Did) ->
    {Pool, _} = current_tabs(),
    case ets:lookup(Pool, {?TARGET, Did}) of
        [{_, Keys}] -> Keys;
        _ -> []
    end.

claimed_key_ids(Payload) ->
    Claimed = maps:get(<<"claimed">>, Payload),
    [maps:get(<<"key_id">>, V) || {_Did, V} <- maps:to_list(Claimed)].

current_tabs() ->
    persistent_term:get({?MODULE, tabs}).
