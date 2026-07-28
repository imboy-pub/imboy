%% @doc E2EE-062：OTK claim 幂等租约的**真 PostgreSQL** 验收。
%%
%% 单测（`e2ee_otk_claim_idempotency_tests`）用状态化 ets mock 证明了
%% logic 层把 request_id 透传下去；但幂等性的**实体**是 repo 的那条 SQL
%% 与 `uk_olm_otk_claim_request` 部分唯一索引——mock 测不出：
%%   - 先查租约 / 后消费之间的 TOCTOU 是否被唯一索引兜住；
%%   - 并发同 request_id 时是否真的只消费一条；
%%   - 部分唯一索引是否放过 NULL 行（旧客户端、迁移前审计行）。
%%
%% 本文件打的是 `olm_identity_repo:claim_one_time_key/4` —— 生产调用链
%% `olm_handler:do_claim_key1 → olm_identity_logic:claim_keys/4 →
%%  olm_identity_ds:claim_one_time_key/4 → 本函数`。
-module(e2ee_otk_claim_idempotency_integration_tests).

-include_lib("eunit/include/eunit.hrl").

-define(DID, <<"dev-otk-idem-01">>).
-define(DID_B, <<"dev-otk-idem-02">>).

otk_idempotency_test_() ->
    _ = eunit_runner:eunit_setup(),
    application:set_env(imboy, env, test),
    case eunit_runner:eunit_try_db() of
        {ok, _Driver, _Conn} ->
            {foreach, fun setup/0, fun cleanup/1, [
                {"同 request_id 重放 100 次只消费一条，且恒返回同一条 key", fun replay_consumes_once/0},
                {"正向可用性：不同 request_id 各自消费一条（不得过度去重）", fun distinct_requests_consume_each/0},
                {"旧客户端（无 request_id）保持逐次消费，NULL 行不撞唯一索引", fun legacy_null_request_id/0},
                {"幂等租约按领取方隔离：换领取方不得命中他人的 claim", fun lease_scoped_to_claimant/0},
                {"并发同 request_id：50 路并发只消费一条（TOCTOU 由唯一索引兜住）",
                    fun concurrent_same_request_consumes_once/0},
                {"batch fan-out：同 request_id 跨设备不串键，重放每设备只消费一条",
                    fun batch_same_request_across_devices/0}
            ]};
        {error, _Reason} ->
            {"Database not available", fun() -> {skip, "Database not available"} end}
    end.

setup() ->
    Target = elib_tsid:generate(),
    Claimer = elib_tsid:generate(),
    Other = elib_tsid:generate(),
    Ctx = #{target => Target, claimer => Claimer, other => Other},
    persistent_term:put({?MODULE, ctx}, Ctx),
    Ctx.

cleanup(#{target := Target}) ->
    Tb = elib_pg_sql:public_tablename(<<"olm_one_time_key">>),
    _ = elib_pg:execute(<<"DELETE FROM ", Tb/binary, " WHERE user_id = $1">>, [Target]),
    persistent_term:erase({?MODULE, ctx}),
    ok.

ctx() -> persistent_term:get({?MODULE, ctx}).

%% 每个用例独立铺池，避免结果依赖执行顺序
seed(Target, N) ->
    seed(Target, ?DID, N).

seed(Target, Did, N) ->
    Keys = [
        {<<"k", (integer_to_binary(I))/binary>>, <<"b64-", (integer_to_binary(I))/binary>>}
     || I <- lists:seq(1, N)
    ],
    {ok, _} = olm_identity_repo:upsert_one_time_keys(Target, Did, Keys, 100),
    ok.

available(Target) ->
    available(Target, ?DID).

available(Target, Did) ->
    {ok, N} = olm_identity_repo:count_one_time_keys(Target, Did),
    N.

%% ===================================================================

replay_consumes_once() ->
    #{target := Target, claimer := Claimer} = ctx(),
    ok = seed(Target, 5),
    ReqId = <<"req-integration-001">>,
    Rows = [
        olm_identity_repo:claim_one_time_key(Target, ?DID, Claimer, ReqId)
     || _ <- lists:seq(1, 100)
    ],
    %% 全部成功
    ?assertEqual([], [R || R <- Rows, element(1, R) =/= ok]),
    %% 恒返回同一条 key
    KeyIds = lists:usort([maps:get(<<"key_id">>, Row) || {ok, Row} <- Rows]),
    ?assertEqual(1, length(KeyIds)),
    %% 池只少一条
    ?assertEqual(4, available(Target)).

distinct_requests_consume_each() ->
    #{target := Target, claimer := Claimer} = ctx(),
    ok = seed(Target, 5),
    {ok, R1} = olm_identity_repo:claim_one_time_key(Target, ?DID, Claimer, <<"req-a">>),
    {ok, R2} = olm_identity_repo:claim_one_time_key(Target, ?DID, Claimer, <<"req-b">>),
    ?assertNotEqual(maps:get(<<"key_id">>, R1), maps:get(<<"key_id">>, R2)),
    ?assertEqual(3, available(Target)).

legacy_null_request_id() ->
    #{target := Target, claimer := Claimer} = ctx(),
    ok = seed(Target, 5),
    %% 三次「无 request_id」的 claim：claim_request_id 全为 NULL，
    %% 部分唯一索引不得因此报冲突（WHERE claim_request_id IS NOT NULL）
    {ok, _} = olm_identity_repo:claim_one_time_key(Target, ?DID, Claimer, <<>>),
    {ok, _} = olm_identity_repo:claim_one_time_key(Target, ?DID, Claimer, <<>>),
    {ok, _} = olm_identity_repo:claim_one_time_key(Target, ?DID, Claimer),
    ?assertEqual(2, available(Target)).

lease_scoped_to_claimant() ->
    #{target := Target, claimer := Claimer, other := Other} = ctx(),
    ok = seed(Target, 5),
    ReqId = <<"req-shared-across-claimants">>,
    {ok, R1} = olm_identity_repo:claim_one_time_key(Target, ?DID, Claimer, ReqId),
    {ok, R2} = olm_identity_repo:claim_one_time_key(Target, ?DID, Other, ReqId),
    ?assertNotEqual(
        maps:get(<<"key_id">>, R1),
        maps:get(<<"key_id">>, R2),
        "同 request_id 换领取方必须各自消费，否则 request_id 成了越权读他人 key 的通道"
    ),
    ?assertEqual(3, available(Target)).

concurrent_same_request_consumes_once() ->
    #{target := Target, claimer := Claimer} = ctx(),
    ok = seed(Target, 50),
    ReqId = <<"req-concurrent-001">>,
    Parent = self(),
    Pids = [
        spawn_link(fun() ->
            R = olm_identity_repo:claim_one_time_key(Target, ?DID, Claimer, ReqId),
            Parent ! {self(), R}
        end)
     || _ <- lists:seq(1, 50)
    ],
    Results = [
        receive
            {Pid, R} -> R
        after 15000 -> {error, timeout}
        end
     || Pid <- Pids
    ],
    Oks = [Row || {ok, Row} <- Results],
    ?assertEqual(50, length(Oks), "并发 claim 不得有失败路径"),
    KeyIds = lists:usort([maps:get(<<"key_id">>, Row) || Row <- Oks]),
    ?assertEqual(
        1,
        length(KeyIds),
        "50 路并发同 request_id 必须恒返回同一条 key；>1 说明 TOCTOU 未被唯一索引兜住"
    ),
    ?assertEqual(49, available(Target)).

%% E2EE-062 第三刀的核心设计判断：batch fan-out **不按设备派生** request_id，
%% 而是把同一条原样传给每个设备——依据是部分唯一索引
%% `uk_olm_otk_claim_request (claimed_by, user_id, device_id, claim_request_id)`
%% 里已经含 device_id。此处在真 PG 上把这个判断钉死（否则只是文件级阅读结论）：
%%   - 同 request_id 的两个设备必须各拿各的 key（串键 = 用别人设备的 prekey 建会话）；
%%   - 整批重放时每设备仍只消费一条。
batch_same_request_across_devices() ->
    #{target := Target, claimer := Claimer} = ctx(),
    ok = seed(Target, ?DID, 5),
    ok = seed(Target, ?DID_B, 5),
    ReqId = <<"req-batch-fanout-001">>,
    {ok, A1} = olm_identity_repo:claim_one_time_key(Target, ?DID, Claimer, ReqId),
    {ok, B1} = olm_identity_repo:claim_one_time_key(Target, ?DID_B, Claimer, ReqId),
    ?assertNotEqual(
        {?DID, maps:get(<<"key_id">>, A1)},
        {?DID_B, maps:get(<<"key_id">>, B1)}
    ),
    ?assertEqual(4, available(Target, ?DID)),
    ?assertEqual(4, available(Target, ?DID_B)),
    %% 整批重放：每设备仍只消费一条，且恒返回同一条 key
    {ok, A2} = olm_identity_repo:claim_one_time_key(Target, ?DID, Claimer, ReqId),
    {ok, B2} = olm_identity_repo:claim_one_time_key(Target, ?DID_B, Claimer, ReqId),
    ?assertEqual(maps:get(<<"key_id">>, A1), maps:get(<<"key_id">>, A2)),
    ?assertEqual(maps:get(<<"key_id">>, B1), maps:get(<<"key_id">>, B2)),
    ?assertEqual(4, available(Target, ?DID)),
    ?assertEqual(4, available(Target, ?DID_B)).
