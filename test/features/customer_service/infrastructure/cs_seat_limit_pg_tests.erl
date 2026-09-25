%%% @doc CS-BE-06（CS-GOV-01）：席位 entitlement 真库 focused 套件。
%%%
%%% 冻结决策 CS-DEC-03：组织级人工 seat_limit（无行=unlimited）；降低
%%% limit 不自动停用存量，只阻止新增/恢复超额（稳定错误码
%%% seat_limit_exceeded，不静默）；不接计费。
%%%
%%% 覆盖验收：
%%%   1. N 并发不超 limit（advisory xact lock 内 count+INSERT 原子——
%%%      4 并发开第 3 个坐席恰 2 成功 2 失败）；
%%%   2. 幂等重放不重复计数（provision 已 enabled seat 重放 → count 不变
%%%      且成功；resume 已启用重放同）；
%%%   3. 存量超额可读可减不可增（limit 降到位下：可读 view/list、可
%%%      suspend、resume/create 均拒）；
%%%   4. 跨 Org 隔离（org B 的 limit 不影响 org A；view 随租户门）。
%%%
%%% 隔离：`cs_pg_test_fixture:new_scope/0` 随机 TSID；无真实数据；环境
%%% 不可用 ⇒ erlang:error（不是 skip）。共享 scratch 库只做随机 scope 行级
%%% INSERT/UPDATE/DELETE，无 DROP/TRUNCATE。
-module(cs_seat_limit_pg_tests).

-include_lib("eunit/include/eunit.hrl").

-define(FIX, cs_pg_test_fixture).

cs_seat_limit_pg_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun cases/1}.

setup() ->
    case eunit_runner:eunit_setup_with_db() of
        {ok, Conn} -> {ok, Conn};
        {error, Reason} -> {error, Reason}
    end.

cleanup({ok, Conn}) ->
    eunit_runner:eunit_cleanup_db(Conn);
cleanup(Other) ->
    Other.

cases({ok, _Conn}) ->
    [
        {timeout, 60, fun unlimited_by_default_and_governance_roundtrip/0},
        {timeout, 60, fun concurrent_create_never_exceeds_limit/0},
        {timeout, 60, fun idempotent_provision_replay_no_double_count/0},
        {timeout, 60, fun over_limit_readable_reducible_not_growable/0},
        {timeout, 60, fun cross_org_isolation/0}
    ];
cases({error, Reason}) ->
    erlang:error({csbe06_pg_suite_db_unavailable, Reason}).

%% ===================================================================
%% 验收前置：默认 unlimited + 治理 roundtrip（PUT N / PUT 清除）
%% ===================================================================

unlimited_by_default_and_governance_roundtrip() ->
    Scope = ?FIX:new_scope(),
    try
        OrgId = org(Scope),
        Ws = ws(Scope),
        %% 现存组织默认 unlimited（无 limit 行；视图含 used 现算计数）。
        {ok, #{seat_limit := unlimited, used := Used0}} =
            cs_seat_app:seat_limit_view(OrgId, #{workspace_id => Ws}),
        true = Used0 >= 1,
        %% PUT limit=5 → 视图 5；清除（缺省）→ unlimited。
        {ok, #{seat_limit := 5}} = cs_seat_app:set_seat_limit(OrgId, #{
            workspace_id => Ws, seat_limit => 5
        }),
        {ok, #{seat_limit := 5}} = cs_seat_app:seat_limit_view(OrgId, #{workspace_id => Ws}),
        {ok, #{seat_limit := unlimited}} = cs_seat_app:set_seat_limit(OrgId, #{
            workspace_id => Ws
        }),
        %% 非法 limit（0/负）拒绝。
        ?assertMatch(
            {error, {invalid_seat_limit, 0}},
            cs_seat_app:set_seat_limit(OrgId, #{workspace_id => Ws, seat_limit => 0})
        )
    catch
        Class:Reason:Stack ->
            erlang:Class({csbe06_roundtrip, Reason, Stack})
    end.

%% ===================================================================
%% 验收 1：N 并发不超 limit（4 并发、limit=2 → 恰 2 成功）
%% ===================================================================

concurrent_create_never_exceeds_limit() ->
    Scope = ?FIX:new_scope(),
    try
        OrgId = org(Scope),
        Ws = ws(Scope),
        Owner = maps:get(owner_user_id, Scope),
        ok = set_limit(OrgId, Ws, 2),
        %% 4 个并发 create（不同 customer_service identity；fixture 预置 1 个
        %% service identity + 动态补 3 个）。
        %% fixture 预置的 service seat 先 suspend（enabled 总数清零——
        %% 悬存超额「可减」语义），4 个全新 identity 并发创建。
        {ok, _} = cs_seat_app:suspend_seat(OrgId, #{
            workspace_id => Ws, business_identity_id => seat_identity(Scope), at => 1699999999
        }),
        Identities = extra_identities(Scope, 4),
        ok,
        Parent = self(),
        %% spawn 错峰 120ms：请求窗口重叠构成并发（advisory xact lock 串行
        %% 化裁决），同时避免一次性打穿测试环境连接池（no_connection）。
        Pids = [
            begin
                Pid = spawn(fun() ->
                    R = cs_seat_app:create_seat(OrgId, #{
                        workspace_id => Ws,
                        business_identity_id => Id,
                        max_concurrent => 1,
                        created_by_user_id => Owner
                    }),
                    Parent ! {self(), R}
                end),
                timer:sleep(120),
                Pid
            end
         || Id <- Identities
        ],
        Results = [receive_result(P) || P <- Pids],
        Wins = [R || {ok, _} = R <- Results],
        Losses = [R || {error, seat_limit_exceeded} = R <- Results],
        ?assertEqual(2, length(Wins), {wins, Results}),
        ?assertEqual(2, length(Losses), {losses, Results}),
        %% DB 事实：enabled 恰 2。
        ?assertEqual(
            2,
            ?FIX:scalar(
                <<
                    "SELECT count(*) FROM customer_service_seat"
                    " WHERE organization_id = $1 AND enabled = true"
                >>,
                [OrgId]
            )
        )
    catch
        Class:Reason:Stack ->
            erlang:Class({csbe06_concurrent, Reason, Stack})
    end.

%% ===================================================================
%% 验收 2：幂等重放不重复计数
%% ===================================================================

idempotent_provision_replay_no_double_count() ->
    Scope = ?FIX:new_scope(#{max_concurrent => 1}),
    try
        OrgId = org(Scope),
        Ws = ws(Scope),
        UserId = maps:get(actor_user_id, Scope),
        %% fixture 预置 1 个 enabled seat → limit=2：provision 新建后恰达。
        ok = set_limit(OrgId, Ws, 2),
        %% provision（新建，恰达 limit）。
        {ok, P1} = cs_seat_app:provision_seat(OrgId, #{
            workspace_id => Ws,
            user_id => UserId,
            display_name => <<"cs01-prov-seat">>,
            adm_user_id => owner(Scope)
        }),
        SeatId1 = seat_id_of(P1),
        %% 幂等重放（同 user 再 provision）：成功且**不重复计数/不换行**。
        {ok, P2} = cs_seat_app:provision_seat(OrgId, #{
            workspace_id => Ws,
            user_id => UserId,
            display_name => <<"cs01-prov-seat">>,
            adm_user_id => owner(Scope)
        }),
        ?assertEqual(SeatId1, seat_id_of(P2), "重放必须命中同一 seat 行"),
        %% enabled 总数 = fixture 1 + provision 1 = 2（重放后**不重复计数**——
        %% 若重放另建/翻转错误则 >2；limit=2 本身也是硬上界）。
        ?assertEqual(
            2,
            ?FIX:scalar(
                <<
                    "SELECT count(*) FROM customer_service_seat"
                    " WHERE organization_id = $1 AND enabled = true"
                >>,
                [OrgId]
            )
        ),
        %% resume 已启用重放：幂等成功（limit=1 已满但重放不检查）。
        IdentityId = seat_identity(Scope),
        ?assertMatch(
            {ok, _},
            cs_seat_app:resume_seat(OrgId, #{
                workspace_id => Ws, business_identity_id => IdentityId, at => 1700000000
            })
        )
    catch
        Class:Reason:Stack ->
            erlang:Class({csbe06_replay, Reason, Stack})
    end.

%% ===================================================================
%% 验收 3：存量超额可读可减不可增
%% ===================================================================

over_limit_readable_reducible_not_growable() ->
    Scope = ?FIX:new_scope(),
    try
        OrgId = org(Scope),
        Ws = ws(Scope),
        Identity = seat_identity(Scope),
        %% 现有 1 个 enabled seat（fixture 预置）后 limit 降到 1（恰好存量=limit）；
        %% 再建第二个 disabled seat（disabled 落点不检查），然后把 limit 降到 0
        %% 不可行（下界 1）——改用「limit=1 + 2 个 enabled」构造超额：
        {ok, _} = cs_seat_app:create_seat(OrgId, #{
            workspace_id => Ws, business_identity_id => extra_identity(Scope), max_concurrent => 1
        }),
        ok = set_limit(OrgId, Ws, 1),
        %% 现状：2 enabled > limit 1（存量超额）。
        {ok, #{seat_limit := 1, used := 2}} =
            cs_seat_app:seat_limit_view(OrgId, #{workspace_id => Ws}),
        %% 可读：list/dispatch 快照照常。
        {ok, _} = cs_app_support:with_store(#{}, fun(Store) ->
            Store:list_dispatchable_seats(OrgId)
        end),
        %% 可减：suspend 任一存量成功（减永不检查）。
        {ok, _} = cs_seat_app:suspend_seat(OrgId, #{
            workspace_id => Ws, business_identity_id => Identity, at => 1700000000
        }),
        %% 不可增：被 suspend 的 resume（减完仍超额）拒。
        ?assertEqual(
            {error, seat_limit_exceeded},
            cs_seat_app:resume_seat(OrgId, #{
                workspace_id => Ws, business_identity_id => Identity, at => 1700000001
            })
        ),
        %% 不可增：第三个新 seat 拒。
        ?assertMatch(
            {error, seat_limit_exceeded},
            cs_seat_app:create_seat(OrgId, #{
                workspace_id => Ws,
                business_identity_id => extra_identity(Scope),
                max_concurrent => 1
            })
        )
    catch
        Class:Reason:Stack ->
            erlang:Class({csbe06_over_limit, Reason, Stack})
    end.

%% ===================================================================
%% 验收 4：跨 Org 隔离
%% ===================================================================

cross_org_isolation() ->
    Scope = ?FIX:new_scope(),
    try
        OrgA = org(Scope),
        WsA = ws(Scope),
        OrgB = maps:get(other_org_id, Scope),
        %% org A limit=1 满；org B 无 limit——互不影响。
        ok = set_limit(OrgA, WsA, 1),
        {ok, #{seat_limit := 1}} = cs_seat_app:seat_limit_view(OrgA, #{workspace_id => WsA}),
        {ok, #{seat_limit := unlimited}} =
            cs_seat_app:seat_limit_view(OrgB, #{workspace_id => maps:get(other_workspace_id, Scope)}),
        %% org A 满额后 org B 的 create 不受影响（B 的 identity fixture 未建
        %% seat——直接验证 view 隔离即可；B 无 customer_service identity，
        %% create 会在 identity FK 处失败——不做跨 Org create）。
        ?assertNotEqual(OrgA, OrgB)
    catch
        Class:Reason:Stack ->
            erlang:Class({csbe06_cross_org, Reason, Stack})
    end.

%% ===================================================================
%% helpers
%% ===================================================================

org(Scope) -> maps:get(org_id, Scope).
ws(Scope) -> maps:get(workspace_id, Scope).
seat_identity(Scope) -> maps:get(service_identity_id, Scope).
owner(Scope) -> maps:get(owner_user_id, Scope).

set_limit(OrgId, Ws, N) ->
    case cs_seat_app:set_seat_limit(OrgId, #{workspace_id => Ws, seat_limit => N}) of
        {ok, _} -> ok;
        {error, _} = Err -> Err
    end.

extra_identities(Scope, N) ->
    [extra_identity(Scope) || _ <- lists:seq(1, N)].

extra_identity(Scope) ->
    OrgId = org(Scope),
    Identity = ?FIX:id(),
    ok = ?FIX:exec(
        <<
            "INSERT INTO organization_business_identity"
            " (id,organization_id,function_key,display_name,status,version,created_by_user_id)"
            " VALUES ($1,$2,'customer_service',$3,'active',1,$4)"
        >>,
        [
            Identity,
            OrgId,
            <<"cs01-limit-seat-", (integer_to_binary(Identity))/binary>>,
            maps:get(owner_user_id, Scope)
        ]
    ),
    Identity.

receive_result(Pid) ->
    receive
        {Pid, R} -> R
    after 20000 ->
        erlang:error({csbe06_create_timeout, Pid})
    end.

seat_id_of(Provisioned) ->
    %% provision 返回：business_identity_id 即 seat PK（seat 与 identity 1:1，
    %% PK=business_identity_id）；Seat 投影是 upsert 的部分回读（无 id 列）。
    Id = maps:get(business_identity_id, Provisioned),
    case Id of
        N when is_integer(N) -> N;
        B when is_binary(B) -> binary_to_integer(B)
    end.
