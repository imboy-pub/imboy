%%% @doc CS-BE-05（seat presence / 派单）真库 focused 套件。
%%%
%%% 冻结决策 CS-DEC-02：运行态（online/away/busy/offline）是派生值——
%%% 心跳 TTL（90s）+ 手动 away 优先 + 容量满 busy；cs_seat 无新增治理列；
%%% 心跳是持久/共享可见 lease（多节点读同一 DB 事实）。
%%%
%%% 覆盖（CS-RUNTIME-02 验收）：
%%%   1. 可注入时钟覆盖 TTL 边界（89s online / 90s online / 91s offline；
%%%      无 presence 行 = 从未上报 = offline）；
%%%   2. away/offline 不派单（select_seat 跳过；全部不在线 →
%%%      no_seat_available → 会话保持 queued）；
%%%   3. busy 仍可查看已分配会话（presence 视图含 active_count 事实）但
%%%      不接新会话（派生 busy + 容量条件双保险）；
%%%   4. 多节点并发无双派（两个进程同时 claim 同一 queued session，
%%%      DB CAS 恰好一个成功）。
%%%
%%% 隔离：`cs_pg_test_fixture:new_scope/0` 随机 TSID scope；无真实数据；
%%% 环境不可用 ⇒ `erlang:error/1`（不是 skip）。与共享 scratch DB 上只做
%%% 随机 scope 的 INSERT/UPDATE/DELETE，无 DROP/TRUNCATE。
-module(cs_presence_dispatch_pg_tests).

-include_lib("eunit/include/eunit.hrl").

-define(FIX, cs_pg_test_fixture).

cs_presence_dispatch_pg_test_() ->
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
        {timeout, 60, fun ttl_boundary_with_injected_clock/0},
        {timeout, 60, fun manual_away_priority_and_clear/0},
        {timeout, 60, fun suspend_rejects_heartbeat_immediately/0},
        {timeout, 60, fun busy_derived_and_capacity_double_guard/0},
        {timeout, 60, fun dispatch_skips_away_offline_keeps_queued/0},
        {timeout, 60, fun concurrent_claims_exactly_one_wins/0},
        {timeout, 60, fun dispatch_without_presence_key_is_backward_compatible/0}
    ];
cases({error, Reason}) ->
    erlang:error({csbe05_pg_suite_db_unavailable, Reason}).

%% ===================================================================
%% 验收 1：可注入时钟覆盖 TTL 边界（90s 无 heartbeat → offline）
%% ===================================================================

ttl_boundary_with_injected_clock() ->
    Scope = ?FIX:new_scope(),
    try
        Identity = seat_identity(Scope),
        T = 1700000000,
        %% 心跳 at=T（服务端时钟注入；客户端不可报时——HTTP 面 at 必在）。
        {ok, Hb} = cs_seat_app:seat_heartbeat(org(Scope), #{
            workspace_id => ws(Scope), business_identity_id => Identity, at => T
        }),
        ?assertEqual(online, maps:get(status, Hb)),
        %% T+89 / T+90：新鲜（TTL 闭区间）→ online。
        ?assertEqual(online, derived_status_at(Scope, Identity, T + 89)),
        ?assertEqual(online, derived_status_at(Scope, Identity, T + 90)),
        %% T+91：越界 → offline。
        ?assertEqual(offline, derived_status_at(Scope, Identity, T + 91)),
        %% DB 行事实：heartbeat 值 = 注入 at（写值不被服务器时钟污染）。
        ?assertEqual(T, stored_heartbeat_at(Scope, Identity))
    catch
        Class:Reason:Stack ->
            erlang:Class({csbe05_ttl_boundary, Reason, Stack})
    end.

%% 无 presence 行 = 从未上报心跳 = offline（从严：不上报不参与自动派单）。
%% 用另一个新 seat（未心跳）验证。
%% （合并进 dispatch_skips_away_offline_keeps_queued：B seat 无行 → offline。）

%% ===================================================================
%% 验收 2a：手动 away 优先于自动派生；clear 后回到自动
%% ===================================================================

manual_away_priority_and_clear() ->
    Scope = ?FIX:new_scope(),
    try
        Identity = seat_identity(Scope),
        T = 1700000000,
        {ok, Ok0} = cs_seat_app:seat_heartbeat(org(Scope), #{
            workspace_id => ws(Scope), business_identity_id => Identity, at => T
        }),
        ?assertEqual(online, maps:get(status, Ok0)),
        %% 手动 away：心跳新鲜也不得 online（手动 away 优先）。
        {ok, Away} = cs_seat_app:set_seat_manual_status(org(Scope), #{
            workspace_id => ws(Scope),
            business_identity_id => Identity,
            manual_status => <<"away">>,
            at => T + 10
        }),
        ?assertEqual(away, maps:get(status, Away)),
        ?assertEqual(away, derived_status_at(Scope, Identity, T + 11)),
        %% 周期心跳不冲掉手动 away（heartbeat 只刷 last_heartbeat_at）。
        {ok, Hb2} = cs_seat_app:seat_heartbeat(org(Scope), #{
            workspace_id => ws(Scope), business_identity_id => Identity, at => T + 12
        }),
        ?assertEqual(away, maps:get(status, Hb2)),
        %% clear（manual_status 缺省）→ 回到自动派生 online。
        {ok, Cleared} = cs_seat_app:set_seat_manual_status(org(Scope), #{
            workspace_id => ws(Scope), business_identity_id => Identity, at => T + 13
        }),
        ?assertEqual(online, maps:get(status, Cleared)),
        %% DB 行事实：manual_status 已清空。
        ?assertEqual(
            undefined,
            stored_manual_status(Scope, Identity),
            "clear 后 manual_status 必须为 NULL"
        )
    catch
        Class:Reason:Stack ->
            erlang:Class({csbe05_manual_away, Reason, Stack})
    end.

%% ===================================================================
%% 验收 2b：suspend 立即拒绝心跳（seat_disabled）
%% ===================================================================

suspend_rejects_heartbeat_immediately() ->
    Scope = ?FIX:new_scope(),
    try
        Identity = seat_identity(Scope),
        T = 1700000000,
        {ok, _} = cs_seat_app:seat_heartbeat(org(Scope), #{
            workspace_id => ws(Scope), business_identity_id => Identity, at => T
        }),
        %% suspend → 心跳立即 seat_disabled（离线坐席不产生"enabled=false
        %% 却派生 online"的矛盾状态）。
        {ok, _} = cs_seat_app:suspend_seat(org(Scope), #{
            workspace_id => ws(Scope), business_identity_id => Identity, at => T + 1
        }),
        ?assertEqual(
            {error, seat_disabled},
            cs_seat_app:seat_heartbeat(org(Scope), #{
                workspace_id => ws(Scope), business_identity_id => Identity, at => T + 1
            })
        ),
        %% resume 后心跳恢复。
        {ok, _} = cs_seat_app:resume_seat(org(Scope), #{
            workspace_id => ws(Scope), business_identity_id => Identity, at => T + 1
        }),
        {ok, Hb} = cs_seat_app:seat_heartbeat(org(Scope), #{
            workspace_id => ws(Scope), business_identity_id => Identity, at => T + 2
        }),
        ?assertEqual(online, maps:get(status, Hb))
    catch
        Class:Reason:Stack ->
            erlang:Class({csbe05_suspend_gate, Reason, Stack})
    end.

%% ===================================================================
%% 验收 3：busy 派生 + 容量双保险；busy 仍可查已分配会话事实
%% ===================================================================

busy_derived_and_capacity_double_guard() ->
    Scope = ?FIX:new_scope(#{max_concurrent => 1}),
    try
        Identity = seat_identity(Scope),
        T = 1700000000,
        {ok, _} = cs_seat_app:seat_heartbeat(org(Scope), #{
            workspace_id => ws(Scope), business_identity_id => Identity, at => T
        }),
        %% 接满 1 个（容量 1）→ 派生 busy；presence 视图仍返回
        %% active_count 事实（busy 可查看已分配会话的读面不阻断）。
        SessionId = open_session(Scope),
        {ok, _} = cs_session_app:claim(org(Scope), #{
            workspace_id => ws(Scope),
            session_id => SessionId,
            business_identity_id => Identity,
            expected_version => 1,
            at => T + 1
        }),
        {ok, View} = cs_seat_app:seat_presence(org(Scope), #{
            workspace_id => ws(Scope), business_identity_id => Identity, at => T + 2
        }),
        ?assertEqual(busy, maps:get(status, View)),
        ?assertEqual(1, maps:get(active_count, View)),
        %% 派单侧：busy 被 select_seat 跳过（derived_status 过滤）+ 容量条件
        %% 双保险——两种路径都不再派给它。
        {ok, Seats} = cs_app_support:with_store(#{}, fun(Store) ->
            Store:list_dispatchable_seats(org(Scope))
        end),
        %% 显式 claim 超容量由容量条件排除（A02 seat_at_capacity 既有套件覆盖）。
        ?assertMatch(
            {error, no_seat_available},
            cs_dispatch:select_seat(
                org(Scope), cs_presence:annotate(Seats, presence_rows(Scope), T + 2)
            )
        )
    catch
        Class:Reason:Stack ->
            erlang:Class({csbe05_busy_capacity, Reason, Stack})
    end.

%% ===================================================================
%% 验收 2c：away/offline 不派单；全部不在线 → 保持 queued
%% ===================================================================

dispatch_skips_away_offline_keeps_queued() ->
    Scope = ?FIX:new_scope(),
    try
        A = seat_identity(Scope),
        T = 1700000000,
        %% A：心跳新鲜但手动 away。
        {ok, _} = cs_seat_app:seat_heartbeat(org(Scope), #{
            workspace_id => ws(Scope), business_identity_id => A, at => T
        }),
        {ok, _} = cs_seat_app:set_seat_manual_status(org(Scope), #{
            workspace_id => ws(Scope),
            business_identity_id => A,
            manual_status => <<"away">>,
            at => T + 1
        }),
        %% B：新 seat，从未心跳（无 presence 行 → offline）。
        B = extra_seat(Scope),
        OrgId = org(Scope),
        {ok, Seats} = cs_app_support:with_store(#{}, fun(Store) ->
            Store:list_dispatchable_seats(OrgId)
        end),
        ?assertEqual(2, length(Seats), "A/B 都是 enabled，快照应含两行"),
        Annotated = cs_presence:annotate(Seats, presence_rows(Scope), T + 2),
        %% away 与 offline 都不参与自动派单。
        ?assertMatch({error, no_seat_available}, cs_dispatch:select_seat(OrgId, Annotated)),
        %% queued 会话在无人派单时保持 queued（insert 后无人 claim）。
        SessionId = open_session(Scope),
        {ok, Session} = cs_session_app:fetch_session(OrgId, #{
            workspace_id => ws(Scope), session_id => SessionId
        }),
        ?assertEqual(queued, maps:get(status, Session)),
        %% B 心跳上线后（仍无 manual away）→ 自动派单可选。
        {ok, _} = cs_seat_app:seat_heartbeat(OrgId, #{
            workspace_id => ws(Scope), business_identity_id => B, at => T + 3
        }),
        {ok, Seats2} = cs_app_support:with_store(#{}, fun(Store) ->
            Store:list_dispatchable_seats(OrgId)
        end),
        ?assertMatch(
            {ok, B},
            cs_dispatch:select_seat(
                OrgId, cs_presence:annotate(Seats2, presence_rows(Scope), T + 4)
            )
        )
    catch
        Class:Reason:Stack ->
            erlang:Class({csbe05_dispatch_skips, Reason, Stack})
    end.

%% ===================================================================
%% 验收 4：多节点并发无双派（DB CAS 恰好一个成功）
%% ===================================================================

concurrent_claims_exactly_one_wins() ->
    Scope = ?FIX:new_scope(#{max_concurrent => 2}),
    try
        A = seat_identity(Scope),
        B = extra_seat(Scope),
        T = 1700000000,
        SessionId = open_session(Scope),
        Parent = self(),
        Claim = fun(IdentityId) ->
            cs_session_app:claim(org(Scope), #{
                workspace_id => ws(Scope),
                session_id => SessionId,
                business_identity_id => IdentityId,
                expected_version => 1,
                at => T
            })
        end,
        %% 两个"节点"同时显式 claim 同一 queued session（同 expected_version）。
        PidA = spawn_link(fun() -> Parent ! {a, Claim(A)} end),
        PidB = spawn_link(fun() -> Parent ! {b, Claim(B)} end),
        _ = [PidA, PidB],
        RA = receive_result(a),
        RB = receive_result(b),
        Wins = [R || R <- [RA, RB], element(1, R) =:= ok],
        ?assertEqual(1, length(Wins), "并发 claim 必须恰好一个成功"),
        ?assertEqual(2, length([R || R <- [RA, RB], element(1, R) =/= undefined])),
        %% 会话归属唯一：DB 里 active 绑定恰一个 identity。
        Count = ?FIX:scalar(
            <<
                "SELECT count(*) FROM customer_service_session"
                " WHERE organization_id = $1 AND id = $2"
                "   AND status = 'active'"
            >>,
            [org(Scope), SessionId]
        ),
        ?assertEqual(1, Count)
    catch
        Class:Reason:Stack ->
            erlang:Class({csbe05_concurrent_claim, Reason, Stack})
    end.

%% ===================================================================
%% 渐进式契约：无 derived_status 键 = 历史行为（旧调用方零破坏）
%% ===================================================================

dispatch_without_presence_key_is_backward_compatible() ->
    Scope = ?FIX:new_scope(),
    try
        A = seat_identity(Scope),
        T = 1700000000,
        OrgId = org(Scope),
        {ok, Seats} = cs_app_support:with_store(#{}, fun(Store) ->
            Store:list_dispatchable_seats(OrgId)
        end),
        %% 无键：从未心跳的 seat 照样可被选中（旧调用方行为不变）。
        ?assertMatch({ok, A}, cs_dispatch:select_seat(OrgId, Seats)),
        %% 显式 offline 键：被过滤（同一快照 + 键 = 新语义）。
        Annotated = [S#{derived_status => offline} || S <- Seats],
        ?assertMatch({error, no_seat_available}, cs_dispatch:select_seat(OrgId, Annotated))
    catch
        Class:Reason:Stack ->
            erlang:Class({csbe05_backward_compat, Reason, Stack})
    end.

%% ===================================================================
%% helpers
%% ===================================================================

receive_result(Tag) ->
    receive
        {Tag, R} -> R
    after 15000 ->
        erlang:error({csbe05_claim_timeout, Tag})
    end.

org(Scope) -> maps:get(org_id, Scope).
ws(Scope) -> maps:get(workspace_id, Scope).
seat_identity(Scope) -> maps:get(service_identity_id, Scope).

open_session(Scope) ->
    {ok, Session} = cs_session_app:open_session(org(Scope), #{
        workspace_id => ws(Scope),
        contact_id => maps:get(contact_id, Scope),
        conversation_id => maps:get(conversation_id, Scope),
        at => 1699999900
    }),
    maps:get(id, Session).

%% 第二个 customer_service seat（随机 TSID；复合 FK 需 identity 行同 Org）。
extra_seat(Scope) ->
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
            <<"cs01-extra-seat-">>,
            maps:get(owner_user_id, Scope)
        ]
    ),
    ok = ?FIX:exec(
        <<
            "INSERT INTO customer_service_seat"
            " (organization_id, business_identity_id, function_key, enabled, max_concurrent,"
            "  created_by_user_id)"
            " VALUES ($1, $2, 'customer_service', true, 1, $3)"
        >>,
        [OrgId, Identity, maps:get(owner_user_id, Scope)]
    ),
    Identity.

presence_rows(Scope) ->
    {ok, Rows} = cs_app_support:with_store(#{}, fun(Store) ->
        Store:list_seat_presence(org(Scope))
    end),
    Rows.

derived_status_at(Scope, Identity, NowSec) ->
    {ok, View} = cs_seat_app:seat_presence(org(Scope), #{
        workspace_id => ws(Scope), business_identity_id => Identity, at => NowSec
    }),
    maps:get(status, View).

stored_heartbeat_at(Scope, Identity) ->
    ?FIX:scalar(
        <<
            "SELECT extract(epoch from last_heartbeat_at)::bigint"
            "  FROM customer_service_seat_presence"
            " WHERE organization_id = $1 AND business_identity_id = $2"
        >>,
        [org(Scope), Identity]
    ).

stored_manual_status(Scope, Identity) ->
    Raw = ?FIX:scalar(
        <<
            "SELECT manual_status FROM customer_service_seat_presence"
            " WHERE organization_id = $1 AND business_identity_id = $2"
        >>,
        [org(Scope), Identity]
    ),
    case Raw of
        null -> undefined;
        undefined -> undefined;
        Other -> Other
    end.
