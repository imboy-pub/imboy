%%% @doc CS-BE-01C（GAP-1）：坐席门的**真库聚焦**验证（生产装配链）。
%%%
%%% 与 `eb_auth_seat_gate_tests`（纯逻辑 + facade meck）互补：本套件直连
%%% scratch PG，走**生产事实装配**（`eb_pg_auth_facts` facts + 经
%%% `customer_service_facade` 的真实 seat 表读写），证明 GAP-1 在真实链路闭合：
%%%
%%%   * CS 职能 assignment active + **无 seat 行**（从未开通坐席）→ 授权放行
%%%     （维持既有行为，落后续 asset ACL）；
%%%   * create_seat（enabled）→ 授权放行；
%%%   * **suspend_seat → 下一次 authorize_via_port 即 `{error, seat_disabled}`**
%%%     （逐请求现读 seat，suspend 即时生效——这正是 CS-INT-01 GAP-1 的缺口）；
%%%   * resume_seat → 恢复放行。
%%%
%%% 数据隔离：`eb_pg_test_fixture:new_scope/0` 全新随机 TSID scope；seat /
%%% assignment 行由本套件显式清理（fixture cleanup 不覆盖 cs_seat 表）。
-module(eb_auth_seat_gate_pg_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

-define(FIX, eb_pg_test_fixture).

gap_closure_on_production_assembly_test_() ->
    {setup, fun setup/0, fun cleanup_db/1, fun cases/1}.

setup() ->
    case eunit_runner:eunit_setup_with_db() of
        {ok, Conn} -> {ok, Conn};
        {error, Reason} -> {error, Reason}
    end.

cleanup_db({ok, Conn}) ->
    eunit_runner:eunit_cleanup_db(Conn);
cleanup_db(_Other) ->
    ok.

cases({ok, _Conn}) ->
    [fun seat_gate_follows_real_seat_lifecycle/0];
cases({error, Reason}) ->
    [
        fun() ->
            ?assertMatch({error, _}, {error, Reason})
        end
    ].

%% 生产装配链全周期：无 seat → enabled seat → suspended → resumed。
seat_gate_follows_real_seat_lifecycle() ->
    Scope = ?FIX:new_scope(),
    Org = maps:get(org_id, Scope),
    Ws = maps:get(workspace_id, Scope),
    Owner = maps:get(owner_user_id, Scope),
    Peer = maps:get(peer_user_id, Scope),
    Service = maps:get(service_identity_id, Scope),
    Route = #{
        auth_context => enterprise_member,
        surface => tenant,
        path => <<"/api/v1/enterprise/organizations/1/conversations/42/messages">>,
        required_function => <<"customer_service">>,
        required_permission => <<"asset.write">>
    },
    Request = #{
        organization_id => Org,
        user_id => Peer,
        credential => #{class => imboy_jwt, user_id => Peer, claims => #{}}
    },
    try
        %% peer 补 active member 行（fixture 只为 owner/actor 造成员），再补
        %% customer_service 经办
        ok = ?FIX:exec(
            <<
                "INSERT INTO organization_member(organization_id,user_id,role,status)"
                " VALUES ($1,$2,'member','active')"
            >>,
            [Org, Peer]
        ),
        ok = ?FIX:exec(
            <<
                "INSERT INTO organization_business_identity_assignment"
                " (id,organization_id,business_identity_id,function_key,user_id,status,"
                "  assigned_by,version) VALUES ($1,$2,$3,'customer_service',$4,'active',$5,1)"
            >>,
            [?FIX:id(), Org, Service, Peer, Owner]
        ),
        %% 1) 从未开通坐席（无 seat 行）→ 维持既有授权行为
        ?assertMatch(
            {ok, #{auth_context := enterprise_member, business_identity_id := Service}},
            eb_auth_app:authorize_via_port(eb_pg_auth_facts, Route, Request)
        ),
        %% 2) 开通坐席（enabled）→ 放行
        {ok, _} = customer_service_facade:create_seat(Org, #{
            business_identity_id => Service,
            workspace_id => Ws,
            created_by_user_id => Owner
        }),
        ?assertMatch(
            {ok, #{auth_context := enterprise_member}},
            eb_auth_app:authorize_via_port(eb_pg_auth_facts, Route, Request)
        ),
        %% 3) suspend → 同一凭证的下一次判定立即 seat_disabled（GAP-1 闭合点）
        {ok, _} = customer_service_facade:suspend_seat(Org, #{
            business_identity_id => Service,
            workspace_id => Ws,
            at => os:system_time(millisecond),
            actor_user_id => Owner
        }),
        ?assertEqual(
            {error, seat_disabled},
            eb_auth_app:authorize_via_port(eb_pg_auth_facts, Route, Request)
        ),
        %% 4) resume → 恢复放行
        {ok, _} = customer_service_facade:resume_seat(Org, #{
            business_identity_id => Service,
            workspace_id => Ws,
            at => os:system_time(millisecond),
            actor_user_id => Owner
        }),
        ?assertMatch(
            {ok, #{auth_context := enterprise_member}},
            eb_auth_app:authorize_via_port(eb_pg_auth_facts, Route, Request)
        )
    after
        ok = ?FIX:exec(
            <<"DELETE FROM customer_service_seat WHERE organization_id=$1 AND business_identity_id=$2">>,
            [Org, Service]
        ),
        ok = ?FIX:exec(
            <<
                "DELETE FROM organization_business_identity_assignment"
                " WHERE organization_id=$1 AND business_identity_id=$2"
            >>,
            [Org, Service]
        ),
        ok = ?FIX:exec(
            <<"DELETE FROM organization_member WHERE organization_id=$1 AND user_id=$2">>,
            [Org, Peer]
        ),
        ?FIX:cleanup(Scope)
    end.
