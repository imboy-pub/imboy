%%% @doc F1/F2（RULING-2026-09-15 §五/§六）事实层投影的真库聚焦测试。
%%%
%%% 覆盖裁决原文的正负例清单：
%%%   * active owner  -> governance_roles=[owner]、治理能力（org.manage 等）、
%%%                      **不含**任何业务读写（owner 不因治理角色获得业务权限）；
%%%   * active admin  -> governance_roles=[admin]、同一治理集、无业务读写；
%%%   * active member + active sales assignment -> governance=[]、恰好 §五第 3 条
%%%                      冻结的 8 项业务能力（含写），**不含**治理能力；
%%%   * active member 无 assignment -> permissions=[]（业务读也拿不到）；
%%%   * suspended/removed 即使历史 role=owner/admin -> permissions=[]；
%%%   * 跨 Org（成员行不存在于目标 Org）-> {error, no_member}。
%%%
%%% 本套件直连 scratch PG（config/sys.local.eb*.config），不 mock 事实层——
%%% 投影的正确性必须以真实 organization_member / assignment 行为准。
-module(eb_pg_auth_facts_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

-define(FIX, eb_pg_test_fixture).

f1_f2_projection_test_() ->
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
    [
        fun owner_projection/0,
        fun member_with_sales_assignment_projection/0,
        fun member_without_assignment_projection/0,
        fun admin_projection/0,
        fun suspended_owner_gets_nothing/0,
        fun removed_admin_gets_nothing/0,
        fun cross_org_rejected/0
    ].

%%% 正例：active owner —— 治理集，无业务读写
owner_projection() ->
    Scope = ?FIX:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        Owner = maps:get(owner_user_id, Scope),
        {ok, Facts} = eb_pg_auth_facts:load_request_facts(#{
            organization_id => Org, user_id => Owner
        }),
        ?assertEqual([<<"owner">>], maps:get(governance_roles, maps:get(member, Facts))),
        ?assertEqual(owner, maps:get(role, maps:get(member, Facts))),
        P = lists:sort(maps:get(permissions, Facts)),
        ?assertEqual(
            lists:sort([
                <<"org.manage">>,
                <<"member.manage">>,
                <<"member.suspend">>,
                <<"retention.manage">>,
                <<"offboarding.manage">>
            ]),
            P
        ),
        %% owner 不因治理角色自动获得业务读写（§五第 2 条）
        ?assertEqual(false, lists:member(<<"contact.write">>, P)),
        ?assertEqual(false, lists:member(<<"conversation.read">>, P))
    after
        ?FIX:cleanup(Scope)
    end.

%%% 正例：active member + active sales assignment —— 恰好 §五第 3 条 8 项业务能力
member_with_sales_assignment_projection() ->
    Scope = ?FIX:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        Actor = maps:get(actor_user_id, Scope),
        {ok, Facts} = eb_pg_auth_facts:load_request_facts(#{
            organization_id => Org, user_id => Actor
        }),
        ?assertEqual([], maps:get(governance_roles, maps:get(member, Facts))),
        P = lists:sort(maps:get(permissions, Facts)),
        ?assertEqual(
            lists:sort([
                <<"contact.read">>,
                <<"contact.write">>,
                <<"note.write">>,
                <<"conversation.read">>,
                <<"conversation.write">>,
                <<"message.write">>,
                <<"asset.read">>,
                <<"asset.write">>
            ]),
            P
        ),
        %% member 即使持有经办身份也无治理资格（§五第 4/5 条）
        ?assertEqual(false, lists:member(<<"org.manage">>, P)),
        ?assertEqual(false, lists:member(<<"member.suspend">>, P))
    after
        ?FIX:cleanup(Scope)
    end.

%%% 负例：active member 无 assignment —— 业务权限为空
member_without_assignment_projection() ->
    Scope = ?FIX:new_scope(),
    Peer = ?FIX:id(),
    try
        Org = maps:get(org_id, Scope),
        ok = ?FIX:exec(
            <<
                "INSERT INTO \"user\"(id,password,account,reg_ip,reg_cosv)"
                " VALUES ($1,'x',$2,'127.0.0.1','x')"
            >>,
            [Peer, <<"ebf1-account-", (integer_to_binary(Peer))/binary>>]
        ),
        ok = ?FIX:exec(
            <<
                "INSERT INTO organization_member(organization_id,user_id,role,status)"
                " VALUES ($1,$2,'member','active')"
            >>,
            [Org, Peer]
        ),
        {ok, Facts} = eb_pg_auth_facts:load_request_facts(#{
            organization_id => Org, user_id => Peer
        }),
        ?assertEqual([], maps:get(governance_roles, maps:get(member, Facts))),
        ?assertEqual([], maps:get(permissions, Facts))
    after
        ?FIX:cleanup(Scope)
    end.

%%% 正例：active admin —— 与 owner 同治理集（§五第 1 条 owner/admin 并列）
admin_projection() ->
    Scope = ?FIX:new_scope(),
    Admin = ?FIX:id(),
    try
        Org = maps:get(org_id, Scope),
        ok = ?FIX:exec(
            <<
                "INSERT INTO \"user\"(id,password,account,reg_ip,reg_cosv)"
                " VALUES ($1,'x',$2,'127.0.0.1','x')"
            >>,
            [Admin, <<"ebf1-account-", (integer_to_binary(Admin))/binary>>]
        ),
        ok = ?FIX:exec(
            <<
                "INSERT INTO organization_member(organization_id,user_id,role,status)"
                " VALUES ($1,$2,'admin','active')"
            >>,
            [Org, Admin]
        ),
        {ok, Facts} = eb_pg_auth_facts:load_request_facts(#{
            organization_id => Org, user_id => Admin
        }),
        ?assertEqual([<<"admin">>], maps:get(governance_roles, maps:get(member, Facts))),
        P = lists:sort(maps:get(permissions, Facts)),
        ?assertEqual(
            lists:sort([
                <<"org.manage">>,
                <<"member.manage">>,
                <<"member.suspend">>,
                <<"retention.manage">>,
                <<"offboarding.manage">>
            ]),
            P
        ),
        ?assertEqual(false, lists:member(<<"message.write">>, P))
    after
        ?FIX:cleanup(Scope)
    end.

%%% 负例：suspended 即使历史 role=owner 也拿不到任何权限（§六第 4 条）
suspended_owner_gets_nothing() ->
    Scope = ?FIX:new_scope(),
    Suspended = ?FIX:id(),
    try
        Org = maps:get(org_id, Scope),
        ok = ?FIX:exec(
            <<
                "INSERT INTO \"user\"(id,password,account,reg_ip,reg_cosv)"
                " VALUES ($1,'x',$2,'127.0.0.1','x')"
            >>,
            [Suspended, <<"ebf1-account-", (integer_to_binary(Suspended))/binary>>]
        ),
        ok = ?FIX:exec(
            <<
                "INSERT INTO organization_member(organization_id,user_id,role,status)"
                " VALUES ($1,$2,'owner','suspended')"
            >>,
            [Org, Suspended]
        ),
        {ok, Facts} = eb_pg_auth_facts:load_request_facts(#{
            organization_id => Org, user_id => Suspended
        }),
        %% role 投影如实保留（判定层 active_member 门会拦），但权限必须为空
        ?assertEqual([<<"owner">>], maps:get(governance_roles, maps:get(member, Facts))),
        ?assertEqual(suspended, maps:get(status, maps:get(member, Facts))),
        ?assertEqual([], maps:get(permissions, Facts))
    after
        ?FIX:cleanup(Scope)
    end.

%%% 负例：removed 同法
removed_admin_gets_nothing() ->
    Scope = ?FIX:new_scope(),
    Removed = ?FIX:id(),
    try
        Org = maps:get(org_id, Scope),
        ok = ?FIX:exec(
            <<
                "INSERT INTO \"user\"(id,password,account,reg_ip,reg_cosv)"
                " VALUES ($1,'x',$2,'127.0.0.1','x')"
            >>,
            [Removed, <<"ebf1-account-", (integer_to_binary(Removed))/binary>>]
        ),
        ok = ?FIX:exec(
            <<
                "INSERT INTO organization_member(organization_id,user_id,role,status)"
                " VALUES ($1,$2,'admin','removed')"
            >>,
            [Org, Removed]
        ),
        {ok, Facts} = eb_pg_auth_facts:load_request_facts(#{
            organization_id => Org, user_id => Removed
        }),
        ?assertEqual([], maps:get(permissions, Facts))
    after
        ?FIX:cleanup(Scope)
    end.

%%% 负例：跨 Org —— 目标 Org 无成员行即拒绝（不降级为空事实）
cross_org_rejected() ->
    Scope = ?FIX:new_scope(),
    try
        OtherOrg = maps:get(other_org_id, Scope),
        Actor = maps:get(actor_user_id, Scope),
        ?assertEqual(
            {error, no_member},
            eb_pg_auth_facts:load_request_facts(#{
                organization_id => OtherOrg, user_id => Actor
            })
        )
    after
        ?FIX:cleanup(Scope)
    end.
