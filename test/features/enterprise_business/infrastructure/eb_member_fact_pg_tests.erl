%%% @doc EB-03R P10 套件：最小只读事实 Port（成员状态 / 默认 Workspace）。
%%%
%%% 判定口径（A10）：只读（零写 callback）、逐请求直读、fail-closed；
%%% 事实**不是**授权结论（授权走 eb_auth_port）。
-module(eb_member_fact_pg_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

member_fact_test_() ->
    {setup, fun setup/0, fun cleanup_db/1, fun cases/1}.

setup() ->
    eunit_runner:eunit_setup_with_db().

cleanup_db({ok, Conn}) ->
    eunit_runner:eunit_cleanup_db(Conn);
cleanup_db(_Other) ->
    ok.

cases({ok, _Conn}) ->
    [
        {timeout, 60, fun member_status_reads_current_fact/0},
        {timeout, 60, fun member_status_is_fail_closed_without_relation/0},
        {timeout, 60, fun default_workspace_resolves_within_org_and_requires_active_member/0},
        {timeout, 60, fun only_read_statements_are_frozen/0},
        {timeout, 60, fun port_declares_zero_write_callbacks/0}
    ];
cases(_Skipped) ->
    {skip, "member fact suite requires the scratch database connection"}.

%% 逐请求直读：状态变化后同一次会话内立即反映（不缓存）。
member_status_reads_current_fact() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        Actor = maps:get(actor_user_id, Scope),
        ?assertEqual({ok, active}, eb_member_fact_pg:member_status(Org, Actor)),
        ok = eb_pg_test_fixture:exec(
            <<
                "UPDATE organization_member SET status='suspended'"
                " WHERE organization_id=$1 AND user_id=$2"
            >>,
            [Org, Actor]
        ),
        ?assertEqual({ok, suspended}, eb_member_fact_pg:member_status(Org, Actor)),
        %% 一旦 suspended，默认 Workspace 解析必须 fail-closed（不得继续给出可用事实）
        ?assertEqual({error, no_default_workspace}, eb_member_fact_pg:default_workspace(Org, Actor))
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

member_status_is_fail_closed_without_relation() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        Actor = maps:get(actor_user_id, Scope),
        Peer = maps:get(peer_user_id, Scope),
        ?assertEqual({error, no_member}, eb_member_fact_pg:member_status(Org, Peer)),
        %% 另一个 Org：Actor 在那里没有成员关系 ⇒ 不得因为 user 存在就返回事实
        %% （注意 owner 会被 113 的同步触发器写进每个新 Org，故不能用 owner 做此负例）
        ?assertEqual(
            {error, no_member},
            eb_member_fact_pg:member_status(maps:get(other_org_id, Scope), Actor)
        ),
        %% 非法入参一律 fail-closed（不得猜）
        ?assertEqual({error, no_member}, eb_member_fact_pg:member_status(Org, <<"not-int">>))
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

default_workspace_resolves_within_org_and_requires_active_member() ->
    Scope = eb_pg_test_fixture:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        Actor = maps:get(actor_user_id, Scope),
        Workspace = maps:get(workspace_id, Scope),
        ?assertEqual({ok, Workspace}, eb_member_fact_pg:default_workspace(Org, Actor)),
        %% 跨 Org：同一个人在别的 Org 没有成员关系 ⇒ 不得解析出别人的 Workspace
        ?assertEqual(
            {error, no_default_workspace},
            eb_member_fact_pg:default_workspace(maps:get(other_org_id, Scope), Actor)
        ),
        %% 解出的 Workspace 必须属于该 Org（同语句带 organization_id 的机械证据）
        ?assertEqual(
            1,
            eb_pg_test_fixture:scalar(
                <<"SELECT count(*) AS n FROM workspace WHERE organization_id=$1 AND id=$2">>,
                [Org, Workspace]
            )
        )
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

only_read_statements_are_frozen() ->
    Statements = eb_member_fact_pg:sql_statements(),
    ?assert(length(Statements) >= 2),
    lists:foreach(
        fun(Sql) ->
            Upper = string:uppercase(binary_to_list(Sql)),
            ?assertNotEqual(nomatch, string:find(Upper, "SELECT")),
            ?assertEqual(nomatch, string:find(Upper, "INSERT")),
            ?assertEqual(nomatch, string:find(Upper, "UPDATE")),
            ?assertEqual(nomatch, string:find(Upper, "DELETE"))
        end,
        Statements
    ).

port_declares_zero_write_callbacks() ->
    Callbacks = eb_member_fact_port:behaviour_info(callbacks),
    ?assertEqual([{default_workspace, 2}, {member_status, 2}], lists:sort(Callbacks)),
    ?assertEqual(
        [],
        [N || {N, _A} <- Callbacks, is_write_name(atom_to_list(N))]
    ).

is_write_name(Name) ->
    re:run(Name, "insert|update|delete|advance|append|purge|suspend", [{capture, none}]) =:= match.
