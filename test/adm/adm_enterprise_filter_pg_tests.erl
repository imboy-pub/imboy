-module(adm_enterprise_filter_pg_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").
-include("eunit_setup.hrl").

%%% ===================================================================
%%% A3（Admin 企业入口服务端强制过滤）真库集成测试（PG 不可用自动 skip）
%%%
%%% 证明链：adm_enterprise_filter 构造的谓词 → elib_pg DSL → 真实 PostgreSQL：
%%%   1. 企业群入口：personal 群零可见（跨库真实行断言，不是 mock）；
%%%   2. 企业频道入口：personal + 禁用（status=0）频道双零可见；
%%%   3. Organization 真源解析：workspace_repo:ids_by_organization 按
%%%      workspace.organization_id 解析；未知组织 fail-closed 为零行；
%%%   4. 跨 O 不可见：org A 过滤下 org B 的 workspace 资源零命中；
%%%   5. workspaces/projects 列表 O 维度过滤（admin_page/5 链路）。
%%%
%%% 造数 autocommit；try/after 显式清理（workspace 删除级联 project/channel/
%%% workspace_member；organization RESTRICT 故最后删）。
%%% ===================================================================

-define(SETUP_TAG, <<"A3-ENT">>).

enterprise_group_personal_zero_leak_pg_test_() ->
    ?TEST_WITH_DB_TIMEOUT(60, fun() ->
        with_enterprise_world(fun(
            #{org_a := OrgA, g_ws_a := GWsA, g_ws_a_disabled := GWsADisabled} = Ctx
        ) ->
            %% 企业入口 + Organization 过滤：可见行 = org A 的工作区群
            %% （§13.1 只强制 scope=workspace + O/W；群不强制 status=1，
            %%   禁用工作区群仍属企业治理视野——与频道（强制 status=1）不同）
            Params = #{preset => <<"enterprise">>, organization_id => OrgA, workspace_id => 0},
            OrgWs = adm_enterprise_filter:org_workspace_ids(Params),
            {ok, [WA1]} = OrgWs,
            ?assertEqual(WA1, maps:get(ws_a1, Ctx)),
            Where = adm_enterprise_filter:group_where(#{}, Params, OrgWs),
            {ok, P} = group_repo:page(1, 50, Where, <<"id ASC">>),
            Ids = [maps:get(<<"id">>, Row) || Row <- maps:get(list, P)],
            ?assertEqual([GWsA, GWsADisabled], Ids),
            %% 负例：personal 群 / 跨 O 群零命中（服务端谓词排除）
            ?assert(false == lists:member(maps:get(g_personal, Ctx), Ids)),
            ?assert(false == lists:member(maps:get(g_ws_b, Ctx), Ids)),

            %% 企业入口不带 Organization：仍强制 scope=workspace（personal 零可见）
            Params2 = #{preset => <<"enterprise">>, organization_id => 0, workspace_id => 0},
            Where2 = adm_enterprise_filter:group_where(#{}, Params2, not_requested),
            {ok, P2} = group_repo:page(1, 50, Where2, <<"id ASC">>),
            Scopes = [maps:get(<<"scope">>, Row) || Row <- maps:get(list, P2)],
            ?assert(lists:member(GWsA, [maps:get(<<"id">>, Row) || Row <- maps:get(list, P2)])),
            ?assertEqual(
                [],
                [Row || Row <- maps:get(list, P2), maps:get(<<"workspace_id">>, Row) =:= null]
            ),
            ?assertEqual([], [S || S <- Scopes, S =/= <<"workspace">>]),
            ?assert(
                false ==
                    lists:member(maps:get(g_personal, Ctx), [
                        maps:get(<<"id">>, Row)
                     || Row <- maps:get(list, P2)
                    ])
            )
        end)
    end).

enterprise_channel_personal_and_disabled_zero_leak_pg_test_() ->
    ?TEST_WITH_DB_TIMEOUT(60, fun() ->
        with_enterprise_world(fun(#{org_a := OrgA, c_ws_a := CWsA} = Ctx) ->
            Params = #{preset => <<"enterprise">>, organization_id => OrgA, workspace_id => 0},
            OrgWs = adm_enterprise_filter:org_workspace_ids(Params),
            Column =
                <<"id, name, scope, workspace_id, status, subscriber_count, created_at, updated_at">>,
            Where = adm_enterprise_filter:channel_where(#{}, Params, OrgWs),
            {ok, P} = channel_ds:page(Column, Where, <<"id ASC">>, 1, 50),
            Ids = [maps:get(<<"id">>, Row) || Row <- maps:get(list, P)],
            ?assertEqual([CWsA], Ids),
            %% 负例：personal 频道、禁用频道（status=0）、跨 O 频道全部零命中
            ?assert(false == lists:member(maps:get(c_personal, Ctx), Ids)),
            ?assert(false == lists:member(maps:get(c_ws_a_disabled, Ctx), Ids)),
            ?assert(false == lists:member(maps:get(c_ws_b, Ctx), Ids))
        end)
    end).

unknown_org_fail_closed_zero_rows_pg_test_() ->
    ?TEST_WITH_DB_TIMEOUT(60, fun() ->
        with_enterprise_world(fun(_Ctx) ->
            UnknownOrg = elib_tsid:generate(organization),
            Params = #{
                preset => <<"enterprise">>, organization_id => UnknownOrg, workspace_id => 0
            },
            ?assertEqual({ok, []}, adm_enterprise_filter:org_workspace_ids(Params)),
            Where = adm_enterprise_filter:group_where(#{}, Params, {ok, []}),
            {ok, P} = group_repo:page(1, 50, Where, <<"id ASC">>),
            ?assertEqual(0, maps:get(total, P)),
            ?assertEqual([], maps:get(list, P))
        end)
    end).

cross_org_workspace_not_invisible_pg_test_() ->
    ?TEST_WITH_DB_TIMEOUT(60, fun() ->
        with_enterprise_world(fun(#{org_a := OrgA, ws_b1 := WB1} = _Ctx) ->
            %% workspace_id 不属于指定 Organization → 服务端复核后零命中
            Params = #{preset => <<"enterprise">>, organization_id => OrgA, workspace_id => WB1},
            OrgWs = adm_enterprise_filter:org_workspace_ids(Params),
            Where = adm_enterprise_filter:group_where(#{}, Params, OrgWs),
            {ok, P} = group_repo:page(1, 50, Where, <<"id ASC">>),
            ?assertEqual(0, maps:get(total, P))
        end)
    end).

workspace_and_project_org_filter_pg_test_() ->
    ?TEST_WITH_DB_TIMEOUT(60, fun() ->
        with_enterprise_world(fun(#{org_a := OrgA, org_b := OrgB, ws_a1 := WA1, p_a := PA} = Ctx) ->
            %% workspaces 列表 O 维度过滤：仅 org A 的工作区
            {ok, WsP} = workspace_ds:admin_page(1, 50, all, <<>>, OrgA),
            WsIds = [maps:get(<<"id">>, Row) || Row <- maps:get(list, WsP)],
            ?assertEqual([WA1], WsIds),
            ?assertEqual(1, maps:get(total, WsP)),

            %% projects 列表 O 维度过滤（只读语义）：仅 org A 的项目
            {ok, PrP} = project_ds:admin_page(1, 50, all, <<>>, OrgA),
            PrIds = [maps:get(<<"id">>, Row) || Row <- maps:get(list, PrP)],
            ?assertEqual([PA], PrIds),
            ?assertEqual(1, maps:get(total, PrP)),

            %% 跨 O：org B 维度只能看到 org B 的资源
            {ok, WsP2} = workspace_ds:admin_page(1, 50, all, <<>>, OrgB),
            ?assertEqual([maps:get(ws_b1, Ctx)], [
                maps:get(<<"id">>, Row)
             || Row <- maps:get(list, WsP2)
            ]),
            {ok, PrP2} = project_ds:admin_page(1, 50, all, <<>>, OrgB),
            ?assertEqual([maps:get(p_b, Ctx)], [
                maps:get(<<"id">>, Row)
             || Row <- maps:get(list, PrP2)
            ])
        end)
    end).

%%% ===================================================================
%%% 造数 / 清理
%%% ===================================================================

with_enterprise_world(Fun) ->
    {ok, Conn} = take_conn(),
    Ctx = seed_enterprise_world(Conn),
    try
        Fun(Ctx)
    after
        cleanup_enterprise_world(Conn, Ctx)
    end.

seed_enterprise_world(Conn) ->
    Owner = elib_tsid:generate(user),
    OrgA = elib_tsid:generate(organization),
    OrgB = elib_tsid:generate(organization),
    WA1 = elib_tsid:generate(workspace),
    WB1 = elib_tsid:generate(workspace),
    GWsA = elib_tsid:generate(group),
    GPersonal = elib_tsid:generate(group),
    GWsB = elib_tsid:generate(group),
    GWsADisabled = elib_tsid:generate(group),
    CWsA = elib_tsid:generate(channel),
    CPersonal = elib_tsid:generate(channel),
    CWsADisabled = elib_tsid:generate(channel),
    CWsB = elib_tsid:generate(channel),
    PA = elib_tsid:generate(project),
    PB = elib_tsid:generate(project),
    Tag = <<?SETUP_TAG/binary, "-", (integer_to_binary(OrgA))/binary>>,

    ok = exec(
        Conn,
        <<"INSERT INTO \"user\"(id,password,account,reg_ip,reg_cosv)",
            " VALUES ($1,'x',$2,'127.0.0.1','x')">>,
        [Owner, Tag]
    ),
    ok = exec(
        Conn,
        <<"INSERT INTO organization(id,name,owner_id,status)", " VALUES ($1,$2,$3,'active')">>,
        [OrgA, <<Tag/binary, "-a">>, Owner]
    ),
    ok = exec(
        Conn,
        <<"INSERT INTO organization(id,name,owner_id,status)", " VALUES ($1,$2,$3,'active')">>,
        [OrgB, <<Tag/binary, "-b">>, Owner]
    ),
    ok = exec(
        Conn,
        <<"INSERT INTO workspace(id,name,owner_id,status,type,organization_id)",
            " VALUES ($1,$2,$3,'active','project',$4)">>,
        [WA1, <<Tag/binary, "-wa1">>, Owner, OrgA]
    ),
    ok = exec(
        Conn,
        <<"INSERT INTO workspace(id,name,owner_id,status,type,organization_id)",
            " VALUES ($1,$2,$3,'active','project',$4)">>,
        [WB1, <<Tag/binary, "-wb1">>, Owner, OrgB]
    ),
    %% project 复合 FK（fk_project_owner_membership）要求 owner 是对应
    %% workspace_member，两工作区各补 owner 成员行（workspace 删除级联清理）
    ok = exec(
        Conn,
        <<"INSERT INTO workspace_member(workspace_id,user_id,role,invited_by,joined_at,status)",
            " VALUES ($1,$2,'owner',NULL,CURRENT_TIMESTAMP,'active')">>,
        [WA1, Owner]
    ),
    ok = exec(
        Conn,
        <<"INSERT INTO workspace_member(workspace_id,user_id,role,invited_by,joined_at,status)",
            " VALUES ($1,$2,'owner',NULL,CURRENT_TIMESTAMP,'active')">>,
        [WB1, Owner]
    ),

    %% 群：org A 工作区群 / personal 群 / org B 工作区群 / org A 禁用群
    ok = exec(
        Conn,
        <<"INSERT INTO \"group\"(id,title,owner_uid,creator_uid,status,scope,workspace_id)",
            " VALUES ($1,$2,$3,$3,1,'workspace',$4)">>,
        [GWsA, <<Tag/binary, "-gwsa">>, Owner, WA1]
    ),
    ok = exec(
        Conn,
        <<"INSERT INTO \"group\"(id,title,owner_uid,creator_uid,status,scope)",
            " VALUES ($1,$2,$3,$3,1,'personal')">>,
        [GPersonal, <<Tag/binary, "-gp">>, Owner]
    ),
    ok = exec(
        Conn,
        <<"INSERT INTO \"group\"(id,title,owner_uid,creator_uid,status,scope,workspace_id)",
            " VALUES ($1,$2,$3,$3,1,'workspace',$4)">>,
        [GWsB, <<Tag/binary, "-gwsb">>, Owner, WB1]
    ),
    ok = exec(
        Conn,
        <<"INSERT INTO \"group\"(id,title,owner_uid,creator_uid,status,scope,workspace_id)",
            " VALUES ($1,$2,$3,$3,0,'workspace',$4)">>,
        [GWsADisabled, <<Tag/binary, "-gwsa0">>, Owner, WA1]
    ),

    %% 频道：org A 工作区频道 / personal / org A 禁用 / org B 工作区
    ok = exec(
        Conn,
        <<"INSERT INTO channel(id,name,creator_uid,status,scope,workspace_id)",
            " VALUES ($1,$2,$3,1,'workspace',$4)">>,
        [CWsA, <<Tag/binary, "-cwsa">>, Owner, WA1]
    ),
    ok = exec(
        Conn,
        <<"INSERT INTO channel(id,name,creator_uid,status,scope)",
            " VALUES ($1,$2,$3,1,'personal')">>,
        [CPersonal, <<Tag/binary, "-cp">>, Owner]
    ),
    ok = exec(
        Conn,
        <<"INSERT INTO channel(id,name,creator_uid,status,scope,workspace_id)",
            " VALUES ($1,$2,$3,0,'workspace',$4)">>,
        [CWsADisabled, <<Tag/binary, "-cwsa0">>, Owner, WA1]
    ),
    ok = exec(
        Conn,
        <<"INSERT INTO channel(id,name,creator_uid,status,scope,workspace_id)",
            " VALUES ($1,$2,$3,1,'workspace',$4)">>,
        [CWsB, <<Tag/binary, "-cwsb">>, Owner, WB1]
    ),

    %% 项目：org A / org B 各一
    ok = exec(
        Conn,
        <<"INSERT INTO project(id,workspace_id,name,owner_id,status)",
            " VALUES ($1,$2,$3,$4,'active')">>,
        [PA, WA1, <<Tag/binary, "-pa">>, Owner]
    ),
    ok = exec(
        Conn,
        <<"INSERT INTO project(id,workspace_id,name,owner_id,status)",
            " VALUES ($1,$2,$3,$4,'active')">>,
        [PB, WB1, <<Tag/binary, "-pb">>, Owner]
    ),

    #{
        conn => Conn,
        owner => Owner,
        org_a => OrgA,
        org_b => OrgB,
        ws_a1 => WA1,
        ws_b1 => WB1,
        g_ws_a => GWsA,
        g_personal => GPersonal,
        g_ws_b => GWsB,
        g_ws_a_disabled => GWsADisabled,
        c_ws_a => CWsA,
        c_personal => CPersonal,
        c_ws_a_disabled => CWsADisabled,
        c_ws_b => CWsB,
        p_a => PA,
        p_b => PB
    }.

cleanup_enterprise_world(Conn, Ctx) ->
    Ids = lists:map(
        fun(K) -> maps:get(K, Ctx) end,
        [g_ws_a, g_personal, g_ws_b, g_ws_a_disabled, c_ws_a, c_personal, c_ws_a_disabled, c_ws_b]
    ),
    ok = exec(Conn, <<"DELETE FROM \"group\" WHERE id = ANY($1)">>, [Ids]),
    ok = exec(Conn, <<"DELETE FROM channel WHERE id = ANY($1)">>, [Ids]),
    %% workspace 删除级联 project/channel/workspace_member
    ok = exec(Conn, <<"DELETE FROM workspace WHERE id = ANY($1)">>, [
        [maps:get(ws_a1, Ctx), maps:get(ws_b1, Ctx)]
    ]),
    ok = exec(Conn, <<"DELETE FROM organization WHERE id = ANY($1)">>, [
        [maps:get(org_a, Ctx), maps:get(org_b, Ctx)]
    ]),
    ok = exec(Conn, <<"DELETE FROM \"user\" WHERE id = $1">>, [maps:get(owner, Ctx)]),
    ok.

exec(Conn, Sql, Params) ->
    case epgsql:equery(Conn, Sql, Params) of
        {ok, _} -> ok;
        {ok, _, _} -> ok;
        {error, Reason} -> erlang:error({seed_sql_failed, Reason, Sql})
    end.

-spec take_conn() -> {ok, pid()} | {error, term()}.
take_conn() ->
    case pooler:take_member(pgsql) of
        error_no_members ->
            timer:sleep(200),
            case pooler:take_member(pgsql) of
                error_no_members -> {error, no_connection};
                Conn -> {ok, Conn}
            end;
        Conn ->
            {ok, Conn}
    end.
