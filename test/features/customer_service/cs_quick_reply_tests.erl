%% CS Quick Reply 后端合同验证（真库：inttest marker DB 同款 disposable PG；
%% 合同 v3/specs/cs-quick-reply.md §1/§2/§4/§5）。
%% 断言面：
%%   * 数据面 CRUD：创建/分页/更新全量（校验在 app 层，此处覆盖数据面语义）；
%%   * 软删：行对一切读不可见；DELETE 幂等（重复删除同 ok）；
%%   * 越权不可见：跨 (org, workspace) find/update/delete 不可见裁决；
%%   * org 约束机械断言：update/soft_delete WHERE 携带 org+ws（铁律6 纵深防御）；
%%   * app 层：title/content trim 长度界；seat 只读投影白名单 + assignment 门。
%% 前置：独占测试 PG（elib_pg 池；真实 user/org/workspace/member + 00000240）。
%% 每次只建立和清理带唯一合成 ID 的本套件数据，不清除共享前缀范围。
-module(cs_quick_reply_tests).

-include_lib("eunit/include/eunit.hrl").

quick_reply_repo_test_() ->
    isolated_pg_fixture:test(fun() ->
        {timeout, 60, {setup, fun setup/0, fun teardown/1, fun(S) -> [?_test(t_all(S))] end}}
    end).

setup() ->
    PrevPgConf = application:get_env(imboy, pg_conf),
    ok = ensure_pool(),
    Scope = #{
        org_id => eb_pg_test_fixture:id(),
        workspace_id => eb_pg_test_fixture:id(),
        other_org_id => eb_pg_test_fixture:id(),
        other_workspace_id => eb_pg_test_fixture:id(),
        owner_user_id => eb_pg_test_fixture:id(),
        seat_user_id => eb_pg_test_fixture:id(),
        nonmember_user_id => eb_pg_test_fixture:id(),
        creator_id => eb_pg_test_fixture:id()
    },
    %% 建立真实 assignment 事实；缺表/缺列必须在 setup 失败，不能变成 forbidden 假绿。
    ok = elib_pg:with_tx(fun(C) -> create_scope_tx(C, Scope) end),
    Scope#{prev_pg_conf => PrevPgConf}.

teardown(S) ->
    try
        %% 数据清理失败直接使 fixture 失败；after 仍恢复连接池配置。
        ok = elib_pg:with_tx(fun(C) -> cleanup_scope_tx(C, S) end)
    after
        restore_pg_conf(maps:get(prev_pg_conf, S))
    end.

restore_pg_conf(Previous) ->
    case Previous of
        {ok, Prev} when is_map(Prev) ->
            ok = application:set_env(imboy, pg_conf, Prev),
            ok = eunit_runner:ensure_default_pool();
        _ ->
            application:unset_env(imboy, pg_conf)
    end.

create_scope_tx(C, S) ->
    Owner = maps:get(owner_user_id, S),
    Seat = maps:get(seat_user_id, S),
    NonMember = maps:get(nonmember_user_id, S),
    lists:foreach(
        fun(Id) ->
            {ok, 1} = elib_pg:execute(
                C,
                <<
                    "INSERT INTO \"user\"(id,password,account,reg_ip,reg_cosv)"
                    " VALUES ($1,'synthetic-test-only',$2,'127.0.0.1','cs-quick-reply-test')"
                >>,
                [Id, fixture_name(<<"cs-qr-user-">>, Id)]
            )
        end,
        [Owner, Seat, NonMember]
    ),
    lists:foreach(
        fun({Org, Workspace}) ->
            {ok, 1} = elib_pg:execute(
                C,
                <<"INSERT INTO organization(id,name,owner_id,status) VALUES ($1,$2,$3,'active')">>,
                [Org, fixture_name(<<"cs-qr-org-">>, Org), Owner]
            ),
            {ok, 1} = elib_pg:execute(
                C,
                <<
                    "INSERT INTO workspace(id,name,owner_id,status,type,organization_id)"
                    " VALUES ($1,$2,$3,'active','project',$4)"
                >>,
                [Workspace, fixture_name(<<"cs-qr-ws-">>, Workspace), Owner, Org]
            )
        end,
        scope_pairs(S)
    ),
    %% Owner 成员由 00000113 的同步触发器建立；这里只补普通 active 成员。
    {ok, 1} = elib_pg:execute(
        C,
        <<
            "INSERT INTO organization_member(organization_id,user_id,role,status)"
            " VALUES ($1,$2,'member','active')"
        >>,
        [maps:get(org_id, S), Seat]
    ),
    ok.

cleanup_scope_tx(C, S) ->
    Owner = maps:get(owner_user_id, S),
    lists:foreach(
        fun({Org, Workspace}) ->
            {ok, _} = elib_pg:execute(
                C,
                <<"DELETE FROM cs_quick_reply WHERE organization_id=$1 AND workspace_id=$2">>,
                [Org, Workspace]
            ),
            {ok, 1} = elib_pg:execute(
                C,
                <<
                    "DELETE FROM workspace"
                    " WHERE id=$1 AND organization_id=$2 AND owner_id=$3 AND name=$4"
                >>,
                [Workspace, Org, Owner, fixture_name(<<"cs-qr-ws-">>, Workspace)]
            ),
            %% 先删除组织，再由 FK 级联成员；不单独删除仍在任的 owner 成员。
            {ok, 1} = elib_pg:execute(
                C,
                <<"DELETE FROM organization WHERE id=$1 AND owner_id=$2 AND name=$3">>,
                [Org, Owner, fixture_name(<<"cs-qr-org-">>, Org)]
            )
        end,
        scope_pairs(S)
    ),
    lists:foreach(
        fun(Id) ->
            {ok, 1} = elib_pg:execute(
                C,
                <<"DELETE FROM \"user\" WHERE id=$1 AND account=$2">>,
                [Id, fixture_name(<<"cs-qr-user-">>, Id)]
            )
        end,
        [Owner, maps:get(seat_user_id, S), maps:get(nonmember_user_id, S)]
    ),
    ok.

scope_pairs(S) ->
    [
        {maps:get(org_id, S), maps:get(workspace_id, S)},
        {maps:get(other_org_id, S), maps:get(other_workspace_id, S)}
    ].

fixture_name(Prefix, Id) ->
    <<Prefix/binary, (integer_to_binary(Id))/binary>>.

t_all(S) ->
    T = maps:get(org_id, S),
    W = maps:get(workspace_id, S),
    WOther = maps:get(other_workspace_id, S),
    OtherOrg = maps:get(other_org_id, S),
    CreatorId = maps:get(creator_id, S),
    %% 1) 创建两行（sort 排序键 10/0）
    Id0 = cs_tsid:new_id(cs_quick_reply),
    Id1 = cs_tsid:new_id(cs_quick_reply),
    {ok, R0} = cs_pg_quick_reply:with_tx(fun(Cx) ->
        cs_pg_quick_reply:insert_tx(Cx, #{
            id => Id0,
            organization_id => T,
            workspace_id => W,
            title => <<"问候语"/utf8>>,
            content => <<"您好"/utf8>>,
            sort => 10,
            created_by => CreatorId
        })
    end),
    ?assertEqual(Id0, maps:get(<<"id">>, R0)),
    ?assertEqual(10, maps:get(<<"sort">>, R0)),
    ?assertEqual(T, maps:get(<<"organization_id">>, R0)),
    {ok, R1} = cs_pg_quick_reply:with_tx(fun(Cx) ->
        cs_pg_quick_reply:insert_tx(Cx, #{
            id => Id1,
            organization_id => T,
            workspace_id => W,
            title => <<"结束语"/utf8>>,
            content => <<"再见"/utf8>>,
            sort => 0,
            created_by => CreatorId
        })
    end),
    ?assert(is_integer(maps:get(<<"created_at">>, R1))),
    %% 2) 分页排序 sort ASC, id ASC + search 包含匹配
    {ok, Rows, Meta} = cs_pg_quick_reply:with_tx(fun(Cx) ->
        cs_pg_quick_reply:page_tx(Cx, T, W, #{page => 1, size => 10})
    end),
    ?assertEqual([Id1, Id0], [maps:get(<<"id">>, R) || R <- Rows]),
    ?assertEqual(2, maps:get(total, Meta)),
    ?assertEqual(1, maps:get(total_page, Meta)),
    {ok, RowsS, MetaS} = cs_pg_quick_reply:with_tx(fun(Cx) ->
        cs_pg_quick_reply:page_tx(Cx, T, W, #{page => 1, size => 10, search => <<"结束"/utf8>>})
    end),
    ?assertEqual([Id1], [maps:get(<<"id">>, R) || R <- RowsS]),
    ?assertEqual(1, maps:get(total, MetaS)),
    %% 3) 更新全量 + 软删行 not_found
    {ok, U} = cs_pg_quick_reply:with_tx(fun(Cx) ->
        cs_pg_quick_reply:update_tx(Cx, Id0, T, W, #{
            title => <<"问候语2"/utf8>>, content => <<"您好，很高兴为您服务"/utf8>>, sort => 5
        })
    end),
    ?assertEqual(<<"问候语2"/utf8>>, maps:get(<<"title">>, U)),
    ?assertEqual(5, maps:get(<<"sort">>, U)),
    %% 4) 跨 (org, workspace) 不可见：update/delete/find on other workspace
    {ok, deleted} = cs_pg_quick_reply:with_tx(fun(Cx) ->
        cs_pg_quick_reply:soft_delete_tx(Cx, Id0, T, W)
    end),
    %% 跨 (org, workspace) 可见性裁决在 app 层（find 探针+boundary 分支，
    %% 探针行携带 org/workspace 输出）；数据面 update/soft_delete 另携带
    %% org+ws 谓词纵深防御（合同 §4.3/§4.4 的 403/404 由 app 映射）。
    ?assertEqual(
        {error, not_found},
        cs_pg_quick_reply:with_tx(fun(Cx) ->
            cs_pg_quick_reply:update_tx(Cx, Id0, T, W, #{title => <<"x">>, content => <<"y">>})
        end)
    ),
    {ok, PageAfterDelete, _} = cs_pg_quick_reply:with_tx(fun(Cx) ->
        cs_pg_quick_reply:page_tx(Cx, T, W, #{page => 1, size => 10})
    end),
    ?assertEqual([Id1], [maps:get(<<"id">>, R) || R <- PageAfterDelete]),
    %% 5) 软删幂等：重复删除同 ok
    {ok, already_deleted} = cs_pg_quick_reply:with_tx(fun(Cx) ->
        cs_pg_quick_reply:soft_delete_tx(Cx, Id0, T, W)
    end),
    %% 5b) org+ws 谓词纵深防御：申报范围不匹配 → 0 行命中（already_deleted，
    %% 而非误删他行；探针 find 只按 id+未删，此谓词是 update/delete 的第二道门）
    Id2 = cs_tsid:new_id(cs_quick_reply),
    {ok, R2} = cs_pg_quick_reply:with_tx(fun(Cx) ->
        cs_pg_quick_reply:insert_tx(Cx, #{
            id => Id2,
            organization_id => T,
            workspace_id => W,
            title => <<"范围行"/utf8>>,
            content => <<"内容"/utf8>>,
            sort => 0,
            created_by => CreatorId
        })
    end),
    lists:foreach(
        fun({DeclaredOrg, DeclaredWorkspace}) ->
            ?assertEqual(
                {error, not_found},
                cs_pg_quick_reply:with_tx(fun(Cx) ->
                    cs_pg_quick_reply:update_tx(Cx, Id2, DeclaredOrg, DeclaredWorkspace, #{
                        title => <<"must not change">>, content => <<"rejected">>, sort => 99
                    })
                end)
            ),
            ?assertEqual({ok, R2}, find_reply(Id2)),
            ?assertEqual(
                {ok, already_deleted},
                cs_pg_quick_reply:with_tx(fun(Cx) ->
                    cs_pg_quick_reply:soft_delete_tx(Cx, Id2, DeclaredOrg, DeclaredWorkspace)
                end)
            ),
            ?assertEqual({ok, R2}, find_reply(Id2))
        end,
        [{T, WOther}, {OtherOrg, W}]
    ),
    %% 6) 应用用例保留认证创建者，更新/删除继续校验组织和工作区归属。
    t_app_crud(T, W, OtherOrg, WOther, CreatorId),
    %% 7) app 层：trim 校验 + seat 投影/assignment 门
    {error, invalid_argument} = cs_quick_reply_app:create(#{
        organization_id => T,
        workspace_id => W,
        adm_user_id => CreatorId,
        title => <<"   ">>,
        content => <<"正文"/utf8>>
    }),
    {error, invalid_argument} = cs_quick_reply_app:create(#{
        organization_id => T,
        workspace_id => W,
        adm_user_id => CreatorId,
        title => <<"标题"/utf8>>,
        content => binary:copy(<<"x">>, 2001)
    }),
    {error, invalid_argument} = cs_quick_reply_app:page(#{
        organization_id => T, page => 0, workspace_id => W
    }),
    t_seat_list(S, [R1, R2]),
    ok.

t_app_crud(OrgId, WorkspaceId, OtherOrgId, OtherWorkspaceId, CreatorId) ->
    {ok, #{quick_reply := Created}} = customer_service_facade:quick_reply_create(OrgId, #{
        workspace_id => WorkspaceId,
        adm_user_id => CreatorId,
        title => <<"  问候语  "/utf8>>,
        content => <<"  您好  "/utf8>>,
        sort => -5
    }),
    Id = maps:get(id, Created),
    ?assertEqual(CreatorId, maps:get(created_by, Created)),
    ?assertEqual(OrgId, maps:get(organization_id, Created)),
    ?assertEqual(<<"问候语"/utf8>>, maps:get(title, Created)),
    ?assertEqual(-5, maps:get(sort, Created)),
    {ok, Stored} = cs_pg_quick_reply:with_tx(fun(C) -> cs_pg_quick_reply:find_tx(C, Id) end),
    ?assertEqual(CreatorId, maps:get(<<"created_by">>, Stored)),
    Update = #{id => Id, workspace_id => WorkspaceId, title => <<"changed">>, content => <<"body">>},
    ?assertEqual(
        {error, organization_boundary_violation},
        customer_service_facade:quick_reply_update(OrgId, Update#{workspace_id => OtherWorkspaceId})
    ),
    ?assertEqual({ok, Stored}, find_reply(Id)),
    ?assertEqual(
        {error, organization_boundary_violation},
        customer_service_facade:quick_reply_delete(OtherOrgId, Update)
    ),
    ?assertEqual({ok, Stored}, find_reply(Id)),
    {ok, #{quick_reply := Updated}} = customer_service_facade:quick_reply_update(OrgId, Update),
    ?assertEqual(CreatorId, maps:get(created_by, Updated)),
    ?assertEqual(<<"changed">>, maps:get(title, Updated)),
    ?assertEqual(0, maps:get(sort, Updated)),
    {ok, #{ok := true}} = customer_service_facade:quick_reply_delete(OrgId, Update),
    {ok, #{ok := true}} = customer_service_facade:quick_reply_delete(OrgId, Update),
    ?assertEqual({error, not_found}, customer_service_facade:quick_reply_update(OrgId, Update)).

find_reply(Id) ->
    cs_pg_quick_reply:with_tx(fun(C) -> cs_pg_quick_reply:find_tx(C, Id) end).

t_seat_list(S, StoredRows) ->
    Org = maps:get(org_id, S),
    Workspace = maps:get(workspace_id, S),
    OtherWorkspace = maps:get(other_workspace_id, S),
    Seat = maps:get(seat_user_id, S),
    NonMember = maps:get(nonmember_user_id, S),
    ?assertEqual({true, true}, cs_pg_quick_reply:seat_assignment_facts(Org, Workspace, Seat)),
    Params = #{workspace_id => Workspace, seat_user_id => Seat},
    {ok, #{list := Rows}} = customer_service_facade:quick_reply_seat_list(Org, Params),
    Expected = [
        #{
            id => maps:get(<<"id">>, Row),
            title => maps:get(<<"title">>, Row),
            content => maps:get(<<"content">>, Row),
            sort => maps:get(<<"sort">>, Row)
        }
     || Row <- StoredRows
    ],
    %% 不依赖数据库对中文 title 的 collation；逐行精确投影排除管理字段泄漏。
    ?assertEqual(lists:sort(Expected), lists:sort(Rows)),
    lists:foreach(
        fun(Row) -> ?assertEqual([content, id, sort, title], lists:sort(maps:keys(Row))) end,
        Rows
    ),
    ?assertEqual(
        {true, false}, cs_pg_quick_reply:seat_assignment_facts(Org, OtherWorkspace, Seat)
    ),
    ?assertEqual(
        {error, forbidden},
        customer_service_facade:quick_reply_seat_list(Org, Params#{workspace_id => OtherWorkspace})
    ),
    ?assertEqual({false, true}, cs_pg_quick_reply:seat_assignment_facts(Org, Workspace, NonMember)),
    ?assertEqual(
        {error, forbidden},
        customer_service_facade:quick_reply_seat_list(Org, Params#{seat_user_id => NonMember})
    ).

%% 独立 VM 使用当前测试配置；T05 变量只覆盖显式提供的字段。
ensure_pool() ->
    %% 本套件经 isolated_pg_fixture:test 在 peer VM 内执行：peer 只继承
    %% -config 与 code path，父 VM 的 pgsql 池不随 peer 走。quality 车道
    %% （IMBOY_TEST_CONFIG 注入 worker config）的 -config pg_conf 是正确的
    %% 克隆库连接（不做 T05 覆盖——空密码泄漏源，full-20261104T081330Z
    %% invalid_password/no_members 连环根因），池缺失时按它就地重建；
    %% 本地单跑（无该 env）仍走 T05 直跑形态。
    case pool_ready() of
        true ->
            ok;
        false ->
            case os:getenv("IMBOY_TEST_CONFIG") of
                false -> ensure_pool_t05();
                _ -> ok = eunit_runner:ensure_default_pool()
            end
    end,
    %% peer VM 不继承父 VM 的 tsid 状态（full-20261104T161607Z 实证：
    %% elib_tsid_not_initialized）。init 幂等，T05 分支已含同款。
    catch elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3}),
    ok.

pool_ready() ->
    _ = code:ensure_loaded(pooler),
    try pooler:take_member(pgsql, 100) of
        Pid when is_pid(Pid) ->
            pooler:return_member(pgsql, Pid, ok),
            true;
        _ ->
            false
    catch
        _:_ -> false
    end.

ensure_pool_t05() ->
    catch elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3}),
    _ = code:ensure_loaded(pooler),
    {ok, _} = application:ensure_all_started(pooler),
    _ = application:load(imboy),
    {ok, Base} = application:get_env(imboy, pg_conf),
    {epgsql, connect, [Opts]} = maps:get(start_mfa, Base),
    Overrides = [
        {"T05_PG_HOST", host},
        {"T05_PG_USER", username},
        {"T05_PG_PASSWORD", password},
        {"T05_PG_DB", database}
    ],
    Selected = lists:foldl(
        fun({Env, Key}, Acc) ->
            case os:getenv(Env) of
                false -> Acc;
                Value -> Acc#{Key => Value}
            end
        end,
        Opts,
        Overrides
    ),
    Connect =
        case os:getenv("T05_PG_PORT") of
            false -> Selected;
            Port -> Selected#{port => list_to_integer(Port)}
        end,
    Conf = Base#{
        name => pgsql,
        init_count => 1,
        max_count => 2,
        start_mfa => {epgsql, connect, [Connect]}
    },
    ok = application:set_env(imboy, pg_conf, Conf),
    catch pooler:rm_pool(pgsql),
    {ok, _} = pooler:new_pool(Conf),
    ok.
