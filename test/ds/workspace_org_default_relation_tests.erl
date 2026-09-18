-module(workspace_org_default_relation_tests).

%% ORG-05 workspace 侧镜像测试（Core Contract C05）：
%%   * create_template：首个 Org Workspace 创建时同事务设默认（钩子恰调一次）；
%%   * 个人域（organization_id=undefined）创建：钩子以 undefined 直通（不动作）；
%%   * 幂等命中（existing）：不触碰默认关系（「首个」语义不被幂等路径伪造）；
%%   * archive / admin_archive：同事务触发默认交接钩子（Org 归属行内回读）。
%% 真库行为（并发恰一/触发器/backfill 等值）由 behavior harness 在一次性 PG 覆盖。

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

-define(OWNER, 900001).
-define(ORG_ID, 700001).
-define(WS_ID, 800001).
-define(GID, 777001).
-define(CID, 666001).
-define(ADM_UID, 910001).

tx_fun() ->
    fun(Fun) ->
        try
            Fun(fake_conn)
        catch
            throw:{abort_tx, Reason} -> {error, Reason}
        end
    end.

norm_sql(Sql) when is_binary(Sql) ->
    binary:replace(Sql, <<"public.">>, <<>>, [global]);
norm_sql(Sql) ->
    norm_sql(iolist_to_binary(Sql)).

tx_query(Sql0) ->
    case norm_sql(Sql0) of
        <<"SELECT id FROM channel">> ->
            {ok, [#{<<"id">> => ?CID}]};
        <<"SELECT id FROM \"group\"">> ->
            {ok, [#{<<"id">> => ?GID}]};
        <<"SELECT id, name, logo, owner_id, organization_id, status, branding, created_at",
            " FROM workspace WHERE id = $1">> ->
            {ok, [workspace_readback_row()]};
        <<"SELECT organization_id FROM workspace WHERE id = $1">> ->
            {ok, [#{<<"organization_id">> => get(t_org_db_id)}]};
        _ ->
            {ok, []}
    end.

workspace_readback_row() ->
    #{
        <<"id">> => ?WS_ID,
        <<"name">> => <<"Team WS">>,
        <<"logo">> => null,
        <<"owner_id">> => ?OWNER,
        <<"organization_id">> => get(t_org_db_id),
        <<"status">> => <<"active">>,
        <<"branding">> => <<"{}">>,
        <<"created_at">> => 0
    }.

%% create_template 全链 mock（对齐 workspace_template_tests 的最小集）
create_mocks() ->
    [
        {workspace_repo, [
            {'add', 2, fun(_Conn, Data) ->
                put(t_org_db_id, maps:get(<<"organization_id">>, Data)),
                {ok, ?WS_ID}
            end},
            {'find_by_owner_and_name', 4, fun(_, _, _, _) -> #{} end},
            {'find_by_request_id', 4, fun(_, _, _, _) -> #{} end},
            {'count_by_owner', 1, fun(_) -> 0 end}
        ]},
        {organization_member_repo, [
            {'find_organization_for_share_tx', 3, fun(_, ?ORG_ID, <<"id,status">>) ->
                {ok, #{<<"id">> => ?ORG_ID, <<"status">> => <<"active">>}}
            end},
            {'find_active_for_share_tx', 4, fun(_, ?ORG_ID, ?OWNER, <<"role">>) ->
                {ok, #{<<"role">> => <<"owner">>}}
            end}
        ]},
        {workspace_member_repo, [
            {'insert_member_tx', 3, fun(_Conn, _WsId, _Data) -> ok end}
        ]},
        {group_member_ds, [
            {'join_group', 5, fun(_Conn, _Mode, _Uid, _Gid, _Opt) -> {ok, ?OWNER} end}
        ]},
        {channel_admin_repo, [
            {'add', 2, fun(_Conn, _Data) -> {ok, ?CID} end}
        ]},
        {channel_subscription_repo, [
            {'upsert_active', 3, fun(_Conn, _CId, _Uid) -> {ok, ok} end}
        ]},
        {group_member_repo, [
            {'find', 3, fun(_, _, _) -> #{} end}
        ]},
        {elib_pg, [
            {'with_tx', 1, tx_fun()},
            {'query', 2, fun(Sql, _Params) -> tx_query(Sql) end},
            {'query', 3, fun(_Conn, Sql, _Params) -> tx_query(Sql) end},
            {'execute', 3, fun(_Conn, _Sql, _Params) -> {ok, 1} end}
        ]},
        {elib_tsid, [
            {'generate', 1, fun
                (group_info) -> ?GID;
                (channel) -> ?CID
            end}
        ]},
        {organization_default_workspace_app, [
            {'ensure_first_workspace_tx', 3, fun(_Conn, OrgId, WsId) ->
                put(t_first_ws_calls, bump(t_first_ws_calls)),
                put(t_first_ws_args, {OrgId, WsId}),
                ok
            end},
            {'replace_or_clear_on_archive_tx', 3, fun(_Conn, OrgId, WsId) ->
                put(t_handover_args, {OrgId, WsId}),
                ok
            end}
        ]}
    ].

bump(Key) ->
    case get(Key) of
        undefined -> 1;
        N -> N + 1
    end.

with_create_mocks(TestFun) ->
    lists:foreach(
        fun({Module, Expectations}) ->
            {ok, _} = meck_helper:setup_mock(Module, Expectations)
        end,
        create_mocks()
    ),
    try
        TestFun()
    after
        lists:foreach(
            fun({Module, _}) -> meck_helper:cleanup_mock(Module) end,
            create_mocks()
        ),
        [erase(K) || K <- [t_org_db_id, t_first_ws_calls, t_first_ws_args, t_handover_args]]
    end.

create_sets_first_default_test_() ->
    [
        {"org workspace creation sets default in the same tx (hook exactly once)", fun() ->
            with_create_mocks(fun() ->
                ?assertMatch(
                    {ok, #{workspace_id := ?WS_ID}, created},
                    workspace_ds:create_template(?OWNER, ?ORG_ID, <<"Team WS">>, undefined)
                ),
                ?assertEqual(1, get(t_first_ws_calls)),
                {?ORG_ID, ?WS_ID} = get(t_first_ws_args)
            end)
        end},
        {"personal workspace creation passes undefined through the hook", fun() ->
            with_create_mocks(fun() ->
                ?assertMatch(
                    {ok, #{workspace_id := ?WS_ID}, created},
                    workspace_ds:create_template(?OWNER, undefined, <<"Personal WS">>, undefined)
                ),
                {undefined, ?WS_ID} = get(t_first_ws_args)
            end)
        end},
        {"idempotent hit does not touch the default relation", fun() ->
            with_create_mocks(fun() ->
                %% 语义键命中 → existing → 钩子不得触发（非「首个」路径）
                meck:expect(workspace_repo, find_by_request_id, 4, fun(_, _, _, _) ->
                    #{<<"id">> => ?WS_ID, <<"organization_id">> => ?ORG_ID}
                end),
                ?assertMatch(
                    {ok, _, existing},
                    workspace_ds:create_template(?OWNER, ?ORG_ID, <<"Team WS">>, <<"req-1">>)
                ),
                ?assertEqual(undefined, get(t_first_ws_calls))
            end)
        end}
    ].

archive_hands_over_default_test_() ->
    [
        {"archive reads org scope in-tx and invokes handover hook", fun() ->
            run_archive_mocks(?ORG_ID, fun() ->
                ?assertMatch(
                    {ok, #{workspace_id := ?WS_ID, status := <<"archived">>}},
                    workspace_logic:archive(?OWNER, ?WS_ID)
                ),
                ?assertEqual({?ORG_ID, ?WS_ID}, get(t_handover_args))
            end)
        end},
        {"personal workspace archive passes undefined (no handover)", fun() ->
            run_archive_mocks(null, fun() ->
                ?assertMatch(
                    {ok, #{workspace_id := ?WS_ID, status := <<"archived">>}},
                    workspace_logic:archive(?OWNER, ?WS_ID)
                ),
                ?assertEqual({undefined, ?WS_ID}, get(t_handover_args))
            end)
        end},
        {"admin archive invokes the same handover hook", fun() ->
            run_archive_mocks(?ORG_ID, fun() ->
                ?assertMatch(
                    {ok, #{workspace_id := ?WS_ID, status := <<"archived">>, archived_by := null}},
                    workspace_logic:admin_archive(?ADM_UID, ?WS_ID)
                ),
                ?assertEqual({?ORG_ID, ?WS_ID}, get(t_handover_args))
            end)
        end}
    ].

%% archive 链路最小 mock：归属 Org 由 tx_query 的 organization_id 子句供给
run_archive_mocks(OrgDbId, TestFun) ->
    Mocks = [
        {workspace_ds, [
            {'find_by_id', 1, fun(_) ->
                #{<<"id">> => ?WS_ID, <<"owner_id">> => ?OWNER, <<"status">> => <<"active">>}
            end},
            %% admin_archive 走 2 元入口（id 列存在性检查）
            {'find_by_id', 2, fun(?WS_ID, <<"id">>) ->
                #{<<"id">> => ?WS_ID}
            end}
        ]},
        {workspace_member_repo, [
            {'find', 3, fun(?WS_ID, U, _) ->
                case U of
                    ?OWNER -> #{<<"role">> => <<"owner">>, <<"status">> => <<"active">>};
                    _ -> #{<<"role">> => <<"member">>, <<"status">> => <<"active">>}
                end
            end}
        ]},
        {elib_pg, [
            {'with_tx', 1, tx_fun()},
            {'query', 3, fun(_Conn, Sql, _Params) ->
                case norm_sql(Sql) of
                    <<"SELECT organization_id FROM workspace WHERE id = $1">> ->
                        {ok, [#{<<"organization_id">> => OrgDbId}]};
                    _ ->
                        {ok, []}
                end
            end},
            {'execute', 3, fun(_Conn, _Sql, _Params) -> {ok, 1} end}
        ]},
        {organization_default_workspace_app, [
            {'replace_or_clear_on_archive_tx', 3, fun(_Conn, OrgId, WsId) ->
                put(t_handover_args, {OrgId, WsId}),
                ok
            end}
        ]}
    ],
    lists:foreach(
        fun({Module, Expectations}) ->
            {ok, _} = meck_helper:setup_mock(Module, Expectations)
        end,
        Mocks
    ),
    try
        TestFun()
    after
        lists:foreach(
            fun({Module, _}) -> meck_helper:cleanup_mock(Module) end,
            Mocks
        ),
        erase(t_org_db_id),
        erase(t_handover_args)
    end.
