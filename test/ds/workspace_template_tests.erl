-module(workspace_template_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% 双体验 v2.5.2 WP3/T4 — workspace_ds:create_template/4 单元测试
%%% 覆盖：Template 原子性（故障注入回滚，I13）、request_id 幂等、
%%% 语义键幂等、Template 资源齐全（Workspace+Owner member+General 群
%%% +Announcements 频道+创建者群成员/频道管理员/订阅者关系）。
%%%
%%% ⚠️ 哨兵与断言形态约定：TestFun 必须是单表达式（`fun() -> helper() end`），
%%% {Desc, fun} 包装会被 ?_test 吞掉静默空转；哨兵经进程字典传递
%%% （eunit generator 与用例执行异进程，Self 消息收不到）。

-define(OWNER, 900001).
-define(ORG_ID, 700001).
-define(WS_ID, 800001).
-define(GID, 777001).
-define(CID, 666001).

-define(SENTINEL_KEYS, [
    t_ws_add,
    t_owner_member_insert,
    t_general_join,
    t_channel_admin_add,
    t_channel_subscribe,
    t_re_create
]).

%% 模拟 elib_pg:with_tx 事务语义：Fun(Conn) 内 throw({abort_tx, R}) → {error, R}
tx_mock_extra() ->
    {elib_pg, [
        {'with_tx', 1, fun(Fun) ->
            try
                Fun(fake_conn)
            catch
                throw:{abort_tx, Reason} -> {error, Reason}
            end
        end},
        {'query', 2, fun(Sql, _Params) -> tx_query(Sql) end},
        {'query', 3, fun(_Conn, Sql, _Params) -> tx_query(Sql) end},
        {'execute', 3, fun(_Conn, _Sql, _Params) -> {ok, 1} end}
    ]}.

%% 全量跑时 sql_driver=pgsql，表名带 public. 前缀；匹配前归一
norm_sql(Sql) when is_binary(Sql) ->
    binary:replace(Sql, <<"public.">>, <<>>, [global]);
norm_sql(Sql) ->
    norm_sql(iolist_to_binary(Sql)).

tx_query(Sql) when is_binary(Sql) ->
    tx_query_norm(norm_sql(Sql));
tx_query(Sql) ->
    tx_query_norm(iolist_to_binary(Sql)).

tx_query_norm(<<"SELECT id FROM channel">>) ->
    {ok, [#{<<"id">> => ?CID}]};
tx_query_norm(<<"SELECT id FROM \"group\"">>) ->
    {ok, [#{<<"id">> => ?GID}]};
%% do_create_template 末尾的同事务回读（ZC-09 M-5 引入，原 mock 写于其前）
tx_query_norm(
    <<
        "SELECT id, name, logo, owner_id, organization_id, status, branding, created_at"
        " FROM workspace WHERE id = $1"
    >>
) ->
    OrganizationId =
        case get(t_ws_add) of
            {Value, _OwnerUid} -> Value;
            _ -> ?ORG_ID
        end,
    {ok, [
        #{
            <<"id">> => ?WS_ID,
            <<"name">> => <<"Team WS">>,
            <<"logo">> => null,
            <<"owner_id">> => ?OWNER,
            <<"organization_id">> => OrganizationId,
            <<"status">> => <<"active">>,
            <<"branding">> => <<"{}">>,
            <<"created_at">> => 0
        }
    ]};
tx_query_norm(_) ->
    {ok, []}.

happy_path_mocks(Fault) ->
    [
        {workspace_repo, [
            {'add', 2, fun(_Conn, Data) ->
                put(
                    t_ws_add,
                    {maps:get(<<"organization_id">>, Data), maps:get(<<"owner_id">>, Data)}
                ),
                {ok, ?WS_ID}
            end},
            {'find_by_owner_and_name', 4, fun(_, _, _, _) -> #{} end},
            {'find_by_request_id', 4, fun(_, _, _, _) -> #{} end},
            {'count_by_owner', 1, fun(_) -> 0 end},
            {'find_by_id', 2, fun(_, _) ->
                #{<<"id">> => ?WS_ID, <<"branding">> => <<"{}">>}
            end}
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
            {'insert_member_tx', 3, fun(_Conn, WsId, Data) ->
                put(t_owner_member_insert, {WsId, maps:get(<<"role">>, Data)}),
                ok
            end}
        ]},
        {group_member_ds, [
            {'join_group', 5, fun(_Conn, _Mode, Uid, Gid, Opt) ->
                put(t_general_join, {Uid, Gid, maps:get(role, Opt, 1)}),
                {ok, Uid}
            end}
        ]},
        {channel_admin_repo, [
            {'add', 2, fun(_Conn, Data) ->
                put(t_channel_admin_add, maps:get(<<"channel_id">>, Data)),
                case Fault of
                    {fail_at, channel_admin} -> {error, injected_failure};
                    _ -> {ok, ?CID}
                end
            end}
        ]},
        {channel_subscription_repo, [
            {'upsert_active', 3, fun(_Conn, CId, Uid) ->
                put(t_channel_subscribe, {CId, Uid}),
                {ok, ok}
            end}
        ]},
        {group_member_repo, [
            {'find', 3, fun(_, _, _) -> #{} end}
        ]},
        tx_mock_extra(),
        {elib_tsid, [
            {'generate', 1, fun
                (group_info) -> ?GID;
                (channel) -> ?CID
            end}
        ]}
    ].

reset_sentinels() ->
    [erase(K) || K <- ?SENTINEL_KEYS],
    ok.

%% ===================================================================
%% Template 资源齐全（I13）
%% ===================================================================

template_creates_all_resources_test_() ->
    ?WITH_MECKS(happy_path_mocks(none), fun() -> template_creates_all_resources_body() end).

template_creates_all_resources_body() ->
    begin
        reset_sentinels(),
        ?assertMatch(
            {ok,
                #{
                    workspace_id := ?WS_ID,
                    channel_id := ?CID,
                    group_id := ?GID
                },
                created},
            workspace_ds:create_template(?OWNER, ?ORG_ID, <<"Team WS">>, <<"req-1">>)
        ),
        ?assert({?ORG_ID, ?OWNER} =:= get(t_ws_add), "workspace row missing organization"),
        ?assert(
            {?WS_ID, <<"owner">>} =:= get(t_owner_member_insert),
            "owner workspace_member missing"
        ),
        ?assert({?OWNER, ?GID, 4} =:= get(t_general_join), "General group member missing"),
        ?assert(?CID =:= get(t_channel_admin_add), "Announcements admin missing"),
        ?assert({?CID, ?OWNER} =:= get(t_channel_subscribe), "Announcements subscription missing"),
        reset_sentinels(),
        ok
    end.

personal_workspace_keeps_organization_null_test_() ->
    ?WITH_MECKS(
        happy_path_mocks(none),
        fun() ->
            reset_sentinels(),
            ?assertMatch(
                {ok, #{workspace := #{<<"organization_id">> := null}}, created},
                workspace_ds:create_template(?OWNER, undefined, <<"Personal WS">>, <<"personal-1">>)
            ),
            ?assertEqual({null, ?OWNER}, get(t_ws_add)),
            reset_sentinels(),
            ok
        end
    ).

%% ===================================================================
%% Template 原子性：故障注入回滚（I13：任一步失败全部回滚）
%% ===================================================================

template_fault_injection_rolls_back_test_() ->
    ?WITH_MECKS(
        happy_path_mocks({fail_at, channel_admin}),
        fun() -> template_fault_injection_rolls_back_body() end
    ).

template_fault_injection_rolls_back_body() ->
    begin
        reset_sentinels(),
        ?assertMatch(
            {error, {channel_admin_create_failed, injected_failure}},
            workspace_ds:create_template(?OWNER, ?ORG_ID, <<"Team WS">>, <<"req-1">>)
        ),
        %% with_tx 未正常返回：事务被 abort_tx 回滚（workspace 行不会提交）。
        %% 原实现此处为两个 after-0 排空 receive（无断言语义），等价排空哨兵。
        reset_sentinels(),
        ok
    end.

%% ===================================================================
%% request_id 幂等：重复请求不产生重复资源
%% ===================================================================

request_id_idempotent_test_() ->
    ?WITH_MECKS(happy_path_mocks(none), fun() -> request_id_idempotent_body() end).

request_id_idempotent_body() ->
    begin
        reset_sentinels(),
        Existing = #{
            <<"id">> => ?WS_ID,
            <<"name">> => <<"Team WS">>,
            <<"owner_id">> => ?OWNER,
            <<"organization_id">> => ?ORG_ID,
            <<"status">> => <<"active">>,
            <<"branding">> => <<"{\"_request_id\":\"req-42\"}">>
        },
        %% 首次创建
        ?assertMatch(
            {ok, _, created},
            workspace_ds:create_template(?OWNER, ?ORG_ID, <<"Team WS">>, <<"req-42">>)
        ),
        ?assert({?ORG_ID, ?OWNER} =:= get(t_ws_add), "first create must insert workspace row"),
        %% 二次请求命中幂等标记
        meck(workspace_repo, [
            {'find_by_request_id', 4, fun(?ORG_ID, ?OWNER, <<"req-42">>, _) -> Existing end},
            {'find_by_owner_and_name', 4, fun(_, _, _, _) -> #{} end},
            {'count_by_owner', 1, fun(_) -> 0 end},
            {'find_by_id', 2, fun(_, _) -> Existing end},
            {'add', 2, fun(_, _) ->
                put(t_re_create, true),
                {ok, 0}
            end}
        ]),
        ?assertMatch(
            {ok, _, existing},
            workspace_ds:create_template(?OWNER, ?ORG_ID, <<"Team WS">>, <<"req-42">>)
        ),
        ?assert(undefined =:= get(t_re_create), "must not re-create workspace"),
        reset_sentinels(),
        ok
    end.

%% ===================================================================
%% 语义键幂等：同 Owner + 同名 active 工作区直接返回既有
%% ===================================================================

semantic_key_idempotent_test_() ->
    ?WITH_MECKS(happy_path_mocks(none), fun() -> semantic_key_idempotent_body() end).

semantic_key_idempotent_body() ->
    begin
        reset_sentinels(),
        Existing = #{
            <<"id">> => ?WS_ID,
            <<"name">> => <<"Team WS">>,
            <<"owner_id">> => ?OWNER,
            <<"organization_id">> => ?ORG_ID,
            <<"status">> => <<"active">>,
            <<"branding">> => <<"{}">>
        },
        meck(workspace_repo, [
            {'find_by_owner_and_name', 4, fun(?ORG_ID, ?OWNER, <<"Team WS">>, _) -> Existing end},
            {'find_by_request_id', 4, fun(_, _, _, _) -> #{} end},
            {'count_by_owner', 1, fun(_) -> 0 end},
            {'find_by_id', 2, fun(_, _) -> Existing end},
            {'add', 2, fun(_, _) ->
                put(t_re_create, true),
                {ok, 0}
            end}
        ]),
        ?assertMatch(
            {ok, #{workspace_id := ?WS_ID}, existing},
            workspace_ds:create_template(?OWNER, ?ORG_ID, <<"Team WS">>, undefined)
        ),
        ?assert(undefined =:= get(t_re_create), "must not re-create workspace"),
        reset_sentinels(),
        ok
    end.

%% ===================================================================
%% 创建上限
%% ===================================================================

owner_workspace_limit_test_() ->
    ?WITH_MECKS(happy_path_mocks(none), fun() -> owner_workspace_limit_body() end).

owner_workspace_limit_body() ->
    begin
        reset_sentinels(),
        meck(workspace_repo, [
            {'count_by_owner', 1, fun(_) -> 100 end},
            {'find_by_request_id', 4, fun(_, _, _, _) -> #{} end},
            {'find_by_owner_and_name', 4, fun(_, _, _, _) -> #{} end}
        ]),
        ?assertMatch(
            {error, owner_workspace_limit},
            workspace_ds:create_template(?OWNER, ?ORG_ID, <<"Team WS">>, <<"req-x">>)
        ),
        reset_sentinels(),
        ok
    end.

organization_scope_guards_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'with_tx', 1, fun(Fun) ->
                    try Fun(fake_conn) of
                        Result -> Result
                    catch
                        throw:{abort_tx, Reason} -> {error, Reason}
                    end
                end}
            ]},
            {organization_member_repo, [
                {'find_organization_for_share_tx', 3, fun(_, ?ORG_ID, <<"id,status">>) ->
                    case get(t_organization_scope_case) of
                        archived -> {ok, #{<<"status">> => <<"archived">>}};
                        missing -> {error, not_found};
                        member -> {ok, #{<<"status">> => <<"active">>}}
                    end
                end},
                {'find_active_for_share_tx', 4, fun(_, ?ORG_ID, ?OWNER, <<"role">>) ->
                    {ok, #{<<"role">> => <<"member">>}}
                end}
            ]}
        ],
        fun() ->
            put(t_organization_scope_case, archived),
            ?assertEqual(
                {error, organization_archived},
                workspace_ds:create_template(?OWNER, ?ORG_ID, <<"WS">>, undefined)
            ),
            put(t_organization_scope_case, missing),
            ?assertEqual(
                {error, organization_not_found},
                workspace_ds:create_template(?OWNER, ?ORG_ID, <<"WS">>, undefined)
            ),
            put(t_organization_scope_case, member),
            ?assertEqual(
                {error, organization_create_forbidden},
                workspace_ds:create_template(?OWNER, ?ORG_ID, <<"WS">>, undefined)
            ),
            erase(t_organization_scope_case),
            ok
        end
    ).

standard_workspace_reads_include_organization_id_test_() ->
    ?WITH_MECKS(
        [
            {workspace_repo, [
                {'find_by_id', 2, fun(?WS_ID, Columns) ->
                    ?assertNotEqual(nomatch, binary:match(Columns, <<"organization_id">>)),
                    #{<<"id">> => ?WS_ID, <<"organization_id">> => ?ORG_ID}
                end},
                {'page_by_member', 4, fun(?OWNER, 1, 10, Columns) ->
                    ?assertNotEqual(nomatch, binary:match(Columns, <<"w.organization_id">>)),
                    {ok, #{list => [#{<<"organization_id">> => ?ORG_ID}]}}
                end}
            ]}
        ],
        fun() ->
            ?assertMatch(
                #{<<"organization_id">> := ?ORG_ID}, workspace_ds:find_by_id(?WS_ID)
            ),
            ?assertMatch(
                {ok, #{list := [#{<<"organization_id">> := ?ORG_ID}]}},
                workspace_ds:page_by_member(?OWNER, 1, 10)
            )
        end
    ).

%%%===================================================================
%%% Internal
%%%===================================================================

-spec meck(atom(), list()) -> ok.
meck(Module, Expectations) ->
    {ok, _} = meck_helper:setup_mock(Module, Expectations),
    ok.
