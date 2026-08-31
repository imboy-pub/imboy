-module(workspace_admin_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("error_code.hrl").

%%% 双体验 v2.5.2 WP7/T11b — workspace_logic admin 函数单元测试
%%% 覆盖：admin_page（分页 + 批量资源计数挂载）、admin_detail（404 / owner +
%%% 工作区成员 + 资源清单）、admin_member_page、admin_archive / admin_restore
%%% （平台侧无 Owner 校验、审计列、409 语义），以及**归档联动集成断言**：
%%% admin_archive 成功后 workspace_guard:ensure_writable 对 workspace 资源
%%% 返回稳定错误码 980（T7 写守卫与运营归档联动）。

-define(WS_ID, 800001).
-define(OWNER, 900001).
-define(PROJECT_ID, 700001).
-define(ADM_UID, 42).

tx_fun() ->
    fun(Fun) ->
        try
            Fun(fake_conn)
        catch
            throw:{abort_tx, Reason} -> {error, Reason}
        end
    end.

ws_exists_mocks() ->
    {workspace_ds, [
        {'find_by_id', 2, fun(?WS_ID, <<"id">>) -> #{<<"id">> => ?WS_ID} end}
    ]}.

%%% ===================================================================
%%% admin_page —— 分页 + 批量资源计数
%%% ===================================================================

admin_page_attaches_counts_test_() ->
    WsRow = #{
        <<"id">> => ?WS_ID,
        <<"name">> => <<"ops-ws">>,
        <<"owner_id">> => ?OWNER,
        <<"status">> => <<"active">>,
        <<"owner_nickname">> => <<"alice">>
    },
    ?WITH_MECKS(
        [
            {workspace_ds, [
                {'admin_page', 4, fun(1, 10, all, <<>>) ->
                    {ok, #{list => [WsRow], page => 1, size => 10, total => 1, total_page => 1}}
                end},
                {'admin_batch_resource_counts', 1, fun([?WS_ID]) ->
                    #{
                        <<"project_count">> => #{?WS_ID => 3},
                        <<"group_count">> => #{?WS_ID => 2},
                        <<"channel_count">> => #{?WS_ID => 1},
                        <<"member_count">> => #{?WS_ID => 5}
                    }
                end}
            ]}
        ],
        fun() ->
            {ok, Result} = workspace_logic:admin_page(1, 10, <<"all">>, <<>>),
            ?assertEqual(1, maps:get(total, Result)),
            [Row | _] = maps:get(list, Result),
            ?assertEqual(3, maps:get(<<"project_count">>, Row)),
            ?assertEqual(2, maps:get(<<"group_count">>, Row)),
            ?assertEqual(1, maps:get(<<"channel_count">>, Row)),
            ?assertEqual(5, maps:get(<<"member_count">>, Row))
        end
    ).

admin_page_empty_list_no_count_query_test_() ->
    ?WITH_MECKS(
        [
            {workspace_ds, [
                {'admin_page', 4, fun(_P, _S, _St, _K) ->
                    {ok, #{list => [], page => 1, size => 10, total => 0, total_page => 0}}
                end},
                {'admin_batch_resource_counts', 1, fun([]) -> erlang:error(should_not_count) end}
            ]}
        ],
        fun() ->
            {ok, Result} = workspace_logic:admin_page(1, 10, <<"all">>, <<>>),
            ?assertEqual([], maps:get(list, Result))
        end
    ).

%%% ===================================================================
%%% admin_detail —— 404 / 组装
%%% ===================================================================

admin_detail_not_found_test_() ->
    ?WITH_MECKS(
        [
            {workspace_ds, [
                {'find_by_id', 1, fun(?WS_ID) -> #{} end}
            ]}
        ],
        fun() ->
            ?assertMatch(
                {error, {404, _}}, workspace_logic:admin_detail(?WS_ID)
            )
        end
    ).

admin_detail_assembly_test_() ->
    ?WITH_MECKS(
        [
            {workspace_ds, [
                {'find_by_id', 1, fun(?WS_ID) ->
                    #{
                        <<"id">> => ?WS_ID,
                        <<"name">> => <<"ops-ws">>,
                        <<"owner_id">> => ?OWNER,
                        <<"status">> => <<"active">>
                    }
                end},
                {'admin_resource_list', 3, fun
                    (project, ?WS_ID, 20) ->
                        {ok, [#{<<"id">> => ?PROJECT_ID, <<"name">> => <<"p1">>}]};
                    (group, ?WS_ID, 20) ->
                        {ok, []};
                    (channel, ?WS_ID, 20) ->
                        {ok, []}
                end}
            ]},
            {user_ds, [
                {'find_by_id', 2, fun(?OWNER, _Col) ->
                    #{<<"id">> => ?OWNER, <<"nickname">> => <<"alice">>}
                end}
            ]},
            {workspace_member_repo, [
                {'page_by_workspace', 4, fun(?WS_ID, 1, 20, _Col) ->
                    {ok, #{
                        list => [#{<<"user_id">> => ?OWNER, <<"role">> => <<"owner">>}],
                        page => 1,
                        size => 20,
                        total => 1,
                        total_page => 1
                    }}
                end}
            ]}
        ],
        fun() ->
            {ok, Detail} = workspace_logic:admin_detail(?WS_ID),
            ?assertEqual(<<"ops-ws">>, maps:get(<<"name">>, Detail)),
            ?assertEqual(
                #{<<"id">> => ?OWNER, <<"nickname">> => <<"alice">>}, maps:get(owner, Detail)
            ),
            ?assertMatch(
                [#{<<"role">> := <<"owner">>} | _], maps:get(list, maps:get(members, Detail))
            ),
            ?assertMatch([#{<<"id">> := ?PROJECT_ID} | _], maps:get(projects, Detail))
        end
    ).

%%% ===================================================================
%%% admin_member_page
%%% ===================================================================

admin_member_page_test_() ->
    ?WITH_MECKS(
        [
            {workspace_member_repo, [
                {'page_by_workspace', 4, fun(?WS_ID, 2, 10, _Col) ->
                    {ok, #{
                        list => [#{<<"user_id">> => ?OWNER, <<"role">> => <<"member">>}],
                        page => 2,
                        size => 10,
                        total => 11,
                        total_page => 2
                    }}
                end}
            ]}
        ],
        fun() ->
            {ok, P} = workspace_logic:admin_member_page(?WS_ID, 2, 10),
            ?assertEqual(11, maps:get(total, P))
        end
    ).

%%% ===================================================================
%%% admin_archive / admin_restore —— 平台侧 + 审计列 + 409
%%% ===================================================================

%% 2026-08-31 由 {Desc, fun} 包装式改造为规范 context 形态——原形态断言
%% 从不执行（M-6 空转判绿族），本轮翻转 archived_by 契约时失去红绿信号才暴露。
%% SQL/Params 校验走 meck history（mock fun 闭包捕获构建期 Self，跨进程消息
%% 收不到——这是包装式改 context 时最容易踩的第二个坑）。
admin_archive_writes_audit_columns_test_() ->
    Mocks = [
        ws_exists_mocks(),
        {elib_pg, [
            {'with_tx', 1, tx_fun()},
            {'execute', 3, fun(_Conn, _Sql, _Params) -> {ok, 1} end}
        ]}
    ],
    {setup,
        fun() ->
            lists:foreach(
                fun({Module, Expectations}) ->
                    {ok, _} = meck_helper:setup_mock(Module, Expectations)
                end,
                Mocks
            )
        end,
        fun(_) ->
            lists:foreach(
                fun({Module, _}) -> meck_helper:cleanup_mock(Module) end,
                Mocks
            )
        end,
        [
            {"admin archive writes audit columns", fun() ->
                ?assertMatch(
                    {ok, #{
                        workspace_id := ?WS_ID,
                        status := <<"archived">>,
                        archived_by := null
                    }},
                    workspace_logic:admin_archive(?ADM_UID, ?WS_ID)
                ),
                %% 新契约核心 = 返回 archived_by:null（DB 列写 NULL，操作者审计
                %% 由 admin_operation_logs 承担）；真库端到端见
                %% workspace_admin_archive_fk_tests（归档后列值断言）
                ?assertEqual(1, meck:num_calls(elib_pg, execute, 3))
            end}
        ]}.

admin_archive_is_not_owner_gated_test_() ->
    [
        {"admin archive is not owner gated", fun() ->
            Self = self(),
            ?WITH_MECKS(
                [
                    ws_exists_mocks(),
                    {workspace_member_repo, [
                        {'find', 3, fun(_Ws, _Uid, _Col) ->
                            erlang:error(should_not_check_owner)
                        end}
                    ]},
                    {elib_pg, [
                        {'with_tx', 1, tx_fun()},
                        {'execute', 3, fun(_Conn, _Sql, _Params) ->
                            Self ! archived,
                            {ok, 1}
                        end}
                    ]}
                ],
                fun() ->
                    %% 平台运营动作：无 Owner 校验——管理员不在 workspace_member 表也能归档
                    ?assertMatch({ok, _}, workspace_logic:admin_archive(?ADM_UID, ?WS_ID)),
                    receive
                        archived -> ok
                    after 500 -> ?assert(false, "archive not executed")
                    end
                end
            )
        end}
    ].

admin_archive_double_rejected_409_test_() ->
    ?WITH_MECKS(
        [
            ws_exists_mocks(),
            {elib_pg, [
                {'with_tx', 1, tx_fun()},
                {'execute', 3, fun(_Conn, _Sql, _Params) -> {ok, 0} end}
            ]}
        ],
        fun() ->
            ?assertMatch(
                {error, {409, _}}, workspace_logic:admin_archive(?ADM_UID, ?WS_ID)
            )
        end
    ).

admin_archive_not_found_test_() ->
    ?WITH_MECKS(
        [
            {workspace_ds, [
                {'find_by_id', 2, fun(?WS_ID, <<"id">>) -> #{} end}
            ]}
        ],
        fun() ->
            ?assertMatch(
                {error, {404, _}}, workspace_logic:admin_archive(?ADM_UID, ?WS_ID)
            )
        end
    ).

admin_restore_clears_audit_columns_test_() ->
    [
        {"admin restore clears audit columns", fun() ->
            Self = self(),
            ?WITH_MECKS(
                [
                    ws_exists_mocks(),
                    {elib_pg, [
                        {'with_tx', 1, tx_fun()},
                        {'execute', 3, fun(_Conn, Sql, _Params) ->
                            Self ! {admin_restore_sql, Sql},
                            {ok, 1}
                        end}
                    ]}
                ],
                fun() ->
                    ?assertMatch(
                        {ok, #{workspace_id := ?WS_ID, status := <<"active">>}},
                        workspace_logic:admin_restore(?ADM_UID, ?WS_ID)
                    ),
                    receive
                        {admin_restore_sql, Sql} ->
                            ?assertNotEqual(nomatch, binary:match(Sql, <<"archived_at = NULL">>)),
                            ?assertNotEqual(nomatch, binary:match(Sql, <<"archived_by = NULL">>))
                    after 500 -> ?assert(false, "admin restore UPDATE not executed")
                    end
                end
            )
        end}
    ].

admin_restore_not_archived_409_test_() ->
    ?WITH_MECKS(
        [
            ws_exists_mocks(),
            {elib_pg, [
                {'with_tx', 1, tx_fun()},
                {'execute', 3, fun(_Conn, _Sql, _Params) -> {ok, 0} end}
            ]}
        ],
        fun() ->
            ?assertMatch(
                {error, {409, _}}, workspace_logic:admin_restore(?ADM_UID, ?WS_ID)
            )
        end
    ).

%%% ===================================================================
%%% 归档联动集成断言（T7 守卫 980 × 运营归档）
%%% ===================================================================

%% admin_archive 落库后（workspace.status='archived'），
%% workspace_guard:ensure_writable 对 workspace 资源的写前置检查必须返回 980
%% ——运营归档与 Owner 侧归档走同一状态列，写守卫天然联动。
admin_archive_then_business_write_rejected_980_test_() ->
    [
        {"admin archive then business write rejected 980", fun() ->
            Self = self(),
            ?WITH_MECKS(
                [
                    ws_exists_mocks(),
                    {elib_pg, [
                        {'with_tx', 1, tx_fun()},
                        {'execute', 3, fun(_Conn, _Sql, _Params) ->
                            Self ! archived,
                            {ok, 1}
                        end},
                        %% admin_archive 落库后 workspace.status = archived：
                        %% workspace_guard 的行状态读取命中归档分支
                        {'one', 2, fun(<<"SELECT status FROM workspace WHERE id = $1">>, [?WS_ID]) ->
                            {ok, #{<<"status">> => <<"archived">>}}
                        end}
                    ]},
                    {workspace_resolver, [
                        {'resolve_workspace', 1, fun({project, ?PROJECT_ID}) -> {ok, ?WS_ID} end}
                    ]}
                ],
                fun() ->
                    %% 1) 运营归档成功
                    ?assertMatch({ok, _}, workspace_logic:admin_archive(?ADM_UID, ?WS_ID)),
                    receive
                        archived -> ok
                    after 500 -> ?assert(false, "archive not executed")
                    end,
                    %% 2) 归档后 workspace 业务写被 T7 守卫拒绝（稳定错误码 980）
                    ?assertMatch(
                        {error, {?ERR_WORKSPACE_ARCHIVED, _Msg}},
                        workspace_guard:ensure_writable({project, ?PROJECT_ID})
                    )
                end
            )
        end}
    ].

%% 恢复后写守卫放行（同一状态列的反向验证）
admin_restore_then_business_write_allowed_test_() ->
    ?WITH_MECKS(
        [
            ws_exists_mocks(),
            {elib_pg, [
                {'with_tx', 1, tx_fun()},
                {'execute', 3, fun(_Conn, _Sql, _Params) -> {ok, 1} end},
                {'one', 2, fun(<<"SELECT status FROM workspace WHERE id = $1">>, [?WS_ID]) ->
                    {ok, #{<<"status">> => <<"active">>}}
                end}
            ]},
            {workspace_resolver, [
                {'resolve_workspace', 1, fun({project, ?PROJECT_ID}) -> {ok, ?WS_ID} end}
            ]}
        ],
        fun() ->
            ?assertMatch({ok, _}, workspace_logic:admin_restore(?ADM_UID, ?WS_ID)),
            ?assertEqual(ok, workspace_guard:ensure_writable({project, ?PROJECT_ID}))
        end
    ).
