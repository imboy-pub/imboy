-module(organization_default_workspace_tests).

%% ORG-05 Explicit Default Workspace（Core Contract C05）单元测试：
%%   * domain 纯函数裁决（archive 交接策略、目标预检、ID 校验）；
%%   * app get/set/clear 编排（org 行锁、幂等、稳定错误码、跨 Org 拒绝映射）；
%%   * 读取不回落 min-ID 推导（负例：无关系行时 not_set，不发 workspace 表查询）；
%%   * Workspace 生命周期钩子（个人域直通、失败抛 abort_tx）。
%% SQL 真实行为（触发器守卫/组合 FK/并发恰一/backfill 等值）由
%% organization_default_workspace_behavior_harness.escript 在一次性 PG 上覆盖。

-include_lib("eunit/include/eunit.hrl").

-define(ORG_ID, 700001).
-define(WS_ID, 800001).
-define(WS_ID_2, 800002).
-define(OPERATOR, 900001).

%% elib_pg:with_tx 同语义直通：测试进程内以 fake_conn 执行事务体。
run_tx(TxFun) ->
    try TxFun(fake_conn) of
        Result -> Result
    catch
        throw:{abort_tx, Reason} -> {error, Reason};
        Class:Reason -> {error, {db_exception, Class, Reason}}
    end.

%% 进程字典哨兵：事件追加到统一顺序表（验证锁序）
record_event(Event) ->
    Prev =
        case get(odw_order) of
            undefined -> [];
            L -> L
        end,
    put(odw_order, Prev ++ [Event]).

%%--------------------------------------------------------------------
%% domain 纯函数
%%--------------------------------------------------------------------

domain_valid_id_test_() ->
    [
        {"positive integer id ok", fun() ->
            ?assertEqual(ok, organization_default_workspace:valid_id(?ORG_ID))
        end},
        {"zero/negative/non-integer rejected", fun() ->
            ?assertMatch({error, {invalid_id, 0}}, organization_default_workspace:valid_id(0)),
            ?assertMatch({error, {invalid_id, -1}}, organization_default_workspace:valid_id(-1)),
            ?assertMatch(
                {error, {invalid_id, <<"x">>}}, organization_default_workspace:valid_id(<<"x">>)
            )
        end}
    ].

domain_ensure_settable_target_test_() ->
    [
        {"active same-org target ok", fun() ->
            ?assertEqual(
                ok,
                organization_default_workspace:ensure_settable_target(
                    ?ORG_ID, ?ORG_ID, <<"active">>
                )
            )
        end},
        {"cross-org target rejected", fun() ->
            ?assertEqual(
                {error, cross_org},
                organization_default_workspace:ensure_settable_target(
                    ?ORG_ID, ?ORG_ID + 1, <<"active">>
                )
            )
        end},
        {"archived target rejected", fun() ->
            ?assertEqual(
                {error, not_active},
                organization_default_workspace:ensure_settable_target(
                    ?ORG_ID, ?ORG_ID, <<"archived">>
                )
            )
        end},
        {"missing target rejected", fun() ->
            ?assertEqual(
                {error, not_found},
                organization_default_workspace:ensure_settable_target(?ORG_ID, null, <<"active">>)
            )
        end}
    ].

domain_archive_decision_test_() ->
    [
        {"policy is replace_with_min_active (transition-equivalence decision)", fun() ->
            ?assertEqual(
                replace_with_min_active, organization_default_workspace:next_default_policy()
            )
        end},
        {"archived default with remaining actives replaces with min id", fun() ->
            %% 升序剩余列表头 = min（与 legacy min-ID 读法等值）
            ?assertEqual(
                {replace, 100},
                organization_default_workspace:archive_decision(true, [100, 300, 900])
            )
        end},
        {"archived default with no remaining is rejected (G3 strong handover)", fun() ->
            %% GZAPP-02/G3（计划 §4.2）：无剩余 active 时拒绝归档
            %% （先显式指定替代默认），不再静默 clear。
            ?assertEqual(
                {error, no_active_replacement},
                organization_default_workspace:archive_decision(true, [])
            )
        end},
        {"archiving non-default never silently clears (defensive arm fail-closed)", fun() ->
            ?assertEqual(
                {error, not_current_default},
                organization_default_workspace:archive_decision(false, [100])
            )
        end}
    ].

%%--------------------------------------------------------------------
%% app get（显式关系唯一真源，无 min-ID 回落）
%%--------------------------------------------------------------------

with_pg_mocks(PgExpectations, TestFun) ->
    meck:new(organization_default_workspace_pg, [non_strict, no_link]),
    lists:foreach(
        fun({Name, Arity, Fun}) ->
            meck:expect(organization_default_workspace_pg, Name, Arity, Fun)
        end,
        PgExpectations
    ),
    try
        TestFun()
    after
        meck:unload(organization_default_workspace_pg)
    end.

get_test_() ->
    [
        {"get returns explicit relation row", fun() ->
            with_pg_mocks(
                [{'find', 1, fun(?ORG_ID) -> {ok, ?WS_ID} end}], fun() ->
                    ?assertEqual({ok, ?WS_ID}, organization_default_workspace_app:get(?ORG_ID))
                end
            )
        end},
        {"get without relation row is not_set and issues NO workspace-table fallback", fun() ->
            meck:new(elib_pg, [non_strict, no_link]),
            meck:expect(elib_pg, query, 1, fun(_Sql) -> erlang:error(no_fallback_query_expected) end),
            meck:expect(organization_default_workspace_pg, find, 1, fun(?ORG_ID) ->
                {error, not_found}
            end),
            try
                %% 负例（ORG-A09）：即使存量存在 id 更小的 archived/active
                %% Workspace（此处由「无任何 workspace 表查询」断言承载），
                %% 读取不得按 min-ID 推导命中。
                ?assertEqual(
                    {error, not_set}, organization_default_workspace_app:get(?ORG_ID)
                ),
                ?assertEqual(0, meck:num_calls(elib_pg, query, 1))
            after
                meck:unload(elib_pg),
                meck:unload(organization_default_workspace_pg)
            end
        end},
        {"get invalid id is not_set without DB access", fun() ->
            meck:new(organization_default_workspace_pg, [non_strict, no_link]),
            try
                ?assertEqual({error, not_set}, organization_default_workspace_app:get(0)),
                ?assertEqual(0, meck:num_calls(organization_default_workspace_pg, find, 1))
            after
                meck:unload(organization_default_workspace_pg)
            end
        end}
    ].

%%--------------------------------------------------------------------
%% app set（org 行锁 + 稳定错误码 + 幂等）
%%--------------------------------------------------------------------

set_mocks(Opts) ->
    meck:new(elib_pg, [non_strict, no_link]),
    meck:expect(elib_pg, with_tx, 1, fun run_tx/1),
    meck:new(organization_owner_store, [non_strict, no_link]),
    meck:expect(organization_owner_store, lock_organization_tx, 2, fun(_Conn, OrgId) ->
        record_event(lock_org),
        case maps:get(org_exists, Opts, true) of
            true -> {ok, #{<<"id">> => OrgId, <<"status">> => <<"active">>}};
            false -> {error, not_found}
        end
    end),
    meck:new(organization_default_workspace_pg, [non_strict, no_link]),
    meck:expect(organization_default_workspace_pg, target_row_tx, 2, fun(_Conn, _WsId) ->
        record_event(target_read),
        RowOrError =
            maps:get(
                target,
                Opts,
                #{<<"organization_id">> => ?ORG_ID, <<"status">> => <<"active">>}
            ),
        case RowOrError of
            {error, Reason} -> {error, Reason};
            Row -> {ok, Row}
        end
    end),
    meck:expect(organization_default_workspace_pg, upsert_tx, 3, fun(_Conn, OrgId, WsId) ->
        record_event(upsert),
        put(odw_upsert, {OrgId, WsId}),
        maps:get(upsert_result, Opts, {ok, changed})
    end),
    ok.

unload_set_mocks() ->
    meck:unload(elib_pg),
    meck:unload(organization_owner_store),
    meck:unload(organization_default_workspace_pg).

set_test_() ->
    [
        {"set changed with org row locked before target read", fun() ->
            set_mocks(#{}),
            try
                ?assertEqual(
                    {ok, changed},
                    organization_default_workspace_app:set(?OPERATOR, ?ORG_ID, ?WS_ID)
                ),
                ?assertEqual({?ORG_ID, ?WS_ID}, get(odw_upsert)),
                %% 锁序（C05 变更锁 org 行）：组织行先，目标读写后
                ?assertEqual([lock_org, target_read, upsert], get(odw_order))
            after
                erase(odw_order),
                erase(odw_upsert),
                unload_set_mocks()
            end
        end},
        {"set same value is unchanged (idempotent)", fun() ->
            set_mocks(#{upsert_result => {ok, unchanged}}),
            try
                ?assertEqual(
                    {ok, unchanged},
                    organization_default_workspace_app:set(?OPERATOR, ?ORG_ID, ?WS_ID)
                )
            after
                erase(odw_order),
                erase(odw_upsert),
                unload_set_mocks()
            end
        end},
        {"set target of another org is rejected (cross-org fail-closed)", fun() ->
            %% 组合 FK 场景的预检镜像：目标行 org 与请求 org 不一致
            set_mocks(
                #{
                    target =>
                        #{<<"organization_id">> => ?ORG_ID + 1, <<"status">> => <<"active">>}
                }
            ),
            try
                ?assertMatch(
                    {error, {409, _}},
                    organization_default_workspace_app:set(?OPERATOR, ?ORG_ID, ?WS_ID)
                ),
                %% 拒绝路径不得落写
                ?assert(undefined =:= get(odw_upsert))
            after
                erase(odw_order),
                erase(odw_upsert),
                unload_set_mocks()
            end
        end},
        {"set archived target rejected 409", fun() ->
            set_mocks(
                #{target => #{<<"organization_id">> => ?ORG_ID, <<"status">> => <<"archived">>}}
            ),
            try
                ?assertMatch(
                    {error, {409, _}},
                    organization_default_workspace_app:set(?OPERATOR, ?ORG_ID, ?WS_ID)
                ),
                ?assert(undefined =:= get(odw_upsert))
            after
                erase(odw_order),
                erase(odw_upsert),
                unload_set_mocks()
            end
        end},
        {"set missing target rejected 404", fun() ->
            set_mocks(#{target => {error, not_found}}),
            try
                ?assertMatch(
                    {error, {404, _}},
                    organization_default_workspace_app:set(?OPERATOR, ?ORG_ID, ?WS_ID)
                )
            after
                erase(odw_order),
                erase(odw_upsert),
                unload_set_mocks()
            end
        end},
        {"set with unknown org rejected 404", fun() ->
            set_mocks(#{org_exists => false}),
            try
                ?assertMatch(
                    {error, {404, _}},
                    organization_default_workspace_app:set(?OPERATOR, ?ORG_ID, ?WS_ID)
                ),
                ?assertEqual([lock_org], get(odw_order))
            after
                erase(odw_order),
                erase(odw_upsert),
                unload_set_mocks()
            end
        end},
        {"set invalid ids rejected 400 without tx", fun() ->
            set_mocks(#{}),
            try
                ?assertMatch(
                    {error, {400, _}}, organization_default_workspace_app:set(?OPERATOR, 0, ?WS_ID)
                ),
                ?assertMatch(
                    {error, {400, _}},
                    organization_default_workspace_app:set(?OPERATOR, ?ORG_ID, -5)
                )
            after
                erase(odw_order),
                erase(odw_upsert),
                unload_set_mocks()
            end
        end}
    ].

%%--------------------------------------------------------------------
%% app clear（幂等）
%%--------------------------------------------------------------------

clear_test_() ->
    [
        {"clear returns cleared", fun() ->
            with_pg_mocks(
                [{'delete_tx', 2, fun(_Conn, _OrgId) -> {ok, cleared} end}], fun() ->
                    meck:new(elib_pg, [non_strict, no_link]),
                    meck:expect(elib_pg, with_tx, 1, fun run_tx/1),
                    meck:new(organization_owner_store, [non_strict, no_link]),
                    meck:expect(organization_owner_store, lock_organization_tx, 2, fun(_C, _O) ->
                        {ok, #{}}
                    end),
                    try
                        ?assertEqual(
                            {ok, cleared},
                            organization_default_workspace_app:clear(?OPERATOR, ?ORG_ID)
                        )
                    after
                        meck:unload(elib_pg),
                        meck:unload(organization_owner_store)
                    end
                end
            )
        end},
        {"clear on empty relation is already_empty (idempotent)", fun() ->
            with_pg_mocks(
                [{'delete_tx', 2, fun(_Conn, _OrgId) -> {ok, already_empty} end}], fun() ->
                    meck:new(elib_pg, [non_strict, no_link]),
                    meck:expect(elib_pg, with_tx, 1, fun run_tx/1),
                    meck:new(organization_owner_store, [non_strict, no_link]),
                    meck:expect(organization_owner_store, lock_organization_tx, 2, fun(_C, _O) ->
                        {ok, #{}}
                    end),
                    try
                        ?assertEqual(
                            {ok, already_empty},
                            organization_default_workspace_app:clear(?OPERATOR, ?ORG_ID)
                        )
                    after
                        meck:unload(elib_pg),
                        meck:unload(organization_owner_store)
                    end
                end
            )
        end}
    ].

%%--------------------------------------------------------------------
%% Workspace 生命周期钩子
%%--------------------------------------------------------------------

hooks_test_() ->
    [
        {"ensure_first_workspace_tx passes through for personal scope (undefined)", fun() ->
            with_pg_mocks(
                [
                    {'ensure_first_workspace_tx', 3, fun(_C, _O, _W) ->
                        erlang:error(must_not_touch_pg_for_personal)
                    end}
                ],
                fun() ->
                    ?assertEqual(
                        ok,
                        organization_default_workspace_app:ensure_first_workspace_tx(
                            fake_conn, undefined, ?WS_ID
                        )
                    )
                end
            )
        end},
        {"ensure_first_workspace_tx failure aborts the enclosing tx", fun() ->
            with_pg_mocks(
                [{'ensure_first_workspace_tx', 3, fun(_C, _O, _W) -> {error, injected} end}],
                fun() ->
                    try
                        organization_default_workspace_app:ensure_first_workspace_tx(
                            fake_conn, ?ORG_ID, ?WS_ID
                        ),
                        ?assert(false, "expected abort_tx throw")
                    catch
                        throw:{abort_tx, {organization_default_workspace_set_failed, injected}} ->
                            ok
                    end
                end
            )
        end},
        {"replace_or_clear_on_archive_tx passes through for personal scope", fun() ->
            with_pg_mocks(
                [
                    {'replace_or_clear_on_archive_tx', 3, fun(_C, _O, _W) ->
                        erlang:error(must_not_touch_pg_for_personal)
                    end}
                ],
                fun() ->
                    ?assertEqual(
                        ok,
                        organization_default_workspace_app:replace_or_clear_on_archive_tx(
                            fake_conn, undefined, ?WS_ID
                        )
                    )
                end
            )
        end},
        {"replace_or_clear_on_archive_tx failure aborts the enclosing tx", fun() ->
            with_pg_mocks(
                [{'replace_or_clear_on_archive_tx', 3, fun(_C, _O, _W) -> {error, injected} end}],
                fun() ->
                    try
                        organization_default_workspace_app:replace_or_clear_on_archive_tx(
                            fake_conn, ?ORG_ID, ?WS_ID
                        ),
                        ?assert(false, "expected abort_tx throw")
                    catch
                        throw:{abort_tx,
                            {organization_default_workspace_handover_failed, injected}} ->
                            ok
                    end
                end
            )
        end},
        {"replace_or_clear_on_archive_tx rejects with dedicated marker when no active replacement (G3)",
            fun() ->
                %% GZAPP-02/G3 强交接：无剩余 active → 专用拒绝标记（供
                %% workspace_logic 映射 409），不是 500 类 handover_failed。
                with_pg_mocks(
                    [
                        {'replace_or_clear_on_archive_tx', 3, fun(_C, _O, _W) ->
                            {error, no_active_replacement}
                        end}
                    ],
                    fun() ->
                        try
                            organization_default_workspace_app:replace_or_clear_on_archive_tx(
                                fake_conn, ?ORG_ID, ?WS_ID
                            ),
                            ?assert(false, "expected abort_tx throw")
                        catch
                            throw:{abort_tx,
                                {default_workspace_handover_required, no_active_replacement}} ->
                                ok
                        end
                    end
                )
            end}
    ].
