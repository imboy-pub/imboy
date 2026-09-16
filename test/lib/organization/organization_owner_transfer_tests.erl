-module(organization_owner_transfer_tests).

%% Owner transfer command（application 层）编排测试：
%% 锁序、冻结顺序（先降旧 → 再升新 → 最后改 owner_id 投影）与错误映射。
%% SQL 真实行为由一次性 PG 上的 organization_owner_behavior_harness.escript 覆盖。

-include_lib("eunit/include/eunit.hrl").

-define(ORG_ID, 101).
-define(OWNER, 201).
-define(TARGET, 203).
-define(AGENT, 205).

%% elib_pg:with_tx 同语义直通：在测试进程内以 fake_conn 执行事务体。
run_tx(TxFun) ->
    try TxFun(fake_conn) of
        Result -> Result
    catch
        throw:{abort_tx, Reason} -> {error, Reason};
        throw:{rollback, Reason} -> {rollback, Reason};
        Class:Reason -> {error, {db_exception, Class, Reason}}
    end.

%% 记录 store 调用顺序（验证冻结实现第 5 点的语句序）
setup_order_recorder(ExtraStoreMocks) ->
    Base =
        [
            {lock_organization_tx, 2, fun(fake_conn, OrgId) ->
                {ok, #{
                    <<"id">> => OrgId,
                    <<"owner_id">> => ?OWNER,
                    <<"status">> =>
                        case get(t_org_status) of
                            undefined -> <<"active">>;
                            S -> S
                        end
                }}
            end},
            {lock_member_with_account_tx, 3, fun(fake_conn, _OrgId, Uid) ->
                case get({t_member, Uid}) of
                    undefined -> {error, not_found};
                    Member -> {ok, Member}
                end
            end},
            {demote_previous_owner_tx, 3, fun(fake_conn, _OrgId, _Uid) ->
                record_event(demote_previous_owner),
                ok
            end},
            {promote_target_tx, 3, fun(fake_conn, _OrgId, Uid) ->
                record_event({promote_target, Uid}),
                ok
            end},
            {update_owner_projection_tx, 3, fun(fake_conn, _OrgId, NewOwner) ->
                record_event({update_owner_projection, NewOwner}),
                {ok, #{
                    <<"id">> => ?ORG_ID, <<"owner_id">> => NewOwner, <<"status">> => <<"active">>
                }}
            end}
        ],
    Merged = merge_mocks(Base, ExtraStoreMocks),
    Merged.

merge_mocks(Base, Overrides) ->
    lists:map(
        fun({Name, Arity, Fun}) ->
            case lists:keyfind(Name, 1, Overrides) of
                {Name, Arity, OverFun} -> {Name, Arity, OverFun};
                _ -> {Name, Arity, Fun}
            end
        end,
        Base
    ).

with_store_mocks(StoreMockTuples, TestFun) ->
    meck:new(organization_owner_store, [non_strict, no_link]),
    lists:foreach(
        fun({Name, Arity, Fun}) -> meck:expect(organization_owner_store, Name, Arity, Fun) end,
        StoreMockTuples
    ),
    meck:new(elib_pg, [non_strict, no_link]),
    meck:expect(elib_pg, with_tx, 1, fun run_tx/1),
    try
        TestFun()
    after
        meck:unload(organization_owner_store),
        meck:unload(elib_pg)
    end.

transfer_orders_demote_then_promote_then_projection_test_() ->
    ?_test(
        with_store_mocks(setup_order_recorder([]), fun() ->
            erase(owner_tx_order),
            erase(t_org_status),
            put(
                {t_member, ?OWNER},
                #{<<"role">> => <<"owner">>, <<"status">> => <<"active">>, <<"account_type">> => 0}
            ),
            put(
                {t_member, ?TARGET},
                #{<<"role">> => <<"admin">>, <<"status">> => <<"active">>, <<"account_type">> => 0}
            ),
            ?assertMatch(
                {ok, #{
                    organization_id := ?ORG_ID,
                    owner_id := ?TARGET,
                    previous_owner_id := ?OWNER,
                    previous_owner_role := <<"admin">>
                }},
                organization_owner_transfer:transfer(?OWNER, ?ORG_ID, ?TARGET)
            ),
            %% 冻结顺序：降旧 → 升新 → 改投影
            Order = [Demoted || Demoted <- get_order()],
            ?assertEqual(
                [
                    demote_previous_owner,
                    {promote_target, ?TARGET},
                    {update_owner_projection, ?TARGET}
                ],
                Order
            )
        end)
    ).

record_event(Event) ->
    Prior =
        case get(owner_tx_order) of
            undefined -> [];
            L -> L
        end,
    put(owner_tx_order, Prior ++ [Event]).

get_order() ->
    case get(owner_tx_order) of
        undefined -> [];
        Order -> Order
    end.

transfer_rejects_when_org_archived_test_() ->
    ?_test(
        with_store_mocks(setup_order_recorder([]), fun() ->
            erase(owner_tx_order),
            put(t_org_status, <<"archived">>),
            put({t_member, ?OWNER}, #{
                <<"role">> => <<"owner">>,
                <<"status">> => <<"active">>,
                <<"account_type">> => 0
            }),
            ?assertMatch(
                {error, {409, _}},
                organization_owner_transfer:transfer(?OWNER, ?ORG_ID, ?TARGET)
            ),
            ?assertEqual(undefined, get(owner_tx_order))
        end)
    ).

transfer_rejects_when_actor_not_projection_owner_test_() ->
    ?_test(
        with_store_mocks(setup_order_recorder([]), fun() ->
            %% 锁定行 owner_id 不是 actor → 403（锁内裁决，无任何写语句）
            meck:expect(
                organization_owner_store,
                lock_organization_tx,
                2,
                fun(fake_conn, OrgId) ->
                    {ok, #{<<"id">> => OrgId, <<"owner_id">> => 999, <<"status">> => <<"active">>}}
                end
            ),
            ?assertMatch(
                {error, {403, _}},
                organization_owner_transfer:transfer(?OWNER, ?ORG_ID, ?TARGET)
            ),
            ?assertEqual(undefined, get(owner_tx_order))
        end)
    ).

transfer_rejects_agent_target_test_() ->
    ?_test(
        with_store_mocks(setup_order_recorder([]), fun() ->
            put({t_member, ?OWNER}, #{
                <<"role">> => <<"owner">>,
                <<"status">> => <<"active">>,
                <<"account_type">> => 0
            }),
            put({t_member, ?AGENT}, #{
                <<"role">> => <<"member">>,
                <<"status">> => <<"active">>,
                <<"account_type">> => 1
            }),
            ?assertMatch(
                {error, {409, _}},
                organization_owner_transfer:transfer(?OWNER, ?ORG_ID, ?AGENT)
            ),
            ?assertEqual(undefined, get(owner_tx_order))
        end)
    ).

transfer_rejects_missing_org_test_() ->
    ?_test(
        with_store_mocks(setup_order_recorder([]), fun() ->
            meck:expect(
                organization_owner_store,
                lock_organization_tx,
                2,
                fun(fake_conn, _OrgId) -> {error, not_found} end
            ),
            ?assertMatch(
                {error, {404, _}},
                organization_owner_transfer:transfer(?OWNER, ?ORG_ID, ?TARGET)
            )
        end)
    ).

transfer_internal_failure_maps_to_500_test_() ->
    ?_test(
        with_store_mocks(
            setup_order_recorder([
                {update_owner_projection_tx, 3, fun(fake_conn, _OrgId, _NewOwner) ->
                    record_event({update_owner_projection, failed}),
                    {error, connection_closed}
                end}
            ]),
            fun() ->
                erase(owner_tx_order),
                erase(t_org_status),
                put({t_member, ?OWNER}, #{
                    <<"role">> => <<"owner">>,
                    <<"status">> => <<"active">>,
                    <<"account_type">> => 0
                }),
                put({t_member, ?TARGET}, #{
                    <<"role">> => <<"admin">>,
                    <<"status">> => <<"active">>,
                    <<"account_type">> => 0
                }),
                ?assertMatch(
                    {error, {500, _}},
                    organization_owner_transfer:transfer(?OWNER, ?ORG_ID, ?TARGET)
                ),
                %% 降旧与升新已执行（事务将被 DB 回滚），投影失败触发 500
                ?assertEqual(
                    [
                        demote_previous_owner,
                        {promote_target, ?TARGET},
                        {update_owner_projection, failed}
                    ],
                    get_order()
                )
            end
        )
    ).

transfer_validates_arguments_test_() ->
    [
        {"self transfer 400",
            ?_assertMatch(
                {error, {400, _}},
                organization_owner_transfer:transfer(?OWNER, ?ORG_ID, ?OWNER)
            )},
        {"非正整数参数 400",
            ?_assertMatch(
                {error, {400, _}},
                organization_owner_transfer:transfer(?OWNER, 0, ?TARGET)
            )},
        {"非整数参数 400",
            ?_assertMatch(
                {error, {400, _}},
                organization_owner_transfer:transfer(<<"x">>, ?ORG_ID, ?TARGET)
            )}
    ].
