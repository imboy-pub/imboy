-module(group_notice_transaction_tests).
-include_lib("eunit/include/eunit.hrl").

fanout_test_() ->
    [
        {timeout, 20, ?_test(pure_mock_fixture:run(fun() -> fanout(Mode) end))}
     || Mode <- [large, delivery_failure, split, rollback]
    ].

fanout(Mode) ->
    Modules = [pooler, epgsql, config_ds, msg_store_repo, message_ds, msg_store_ds, elib_dt],
    Conn = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    Count = atomics:new(3, []),
    try
        [ok = meck:new(M, [no_link, passthrough]) || M <- Modules],
        meck:expect(config_ds, env, fun(sql_driver) -> pgsql end),
        meck:expect(pooler, take_member, fun(pgsql) -> Conn end),
        meck:expect(pooler, return_member, fun(pgsql, _) -> ok end),
        meck:expect(pooler, return_member, fun(pgsql, _, fail) -> ok end),
        meck:expect(epgsql, with_transaction, fun(C, F, _) -> F(C) end),
        meck:expect(epgsql, get_cmd_status, fun(_) -> {ok, commit} end),
        meck:expect(epgsql, squery, fun(_, <<"ROLLBACK">>) -> {ok, [], []} end),
        meck:expect(elib_dt, now, fun() -> <<"2026-11-11T00:00:00Z">> end),
        meck:expect(msg_store_repo, stage_s2c_tx, fun(C, _Mid, _Action, _Payload, _From, _To, _At) ->
            ?assertEqual(Conn, C),
            {ok, atomics:add_get(Count, 1, 1)}
        end),
        meck:expect(message_ds, assemble_msg, fun(_, _, To, _, Mid, _, _, _) ->
            #{<<"to">> => To, <<"id">> => Mid}
        end),
        meck:expect(msg_store_ds, enqueue, fun(<<"s2c">>, _, _) ->
            atomics:add(Count, 3, 1),
            ok
        end),
        meck:expect(message_ds, send_next, fun(To, _, _, _) ->
            atomics:add(Count, 2, 1),
            case {Mode, To} of
                {delivery_failure, 1} -> {error, synthetic_delivery_failure};
                _ -> ok
            end
        end),
        Recipients =
            case Mode of
                large -> lists:seq(1, 2500);
                _ -> [1, 2, 3]
            end,
        Result = elib_pg:with_tx(fun(C) ->
            ok = msg_s2c_ds:send_tx(C, 9, Recipients, <<"group_member_leave">>, #{}, <<"effect">>),
            case Mode of
                split ->
                    ok = msg_s2c_ds:send_tx(
                        C, 9, [4, 5], <<"group_member_leave">>, #{}, <<"effect">>
                    );
                _ ->
                    ok
            end,
            ?assertEqual(0, atomics:get(Count, 2)),
            case Mode of
                rollback -> throw({abort_tx, synthetic_abort});
                _ -> ok
            end
        end),
        case Mode of
            rollback ->
                ?assertEqual({error, synthetic_abort}, Result),
                ?assertEqual(0, atomics:get(Count, 2)),
                ?assertEqual(0, atomics:get(Count, 3));
            _ ->
                case Mode of
                    delivery_failure ->
                        ?assertMatch({error, {after_commit_failed, ok, [_]}}, Result);
                    _ ->
                        ?assertEqual(ok, Result)
                end,
                Expected =
                    case Mode of
                        split -> 5;
                        _ -> length(Recipients)
                    end,
                ?assertEqual(Expected, atomics:get(Count, 1)),
                ?assertEqual(Expected, atomics:get(Count, 2)),
                ExpectedKicks =
                    case Mode of
                        split -> 2;
                        _ -> 1
                    end,
                ?assertEqual(ExpectedKicks, atomics:get(Count, 3))
        end
    after
        [meck:unload(M) || M <- lists:reverse(Modules)],
        Conn ! stop
    end.

member_lookup_test_() ->
    [
        ?_test(pure_mock_fixture:run(fun() -> member_lookup(Result) end))
     || Result <- [{ok, [#{<<"id">> => 77}]}, {ok, []}, {error, synthetic_database_failure}]
    ].

member_lookup(Result) ->
    meck:new(elib_pg, [no_link, passthrough]),
    meck:new(config_ds, [no_link, passthrough]),
    meck:expect(config_ds, env, fun(sql_driver) -> pgsql end),
    Conn = self(),
    meck:expect(elib_pg, query, fun(C, _Sql, [10, 2]) ->
        ?assertEqual(Conn, C),
        Result
    end),
    meck:expect(elib_pg, one, fun(_, _) -> error(nested_connection) end),
    try
        Expected =
            case Result of
                {ok, [Row]} -> {ok, Row};
                {ok, []} -> {ok, #{}};
                Error -> Error
            end,
        ?assertEqual(Expected, group_member_repo:find_tx(Conn, 10, 2, <<"id">>))
    after
        meck:unload(elib_pg),
        meck:unload(config_ds)
    end.

leave_lookup_error_is_not_absence_test() ->
    pure_mock_fixture:run(fun() ->
        meck:new(group_member_repo, [no_link, non_strict]),
        meck:new(micro_community_access_repo, [no_link, passthrough]),
        Conn = self(),
        meck:expect(micro_community_access_repo, lock_group_tx, fun(C, 10) ->
            ?assertEqual(Conn, C),
            ok
        end),
        meck:expect(group_member_repo, find_tx, fun(C, 10, 2, _) ->
            ?assertEqual(Conn, C),
            {error, synthetic_database_failure}
        end),
        meck:expect(group_member_repo, find, fun(_, _, _) -> error(nested_connection) end),
        meck:expect(group_member_repo, tablename, fun() -> <<"group_member">> end),
        try
            ?assertEqual({error, synthetic_database_failure}, group_member_ds:leave(Conn, 2, 10, 2))
        after
            meck:unload(group_member_repo),
            meck:unload(micro_community_access_repo)
        end
    end).

already_active_join_does_not_publish_test() ->
    pure_mock_fixture:run(fun() ->
        Modules = [group_member_ds, elib_pg, workspace_guard, imboy_domain_event],
        [meck:new(M, [no_link, non_strict]) || M <- Modules],
        meck:expect(group_member_ds, join_group, fun(_, _, _, _, _) -> {ok, 0} end),
        meck:expect(elib_pg, with_tx, fun(F) -> F(self()) end),
        meck:expect(workspace_guard, ensure_writable_tx, fun(_, _) -> ok end),
        meck:expect(workspace_guard, abort_on_error, fun(ok) -> ok end),
        meck:expect(imboy_domain_event, publish, fun(_) -> error(duplicate_join_notice) end),
        try
            ?assertEqual(ok, group_member_logic:join_group(<<"invite">>, 2, 10, #{})),
            ?assertEqual(0, meck:num_calls(imboy_domain_event, publish, 1))
        after
            [meck:unload(M) || M <- Modules]
        end
    end).

strict_role_update_test_() ->
    [
        ?_test(pure_mock_fixture:run(fun() -> strict_role_update(Result) end))
     || Result <- [{ok, 1}, {ok, 0}, {error, synthetic_role_update_failure}]
    ].

strict_role_update(Result) ->
    Modules = [elib_pg, config_ds],
    [meck:new(M, [no_link, passthrough]) || M <- Modules],
    Conn = self(),
    meck:expect(config_ds, env, fun(sql_driver) -> pgsql end),
    meck:expect(elib_pg, update, fun(C, _, _, Where, [10, 4]) ->
        ?assertEqual(Conn, C),
        ?assertNotEqual(nomatch, binary:match(Where, <<"status = 1">>)),
        Result
    end),
    try
        Actual = group_member_ds:update_active_role_tx(Conn, 10, 4, 4),
        case Result of
            {ok, 1} -> ?assertEqual(ok, Actual);
            {ok, 0} -> ?assertMatch({error, Message} when is_binary(Message), Actual);
            {error, _} -> ?assertEqual(Result, Actual)
        end
    after
        [meck:unload(M) || M <- Modules]
    end.

transfer_member_race_test_() ->
    [
        {timeout, 20, ?_test(pure_mock_fixture:run(fun() -> transfer_member_race(Op, Target) end))}
     || Op <- [transfer, keep_admin, drop_admin], Target <- [2, 4]
    ].

transfer_member_race(Op, Target) ->
    Modules = [
        group_ds, group_member_ds, workspace_guard, group_log_ds, group_event_handler, elib_pg
    ],
    [meck:new(M, [no_link, non_strict]) || M <- Modules],
    meck:expect(group_ds, find_by_id, fun(10, _) -> #{<<"owner_uid">> => 2} end),
    meck:expect(group_ds, update_owner_tx, fun(_, 10, 4, 2) -> ok end),
    meck:expect(group_member_ds, find_by_gid_and_uid, fun(10, 4, _) -> #{<<"id">> => 88} end),
    meck:expect(group_member_ds, update_role, fun(_, 10, _, _) -> ok end),
    meck:expect(group_member_ds, update_active_role_tx, fun(C, 10, Uid, _) ->
        ?assertEqual(self(), C),
        case Uid of
            Target -> {error, <<"active member disappeared">>};
            _ -> ok
        end
    end),
    meck:expect(workspace_guard, ensure_writable_tx, fun(_, _) -> ok end),
    meck:expect(workspace_guard, abort_on_error, fun(ok) -> ok end),
    meck:expect(group_log_ds, add, fun(_, _) -> {ok, 99} end),
    meck:expect(group_event_handler, stage_tx, fun(_, _, _) -> ok end),
    meck:expect(elib_pg, with_tx, fun(F) ->
        try F(self()) of
            Value ->
                put(tx_outcome, committed),
                Value
        catch
            throw:{abort_tx, Reason} ->
                put(tx_outcome, rolled_back),
                {error, Reason}
        end
    end),
    try
        Result =
            case Op of
                transfer -> group_logic:transfer(2, 10, 4);
                keep_admin -> group_logic:transfer(2, 10, 4, true);
                drop_admin -> group_logic:transfer(2, 10, 4, false)
            end,
        ?assertMatch({error, Message} when is_binary(Message), Result),
        ?assertEqual(rolled_back, get(tx_outcome)),
        ?assertEqual(0, meck:num_calls(group_log_ds, add, 2)),
        ?assertEqual(0, meck:num_calls(group_event_handler, stage_tx, 3))
    after
        [meck:unload(M) || M <- Modules]
    end.

durable_notice_test_() ->
    [
        ?_test(pure_mock_fixture:run(fun() -> durable_notice(Mode) end))
     || Mode <- [removed, transfer, lookup_error]
    ].

durable_notice(Mode) ->
    Modules = [group_member_ds, msg_s2c_ds],
    [meck:new(M, [no_link, non_strict]) || M <- Modules],
    Conn = self(),
    meck:expect(group_member_ds, member_uids_tx, fun(C, 10) ->
        ?assertEqual(Conn, C),
        case Mode of
            lookup_error -> {error, synthetic_recipient_failure};
            _ -> {ok, [3, 4]}
        end
    end),
    meck:expect(msg_s2c_ds, send_tx, fun(C, From, To, Action, Payload, EffectKey) ->
        ?assertEqual(Conn, C),
        case Mode of
            removed ->
                ?assertEqual({2, [2, 3, 4], <<"group_member_leave">>}, {From, To, Action}),
                ?assertEqual(#{<<"gid">> => 10, <<"leave_uid">> => 2}, Payload);
            transfer ->
                ?assertEqual({2, [3, 4], <<"group_transfer">>}, {From, To, Action}),
                ?assertEqual(4, maps:get(<<"to_owner_uid">>, Payload))
        end,
        put(effect_key, EffectKey),
        ok
    end),
    try
        Event =
            case Mode of
                transfer -> {owner_transferred, 10, 2, 4};
                _ -> {member_removed, 10, 2}
            end,
        case Mode of
            lookup_error ->
                ?assertEqual(
                    {error, synthetic_recipient_failure},
                    group_event_handler:stage_tx(Conn, Event, 77)
                ),
                ?assertEqual(0, meck:num_calls(msg_s2c_ds, send_tx, 6));
            _ ->
                ?assertEqual(ok, group_event_handler:stage_tx(Conn, Event, 77)),
                First = get(effect_key),
                ?assertEqual(ok, group_event_handler:stage_tx(Conn, Event, 77)),
                ?assertEqual(First, get(effect_key)),
                ?assertEqual(ok, group_event_handler:stage_tx(Conn, Event, 78)),
                ?assertNotEqual(First, get(effect_key))
        end
    after
        [meck:unload(M) || M <- Modules]
    end.

business_atomicity_test_() ->
    [
        {timeout, 20, ?_test(pure_mock_fixture:run(fun() -> business_atomicity(Op, Mode) end))}
     || Op <- [leave, transfer, keep_admin, drop_admin],
        Mode <- [stage_failure, committed, online_failure]
    ].

business_atomicity(Op, Mode) ->
    Modules = [
        group_ds,
        group_member_ds,
        workspace_guard,
        group_log_ds,
        group_event_handler,
        imboy_domain_event,
        elib_pg
    ],
    [meck:new(M, [no_link, non_strict]) || M <- Modules],
    meck:expect(group_ds, find_by_id, fun(10, _) -> #{<<"owner_uid">> => 2} end),
    meck:expect(group_ds, update_owner_tx, fun(_, 10, 4, 2) -> ok end),
    meck:expect(group_ds, leave, fun(2, 10) -> ok end),
    meck:expect(group_member_ds, find_by_gid_and_uid, fun(10, 4, _) -> #{<<"id">> => 88} end),
    meck:expect(group_member_ds, update_active_role_tx, fun(_, 10, _, _) -> ok end),
    meck:expect(group_member_ds, leave, fun(_, 2, 10, 2) -> {ok, 2, #{<<"id">> => 77}} end),
    meck:expect(workspace_guard, ensure_writable_tx, fun(_, _) -> ok end),
    meck:expect(workspace_guard, abort_on_error, fun(ok) -> ok end),
    meck:expect(group_log_ds, add, fun(_, _) -> {ok, 99} end),
    meck:expect(imboy_domain_event, publish, fun(_) -> ok end),
    meck:expect(group_event_handler, stage_tx, fun(Conn, Event, EffectId) ->
        ?assertEqual(self(), Conn),
        ?assertEqual(true, get(tx_open)),
        Expected =
            case Op of
                leave -> {{member_removed, 10, 2}, 77};
                _ -> {{owner_transferred, 10, 2, 4}, 99}
            end,
        ?assertEqual(Expected, {Event, EffectId}),
        case Mode of
            stage_failure -> {error, synthetic_staging_failure};
            _ -> ok
        end
    end),
    meck:expect(elib_pg, with_tx, fun(F) ->
        put(tx_open, true),
        try F(self()) of
            Value ->
                put(tx_outcome, committed),
                case Mode of
                    online_failure ->
                        {error,
                            {after_commit_failed, Value, [
                                {{create_receipt_notify, <<"m">>}, synthetic_online_failure}
                            ]}};
                    _ ->
                        Value
                end
        catch
            throw:{abort_tx, Reason} ->
                put(tx_outcome, rolled_back),
                {error, Reason}
        after
            erase(tx_open)
        end
    end),
    try
        Result =
            case Op of
                leave -> group_member_logic:leave(2, 10, 2);
                transfer -> group_logic:transfer(2, 10, 4);
                keep_admin -> group_logic:transfer(2, 10, 4, true);
                drop_admin -> group_logic:transfer(2, 10, 4, false)
            end,
        case Mode of
            stage_failure ->
                ?assertEqual(rolled_back, get(tx_outcome)),
                ?assertEqual(0, meck:num_calls(group_ds, leave, 2));
            _ ->
                ?assertEqual(committed, get(tx_outcome)),
                ?assertEqual(ok, Result),
                ?assertEqual(1, meck:num_calls(group_event_handler, stage_tx, 3))
        end,
        ?assertEqual(0, meck:num_calls(imboy_domain_event, publish, 1))
    after
        [meck:unload(M) || M <- Modules]
    end.
