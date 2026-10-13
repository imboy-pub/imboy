-module(elib_pg_commit_actions_tests).
-include_lib("eunit/include/eunit.hrl").

commit_actions_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun(C) -> ?_test(committed_actions_are_deferred_and_deduplicated(C)) end,
        fun(C) -> ?_test(rollback_discards_actions(C)) end,
        fun(C) -> ?_test(silent_rollback_does_not_run_actions(C)) end,
        fun(C) -> ?_test(commit_exception_discards_actions(C)) end,
        fun(C) -> ?_test(action_failure_is_not_a_rollback(C)) end,
        fun(C) -> ?_test(nested_transactions_restore_outer_queue(C)) end,
        fun(_) -> ?_test(missing_context_is_rejected()) end,
        fun(C) -> ?_test(action_capacity_aborts_before_commit(C)) end
    ]}.

setup() ->
    Conns = [
        spawn(fun() ->
            receive
                stop -> ok
            end
        end)
     || _ <- [1, 2]
    ],
    meck:new(pooler, [no_link, passthrough]),
    meck:expect(pooler, take_member, fun(pgsql) -> hd(Conns) end),
    meck:expect(pooler, return_member, fun(pgsql, _) -> ok end),
    meck:new(config_ds, [no_link, passthrough]),
    meck:expect(config_ds, env, fun(sql_driver) -> pgsql end),
    meck:new(epgsql, [no_link, passthrough]),
    meck:expect(epgsql, with_transaction, fun(C, F, _) -> F(C) end),
    meck:expect(epgsql, get_cmd_status, fun(_) -> {ok, commit} end),
    meck:expect(epgsql, squery, fun(_, <<"ROLLBACK">>) -> {ok, [], []} end),
    Conns.
cleanup(Conns) ->
    [meck:unload(M) || M <- [epgsql, config_ds, pooler]],
    [C ! stop || C <- Conns].

committed_actions_are_deferred_and_deduplicated(_Conns) ->
    put(commit_test_events, []),
    Result = elib_pg:with_tx(fun(C) ->
        ok = elib_pg:after_commit(C, group, fun() -> record_action(old) end),
        ok = elib_pg:after_commit(C, group, fun() ->
            ?assertEqual(1, meck:num_calls(pooler, return_member, 2)),
            record_action(new)
        end),
        ?assertEqual([], get(commit_test_events)),
        {ok, joined}
    end),
    ?assertEqual({ok, joined}, Result),
    ?assertEqual([new], get(commit_test_events)),
    ?assertEqual(undefined, get(elib_pg_commit_actions)),
    ?assertEqual(1, meck:num_calls(epgsql, get_cmd_status, 1)).

rollback_discards_actions(_Conns) ->
    meck:expect(epgsql, with_transaction, fun(C, F, _) ->
        try
            F(C)
        catch
            throw:{rollback, R} -> {rollback, R}
        end
    end),
    put(commit_test_events, []),
    ?assertEqual(
        {rollback, aborted},
        elib_pg:with_tx(fun(C) ->
            elib_pg:after_commit(C, group, fun() -> record_action(incorrect) end),
            throw({rollback, aborted})
        end)
    ),
    ?assertEqual([], get(commit_test_events)),
    ?assertEqual(undefined, get(elib_pg_commit_actions)),
    ?assertEqual(0, meck:num_calls(epgsql, get_cmd_status, 1)).

silent_rollback_does_not_run_actions(_Conns) ->
    meck:expect(epgsql, get_cmd_status, fun(_) -> {ok, rollback} end),
    put(commit_test_events, []),
    ?assertEqual(
        {error, {commit_not_confirmed, {ok, joined}, {ok, rollback}}},
        elib_pg:with_tx(fun(C) ->
            elib_pg:after_commit(C, group, fun() -> record_action(incorrect) end),
            {ok, joined}
        end)
    ),
    ?assertEqual([], get(commit_test_events)).

commit_exception_discards_actions(_Conns) ->
    meck:expect(epgsql, with_transaction, fun(C, F, _) ->
        F(C),
        error(commit_failed)
    end),
    put(commit_test_events, []),
    ?assertEqual(
        {error, {db_exception, error, commit_failed}},
        elib_pg:with_tx(fun(C) ->
            elib_pg:after_commit(C, group, fun() -> record_action(incorrect) end),
            ok
        end)
    ),
    ?assertEqual([], get(commit_test_events)),
    ?assertEqual(undefined, get(elib_pg_commit_actions)).

action_failure_is_not_a_rollback(_Conns) ->
    ?assertEqual(
        {error, {after_commit_failed, {ok, joined}, [{group, {error, cache_unavailable}}]}},
        elib_pg:with_tx(fun(C) ->
            elib_pg:after_commit(C, group, fun() -> error(cache_unavailable) end),
            {ok, joined}
        end)
    ),
    ?assertEqual(0, meck:num_calls(epgsql, squery, 2)),
    ?assertEqual(1, meck:num_calls(epgsql, with_transaction, 3)).

nested_transactions_restore_outer_queue(Conns) ->
    put(commit_test_events, []),
    put(commit_test_take, 0),
    meck:expect(pooler, take_member, fun(pgsql) ->
        N = get(commit_test_take) + 1,
        put(commit_test_take, N),
        lists:nth(N, Conns)
    end),
    Result = elib_pg:with_tx(fun(C) ->
        elib_pg:after_commit(C, outer, fun() -> record_action(outer) end),
        InnerResult = elib_pg:with_tx(fun(Inner) ->
            elib_pg:after_commit(Inner, inner, fun() -> record_action(inner) end),
            inner_result
        end),
        ?assertEqual(inner_result, InnerResult),
        ?assertEqual([inner], get(commit_test_events)),
        outer_result
    end),
    ?assertEqual(outer_result, Result),
    ?assertEqual([inner, outer], get(commit_test_events)),
    ?assertEqual(undefined, get(elib_pg_commit_actions)).

missing_context_is_rejected() ->
    ?assertThrow(
        {abort_tx, transaction_context_missing},
        elib_pg:after_commit(self(), group, fun() -> ok end)
    ).
action_capacity_aborts_before_commit(_Conns) ->
    put(commit_test_events, []),
    Result = elib_pg:with_tx(fun(C) ->
        lists:foreach(
            fun(Id) ->
                elib_pg:after_commit(C, Id, fun() -> record_action(incorrect) end)
            end,
            lists:seq(1, 2048)
        ),
        %% 满额时覆盖既有 Id 不增加空间；新 Id 必须回滚。
        ok = elib_pg:after_commit(C, 1, fun() -> ok end),
        elib_pg:after_commit(C, 2049, fun() -> ok end)
    end),
    ?assertEqual({error, commit_actions_full}, Result),
    ?assertEqual([], get(commit_test_events)),
    ?assertEqual(undefined, get(elib_pg_commit_actions)),
    ?assertEqual(0, meck:num_calls(epgsql, get_cmd_status, 1)).

record_action(Tag) ->
    put(commit_test_events, get(commit_test_events) ++ [Tag]),
    ok.
