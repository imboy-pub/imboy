-module(talk_record_export_sweep_race_tests).
-include_lib("eunit/include/eunit.hrl").

deleted_parent_does_not_abort_sweep_test() ->
    Modules = [attachment_derived_artifact, talk_record_export_repo, elib_pg],
    meck:new(Modules, [non_strict, no_link]),
    try
        meck:expect(attachment_derived_artifact, reconcile, fun() -> {ok, 0} end),
        meck:expect(elib_pg, with_conn, fun(_) ->
            {ok, [#{<<"record_id">> => 1}, #{<<"record_id">> => 2}]}
        end),
        meck:expect(elib_pg, with_tx, fun(F, _) -> F(conn) end),
        meck:expect(elib_pg, query, fun(conn, _, [_]) -> {ok, []} end),
        ?assertEqual({ok, 0}, talk_record_export_artifact:sweep()),
        ?assertEqual(2, meck:num_calls(elib_pg, query, '_')),
        ?assert(meck:validate(Modules))
    after
        meck:unload(Modules)
    end.

error_after_expiry_update_aborts_transaction_test() ->
    Modules = [
        attachment_derived_artifact,
        talk_record_export_repo,
        talk_record_attachment_adapter,
        elib_pg
    ],
    meck:new(Modules, [non_strict, no_link]),
    try
        meck:expect(attachment_derived_artifact, reconcile, fun() -> {ok, 0} end),
        meck:expect(elib_pg, with_conn, fun(_) -> {ok, [#{<<"record_id">> => 1}]} end),
        meck:expect(elib_pg, with_tx, fun(F, _) ->
            try
                F(conn)
            catch
                throw:Reason ->
                    put(rollback_seen, Reason),
                    {rollback, Reason}
            end
        end),
        meck:expect(elib_pg, query, fun(conn, _, [1]) -> {ok, [#{<<"id">> => 1}]} end),
        meck:expect(elib_pg, execute, fun(conn, _, [1]) -> {ok, 1} end),
        meck:expect(
            talk_record_attachment_adapter,
            evaluate_release_blockers_tx,
            fun(conn, 1) -> {error, injected_failure} end
        ),
        ?assertEqual({error, injected_failure}, talk_record_export_artifact:sweep()),
        ?assertEqual({abort_tx, injected_failure}, erase(rollback_seen)),
        ?assertEqual(1, meck:num_calls(elib_pg, execute, '_')),
        ?assert(meck:validate(Modules))
    after
        meck:unload(Modules)
    end.
