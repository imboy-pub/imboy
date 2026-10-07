%%% Transfer audit failures must abort the transaction and never publish a notice.
-module(group_transfer_audit_tests).
-include_lib("eunit/include/eunit.hrl").

transfer_audit_failure_test_() ->
    [
        {timeout, 30, fun() -> assert_audit_failure(standard) end},
        {timeout, 30, fun() -> assert_audit_failure(keep_admin) end},
        {timeout, 30, fun() -> assert_audit_failure(drop_admin) end}
    ].

assert_audit_failure(Mode) ->
    Modules = [
        group_ds,
        group_member_ds,
        workspace_guard,
        group_log_ds,
        elib_pg,
        imboy_domain_event
    ],
    try
        lists:foreach(fun(M) -> meck:new(M, [non_strict, no_link]) end, Modules),
        meck:expect(group_ds, find_by_id, fun(1, _) -> #{<<"owner_uid">> => 100} end),
        meck:expect(group_ds, update_owner_tx, fun(_, 1, 200, 100) -> ok end),
        meck:expect(
            group_member_ds,
            find_by_gid_and_uid,
            fun(1, 200, _) -> #{<<"id">> => 9} end
        ),
        meck:expect(group_member_ds, update_role, fun(_, 1, _, _) -> ok end),
        meck:expect(workspace_guard, ensure_writable_tx, fun(_, _) -> ok end),
        meck:expect(workspace_guard, abort_on_error, fun(ok) -> ok end),
        meck:expect(group_log_ds, add, fun(_, _) -> {error, audit_insert_failed} end),
        meck:expect(imboy_domain_event, publish, fun(_) -> ok end),
        meck:expect(elib_pg, with_tx, fun(F) ->
            try F(self()) of
                Value ->
                    put(transfer_test_tx_outcome, committed),
                    Value
            catch
                throw:{abort_tx, Reason} ->
                    put(transfer_test_tx_outcome, rolled_back),
                    {error, Reason}
            end
        end),
        Result =
            case Mode of
                standard -> group_logic:transfer(100, 1, 200);
                keep_admin -> group_logic:transfer(100, 1, 200, true);
                drop_admin -> group_logic:transfer(100, 1, 200, false)
            end,
        ?assertMatch({error, _}, Result),
        ?assertEqual(rolled_back, get(transfer_test_tx_outcome)),
        ?assertEqual(0, meck:num_calls(imboy_domain_event, publish, 1))
    after
        erase(transfer_test_tx_outcome),
        lists:foreach(fun(M) -> catch meck:unload(M) end, Modules)
    end.
