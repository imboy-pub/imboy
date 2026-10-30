-module(attachment_confirm_lock_error_tests).
-include_lib("eunit/include/eunit.hrl").

lock_error_aborts_before_metadata_write_test() ->
    Key = <<"u9/file/example">>,
    Conn = make_ref(),
    %% Preserve the repository's exact error term, including its SQLSTATE.
    Reason = {database_error, <<"40P01">>, deadlock_detected},
    ok = meck:new(attach_pending_repo, [no_link]),
    ok = meck:new(attachment_ds, [no_link]),
    ok = meck:new(attachment_repo, [no_link]),
    try
        ok = meck:expect(
            attach_pending_repo,
            lock_session_tx,
            fun(C, K) ->
                ?assertEqual({Conn, Key}, {C, K}),
                {error, Reason}
            end
        ),
        ?assertThrow(
            {abort_tx, Reason},
            attach_logic:confirm_v5_tx(
                Conn,
                Key,
                <<"private">>,
                none,
                0,
                9,
                #{},
                #{}
            )
        ),
        ?assertEqual(1, meck:num_calls(attach_pending_repo, lock_session_tx, '_')),
        ?assertEqual(0, meck:num_calls(attachment_ds, save_confirmed_tx, '_')),
        ?assertEqual(0, meck:num_calls(attachment_repo, confirmation_row_tx, '_'))
    after
        meck:unload(attachment_repo),
        meck:unload(attachment_ds),
        meck:unload(attach_pending_repo)
    end.
