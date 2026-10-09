-module(micro_community_refund_tests).
-include_lib("eunit/include/eunit.hrl").
-include("micro_community_test.hrl").

refunding_placeholder_keeps_membership_and_subscription_test_() ->
    fixture(1, 5, {ok, 1}, {ok, 1}, fun() ->
        ?assertEqual({ok, 1}, channel_order_ds:mark_refunding(<<"isolated-order">>)),
        assert_no_revocation()
    end).

successful_refund_deletes_subscription_and_revokes_in_same_tx_test_() ->
    fixture(5, 2, {ok, 1}, {ok, 1}, fun() ->
        ?assertEqual(ok, channel_order_ds:finalize_refund(<<"isolated-order">>, 7, <<"reason">>)),
        ?assertEqual(1, meck:num_calls(channel_subscription_repo, delete, [tx_conn, 10, 7])),
        ?assertEqual(1, meck:num_calls(channel_repo, increment_subscribers, [tx_conn, 10, -1])),
        ?assertEqual(
            1, meck:num_calls(micro_community_access_ds, revoke_channel_tx, [tx_conn, 10, 7])
        )
    end).

subscription_write_failure_aborts_transaction_without_revoke_test_() ->
    fixture(5, 2, {ok, 1}, {error, delete_failed}, fun() ->
        ?assertEqual(
            {error, delete_failed},
            channel_order_ds:finalize_refund(<<"isolated-order">>, 7, <<"reason">>)
        ),
        ?assertEqual(rollback, get(refund_tx_outcome)),
        ?assertEqual(0, meck:num_calls(micro_community_access_ds, revoke_channel_tx, '_'))
    end).

order_write_failure_aborts_without_subscription_delete_test_() ->
    fixture(5, 2, {error, cas_failed}, {ok, 1}, fun() ->
        ?assertEqual(
            {error, cas_failed},
            channel_order_ds:finalize_refund(<<"isolated-order">>, 7, <<"reason">>)
        ),
        ?assertEqual(rollback, get(refund_tx_outcome)),
        assert_no_revocation()
    end).

zero_cas_never_revokes_test_() ->
    fixture(5, 2, {ok, 0}, {ok, 1}, fun() ->
        ?assertEqual(
            {error, not_found_or_not_refunding},
            channel_order_ds:finalize_refund(<<"isolated-order">>, 7, <<"reason">>)
        ),
        assert_no_revocation()
    end).

revocation_failure_aborts_successful_refund_test_() ->
    fixture(5, 2, {ok, 1}, {ok, 1}, fun() ->
        meck:expect(
            micro_community_access_ds,
            revoke_channel_tx,
            fun(tx_conn, 10, 7) -> throw({abort_tx, revoke_failed}) end
        ),
        ?assertEqual(
            {error, revoke_failed},
            channel_order_ds:finalize_refund(<<"isolated-order">>, 7, <<"reason">>)
        ),
        ?assertEqual(rollback, get(refund_tx_outcome))
    end).

fixture(From, To, Cas, Delete, Test) ->
    ?MC_WITH_MECKS(
        [
            {elib_pg, [
                {with_tx, 1, fun(Fun) ->
                    try Fun(tx_conn) of
                        Result ->
                            put(refund_tx_outcome, commit),
                            Result
                    catch
                        throw:{abort_tx, Reason} ->
                            put(refund_tx_outcome, rollback),
                            {error, Reason}
                    end
                end}
            ]},
            {channel_order_repo, [
                {find_for_transition_tx, 2, fun(tx_conn, <<"isolated-order">>) ->
                    {ok, [#{<<"channel_id">> => 10, <<"user_id">> => 7}]}
                end},
                {transition_tx, 4, fun(tx_conn, <<"isolated-order">>, ActualFrom, {ActualTo, _}) ->
                    ?assertEqual(From, ActualFrom),
                    ?assertEqual(To, ActualTo),
                    Cas
                end}
            ]},
            {micro_community_access_repo, [{lock_channel_tx, 2, fun(tx_conn, 10) -> ok end}]},
            {channel_subscription_repo, [{delete, 3, fun(tx_conn, 10, 7) -> Delete end}]},
            {channel_repo, [{increment_subscribers, 3, fun(tx_conn, 10, -1) -> {ok, 1} end}]},
            {micro_community_access_ds, [{revoke_channel_tx, 3, fun(tx_conn, 10, 7) -> ok end}]}
        ],
        fun() ->
            erase(refund_tx_outcome),
            Test()
        end
    ).

assert_no_revocation() ->
    ?assertEqual(0, meck:num_calls(channel_subscription_repo, delete, '_')),
    ?assertEqual(0, meck:num_calls(micro_community_access_ds, revoke_channel_tx, '_')).
