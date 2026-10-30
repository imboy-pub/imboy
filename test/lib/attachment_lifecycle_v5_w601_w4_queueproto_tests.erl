-module(attachment_lifecycle_v5_w601_w4_queueproto_tests).

-include_lib("eunit/include/eunit.hrl").

-define(P, attachment_lifecycle_v5_w601_w4_queueproto).
-define(X, attachment_lifecycle_v5_w601_w4_context).

safe_queue_operations_test_() ->
    {setup,
        fun() ->
            ok = meck:new([?X, attachment_gc_repo], [non_strict]),
            ok = meck:expect(?X, q, fun(_, _) -> ok end),
            ok = meck:expect(?X, token, fun() -> <<"token">> end),
            ok
        end,
        fun(_) -> meck:unload([?X, attachment_gc_repo]) end, fun(_) ->
            [
                fun refuse_nonempty_queues/0,
                fun rollback_other_ordinary/0,
                fun rollback_other_enterprise/0,
                fun commit_matching_target/0,
                fun rollback_claim_error/0,
                fun commit_matching_enterprise/0
            ]
        end}.

refuse_nonempty_queues() ->
    meck:reset(?X),
    ok = meck:expect(?X, e, fun(_, _) -> {ok, [], [{true}]} end),
    ?assertError(
        {w601_queue_not_isolated, "attachment_object_deletion"},
        ?P:reset_ordinary(conn)
    ),
    ?assertError(
        {w601_queue_not_isolated, "enterprise_asset_delete_queue"},
        ?P:reset_eb(conn)
    ),
    ?assertEqual(0, meck:num_calls(?X, q, '_')),
    ?assertEqual(0, meck:num_calls(attachment_gc_repo, v5_ack, '_')),
    ok = meck:expect(?X, e, fun(_, _) -> {ok, [], [{false}]} end),
    ?assertEqual(ok, ?P:drain_ordinary(conn)),
    ?assertEqual(ok, ?P:drain_eb(conn)).

rollback_other_ordinary() ->
    meck:reset(?X),
    ok = meck:expect(
        attachment_gc_repo,
        v5_claim,
        fun(_, _) -> {ok, #{<<"out_path">> => <<"other">>}} end
    ),
    ?assertError(
        {w601_target_job_not_claimed, tag, {ok, #{<<"out_path">> := <<"other">>}}},
        ?P:claim_ordinary_until(conn, <<"target">>, 1, tag)
    ),
    ?assertEqual(1, meck:num_calls(?X, q, [conn, "BEGIN"])),
    ?assertEqual(1, meck:num_calls(?X, q, [conn, "ROLLBACK"])),
    ?assertEqual(0, meck:num_calls(?X, q, [conn, "COMMIT"])),
    ?assertEqual(0, meck:num_calls(attachment_gc_repo, v5_ack, '_')).

rollback_other_enterprise() ->
    meck:reset(?X),
    ok = meck:expect(
        ?X,
        e,
        fun(_, _, _) -> {ok, [], [{1, 2, 4, key, d, n, c, l, p, 1}]} end
    ),
    ?assertError(
        {w601_target_job_not_claimed, {enterprise, 1, 2, 3}, _},
        ?P:claim_eb_for(conn, 1, 2, 3)
    ),
    ?assertEqual(1, meck:num_calls(?X, q, [conn, "ROLLBACK"])),
    ?assertEqual(0, meck:num_calls(?X, q, [conn, "COMMIT"])),
    ?assertEqual(1, meck:num_calls(?X, e, '_')).

commit_matching_target() ->
    meck:reset(?X),
    ok = meck:expect(
        attachment_gc_repo,
        v5_claim,
        fun(_, _) -> {ok, #{<<"out_path">> => <<"target">>}} end
    ),
    ?assertMatch(
        {ok, #{<<"out_path">> := <<"target">>}},
        ?P:claim_ordinary_until(conn, <<"target">>, 1, tag)
    ),
    ?assertEqual(1, meck:num_calls(?X, q, [conn, "COMMIT"])),
    ?assertEqual(0, meck:num_calls(?X, q, [conn, "ROLLBACK"])).

rollback_claim_error() ->
    meck:reset(?X),
    ok = meck:expect(attachment_gc_repo, v5_claim, fun(_, _) -> erlang:error(db_failure) end),
    ?assertError(db_failure, ?P:claim_ordinary_until(conn, <<"target">>, 1, tag)),
    ?assertEqual(1, meck:num_calls(?X, q, [conn, "ROLLBACK"])),
    ?assertEqual(0, meck:num_calls(?X, q, [conn, "COMMIT"])).

commit_matching_enterprise() ->
    meck:reset(?X),
    ok = meck:expect(
        ?X,
        e,
        fun(_, _, _) -> {ok, [], [{1, 2, 3, key, d, n, c, l, p, 1}]} end
    ),
    ?assertMatch(
        {ok, #{org := 1, ws := 2, asset_id := 3}},
        ?P:claim_eb_for(conn, 1, 2, 3)
    ),
    ?assertEqual(1, meck:num_calls(?X, q, [conn, "COMMIT"])),
    ?assertEqual(0, meck:num_calls(?X, q, [conn, "ROLLBACK"])).
