-module(attach_pending_cleanup_tests).
-include_lib("eunit/include/eunit.hrl").

%% Unit orchestration only; PostgreSQL durability/locks and object bytes require
%% the separate integration suite. No application boot or external storage here.
cleanup_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun recorded_bucket/0,
        fun enqueue_failure/0,
        fun commit_failure/0,
        fun confirmation_wins/0,
        fun old_job_without_expired_pending/0,
        fun object_failure/0,
        fun completion_failure/0,
        fun acknowledgment_commit_failure/0,
        fun fallback_bucket/0,
        fun retry_schedule_failure/0,
        fun scan_failure/0,
        fun queue_scan_failure/0,
        fun competing_worker/0,
        fun orphan_handoff/0
    ]}.

setup() ->
    Modules = [
        attach_pending_repo,
        attachment_repo,
        attachment_gc_repo,
        elib_pg,
        elib_oss
    ],
    lists:foreach(fun(M) -> meck:new(M, [non_strict, no_link]) end, Modules),
    meck:expect(elib_pg, with_tx, fun transaction/1),
    meck:expect(attach_pending_repo, list_expired, fun(_) -> {ok, []} end),
    meck:expect(
        attach_pending_repo,
        claim_expired_tx,
        fun(_, Key, _) -> {ok, [pending_row(Key)]} end
    ),
    meck:expect(attachment_gc_repo, enqueue_tx, fun(_, _, _) -> ok end),
    meck:expect(attachment_gc_repo, pending, fun() -> {ok, []} end),
    meck:expect(
        attachment_gc_repo,
        lock_tx,
        fun(_, Key) -> {ok, [job_row(Key)]} end
    ),
    meck:expect(attachment_gc_repo, complete_tx, fun(_, _) -> ok end),
    meck:expect(attachment_gc_repo, defer, fun(_) -> {ok, 1} end),
    meck:expect(elib_oss, delete_object, fun(_, _) -> ok end),
    meck:expect(elib_oss, get_bucket, fun(_) -> <<"fallback">> end),
    Modules.

cleanup(Modules) -> lists:foreach(fun meck:unload/1, Modules).
transaction(Fun) ->
    try
        Fun(test_connection)
    catch
        throw:{abort_tx, Reason} -> {error, Reason}
    end.
pending_row(Key) -> #{<<"object_key">> => Key, <<"bucket">> => <<"recorded">>}.
job_row(Key) -> #{<<"path">> => Key, <<"bucket">> => <<"recorded">>}.
expired() ->
    meck:expect(
        attach_pending_repo,
        list_expired,
        fun(24) -> {ok, [pending_row(<<"key">>)]} end
    ).
due() ->
    meck:expect(
        attachment_gc_repo,
        pending,
        fun() -> {ok, [#{<<"path">> => <<"key">>}]} end
    ).
counts(Cleaned, Errors) ->
    ?assertEqual(
        {ok, #{cleaned => Cleaned, errors => Errors}},
        attachment_ds:pending_cleanup(24)
    ).
no_delete() -> ?assertNot(meck:called(elib_oss, delete_object, ['_', '_'])).
retry_called() -> ?assert(meck:called(attachment_gc_repo, defer, [<<"key">>])).

recorded_bucket() ->
    expired(),
    due(),
    meck:expect(elib_pg, with_tx, fun(Fun) ->
        Result = transaction(Fun),
        put(committed, true),
        Result
    end),
    meck:expect(elib_oss, delete_object, fun(<<"recorded">>, <<"key">>) ->
        ?assertEqual(true, get(committed)),
        ok
    end),
    counts(1, 0),
    ?assert(
        meck:called(
            attachment_gc_repo,
            enqueue_tx,
            [test_connection, <<"key">>, <<"recorded">>]
        )
    ),
    ?assertNot(meck:called(attach_pending_repo, delete_by_keys, ['_'])),
    erase(committed).

enqueue_failure() ->
    expired(),
    meck:expect(attachment_gc_repo, enqueue_tx, fun(_, _, _) -> {error, unavailable} end),
    counts(0, 1),
    no_delete().

commit_failure() ->
    expired(),
    meck:expect(elib_pg, with_tx, fun(Fun) ->
        ?assertEqual(queued, transaction(Fun)),
        {error, commit_failed}
    end),
    counts(0, 1),
    no_delete().

confirmation_wins() ->
    expired(),
    meck:expect(attach_pending_repo, claim_expired_tx, fun(_, _, _) -> {ok, []} end),
    counts(0, 0),
    no_delete(),
    ?assertNot(meck:called(attachment_gc_repo, enqueue_tx, ['_', '_', '_'])).

old_job_without_expired_pending() ->
    due(),
    counts(1, 0),
    ?assert(meck:called(elib_oss, delete_object, [<<"recorded">>, <<"key">>])),
    ?assert(meck:called(attachment_gc_repo, complete_tx, [test_connection, <<"key">>])).

object_failure() ->
    due(),
    meck:expect(elib_oss, delete_object, fun(_, _) -> {error, timeout} end),
    counts(0, 1),
    retry_called(),
    ?assertNot(meck:called(attachment_gc_repo, complete_tx, ['_', '_'])).

completion_failure() ->
    due(),
    meck:expect(attachment_gc_repo, complete_tx, fun(_, _) -> {error, commit_failed} end),
    counts(0, 1),
    retry_called(),
    ?assert(meck:called(elib_oss, delete_object, [<<"recorded">>, <<"key">>])).

acknowledgment_commit_failure() ->
    due(),
    meck:expect(elib_pg, with_tx, fun(Fun) ->
        ?assertEqual(cleaned, transaction(Fun)),
        {error, commit_failed}
    end),
    counts(0, 1),
    retry_called(),
    ?assert(
        meck:called(
            attachment_gc_repo,
            complete_tx,
            [test_connection, <<"key">>]
        )
    ),
    ?assert(meck:called(elib_oss, delete_object, [<<"recorded">>, <<"key">>])).

fallback_bucket() ->
    expired(),
    meck:expect(attach_pending_repo, claim_expired_tx, fun(_, Key, _) ->
        {ok, [#{<<"object_key">> => Key, <<"bucket">> => <<>>}]}
    end),
    counts(0, 0),
    ?assert(
        meck:called(
            attachment_gc_repo,
            enqueue_tx,
            [test_connection, <<"key">>, <<"fallback">>]
        )
    ).

retry_schedule_failure() ->
    due(),
    meck:expect(elib_oss, delete_object, fun(_, _) -> {error, timeout} end),
    meck:expect(attachment_gc_repo, defer, fun(_) -> {error, unavailable} end),
    counts(0, 1),
    retry_called(),
    ?assertNot(meck:called(attachment_gc_repo, complete_tx, ['_', '_'])).

scan_failure() ->
    meck:expect(attach_pending_repo, list_expired, fun(_) -> {error, scan_failed} end),
    ?assertEqual({error, scan_failed}, attachment_ds:pending_cleanup(24)),
    ?assertNot(meck:called(attachment_gc_repo, pending, [])),
    no_delete().

queue_scan_failure() ->
    meck:expect(attachment_gc_repo, pending, fun() -> {error, unavailable} end),
    ?assertEqual({error, unavailable}, attachment_ds:pending_cleanup(24)),
    no_delete().

competing_worker() ->
    due(),
    meck:expect(attachment_gc_repo, lock_tx, fun(_, _) -> {ok, []} end),
    counts(0, 0),
    no_delete(),
    ?assertNot(meck:called(attachment_gc_repo, defer, ['_'])).

orphan_handoff() ->
    meck:expect(
        attachment_repo,
        orphan_list_for_delete,
        fun(#{}) -> {ok, [#{<<"id">> => 1}]} end
    ),
    meck:expect(attachment_repo, delete_orphan_tx, fun(_, 1, #{}) ->
        {ok, [#{<<"path">> => <<"orphan">>, <<"scope">> => <<"group">>}]}
    end),
    meck:expect(elib_oss, get_bucket, fun(<<"group">>) -> <<"group-bucket">> end),
    ?assertEqual({ok, #{cleaned => 0, errors => 0}}, attachment_ds:orphan_cleanup(#{})),
    ?assert(
        meck:called(
            attachment_gc_repo,
            enqueue_tx,
            [test_connection, <<"orphan">>, <<"group-bucket">>]
        )
    ),
    no_delete().
