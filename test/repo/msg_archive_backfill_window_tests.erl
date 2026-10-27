-module(msg_archive_backfill_window_tests).
-include_lib("eunit/include/eunit.hrl").

large_history_starts_from_zero_test() ->
    with_window(2500, 0, 1000, 50, [], fun() ->
        ?assertEqual({ok, [], 1000, true}, backfill(0, 50))
    end).

full_page_advances_only_through_committed_rows_test() ->
    Rows = [#{<<"conv_seq">> => Seq} || Seq <- [1, 2, 3]],
    with_window(2500, 0, 1000, 2, Rows, fun() ->
        ?assertEqual({ok, lists:sublist(Rows, 2), 2, true}, backfill(0, 2))
    end).

retention_holes_reach_window_end_test() ->
    Rows = [#{<<"conv_seq">> => 2001}],
    with_window(2500, 2000, 2500, 50, Rows, fun() ->
        ?assertEqual({ok, Rows, 2500, false}, backfill(2000, 50))
    end).

exact_page_reaches_window_end_test() ->
    Rows = [#{<<"conv_seq">> => Seq} || Seq <- [1, 2]],
    with_window(2500, 0, 1000, 2, Rows, fun() ->
        ?assertEqual({ok, Rows, 1000, true}, backfill(0, 2))
    end).

overshoot_cursor_never_reads_above_safe_head_test() ->
    with_window(20, 40, 20, 50, [], fun() ->
        ?assertEqual({ok, [], 40, false}, backfill(40, 50))
    end).

backfill(AfterSeq, Limit) ->
    msg_archive_repo:backfill(<<"c2c:100:200">>, AfterSeq, Limit, <<"c2c">>, 100, 200).

with_window(SafeHead, AfterSeq, EndSeq, Limit, Rows, Check) ->
    ok = meck:new(elib_pg, [passthrough, no_link]),
    try
        ok = meck:expect(elib_pg, with_tx, fun(Callback) -> Callback(connection) end),
        ok = meck:expect(elib_pg, query, fun(connection, Sql, Params) ->
            case binary:match(Sql, <<" AS formal_max">>) of
                nomatch ->
                    ?assertNotEqual(nomatch, binary:match(Sql, <<"conv_seq <= $3">>)),
                    ?assertNotEqual(nomatch, binary:match(Sql, <<"accepted_at">>)),
                    ?assertEqual([<<"c2c:100:200">>, AfterSeq, EndSeq, Limit + 1], Params),
                    {ok, Rows};
                _ ->
                    {ok, [#{<<"formal_max">> => SafeHead, <<"staging_min">> => 0}]}
            end
        end),
        Check()
    after
        meck:unload(elib_pg)
    end.
