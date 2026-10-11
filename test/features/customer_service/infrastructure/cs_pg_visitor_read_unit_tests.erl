-module(cs_pg_visitor_read_unit_tests).
-include_lib("eunit/include/eunit.hrl").

scope() ->
    #{
        session_id => 3,
        conversation_id => 4,
        contact_id => 5,
        installation_id => 6,
        token_id => 7,
        origin => <<"https://widget.test">>,
        request_host => undefined
    }.
rows(Ids) -> [#{<<"id">> => Id} || Id <- Ids].
valid() ->
    #{
        <<"installation_status">> => <<"active">>,
        <<"revoked">> => false,
        <<"expired">> => false,
        <<"read_at">> => <<"2026-10-11T01:00:00.000000Z">>
    }.
receipt(Id) -> #{<<"message_id">> => Id, <<"read_at">> => <<"2026-10-11T01:00:00.000000Z">>}.
base(Ids) ->
    [
        {ok, [#{<<"id">> => 7, <<"allowed_origins">> => [<<"https://widget.test">>]}]},
        {ok, [#{<<"id">> => 3}]},
        {ok, rows(Ids)},
        {ok, [valid()]}
    ].
mock(Replies, Fun) ->
    meck:new(elib_pg, [non_strict, no_link]),
    meck:new(cs_pg_seat, [non_strict, no_link]),
    meck:expect(cs_pg_seat, insert_event_in, fun(synthetic_conn, 1, Event) ->
        put(hint, Event),
        {ok, 88}
    end),
    put(replies, Replies),
    put(queries, []),
    meck:expect(elib_pg, with_tx, fun(Tx) ->
        try
            Tx(synthetic_conn)
        catch
            throw:{rollback, E} -> {rollback, E}
        end
    end),
    meck:expect(elib_pg, query, fun(_, Sql, Params) ->
        put(queries, get(queries) ++ [{Sql, Params}]),
        [Reply | Rest] = get(replies),
        put(replies, Rest),
        Reply
    end),
    try
        Fun()
    after
        meck:unload(elib_pg),
        meck:unload(cs_pg_seat),
        erase(hint),
        erase(replies),
        erase(queries)
    end.

confirm_success_preserves_requested_first_times_test() ->
    Replies =
        base([10, 20]) ++
            [
                {ok, 2},
                {ok, [#{<<"highest_confirmed_id">> => 30}]},
                {ok, [receipt(10), receipt(20)]}
            ],
    mock(Replies, fun() ->
        ?assertEqual(
            {ok, #{
                session_id => 3,
                highest_confirmed_id => 30,
                receipts => [
                    #{message_id => 10, read_at => maps:get(<<"read_at">>, receipt(10))},
                    #{message_id => 20, read_at => maps:get(<<"read_at">>, receipt(20))}
                ]
            }},
            cs_pg_visitor_read:confirm_visitor_message_reads(1, 2, scope(), [20, 10, 10], 0)
        ),
        Q = get(queries),
        ?assertEqual(7, length(Q)),
        ?assertMatch(
            #{action := <<"message.read">>, session_id := 3, detail := #{<<"message_id">> := 20}},
            get(hint)
        ),
        {Credential, _} = lists:nth(1, Q),
        ?assertNotEqual(nomatch, binary:match(Credential, <<"FOR SHARE OF i,t">>)),
        {Session, _} = lists:nth(2, Q),
        ?assertNotEqual(nomatch, binary:match(Session, <<"original.widget_installation_id=$5">>)),
        ?assertNotEqual(nomatch, binary:match(Session, <<"original.contact_id=s.contact_id">>)),
        {Messages, _} = lists:nth(3, Q),
        ?assertNotEqual(nomatch, binary:match(Messages, <<"ORDER BY id FOR SHARE">>)),
        {Clock, _} = lists:nth(4, Q),
        ?assertNotEqual(nomatch, binary:match(Clock, <<"clock_timestamp()">>)),
        {Insert, _} = lists:nth(5, Q),
        ?assertNotEqual(nomatch, binary:match(Insert, <<"ON CONFLICT DO NOTHING">>)),
        {Cursor, _} = lists:nth(6, Q),
        ?assertNotEqual(nomatch, binary:match(Cursor, <<"GREATEST">>))
    end).

batch_missing_or_cross_message_is_atomic_test() ->
    mock(
        [
            {ok, [#{<<"allowed_origins">> => [<<"https://widget.test">>]}]},
            {ok, [#{}]},
            {ok, rows([10])}
        ],
        fun() ->
            ?assertEqual(
                {error, not_found},
                cs_pg_visitor_read:confirm_visitor_message_reads(1, 2, scope(), [10, 99], 0)
            ),
            ?assertEqual(3, length(get(queries)))
        end
    ).

credential_and_origin_fail_closed_test_() ->
    [
        {integer_to_list(N), fun() ->
            mock(Replies, fun() ->
                ?assertEqual(
                    {error, Error},
                    cs_pg_visitor_read:confirm_visitor_message_reads(1, 2, scope(), [10], 0)
                ),
                ?assertEqual(length(Replies), length(get(queries)))
            end)
        end}
     || {N, Replies, Error} <- [
            {1, [{ok, []}], visit_token_invalid},
            {2, [{ok, [#{<<"allowed_origins">> => [<<"https://widget.test">>]}]}, {ok, []}],
                not_found},
            {3,
                lists:sublist(base([10]), 3) ++
                    [{ok, [#{<<"installation_status">> => <<"revoked">>}]}],
                installation_revoked},
            {4,
                lists:sublist(base([10]), 3) ++
                    [{ok, [#{<<"installation_status">> => <<"active">>, <<"revoked">> => true}]}],
                token_revoked},
            {5,
                lists:sublist(base([10]), 3) ++
                    [
                        {ok, [
                            #{
                                <<"installation_status">> => <<"active">>,
                                <<"revoked">> => false,
                                <<"expired">> => true
                            }
                        ]}
                    ],
                token_expired}
        ]
    ].

storage_error_rolls_back_test() ->
    mock(base([10]) ++ [{error, synthetic_write_failure}], fun() ->
        ?assertMatch(
            {error, _}, cs_pg_visitor_read:confirm_visitor_message_reads(1, 2, scope(), [10], 0)
        ),
        ?assertEqual(5, length(get(queries)))
    end).

empty_or_invalid_internal_batch_no_queries_test_() ->
    [
        fun() ->
            mock([], fun() ->
                ?assertMatch(
                    {error, _},
                    cs_pg_visitor_read:confirm_visitor_message_reads(1, 2, scope(), Ids, 0)
                ),
                ?assertEqual([], get(queries))
            end)
        end
     || Ids <- [[], [0], [-1], [1.0], [<<"1">>], lists:seq(1, 201)]
    ].

hint_failure_rolls_back_receipts_and_cursor_test() ->
    mock(
        base([10]) ++ [{ok, 1}, {ok, [#{<<"highest_confirmed_id">> => 10}]}, {ok, [receipt(10)]}],
        fun() ->
            meck:expect(cs_pg_seat, insert_event_in, fun(_, _, _) ->
                {error, synthetic_hint_failure}
            end),
            ?assertEqual(
                {error, {visitor_read_hint_failed, synthetic_hint_failure}},
                cs_pg_visitor_read:confirm_visitor_message_reads(1, 2, scope(), [10], 0)
            )
        end
    ).

receipt_readback_failure_rolls_back_test() ->
    mock(base([10]) ++ [{ok, []}, {ok, [#{<<"highest_confirmed_id">> => 10}]}, {ok, []}], fun() ->
        ?assertEqual(
            {error, visitor_read_storage_failure},
            cs_pg_visitor_read:confirm_visitor_message_reads(1, 2, scope(), [10], 0)
        ),
        ?assertEqual(undefined, get(hint))
    end).
invalid_scope_never_queries_test_() ->
    [
        fun() ->
            mock([], fun() ->
                ?assertMatch(
                    {error, _},
                    cs_pg_visitor_read:confirm_visitor_message_reads(1, 2, Scope, [10], 0)
                ),
                ?assertEqual([], get(queries))
            end)
        end
     || Scope <- [
            undefined, #{}, (scope())#{installation_id => undefined}, (scope())#{token_id => 0}
        ]
    ].
fetch_receipts_does_not_infer_lower_ids_test() ->
    meck:new(elib_pg, [non_strict, no_link]),
    try
        meck:expect(elib_pg, query, fun(Sql, [1, 2, 3, 5, 4, [10, 20]]) ->
            ?assertEqual(nomatch, binary:match(Sql, <<"read_cursor">>)),
            {ok, [receipt(20)]}
        end),
        ?assertEqual(
            {ok, [#{message_id => 20, read_at => maps:get(<<"read_at">>, receipt(20))}]},
            cs_pg_visitor_read:fetch_visitor_message_reads(1, 2, scope(), [10, 20])
        ),
        ?assertEqual({ok, []}, cs_pg_visitor_read:fetch_visitor_message_reads(1, 2, scope(), [])),
        ?assertMatch({error, _}, cs_pg_visitor_read:fetch_visitor_message_reads(1, 2, #{}, [])),
        ?assertMatch(
            {error, _}, cs_pg_visitor_read:fetch_visitor_message_reads(1, 2, scope(), [-1])
        ),
        meck:expect(elib_pg, query, fun(_, _) -> {error, synthetic_read_failure} end),
        ?assertMatch(
            {error, _}, cs_pg_visitor_read:fetch_visitor_message_reads(1, 2, scope(), [10])
        )
    after
        meck:unload(elib_pg)
    end.
