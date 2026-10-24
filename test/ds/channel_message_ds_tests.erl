-module(channel_message_ds_tests).
-include_lib("eunit/include/eunit.hrl").

get_stats_test_() ->
    {setup, fun() -> meck:new(elib_pg, [passthrough, no_link, unstick]) end,
        fun(_) -> meck:unload(elib_pg) end, fun(_) ->
            Cases = [
                {integer_counts, row(1, 2), {ok, 1, 2}},
                {numeric_text_zero, row(0, <<"0">>), {ok, 0, 0}},
                {numeric_text_one, row(1, <<"1">>), {ok, 1, 1}},
                {arbitrary_precision, row(1, <<"18446744073709551616">>),
                    {ok, 1, 18446744073709551616}},
                {negative_integer, row(1, -1), invalid()},
                {negative_text, row(1, <<"-1">>), invalid()},
                {fractional_text, row(1, <<"1.5">>), invalid()},
                {float_value, row(1, 1.0), invalid()},
                {null_value, row(1, null), invalid()},
                {missing_field, {ok, #{<<"total_messages">> => 1}}, invalid()},
                {database_error, {error, synthetic_database_failure},
                    {error, synthetic_database_failure}}
            ],
            [
                {
                    atom_to_list(Name),
                    ?_test(begin
                        meck:expect(elib_pg, one, fun(_, [997301]) -> Response end),
                        ?assertEqual(Expected, channel_message_ds:get_stats(997301)),
                        ?assert(meck:validate(elib_pg))
                    end)
                }
             || {Name, Response, Expected} <- Cases
            ]
        end}.

stats_direct_caller_test_() ->
    {setup,
        fun() ->
            ok = meck:new(elib_pg, [passthrough, no_link, unstick]),
            meck:new(channel_ds, [passthrough, no_link, unstick])
        end,
        fun(_) ->
            meck:unload(channel_ds),
            meck:unload(elib_pg)
        end,
        fun(_) ->
            [
                ?_test(begin
                    setup_channel(),
                    meck:expect(elib_pg, one, fun(_, [997301]) -> row(1, <<"1">>) end),
                    {ok, Stats} = channel_logic_stats:get_channel_stats_admin(<<"997301">>),
                    ?assertEqual(1, maps:get(<<"total_messages">>, Stats)),
                    ?assertEqual(1, maps:get(<<"total_views">>, Stats)),
                    Encoded = jsone:encode(Stats),
                    Decoded = jsone:decode(Encoded, [{object_format, map}]),
                    ?assertEqual(1, maps:get(<<"total_views">>, Decoded))
                end),
                ?_test(begin
                    setup_channel(),
                    meck:expect(elib_pg, one, fun(_, [997301]) -> row(1, <<"bad">>) end),
                    ?assertMatch(
                        {error, _},
                        channel_logic_stats:get_channel_stats_admin(<<"997301">>)
                    )
                end)
            ]
        end}.

setup_channel() ->
    meck:expect(channel_ds, find_by_id, fun(997301, _) ->
        #{
            <<"id">> => 997301,
            <<"name">> => <<"synthetic">>,
            <<"subscriber_count">> => 1
        }
    end),
    meck:expect(channel_ds, get_reaction_count, fun(997301) -> {ok, 0} end).

row(Messages, Views) ->
    {ok, #{<<"total_messages">> => Messages, <<"total_views">> => Views}}.

invalid() ->
    {error, invalid_channel_message_stats}.

%% SQL shape/parameter test, not a PostgreSQL behavioral oracle.
message_page_sql_boundary_test_() ->
    {setup, fun() -> meck:new(elib_pg, [passthrough, no_link, unstick]) end,
        fun(_) -> meck:unload(elib_pg) end, fun(_) ->
            ?_test(begin
                At = <<"2026-10-24T01:02:03.123456Z">>,
                meck:expect(elib_pg, query, fun(Sql, [42, At0, CursorId, 21]) ->
                    EncodedId = epgsql_codec_integer:encode(CursorId, int8, []),
                    ?assertEqual(
                        9223372036854775807,
                        epgsql_codec_integer:decode(EncodedId, int8, [])
                    ),
                    ?assertEqual(At, At0),
                    ?assertNotEqual(
                        nomatch,
                        binary:match(
                            Sql,
                            <<"(created_at,id) < ($2::timestamptz,$3::bigint)">>
                        )
                    ),
                    ?assertNotEqual(
                        nomatch,
                        binary:match(
                            Sql,
                            <<"ORDER BY created_at DESC,id DESC LIMIT $4">>
                        )
                    ),
                    ?assertNotEqual(nomatch, binary:match(Sql, <<"SS.US">>)),
                    {ok, []}
                end),
                ?assertEqual(
                    {ok, []},
                    channel_message_ds:list_by_channel_page(
                        42, {At, <<"9223372036854775807">>}, 20
                    )
                )
            end)
        end}.
