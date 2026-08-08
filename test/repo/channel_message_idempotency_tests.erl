-module(channel_message_idempotency_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

add_with_request_id_returns_inserted_for_new_key_test_() ->
    ?WITH_MECKS(
        [
            {elib_tsid, [
                {'generate', 1, fun(channel_message) -> 9002 end}
            ]},
            {elib_pg, [
                {'execute', 2, fun(Sql, _Params) ->
                    SqlBin = iolist_to_binary(Sql),
                    ?assert(
                        re:run(
                            SqlBin, <<"ON CONFLICT \\(author_id, channel_id, request_id\\)"/utf8>>
                        ) =/= nomatch
                    ),
                    {ok, 1}
                end}
            ]}
        ],
        fun() ->
            Data = #{
                channel_id => 11,
                author_id => 1001,
                content => <<"hello">>,
                msg_type => <<"text">>,
                payload => <<"{}">>
            },
            ?assertEqual(
                {ok, 9002, inserted},
                channel_message_repo:add_with_request_id(Data, <<"req-1">>)
            )
        end
    ).

add_with_request_id_returns_duplicate_for_same_payload_test_() ->
    ?WITH_MECKS(
        [
            {elib_tsid, [
                {'generate', 1, fun(channel_message) -> 9003 end}
            ]},
            {elib_pg, [
                {'execute', 2, fun(_Sql, _Params) -> {ok, 0} end},
                {'query', 2, fun(
                    Sql,
                    [1001, 11, <<"req-1">>, <<"hello">>, <<"text">>, <<"{}">>]
                ) ->
                    SqlBin = iolist_to_binary(Sql),
                    ?assert(re:run(SqlBin, <<"payload = \\$6::jsonb">>) =/= nomatch),
                    {ok, [#{<<"id">> => 9002}]}
                end}
            ]}
        ],
        fun() ->
            Data = #{
                channel_id => 11,
                author_id => 1001,
                content => <<"hello">>,
                msg_type => <<"text">>,
                payload => <<"{}">>
            },
            ?assertEqual(
                {ok, 9002, duplicate},
                channel_message_repo:add_with_request_id(Data, <<"req-1">>)
            )
        end
    ).

add_with_request_id_rejects_payload_conflict_test_() ->
    ?WITH_MECKS(
        [
            {elib_tsid, [
                {'generate', 1, fun(channel_message) -> 9004 end}
            ]},
            {elib_pg, [
                {'execute', 2, fun(_Sql, _Params) -> {ok, 0} end},
                {'query', 2, fun(_Sql, _Params) -> {ok, []} end}
            ]}
        ],
        fun() ->
            Data = #{
                channel_id => 11,
                author_id => 1001,
                content => <<"different">>,
                msg_type => <<"text">>,
                payload => <<"{}">>
            },
            ?assertEqual(
                {error, request_id_conflict},
                channel_message_repo:add_with_request_id(Data, <<"req-1">>)
            )
        end
    ).
