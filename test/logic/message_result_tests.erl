-module(message_result_tests).
-include_lib("eunit/include/eunit.hrl").

message_result_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun original_sender_and_conversation_are_bound/0,
        fun missing_archive_is_unknown/0,
        fun invalid_sequence_is_unknown/0,
        fun revoked_group_never_queries/0,
        fun group_membership_boundary_is_bound/0,
        fun invalid_parameters_never_query/0,
        fun database_failure_is_not_nonacceptance/0,
        fun repo_query_is_parameterized_and_scoped/0,
        fun unauthenticated_handler_rejects/0,
        fun handler_binds_authenticated_sender/0
    ]}.

setup() ->
    lists:foreach(
        fun(M) -> meck:new(M, [non_strict, no_link]) end,
        [msg_archive_ds, group_ds, elib_pg, cowboy_req, elib_response]
    ),
    meck:expect(msg_archive_ds, conv_key_c2c, fun(7, 8) -> <<"c2c:7:8">> end),
    meck:expect(msg_archive_ds, conv_key_c2g, fun(9) -> <<"c2g:9">> end).

cleanup(_) ->
    lists:foreach(
        fun meck:unload/1,
        [msg_archive_ds, group_ds, elib_pg, cowboy_req, elib_response]
    ).

original_sender_and_conversation_are_bound() ->
    meck:expect(msg_archive_ds, message_result, fun(<<"c2c:7:8">>, <<"original-id">>, 7, 0) ->
        {ok, [#{<<"conv_seq">> => 42}]}
    end),
    {ok, Payload} = query(),
    ?assertEqual(<<"committed">>, maps:get(<<"state">>, Payload)),
    ?assertEqual(42, maps:get(<<"conv_seq">>, Payload)),
    ?assertEqual(8, maps:get(<<"peer_id">>, Payload)),
    ?assertEqual(
        [<<"chat_type">>, <<"conv_seq">>, <<"message_id">>, <<"peer_id">>, <<"state">>],
        lists:sort(maps:keys(Payload))
    ).

missing_archive_is_unknown() ->
    meck:expect(msg_archive_ds, message_result, fun(_, _, _, _) -> {ok, []} end),
    ?assertMatch({ok, #{<<"state">> := <<"unknown">>}}, query()).

invalid_sequence_is_unknown() ->
    meck:expect(msg_archive_ds, message_result, fun(_, _, _, _) ->
        {ok, [#{<<"conv_seq">> => null}]}
    end),
    ?assertMatch({ok, #{<<"state">> := <<"unknown">>}}, query()).

revoked_group_never_queries() ->
    meck:expect(group_ds, authorize_group_history, fun(7, 9) -> {error, denied} end),
    ?assertMatch(
        {error, _, 403}, messaging_logic:message_result(7, <<"c2g">>, <<"9">>, <<"original-id">>)
    ),
    ?assertEqual(0, meck:num_calls(msg_archive_ds, message_result, '_')).

group_membership_boundary_is_bound() ->
    meck:expect(group_ds, authorize_group_history, fun(7, 9) -> {ok, #{start_seq => 80}} end),
    meck:expect(msg_archive_ds, message_result, fun(<<"c2g:9">>, <<"original-id">>, 7, 80) ->
        {ok, []}
    end),
    ?assertMatch(
        {ok, #{<<"state">> := <<"unknown">>}},
        messaging_logic:message_result(7, <<"c2g">>, <<"9">>, <<"original-id">>)
    ).

invalid_parameters_never_query() ->
    lists:foreach(
        fun({Type, Peer, Id}) ->
            ?assertMatch({error, _, 400}, messaging_logic:message_result(7, Type, Peer, Id))
        end,
        [
            {<<"c2c">>, <<"-1">>, <<"id">>},
            {<<"c2c">>, <<"bad">>, <<"id">>},
            {<<"c2c">>, <<"9223372036854775808">>, <<"id">>},
            {<<"c2c">>, <<"8">>, <<"id\n">>},
            {<<"other">>, <<"8">>, <<"id">>}
        ]
    ),
    ?assertEqual(0, meck:num_calls(msg_archive_ds, message_result, '_')).

database_failure_is_not_nonacceptance() ->
    meck:expect(msg_archive_ds, message_result, fun(_, _, _, _) -> {error, unavailable} end),
    ?assertMatch({error, _, 500}, query()).

repo_query_is_parameterized_and_scoped() ->
    meck:expect(elib_pg, query, fun(Sql, [<<"c2c:7:8">>, <<"original-id">>, 7, 0]) ->
        ?assertNotEqual(nomatch, binary:match(Sql, <<"from_id=$3">>)),
        ?assertNotEqual(nomatch, binary:match(Sql, <<"conv_seq >= $4">>)),
        {ok, []}
    end),
    ?assertEqual({ok, []}, msg_archive_repo:message_result(<<"c2c:7:8">>, <<"original-id">>, 7, 0)).

unauthenticated_handler_rejects() ->
    meck:expect(elib_response, error, fun(request, _, 401) -> rejected end),
    ?assertEqual(rejected, msg_handler:message_result(request, #{})),
    ?assertEqual(0, meck:num_calls(cowboy_req, parse_qs, '_')).

handler_binds_authenticated_sender() ->
    meck:expect(cowboy_req, parse_qs, fun(request) ->
        [
            {<<"chat_type">>, <<"c2c">>},
            {<<"peer_id">>, <<"8">>},
            {<<"message_id">>, <<"original-id">>},
            {<<"from_id">>, <<"999">>}
        ]
    end),
    meck:expect(msg_archive_ds, message_result, fun(<<"c2c:7:8">>, <<"original-id">>, 7, 0) ->
        {ok, [#{<<"conv_seq">> => 42}]}
    end),
    meck:expect(elib_response, success, fun(request, #{<<"state">> := <<"committed">>}) ->
        accepted
    end),
    ?assertEqual(
        {ok, accepted, #{current_uid => 7}},
        msg_handler:init(request, #{action => message_result, current_uid => 7})
    ).

query() -> messaging_logic:message_result(7, <<"c2c">>, <<"8">>, <<"original-id">>).
