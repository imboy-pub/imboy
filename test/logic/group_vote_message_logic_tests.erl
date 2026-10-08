-module(group_vote_message_logic_tests).
-include_lib("eunit/include/eunit.hrl").

vote() ->
    #{
        <<"vote_id">> => <<"abc">>,
        <<"group_id">> => 123,
        <<"creator_id">> => 456,
        <<"title">> => <<"去哪里旅游"/utf8>>
    }.

publish_test_() ->
    {foreach, fun() -> meck:new(msg_c2g_logic, [passthrough]) end,
        fun(_) -> meck:unload(msg_c2g_logic) end, [
            fun accepted/0,
            fun rejected/0,
            fun missing_ack/0,
            fun exception/0,
            fun collaboration_types/0
        ]}.

accepted() ->
    meck:expect(msg_c2g_logic, c2g, fun(Id, Uid, Data) ->
        ?assertEqual(456, Uid),
        ?assertEqual(<<"groupVote">>, maps:get(<<"msg_type">>, Data)),
        ?assertEqual(<<"123">>, maps:get(<<"to">>, Data)),
        ?assertEqual(<<"abc">>, maps:get(<<"vote_id">>, maps:get(<<"payload">>, Data))),
        self() ! {reply, #{<<"id">> => Id, <<"type">> => <<"C2G_SERVER_ACK">>}},
        ok
    end),
    {ok, Message} = group_vote_message_logic:publish(vote()),
    ?assertEqual(<<"456">>, maps:get(<<"from">>, Message)),
    ?assertEqual(<<"vote_abc">>, maps:get(<<"id">>, Message)).

rejected() ->
    meck:expect(msg_c2g_logic, c2g, fun(Id, _, _) ->
        self() ! {reply, #{<<"id">> => Id, <<"type">> => <<"C2G_ERROR">>}},
        ok
    end),
    ?assertEqual({error, message_rejected}, group_vote_message_logic:publish(vote())).

missing_ack() ->
    meck:expect(msg_c2g_logic, c2g, fun(_, _, _) -> ok end),
    ?assertEqual({error, message_not_accepted}, group_vote_message_logic:publish(vote())).

exception() ->
    meck:expect(msg_c2g_logic, c2g, fun(_, _, _) -> error(unavailable) end),
    ?assertEqual({error, message_not_accepted}, group_vote_message_logic:publish(vote())).

collaboration_types() ->
    meck:expect(msg_c2g_logic, c2g, fun(Id, _, Data) ->
        ?assertEqual(<<"123">>, maps:get(<<"to">>, Data)),
        self() ! {reply, #{<<"id">> => Id, <<"type">> => <<"C2G_SERVER_ACK">>}},
        ok
    end),
    lists:foreach(
        fun(Type) ->
            {ok, Message} = group_collab_message_logic:publish(
                123,
                456,
                Type,
                <<"abc">>,
                #{<<"id">> => <<"abc">>, <<"group_id">> => <<"123">>}
            ),
            ?assertEqual(Type, maps:get(<<"msg_type">>, Message)),
            ?assert(byte_size(maps:get(<<"id">>, Message)) =< 40)
        end,
        [<<"groupSchedule">>, <<"groupTask">>]
    ).
