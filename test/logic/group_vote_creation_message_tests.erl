-module(group_vote_creation_message_tests).
-include_lib("eunit/include/eunit.hrl").

modules() -> [group_vote_ds, group_ds, elib_id, group_vote_message_logic].
setup() ->
    [meck:new(M, [non_strict]) || M <- modules()],
    meck:expect(group_ds, is_member, fun(_, _) -> true end),
    meck:expect(elib_id, gen, fun(_) -> <<"abc">> end),
    meck:expect(group_vote_ds, insert_vote, fun(_) -> {ok, 1, []} end),
    meck:expect(group_vote_ds, insert_options_batch, fun(_) -> {ok, 2} end).

creation_test_() ->
    {foreach, fun setup/0, fun(_) -> [meck:unload(M) || M <- modules()] end, [
        fun success/0, fun failed_options/0, fun delivery_failed/0
    ]}.

create() ->
    group_vote_logic:create_vote(
        123,
        456,
        <<"旅游"/utf8>>,
        [#{option_text => <<"广州"/utf8>>}, #{option_text => <<"北京"/utf8>>}],
        #{},
        #{}
    ).

success() ->
    meck:expect(group_vote_message_logic, publish, fun(Vote) ->
        ?assertEqual(1, meck:num_calls(group_vote_ds, insert_options_batch, '_')),
        ?assertEqual(<<"abc">>, maps:get(<<"vote_id">>, Vote)),
        {ok, #{<<"id">> => <<"vote_abc">>}}
    end),
    {ok, Vote} = create(),
    ?assertEqual(#{<<"id">> => <<"vote_abc">>}, maps:get(<<"chat_message">>, Vote)),
    ?assertEqual(1, meck:num_calls(group_vote_message_logic, publish, '_')).

failed_options() ->
    meck:expect(group_vote_ds, insert_options_batch, fun(_) -> {error, unavailable} end),
    meck:expect(group_vote_message_logic, publish, fun(_) -> error(must_not_publish) end),
    ?assertEqual({error, unavailable}, create()),
    ?assertEqual(0, meck:num_calls(group_vote_message_logic, publish, '_')).

delivery_failed() ->
    meck:expect(group_vote_message_logic, publish, fun(_) -> {error, rejected} end),
    {ok, Vote} = create(),
    ?assertEqual(<<"abc">>, maps:get(<<"vote_id">>, Vote)),
    ?assertEqual(<<"failed">>, maps:get(<<"message_status">>, Vote)),
    ?assertEqual(false, maps:is_key(<<"chat_message">>, Vote)).
