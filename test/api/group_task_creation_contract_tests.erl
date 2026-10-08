-module(group_task_creation_contract_tests).
-include_lib("eunit/include/eunit.hrl").

modules() -> [elib_param, group_task_logic, elib_response, group_ds, group_collab_message_logic].
setup() ->
    [meck:new(M, [non_strict]) || M <- modules()],
    meck:expect(elib_param, post, fun(_) ->
        #{<<"group_id">> => <<"123">>, <<"title">> => <<"作业"/utf8>>}
    end),
    meck:expect(group_task_logic, create, fun(123, 456, _, _) -> {ok, 42} end),
    meck:expect(group_task_logic, assign, fun(42, Uids, 456) ->
        put(assigned, Uids),
        ok
    end),
    meck:expect(group_ds, member_uids, fun(123) -> [456, 789] end),
    meck:expect(group_collab_message_logic, publish, fun(123, 456, <<"groupTask">>, <<"42">>, _) ->
        {ok, #{<<"id">> => <<"message_42">>}}
    end),
    meck:expect(elib_response, success, fun(_, Payload, _) -> {success, Payload} end),
    meck:expect(elib_response, error, fun(_, _, Code) -> {error, Code} end).

create() -> group_task_handler:handle_action(create, #{}, #{current_uid => 456}).

creation_test_() ->
    {foreach, fun setup/0, fun(_) -> [meck:unload(M) || M <- modules()] end, [
        fun defaults/0, fun specified/0, fun assignment_failed/0, fun publication_failed/0
    ]}.

defaults() ->
    {success, Payload} = create(),
    ?assertEqual([456, 789], get(assigned)),
    ?assertEqual(42, maps:get(<<"task_id">>, Payload)),
    ?assert(maps:is_key(<<"chat_message">>, Payload)).

specified() ->
    meck:expect(elib_param, post, fun(_) ->
        #{
            <<"group_id">> => <<"123">>,
            <<"title">> => <<"作业"/utf8>>,
            <<"user_ids">> => [789]
        }
    end),
    ?assertMatch({success, _}, create()),
    ?assertEqual([789], get(assigned)),
    ?assertEqual(0, meck:num_calls(group_ds, member_uids, '_')).

assignment_failed() ->
    meck:expect(group_task_logic, assign, fun(_, _, _) -> {error, <<"failed">>, 980} end),
    ?assertEqual({error, 980}, create()),
    ?assertEqual(0, meck:num_calls(group_collab_message_logic, publish, '_')).

publication_failed() ->
    meck:expect(group_collab_message_logic, publish, fun(_, _, _, _, _) -> {error, rejected} end),
    {success, Payload} = create(),
    ?assertEqual(42, maps:get(<<"task_id">>, Payload)),
    ?assertEqual(<<"failed">>, maps:get(<<"message_status">>, Payload)).
