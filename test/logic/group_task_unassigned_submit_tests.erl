-module(group_task_unassigned_submit_tests).
-include_lib("eunit/include/eunit.hrl").

setup() ->
    meck:new(group_task_ds, [non_strict]),
    meck:new(group_ds, [non_strict]),
    erase(assignments),
    meck:expect(group_task_ds, find_by_task_id, fun(_) ->
        {ok, #{<<"id">> => 100, <<"group_id">> => 123, <<"deadline">> => undefined}}
    end),
    meck:expect(group_ds, is_member, fun(_, _) -> true end),
    meck:expect(group_ds, member_uids, fun(_) -> [1, 2] end),
    meck:expect(group_task_ds, assignment_list_by_task_id, fun(_, _, _) -> {ok, []} end),
    meck:expect(group_task_ds, assignment_find_by_task_and_user, fun(_, Uid) ->
        case get({assignment, Uid}) of
            undefined -> {error, not_found};
            Value -> {ok, Value}
        end
    end),
    meck:expect(group_task_ds, assignment_insert, fun(Data) ->
        Uid = maps:get(user_id, Data),
        put({assignment, Uid}, #{<<"id">> => Uid, <<"status">> => 0}),
        {ok, Uid, []}
    end),
    meck:expect(group_task_ds, assignment_update, fun(_, _) -> {ok, 1} end),
    erase({assignment, 1}),
    erase({assignment, 2}).

submit_test_() ->
    {foreach, fun setup/0,
        fun(_) ->
            meck:unload(group_task_ds),
            meck:unload(group_ds)
        end,
        [fun empty_assignments/0, fun explicit_assignments/0, fun outsider/0]}.

empty_assignments() ->
    ?assertEqual(ok, group_task_logic:submit(<<"task_abc">>, 1, #{})),
    ?assertEqual(2, meck:num_calls(group_task_ds, assignment_insert, '_')).

explicit_assignments() ->
    meck:expect(group_task_ds, assignment_list_by_task_id, fun(_, _, _) ->
        {ok, [#{<<"user_id">> => 2}]}
    end),
    ?assertMatch({error, _, _}, group_task_logic:submit(<<"task_abc">>, 1, #{})),
    ?assertEqual(0, meck:num_calls(group_task_ds, assignment_insert, '_')).

outsider() ->
    meck:expect(group_ds, is_member, fun(_, _) -> false end),
    ?assertMatch({error, _, _}, group_task_logic:submit(<<"task_abc">>, 1, #{})),
    ?assertEqual(0, meck:num_calls(group_task_ds, assignment_insert, '_')).
