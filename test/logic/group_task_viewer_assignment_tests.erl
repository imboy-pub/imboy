-module(group_task_viewer_assignment_tests).
-include_lib("eunit/include/eunit.hrl").

viewer_assignment_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [fun submitted/0, fun unassigned/0, fun outsider/0]}.

setup() ->
    meck:new(group_task_ds, [non_strict]),
    meck:new(group_ds, [non_strict]),
    meck:expect(group_task_ds, find_by_id, fun(100) ->
        {ok, #{<<"id">> => 100, <<"group_id">> => 123, <<"task_id">> => <<"task_owned">>}}
    end),
    meck:expect(group_ds, is_member, fun(_, _) -> true end),
    meck:expect(group_task_ds, assignment_find_by_task_and_user, fun(<<"task_owned">>, 1) ->
        {ok, #{<<"status">> => 2, <<"user_id">> => 1}}
    end).

cleanup(_) ->
    meck:unload(group_task_ds),
    meck:unload(group_ds).

submitted() ->
    {ok, Task} = group_task_logic:detail(100, 1),
    ?assertMatch(#{<<"status">> := 2, <<"user_id">> := 1}, maps:get(<<"my_assignment">>, Task)).

unassigned() ->
    meck:expect(group_task_ds, assignment_find_by_task_and_user, fun(_, _) ->
        {error, not_found}
    end),
    {ok, Task} = group_task_logic:detail(100, 1),
    ?assertEqual(null, maps:get(<<"my_assignment">>, Task)).

outsider() ->
    meck:expect(group_ds, is_member, fun(_, _) -> false end),
    ?assertMatch({error, _, _}, group_task_logic:detail(100, 1)),
    ?assertEqual(0, meck:num_calls(group_task_ds, assignment_find_by_task_and_user, '_')).
