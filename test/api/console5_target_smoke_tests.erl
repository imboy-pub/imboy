-module(console5_target_smoke_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% @doc
%%% Console-5 目标模块快速回归（无外部依赖）
%%%===================================================================

group_file_handle_action_false_passthrough_test() ->
    Req = #{req => group_file},
    ?assertEqual(Req, group_file_handler:handle_action(false, Req, #{})).

group_task_handle_action_false_passthrough_test() ->
    Req = #{req => group_task},
    ?assertEqual(Req, group_task_handler:handle_action(false, Req, #{})).

group_vote_handle_action_false_passthrough_test() ->
    Req = #{req => group_vote},
    ?assertEqual(Req, group_vote_handler:handle_action(false, Req, #{})).

group_album_handle_action_false_passthrough_test() ->
    Req = #{req => group_album},
    ?assertEqual(Req, group_album_handler:handle_action(false, Req, #{})).

mention_init_false_action_passthrough_test() ->
    Req = #{req => mention},
    {ok, Req1, State} = mention_handler:init(Req, #{action => false, current_uid => 10}),
    ?assertEqual(Req, Req1),
    ?assertEqual(10, maps:get(current_uid, State)).

group_schedule_init_unknown_action_passthrough_test() ->
    Req = #{req => schedule},
    {ok, Req1, State} = group_schedule_handler:init(Req, #{action => unknown_action, current_uid => 20}),
    ?assertEqual(Req, Req1),
    ?assertEqual(20, maps:get(current_uid, State)).
