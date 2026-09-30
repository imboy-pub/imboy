-module(workspace_group_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

member_group_dispatch_test_() ->
    ?WITH_MECKS(
        [
            {auth_ds, [{'current_uid', 1, fun(_) -> 1 end}]},
            {cowboy_req, [{'binding', 2, fun(workspace_id, _) -> <<"100">> end}]},
            {elib_param, [
                {'int', 3, fun(Key, Req, Default) -> {ok, maps:get(Key, Req, Default)} end}
            ]},
            {workspace_logic, [
                {'ensure_member', 2, fun(100, 1) ->
                    case get(deny_workspace) of
                        true -> {error, {403, <<"denied">>}};
                        _ -> {ok, <<"member">>}
                    end
                end}
            ]},
            {group_logic, [
                {'list_workspace_groups', 2, fun(100, Limit) ->
                    put(directory_limit, Limit),
                    {ok, []}
                end},
                {'list_member_workspace_groups', 4, fun(100, 1, Cursor, Limit) ->
                    put(member_page_call, {Cursor, Limit}),
                    {ok, #{list => [], has_more => false, next_cursor => 0}}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_, Payload) -> Payload end},
                {'error', 3, fun(_, _, Code) -> {error, Code} end}
            ]}
        ],
        fun() ->
            workspace_handler:handle_action(group_list, #{limit => 17}, #{}),
            ?assertEqual(17, erase(directory_limit)),
            ?assertEqual(undefined, erase(member_page_call)),
            ?assertMatch(
                #{workspace_id := 100, has_more := false},
                workspace_handler:handle_action(
                    group_list, #{member_only => 1, cursor => 123, limit => 500}, #{}
                )
            ),
            ?assertEqual({123, 200}, erase(member_page_call)),
            ?assertEqual(
                {error, 400}, workspace_handler:handle_action(group_list, #{member_only => 2}, #{})
            ),
            ?assertEqual(undefined, erase(directory_limit)),
            ?assertEqual(undefined, erase(member_page_call)),
            put(deny_workspace, true),
            ?assertEqual(
                {error, 403}, workspace_handler:handle_action(group_list, #{member_only => 1}, #{})
            ),
            ?assertEqual(undefined, erase(member_page_call))
        end
    ).
