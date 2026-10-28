-module(workspace_group_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

member_group_dispatch_test_() ->
    ?WITH_MECKS(
        [
            {auth_ds, [{'current_uid', 1, fun(_) -> 1 end}]},
            %% W07.a/b：group_list 现调 binary(q,...) 与 parse_qs 判
            %% pagination=cursor；本套件不测这两者，给缺省直通 mock。
            {cowboy_req, [
                {'binding', 2, fun(workspace_id, _) -> <<"100">> end},
                {'parse_qs', 1, fun(_Req) -> [] end}
            ]},
            {elib_param, [
                {'int', 3, fun(Key, Req, Default) -> {ok, maps:get(Key, Req, Default)} end},
                {'binary', 3, fun(_Key, _Req, Default) -> {ok, Default} end}
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
                %% W07.a：list_workspace_groups/3（末位服务端 q）。
                {'list_workspace_groups', 3, fun(100, Limit, _Q) ->
                    put(directory_limit, Limit),
                    {ok, []}
                end},
                %% W07.a：list_member_workspace_groups/6（末位服务端 q）。
                {'list_member_workspace_groups', 6, fun(100, 1, Cursor, Limit, Preview, _Q) ->
                    put(member_page_call, {Cursor, Limit, Preview}),
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
            ?assertEqual({123, 200, false}, erase(member_page_call)),
            ?assertEqual(
                {error, 400}, workspace_handler:handle_action(group_list, #{member_only => 2}, #{})
            ),
            ?assertEqual(undefined, erase(directory_limit)),
            ?assertEqual(undefined, erase(member_page_call)),
            workspace_handler:handle_action(group_list, #{member_only => 1, preview => 1}, #{}),
            ?assertEqual({0, 100, true}, erase(member_page_call)),
            ?assertEqual(
                {error, 400}, workspace_handler:handle_action(group_list, #{preview => 1}, #{})
            ),
            ?assertEqual(
                {error, 400},
                workspace_handler:handle_action(group_list, #{member_only => 1, preview => 2}, #{})
            ),
            put(deny_workspace, true),
            ?assertEqual(
                {error, 403}, workspace_handler:handle_action(group_list, #{member_only => 1}, #{})
            ),
            ?assertEqual(undefined, erase(member_page_call))
        end
    ).
