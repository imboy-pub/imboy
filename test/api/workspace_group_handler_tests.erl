-module(workspace_group_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%% 走真实 handler / elib_param / elib_response / jsone，仅在 Cowboy 出口截获 raw body。
signed_terminal_cursor_is_json_null_test_() ->
    workspace_json_fixture(fun() ->
        lists:foreach(
            fun(MemberOnly) ->
                Page = maps:get(
                    <<"payload">>,
                    workspace_json_request([
                        {<<"pagination">>, <<"cursor">>}, {<<"member_only">>, MemberOnly}
                    ])
                ),
                ?assertEqual(
                    #{
                        <<"workspace_id">> => 100,
                        <<"list">> => [#{<<"id">> => 10}],
                        <<"has_more">> => false,
                        <<"next_cursor">> => null
                    },
                    Page
                ),
                case MemberOnly of
                    <<"0">> ->
                        ?assertEqual(
                            {100, 16#7FFFFFFFFFFFFFFF, 100, <<>>},
                            erase(json_signed_directory_fetch)
                        ),
                        ?assertEqual(undefined, erase(json_member_fetch));
                    <<"1">> ->
                        ?assertEqual({100, 1, 0, 100, false, <<>>}, erase(json_member_fetch)),
                        ?assertEqual(undefined, erase(json_signed_directory_fetch))
                end,
                ?assertEqual(undefined, erase(json_legacy_directory_fetch))
            end,
            [<<"0">>, <<"1">>]
        )
    end).

legacy_numeric_cursor_and_directory_envelope_test_() ->
    workspace_json_fixture(fun() ->
        MemberPage = maps:get(
            <<"payload">>,
            workspace_json_request([
                {<<"member_only">>, <<"1">>}, {<<"cursor">>, <<"123">>}
            ])
        ),
        ?assertEqual(
            #{
                <<"workspace_id">> => 100,
                <<"list">> => [#{<<"id">> => 10}],
                <<"has_more">> => false,
                <<"next_cursor">> => 0
            },
            MemberPage
        ),
        ?assertEqual({100, 1, 123, 100, false, <<>>}, erase(json_member_fetch)),
        DirectoryPage = maps:get(
            <<"payload">>,
            workspace_json_request([
                {<<"cursor">>, <<"123">>}
            ])
        ),
        %% 精确匹配旧目录 envelope：不得悄悄新增分页字段。
        ?assertEqual(
            #{
                <<"workspace_id">> => 100,
                <<"list">> => [#{<<"id">> => 10}]
            },
            DirectoryPage
        ),
        ?assertEqual({100, 100, <<>>}, erase(json_legacy_directory_fetch)),
        ?assertEqual(undefined, erase(json_member_fetch)),
        ?assertEqual(undefined, erase(json_signed_directory_fetch))
    end).

signed_cursor_workspace_and_kind_rejected_before_fetch_test_() ->
    workspace_json_fixture(fun() ->
        Ctx = #{scope => <<"workspace">>, workspace_id => 100, q => <<>>},
        lists:foreach(
            fun({MemberOnly, Kind, OtherKind}) ->
                lists:foreach(
                    fun({TokenKind, TokenCtx}) ->
                        {ok, Token} = list_cursor:issue(
                            1,
                            TokenKind,
                            TokenCtx#{upper => 10},
                            os:system_time(second)
                        ),
                        Response = workspace_json_request([
                            {<<"pagination">>, <<"cursor">>},
                            {<<"member_only">>, MemberOnly},
                            {<<"cursor">>, Token}
                        ]),
                        ?assertEqual(400, maps:get(<<"code">>, Response)),
                        ?assertEqual(undefined, erase(json_member_fetch)),
                        ?assertEqual(undefined, erase(json_signed_directory_fetch)),
                        ?assertEqual(undefined, erase(json_legacy_directory_fetch))
                    end,
                    [{Kind, Ctx#{workspace_id => 101}}, {OtherKind, Ctx}]
                )
            end,
            [
                {<<"0">>, <<"ws_group_directory">>, <<"ws_group_member">>},
                {<<"1">>, <<"ws_group_member">>, <<"ws_group_directory">>}
            ]
        )
    end).

workspace_json_fixture(Test) ->
    Mocks = [
        {auth_ds, [{current_uid, 1, fun(_) -> 1 end}]},
        {cowboy_req, [
            {binding, 2, fun(workspace_id, _) -> <<"100">> end},
            {method, 1, fun(_) -> <<"GET">> end},
            {parse_qs, 1, fun(#{qs := Qs}) -> Qs end},
            {reply, 4, fun(Status, Headers, Body, Req) ->
                Req#{status => Status, headers => Headers, raw_body => Body}
            end}
        ]},
        {workspace_logic, [{ensure_member, 2, fun(100, 1) -> {ok, <<"member">>} end}]},
        {config_ds, [
            {env, 3, fun(imboy, list_cursor_signing_key, _) ->
                <<"workspace-synthetic-signing-key-32-bytes-only">>
            end}
        ]},
        {group_logic, [
            {list_workspace_groups, 3, fun(WsId, Limit, Q) ->
                put(json_legacy_directory_fetch, {WsId, Limit, Q}),
                {ok, [#{<<"id">> => 10}]}
            end},
            {list_workspace_groups_before, 4, fun(WsId, Before, Limit, Q) ->
                put(json_signed_directory_fetch, {WsId, Before, Limit, Q}),
                {ok, [#{<<"id">> => 10}]}
            end},
            {list_member_workspace_groups, 6, fun(WsId, Uid, Cursor, Limit, Preview, Q) ->
                put(json_member_fetch, {WsId, Uid, Cursor, Limit, Preview, Q}),
                {ok, #{list => [#{<<"id">> => 10}], has_more => false, next_cursor => 0}}
            end}
        ]}
    ],
    {setup,
        fun() ->
            lists:foreach(
                fun({Mod, Exp}) ->
                    {ok, _} = meck_helper:setup_mock(Mod, Exp)
                end,
                Mocks
            )
        end,
        fun(_) ->
            meck_helper:cleanup_mocks([Mod || {Mod, _} <- Mocks])
        end,
        fun(_) -> ?_test(Test()) end}.

workspace_json_request(Qs) ->
    %% elib_param 的缓存是 request-local；同一测试进程模拟多请求须重置。
    erase({elib_param, qs_vals}),
    try
        Req = workspace_handler:handle_action(group_list, #{qs => Qs}, #{}),
        ?assertEqual(200, maps:get(status, Req)),
        ?assertEqual(
            <<"application/json; charset=utf-8">>,
            maps:get(<<"content-type">>, maps:get(headers, Req))
        ),
        Response = jsone:decode(maps:get(raw_body, Req)),
        ?assertEqual(
            [<<"code">>, <<"msg">>, <<"payload">>, <<"sv_ts">>],
            lists:sort(maps:keys(Response))
        ),
        ?assert(is_integer(maps:get(<<"sv_ts">>, Response))),
        case maps:get(<<"code">>, Response) of
            0 -> ?assertEqual(<<"success">>, maps:get(<<"msg">>, Response));
            _ -> ok
        end,
        Response
    after
        erase({elib_param, qs_vals})
    end.

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
