-module(workspace_attachment_logic_tests).
-include_lib("eunit/include/eunit.hrl").

filter_validation_test() ->
    ?assertMatch(
        {error, {400, _}},
        workspace_attachment_logic:filters(#{<<"group_id">> => <<"1">>, <<"channel_id">> => <<"2">>})
    ),
    ?assertMatch(
        {error, {400, _}},
        workspace_attachment_logic:filters(#{
            <<"from_ms">> => <<"200">>, <<"until_ms">> => <<"100">>
        })
    ),
    ?assertMatch(
        {error, {400, _}}, workspace_attachment_logic:filters(#{<<"user_id">> => <<"1 OR 1=1">>})
    ),
    {ok, Filter} = workspace_attachment_logic:filters(#{<<"keyword">> => <<"100%_!">>}),
    ?assertEqual(<<"%100!%!_!!%">>, maps:get(keyword, Filter)).

projection_hides_storage_test() ->
    Row = #{
        <<"id">> => 1,
        <<"name">> => <<"test.pdf">>,
        <<"path">> => <<"private/key">>,
        <<"url">> => <<"secret">>,
        <<"scope">> => <<"group">>
    },
    ?assertEqual(
        #{<<"id">> => 1, <<"name">> => <<"test.pdf">>, <<"scope">> => <<"group">>},
        workspace_attachment_logic:project(Row)
    ).

authorization_test() ->
    Modules = [workspace_logic, workspace_attachment_ds, attach_logic],
    lists:foreach(fun(M) -> meck:new(M, [non_strict, no_link]) end, Modules),
    try
        meck:expect(workspace_logic, ensure_member, fun
            (10, 1) -> {ok, <<"member">>};
            (_, _) -> {error, {403, <<"denied">>}}
        end),
        Rows = [
            #{<<"id">> => 2, <<"name">> => <<"allowed">>, <<"path">> => <<"key">>},
            #{<<"id">> => 1, <<"name">> => <<"hidden">>}
        ],
        meck:expect(workspace_attachment_ds, scan, fun(10, _) -> {ok, Rows} end),
        meck:expect(attach_logic, authorize, fun(1, R) -> maps:get(<<"id">>, R) =:= 2 end),
        ?assertMatch({error, {403, _}}, workspace_attachment_logic:search(2, 10, #{})),
        ?assertEqual(0, meck:num_calls(workspace_attachment_ds, scan, '_')),
        {ok, #{items := Items, workspace_id := 10}} = workspace_attachment_logic:search(1, 10, #{}),
        ?assertEqual([#{<<"id">> => 2, <<"name">> => <<"allowed">>}], Items),
        meck:expect(workspace_attachment_ds, scan, fun(10, _) -> {ok, [hd(Rows)]} end),
        meck:expect(attach_logic, view_url, fun(1, <<"key">>) ->
            {ok, <<"https://fixture.invalid/signed">>}
        end),
        ?assertMatch({ok, _}, workspace_attachment_logic:open(1, 10, 2)),
        meck:expect(workspace_attachment_ds, scan, fun(10, _) ->
            {ok, [(hd(Rows))#{<<"cipher">> => <<"AES-256-GCM">>}]}
        end),
        ?assertMatch({error, {409, _}}, workspace_attachment_logic:open(1, 10, 2)),
        ?assertEqual(1, meck:num_calls(attach_logic, view_url, '_')),
        meck:expect(workspace_attachment_ds, scan, fun(10, _) -> {error, unavailable} end),
        ?assertMatch({error, {503, _}}, workspace_attachment_logic:search(1, 10, #{}))
    after
        lists:foreach(fun meck:unload/1, Modules)
    end.
