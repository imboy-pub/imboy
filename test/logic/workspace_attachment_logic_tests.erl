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

retention_projection_preserves_status_without_storage_test() ->
    Row = #{
        <<"id">> => 1,
        <<"retention_state">> => <<"retained">>,
        <<"retained_at">> => <<"2026-10-09T10:00:00Z">>,
        <<"path">> => <<"private/key">>,
        <<"bucket">> => <<"private-bucket">>,
        <<"url">> => <<"secret">>,
        <<"organization_internal_note">> => <<"not-public">>
    },
    ?assertEqual(
        maps:with([<<"id">>, <<"retention_state">>, <<"retained_at">>], Row),
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

talk_record_index_acl_and_locator_test() ->
    Modules = [workspace_logic, organization_logic, workspace_attachment_ds, attach_logic],
    lists:foreach(fun(M) -> meck:new(M, [non_strict, no_link]) end, Modules),
    try
        meck:expect(workspace_logic, ensure_member, fun
            (10, 1) -> {ok, member};
            (_, _) -> {error, {403, <<"denied">>}}
        end),
        meck:expect(organization_logic, detail, fun
            (1, 100) -> {ok, #{<<"status">> => <<"active">>}};
            (_, _) -> {error, {403, <<"denied">>}}
        end),
        Row = #{
            <<"id">> => 77,
            <<"scope">> => <<"talk_record">>,
            <<"scope_ref">> => <<"50">>,
            <<"record_id">> => 50,
            <<"name">> => <<"audio.aac">>,
            <<"source_name">> => <<"stale title">>,
            <<"workspace_id">> => null,
            <<"organization_id">> => 100,
            <<"path">> => <<"never-expose">>,
            <<"attachment_role">> => <<"assembled_audio">>
        },
        meck:expect(workspace_attachment_ds, scan, fun(_, _) -> {ok, [Row]} end),
        meck:expect(workspace_attachment_ds, scan_organization, fun(_, _) -> {ok, [Row]} end),
        meck:expect(workspace_attachment_ds, can_govern, fun(_, _) -> false end),
        meck:expect(workspace_attachment_ds, visible_record, fun
            (1, 50) ->
                {ok, #{<<"title">> => <<"current title">>}};
            (_, _) ->
                {error, not_found}
        end),
        {ok, #{items := [Item], organization_id := 100, workspace_id := null}} =
            workspace_attachment_logic:search_organization(1, 100, #{}),
        ?assertEqual(<<"current title">>, maps:get(<<"source_name">>, Item)),
        ?assertEqual(false, maps:get(<<"can_govern">>, Item)),
        meck:expect(workspace_attachment_ds, can_govern, fun(1, 100) -> true end),
        {ok, #{items := [Governed]}} = workspace_attachment_logic:search_organization(1, 100, #{}),
        ?assertEqual(true, maps:get(<<"can_govern">>, Governed)),
        ?assertEqual(<<"talk_record">>, maps:get(<<"open_kind">>, Item)),
        ?assertNot(maps:is_key(<<"path">>, Item)),
        ?assertEqual(
            {ok, #{open_kind => <<"talk_record">>, record_id => 50}},
            workspace_attachment_logic:open(1, 10, 77)
        ),
        ?assertEqual(0, meck:num_calls(attach_logic, view_url, '_')),
        meck:expect(workspace_attachment_ds, visible_record, fun(_, _) -> {error, not_found} end),
        ?assertMatch({ok, #{items := []}}, workspace_attachment_logic:search(1, 10, #{})),
        ?assertMatch({error, {404, _}}, workspace_attachment_logic:open(1, 10, 77)),
        ?assertMatch({error, {403, _}}, workspace_attachment_logic:search_organization(2, 100, #{}))
    after
        lists:foreach(fun meck:unload/1, Modules)
    end.

raw_cursor_survives_acl_filtered_empty_page_test() ->
    Modules = [workspace_logic, workspace_attachment_ds, attach_logic],
    lists:foreach(fun(M) -> meck:new(M, [non_strict, no_link]) end, Modules),
    try
        meck:expect(workspace_logic, ensure_member, fun(_, _) -> {ok, member} end),
        meck:expect(workspace_attachment_ds, scan, fun(_, _) ->
            {ok, [#{<<"id">> => Id} || Id <- lists:seq(200, 101, -1)]}
        end),
        meck:expect(attach_logic, authorize, fun(_, _) -> false end),
        ?assertMatch(
            {ok, #{items := [], next_cursor := 101}},
            workspace_attachment_logic:search(1, 10, #{})
        ),
        ?assertMatch(
            {error, {400, _}}, workspace_attachment_logic:filters(#{<<"source">> => <<"sql">>})
        ),
        ?assertMatch(
            {error, {400, _}},
            workspace_attachment_logic:filters(#{
                <<"source">> => <<"talk_record">>, <<"group_id">> => <<"1">>
            })
        )
    after
        lists:foreach(fun meck:unload/1, Modules)
    end.
