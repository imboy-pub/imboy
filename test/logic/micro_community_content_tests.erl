-module(micro_community_content_tests).
-include_lib("eunit/include/eunit.hrl").

missing_tags_preserves_payload_test() ->
    Payload = #{<<"caption">> => <<"hello">>, <<"nested">> => #{<<"a">> => [1]}},
    ?assertEqual({ok, Payload}, channel_message_tags:normalize_payload(Payload)).

trim_and_stable_dedup_test() ->
    Payload = #{
        <<"tags">> => [<<" b ">>, <<"a">>, <<"b">>, <<" 中文 "/utf8>>],
        <<"attachment">> => #{<<"key">> => <<"asset">>}
    },
    ?assertEqual(
        {ok, Payload#{<<"tags">> => [<<"b">>, <<"a">>, <<"中文"/utf8>>]}},
        channel_message_tags:normalize_payload(Payload)
    ).

unicode_character_limits_test() ->
    Twenty = unicode:characters_to_binary(lists:duplicate(20, 16#4E2D)),
    TwentyOne = unicode:characters_to_binary(lists:duplicate(21, 16#4E2D)),
    ?assertMatch({ok, _}, normalize([Twenty])),
    ?assertEqual({error, invalid_tags}, normalize([TwentyOne])),
    ?assertMatch({ok, _}, normalize([<<"😀"/utf8>>])).

raw_item_count_limit_test() ->
    ?assertEqual({ok, #{<<"tags">> => [<<"a">>]}}, normalize(lists:duplicate(5, <<"a">>))),
    ?assertEqual({error, invalid_tags}, normalize(lists:duplicate(6, <<"a">>))).

invalid_tags_test() ->
    Invalid = [
        null,
        <<"a">>,
        #{},
        42,
        [42],
        [null],
        [<<>>],
        [<<" \t\n">>],
        [<<255>>],
        [<<16#E4, 16#B8>>],
        [<<16#C0, 16#AF>>]
    ],
    [?assertEqual({error, invalid_tags}, normalize(Value)) || Value <- Invalid].

empty_array_clears_tags_test() ->
    ?assertEqual({ok, #{<<"tags">> => []}}, normalize([])).

merge_only_updates_tags_test() ->
    Base = #{
        <<"tags">> => [<<"old">>],
        <<"caption">> => <<"keep">>,
        <<"nested">> => #{<<"asset">> => [1, 2]}
    },
    Patch = #{<<"tags">> => [<<" new ">>], <<"caption">> => <<"discard">>},
    ?assertEqual(
        {ok, Base#{<<"tags">> => [<<"new">>]}},
        channel_message_tags:merge_tags(Base, Patch)
    ),
    ?assertEqual({ok, Base}, channel_message_tags:merge_tags(Base, #{<<"caption">> => <<"x">>})),
    ?assertEqual(
        {ok, Base#{<<"tags">> => []}},
        channel_message_tags:merge_tags(Base, #{<<"tags">> => []})
    ),
    ?assertEqual(
        {error, invalid_tags},
        channel_message_tags:merge_tags(Base, #{<<"tags">> => [<<255>>]})
    ).

invalid_payload_test() ->
    ?assertEqual({error, invalid_payload}, channel_message_tags:normalize_payload([])),
    ?assertEqual({error, invalid_payload}, channel_message_tags:merge_tags(#{}, null)),
    ?assertEqual({error, invalid_payload}, channel_message_tags:merge_tags(null, #{})).

normalize(Tags) ->
    channel_message_tags:normalize_payload(#{<<"tags">> => Tags}).

invalid_publish_tags_zero_write_test() ->
    with_mocks([channel_ds], fun() ->
        meck:expect(channel_ds, publish_message, fun(_, _, _, _, _) ->
            erlang:error(unexpected_write)
        end),
        [
            ?assertMatch(
                {error, _},
                channel_logic_message:publish_message(
                    1,
                    <<"2">>,
                    <<"body">>,
                    <<"text">>,
                    #{<<"tags">> => Tags}
                )
            )
         || Tags <- [null, [1], [<<255>>], lists:duplicate(6, <<"a">>)]
        ],
        ?assertEqual(false, meck:called(channel_ds, publish_message, '_'))
    end).

phase1_blocks_search_and_mutations_zero_ds_test() ->
    with_mocks([imboy_policy, imboy_feature, micro_community_content_ds], fun() ->
        meck:expect(imboy_policy, current_profile, fun() -> phase1_delivery end),
        meck:expect(imboy_feature, enabled, fun(channel) -> true end),
        ?assertMatch({error, {403, _}}, micro_community_content_logic:search(1, 2, #{})),
        ?assertMatch({error, {403, _}}, micro_community_content_logic:set_essence(1, 2, 3, true)),
        ?assertMatch({error, {403, _}}, micro_community_content_logic:set_tags(1, 2, 3, [])),
        ?assertEqual([], meck:history(micro_community_content_ds))
    end).

ordinary_author_cannot_mark_essence_test() ->
    content_service(fun() ->
        mock_context(0, true),
        ?assertMatch({error, {403, _}}, micro_community_content_ds:set_essence(1, 2, 3, true)),
        ?assertEqual(false, meck:called(micro_community_content_repo, essence_tx, '_'))
    end).

explicit_role1_creator_cannot_mark_essence_test() ->
    content_service(fun() ->
        mock_context(1, true),
        ?assertMatch({error, {403, _}}, micro_community_content_ds:set_essence(1, 2, 3, true)),
        ?assertEqual(false, meck:called(micro_community_content_repo, essence_tx, '_'))
    end).

cross_channel_message_idor_zero_write_test() ->
    content_service(fun() ->
        mock_context(2, true),
        meck:expect(micro_community_content_repo, message_tx, fun(fake_conn, 2, 999) ->
            {error, not_found}
        end),
        ?assertMatch({error, {404, _}}, micro_community_content_ds:set_essence(1, 2, 999, true)),
        ?assertEqual(false, meck:called(micro_community_content_repo, essence_tx, '_'))
    end).

search_denial_never_reads_rows_or_total_test() ->
    content_service(fun() ->
        mock_context(0, false),
        ?assertMatch(
            {error, {403, _}},
            micro_community_content_ds:search(
                1,
                2,
                #{q => <<"secret">>, tag => <<>>, essence => null},
                {1, 10}
            )
        ),
        ?assertEqual(false, meck:called(micro_community_content_repo, search_tx, '_'))
    end).

archived_workspace_denies_mutation_zero_write_test() ->
    content_service(fun() ->
        mock_context(2, true),
        meck:expect(
            workspace_guard,
            ensure_writable_tx,
            fun(fake_conn, {channel, 2}) -> {error, {980, <<"archived">>}} end
        ),
        ?assertMatch({error, {980, _}}, micro_community_content_ds:set_essence(1, 2, 3, true)),
        ?assertEqual(false, meck:called(micro_community_content_repo, message_tx, '_')),
        ?assertEqual(false, meck:called(micro_community_content_repo, essence_tx, '_'))
    end).

permission_db_error_fails_closed_test() ->
    content_service(fun() ->
        meck:expect(
            micro_community_content_repo,
            context_tx,
            fun(_, _, _) -> throw({abort_tx, {503, <<"unavailable">>}}) end
        ),
        ?assertMatch({error, {503, _}}, micro_community_content_ds:search(1, 2, #{}, {1, 10})),
        ?assertEqual(false, meck:called(micro_community_content_repo, search_tx, '_')),
        ?assertMatch({error, {503, _}}, micro_community_content_ds:set_essence(1, 2, 3, true)),
        ?assertEqual(false, meck:called(micro_community_content_repo, essence_tx, '_'))
    end).

invalid_ds_tags_never_open_transaction_test() ->
    with_mocks([elib_pg], fun() ->
        ?assertMatch({error, {400, _}}, micro_community_content_ds:set_tags(1, 2, 3, [<<255>>])),
        ?assertEqual([], meck:history(elib_pg))
    end).

search_pagination_and_combined_filters_test() ->
    with_mocks([imboy_policy, imboy_feature, micro_community_content_ds], fun() ->
        meck:expect(imboy_policy, current_profile, fun() -> community end),
        meck:expect(imboy_feature, enabled, fun(channel) -> true end),
        meck:expect(
            micro_community_content_ds,
            search,
            fun(1, 2, #{q := <<"中文"/utf8>>, tag := <<"tag">>, essence := true}, {2, 10}) ->
                {ok, #{list => [], total => 0, page => 2, size => 10}}
            end
        ),
        ?assertMatch(
            {ok, #{page := 2, size := 10}},
            micro_community_content_logic:search(
                1,
                <<"2">>,
                #{
                    <<"q">> => <<"中文"/utf8>>,
                    <<"tag">> => <<" tag ">>,
                    <<"essence">> => <<"true">>,
                    <<"page">> => <<"2">>
                }
            )
        ),
        [
            ?assertMatch({error, {400, _}}, micro_community_content_logic:search(1, 2, Opts))
         || Opts <- [
                #{<<"size">> => 0},
                #{<<"size">> => 51},
                #{<<"page">> => -1},
                #{<<"q">> => <<255>>},
                #{<<"essence">> => <<"anything">>}
            ]
        ]
    end).

manager_essence_write_is_independent_of_pinning_test() ->
    content_service(fun() ->
        mock_context(2, true),
        meck:expect(
            micro_community_content_repo,
            essence_tx,
            fun(fake_conn, 2, 3, true) ->
                {ok, #{<<"id">> => 3, <<"is_essence">> => true, <<"is_pinned">> => false}}
            end
        ),
        ?assertMatch(
            {ok, #{<<"is_essence">> := true, <<"is_pinned">> := false}},
            micro_community_content_ds:set_essence(1, 2, 3, true)
        ),
        ?assert(meck:called(micro_community_content_repo, essence_tx, [fake_conn, 2, 3, true]))
    end).

content_service(Fun) ->
    with_mocks([elib_pg, micro_community_content_repo, workspace_guard], fun() ->
        meck:expect(elib_pg, with_tx, fun(Tx, []) -> Tx(fake_conn) end),
        meck:expect(workspace_guard, ensure_writable_tx, fun(_, _) -> ok end),
        meck:expect(
            micro_community_content_repo,
            message_tx,
            fun(fake_conn, 2, 3) ->
                {ok, #{
                    <<"id">> => 3,
                    <<"channel_id">> => 2,
                    <<"author_id">> => 1,
                    <<"payload">> => #{}
                }}
            end
        ),
        Fun()
    end).

mock_context(Role, WorkspaceAllowed) ->
    meck:expect(
        micro_community_content_repo,
        context_tx,
        fun(fake_conn, 1, 2) ->
            {ok, #{
                <<"id">> => 2,
                <<"creator_uid">> => 1,
                <<"status">> => 1,
                <<"access_type">> => 0,
                <<"visibility">> => 0,
                role => Role,
                workspace_allowed => WorkspaceAllowed,
                subscribed => false,
                purchased => false
            }}
        end
    ).

with_mocks(Modules, Fun) ->
    [meck:new(Module, [non_strict, no_link]) || Module <- Modules],
    try
        Fun()
    after
        [meck:unload(Module) || Module <- Modules]
    end.

repo_search_parameterization_and_stable_page_test() ->
    with_mocks([elib_pg], fun() ->
        Q = <<"' OR true --">>,
        Tag = <<"secret">>,
        meck:expect(elib_pg, query, fun(fake_conn, Sql, Params) ->
            ?assertEqual(nomatch, binary:match(Sql, Q)),
            ?assertNotEqual(nomatch, binary:match(Sql, <<"status=1 AND NOT revoked">>)),
            ?assertNotEqual(
                nomatch,
                binary:match(Sql, <<"to_tsvector('jiebacfg'::regconfig,coalesce(content,''))">>)
            ),
            ?assertEqual([2, Q, Tag, true], lists:sublist(Params, 4)),
            case binary:match(Sql, <<"count(*)">>) of
                nomatch ->
                    ?assertNotEqual(
                        nomatch, binary:match(Sql, <<"ORDER BY id DESC LIMIT $5 OFFSET $6">>)
                    ),
                    ?assertEqual([2, Q, Tag, true, 10, 10], Params),
                    {ok, []};
                _ ->
                    {ok, [#{<<"total">> => 0}]}
            end
        end),
        ?assertMatch(
            {ok, #{total := 0, page := 2, size := 10}},
            micro_community_content_repo:search_tx(
                fake_conn, 2, #{q => Q, tag => Tag, essence => true}, {2, 10}
            )
        )
    end).

repo_tags_preserves_payload_and_scoped_id_test() ->
    with_mocks([elib_pg], fun() ->
        meck:expect(elib_pg, query, fun(fake_conn, Sql, [2, 3, Json]) ->
            ?assertEqual([<<"tag">>], jsone:decode(Json)),
            ?assertNotEqual(
                nomatch, binary:match(Sql, <<"jsonb_set(payload,'{tags}',$3::jsonb,true)">>)
            ),
            ?assertNotEqual(
                nomatch,
                binary:match(Sql, <<"channel_id=$1 AND id=$2 AND status=1 AND NOT revoked">>)
            ),
            ?assertEqual(nomatch, binary:match(Sql, <<"is_pinned=">>)),
            ?assertEqual(nomatch, binary:match(Sql, <<"is_essence=">>)),
            {ok, [
                #{
                    <<"id">> => 3,
                    <<"payload">> => jsone:encode(#{
                        <<"tags">> => [<<"tag">>], <<"asset">> => <<"unchanged">>
                    })
                }
            ]}
        end),
        ?assertMatch(
            {ok, #{<<"payload">> := #{<<"asset">> := <<"unchanged">>}}},
            micro_community_content_repo:tags_tx(fake_conn, 2, 3, [<<"tag">>])
        )
    end).

repo_admin_role_precedes_creator_and_permanent_order_test() ->
    with_mocks([elib_pg], fun() ->
        meck:expect(elib_pg, query, fun(fake_conn, Sql, Params) ->
            case binary:match(Sql, <<"FROM channel WHERE">>) of
                {_, _} ->
                    ?assertEqual([2], Params),
                    {ok, [#{<<"id">> => 2, <<"creator_uid">> => 1, <<"scope">> => <<"personal">>}]};
                nomatch ->
                    case binary:match(Sql, <<"FROM channel_admin">>) of
                        {_, _} ->
                            {ok, [#{<<"role">> => 1}]};
                        nomatch ->
                            case binary:match(Sql, <<"FROM channel_order">>) of
                                {_, _} ->
                                    case binary:match(Sql, <<"FOR SHARE">>) of
                                        nomatch ->
                                            ?assertNotEqual(
                                                nomatch,
                                                binary:match(Sql, <<"subscription_end_at IS NULL">>)
                                            ),
                                            ?assertNotEqual(
                                                nomatch, binary:match(Sql, <<"clock_timestamp()">>)
                                            );
                                        _ ->
                                            ok
                                    end,
                                    {ok, [#{<<"id">> => 7}]};
                                nomatch ->
                                    {ok, []}
                            end
                    end
            end
        end),
        ?assertMatch(
            {ok, #{role := 1, purchased := true}},
            micro_community_content_repo:context_tx(fake_conn, 1, 2)
        )
    end).

author_tags_expired_window_zero_write_test() ->
    content_service(fun() ->
        mock_context(0, true),
        meck:expect(
            micro_community_content_repo,
            message_tx,
            fun(fake_conn, 2, 3) ->
                {ok, #{
                    <<"id">> => 3,
                    <<"channel_id">> => 2,
                    <<"author_id">> => 1,
                    <<"created_at">> => <<"2000-01-01T00:00:00Z">>
                }}
            end
        ),
        Previous = application:get_env(imboy, channel_edit_window_seconds),
        application:set_env(imboy, channel_edit_window_seconds, 86400),
        try
            ?assertMatch(
                {error, {403, _}}, micro_community_content_ds:set_tags(1, 2, 3, [<<"tag">>])
            ),
            ?assertEqual(false, meck:called(micro_community_content_repo, tags_tx, '_'))
        after
            restore_edit_window(Previous)
        end
    end).

non_author_tags_zero_write_test() ->
    content_service(fun() ->
        mock_context(0, true),
        meck:expect(
            micro_community_content_repo,
            message_tx,
            fun(fake_conn, 2, 3) ->
                {ok, #{
                    <<"id">> => 3,
                    <<"channel_id">> => 2,
                    <<"author_id">> => 99,
                    <<"created_at">> => <<"2026-10-08T00:00:00Z">>
                }}
            end
        ),
        ?assertMatch({error, {403, _}}, micro_community_content_ds:set_tags(1, 2, 3, [])),
        ?assertEqual(false, meck:called(micro_community_content_repo, tags_tx, '_'))
    end).

restore_edit_window({ok, Value}) -> application:set_env(imboy, channel_edit_window_seconds, Value);
restore_edit_window(undefined) -> application:unset_env(imboy, channel_edit_window_seconds).

workspace_lock_precedes_channel_authorization_test() ->
    content_service(fun() ->
        put(mc_workspace_locked, false),
        meck:expect(
            workspace_guard,
            ensure_writable_tx,
            fun(fake_conn, {channel, 2}) ->
                put(mc_workspace_locked, true),
                ok
            end
        ),
        meck:expect(micro_community_content_repo, context_tx, fun(fake_conn, 1, 2) ->
            ?assertEqual(true, get(mc_workspace_locked)),
            {ok, #{
                <<"id">> => 2,
                <<"status">> => 1,
                <<"access_type">> => 0,
                <<"visibility">> => 0,
                role => 0,
                workspace_allowed => false,
                subscribed => false,
                purchased => false
            }}
        end),
        try
            ?assertMatch({error, {403, _}}, micro_community_content_ds:set_essence(1, 2, 3, true))
        after
            erase(mc_workspace_locked)
        end
    end).

archived_workspace_content_remains_readable_test() ->
    content_service(fun() ->
        mock_context(0, true),
        meck:expect(
            workspace_guard,
            ensure_writable_tx,
            fun(fake_conn, {channel, 2}) -> {error, {980, <<"archived">>}} end
        ),
        meck:expect(
            micro_community_content_repo,
            search_tx,
            fun(fake_conn, 2, #{}, {1, 10}) ->
                {ok, #{list => [], total => 0, page => 1, size => 10}}
            end
        ),
        ?assertMatch({ok, #{total := 0}}, micro_community_content_ds:search(1, 2, #{}, {1, 10}))
    end).

repo_search_decodes_binary_payload_and_keeps_unknown_fields_test() ->
    with_mocks([elib_pg], fun() ->
        meck:expect(elib_pg, query, fun(fake_conn, Sql, _) ->
            case binary:match(Sql, <<"count(*)">>) of
                nomatch ->
                    {ok, [
                        #{
                            <<"id">> => 3,
                            <<"payload">> =>
                                jsone:encode(#{
                                    <<"tags">> => [], <<"unknown">> => #{<<"nested">> => true}
                                })
                        }
                    ]};
                _ ->
                    {ok, [#{<<"total">> => 1}]}
            end
        end),
        ?assertMatch(
            {ok, #{list := [#{<<"payload">> := #{<<"unknown">> := #{<<"nested">> := true}}}]}},
            micro_community_content_repo:search_tx(
                fake_conn, 2, #{q => <<>>, tag => <<>>, essence => null}, {1, 10}
            )
        )
    end).

repo_invalid_payload_aborts_transaction_test() ->
    with_mocks([elib_pg], fun() ->
        meck:expect(elib_pg, query, fun(_, _, _) ->
            {ok, [#{<<"id">> => 3, <<"payload">> => <<"[]">>}]}
        end),
        ?assertThrow(
            {abort_tx, {503, _}}, micro_community_content_repo:tags_tx(fake_conn, 2, 3, [])
        )
    end).
