-module(micro_community_owned_http_fixture_tests).
-include_lib("eunit/include/eunit.hrl").

required_environment_test() ->
    with_env(fun() ->
        lists:foreach(
            fun({Key, Value}) ->
                os:unsetenv(Key),
                ?assertThrow({mc_http_failed, required_run_environment}, target()),
                os:putenv(Key, Value)
            end,
            environment()
        )
    end).

unsafe_port_test() ->
    with_env(fun() ->
        lists:foreach(
            fun(Port) ->
                os:putenv("MC_RUN_PG_PORT", Port),
                ?assertThrow({mc_http_failed, _}, target())
            end,
            ["", "invalid", "56624x", "-1", "0", "65536", "4323", "5432"]
        )
    end).

explicit_current_owner_test() ->
    with_env(fun() ->
        E = target(),
        ?assertEqual(
            <<"Owned by MC run imboy-delivery-v3; synthetic micro-community test data only">>,
            maps:get(owner, E)
        ),
        ?assertEqual("/tmp/imboy-delivery-v3-private/pg-owned.config", maps:get(config, E)),
        ?assertEqual(ok, micro_community_owned_http_fixture:validate_target(opts(), E))
    end).

wrong_targets_test() ->
    with_env(fun() ->
        lists:foreach(
            fun(Opts) ->
                ?assertThrow(
                    {mc_http_failed, private_target_rejected},
                    micro_community_owned_http_fixture:validate_target(Opts, target())
                )
            end,
            [
                (opts())#{port => 4323},
                (opts())#{port => 5432},
                (opts())#{database => "imboy_test_group"},
                (opts())#{host => "192.0.2.1"}
            ]
        )
    end).

connection_bypass_rejected_test() ->
    with_env(fun() ->
        with_epgsql(fun() ->
            meck:expect(epgsql, connect, fun(_) -> error(unexpected_connection) end),
            lists:foreach(
                fun(Opts) ->
                    ?assertEqual(
                        {error, owned_target_rejected},
                        micro_community_owned_http_fixture:connect_owned(Opts)
                    )
                end,
                [(opts())#{port => 4323}, (opts())#{database => "imboy_test_group"}]
            ),
            ?assertEqual(0, meck:num_calls(epgsql, connect, '_'))
        end)
    end).

old_owner_rejected_before_settings_test() ->
    with_env(fun() ->
        with_epgsql(fun() ->
            meck:expect(epgsql, connect, fun(_) -> {ok, synthetic_connection} end),
            meck:expect(epgsql, equery, fun(synthetic_connection, _, []) ->
                {ok, [], [
                    {<<"delivery_v3_mc">>,
                        <<"Owned by MC run mc-20261008T083109Z; synthetic micro-community test data only">>}
                ]}
            end),
            meck:expect(epgsql, close, fun(synthetic_connection) -> ok end),
            ?assertEqual(
                {error, owned_target_rejected},
                micro_community_owned_http_fixture:connect_owned(opts())
            ),
            ?assertEqual(1, meck:num_calls(epgsql, close, '_')),
            ?assertEqual(0, meck:num_calls(epgsql, squery, '_'))
        end)
    end).

current_owner_connects_test() ->
    with_env(fun() ->
        with_epgsql(fun() ->
            Owner = maps:get(owner, target()),
            meck:expect(epgsql, connect, fun(_) -> {ok, synthetic_connection} end),
            meck:expect(epgsql, equery, fun(synthetic_connection, _, []) ->
                {ok, [], [{<<"delivery_v3_mc">>, Owner}]}
            end),
            meck:expect(epgsql, squery, fun(synthetic_connection, _) ->
                [{ok, [], []}, {ok, [], []}]
            end),
            ?assertEqual(
                {ok, synthetic_connection},
                micro_community_owned_http_fixture:connect_owned(opts())
            ),
            ?assertEqual(1, meck:num_calls(epgsql, squery, '_'))
        end)
    end).

trailing_newline_environment_test() ->
    with_env(fun() ->
        lists:foreach(
            fun({Key, Value}) ->
                os:putenv(Key, Value ++ "\n"),
                ?assertThrow({mc_http_failed, _}, target()),
                os:putenv(Key, Value)
            end,
            [
                {"MC_RUN_ID", "imboy-delivery-v3"},
                {"MC_RUN_PG_DATABASE", "delivery_v3_mc"},
                {"MC_RUN_PG_PORT", "56624"}
            ]
        )
    end).

private_config_file_test() ->
    with_env(fun() ->
        Saved = os:getenv("IMBOY_MICRO_PG_CONFIG"),
        Dir = filename:join(
            "/tmp",
            "mc-owned-guard-" ++
                integer_to_list(erlang:unique_integer([positive]))
        ),
        ok = file:make_dir(Dir),
        Path = filename:join(Dir, "pg-owned.config"),
        Other = filename:join(Dir, "other.config"),
        try
            os:putenv("MC_RUN_PRIVATE_DIR", Dir),
            os:putenv("IMBOY_MICRO_PG_CONFIG", Path),
            write_config(Path, opts()),
            ?assertEqual(opts(), micro_community_owned_http_fixture:private_config()),
            ok = file:change_mode(Path, 8#644),
            ?assertThrow(
                {mc_http_failed, private_config_permissions},
                micro_community_owned_http_fixture:private_config()
            ),
            ok = file:change_mode(Path, 8#600),
            ok = file:rename(Path, Other),
            ok = file:make_symlink(Other, Path),
            ?assertThrow(
                {mc_http_failed, private_config_unavailable},
                micro_community_owned_http_fixture:private_config()
            ),
            ok = file:delete(Path),
            ok = file:rename(Other, Path),
            os:putenv("IMBOY_MICRO_PG_CONFIG", Other),
            ?assertThrow(
                {mc_http_failed, private_config_path_required},
                micro_community_owned_http_fixture:private_config()
            ),
            os:putenv("IMBOY_MICRO_PG_CONFIG", Path),
            write_config(Path, (opts())#{port => 4323}),
            ?assertThrow(
                {mc_http_failed, private_target_rejected},
                micro_community_owned_http_fixture:private_config()
            )
        after
            file:delete(Path),
            file:delete(Other),
            file:del_dir(Dir),
            restore("IMBOY_MICRO_PG_CONFIG", Saved)
        end
    end).

rejected_database_results_close_test() ->
    with_env(fun() ->
        Owner = maps:get(owner, target()),
        lists:foreach(
            fun(Result) ->
                with_epgsql(fun() ->
                    meck:expect(epgsql, connect, fun(_) -> {ok, synthetic_connection} end),
                    meck:expect(epgsql, equery, fun(synthetic_connection, _, []) -> Result end),
                    meck:expect(epgsql, close, fun(synthetic_connection) -> ok end),
                    ?assertEqual(
                        {error, owned_target_rejected},
                        micro_community_owned_http_fixture:connect_owned(opts())
                    ),
                    ?assertEqual(1, meck:num_calls(epgsql, close, '_')),
                    ?assertEqual(0, meck:num_calls(epgsql, squery, '_'))
                end)
            end,
            [
                {ok, [], [{<<"wrong_database">>, Owner}]},
                {ok, [], []},
                {error, synthetic_query_failure}
            ]
        )
    end).

write_config(Path, Opts) ->
    ok = file:write_file(Path, io_lib:format("~p.~n", [Opts])),
    ok = file:change_mode(Path, 8#600).
restore(Key, false) -> os:unsetenv(Key);
restore(Key, Value) -> os:putenv(Key, Value).

target() -> micro_community_owned_http_fixture:expected_target().
opts() -> #{host => "127.0.0.1", port => 56624, database => "delivery_v3_mc"}.
environment() ->
    [
        {"MC_RUN_ID", "imboy-delivery-v3"},
        {"MC_RUN_PRIVATE_DIR", "/tmp/imboy-delivery-v3-private"},
        {"MC_RUN_PG_PORT", "56624"},
        {"MC_RUN_PG_DATABASE", "delivery_v3_mc"}
    ].
with_env(Fun) ->
    Saved = [{Key, os:getenv(Key)} || {Key, _} <- environment()],
    try
        [os:putenv(Key, Value) || {Key, Value} <- environment()],
        Fun()
    after
        [
            case Value of
                false -> os:unsetenv(Key);
                _ -> os:putenv(Key, Value)
            end
         || {Key, Value} <- Saved
        ]
    end.
with_epgsql(Fun) ->
    ok = meck:new(epgsql, [non_strict, no_link]),
    try
        Fun()
    after
        meck:unload(epgsql)
    end.

cursor_route_whitelist_test() ->
    Exact =
        {"/api/v1/channel/:channel_id/messages", channel_handler, #{
            action => messages, required_feature => channel
        }},
    ?assertEqual([Exact], micro_community_owned_http_fixture:validate_cursor_routes([Exact])),
    lists:foreach(
        fun(Routes) ->
            ?assertThrow(
                {mc_http_failed, cursor_route_whitelist_mismatch},
                micro_community_owned_http_fixture:validate_cursor_routes(Routes)
            )
        end,
        [
            [],
            [Exact, Exact],
            [{"/api/v1/channel/:channel_id/messages", channel_handler, #{action => messages}}],
            [
                {"/api/v1/channel/:channel_id/messages", channel_handler, #{
                    action => publish_message
                }}
            ],
            [{"/api/v1/channel/:channel_id/messages", adm_channel_handler, #{action => messages}}],
            [{"/api/v1/channel/:channel_id/:any", channel_handler, #{action => messages}}],
            [Exact, {"/arbitrary", channel_handler, #{action => messages}}]
        ]
    ).

cursor_route_matches_real_router_test() ->
    Routes = [
        R
     || {_, HostRoutes} <- imboy_router:get_routes(),
        R = {Path, _, _} <- HostRoutes,
        Path =:= "/api/v1/channel/:channel_id/messages"
    ],
    ?assertEqual(Routes, micro_community_owned_http_fixture:validate_cursor_routes(Routes)).
