-module(imboy_quality_cleanup_result_tests).
-include_lib("eunit/include/eunit.hrl").

cleanup_result_test() ->
    Good = {{ok, 12}, #{exit => 143, cleanup => ok}},
    ?assertEqual(
        true, maps:get(<<"ok">>, imboy_quality_env:cleanup_result([{"db", ok}], Good, []))
    ),
    Bad = [
        {{error, eacces}, #{exit => 143, cleanup => ok}},
        {{ok, 12}, #{exit => stop_timeout, cleanup => {error, process_exit_unverified}}},
        {{ok, 12}, #{exit => {cleanup_failed, denied}, cleanup => {error, denied}}},
        {{ok, 12}, #{exit => 137, cleanup => ok}},
        {not_started, {error, unavailable}}
    ],
    [
        ?assertEqual(false, maps:get(<<"ok">>, imboy_quality_env:cleanup_result([], G, [])))
     || G <- Bad
    ],
    ?assertEqual(
        false, maps:get(<<"ok">>, imboy_quality_env:cleanup_result([{"db", failed}], Good, []))
    ),
    ?assertEqual(
        false,
        maps:get(
            <<"ok">>,
            imboy_quality_env:cleanup_result([], Good, [{"dir", {error, ownership_not_verified}}])
        )
    ).

invalid_resources_are_not_clean_test() ->
    ?assertEqual(false, maps:get(<<"ok">>, imboy_quality_env:release(undefined))).

release_once_reports_recorded_cleanup_failure_test() ->
    erase(resources_released),
    try
        put(resources_released, {completed, #{<<"ok">> => false}}),
        ?assertMatch(
            {error, {cleanup_incomplete, #{<<"ok">> := false}}},
            imboy_quality_env:release_once(undefined)
        ),
        put(resources_released, true),
        ?assertMatch(
            {error, {cleanup_incomplete, missing_cleanup_result}},
            imboy_quality_env:release_once(undefined)
        )
    after
        erase(resources_released)
    end.

postgres_without_live_port_handle_fails_closed_test() ->
    ?assertEqual(
        {error, live_postgres_handle_unavailable},
        imboy_quality_env_cleanup:verify_postgres("/unused", #{}, "/unused")
    ).

pg_and_log_failure_do_not_skip_garage_stop_test() ->
    erase(resources_released),
    erase(garage_stopped),
    Prepared = #{garage => {ok, #{log => "log"}}, ctrl => "/tmp", runtime_dirs => []},
    Drop = fun(_) -> error(simulated_pg_disconnect) end,
    Copy = fun(_, _) -> error(simulated_log_error) end,
    Stop = fun(_) ->
        put(garage_stopped, true),
        #{exit => 143, cleanup => retained, receipt => make_ref()}
    end,
    try
        Result = imboy_quality_env:release_with(Prepared, Drop, Stop, Copy, fun(_, _) ->
            error(must_not_delete)
        end),
        ?assertEqual(true, get(garage_stopped)),
        ?assertMatch({completed, #{<<"ok">> := false}}, get(resources_released)),
        ?assertEqual(false, maps:get(<<"ok">>, Result)),
        ?assertMatch([_], maps:get(<<"clone_drop_failed">>, Result))
    after
        erase(resources_released),
        erase(garage_stopped)
    end.

garage_stop_exception_does_not_skip_runtime_check_test() ->
    erase(resources_released),
    Prepared = #{
        garage => {ok, #{log => "log"}},
        ctrl => "/tmp",
        runtime_dirs => ["/this/path/does/not/exist"]
    },
    try
        Result = imboy_quality_env:release_with(
            Prepared,
            fun(_) -> [] end,
            fun(_) -> error(simulated_stop_failure) end,
            fun(_, _) -> {ok, 1} end,
            fun(_, _) -> error(must_not_delete) end
        ),
        ?assertEqual(false, maps:get(<<"ok">>, Result)),
        ?assertMatch([_], maps:get(<<"runtime_cleanup_failed">>, Result)),
        ?assertMatch({completed, #{<<"ok">> := false}}, get(resources_released))
    after
        erase(resources_released)
    end.

garage_stop_precedes_log_copy_and_delete_test() ->
    erase(resources_released),
    put(events, []),
    Append = fun(E) ->
        put(events, get(events) ++ [E]),
        ok
    end,
    Prepared = #{garage => {ok, #{log => "log"}}, ctrl => "/tmp", runtime_dirs => []},
    try
        Result = imboy_quality_env:release_with(
            Prepared,
            fun(_) -> [] end,
            fun(#{keep_dir := true}) ->
                Append(stop),
                #{exit => 143, cleanup => retained, receipt => make_ref()}
            end,
            fun(_, _) ->
                Append(copy),
                {ok, 10}
            end,
            fun(_, _) ->
                Append(delete),
                ok
            end
        ),
        ?assertEqual([stop, copy, delete], get(events)),
        ?assertEqual(true, maps:get(<<"ok">>, Result))
    after
        erase(resources_released),
        erase(events)
    end.

existing_config_is_preserved_test() ->
    Root = filename:join("/tmp", "imboy-config-test-" ++ os:getpid()),
    Config = filename:join([Root, "config", "sys.quality.123.other.config"]),
    ok = filelib:ensure_dir(Config),
    ok = file:write_file(Config, <<"foreign">>, [exclusive]),
    try
        ?assertEqual(
            {error, config_ownership_not_verified}, imboy_quality_env:cleanup_configs(Root)
        ),
        ?assertEqual({ok, <<"foreign">>}, file:read_file(Config))
    after
        file:delete(Config),
        file:del_dir(filename:join(Root, "config")),
        file:del_dir(Root)
    end.

existing_evidence_is_not_overwritten_test() ->
    Root = filename:join("/tmp", "imboy-copy-test-" ++ os:getpid()),
    ok = file:make_dir(Root),
    Source = filename:join(Root, "source"),
    Target = filename:join(Root, "target"),
    ok = file:write_file(Source, <<"fresh">>, [exclusive]),
    ok = file:write_file(Target, <<"old">>, [exclusive]),
    try
        ?assertException(
            error,
            {badmatch, {error, eexist}},
            imboy_quality_env_cleanup:copy_exclusive(Source, Target)
        ),
        ?assertEqual({ok, <<"old">>}, file:read_file(Target))
    after
        file:delete(Source),
        file:delete(Target),
        file:del_dir(Root)
    end.

owned_runtime_dirs_cleanup_and_preserve_unowned_test() ->
    Root = filename:join("/tmp", "imboy-owned-cleanup-" ++ os:getpid()),
    ok = file:make_dir(Root),
    try
        Entries = imboy_quality_env_cleanup:reserve_dirs(Root, "123456"),
        [First | Rest] = Entries,
        Dir = maps:get(path, First),
        Marker = filename:join(Dir, ".imboy-quality-owner"),
        Data = filename:join(Dir, "data"),
        ok = file:write_file(Data, <<"ours">>),
        ?assertException(error, _, imboy_quality_env_cleanup:reserve_dirs(Root, "123456")),
        ?assertEqual({ok, <<"ours">>}, file:read_file(Data)),
        ok = file:write_file(Marker, <<"tampered">>),
        ?assertEqual(
            {error, ownership_not_verified}, imboy_quality_env_cleanup:cleanup_owned(First)
        ),
        ?assertEqual({ok, <<"ours">>}, file:read_file(Data)),
        ok = file:write_file(Marker, maps:get(token, First)),
        ?assertEqual(ok, imboy_quality_env_cleanup:cleanup_owned(First)),
        [?assertEqual(ok, imboy_quality_env_cleanup:cleanup_owned(E)) || E <- Rest],
        ?assertException(error, _, imboy_quality_env_cleanup:reserve_dirs(Root, "../bad"))
    after
        [
            file:del_dir(filename:join([Root, "runtime", Name]))
         || Name <- ["attv5", "hird", "textguard", "talk_owned"]
        ],
        file:del_dir(filename:join(Root, "runtime")),
        file:del_dir(Root)
    end.

postgres_binding_requires_registered_process_and_loopback_test() ->
    Root = "/tmp/owned-volume",
    Data = Root ++ "/pgdata",
    Exec = Root ++ "/postgres-toolchain/bin/postgres",
    Port = 25496,
    Identity = list_to_binary("Wed Nov 4 10:00:00 2026 " ++ Exec ++ " -D " ++ Data ++ " -p 25496"),
    PG = #{
        <<"kind">> => <<"ISOLATED_POSTGRESQL">>,
        <<"status">> => <<"RUNNING">>,
        <<"pid">> => 123,
        <<"host">> => <<"127.0.0.1">>,
        <<"port">> => Port,
        <<"data_dir">> => list_to_binary(Data),
        <<"executable">> => list_to_binary(Exec),
        <<"start_identity">> => Identity
    },
    Args = #{host => "127.0.0.1", port => Port},
    Socket = <<"p123\nn127.0.0.1:25496\n">>,
    ExecInfo = list_to_binary("p123\nn" ++ Exec ++ "\n"),
    ?assertEqual(
        ok,
        imboy_quality_env_cleanup:verified_pg_binding(PG, Args, Root, Identity, {Socket, ExecInfo})
    ),
    ?assertMatch(
        {error, _},
        imboy_quality_env_cleanup:verified_pg_binding(
            PG, Args#{port := 25497}, Root, Identity, {Socket, ExecInfo}
        )
    ),
    ?assertMatch(
        {error, _},
        imboy_quality_env_cleanup:verified_pg_binding(
            PG, Args#{host := "localhost"}, Root, Identity, {Socket, ExecInfo}
        )
    ),
    ?assertMatch(
        {error, _},
        imboy_quality_env_cleanup:verified_pg_binding(
            PG, Args, Root, <<"changed process">>, {Socket, ExecInfo}
        )
    ),
    ?assertMatch(
        {error, _},
        imboy_quality_env_cleanup:verified_pg_binding(
            PG, Args, Root, Identity, {<<"p123\nn127.0.0.1:25497\n">>, ExecInfo}
        )
    ),
    ?assertMatch(
        {error, _},
        imboy_quality_env_cleanup:verified_pg_binding(
            PG, Args, Root, Identity, {Socket, <<"p123\nn/other/postgres\n">>}
        )
    ),
    ?assertMatch(
        {error, _},
        imboy_quality_env_cleanup:verified_pg_binding(
            PG#{<<"data_dir">> := <<"/other/data">>}, Args, Root, Identity, {Socket, ExecInfo}
        )
    ).

postgres_reconnect_rejects_changed_process_or_system_test() ->
    PG = #{
        <<"pid">> => 123,
        <<"port">> => 25496,
        <<"system_identifier">> => <<"system-a">>,
        <<"start_identity">> => <<"start-a">>
    },
    ?assertEqual(ok, imboy_quality_env_cleanup:verified_reconnect(PG, PG, <<"system-a">>)),
    ?assertMatch(
        {error, _},
        imboy_quality_env_cleanup:verified_reconnect(
            PG,
            PG#{<<"pid">> := 124},
            <<"system-a">>
        )
    ),
    ?assertMatch(
        {error, _},
        imboy_quality_env_cleanup:verified_reconnect(
            PG,
            PG#{<<"start_identity">> := <<"start-b">>},
            <<"system-a">>
        )
    ),
    ?assertMatch({error, _}, imboy_quality_env_cleanup:verified_reconnect(PG, PG, <<"system-b">>)),
    ?assertMatch({error, _}, imboy_quality_env_cleanup:verified_reconnect(PG, PG, <<>>)).

postgres_drift_still_stops_garage_test() ->
    erase(resources_released),
    erase(garage_stopped),
    PG = #{<<"pid">> => 123, <<"system_identifier">> => <<"expected">>},
    Prepared = #{garage => {ok, #{log => "log"}}, ctrl => "/tmp", runtime_dirs => []},
    try
        Result = imboy_quality_env:release_with(
            Prepared,
            fun(_) ->
                ok = imboy_quality_env_cleanup:verified_reconnect(PG, PG, <<"different">>),
                []
            end,
            fun(_) ->
                put(garage_stopped, true),
                #{exit => 143, cleanup => retained, receipt => make_ref()}
            end,
            fun(_, _) -> {ok, 1} end,
            fun(_, _) -> ok end
        ),
        ?assertEqual(true, get(garage_stopped)),
        ?assertEqual(false, maps:get(<<"ok">>, Result)),
        ?assertMatch([_], maps:get(<<"clone_drop_failed">>, Result))
    after
        erase(resources_released),
        erase(garage_stopped)
    end.

unverified_orphans_are_not_reaped_test() ->
    ?assertMatch(
        #{<<"status">> := <<"no_unverified_orphans">>},
        imboy_quality_env:orphan_recovery([], [], [], [])
    ),
    ?assertException(
        error,
        {unverified_orphans, _},
        imboy_quality_env:orphan_recovery(["db_123"], ["123"], ["dir"], ["config"])
    ).

volume_identity_must_bind_filesystem_and_marker_test() ->
    Ctrl = "/tmp/quality-control",
    Root = Ctrl ++ "/isolated-resources-test",
    Image = Root ++ ".sparsebundle",
    Marker = Root ++ "/.imboy-quality-owned",
    Hash = binary:copy(<<"a">>, 64),
    RootInfo = #{<<"major_device">> => 2, <<"minor_device">> => 0, <<"inode">> => 10},
    ParentInfo = #{<<"major_device">> => 1, <<"minor_device">> => 0, <<"inode">> => 11},
    V = #{
        <<"kind">> => <<"ISOLATED_APFS_VOLUME">>,
        <<"path">> => list_to_binary(Root),
        <<"image">> => list_to_binary(Image),
        <<"device">> => <<"/dev/disk99s1">>,
        <<"ownership_marker">> => list_to_binary(Marker),
        <<"ownership_marker_sha256">> => Hash,
        <<"filesystem_identity">> => #{<<"root">> => RootInfo, <<"parent">> => ParentInfo}
    },
    ?assertEqual(
        {ok, Root}, imboy_quality_env:verified_volume_root(V, Ctrl, {Hash, RootInfo, ParentInfo})
    ),
    ?assertMatch(
        {error, _},
        imboy_quality_env:verified_volume_root(V, Ctrl, {
            binary:copy(<<"b">>, 64), RootInfo, ParentInfo
        })
    ),
    ?assertMatch(
        {error, _},
        imboy_quality_env:verified_volume_root(
            V, Ctrl, {Hash, RootInfo#{<<"inode">> := 99}, ParentInfo}
        )
    ),
    ?assertMatch(
        {error, _},
        imboy_quality_env:verified_volume_root(
            V#{<<"image">> := <<"/tmp/other">>}, Ctrl, {Hash, RootInfo, ParentInfo}
        )
    ),
    Unmounted = RootInfo#{<<"major_device">> := 1, <<"inode">> := 10},
    VUnmounted = V#{
        <<"filesystem_identity">> := #{<<"root">> => Unmounted, <<"parent">> => ParentInfo}
    },
    ?assertMatch(
        {error, _},
        imboy_quality_env:verified_volume_root(VUnmounted, Ctrl, {Hash, Unmounted, ParentInfo})
    ),
    ?assertMatch(
        {error, _},
        imboy_quality_env:verified_volume_root(
            maps:remove(<<"filesystem_identity">>, V), Ctrl, {Hash, RootInfo, ParentInfo}
        )
    ).
