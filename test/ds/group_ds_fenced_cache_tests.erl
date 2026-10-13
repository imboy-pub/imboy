-module(group_ds_fenced_cache_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

stale_query_and_waiter_return_current_members_test_() ->
    ?TEST_WITH_APP(fun() ->
        with_fenced_group(fun() ->
            Gid = 9912101,
            Key = {group, Gid},
            Parent = self(),
            Calls = atomics:new(1, []),
            meck:expect(group_member_repo, list_by_gid, fun(G, <<"user_id">>) when G =:= Gid ->
                case atomics:add_get(Calls, 1, 1) of
                    1 ->
                        Parent ! {old_query, self()},
                        receive
                            resume -> rows([41])
                        after 3000 -> error(query_timeout)
                        end;
                    _ ->
                        rows([42])
                end
            end),
            Reader = spawn(fun() -> Parent ! {result, self(), group_ds:member_uids_strict(Gid)} end),
            receive
                {old_query, Reader} -> ok
            after 2000 -> error(reader_timeout)
            end,
            Waiter = spawn(fun() -> Parent ! {result, self(), group_ds:member_uids_strict(Gid)} end),
            try
                ok = await_cache_wait(Waiter, 100),
                ok = group_ds:leave(41, Gid),
                Reader ! resume,
                lists:foreach(
                    fun(P) ->
                        receive
                            {result, P, R} -> ?assertEqual({ok, [42]}, R)
                        after 2000 -> error(result_timeout)
                        end
                    end,
                    [Reader, Waiter]
                ),
                ?assertEqual(2, atomics:get(Calls, 1)),
                ?assertEqual({ok, [42]}, imboy_cache:get(Key)),
                %% 从 legacy 遗留逻辑键中不能读取旧成员。
                ok = depcache:set(Key, [99], 60, [], imboy_cache),
                ?assertEqual({ok, [42]}, group_ds:member_uids_strict(Gid))
            after
                exit(Reader, kill),
                exit(Waiter, kill)
            end
        end)
    end).

empty_members_are_not_cached_test_() ->
    ?TEST_WITH_APP(fun() ->
        with_fenced_group(fun() ->
            Gid = 9912102,
            Key = {group, Gid},
            Calls = atomics:new(1, []),
            meck:expect(group_member_repo, list_by_gid, fun(G, <<"user_id">>) when G =:= Gid ->
                case atomics:add_get(Calls, 1, 1) of
                    1 -> rows([]);
                    _ -> rows([8])
                end
            end),
            ?assertEqual({ok, []}, group_ds:member_uids_strict(Gid)),
            ?assertEqual(undefined, imboy_cache:get(Key)),
            ?assertEqual({ok, [8]}, group_ds:member_uids_strict(Gid)),
            ?assertEqual(2, atomics:get(Calls, 1))
        end)
    end).

query_errors_are_not_cached_test_() ->
    ?TEST_WITH_APP(fun() ->
        with_fenced_group(fun() ->
            Gid = 9912103,
            Key = {group, Gid},
            Calls = atomics:new(1, []),
            meck:expect(group_member_repo, list_by_gid, fun(G, <<"user_id">>) when G =:= Gid ->
                case atomics:add_get(Calls, 1, 1) of
                    1 -> {error, db_down};
                    _ -> rows([8])
                end
            end),
            ?assertEqual({error, db_down}, group_ds:member_uids_strict(Gid)),
            ?assertEqual(undefined, imboy_cache:get(Key)),
            ?assertEqual({ok, [8]}, group_ds:member_uids_strict(Gid)),
            ?assertEqual(2, atomics:get(Calls, 1))
        end)
    end).

continuous_invalidation_unwraps_uncached_error_test_() ->
    ?TEST_WITH_APP(fun() ->
        with_fenced_group(fun() ->
            Gid = 9912104,
            Key = {group, Gid},
            Calls = atomics:new(1, []),
            meck:expect(group_member_repo, list_by_gid, fun(G, <<"user_id">>) when G =:= Gid ->
                atomics:add_get(Calls, 1, 1),
                ok = imboy_cache:invalidate(Key),
                {error, db_down}
            end),
            ?assertEqual({error, db_down}, group_ds:member_uids_strict(Gid)),
            ?assertEqual(3, atomics:get(Calls, 1)),
            ?assertEqual(undefined, imboy_cache:get(Key))
        end)
    end).

unexpected_query_shape_is_not_cached_test_() ->
    ?TEST_WITH_APP(fun() ->
        with_fenced_group(fun() ->
            Gid = 9912105,
            Key = {group, Gid},
            meck:expect(group_member_repo, list_by_gid, fun(G, <<"user_id">>) when G =:= Gid ->
                {ok, invalid_rows}
            end),
            ?assertEqual(
                {error, {unexpected_repo_result, {ok, invalid_rows}}},
                group_ds:member_uids_strict(Gid)
            ),
            ?assertEqual(undefined, imboy_cache:get(Key))
        end)
    end).

with_fenced_group(Fun) ->
    Saved = [{cache_fenced_namespaces, application:get_env(imboy, cache_fenced_namespaces)}],
    {D, M, S} = cache_fixture_helper:setup(Saved, fun group_components/0),
    try
        Fun()
    after
        Actions =
            [
                fun() -> meck:unload(micro_community_access_repo) end,
                fun() -> meck:unload(group_member_repo) end,
                fun() ->
                    cache_fixture_helper:invalidate([{group, G} || G <- lists:seq(9912101, 9912105)])
                end
            ] ++
                [fun() -> cache_fixture_helper:release(P) end || P <- [S, M, D]],
        try
            cache_fixture_helper:cleanup(Actions)
        after
            cache_fixture_helper:restore_env(Saved)
        end
    end.
group_components() ->
    application:set_env(imboy, cache_fenced_namespaces, [group]),
    application:ensure_all_started(crypto),
    application:ensure_all_started(syn),
    syn:add_node_to_scopes([imboy_cache]),
    D = cache_fixture_helper:acquire(imboy_cache, fun() ->
        depcache:start_link(imboy_cache, #{memory_max => 64})
    end),
    M = cache_fixture_helper:acquire(elib_metric, fun elib_metric:start_link/0),
    S = cache_fixture_helper:acquire(imboy_cache_sync, fun imboy_cache_sync:start_link/0),
    cache_fixture_helper:invalidate([{group, G} || G <- lists:seq(9912101, 9912105)]),
    cache_fixture_helper:mock(group_member_repo, [unstick, passthrough, no_link]),
    cache_fixture_helper:mock(micro_community_access_repo, [unstick, passthrough, no_link]),
    %% 仅验证缓存回填；真实 discussion ACL 的数据库验证独立执行。
    meck:expect(micro_community_access_repo, filter_uids, fun(_, Uids) -> {ok, Uids} end),
    {D, M, S}.

rows(Uids) -> {ok, [#{<<"user_id">> => U} || U <- Uids]}.
await_cache_wait(_Pid, 0) ->
    error(waiter_not_blocked);
await_cache_wait(Pid, N) ->
    case process_info(Pid, current_function) of
        {current_function, {gen, do_call, 4}} ->
            ok;
        _ ->
            timer:sleep(5),
            await_cache_wait(Pid, N - 1)
    end.
